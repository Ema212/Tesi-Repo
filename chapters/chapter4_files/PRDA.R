#
require("truncnorm")
#

##### It calculates Cohen's d [Internal Function]
cohen_d <- function(x1,x2) {
  
  (m1 <- mean(x1, na.rm = TRUE))
  (m2 <- mean(x2, na.rm = TRUE))
  S1 <- sum((x1-m1)^2, na.rm=TRUE)
  S2 <- sum((x2-m2)^2, na.rm=TRUE)
  (n1 <- length(na.omit(x1)))
  (n2 <- length(na.omit(x2)))
  (df <- n1+n2-2)
  
  (pool.s <- sqrt( (S1 + S2) / df ) )
  (d <- (m2 - m1)/pool.s)
  
  s1 <- sd(x1, na.rm = TRUE)
  s2 <- sd(x2, na.rm = TRUE)
  (tstat <- (m2-m1) / (pool.s * sqrt((n1+n2)/(n1*n2))))
  (pval <- pt(abs(tstat),df,lower.tail=FALSE)*2)
  
  return(list(d=d,tstat=tstat,pval=pval))
}

##### It calculates power, typeM error & typeS error based on B replicates [Internal Function]
# Note for design_analysis:  target_d is equal to sim_y
sim_estimate <- function(sim_y, target_d, n1 = n1, n2 = n1, sig.level = .05, B = 1e4 ) {
  outsim <- t(replicate( B, {
    x1 <- rnorm(n1)
    x2 <- rnorm(n2,sim_y)
    d <- cohen_d(x1,x2)
    sim <- c(d$d,d$tstat,d$pval,sim_y)
  }))
  colnames(outsim) <- c("est_d","tstat","pval","sim_d")
  outsim <- data.frame(outsim)
  (outsim$power <- sum( outsim$pval < sig.level ) / nrow(outsim))
  (outsim$typeS <- with(outsim, sum( (pval < sig.level) & (tstat<0)) / sum(pval < sig.level)  ) )
  (outsim$typeM <- with(outsim, mean(abs(est_d[pval< sig.level]))) / target_d)
  return(outsim)
}

##### It performs retrospective design analysis [Internal Function]
design_analysis_power <- function( n, d, sig.level = 0.05, B = 1e4 ,tol=.005 ) {
  out <- sim_estimate(sim_y=d,target_d=d,n1=n,sig.level=sig.level,B=B)
  out1=list(d=d,n=n,power=out[1,"power"],typeS=out[1,"typeS"],typeM=out[1,"typeM"])
  return(out1)
}

##### It performs prospective design analysis [Internal Function]
design_analysis_n <- function( d, power, sig.level = 0.05, rangen = c(2,1000), B = 1e4, tol = .005 ){
  
  n_seq <- seq( rangen[1], rangen[2], by = 1 )
  (n_target <- round(median(n_seq)))
  find_power <- FALSE
  
  ## check with maximum N
  #cat("Estimating power with n =",rangen[2],"\n")
  (est_P <- sim_estimate( n1 = rangen[2], n2 = rangen[2], sim_y = d, target_d=d, sig.level = sig.level, B = B ))
  (est_power <- est_P[1,]$power)
  
  if ( est_power < power ) {
    cat(paste0("Actual power = ", est_power, " with n = ", rangen[2], " (per group); " ),"\n")
    cat(paste0("   try to increase maximum of rangen > ", rangen[2],"."),"\n")
    out <- NULL
  } else {
    
    ## estimating power
    while( (!find_power) ) {
      #cat("Estimating power with n =",n_target,"\n")
      (est_P <- sim_estimate( n1 = n_target, n2 = n_target, sim_y = d, target_d=d, sig.level = sig.level, B = B))
      (est_power <- est_P[1,]$power)
      
      if ( (est_power<=(power+tol)) & (est_power>(power-tol)) ) {
        find_power <- TRUE
      } else {
        
        if (length(n_seq)==1) {
          print(n_seq)
          stop(" ")
        }
        
        
        if ( est_power > (power-tol) ) {
          (n_seq <- seq( min(n_seq), n_target, by = 1))
          (n_target <- round(median(n_seq)))
        } else {
          (n_seq <- seq( n_target, max(n_seq), by = 1))
          (n_target <- round(median(n_seq)))
        }
      }
    }
    out <- list( d = d, power = power, n = n_target, typeS=est_P[1,"typeS"],typeM=est_P[1,"typeM"])
  }
  
  #cat("","\n")
  if (!is.null(out))  return(out)
}

##### It performs prospective and retrospective design analysis
# Note: plausible 'd' is a fixed value. 'n' is the sample size per group
design_analysis <- function( d, n = NULL, power = NULL, sig.level = 0.05,
                             B = 1e4 , rangen = c( 2, 1000 ),  tol=.005) {
  if  (d <= 0){
    stop("A d greater than 0 must be entered")
  }
  
  
  if (sum(sapply(list(n, d, power, sig.level), is.null)) != 1) {
    stop("It is necessary to provide either 'n' or 'power'")
  }
  
  if (is.null(n)) {
    out <- design_analysis_n( d=d, power=power, sig.level=sig.level, rangen=rangen, B=B )
  } else {
    if (is.null(power)) {
      out <- design_analysis_power( n, d, sig.level, B )
    } 
  }
  
  if (!is.null(out)) return(out)
}

######## Sampling Cohen's d given a bounded distribution [Internal Function]
sampling_d <- function( target_limits = c(0,1), distribution = c("uniform","normal"), k = 1/6, B0 = 1e4 ) {
  
  distribution <- match.arg(distribution)
  my <- (sum(target_limits)/2)
  sy <- diff(target_limits)*k
  if (distribution=="uniform") {
    y <- runif(B0,target_limits[1], target_limits[2])
  } else {
    y <- rtruncnorm(B0,target_limits[1], target_limits[2], mean = my, sd = sy)
    
  }
  return(list(y=y,my=my,sy=sy))
}

######## It performs retrospective design analysis given a plausible interval and a distribution for d.
########  It works also for different sample sizes per group.
########  It works also given a fixed value of d.
design_est<- function( n1 , n2 = n1, target_d = NULL, target_d_limits = NULL, distribution = c("uniform","normal"),
                       k = 1/6, sig.level = 0.05, B = 500, B0 = 500, return_data = FALSE ) {
  
  # CHECKS
  distribution <- match.arg(distribution)
  
  
  if (sum(sapply(list(target_d,target_d_limits), is.null)) != 1) {
    stop("It is necessary to provide either 'target_d' or 'target_d_limits'.")
  }
  
  if (!is.null(target_d)) {
    if (target_d<=0) stop("Target_d must be greater than 0")
  } 
  
  if (!is.null(target_d_limits)) {
    if (min(target_d_limits)<0) stop("Minimum of target_d_limits must be greater than 0")
    if (distribution=="uniform") k<-NULL
  } 
  
  
  
  
  call_fix <- list(n1=n1, n2=n2, target_d=target_d, B=B)
  call_est <- list(n1=n1, n2=n2, target_d_limits=target_d_limits, distribution=distribution,k=k,B=B,B0=B0)
  
  
  
  ############## plausible interval and distribution for d
  if(!is.null(target_d_limits))
  {
    my <- (sum(target_d_limits)/2) 
    sy <- diff(target_d_limits)*k 
    y <- sampling_d(target_d_limits,distribution = distribution,B0=B0, k = k) ###
    sim_cohen <- y$y
    # 
    data <- t(sapply(1:B0, function(b){
      temp <- sim_estimate(sim_cohen[b], target_d=my, n1 = n1, n2 = n2, B = B )
      c(temp[1,"power"],temp[1,"typeS"],temp[1,"typeM"])
    }))
    data <- data.frame(data)
    data<-data.frame(data)
    names(data)<-c("power","typeS","typeM")
    
    (power <- mean(data$power))
    (typeS <- mean(data$typeS,na.rm = TRUE))
    (typeM <- mean(data$typeM,na.rm = TRUE))
    
    if (return_data) {
      results <- list(power=power,typeS=typeS,typeM=typeM,data=data)
      outList <-  list(call=call_est,results=results)
    } else {
      results <-  list(power=power,typeS=typeS,typeM=typeM) 
      outList <-  list(call=call_est,results=results)
    }
    return(outList)
  }
  ########### fixed d
  if (is.null(target_d_limits)) {
    temp<-sim_estimate(sim_y=target_d, target_d=target_d, n1 = n1, n2 = n2, B = B )
    results<-list(power=temp[1,"power"],typeS=temp[1,"typeS"],typeM=temp[1,"typeM"])
    outList <-  list(call=call_fix,results=results)
    return(outList)
  }
  
}
# Authors: m.p. & g.a. 2019
# For any further information please contact:
# massimiliano.pastore@unipd.it
# gianmarco altoè.unipd.it
