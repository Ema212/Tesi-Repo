# mini meta (Goth et al., 2016)
# seguendo il fixed effect approach 
## (per quando l'autore pensa che ci sia un vero population ES, che succede quando gli studi sono simili metodologicamente)
## (o si fa uno statement su studi sotto mano senza voler generalizzare)
# invece, vedere step 7 per random effect approach

# step 2: tutti gli ES devono essere indipendenti
# step 3: convertire gli ES nella stessa metrica (tutti d?)

# step 4: CALCOLA
# converti tutti i *d* in *r* (formula 4)
# calcola la media ponderata per correlazioni
## Fisher’s z transformation for normalization (the ES is now represented as r_z) -- VEDI NOTA 4
## gli r_z possono essere combinati nella formula 5

# step 5: INTERPRETA
## converti il mean ES da correlazione a *d* usando la formula 6

# step 7: RANDOM-EFFECT APPROACH
## media aritmetica degli ES con N = numero di studi
# nota: sarebbe da usare un metodo che dà pesi diversi alle meta-analisi in base alla loro numerosità campionaria

library(metafor)
library(dplyr)
library(purrr)

rm(list = ls())
################################################################################ FIXED EFFECT APPROACH
########### step 3: convertire gli ES nella stessa metrica (tutti d)
# fattore di correzione J (Hedges)
J_factor <- function(n1, n2) {
  df <- n1 + n2 - 2
  1 - 3 / (4*df - 1)
}
# da hedges' g o SMD a cohen's d
g_to_d <- function(g_or_smd, n1, n2) {
  g_or_smd / J_factor(n1, n2)
}

# studio 1
SMD <- -0.44
n1  <- 137  # approx guided
n2  <- 138  # approx unguided

J   <- J_factor(n1, n2)
(d_1   <- g_to_d(SMD, n1, n2))

# studio 2
g   <- 0.88
n1  <- 43   # iCBT
n2  <- 44   # TAU
J   <- J_factor(n1, n2)
(d_2 <- g_to_d(g, n1, n2))

# studio 4
g   <- 1.35 
n1  <- 328 #iCBT senza guida clinica
n2  <- 85 #iCBT clinician-supervised
J   <- J_factor(n1, n2)
(d_4 <- g_to_d(g, n1, n2))

# studio 6
g  <- 1.40   # hedges' g (between-groups) tabella 3
n1 <- 46     # trattamento (pre validi)
n2 <- 53     # WLC (pre validi)

J <- J_factor(n1, n2)
(d_6 <- g_to_d(g, n1, n2))

# studio 7
g  <- 0.91   # post-trattamento (non follow-up)
n1 <- 77     # trattamento
n2 <- 79     # controllo

J <- J_factor(n1, n2)
(d_7 <- g_to_d(g, n1, n2))

########### step 3: altri effect size d e tabella
d_3 <- 0.72
d_5 <- 0.84

d <- c(d_1, d_2, d_3, d_4, d_5, d_6, d_7)
n1 <- c(137, 43, 32, 328, NA, 46, 77)
n2 <- c(138, 44, NA, 85, NA, 53, 79)

tabella <- data.frame(
  source = 1:7,
  n1 = n1,
  n2 = n2,
  "cohen d" = d
)
tabella

########### step 4: media pesata degli ES
# converti tutti i *d* in *r* (formula 4)
d_to_r <- function(d, n1, n2) {
  N <- n1 + n2
  P <- n1 / N
  Q <- n2 / N
  # Formula 4: r = sqrt( d^2 / ( d^2 + 1/(P*Q) ) )
  r <- sqrt( (d^2) / ( (d^2) + 1/(P*Q) ) )
  # preserva il segno di d
  r * sign(d)
}
df_step4 <- tabella %>%
  mutate(
    N      = n1 + n2,
    r      = d_to_r(d, n1, n2),
    rz     = atanh(r),        # Fisher's z
    w      = N - 3            # pesi (Formula 5)
  )
# calcola la media ponderata per correlazioni
weighted_rz <- with(df_step4, sum(w * rz, na.rm = TRUE) / sum(w, na.rm = TRUE)) # media pesata su rz con pesi w = N - 3
(weighted_r  <- tanh(weighted_rz)) # back-transform a r

########### step 5: convertire r in d 
d_minimetaFEa <- (2 * weighted_r) / sqrt(1 - weighted_r^2)
d_minimetaFEa # VALORE RISULTATO DALLA MINI-META USANDO FIXED EFFECT APPROACH


################################################################################ RANDOM EFFECT APPROACH
# (che bisognerebbe usare perché gli studi sono metodologicamente diversi)
########### step 7: media aritmetica degli ES con N = numero di studi
k <- length(d)                              # numero di studi coinvolti
d_minimetaREa <- sum(d, na.rm = TRUE) / k   # media aritmetica 
d_minimetaREa # VALORE RISULTATO DALLA MINI-META USANDO RANDOM EFFECT APPROACH



