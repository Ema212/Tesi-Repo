ggplot(
  DATA, aes(x = Part_A, fill = group)) + #x è asse x, fill è colore, fill deve tenrere due livelli
  xlim(7, 90) +
  ylim(0.0, 0.06) +
  geom_density(alpha=0.8, color = "black") + 
  labs(x = "Outcome", y = NULL, fill="Legend") +
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.ticks.length = unit(0.4, "cm"), 
        axis.text.x = element_text(size = 15, color = "black"),
        axis.text.y = element_text(size = 15, color = "black"), 
        axis.title = element_text(size = 15), 
        legend.position = c(0.87, 0.6),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15))

#INPUT: 2 variabili, 
#- due livelli 
#- sss

library(ggplot2)
library(sn)

# Sequenza x su cui valutare le densità
x <- seq(-1, 2, length.out = 1000)

library(sn)
# Plausible: normale con mean = 0.3, sd = 0.4
plausible = dnorm(x, mean = 0.3, sd = 0.3)
sesoi = dsn(x, xi = 0.3, omega = 0.3, alpha = 7)




# 1) Grafico Plausible

#dataframe solo Plausible
df_plausible <- data.frame(
  x = x,
  density = plausible
)

#grafico
ggplot(df_plausible, aes(x = x, y = density)) +
  geom_area(fill = "#32394e", alpha = 0.5, color = "black", size = 0.7) +
  labs(x = "Plausible effect", y = NULL) +
  scale_y_continuous(labels = NULL) +
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 15, color = "black", family = "Times New Roman"),
        axis.text.y = element_text(size = 15, color = "black", family = "Times New Roman"), 
        axis.title = element_text(size = 15)) 


# 2) Grafico SESOI

#dataframe solo SESOI
df_sesoi <- data.frame(
  x = x,
  density = sesoi
)

#grafico
ggplot(df_sesoi, aes(x = x, y = density)) +
  geom_area(fill = "#6fcde8", alpha = 0.5, color = "black", size = 0.7) +
  labs(x = "SESOI", y = NULL) +
  scale_y_continuous(labels = NULL) +
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 15, color = "black", family = "Times New Roman"),
        axis.text.y = element_text(size = 15, color = "black", family = "Times New Roman"), 
        axis.title = element_text(size = 15)) 

# 3) Grafico Finale

# dataframe
df_match <- data.frame(
  x = rep(x, 2),
  density = c(plausible, sesoi),
  group = rep(c("Plausible", "SESOI"), each = length(x))
)

ggplot(df_match, aes(x = x, y = density, fill = group)) +
  geom_area(alpha = 0.5, position = "identity", color= "black", size=0.7) +
  scale_fill_manual(values = c("Plausible" = "#32394e", "SESOI" = "#6fcde8")) + 
  labs(x = "Effect size", y = NULL, fill = NULL) +
  scale_y_continuous(labels = NULL) +
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.ticks.y = element_blank(),  
        axis.text.x = element_text(size = 15, color = "black"),
        axis.text.y = element_text(size = 15, color = "black"), 
        axis.title = element_text(size = 15), 
        legend.position = c(0.87, 0.6),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15))
