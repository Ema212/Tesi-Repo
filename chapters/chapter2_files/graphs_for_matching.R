library(ggplot2)
library(patchwork)
library(sn)

x <- seq(-4, 4, length.out = 1000)
mu1 <- 0.2
mu2 <- 0.45
sd <- 0.80

y1 <- dnorm(x, mean = mu1, sd = 0.40)
y2 <- dsn(x, xi = mu2, omega = 0.9, alpha = 5)  # alpha > 0 asimmetria a destra

df1 <- data.frame(x = x, density = y1)
df2 <- data.frame(x = x, density = y2)

# Primo grafico: solo media 0

p1 <- ggplot(df1, aes(x = x, y = density)) +
  geom_line(color = "#1b3a60", size = 1) +
  
  labs(title = "Plausible effect size",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", size = 0.3),
    legend.position = "none"
  ) +
  
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-1, 3))+
  scale_y_continuous(limits = c(0, 1))

# Secondo grafico: solo media 0.65

p2 <- ggplot(df2, aes(x = x, y = density)) +
  geom_line(color = "#6b8e23", size = 1) +
  
  labs(title = "SESOI",
       x = NULL, y = NULL) +
  
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", size = 0.3),
    legend.position = "none"
  ) +
  
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-1, 3))+
  scale_y_continuous(limits = c(0, 1))

# Grafici affiancati
graph2= p1 + p2
graph2
