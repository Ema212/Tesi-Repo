library(ggplot2)
library(sn)

# Parametri
x <- seq(-4, 4, length.out = 1000)
mu1 <- 0.2
mu2 <- 0.45

# Curve: normale simmetrica e skew normale
y1 <- dnorm(x, mean = mu1, sd = 0.40)
y2 <- dsn(x, xi = mu2, omega = 0.9, alpha = 5)

# Unico data.frame con colonna "group"
df <- data.frame(
  x = rep(x, 2),
  density = c(y1, y2),
  group = factor(rep(c("Plausible effect size", "SESOI"), each = length(x)))
)

# Plot unico
ggplot(df, aes(x = x, y = density, color = group)) +
  geom_line(size = 1) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-1, 3)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("#1b3a60", "#6b8e23")) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", size = 0.3),
    legend.position = "none"
  )
