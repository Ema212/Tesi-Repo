library(ggplot2)

effs = data.frame(
  x = c(0, 0.22, 0.43, 0.72), 
  y = c(1, 2, 3, 4)
)

poly_df = data.frame(
  x = c(0.27, 0.44, 0.61, 0.44),
  y = c(0.50, 0.56, 0.50, 0.44)
)

# GRAFICO SESOI

graph_p = ggplot(effs, aes(x = x, y = y)) +
  theme_classic() + 
  theme(
    axis.line.y = element_line(color = "white"),
    axis.text.x = element_text(size = 10, color = "black", family= "Times"),
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.ticks.y = element_blank()
  ) +
  # preferisco coord_cartesian per non rimuovere righe
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  
  # poligono *dopo* gli altri layer, con group=1 e fill evidente
  geom_polygon(
    data = poly_df,
    aes(x = x, y = y, group = 1),
    inherit.aes = FALSE,
    fill = "#f7a072",
    colour = "black",
    size = 0.4,
    alpha = 0.8
  )

print(graph_p)



# GRAFICO SESOI

graph_s = ggplot(effs, aes(x = x, y = y)) +
  theme_classic() + 
  theme(
    axis.line.y = element_line(color = "white"),
    axis.text.x = element_text(size = 10, color = "black", family= "Times"),
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.ticks.y = element_blank()
  ) +
  # preferisco coord_cartesian per non rimuovere righe
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  
  # layer interni (linee/segmenti)
  geom_vline(xintercept = 0.22, linetype = 2, color = "#1b3a60") +
  geom_vline(xintercept = 0.43, linetype = 2, color = "#6b8e23") +
  geom_vline(xintercept = 0.72, linetype = 2, color = "#a52a2a") +
  geom_segment(x= 0.22, y = -1, xend = 0.22, yend= 0.5, colour = "#1b3a60") +
  geom_segment(x= 0.43, y = -1, xend = 0.43, yend= 0.5, colour = "#6b8e23") +
  geom_segment(x= 0.72, y = -1, xend = 0.72, yend= 0.5, colour = "#a52a2a")

print(graph_s)



# GRAFICO COMBINATO
graph_m = ggplot(effs, aes(x = x, y = y)) +
  theme_classic() + 
  theme(
    axis.line.y = element_line(color = "white"),
    axis.text.x = element_text(size = 10, color = "black", family= "Times"),
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.ticks.y = element_blank()
  ) +
  # preferisco coord_cartesian per non rimuovere righe
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  
  # layer interni (linee/segmenti)
  geom_vline(xintercept = 0.22, linetype = 2, color = "#1b3a60") +
  geom_vline(xintercept = 0.43, linetype = 2, color = "#6b8e23") +
  geom_vline(xintercept = 0.72, linetype = 2, color = "#a52a2a") +
  geom_segment(x= 0.22, y = -1, xend = 0.22, yend= 0.5, colour = "#1b3a60") +
  geom_segment(x= 0.43, y = -1, xend = 0.43, yend= 0.5, colour = "#6b8e23") +
  geom_segment(x= 0.72, y = -1, xend = 0.72, yend= 0.5, colour = "#a52a2a") +
  
  # poligono *dopo* gli altri layer, con group=1 e fill evidente
  geom_polygon(
    data = poly_df,
    aes(x = x, y = y, group = 1),
    inherit.aes = FALSE,
    fill = "#f7a072",
    colour = "black",
    size = 0.6,
    alpha = 0.8
  )

print(graph_m)

