library(ggplot2)

effs = data.frame(
x = c(0, 0.22, 0.34, 0.59), 
y= c(1, 2, 3, 4), 
z = c(1, 0, -1, 0), 
q = c(0, 1, 0, -1))

poly_df = data.frame(
  x = c(0.27, 0.46, 0.62, 0.46),
  y = c(2.25, 2, 2.25, 2.5))


graph = ggplot(effs, aes(x = x, y = y)) +
  theme_classic() + 
  theme(axis.line.y = element_line(color = "white"),
        axis.text.x = element_text(size = 15, color = "black"),
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank()) +
  
  scale_x_continuous(limits = c(0 : 1)) +
  scale_y_continuous(limits = c(0 : 5)) +
  
  geom_polygon(
    data = poly_df, 
    aes(x = x, y = y), 
    inherit.aes = FALSE, 
    fill = "orange", 
    color = "black"
  ) +
  
  geom_vline(xintercept = 0.22, linetype = 2, color = "blue") +
  geom_vline(xintercept = 0.34, linetype = 2, color = "red") +
  geom_vline(xintercept = 0.59, linetype = 2, color = "green") +
  geom_segment(x= 0.34, y = -1, xend = 0.34, yend= 0.5, colour = "red") +
  geom_segment(x= 0.22 , y = -1, xend = 0.22, yend= 0.5, colour = "blue") +
  geom_segment(x= 0.59, y = -1, xend = 0.59, yend= 0.5, colour = "green")
  

graph
geom_polygon(data = poly_df, aes(x = x, y = y), inherit.aes = FALSE) +
  

        