print("Hi, I'm Bella")

library("ggplot2")
library("here")

plot1 <- ggplot(aes(x = Sepal.Length, y = Sepal.Width), 
                data = iris) +
  geom_point()
plot1

ggsave(here("figures/plot_1_b.png"))
