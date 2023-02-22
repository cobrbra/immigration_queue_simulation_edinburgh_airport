print("hi, I'm Jacob")

library(tidyverse)

mtcars %>% 
  dim()

mtcars %>% 
  filter(mpg < 20) %>% 
  dim()

fig_1 <- data.frame(x = 1:10, y = 15:24) %>% 
  ggplot(aes(x = x, y = y)) + geom_point()

fig_1

ggsave(here("figures/fig_1.png"), fig_1)
