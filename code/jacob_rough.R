print("hi, I'm Jacob")

library(tidyverse)

mtcars %>% 
  dim()

mtcars %>% 
  filter(mpg < 20) %>% 
  dim()
