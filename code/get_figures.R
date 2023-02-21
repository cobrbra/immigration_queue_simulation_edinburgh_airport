library(tidyverse)
library(here)

get_figures <- function(results) {
  figures <- list()
  
  figures$mt_cars_summary <- results$mt_table %>% 
    ggplot(aes(x = cyl, y = mpg)) + 
      geom_point() + 
      geom_smooth(method = "lm")
  
  figures$mpg_model_actual_vs_predicted <- results$mpg_model_predictions %>% 
    ggplot(aes(x = actual, y = predicted)) + geom_point()
  
  # figures$figure_1 <- ...
  # insert more code that generates figures here
  
  
  for (figure_index in seq_len(length(figures))) {
    ggsave(paste0(here("figures/"), names(figures)[figure_index], ".png"), 
           figures[[figure_index]])
  }
  return(figures)
}