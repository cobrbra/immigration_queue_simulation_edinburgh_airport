library(tidyverse)
library(here)

get_figures <- function(results) {
  # results <- targets::tar_read(example_results) # Use for debugging, COMMENT WHEN RUNNING TARGETS
  
  figures <- list()
  
  figures$mt_cars_summary <- results$mt_table %>% 
    ggplot(aes(x = cyl, y = mpg)) + 
      geom_point() + 
      geom_smooth(method = "lm") +
      theme_minimal()
  
  figures$mpg_model_actual_vs_predicted <- results$mpg_model_predictions %>% 
    ggplot(aes(x = actual, y = predicted)) + 
      geom_point() +
      geom_smooth(method = "lm") + 
      theme_minimal()
  
  # figures$figure_1 <- ...
  # insert more code that generates figures here
  
  
  for (figure_index in seq_len(length(figures))) {
    ggsave(paste0(here("figures/"), names(figures)[figure_index], ".png"), 
           figures[[figure_index]])
  }
  return(figures)
}