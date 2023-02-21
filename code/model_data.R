library(tidyverse)
library(here)

model_data <- function(processed_data, simulated_data) {
  models <- list()
  # models$model_1 <- ...
  
  
  for (model_index in seq_len(length(models))) {
    write_rds(models[[model_index]], 
              file = paste0(here("results/", names(models)[model_index])))
  }
  return(models)
}