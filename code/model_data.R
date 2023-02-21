library(tidyverse)
library(here)

model_data <- function(processed_data, simulated_data) {
  models <- list()
  # models$model_1 <- ...
  
  
  for (model_index in seq_len(length(models))) {
    model <- models[model_index]
    model_name <- names(models)[model_index]
    write_rds(model, file = paste0(here("results/", model_name)))
  }
  return(models)
}