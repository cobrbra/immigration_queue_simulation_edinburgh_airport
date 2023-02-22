library(tidyverse)
library(here)

model_data <- function(processed_data, simulated_data) {
  models <- list()
  
  models$model_mpg_from_rest <- processed_data %>% # read_csv(here("processed_data/example_processed_data.csv")) %>% # 
    lm(formula = mpg ~ .)
  
  # models$model_1 <- ...
  # add code here to generate models
  
  for (model_index in seq_len(length(models))) {
    write_rds(models[[model_index]], 
              file = paste0(here("results/", names(models)[model_index])))
  }
  return(models)
}