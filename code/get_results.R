library(tidyverse)
library(here)

get_results <- function(processed_data, simulated_data, models) {
  results <- list()
  
  results$mt_table <- processed_data
  
  # results$results_1 <- ...
  # insert more code that generates results here
  
  for (result_index in seq_len(length(results))) {
    write_csv(results[[result_index]], 
              here(paste0("results/", names(results)[result_index])))
  }
  return(results)
}