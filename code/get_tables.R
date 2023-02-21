library(tidyverse)
library(here)
library(xtable)

get_tables <- function(results) {
  tables <- list()
  
  tables$mtcars_full <- results$mt_table
  
  # tables$table_1 <- ...
  # add code to generate new tables here
  
  for (table_index in seq_len(length(tables))) {
    print(xtable(tables[[table_index]]), 
          file = paste0(here("tables/", names(tables)[table_index]), ".tex"))
  }
  return(tables)
}