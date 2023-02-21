library(tidyverse)
library(here)

get_tables <- function(results) {
  tables <- list()
  
  # tables$table_1 <- ...
  # add code to generate new tables here
  
  for (table_index in seq_len(length(tables))) {
    write_csv(tables[[table_index]], 
              paste0(here("tables/", names(tables)[table_index])))
  }
  return(tables)
}