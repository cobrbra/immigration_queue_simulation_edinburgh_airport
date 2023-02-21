library(tidyverse)
library(here)

get_tables <- function(results) {
  tables <- list()
  # tables$table_1 <- ...
  
  for (table_index in seq_len(length(tables))) {
    table <- tables[table_index]
    table_name <- names(tables)[table_index]
    write_csv(table, paste0(here("tables/", table_name)))
  }
  return(tables)
}