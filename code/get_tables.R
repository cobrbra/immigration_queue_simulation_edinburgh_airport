library(tidyverse)
library(here)
library(xtable)

get_tables <- function(results) {

  tables <- list()
  captions <- list()
  
  tables$mtcars_full <- results$mt_table # read_csv("processed_data/example_processed_data.csv") # 
  captions$mtcars_full <- "MTCars dataset restricted to observations with <20 miles per gallon"
  
  # tables$table_1 <- ...
  # add code to generate new tables here
  
  # the code below does pre-processing to produce .tex tables to be used in the report
  
  for (table_index in seq_len(length(tables))) {
    table_name <- names(tables)[table_index]
    if (is.null(captions[[table_name]])) {
      captions[[table_name]] <- ""
    }
    captions[[table_name]] <- paste(captions[[table_name]], 
                                    paste0("\\label{tab:", table_name, "}"))
    print(captions[[table_name]])
    print(xtable(tables[[table_index]],
                 caption = captions[[table_name]]), 
          file = paste0(here("tables/"), table_name, ".tex"))
  }
  return(tables)
}