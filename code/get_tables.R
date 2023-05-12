get_tables <- function(future_aircrafts_arrivals, 
                       observed_aircrafts_arrivals, 
                       core_recommendation,
                       ...) {
  # results <- targets::tar_read(example_results) # Use for debugging, COMMENT WHEN RUNNING TARGETS
  
  tables <- list()
  captions <- list()
  bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
  
  tables$core_recommendation <- matrix(as.integer(tar_read(core_recommendation)), 
                                       nrow = 1, byrow = TRUE) %>%
      as_tibble(.name_repair = ~ as.character(2023:2027))
  captions$core_recommendation <- c("Core recommendation for eGate availability.")
  

  # the code below does pre-processing to produce .tex tables to be used in the report
  
  for (table_index in seq_len(length(tables))) {
    table_name <- names(tables)[table_index]
    if (is.null(captions[[table_name]])) {
      captions[[table_name]] <- ""
    }
    captions[[table_name]] <- paste(captions[[table_name]], 
                                    paste0("\\label{tab:", table_name, "}"))
    xtab <- xtable(tables[[table_index]],
                  caption = captions[[table_name]])
    align(xtab) <- rep("c", ncol(tables[[table_index]]) + 1)
    print(xtab, 
          file = here(paste0("tables/", table_name, ".tex")),
          include.rownames = FALSE,
          sanitize.colnames.function = bold)
  }
  return(tables)
}
