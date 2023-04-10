library(tidyverse)
library(here)
library(xtable)

get_tables <- function(future_aircrafts_arrivals, 
                       aircrafts_observed_arrivals, 
                       ...) {
  # results <- targets::tar_read(example_results) # Use for debugging, COMMENT WHEN RUNNING TARGETS
  
  tables <- list()
  captions <- list()
  bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
  
  
  tables$anticipated_schedule <- future_aircrafts_arrivals %>% 
    mutate(Year = format(sched_aircraft_datetime_posix, "%Y")) %>% 
    group_by(Year) %>% 
    nest() %>% 
    mutate(
      `Number of Flights` = unlist(map(data, nrow)),
      `Start Date` = unlist(map(data, 
                                ~ format(min(.$sched_aircraft_date_posix),
                                         format = "%d/%m"))),
      `End Date` = unlist(map(data,
                             ~ format(max(.$sched_aircraft_date_posix),
                                      format = "%d/%m")))) %>% 
    select(Year, `Start Date`, `End Date`, `Number of Flights`) 
  captions$anticipated_schedule <- "Anticipated flight schedule, 2023-2027."
  
  tables$observed_schedule <- (aircrafts_observed_arrivals) %>%
    mutate(Year = as.integer(format(sched_aircraft_datetime_posix, "%Y"))) %>% 
    group_by(Year) %>% 
    nest() %>% 
    mutate(
      `Number of Flights` = unlist(map(data, nrow)),
      `Start Date` = unlist(map(data, 
                                ~ format(min(.$sched_aircraft_date_posix),
                                         format = "%d/%m"))),
      `End Date` = unlist(map(data,
                              ~ format(max(.$sched_aircraft_date_posix),
                                       format = "%d/%m")))) %>% 
    select(Year, `Start Date`, `End Date`, `Number of Flights`) %>% 
    arrange(Year)
  captions$observed_schedule <- "Hisorical arrivals for Edinburgh Airport"
  
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
          file = paste0(here("tables/"), table_name, ".tex"),
          include.rownames = FALSE,
          sanitize.colnames.function = bold)
  }
  return(tables)
}