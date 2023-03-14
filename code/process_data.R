library(tidyverse)
library(here)
library(readxl)

get_processed_data <- function(file) {
  
  
  processed_data <- read_csv(file) %>%
    filter(mpg < 20)
  write_csv(processed_data, here("processed_data/example_processed_data.csv"))
  
  
  return(processed_data)
}


process_airports <- function(file) {
  airports_cols <- c("icao_code", "iata_code", "airport_name", "city", "country",
                   "lat_degrees", "lat_minutes", "lat_seconds", "lat_direction",
                   "lon_degrees", "lon_minutes", "lon_seconds", "lon_direction",
                   "altitude", "lat_dec", "lon_dec")
  processed_data <- read_delim(file, 
                               delim = ":",
                               col_names = airports_cols) 
  

}

process_aircrafts <- function(file) {
  aircraft_cols <- c("aircraft_name", "long_code", "short_code", "max_passengers", "country_of_origin")
  read_delim(file, 
             delim = ";",
             col_names = aircraft_cols)
}


check_aircrafts_observed_arrivals <- function(aircrafts_observed_arrivals) {
  if (!is.data.frame(aircrafts_observed_arrivals)) {
    stop("Aircraft schedule should be dataframe.")
  }
  necessary_columns <- c(
    "aircraft_id",
    "dep_country",
    "dep_airport",
    "ac_type",
    "t_sched",
    "t_actual",
    "sched_arrival_datetime",
    "actual_arrival_datetime",
    "sched_arrival_date",
    "actual_arrival_date",
    "sched_arrival_time",
    "actual_arrival_time",
    "des_rwy",
    "max_passengers",
    "n_passengers",
    "coached",
    "taxi_time",
    "walk_time"
  )
  if (any(!(necessary_columns %in% colnames(aircrafts_observed_arrivals)))) {
    stop(
      paste(c("Aircraft Observed Arrivals should contain columns", necessary_columns), 
            collapse = " "))
  }
}

process_aircrafts_observed_arrivals <- function(folder_name, 
                                                airports_reference,
                                                aircrafts_reference) {
  files <- map(1:4, 
              ~ paste0(folder_name, 
                      "/Q", ., " 2019.xlsx"))
  
  # bind together files for aircraft arrivals
  aircrafts_observed_arrivals <- map(files,
                                     ~ read_xlsx(path = .)) %>% 
    bind_rows() %>% 
    as.data.frame() %>% 
    select(-c(des_time,t)) %>% 
    drop_na() %>% 
    mutate(sched_arrival_datetime = t_sched,
           actual_arrival_datetime = t_actual,
           sched_arrival_date = as.Date(t_sched),
           actual_arrival_date = as.Date(t_actual),
           t_sched = as.numeric(t_sched),
           t_actual = as.numeric(t_actual)) %>% 
    mutate(sched_arrival_time = t_sched - 86400 * as.numeric(sched_arrival_date),
           actual_arrival_time = t_actual - 86400 * as.numeric(actual_arrival_date))
  
  # bind together with airports data
  aircrafts_observed_arrivals <- aircrafts_observed_arrivals %>% 
    inner_join(airports_reference, by = c("dep_af" = "icao_code"))
  
  # bind together with aircraft data
  aircrafts_observed_arrivals <- aircrafts_observed_arrivals %>% 
    inner_join(aircrafts_reference %>% 
                 mutate(max_passengers = as.integer(max_passengers)) %>% 
                 select(long_code, max_passengers) %>% 
                 filter(long_code != "\\N") %>% 
                 drop_na() %>% 
                 group_by(long_code) %>% 
                 summarise(max_passengers = mean(max_passengers)), 
               by = c("ac_type" = "long_code"))
  
  aircrafts_observed_arrivals <- aircrafts_observed_arrivals %>% 
    mutate(aircraft_id = rep(NA, nrow(aircrafts_observed_arrivals)),
           dep_country = country,
           dep_airport = iata_code,
           n_passengers = rep(NA, nrow(aircrafts_observed_arrivals)),
           coached = rep(NA, nrow(aircrafts_observed_arrivals)),
           taxi_time = rep(NA, nrow(aircrafts_observed_arrivals)),
           walk_time = rep(NA, nrow(aircrafts_observed_arrivals)),
           n_nat_UKIE = rep(NA, nrow(aircrafts_observed_arrivals)),
           n_nat_EU_plus = rep(NA, nrow(aircrafts_observed_arrivals)),
           n_nat_other_easy = rep(NA, nrow(aircrafts_observed_arrivals)),
           n_nat_other_hard = rep(NA, nrow(aircrafts_observed_arrivals))) %>% 
    select("aircraft_id",
           "dep_country",
           "dep_airport",
           "ac_type",
           "t_sched",
           "t_actual",
           "sched_arrival_datetime",
           "actual_arrival_datetime",
           "sched_arrival_date",
           "actual_arrival_date",
           "sched_arrival_time",
           "actual_arrival_time",
           "des_rwy",
           "max_passengers",
           "n_passengers",
           "coached",
           "taxi_time",
           "walk_time",
           "n_nat_UKIE",
           "n_nat_EU_plus",
           "n_nat_other_easy",
           "n_nat_other_hard")
  
  check_aircrafts_observed_arrivals(aircrafts_observed_arrivals)
  return(aircrafts_observed_arrivals)
}

get_time_from_numeric <- function(time_numeric_vector) {
  time_character_vector <- format(as.POSIXct(time_numeric_vector, 
                                             origin = '1970-01-01'), 
                                  format = '%H:%M')
  
  return(time_character_vector)
}

