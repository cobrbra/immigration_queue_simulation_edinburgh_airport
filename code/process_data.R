library(tidyverse)
library(here)

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


check_aircrafts_observed_arrivals <- function(aircrafts_observed_arrivals) {
  if (!is.data.frame(aircrafts_observed_arrivals)) {
    stop("Aircraft schedule should be dataframe.")
  }
  necessary_columns <- c(
    "aircraft_id",
    "scheuled_arrival",
    "aircraft_arrival",
    "arriving_from",
    "airline",
    "plane_type",
    "plane_capacity",
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

process_aircrafts_observed_arrivals <- function(file) {
  # this is a placeholder to be changed
  aircraft_schedule <- data.frame(
    aircraft_id = character(),
    scheuled_arrival = character(),
    aircraft_arrival = character(),
    arriving_from = character(),
    airline = character(),
    plane_type = character(),
    plane_capacity = numeric(),
    n_passengers = numeric(),
    coached = numeric(),
    taxi_time = numeric(),
    walk_time = numeric()
  )
  check_aircrafts_observed_arrivals(aircraft_schedule)
  return(aircraft_schedule)
}

