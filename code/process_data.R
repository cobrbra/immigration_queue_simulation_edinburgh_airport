library(tidyverse)
library(here)

get_processed_data <- function(file) {
  
  
  processed_data <- read_csv(file) %>%
    filter(mpg < 20)
  write_csv(processed_data, here("processed_data/example_processed_data.csv"))
  
  
  return(processed_data)
}


process_aircraft <- function(file) {
  planes_cols <- c("icao_code", "iata_code", "airport_name", "city", "country",
                   "lat_degrees", "lat_minutes", "lat_seconds", "lat_direction",
                   "lon_degrees", "lon_minutes", "lon_seconds", "lon_direction",
                   "altitude", "lat_dec", "lon_dec")
  processed_data <- read_delim(file, # read_delim(here("raw_data/aircraft/GlobalAirportDatabase.txt"), # 
                               delim = ":",
                               col_names = planes_cols) 
  write_csv(processed_data, here("processed_data/planes_data.csv"))

}


check_aircraft_schedule <- function(aircraft_schedule) {
  if (!is.data.frame(aircraft_schedule)) {
    stop("Aircraft schedule should be dataframe.")
  }
  necessary_columns <- c(
    "aircraft_id",
    "scheuled_arrival",
    "actual_arrival",
    "arriving_from",
    "airline",
    "plane_type",
    "plane_capacity",
    "n_passengers",
    "coached"
  )
  if (any(!(necessary_columns %in% colnames(aircraft_schedule)))) {
    stop(
      paste(c("Aircraft schedule should contain columns", necessary_columns), 
            collapse = " "))
  }
}
process_aircraft_schedule <- function(file) {
  aircraft_schedule <- data.frame(
    aircraft_id = character(),
    scheuled_arrival = character(),
    actual_arrival = character(),
    arriving_from = character(),
    airline = character(),
    plane_type = character(),
    plane_capacity = numeric(),
    n_passengers = numeric(),
    coached = numeric()
  )
  check_aircraft_schedule(aircraft_schedule)
  return(aircraft_schedule)
}

