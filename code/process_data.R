library(tidyverse)
library(here)

get_processed_data <- function(file) {
  
  
  processed_data <- read_csv(file) %>%
    filter(mpg < 20)
  write_csv(processed_data, here("processed_data/example_processed_data.csv"))
  
  
  return(processed_data)
}


process_planes_data <- function(file) {
  planes_cols <- c("icao_code", "iata_code", "airport_name", "city", "country",
                   "lat_degrees", "lat_minutes", "lat_seconds", "lat_direction",
                   "lon_degrees", "lon_minutes", "lon_seconds", "lon_direction",
                   "altitude", "lat_dec", "lon_dec")
  processed_data <- read_delim(file, # read_delim(here("raw_data/planes/GlobalAirportDatabase.txt"), # 
                               delim = ":",
                               col_names = planes_cols) 
  write_csv(processed_data, here("processed_data/planes_data.csv"))

}