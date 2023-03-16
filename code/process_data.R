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


check_aircrafts_arrivals <- function(aircrafts_observed_arrivals) {
  if (!is.data.frame(aircrafts_observed_arrivals)) {
    stop("Aircraft schedule should be dataframe.")
  }
  necessary_columns <- c(
    "flight_id",
    "dep_country",
    "dep_airport",
    "ac_type",
    "aircraft_datetime_int",
    "aircraft_time_int",
    "aircraft_datetime_posix",
    "aircraft_date_posix",
    "sched_aircraft_datetime_int",
    "sched_aircraft_time_int",
    "sched_aircraft_datetime_posix",
    "sched_aircraft_date_posix",
    "des_rwy",
    "max_passengers",
    "n_passengers",
    "coached"
  )
  if (any(!(necessary_columns %in% colnames(aircrafts_observed_arrivals)))) {
    stop(
      paste(c("Aircraft Observed Arrivals should contain columns", necessary_columns), 
            collapse = " "))
  }
}


simulate_aircrafts_arrivals <- function(n_aircrafts = 5, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  
  aircrafts <- data.frame(
    flight_id = paste0("F", str_pad(1:n_aircrafts, 10, pad = "0")),
    dep_country = c("UK", rep("NETHERLANDS", n_aircrafts - 1)),
    dep_airport = c("LGW", rep("AMS", n_aircrafts - 1)),
    ac_type = "A320",
    des_rwy = 1,
    sched_aircraft_datetime_int = 800000 + cumsum(rexp(n = n_aircrafts, rate = 1e-3)),
    max_passengers = 150,
    n_passengers = round(runif(n_aircrafts, 100, 150)),
    coached = sample(c(TRUE, FALSE), size = n_aircrafts, replace = TRUE)
  ) %>% mutate(
    aircraft_datetime_int = sched_aircraft_datetime_int + rexp(n = n_aircrafts, rate = 5e-3)
  ) %>% mutate(
    sched_aircraft_datetime_posix = as.POSIXct(sched_aircraft_datetime_int, origin = '1970-01-01 00:00:00'),
    aircraft_datetime_posix = as.POSIXct(aircraft_datetime_int, origin = '1970-01-01 00:00:00')
  ) %>% mutate(
    sched_aircraft_date_posix = as.Date(sched_aircraft_datetime_posix),
    aircraft_date_posix = as.Date(aircraft_datetime_posix)
  ) %>% mutate(
    sched_aircraft_time_int = sched_aircraft_datetime_int - as.numeric(sched_aircraft_date_posix - as.Date('1970-01-01 00:00:00')) * 86400,
    aircraft_time_int = aircraft_datetime_int - as.numeric(aircraft_date_posix - as.Date('1970-01-01 00:00:00')) * 86400
  )
  
  check_aircrafts_arrivals(aircrafts)
  return(aircrafts)
}

process_aircrafts_arrivals <- function(folder_name, 
                                                airports_reference,
                                                aircrafts_reference) {
  files <- map2(.x = rep(1:4, 2),
                .y = rep(c(" 2019", " 2022"), each = 4),
                .f = ~ paste0(folder_name, 
                      "/Q", .x, .y, ".xlsx"))
  
  # bind together files for aircraft arrivals
  aircrafts_observed_arrivals <- map(files,
                                     ~ read_xlsx(path = .)) %>% 
    bind_rows() %>% 
    as.data.frame() %>% 
    select(-c(des_time,t)) %>% 
    drop_na() %>% 
    mutate(sched_aircraft_datetime_posix = t_sched,
           aircraft_datetime_posix = t_actual,
           sched_aircraft_date_posix = as.Date(sched_aircraft_datetime_posix),
           aircraft_date_posix = as.Date(aircraft_datetime_posix),
           sched_aircraft_datetime_int = as.numeric(t_sched),
           aircraft_datetime_int = as.numeric(aircraft_datetime_posix)) %>% 
    mutate(sched_aircraft_time_int = sched_aircraft_datetime_int - 86400 * as.numeric(sched_aircraft_date_posix),
           aircraft_time_int = aircraft_datetime_int - 86400 * as.numeric(aircraft_date_posix))
  
  # bind together with airports data
  aircrafts_observed_arrivals <- aircrafts_observed_arrivals %>% 
    inner_join(airports_reference, by = c("dep_af" = "icao_code")) %>% 
    mutate(country = if_else(country == "ENGALND", "ENGLAND", country))
  
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
    mutate(flight_id = id,
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
    select("flight_id",
           "dep_country",
           "dep_airport",
           "ac_type",
           "sched_aircraft_datetime_int",
           "aircraft_datetime_int",
           "sched_aircraft_datetime_posix",
           "aircraft_datetime_posix",
           "sched_aircraft_date_posix",
           "aircraft_date_posix",
           "sched_aircraft_time_int",
           "aircraft_time_int",
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
  
  check_aircrafts_arrivals(aircrafts_observed_arrivals)
  return(aircrafts_observed_arrivals)
}
