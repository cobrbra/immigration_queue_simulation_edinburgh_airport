library(tidyverse)
library(here)
library(readxl)

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
  aircraft_cols <- c("aircraft_name", "long_code", "short_code", 
                     "max_passengers", "country_of_origin")
  read_delim(file, 
             delim = ";",
             col_names = aircraft_cols, 
             na = "\\N")
}

necessary_aircrafts_arrivals_columns <- c(
  "flight_id",
  "airport_classification",
  "aircraft_datetime_int",
  "aircraft_time_int",
  "aircraft_datetime_posix",
  "aircraft_date_posix",
  "sched_aircraft_datetime_int",
  "sched_aircraft_time_int",
  "sched_aircraft_datetime_posix",
  "sched_aircraft_date_posix",
  "max_passengers",
  "n_passengers",
  "coached",
  "n_nat_UKIE",
  "n_nat_EU_plus",
  "n_nat_other_easy",
  "n_nat_other_hard"
)

necessary_complete_aircrafts_arrivals_columns <- c(
  "flight_id",
  "sched_aircraft_datetime_int",
  "max_passengers"
)

check_aircrafts_arrivals <- function(observed_aircrafts_arrivals, 
                                     complete = FALSE) {
  if (!is.data.frame(observed_aircrafts_arrivals)) {
    stop("Aircraft schedule should be dataframe.")
  }
  
  necessary_columns_missing <- !(necessary_aircrafts_arrivals_columns %in% 
                                   colnames(observed_aircrafts_arrivals))
  if (any(necessary_columns_missing)) {
    stop(
      paste(c("Aircraft arrivals should contain columns",
              necessary_aircrafts_arrivals_columns[necessary_columns_missing]), 
            collapse = " ")
    )
  }
  
  necessary_complete_columns_missing <- !(necessary_complete_aircrafts_arrivals_columns %in% 
                                        colnames(observed_aircrafts_arrivals %>% keep(~all(!is.na(.x)))))
  if (any(necessary_complete_columns_missing)) {
    stop(
      paste(c("Aircraft arrivals should contain complete columns:",
              necessary_complete_aircrafts_arrivals_columns[necessary_complete_columns_missing]),
            collapse = " ")
    )
  }
  
  necessary_columns_not_complete <- !(necessary_aircrafts_arrivals_columns %in% 
                                        colnames(observed_aircrafts_arrivals %>% keep(~all(!is.na(.x)))))
  if (complete & any(necessary_columns_not_complete)) {
    stop(
      paste(c("Complete aircraft arrivals should contain complete columns:",
              necessary_aircrafts_arrivals_columns[necessary_columns_not_complete]),
            collapse = " ")
    )
  }
}

get_datetime_alternates <- function(events_with_datetime_int, 
                                    column_prefixes = c("aircraft")) {
  for (column_prefix in column_prefixes) {
    events_with_datetime_int[[paste0(column_prefix, "_datetime_posix")]] <- 
      events_with_datetime_int[[paste0(column_prefix, "_datetime_int")]] %>% 
        as.POSIXct(origin = "1970-01-01 00:00:00")
    events_with_datetime_int[[paste0(column_prefix, "_date_posix")]] <- 
      events_with_datetime_int[[paste0(column_prefix, "_datetime_posix")]] %>% 
        as.Date()
    events_with_datetime_int[[paste0(column_prefix, "_time_int")]] <- 
      events_with_datetime_int[[paste0(column_prefix, "_datetime_int")]] -
        86400 * as.numeric(
          events_with_datetime_int[[paste0(column_prefix, "_date_posix")]] - 
            as.Date('1970-01-01 00:00:00')
        )
  }
  return(events_with_datetime_int)
}

simulate_aircrafts_arrivals <- function(n_aircrafts = 5, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  
  aircrafts <- data.frame(
    flight_id = paste0("F", str_pad(1:n_aircrafts, 10, pad = "0")),
    dep_country = c("UK", rep("NETHERLANDS", n_aircrafts - 1)),
    dep_airport = c("LGW", rep("AMS", n_aircrafts - 1)),
    airport_classification = c("UK_plus", rep("EU_plus_hub", n_aircrafts - 1)),
    ac_type = "A320",
    des_rwy = 1,
    sched_aircraft_datetime_int = 800000 + cumsum(rexp(n = n_aircrafts, rate = 1e-3)),
    max_passengers = 150,
    n_passengers = round(runif(n_aircrafts, 100, 150)),
    coached = sample(c(TRUE, FALSE), size = n_aircrafts, replace = TRUE),
    n_nat_UKIE = rep(NA, n_aircrafts),
    n_nat_EU_plus = rep(NA, n_aircrafts),
    n_nat_other_easy = rep(NA, n_aircrafts),
    n_nat_other_hard = rep(NA, n_aircrafts)
  ) %>% mutate(
    aircraft_datetime_int = sched_aircraft_datetime_int + rexp(n = n_aircrafts, rate = 5e-3)
  ) %>% get_datetime_alternates(column_prefixes = c("aircraft", "sched_aircraft"))
  
  check_aircrafts_arrivals(aircrafts)
  return(aircrafts)
}


process_aircrafts_arrivals <- function(folder_name, 
                                                airports_reference,
                                                aircrafts_reference) {
  years_not_split_by_runway <- c(" 2019", " 2022")
  years_split_by_runway <- c(" 2020", " 2021")
  files_not_split_by_runway <- map2(
    .x = rep(1:4, length(years_not_split_by_runway)),
    .y = rep(years_not_split_by_runway, each = 4),
    .f = ~ paste0(folder_name, 
                  "/Q", .x, .y, ".xlsx")
    )
  files_split_by_runway <- map2(
    .x = rep(1:4, length(years_split_by_runway)),
    .y = rep(years_split_by_runway, each = 4),
    .f = ~ paste0(folder_name, 
                  "/Q", .x, .y)
  )
  files_split_by_runway <- paste0(
    rep(files_split_by_runway, each = 2),
    rep(c(" R06.xlsx", " R24.xlsx"), times = length(files_split_by_runway))
  )
  files <- c(files_not_split_by_runway, files_split_by_runway)
  
  # bind together files for aircraft arrivals
  observed_aircrafts_arrivals <- map(files,
                                     ~ read_xlsx(path = .)) %>% 
    bind_rows() %>% 
    as.data.frame() %>% 
    select(-c(des_time,t)) %>% 
    drop_na() %>% 
    mutate(sched_aircraft_datetime_int = as.numeric(t_sched),
           aircraft_datetime_int = as.numeric(t_actual)) %>% 
    get_datetime_alternates(column_prefixes = c("sched_aircraft", "aircraft"))
  
  # bind together with airports data
  observed_aircrafts_arrivals <- observed_aircrafts_arrivals %>% 
    inner_join(airports_reference, by = c("dep_af" = "icao_code")) %>% 
    mutate(country = if_else(country == "ENGALND", "ENGLAND", country))
  
  # bind together with aircraft data
  observed_aircrafts_arrivals <- observed_aircrafts_arrivals %>% 
    inner_join(aircrafts_reference %>% 
                 drop_na(long_code, max_passengers) %>% 
                 mutate(max_passengers = as.numeric(max_passengers)) %>% 
                 select(long_code, max_passengers) %>% 
                 group_by(long_code) %>% 
                 summarise(max_passengers = mean(max_passengers)), 
               by = c("ac_type" = "long_code"))
  
  observed_aircrafts_arrivals <- observed_aircrafts_arrivals %>% 
    mutate(flight_id = id,
           dep_country = country,
           dep_airport = iata_code,
           airport_classification = rep(NA, nrow(observed_aircrafts_arrivals)),
           n_passengers = rep(NA, nrow(observed_aircrafts_arrivals)),
           coached = rep(NA, nrow(observed_aircrafts_arrivals)),
           walk_time = rep(NA, nrow(observed_aircrafts_arrivals)),
           n_nat_UKIE = rep(NA, nrow(observed_aircrafts_arrivals)),
           n_nat_EU_plus = rep(NA, nrow(observed_aircrafts_arrivals)),
           n_nat_other_easy = rep(NA, nrow(observed_aircrafts_arrivals)),
           n_nat_other_hard = rep(NA, nrow(observed_aircrafts_arrivals))) %>% 
    select("flight_id",
           "dep_country",
           "dep_airport",
           "ac_type",
           "airport_classification",
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
           "walk_time",
           "n_nat_UKIE",
           "n_nat_EU_plus",
           "n_nat_other_easy",
           "n_nat_other_hard")
  
  check_aircrafts_arrivals(observed_aircrafts_arrivals)
  return(observed_aircrafts_arrivals)
}

process_future_aircrafts_arrivals <- function(file) {
  future_aircraft_arrivals <- 3:7 %>% 
    map(~read_xlsx(file, 
                   sheet = .)) %>% 
    bind_rows() %>% 
    mutate(flight_date = format(`flight date`, format = "%Y-%m-%d"),
           arrival_time = format(`arrival time`, format  = "%H:%M:%S"),
           n_passengers = passengers) %>% 
    mutate(sched_aircraft_datetime_int = as.numeric(as.POSIXct(paste(flight_date, arrival_time)))) %>% 
    get_datetime_alternates(column_prefixes = c("sched_aircraft")) 
  
  n_future_flights <- nrow(future_aircraft_arrivals)
  
  future_aircraft_arrivals <- future_aircraft_arrivals %>%
    mutate(flight_id = paste0("F", str_pad(1:n_future_flights, 10, pad = "0")),
           dep_country = rep(NA, n_future_flights),
           dep_airport = rep(NA, n_future_flights),
           airport_classification = rep(NA, n_future_flights),
           ac_type = rep(NA, n_future_flights),
           aircraft_datetime_int = rep(NA, n_future_flights),
           aircraft_time_int = rep(NA, n_future_flights),
           aircraft_datetime_posix = rep(NA, n_future_flights),
           aircraft_date_posix = rep(NA, n_future_flights),
           des_rwy = rep(NA, n_future_flights),
           max_passengers = n_passengers,
           coached = rep(NA, n_future_flights),
           n_nat_UKIE = rep(NA, n_future_flights),
           n_nat_EU_plus = rep(NA, n_future_flights),
           n_nat_other_easy = rep(NA, n_future_flights),
           n_nat_other_hard = rep(NA, n_future_flights)) %>% 
    select(flight_id, dep_country, dep_airport, airport_classification, ac_type,
           aircraft_datetime_int,aircraft_time_int, aircraft_datetime_posix, aircraft_date_posix,
           sched_aircraft_datetime_int, sched_aircraft_time_int, sched_aircraft_datetime_posix, sched_aircraft_date_posix,
           des_rwy, max_passengers, n_passengers, coached, n_nat_UKIE, n_nat_EU_plus, n_nat_other_easy, n_nat_other_hard)
  
  check_aircrafts_arrivals(future_aircraft_arrivals)
  return(future_aircraft_arrivals)
}

filter_arrivals_for_equivalent_weeks <- function(observed_aircrafts_arrivals, UK_plus_countries) {
  observed_aircrafts_arrivals %>% 
    mutate(Year = format(sched_aircraft_datetime_posix, format = "%Y")) %>% 
    filter(((sched_aircraft_date_posix >= as.Date("2022-07-11")) &
              (sched_aircraft_date_posix <= as.Date("2022-07-17"))) |
             ((sched_aircraft_date_posix >= as.Date("2021-07-12")) &
              (sched_aircraft_date_posix <= as.Date("2021-07-18"))) |
             ((sched_aircraft_date_posix >= as.Date("2020-07-13")) &
              (sched_aircraft_date_posix <= as.Date("2020-07-17"))) |
             ((sched_aircraft_date_posix >= as.Date("2019-07-08")) &
              (sched_aircraft_date_posix <= as.Date("2019-07-14")))) %>% 
    filter(!(dep_country %in% UK_plus_countries)) %>% 
    return()
}

process_future_coached_levels <- function(file) {
  future_coached_levels <- read_xlsx(file, 
            range = "assumptions!B21:G22",
            col_names = c("coached_status", "2023", "2024", "2025", "2026", "2027")) %>% 
    mutate(coached_status = if_else(coached_status == "% of arriving flights coached", "Coached", "Contact")) %>% 
    pivot_longer(cols = - coached_status, names_to = "Year", values_to = "Percent")
  
  return(future_coached_levels)
}





process_aircrafts_quantiles <- function(aircrafts, 
                                        EU_plus_hubs, other_hubs, UK_plus_countries,
                                        EU_plus_countries, load_factor, 
                                        seed = NULL
){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  ac <- aircrafts
  
  ac <- ac[!ac$dep_country %in% targets::tar_read(UK_plus_countries), ]
  
  ac_class <- get_airport_classification(airport_country = ac$dep_country, 
                                         airport_3letter = ac$dep_airport, 
                                         EU_plus_hubs = EU_plus_hubs, 
                                         other_hubs = other_hubs,
                                         UK_plus_countries =  UK_plus_countries,
                                         EU_plus_countries = EU_plus_countries)
  
  ac_pass <- get_n_passengers(max_passengers = ac$max_passengers, 
                              load_factor = load_factor,
                              seed = seed)
  
  
  quants_pass <- quantile(ac_pass, probs = seq(from = 0.2, to = 1, by = 0.2))
  
  ac_pass_quantile <- sapply(lapply(ac_pass, FUN = `>`, quants_pass), sum) + 1
  
  tab_ac <- table(ac_class, ac_pass_quantile)
  
  tab_ac_prop <- prop.table(tab_ac, margin = 2)
  
  res_list <- list(quantiles = quants_pass, table = tab_ac_prop)
  
  return(res_list)
}


simulate_future_airport_classification <- function(aircrafts, quantile_list, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  n_aircrafts <- dim(aircrafts)[1]
  quantiles <- quantile_list[[1]]
  table_for_sampling <- quantile_list[[2]]
  
  ap_classifications <- rownames(table_for_sampling)
  
  airport_classification <- numeric(n_aircrafts)
  
  for(i in seq_len(n_aircrafts)) {
    
    selected_quantile <- sum(aircrafts$n_passengers[i] > quantiles) + 1
    airport_classification[i] <- sample(x = ap_classifications, size = 1, prob = table_for_sampling[, selected_quantile])
    
  }
  
  return(aircrafts %>% mutate(airport_classification = airport_classification))
  
}




