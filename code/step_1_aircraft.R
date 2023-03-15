check_passengers_from_aircraft <- function(passengers_from_aircraft) {
  if (!is.data.frame(passengers_from_aircraft)) {
    stop("Passengers from aircraft should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "flight_id",
    "nationality",
    "coached",
    "aircraft_arrival"
  )
  if (any(!(necessary_columns %in% colnames(passengers_from_aircraft)))) {
    stop(
      paste(c("Passengers from aircraft should contain columns", necessary_columns), 
            collapse = " "))
  }
  
  if (length(passengers_from_aircraft$passenger_id) != length(unique(passengers_from_aircraft$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
}



# get_taxi_time <- function(coached, seed = NULL){
#   
#   if (!is.null(seed)) {set.seed(seed)}
#   
#   n_aircrafts <- length(coached)
#   taxi_time <- numeric(n_aircrafts)
#   coached_taxi_time <- pmax(20, rnorm(n_aircrafts, mean = 300, sd = 120))
#   contact_taxi_time <- pmax(20, rnorm(n_aircrafts, mean = 500, sd = 240))
#   taxi_time <- if_else(coached, coached_taxi_time, contact_taxi_time)
#   
#   return(taxi_time)
#   
# }
# 
# get_walk_time <- function(coached, seed = NULL){
#   # generates minimum walk time
#   
#   if (!is.null(seed)) {set.seed(seed)}
#   
#   n_aircrafts <- length(coached)
#   walk_time <- numeric(n_aircrafts)
#   coached_walk_time <- runif(n_aircrafts, min = 600, max = 900)
#   contact_walk_time <- pmax(180, rpois(n = n_aircrafts, lambda = 300))
#   walk_time = if_else(coached, coached_walk_time, contact_walk_time)
#   
#   return(walk_time)
#   
# }


get_n_passengers <- function(max_passengers, load_factor_mean, 
                             load_factor_sd, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  load_factor <- rnorm(n = length(max_passengers),
                       mean = load_factor_mean, sd = load_factor_sd) 
  load_factor <- pmin(load_factor, 1)
  load_factor <- pmax(load_factor, 0)
  
  n_passengers <- round(max_passengers * load_factor)
  
  return(n_passengers)
}


get_airport_classification <- function(airport_country, 
                                       airport_3letter, 
                                       EU_plus_hubs, 
                                       other_hubs, 
                                       UK_plus_countries,
                                       EU_plus_countries){
  
    airport_classification <- case_when(
      airport_country %in% UK_plus_countries ~ "UK_plus",
      airport_3letter %in% EU_plus_hubs ~ "EU_plus_hub",
      airport_country %in% EU_plus_countries ~ "EU_plus_nonhub",
      airport_3letter %in% other_hubs ~ "other_hub",
      TRUE ~ "other_nonhub")

  return(airport_classification)
}

get_coached_status <- function(flight_id, prob_coached = .15, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  coached <- as.logical(rbinom(1, length(flight_id), prob = prob_coached))
  return(coached)
}


get_nationality_split <- function(aircrafts, EU_plus_hubs, other_hubs, prop_nationality, 
                                  UK_plus_countries, EU_plus_countries,
                                  seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  if(!all(dim(prop_nationality) == c(5, 5))){stop("Prop Nationality needs to be 5x5.")}
  
  n_aircrafts <- dim(aircrafts)[1]
  
  aircrafts_with_airport_class <- aircrafts %>% 
    mutate(airport_classification = get_airport_classification(
      airport_country = dep_country,
      airport_3letter = dep_airport,
      EU_plus_hubs = EU_plus_hubs, 
      other_hubs = other_hubs, 
      UK_plus_countries = UK_plus_countries, 
      EU_plus_countries = EU_plus_countries
    ))
  
  sim_passengers <- function(n_passenegers, airport_classification) {
    sample(x = colnames(prop_nationality)[-1],
           size = n_passenegers,
           replace = TRUE,
           prob = prop_nationality[prop_nationality$Arriving_from == airport_classification][[airport_classification]]
    )
  }
  
  aircrafts_with_simmed_nationality <- aircrafts_with_airport_class %>% 
    mutate(simmed_passengers = map2(.x = n_passengers, 
                                    .y = airport_classification,
                                    .f = ~ sim_passengers(.x, .y))) %>% 
    mutate(n_nat_UK_plus = unlist(map(simmed_passengers, ~ sum(. == "UK_plus"))),
           n_nat_EU_plus = unlist(map(simmed_passengers, ~ sum(. == "EU_plus"))),
           n_nat_other_easy = unlist(map(simmed_passengers, ~ sum(. == "other_easy"))),
           n_nat_other_hard = unlist(map(simmed_passengers, ~ sum(. == "other_hard")))) %>% 
    select(-simmed_passengers)
  
  return(aircrafts_with_simmed_nationality)
}


get_passengers_from_aircrafts <- function(aircrafts, 
                                          EU_plus_hubs,
                                          other_hubs,
                                          prop_nationality,
                                          UK_plus_countries,
                                          EU_plus_countries,
                                          load_factor_mean,
                                          load_factor_sd,
                                          seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  aircrafts_with_passengers <- aircrafts %>% 
    mutate(n_passengers = get_n_passengers(max_passengers, load_factor_mean, load_factor_sd)) %>% 
    mutate(coached = if_else(is.na(coached), get_coached_status(flight_id), coached)) %>% 
    get_nationality_split(EU_plus_hubs = tar_read(EU_plus_hubs),
                          other_hubs = tar_read(other_hubs), 
                          prop_nationality = tar_read(nationality_props), 
                          UK_plus_countries = tar_read(UK_plus_countries), 
                          EU_plus_countries = tar_read(EU_plus_countries))
  
  n_passengers_aircraft <- aircrafts_with_passengers$n_passengers
  n_passengers <- sum(n_passengers_aircraft)

  passengers <- data.frame(
    
    passenger_id = paste0("P", str_pad(seq_len(n_passengers), 10, pad = "0"),
                          recycle0 = TRUE),
    flight_id = rep(aircrafts_with_passengers$flight_id, n_passengers_aircraft),
    nationality = aircrafts_with_passengers %>% 
      select(n_nat_UK_plus, n_nat_EU_plus, n_nat_other_easy, n_nat_other_hard) %>% 
      pivot_longer(cols = everything(), names_to = "nat", values_to = "n_nat") %>% 
      {rep(.$nat, .$n_nat)},
    airport_classification = rep(aircrafts_with_passengers$airport_classification, n_passengers_aircraft),
    aircraft_arrival = rep(aircrafts_with_passengers$t_actual, n_passengers_aircraft),
    coached = rep(aircrafts_with_passengers$coached, n_passengers_aircraft)
  )
  
  check_passengers_from_aircraft(passengers)
  return(passengers)
}
