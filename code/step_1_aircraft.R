check_passengers_after_aircrafts <- function(passengers_after_aircraft) {
  if (!is.data.frame(passengers_after_aircraft)) {
    stop("Passengers from aircraft should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "flight_id",
    "nationality",
    "airport_classification",
    "coached",
    "aircraft_datetime_int",
    "aircraft_time_int",
    "aircraft_datetime_posix",
    "aircraft_date_posix"
  )
  if (any(!(necessary_columns %in% colnames(passengers_after_aircraft)))) {
    stop(
      paste(c("Passengers from aircraft should contain columns", necessary_columns), 
            collapse = " "))
  }
  
  if (length(passengers_after_aircraft$passenger_id) != length(unique(passengers_after_aircraft$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
}

get_airport_classification <- function(airport_country, 
                                       airport_3letter, 
                                       hubs,
                                       countries){
  
  airport_classification <- case_when(
    airport_country %in% countries$UKIE ~ "UKIE",
    airport_3letter %in% hubs$EU_plus ~ "EU_plus_hub",
    airport_country %in% countries$EU_plus ~ "EU_plus_nonhub",
    airport_3letter %in% hubs$other ~ "other_hub",
    TRUE ~ "other_nonhub")
  
  return(airport_classification)
}

sim_airport_classification <- function(n_passengers, quantile_list, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  n_aircrafts <- length(n_passengers)
  quantiles <- quantile_list$quantiles
  table_for_sampling <- quantile_list$table
  
  ap_classifications <- rownames(table_for_sampling)
  
  airport_classification <- numeric(n_aircrafts)
  
  selected_quantiles <- map(n_passengers, ~ sum(. > quantiles) + 1)
  airport_classification <- map(selected_quantiles, 
                                ~sample(x = ap_classifications, 
                                        size = 1, 
                                        prob = table_for_sampling[, .])) %>% 
    unlist()
  
  return(airport_classification)
  
}

sim_coached_status <- function(aircrafts_arrivals, coached_levels, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  aircrafts_arrivals <- aircrafts_arrivals %>% 
    mutate(year = format(sched_aircraft_datetime_posix, "%Y")) %>% 
    group_by(year) %>% 
    nest() %>% 
    inner_join(coached_levels, by = "year") %>% 
    unnest(data) %>% 
    ungroup() %>% 
    mutate(coached = as.logical(rbinom(nrow(.), 1, prob = prob_coached))) %>% 
    select(-c(year, prob_coached))
  
  return(aircrafts_arrivals)
}

sim_delay_times <- function(flight_id, 
                            delay_dist,
                            seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  n_flights <- length(flight_id)
  simulated_delay_status <- sample(c("delayed", "on-time", "early"), 
                                   size = n_flights,
                                   prob = c(delay_dist$prop_delayed,
                                            delay_dist$prop_on_time,
                                            delay_dist$prop_early),
                                   replace = TRUE)
  simulated_delay <- case_when(
    simulated_delay_status == "early" ~ - delay_dist$on_time_window/2 - 
      rexp(n = n_flights, rate = 1/(delay_dist$mean_early_time - delay_dist$on_time_window/2)),
    simulated_delay_status == "on-time" ~ 
      runif(n = n_flights, - delay_dist$on_time_window/2, delay_dist$on_time_window/2),
    simulated_delay_status == "delayed" ~ delay_dist$on_time_window/2 + 
      rexp(n = n_flights, rate = 1/(delay_dist$mean_delay_time - delay_dist$on_time_window/2))
  ) 
  return(simulated_delay)
}

sim_n_passengers <- function(max_passengers, load_factor, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  load_factors <- rnorm(n = length(max_passengers),
                       mean = load_factor$mean, sd = load_factor$sd) 
  load_factors <- pmin(load_factors, 1)
  load_factors <- pmax(load_factors, 0)
  
  n_passengers <- round(max_passengers * load_factors)
  
  return(n_passengers)
}

sim_nationality_split <- function(aircrafts_arrivals, hubs, prop_nationality, 
                                  countries, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  if(!all(dim(prop_nationality) == c(5, 5))){stop("prop_nationality needs to be 5x5.")}
  
  sim_passengers <- function(n_passenegers, airport_classification) {
    sample(x = colnames(prop_nationality)[-1],
           size = n_passenegers,
           replace = TRUE,
           prob = prop_nationality[prop_nationality$Arriving_from == airport_classification, ][-1]
    )
  }
  
  aircrafts_with_simmed_nationality <- aircrafts_arrivals %>% 
    mutate(simmed_passengers = map2(.x = n_passengers, 
                                    .y = airport_classification,
                                    .f = ~ sim_passengers(.x, .y))) %>% 
    mutate(n_nat_UKIE = unlist(map(simmed_passengers, ~ sum(. == "UKIE"))),
           n_nat_EU_plus = unlist(map(simmed_passengers, ~ sum(. == "EU_plus"))),
           n_nat_other_easy = unlist(map(simmed_passengers, ~ sum(. == "other_easy"))),
           n_nat_other_hard = unlist(map(simmed_passengers, ~ sum(. == "other_hard")))) %>% 
    select(-simmed_passengers)
  
  return(aircrafts_with_simmed_nationality)
}

complete_aircrafts_arrivals <- function(aircrafts_arrivals, 
                                        hubs,
                                        countries,
                                        prop_nationality,
                                        delay_dist = NULL,
                                        n_passengers_quantiles = NULL,
                                        load_factor = NULL,
                                        coached_levels = NULL,
                                        seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  completed_aircrafts_arrivals <- aircrafts_arrivals
  
  # Simulate aircraft delays
  if (all(is.na(completed_aircrafts_arrivals$aircraft_datetime_int))) {
    if (is.null(delay_dist)) stop("must supply delay_dist to complete_aircrafts_arrivals")
    completed_aircrafts_arrivals <- completed_aircrafts_arrivals %>% 
      mutate(aircraft_datetime_int =
               sched_aircraft_datetime_int + sim_delay_times(flight_id, delay_dist)
      )
  }
  
  # Get full datetime columns
  completed_aircrafts_arrivals <- get_datetime_alternates(
    completed_aircrafts_arrivals,
    column_prefixes = c("sched_aircraft", "aircraft")
  )
  
  # Get or simulate airport classification
  if (all(is.na(completed_aircrafts_arrivals$airport_classification))) {
    if (all(!is.na(completed_aircrafts_arrivals$dep_country)) &
        all(!is.na(completed_aircrafts_arrivals$dep_airport))) {
      completed_aircrafts_arrivals <- completed_aircrafts_arrivals %>% 
        mutate(airport_classification = get_airport_classification(
          airport_country = dep_country,
          airport_3letter = dep_airport,
          hubs = hubs,
          countries = countries
        ))
    } 
    else if (all(!is.na(completed_aircrafts_arrivals$n_passengers))) {
      if (is.null(n_passengers_quantiles)) stop("must supply n_passengers_quantiles to complete_aircrafts_arrivals")
      completed_aircrafts_arrivals <- completed_aircrafts_arrivals %>% 
        mutate(airport_classification = sim_airport_classification(
          n_passengers,
          n_passengers_quantiles
        ))
    }
    else {
      stop("Aircraft arrivals must either have complete n_passengers or dep_country/dep_airport.")
    }
  }
  
  # Simulate number of passengers
  if (all(is.na(completed_aircrafts_arrivals$n_passengers))) {
    if (is.null(load_factor)) stop("must supply load_factor to complete_aircrafts_arrivals")
    completed_aircrafts_arrivals <- completed_aircrafts_arrivals %>% 
      mutate(n_passengers = sim_n_passengers(max_passengers, load_factor))
  }
  
  # Simulate coached status
  if(all(is.na(completed_aircrafts_arrivals$coached))) {
    if (is.null(coached_levels)) stop("must supply coached_levels to complete_aircraft_arrivals")
    completed_aircrafts_arrivals <- completed_aircrafts_arrivals %>% 
      sim_coached_status(coached_levels = coached_levels)
  }
  
  # Simulate nationality breakdown
  completed_aircrafts_arrivals <- completed_aircrafts_arrivals %>% 
    sim_nationality_split(hubs = hubs, 
                          prop_nationality = prop_nationality, 
                          countries = countries)
  
  
  check_aircrafts_arrivals(completed_aircrafts_arrivals, complete = TRUE)
  return(completed_aircrafts_arrivals)
}

get_passengers_after_aircrafts <- function(aircrafts_arrivals, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  n_passengers_total <- sum(aircrafts_arrivals$n_passengers)

  passengers <- data.frame(
    passenger_id = paste0("P", str_pad(seq_len(n_passengers_total), 10, pad = "0"),
                          recycle0 = TRUE),
    flight_id = rep(aircrafts_arrivals$flight_id, aircrafts_arrivals$n_passengers),
    nationality = aircrafts_arrivals %>% 
      select(n_nat_UKIE, n_nat_EU_plus, n_nat_other_easy, n_nat_other_hard) %>% 
      pivot_longer(cols = everything(), names_to = "nat", values_to = "n_nat") %>% 
      {rep(.$nat, .$n_nat)} %>% 
      str_sub(3,-1),
    airport_classification = rep(aircrafts_arrivals$airport_classification, aircrafts_arrivals$n_passengers),
    aircraft_datetime_int = rep(aircrafts_arrivals$aircraft_datetime_int, aircrafts_arrivals$n_passengers),
    aircraft_datetime_posix = rep(aircrafts_arrivals$aircraft_datetime_posix, aircrafts_arrivals$n_passengers),
    aircraft_time_int = rep(aircrafts_arrivals$aircraft_time_int, aircrafts_arrivals$n_passengers),
    aircraft_date_posix = rep(aircrafts_arrivals$aircraft_date_posix, aircrafts_arrivals$n_passengers),
    coached = rep(aircrafts_arrivals$coached, aircrafts_arrivals$n_passengers)
  ) %>% 
    sample_frac(1) %>% 
    arrange(aircraft_datetime_posix)
  
  check_passengers_after_aircrafts(passengers)
  return(passengers)
}




