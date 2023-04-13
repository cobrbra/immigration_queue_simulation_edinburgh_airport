check_passengers_after_aircraft <- function(passengers_after_aircraft) {
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


sim_n_passengers <- function(max_passengers, load_factor, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  load_factors <- rnorm(n = length(max_passengers),
                       mean = load_factor$mean, sd = load_factor$sd) 
  load_factors <- pmin(load_factors, 1)
  load_factors <- pmax(load_factors, 0)
  
  n_passengers <- round(max_passengers * load_factors)
  
  return(n_passengers)
}


get_airport_classification <- function(airport_country, 
                                       airport_3letter, 
                                       hubs,
                                       countries){
  
    airport_classification <- case_when(
      airport_country %in% countries$UK_plus ~ "UK_plus",
      airport_3letter %in% hubs$EU_plus ~ "EU_plus_hub",
      airport_country %in% countries$EU_plus ~ "EU_plus_nonhub",
      airport_3letter %in% hubs$other ~ "other_hub",
      TRUE ~ "other_nonhub")

  return(airport_classification)
}

sim_coached_status <- function(aircrafts_arrivals, coached_levels, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  aircrafts_arrivals <- aircrafts_arrivals %>% 
    mutate(year = format(sched_aircraft_datetime_posix, "%Y")) %>% 
    group_by(year) %>% 
    nest() %>% 
    inner_join(coached_levels) %>% 
    unnest(data) %>% 
    ungroup() %>% 
    mutate(coached = as.logical(rbinom(nrow(.), 1, prob = prob_coached))) %>% 
    select(-c(year, prob_coached))
      
  return(aircrafts_arrivals)
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

# TODO: function for walk time baseline

get_nationality_split <- function(aircrafts, hubs, prop_nationality, 
                                  countries,
                                  seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  if(!all(dim(prop_nationality) == c(5, 5))){stop("Prop Nationality needs to be 5x5.")}
  
  n_aircrafts <- dim(aircrafts)[1]
  
  if (!("airport_classification" %in% colnames(aircrafts)) | 
      (("airport_classification" %in% colnames(aircrafts)) & 
       any(is.na(aircrafts[["airport_classification"]])))) {
    aircrafts_with_airport_class <- aircrafts %>% 
      mutate(airport_classification = get_airport_classification(
        airport_country = dep_country,
        airport_3letter = dep_airport,
        hubs = hubs,
        countries = countries
      ))
  }
  else{
    aircrafts_with_airport_class <- aircrafts
  }
  
  sim_passengers <- function(n_passenegers, airport_classification) {
    sample(x = colnames(prop_nationality)[-1],
           size = n_passenegers,
           replace = TRUE,
           prob = prop_nationality[prop_nationality$Arriving_from == airport_classification, ][-1]
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


get_passengers_after_aircrafts <- function(aircrafts, 
                                          hubs,
                                          prop_nationality,
                                          countries,
                                          load_factor,
                                          coached_levels,
                                          seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  aircrafts_with_passengers <- aircrafts %>% 
    mutate(n_passengers = if_else(is.na(n_passengers),
                                  sim_n_passengers(max_passengers, load_factor),
                                  n_passengers)) %>% 
    sim_coached_status(coached_levels = coached_levels) %>% 
    get_nationality_split(hubs = hubs, 
                          prop_nationality = prop_nationality, 
                          countries = countries)
  
  n_passengers_aircraft <- aircrafts_with_passengers$n_passengers
  n_passengers <- sum(n_passengers_aircraft)

  passengers <- data.frame(
    
    passenger_id = paste0("P", str_pad(seq_len(n_passengers), 10, pad = "0"),
                          recycle0 = TRUE),
    flight_id = rep(aircrafts_with_passengers$flight_id, n_passengers_aircraft),
    nationality = aircrafts_with_passengers %>% 
      select(n_nat_UK_plus, n_nat_EU_plus, n_nat_other_easy, n_nat_other_hard) %>% 
      pivot_longer(cols = everything(), names_to = "nat", values_to = "n_nat") %>% 
      {rep(.$nat, .$n_nat)} %>% 
      str_sub(3,-1),
    airport_classification = rep(aircrafts_with_passengers$airport_classification, n_passengers_aircraft),
    aircraft_datetime_int = rep(aircrafts_with_passengers$aircraft_datetime_int, n_passengers_aircraft),
    aircraft_datetime_posix = rep(aircrafts_with_passengers$aircraft_datetime_posix, n_passengers_aircraft),
    aircraft_time_int = rep(aircrafts_with_passengers$aircraft_time_int, n_passengers_aircraft),
    aircraft_date_posix = rep(aircrafts_with_passengers$aircraft_date_posix, n_passengers_aircraft),
    coached = rep(aircrafts_with_passengers$coached, n_passengers_aircraft)
  ) %>% 
    sample_frac(1)
  
  check_passengers_after_aircraft(passengers)
  return(passengers)
}

complete_aircrafts_arrivals <- function(aircrafts_arrivals, 
                                        delay_dist,
                                        hubs,
                                        countries,
                                        n_passengers_quantiles,
                                        load_factor,
                                        seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  completed_aircrafts_arrivals <- aircrafts_arrivals
  
  # Simulate aircraft delays
  if (all(is.na(completed_aircrafts_arrivals$aircraft_datetime_int))) {
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
    completed_aircrafts_arrivals <- completed_aircrafts_arrivals %>% 
      mutate(n_passengers = sim_n_passengers(max_passengers, load_factor))
  }
  
  # check_aircrafts_arrivals(completed_aircrafts_arrivals, complete = TRUE)
  return(completed_aircrafts_arrivals)
}


