check_passengers_after_route <- function(passengers_after_route) {
  if (!is.data.frame(passengers_after_route)) {
    stop("Passengers from route should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "flight_id",
    "nationality",
    "airport_classification",
    "route_datetime_int",
    "route_time_int",
    "route_datetime_posix",
    "route_date_posix")
  
  if (any(!(necessary_columns %in% colnames(passengers_after_route)))) {
    stop(
      paste(c("Passengers from route should contain columns", necessary_columns), 
            collapse = " "))
  }
  
  if (length(passengers_after_route$passenger_id) != length(unique(passengers_after_route$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
}


get_coach_time <- function(passengers, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  n_flights <- n_distinct(passengers$flight_id)
  passengers_with_coach_time <- passengers %>% 
    nest(cols = -c(coached, flight_id)) %>% 
    mutate(coach_time = if_else(coached, pmax(60, rnorm(n_flights, 23*60, 6*60)), 0)) %>% 
    unnest(cols)
  return(passengers_with_coach_time)
}

get_walk_time <- function(passengers, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  n_passengers <- nrow(passengers)
  coached_walk_time <- rexp(n_passengers, .5)
  contact_walk_time <- rexp(n_passengers, .2) # TODO: this cumsum is over the whole dataset and shouldn't be!
  passengers_with_walk_time <- passengers %>% 
    mutate(walk_time = if_else(coached, coached_walk_time, contact_walk_time)) %>% 
    group_by(flight_id) %>% 
    mutate(walk_time = cumsum(walk_time)) %>% 
    ungroup()
  return(passengers_with_walk_time)
}


get_passengers_after_route <- function(passengers_after_aircraft) {
  passengers_after_route <- passengers_after_aircraft %>% 
    get_coach_time() %>% 
    get_walk_time() %>% 
    mutate(route_datetime_int = aircraft_datetime_int + coach_time + walk_time) %>%
    get_datetime_alternates(column_prefixes = c("route")) %>% 
    select(-c(coach_time, walk_time, 
              aircraft_datetime_int, aircraft_datetime_posix, 
              aircraft_time_int, aircraft_date_posix)) %>%
    arrange(route_datetime_int)
  
  check_passengers_after_route(passengers_after_route)
  return(passengers_after_route)
}
