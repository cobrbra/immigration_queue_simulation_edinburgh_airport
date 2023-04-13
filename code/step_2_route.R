check_passengers_after_routes <- function(passengers_after_routes) {
  if (!is.data.frame(passengers_after_routes)) {
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
    "route_date_posix",
    "sched_aircraft_date_posix")
  
  if (any(!(necessary_columns %in% colnames(passengers_after_routes)))) {
    stop(
      paste(c("Passengers from route should contain columns", necessary_columns), 
            collapse = " "))
  }
  
  if (length(passengers_after_routes$passenger_id) != length(unique(passengers_after_routes$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
}

sim_aircraft_route_time <- function(passengers, coach_dist, base_walk_dist, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  n_flights <- n_distinct(passengers$flight_id)
  coach_times <- rnorm(n_flights, coach_dist$mean, coach_dist$sd)
  coach_times[coach_times <= 0] <- 0
  base_walk_times <- runif(n_flights, base_walk_dist$min, base_walk_dist$max)
  passengers_with_coach_time <- passengers %>% 
    nest(flight_data = -c(coached, flight_id)) %>% 
    mutate(aircraft_route_time = if_else(coached, coach_times, base_walk_times)) %>% 
    unnest(flight_data)
  return(passengers_with_coach_time)
}

sim_passenger_route_time <- function(passengers, 
                          walk_dist,
                          seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  n_passengers <- nrow(passengers)
  coached_walk_time <- rexp(n_passengers, 1/walk_dist$coached_inter)
  contact_walk_time <- rexp(n_passengers, 1/walk_dist$contact_inter) 
  passengers_with_walk_time <- passengers %>% 
    mutate(passenger_intervals = if_else(coached, coached_walk_time, contact_walk_time)) %>% 
    group_by(flight_id) %>% 
    mutate(passenger_route_time = cumsum(passenger_intervals)) %>% 
    ungroup()
  return(passengers_with_walk_time)
}


get_passengers_after_routes <- function(passengers_after_aircraft, 
                                        coach_dist,
                                        walk_dist,
                                        base_walk_dist,
                                        seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  passengers_after_routes <- passengers_after_aircraft %>% 
    sim_aircraft_route_time(coach_dist = coach_dist,
                            base_walk_dist = base_walk_dist) %>% 
    sim_passenger_route_time(walk_dist = walk_dist) %>% 
    mutate(route_datetime_int = aircraft_datetime_int + 
             aircraft_route_time + passenger_route_time) %>%
    get_datetime_alternates(column_prefixes = c("route")) %>% 
    select(-c(aircraft_route_time, passenger_route_time, 
              aircraft_datetime_int, aircraft_datetime_posix, 
              aircraft_time_int, aircraft_date_posix)) %>%
    arrange(route_datetime_int) %>% 
    as.data.frame()
  
  check_passengers_after_routes(passengers_after_routes)
  return(passengers_after_routes)
}
