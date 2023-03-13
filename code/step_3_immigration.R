check_passengers_from_immigration <- function(passengers_from_immigration) {
  if (!is.data.frame(passengers_from_immigration)) {
    stop("Passengers from immigration should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "aircraft_id",
    "nationality",
    "aircraft_arrival",
    "arrival_at_hall",
    "arrival_at_queue",
    "arrival_from_immigration",
    "queue_length",
    "e_gate"
  )
  if (any(!(necessary_columns %in% colnames(passengers_from_immigration)))) {
    stop(
      paste(c("Passengers from immigration should contain columns", necessary_columns), 
            collapse = " "))
  }
  
  if (length(passengers_from_immigration$passenger_id) != length(unique(passengers_from_immigration$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
  
}




get_check_handling_time <- function(checks, check_id, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  check_handling_time <- rexp(1, checks$handling_rates[check_id])
  
  return (check_handling_time)
}




immigration_queue <- function(passengers, 
                  checks, 
                  handling_time_func = get_check_handling_time,
                  seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  n_checks <- checks$n_checks
  check_times <- rep(0, times = checks$n_checks)
  
  n_passengers <- dim(passengers)[1]
  
  
  for(i in seq_len(n_passengers)){ 
    
    next_arrival_time <- passengers$arrival_at_queue[i]
    
    # if a check is idle, update their time
    check_times <- pmax(check_times, next_arrival_time)
    
    # decide which check to send the passenger to
    next_free_check <- which(check_times == min(check_times))[1] # TODO: edit for E-Gates with conditions
    next_free_check_time <- min(check_times)
    
    # how long it takes to handle passenger
    handling_time <- handling_time_func(checks, next_free_check)
    
    # update accordingly for passenger
    passengers$check_start_time[i] <- next_free_check_time
    passengers$check_end_time[i] <- next_free_check_time + handling_time
    passengers$check_handled[i] <- checks$check_ids[next_free_check]
    
    # update check 
    check_times[next_free_check] <- check_times[next_free_check] + handling_time
    
    
  }
  
  return(passengers)
  
}

get_passengers_from_immigration <- function(passengers_from_route) {
  passengers_from_immigration <- data.frame(
    passenger_id = character(),
    aircraft_id = character(),
    nationality = character(),
    aircraft_arrival = character(),
    arrival_at_hall = character(),
    arrival_at_queue = character(),
    arrival_from_immigration = character(),
    queue_length = numeric(),
    e_gate = numeric()
  )
  check_passengers_from_immigration(passengers_from_immigration)
  return(passengers_from_immigration)
}
