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

get_desk_handling_time <- function(desks, desk_id, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  desk_handling_time <- rexp(1, desks$desk_handling_rates[desk_id])
  
  return (desk_handling_time)
}

queue <- function(passengers, 
                  desks, 
                  handling_time_func = get_desk_handling_time,
                  seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  n_desks <- desks$n_desks
  desk_times <- rep(0, times = desks$n_desks)
  
  n_passengers <- dim(passengers)[1]
  
  
  for(i in 1:n_passengers){ 
    
    next_arrival_time <- passengers$arrival_time[i]
    
    # if a desk is idle, update their time
    desk_times <- pmax(desk_times, next_arrival_time)
    
    # decide which desk to send the passenger to
    next_free_desk <- which(desk_times == min(desk_times))[1] # TODO: edit for E-Gates with conditions
    next_free_desk_time <- min(desk_times)
    
    # how long it takes to handle passenger
    handling_time <- handling_time_func(desks, next_free_desk)
    
    # update accordingly for passenger
    passengers$desk_start_time[i] <- next_free_desk_time
    passengers$desk_end_time[i] <- next_free_desk_time + handling_time
    passengers$desk_handled[i] <- desks$desk_ids[next_free_desk]
    
    # update desk 
    desk_times[next_free_desk] <- desk_times[next_free_desk] + handling_time
    
    
  }
  
  return(passengers)
  
}

get_passengers_from_immigration <- function(passengers_from_hall) {
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
