check_passengers_from_aircraft <- function(passengers_from_aircraft) {
  if (!is.data.frame(passengers_from_aircraft)) {
    stop("Passengers from aircraft should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "aircraft_id",
    "nationality",
    "coached",
    "aircraft_arrival",
    "walk_time", 
    "taxi_time"
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



get_taxi_time <- function(coached){
  
  n_aircrafts <- length(coached)
  taxi_time <- numeric(n_aircrafts)
  
  for(j in seq_len(n_aircrafts)){
    
    if(coached[j]){
      taxi_time[j]<- pmax(2, rnorm(1, mean = 4, sd = 0.5))
    } else {
      taxi_time[j] <- pmax(2, rnorm(1, mean = 5, sd = 1))
    }
    
  }
  
  return(taxi_time)
  
}


get_walk_time <- function(coached){
  
  n_aircrafts <- length(coached)
  walk_time <- numeric(n_aircrafts)
  
  for(j in seq_len(n_aircrafts)){
    
    if(coached[j]){
      walk_time[j]<- runif(1, min = 10, max = 15) # include getting everyone into busses, driving over, and walking
    } else {
      walk_time[j] <- pmax(3, rpois(n = 1, lambda = 5))
    }
    
  }
  
  return(walk_time)
  
}


get_passengers_from_aircrafts <- function(aircrafts, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  n_passengers_aircraft <- aircrafts$n_passengers
  n_passengers <- sum(n_passengers_aircraft)
  print(n_passengers_aircraft)
  
  passengers <- data.frame(
    
    passenger_id = paste0("P", str_pad(seq_len(n_passengers), 6, pad = "0"),
                          recycle0 = TRUE),
    aircraft_id = rep(aircrafts$aircraft_id, n_passengers_aircraft),
    nationality = character(n_passengers),
    aircraft_arrival = rep(aircrafts$aircraft_arrival, n_passengers_aircraft),
    coached = rep(aircrafts$coached, n_passengers_aircraft),
    taxi_time = rep(aircrafts$taxi_time, n_passengers_aircraft),
    walk_time = rep(aircrafts$walk_time, n_passengers_aircraft),
    arrival_at_hall = character(n_passengers)
    
  )
  
  check_passengers_from_aircraft(passengers)
  return(passengers)
}
