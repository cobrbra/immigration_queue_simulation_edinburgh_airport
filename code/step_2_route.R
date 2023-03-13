check_passengers_from_route <- function(passengers_from_route) {
  if (!is.data.frame(passengers_from_route)) {
    stop("Passengers from route should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "aircraft_id",
    "nationality",
    "aircraft_arrival",
    "arrival_at_hall",
    "walk_time", 
    "taxi_time"
  )
  if (any(!(necessary_columns %in% colnames(passengers_from_route)))) {
    stop(
      paste(c("Passengers from route should contain columns", necessary_columns), 
            collapse = " "))
  }
  
  if (length(passengers_from_route$passenger_id) != length(unique(passengers_from_route$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
}






route_process  <- function(passengers, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  aircraft_id <- unique(passengers$aircraft_id)
  
  n_aircrafts <- length(aircraft_id)
  
  n_passengers_aircraft <- table(passengers$aircraft_id)
  rownames(n_passengers_aircraft) <- NULL
  
  coached <- as.logical(tapply(X = passengers$coached, FUN = prod, INDEX = passengers$aircraft_id))
  
  
  # get the minimum arrival at hall per aircraft
  passengers$arrival_at_hall <- passengers$aircraft_arrival + passengers$taxi_time + passengers$walk_time
  
  for(j in seq_len(n_aircrafts)){
    
    # spread out passenger arrival differently for coached and contact
    
    bool_iter <- passengers$aircraft_id == aircraft_id[j]
    
    # TODO: make these calls to randomness separate function calls
    if(coached[j]){
      
      # we should get this distribution from them 
      passengers$arrival_at_hall[bool_iter] <- passengers$arrival_at_hall[bool_iter] + runif(n = sum(bool_iter), min = 0.5, max = 4)
      
    } else {
      
      # we should get this distribution from them 
      passengers$arrival_at_hall[bool_iter] <- passengers$arrival_at_hall[bool_iter] + cumsum(rexp(n = sum(bool_iter), rate = 12))
    }
    
    
  }
  
  passengers <- passengers[order(passengers$arrival_at_hall), ]
  
  return(passengers)
  
  
}





get_passengers_from_route <- function(passengers_from_aircraft) {
  passengers_from_route <- data.frame(
    passenger_id = character(),
    aircraft_id = character(),
    nationality = character(),
    aircraft_arrival = character(),
    arrival_at_hall = character()
  )
  check_passengers_from_route(passengers_from_route)
  return(passengers_from_route)
}
