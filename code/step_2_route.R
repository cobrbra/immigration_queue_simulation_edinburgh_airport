check_passengers_from_route <- function(passengers_from_route) {
  if (!is.data.frame(passengers_from_route)) {
    stop("Passengers from route should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "flight_id",
    "nationality",
    "aircraft_arrival",
    "arrival_at_hall"
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


get_coach_time <- function(passengers, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  n_flights <- n_distinct(passengers$flight_id)
  passengers_with_coach_time <- passengers %>% 
    nest(cols = -c(coached, flight_id)) %>% 
    mutate(coach_time = if_else(coached, pmax(60, rnorm(n_flights, 23*60, 6*60)), 0)) %>% 
    unnest(cols)
  return(passengers_with_coach_time)
}

get_walk_time <- function(coached, n_passengers, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  n_passengers <- length(coached)
  coached_walk_time <- cumsum(rexp(n_passengers, .5))
  contact_walk_time <- cumsum(rexp(n_passengers, .2)) # TODO: this cumsum is over the whole dataste and shouldn't be!
  walk_time <- if_else(coached, coached_walk_time, contact_walk_time)
  return(walk_time)
}


get_passengers_from_route <- function(passengers_from_aircraft) {
  passengers_from_route <- passengers_from_aircraft %>% 
    get_coach_time() %>% 
    mutate(walk_time = get_walk_time(coached = coached)) %>% 
    mutate(arrival_at_hall = aircraft_arrival + coach_time + walk_time) %>% 
    # select(-c(coach_time, walk_time)) %>% 
    arrange(arrival_at_hall)
  
  check_passengers_from_route(passengers_from_route)
  return(passengers_from_route)
}


# route_process  <- function(passengers, seed = NULL){
#   
#   if (!is.null(seed)) {set.seed(seed)}
#   
#   flight_id <- unique(passengers$flight_id)
#   
#   n_aircrafts <- length(flight_id)
#   
#   n_passengers_aircraft <- table(passengers$flight_id)
#   rownames(n_passengers_aircraft) <- NULL
#   
#   coached <- as.logical(tapply(X = passengers$coached, FUN = prod, INDEX = passengers$flight_id))
#   
#   
#   # get the minimum arrival at hall per aircraft
#   passengers$arrival_at_hall <- passengers$aircraft_arrival + passengers$taxi_time + passengers$walk_time
#   
#   for(j in seq_len(n_aircrafts)){
#     
#     # spread out passenger arrival differently for coached and contact
#     
#     bool_iter <- passengers$flight_id == flight_id[j]
#     
#     # TODO: make these calls to randomness separate function calls
#     if(coached[j]){
#       
#       # we should get this distribution from them 
#       passengers$arrival_at_hall[bool_iter] <- passengers$arrival_at_hall[bool_iter] + runif(n = sum(bool_iter), min = 0.5, max = 4)
#       
#     } else {
#       
#       # we should get this distribution from them 
#       passengers$arrival_at_hall[bool_iter] <- passengers$arrival_at_hall[bool_iter] + cumsum(rexp(n = sum(bool_iter), rate = 12))
#     }
#     
#     
#   }
#   
#   passengers <- passengers[order(passengers$arrival_at_hall), ]
#   
#   return(passengers)
#   
#   
# }
