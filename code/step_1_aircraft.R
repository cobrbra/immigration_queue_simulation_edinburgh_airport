check_passengers_from_aircraft <- function(passengers_from_aircraft) {
  if (!is.data.frame(passengers_from_aircraft)) {
    stop("Passengers from aircraft should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "aircraft_id",
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



get_passenger_from_aircrafts <- function(aircrafts, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  n_passengers_aircraft <- aircrafts$passenger_on_board
  n_passengers <- sum(n_passengers_aircraft)
  
  passengers <- data.frame(
    
    passenger_id =paste0("P", str_pad(1:n_passengers, 6, pad = "0")),
    aircraft_id = rep(aircrafts$aircraft_id, n_passengers_aircraft),
    nationality = character(n_passengers),
    aircraft_arrival = rep(aircrafts$aircraft_arrival, n_passengers_aircraft),
    coached = rep(aircrafts$coached, n_passengers_aircraft),
    arrival_at_hall = character(n_passengers)
    
  )
  
  check_passengers_from_aircraft(passengers_from_aircraft)
  return(passengers)
}
