check_passengers_from_route <- function(passengers_from_route) {
  if (!is.data.frame(passengers_from_route)) {
    stop("Passengers from route should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "aircraft_id",
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
