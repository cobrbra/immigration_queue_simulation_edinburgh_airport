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

get_passengers_from_aircraft <- function(aircraft_schedule) {
  passengers_from_aircraft <- data.frame(
    passenger_id = character(),
    aircraft_id = character(),
    nationality = character(),
    coached = numeric(),
    aircraft_arrival = character()
  )
  check_passengers_from_aircraft(passengers_from_aircraft)
  return(passengers_from_aircraft)
}
