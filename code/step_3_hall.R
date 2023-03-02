check_passengers_from_hall <- function(passengers_from_hall) {
  if (!is.data.frame(passengers_from_hall)) {
    stop("Passengers from hall should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "aircraft_id",
    "nationality",
    "aircraft_arrival",
    "arrival_at_hall",
    "arrival_at_queue",
    "queue_length",
    "e_gate"
  )
  if (any(!(necessary_columns %in% colnames(passengers_from_hall)))) {
    stop(
      paste(c("Passengers from hall should contain columns", necessary_columns), 
            collapse = " "))
  }
  
  if (length(passengers_from_hall$passenger_id) != length(unique(passengers_from_hall$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
}

get_passengers_from_hall <- function(passengers_from_route) {
  passengers_from_hall <- data.frame(
    passenger_id = character(),
    aircraft_id = character(),
    nationality = character(),
    aircraft_arrival = character(),
    arrival_at_hall = character(),
    arrival_at_queue = character(),
    queue_length = numeric(),
    e_gate = numeric()
  )
  check_passengers_from_hall(passengers_from_hall)
  return(passengers_from_hall)
}
