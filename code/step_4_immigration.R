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
  check_passengers_from_hall(passengers_from_hall)
  return(passengers_from_hall)
}
