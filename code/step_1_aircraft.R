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
  # generates minimum walk time
  
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





get_n_passengers <- function(max_passengers, load_factor_mean, 
                             load_factor_sd, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  load_factor <- rnorm(n = length(max_passengers),
                       mean = load_factor_mean, sd = load_factor_sd) 
  load_factor <- pmin(load_factor, 1)
  load_factor <- pmax(load_factor, 0)
  
  n_passengers <- round(max_passengers * load_factor)
  
  return(n_passengers)
}


get_airport_classification <- function(airport_country, airport_3letter, 
                                       EU_hubs, other_hubs, EU_countries){
  
  res <- character(1)
  
  if(airport_3letter %in% EU_hubs) {
    res <- "EU_hub"
  } else if(airport_country %in% EU_countries) {
    res <- "EU_nonhub"
  } else if(airport_3letter %in% Other_hubs){
    res <- "other_hub"
  } else {
    res <- "other_nonhub"
  }
  
  return(res)
}


get_nationality_split <- function(aircrafts, EU_hubs, Other_hubs, prop_nationality, 
                                  EU_countries,
                                  seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  if(dim(prop_nationality)!= c(4, 5)){stop("Prop Nationality needs to be 4x5.")}
  
  n_aircrafts <- dim(aircrafts)[1]
  
  for(j in seq_len(n_aircrafts)){
    
    airport_classification <- 
      get_airport_classification(airport_country = aircrafts$dep_country[j],
                                 airport_3letter = aircrafts$dep_airport[j],
                                 EU_hubs = EU_hubs, other_hubs = other_hubs,
                                 EU_countries = EU_countries)
    
    sim_passengers <- sample(x = colnames(prop_nationality[2:5]), size = aircrafts$n_passengers[j],
                            replace = TRUE, 
                            prob = prop_nationality[prop_nationality$Arriving_from == airport_classification, 2:5])
    
    aircrafts$n_nat_UKIE[j] <- sum(sim_passengers == "UKIE")
    aircrafts$n_nat_EU_plus[j] <- sum(sim_passengers == "EU_plus")
    aircrafts$n_nat_other_easy[j] <- sum(sim_passengers == "other_easy")
    aircrafts$n_nat_other_hard[j] <- sum(sim_passengers == "other_hard")
    
  }
  
  return(aircrafts)
}




# prop_nationality <- read.table(file = targets::tar_read(nationality_props), sep = ";", header = TRUE)



get_passengers_from_aircrafts <- function(aircrafts, seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  
  n_passengers_aircraft <- aircrafts$n_passengers
  n_passengers <- sum(n_passengers_aircraft)
  # print(n_passengers_aircraft)
  
  passengers <- data.frame(
    
    passenger_id = paste0("P", str_pad(seq_len(n_passengers), 6, pad = "0"),
                          recycle0 = TRUE),
    aircraft_id = rep(aircrafts$aircraft_id, n_passengers_aircraft),
    nationality = character(n_passengers),
    aircraft_arrival = rep(aircrafts$aircraft_arrival, n_passengers_aircraft),
    coached = rep(aircrafts$coached, n_passengers_aircraft),
    taxi_time = rep(aircrafts$taxi_time, n_passengers_aircraft),
    walk_time = rep(aircrafts$walk_time, n_passengers_aircraft),
    arrival_at_hall = character(n_passengers),
    nationality = rep("EU_plus", n_passengers) # needs adaption
    # does nor work yet
    # nationality = rep(c("UKIE", "EU_plus", "other_easy, other_hard"), 
    #                   each = c(aircrafts$n_nat_UKIE, aircrafts$n_nat_EU_plus,
    #                            aircrafts$n_nat_other_easy, aircrafts$n_nat_other_hard) )
  )
  
  check_passengers_from_aircraft(passengers)
  return(passengers)
}
