check_passengers_after_immigration <- function(passengers_after_immigration) {
  if (!is.data.frame(passengers_after_immigration)) {
    stop("Passengers from immigration should be dataframe.")
  }
  necessary_columns <- c(
    "passenger_id",
    "flight_id",
    "nationality",
    "airport_classification",
    "route_datetime_int",
    "route_time_int",
    "route_datetime_posix",
    "route_date_posix",
    "immigration_datetime_int",
    "immigration_time_int",
    "immigration_datetime_posix",
    "immigration_date_posix",
    "queue_time",
    "egate_eligible",
    "egate_used",
    "egate_failed"
  )
  if (any(!(necessary_columns %in% colnames(passengers_after_immigration)))) {
    stop(
      paste(c("Passengers from immigration should contain columns", necessary_columns), 
            collapse = " "))
  }
  
  if (length(passengers_after_immigration$passenger_id) != length(unique(passengers_after_immigration$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
  
}

get_egate_handling_time <- function(borderchecks, bordercheck_id, seed = NULL) {
  
  if (!is.null(seed)) {set.seed(seed)}
  
  # bordercheck_handling_time <- rexp(1, borderchecks$bordercheck_rates[bordercheck_id])
  bordercheck_handling_time <- pmax(20, rnorm(1, mean = borderchecks$bordercheck_rates[bordercheck_id], sd = 5))
  
  return (bordercheck_handling_time)
}

alt_get_egate_handling_times <- function(handling_time_mean = 45, 
                                         handling_time_sd = 5, 
                                         handling_time_lower_bound = 20, 
                                         n_passengers = 1, 
                                         seed = NULL) {
  
  if (!is.null(seed)) {set.seed(seed)}
  
  bordercheck_handling_times <- pmax(handling_time_lower_bound, 
                                     rnorm(n_passengers, 
                                           mean = handling_time_mean, 
                                           sd = handling_time_sd))
  
  return (bordercheck_handling_times)
}



get_desk_handling_time <- function(borderchecks, bordercheck_id, seed = NULL) {
  # TODO: input nationality as well
  # TODO: differentiate egate and desk
  if (!is.null(seed)) {set.seed(seed)}
  # bordercheck_handling_time <- rexp(1, borderchecks$bordercheck_rates[bordercheck_id])
  bordercheck_handling_time <- pmax(30, rnorm(1, mean = borderchecks$bordercheck_rates[bordercheck_id]), sd = 10)
  
  return (bordercheck_handling_time)
}


get_egate_eligible <- function(nationality){
  
  egate_eligibility <- if_else(nationality %in% c("nat_UK_plus", "nat_EU_plus"), "eligible", "not_eligible")
  return(egate_eligibility)
  
}


get_egate_usage <- function(egate_eligibility, egate_uptake_prop){
  
  n_passengers <- length(egate_eligibility)
  egate_usage <- rep("desk", times = n_passengers)
  
  bool_eligible <- egate_eligibility == "eligible"
  n_eligible <- sum(bool_eligible)
  egate_usage[bool_eligible] <- ifelse(runif(n = n_eligible) < egate_uptake_prop, "egate", "desk")
  
  return(egate_usage)
}


get_egate_failure <- function(egate_used, egate_failure_prop){
  
  n_passengers <- length(egate_used)
  egate_failed <- rep("no_egate", times = n_passengers)
  
  bool_usage <- egate_used == "egate"
  n_usage <- sum(bool_usage)
  egate_failed[bool_usage] <- ifelse(runif(n = n_usage) < egate_failure_prop, "failed", "passed")
  
  return(egate_failed)
  
}

immigration_queue <- function(passengers, 
                  bordercheck_desks, bordercheck_egates, 
                  egate_uptake_prop, egate_failure_prop, 
                  egate_failed_passenger_next, 
                  seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  passengers <- passengers %>% 
    mutate(egate_eligibility = get_egate_eligible(nationality)) %>% 
    mutate(egate_used = get_egate_usage(egate_eligibility, egate_uptake_prop)) %>% 
    mutate(egate_failed = get_egate_failure(egate_used, egate_failure_prop)) %>% 
    mutate(bordercheck_start_time = numeric(n()), 
           bordercheck_end_time = numeric(n()),
           bordercheck_handled = numeric(n()))
  
  egate_numeric_columns <- c("route_datetime_int", "egate_handling_time", "bordercheck_start_time", 
                             "bordercheck_end_time", "bordercheck_handled")
  passengers_egate <- passengers %>% 
    filter(egate_used == "egate") %>% 
    mutate(egate_handling_time = alt_get_egate_handling_times(n_passengers = n()))
  passengers_egate_matrix <- passengers_egate %>%
    select(all_of(egate_numeric_columns)) %>% 
    as.matrix()
    
  desk_numeric_columns <- c("route_datetime_int", "bordercheck_start_time", "bordercheck_end_time",
                            "bordercheck_handled")
  passengers_desk <- passengers %>% 
    filter(egate_used == "desk") 
  passengers_desk_matrix <- passengers_desk %>% 
    select(all_of(desk_numeric_columns)) %>% 
    as.matrix()
  
  #### get all the egate passengers through
  
  
  n_borderchecks <- bordercheck_egates$n_borderchecks
  bordercheck_egates$bordercheck_ids_int <- as.integer(str_sub(bordercheck_egates$bordercheck_ids, 2, -1))
  bordercheck_desks$bordercheck_ids_int <- as.integer(str_sub(bordercheck_desks$bordercheck_ids, 2, -1))
  
  bordercheck_times <- rep(0, times = n_borderchecks)
  
  n_passengers_egate <- dim(passengers_egate)[1]
  for(passenger_index in seq_len(n_passengers_egate)){ 
    
    next_arrival_time <- passengers_egate_matrix[passenger_index, "route_datetime_int"]
    
    # if a check is idle, update their time
    bordercheck_times <- pmax(bordercheck_times, next_arrival_time)
    
    # decide which check to send the passenger to
    next_free_bordercheck <- which(bordercheck_times == min(bordercheck_times))[1] 
    next_free_bordercheck_time <- min(bordercheck_times)
    
    # how long it takes to handle passenger
    handling_time <- passengers_egate_matrix[passenger_index, "egate_handling_time"]

    # update accordingly for passenger
    passengers_egate_matrix[passenger_index, "bordercheck_start_time"] <- next_free_bordercheck_time
    passengers_egate_matrix[passenger_index, "bordercheck_end_time"] <- next_free_bordercheck_time + handling_time
    passengers_egate_matrix[passenger_index, "bordercheck_handled"] <- bordercheck_egates$bordercheck_ids_int[next_free_bordercheck]
    
    # update check 
    bordercheck_times[next_free_bordercheck] <- bordercheck_times[next_free_bordercheck] + handling_time
    
    
  }
  passengers_egate[, egate_numeric_columns] <- passengers_egate_matrix
  # at this point the egate passengers are processed
  # now deal with the failed passengers
  passengers_failed <- passengers_egate[passengers_egate$egate_failed == "failed", ]
  passengers_failed$bordercheck_egate_end_time <- passengers_failed$bordercheck_end_time
  passengers_failed <- passengers_failed[order(passengers_failed$bordercheck_end_time), ]
  failed_numeric_columns <- c("bordercheck_egate_end_time", "bordercheck_start_time", "bordercheck_end_time",
                              "bordercheck_handled")
  passengers_failed_matrix <- passengers_failed %>% 
    select(all_of(failed_numeric_columns)) %>% 
    as.matrix()
  
  n_passengers_failed <- dim(passengers_failed)[1]
  
  
  
  ### now let's look at the desk queue, where we input the failed egate passengers
  
  n_borderchecks <- bordercheck_desks$n_borderchecks
  bordercheck_times <- rep(0, times = n_borderchecks)
  
  n_passengers_desk <- dim(passengers_desk)[1]
  
  n_total <- n_passengers_desk + n_passengers_failed
  
  
  
  still_passengers_left <- TRUE
  
  i_desk <- 1
  i_failed <- 1
  
    while(still_passengers_left){
    
    if(i_desk > n_passengers_desk){
      next_desk_arrival_time <- Inf
    } else {
      next_desk_arrival_time <- passengers_desk_matrix[i_desk, "route_datetime_int"]
    }
    
    
    if(i_failed > n_passengers_failed){
      next_failed_arrival_time <- Inf
    } else {
      next_failed_arrival_time <- passengers_failed_matrix[i_failed, "bordercheck_egate_end_time"]
    }
    
    
    next_arrival_time <- min(next_desk_arrival_time, next_failed_arrival_time)
    
    # if a desk is idle, update their time
    bordercheck_times <- pmax(bordercheck_times, next_arrival_time)
    
    # decide which desk to send the passenger to
    next_free_bordercheck <- which(bordercheck_times == min(bordercheck_times))[1] 
    next_free_bordercheck_time <- min(bordercheck_times)
    
    
    
    # decide who will be handled
    if(next_failed_arrival_time > next_free_bordercheck_time){
      # no failed passengers yet
      next_passenger <- "desk"
    } else if(next_desk_arrival_time > next_free_bordercheck_time){
      # no desk passengers yet
      next_passenger <- "failed"
    } else if(is.infinite(next_failed_arrival_time)){
      # all failed already processed
      next_passenger <- "desk"
    } else if(is.infinite(next_desk_arrival_time)){
      # all desk already processed
      next_passenger <- "failed"
    } else {
      # both desk and failed passenger waiting
      next_passenger <- ifelse(runif(1) < egate_failed_passenger_next, "failed", "desk")
    }
    
    if(next_passenger == "desk"){
      
      # how long it takes to handle passenger
      handling_time <- get_desk_handling_time(bordercheck_desks, next_free_bordercheck) #TODO: add nationality
      
      # update accordingly for passenger
      passengers_desk_matrix[i_desk, "bordercheck_start_time"] <- next_free_bordercheck_time
      passengers_desk_matrix[i_desk, "bordercheck_end_time"] <- next_free_bordercheck_time + handling_time
      passengers_desk_matrix[i_desk, "bordercheck_handled"] <- bordercheck_desks$bordercheck_ids_int[next_free_bordercheck]
      
      i_desk <- i_desk+1
      
    } else {
      
      # how long it takes to handle passenger
      handling_time <- get_desk_handling_time(bordercheck_desks, next_free_bordercheck) #TODO: add nationality
      
      # update accordingly for passenger
      passengers_failed_matrix[i_failed, "bordercheck_start_time"] <- next_free_bordercheck_time
      passengers_failed_matrix[i_failed, "bordercheck_end_time"] <- next_free_bordercheck_time + handling_time
      passengers_failed_matrix[i_failed, "bordercheck_handled"] <- bordercheck_desks$bordercheck_ids_int[next_free_bordercheck]
      
      i_failed <- i_failed+1
      
    }
    
    # update desks 
    bordercheck_times[next_free_bordercheck] <- bordercheck_times[next_free_bordercheck] + handling_time
    
    if((i_desk + i_failed - 2) == n_total ){
      still_passengers_left <- FALSE
    }
   
  }
  passengers_failed[, failed_numeric_columns] <- passengers_failed_matrix
  passengers_desk[, desk_numeric_columns] <- passengers_desk_matrix
  passengers_egate$bordercheck_handled <- paste0("E", str_pad(passengers_egate$bordercheck_handled, 2, pad = "0"))
  passengers_desk$bordercheck_handled <- paste0("D", str_pad(passengers_desk$bordercheck_handled, 2, pad = "0"))
  passengers_failed$bordercheck_handled <- paste0("D", str_pad(passengers_failed$bordercheck_handled, 2, pad = "0"))
  
  passengers_egate$egate_handling_time <- NULL
  passengers_failed$egate_handling_time <- NULL
  passengers_failed$bordercheck_egate_end_time <- NULL
  
  passengers <- rbind(passengers_egate[passengers_egate$egate_failed == "passed", ], 
                      passengers_desk, 
                      passengers_failed)
  passengers <- passengers %>% 
    arrange(route_datetime_int)
  
  return(passengers)
  
}




get_passengers_after_immigration <- function(passengers_after_route) {
  # TODO: this function needs to be adapted with the big immigration_queue 
  passengers_after_immigration <- data.frame(
    passenger_id = character(),
    aircraft_id = character(),
    nationality = character(),
    aircraft_arrival = character(),
    arrival_at_hall = character(),
    arrival_at_queue = character(),
    arrival_after_immigration = character(),
    queue_length = numeric(),
    e_gate = numeric()
  )
  check_passengers_after_immigration(passengers_after_immigration)
  return(passengers_after_immigration)
}







