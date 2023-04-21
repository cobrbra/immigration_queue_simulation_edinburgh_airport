necessary_immigration_columns <- c(
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
  "egate_failed",
  "sched_aircraft_date_posix"
)

check_passengers_after_immigration <- function(passengers_after_immigration) {
  if (!is.data.frame(passengers_after_immigration)) {
    stop("Passengers from immigration should be dataframe.")
  }
  
  necessary_columns_missing <- !(necessary_immigration_columns %in% 
                                   colnames(passengers_after_immigration))
  if (any(necessary_columns_missing)) {
    stop(
      paste(c("Passengers from immigration should contain columns", 
              necessary_immigration_columns[necessary_columns_missing]), 
            collapse = " "))
  }
  
  if (length(passengers_after_immigration$passenger_id) != length(unique(passengers_after_immigration$passenger_id))) {
    stop("Passenger IDs are not unique.")
  }
  
}

sim_egate_handling_times <- function(bordercheck_egates, 
                                     n_passengers = 1, 
                                     seed = NULL) {
  
  if (!is.null(seed)) {set.seed(seed)}
  
  bordercheck_handling_times <- rnorm(n_passengers, 
                                      mean = bordercheck_egates$bordercheck_mean, 
                                      sd = bordercheck_egates$bordercheck_sd)
  bordercheck_handling_times[
    bordercheck_handling_times < 0
  ] <- 0
  
  return (bordercheck_handling_times)
}

sim_desk_handling_time <- function(borderchecks, bordercheck_id, seed = NULL) {
  # TODO: input nationality as well
  if (!is.null(seed)) {set.seed(seed)}
  bordercheck_handling_time <- max(0, rnorm(1, mean = borderchecks$bordercheck_means[bordercheck_id], sd = borderchecks$bordercheck_sd))
  
  return (bordercheck_handling_time)
}

alt_sim_desk_handling_times <- function(borderchecks, n_passengers) {
  n_borderchecks <- length(borderchecks$bordercheck_means)
  return(matrix(rnorm(n_borderchecks*n_passengers,
                      mean = rep(borderchecks$bordercheck_means, n_passengers),
                      sd = borderchecks$bordercheck_sd), 
                n_borderchecks,
                n_passengers))
}

get_egate_eligible <- function(nationality){
  
  egate_eligibility <- if_else(nationality %in% c("nat_UKIE", "nat_EU_plus"), 
                               true = "eligible", 
                               false = "not_eligible")
  
  return(egate_eligibility)
}

sim_boosted_egate_eligible <- function(egate_eligible, target_eligibility = 0.85) {
  n_passengers <- length(egate_eligible)
  baseline_eligibility <- sum(egate_eligible == "eligible") / n_passengers
  if (target_eligibility > baseline_eligibility) {
    boosted_egate_eligible <- if_else(
      egate_eligible == "eligible",
      "eligible",
      sample(c("eligible", "not_eligible"),
             size = n_passengers,
             prob = c((target_eligibility - baseline_eligibility)/(1-baseline_eligibility), 
                      1 - (target_eligibility - baseline_eligibility)/(1-baseline_eligibility)),
             replace = TRUE)
    )
  }
  else {
    boosted_egate_eligible <- if_else(
      egate_eligible == "not_eligible",
      "not_eligible",
      sample(c("eligible", "not_eligible"),
             size = n_passengers,
             prob = c(target_eligibility / baseline_eligibility,
                      1 - target_eligibility / baseline_eligibility),
             replace = TRUE)
    )
  }
  return(boosted_egate_eligible)
}


sim_egate_usage <- function(egate_eligibility, egate_uptake_prop){
  
  n_passengers <- length(egate_eligibility)
  egate_usage <- sample(c("egate", "desk"),
                        size = n_passengers,
                        replace = TRUE,
                        prob = c(egate_uptake_prop, 1 - egate_uptake_prop))
  
  egate_usage[egate_eligibility == "not_eligible"] <- "desk"
  return(egate_usage)
}

sim_egate_failure <- function(egate_used, egate_failure_prop, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  n_passengers <- length(egate_used)
  egate_failed <-  sample(c("failed", "passed"),
                          size = n_passengers,
                          replace = TRUE,
                          prob = c(egate_failure_prop, 1 - egate_failure_prop))
  egate_failed[egate_used == "desk"] <- "no_egate"
  return(egate_failed)
}

get_numeric_matrix <- function(df, numeric_columns) {
  return(as.matrix(df[, numeric_columns]))
}

sim_egate_queue <- function(passengers_egate_matrix, bordercheck_egates) {
  
  bordercheck_times <- rep(0, times = bordercheck_egates$n_borderchecks)
  n_passengers_egate <- dim(passengers_egate_matrix)[1]

  for (passenger_index in seq_len(n_passengers_egate)){ 
    
    next_arrival_time <- passengers_egate_matrix[passenger_index, "route_datetime_int"]
    
    # if a check is idle, update their time
    bordercheck_times[bordercheck_times < next_arrival_time] <- next_arrival_time
    
    # decide which check to send the passenger to
    next_free_bordercheck <- which.min(bordercheck_times) 
    next_free_bordercheck_time <- min(bordercheck_times)
    
    # how long it takes to handle passenger
    handling_time <- passengers_egate_matrix[passenger_index, "egate_handling_time"]
    
    # update accordingly for passenger
    passengers_egate_matrix[passenger_index, "bordercheck_start_time"] <- next_free_bordercheck_time
    passengers_egate_matrix[passenger_index, "bordercheck_end_time"] <- next_free_bordercheck_time + handling_time
    passengers_egate_matrix[passenger_index, "bordercheck_handled"] <- next_free_bordercheck
    
    # update check 
    bordercheck_times[next_free_bordercheck] <- bordercheck_times[next_free_bordercheck] + handling_time
  }
  return(passengers_egate_matrix)
}

sim_desk_queue <- function(passengers_desk_matrix, 
                           passengers_failed_matrix, 
                           bordercheck_desks,
                           failed_egate_priority) {
  
  n_desks <- bordercheck_desks$n_borderchecks
  bordercheck_times <- rep(0, times = n_desks)
  n_passengers_desk <- nrow(passengers_desk_matrix)
  n_passengers_failed <- nrow(passengers_failed_matrix)
  failed_desk_allocations <- sample(c("failed", "desk"), 
                                    size = n_passengers_failed,
                                    replace = TRUE,
                                    prob = c(failed_egate_priority, 1 - failed_egate_priority))
  still_passengers_left <- TRUE
  i_desk <- i_failed <- 1
  desk_handling_times <- alt_sim_desk_handling_times(bordercheck_desks,
                                                     n_passengers_desk)
  failed_handling_times <- alt_sim_desk_handling_times(bordercheck_desks,
                                                       n_passengers_failed)
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
    bordercheck_times[bordercheck_times < next_arrival_time] <- next_arrival_time
    
    # decide which desk to send the passenger to
    next_free_bordercheck <- which.min(bordercheck_times) 
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
      next_passenger <- failed_desk_allocations[i_failed]
    }
    
    # how long it takes to handle passenger
    if(next_passenger == "desk"){
      handling_time <- desk_handling_times[next_free_bordercheck, i_desk]
      # update accordingly for passenger
      passengers_desk_matrix[i_desk, "bordercheck_start_time"] <- next_free_bordercheck_time
      passengers_desk_matrix[i_desk, "bordercheck_end_time"] <- next_free_bordercheck_time + handling_time
      passengers_desk_matrix[i_desk, "bordercheck_handled"] <- next_free_bordercheck
      
      i_desk <- i_desk + 1

    } else {
      handling_time <- failed_handling_times[next_free_bordercheck, i_failed]
      # update accordingly for passenger
      passengers_failed_matrix[i_failed, "bordercheck_start_time"] <- next_free_bordercheck_time
      passengers_failed_matrix[i_failed, "bordercheck_end_time"] <- next_free_bordercheck_time + handling_time
      passengers_failed_matrix[i_failed, "bordercheck_handled"] <- next_free_bordercheck
      
      i_failed <- i_failed + 1

    }
    
    # update desks 
    bordercheck_times[next_free_bordercheck] <- bordercheck_times[next_free_bordercheck] + handling_time
    
    if((i_desk + i_failed - 2) == n_passengers_desk + n_passengers_failed){
      still_passengers_left <- FALSE
    }
    
  }
  return(list(passengers_desk_matrix = passengers_desk_matrix, 
              passengers_failed_matrix = passengers_failed_matrix))
}

immigration_queue <- function(passengers, 
                              bordercheck_desks, 
                              bordercheck_egates, 
                              egate_uptake_prop, 
                              target_eligibility,
                              egate_failure_prop, 
                              failed_egate_priority, 
                              seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  passengers <- passengers %>% 
    mutate(base_egate_eligibility = get_egate_eligible(nationality),
           egate_eligibility = sim_boosted_egate_eligible(base_egate_eligibility, target_eligibility),
           bordercheck_start_time = numeric(n()), 
           bordercheck_end_time = numeric(n()),
           bordercheck_handled = numeric(n())) %>% 
    mutate(egate_used = sim_egate_usage(egate_eligibility, egate_uptake_prop)) %>% 
    mutate(egate_failed = sim_egate_failure(egate_used, egate_failure_prop)) 
  
  passengers_egate <- passengers %>% 
    filter(egate_used == "egate") %>% 
    mutate(egate_handling_time = sim_egate_handling_times(bordercheck_egates,
                                                          n_passengers = n()))
  egate_numeric_columns <- c("route_datetime_int", "egate_handling_time", "bordercheck_start_time", 
                             "bordercheck_end_time", "bordercheck_handled")
  passengers_egate_matrix <- passengers_egate %>%
    get_numeric_matrix(egate_numeric_columns)
    
  desk_numeric_columns <- c("route_datetime_int", "bordercheck_start_time", "bordercheck_end_time",
                            "bordercheck_handled")
  passengers_desk <- filter(passengers, egate_used == "desk") 
  passengers_desk_matrix <- get_numeric_matrix(passengers_desk, desk_numeric_columns)
  
  #### get all the egate passengers through
  passengers_egate_matrix <- sim_egate_queue(passengers_egate_matrix, bordercheck_egates)
  passengers_egate[, egate_numeric_columns] <- passengers_egate_matrix
  
  # at this point the egate passengers are processed
  # now deal with the failed passengers
  passengers_failed <- passengers_egate %>% 
    filter(egate_failed == "failed") %>% 
    mutate(bordercheck_egate_end_time = bordercheck_end_time) %>% 
    arrange(bordercheck_end_time)
  
  failed_numeric_columns <- c("bordercheck_egate_end_time", "bordercheck_start_time", "bordercheck_end_time",
                              "bordercheck_handled")
  passengers_failed_matrix <- get_numeric_matrix(passengers_failed, failed_numeric_columns)
  
  ### now let's look at the desk queue, where we input the failed egate passengers
  
  simulated_desk_queue <- sim_desk_queue(passengers_desk_matrix,
                                         passengers_failed_matrix,
                                         bordercheck_desks,
                                         failed_egate_priority)
  
  passengers_failed[, failed_numeric_columns] <- simulated_desk_queue$passengers_failed_matrix
  passengers_desk[, desk_numeric_columns] <- simulated_desk_queue$passengers_desk_matrix
  
  passengers_egate$egate_handling_time <- NULL
  passengers_failed$egate_handling_time <- NULL
  passengers_failed$bordercheck_egate_end_time <- NULL
  
  passengers <- bind_rows(passengers_egate[passengers_egate$egate_failed == "passed", ], 
                      passengers_desk, 
                      passengers_failed)
  passengers <- passengers %>% 
    arrange(route_datetime_int)
  
  return(passengers)
  
}


