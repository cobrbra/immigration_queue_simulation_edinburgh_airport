

#### WAIT TIMES ####

get_wait_times_raw <- function(passengers){
  
  wait_times <- passengers$bordercheck_start_time - passengers$route_time_int
  
  res <- data.frame(passenger_id = passengers$passenger_id, nationality = passengers$nationality, egate_eligibility = passengers$egate_eligibility,
                    egate_used = passengers$egate_used, egate_failed = passengers$egate_failed, wait_times = wait_times)
  
  return(res)
}



waittimes_summary_helper <- function(waittimes){
  
  wt <- waittimes$wait_times
  
  res <- c(mean(wt), 
           quantile(wt, c(0.5, 0.75, 0, 1)))
  names(res)[1] <- "mean"
  
  return(res)
}



get_wait_times_summary <- function(passengers){
  
  waittimes <- get_wait_times_raw(passengers)
  
  res <- list(used_egate = waittimes_summary_helper(waittimes[passengers$egate_used == "egate", ]),
              used_desk = waittimes_summary_helper(waittimes[passengers$egate_used == "desk", ]),
              egate_failed = waittimes_summary_helper(waittimes[passengers$egate_failed == "failed", ]),
              eligibility_egate = waittimes_summary_helper(waittimes[passengers$egate_eligibility == "eligible", ]), 
              eligibility_noegate = waittimes_summary_helper(waittimes[passengers$egate_eligibility == "not_eligible", ]), 
              coached = waittimes_summary_helper(waittimes[passengers$coached == TRUE, ]),
              contact = waittimes_summary_helper(waittimes[passengers$coached == FALSE, ]), 
              total = waittimes_summary_helper(waittimes))
  
  return(res)
  
  
}




### QUEUE LENGTH ###


queue_length_helper <- function(passengers, input_times){
  
  n_passengers <- dim(passengers)[1]
  n_times <- length(input_times)
  
  times_matrix <- matrix(rep(input_times, each = n_passengers),
                         nrow = n_passengers, byrow = FALSE) 
  arrival_matrix <- matrix(rep(passengers$route_time_int, each = n_times),
                           nrow = n_passengers, byrow = TRUE)
  start_bordercheck_matrix <- matrix(rep(passengers$bordercheck_start_time, each = n_times),
                                     nrow = n_passengers, byrow = TRUE)
  
  res_matrix <- arrival_matrix < times_matrix & start_bordercheck_matrix > times_matrix
  
  res <- colSums(res_matrix)
  
  return(res)  
}



get_queue_length <- function(passengers, input_times){
  
  res <- data.frame(input_times,
                    desk = queue_length_helper(passengers = passengers[passengers$egate_used == "desk", ], input_times = input_times),
                    egate = queue_length_helper(passengers = passengers[passengers$egate_used == "egate", ], input_times = input_times),
                    total = 0)
  res$total <- res$desk + res$egate
  
  return(res)
}











