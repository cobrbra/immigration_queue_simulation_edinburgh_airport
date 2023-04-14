### QUEUE LENGTH ###


get_queue_length_at_time_point <- function(time_point, passengers) {
  not_yet_done <- passengers$bordercheck_start_time > time_point
  not_yet_started <- passengers$route_datetime_int < time_point
  return(sum(not_yet_done & not_yet_started))
}

get_queue_lengths <- function(passengers, input_time_interval = 15*60){
  queue_lengths <- data.frame(
    queue_length_datetime_int = input_time_interval * unique(
      round(passengers$route_datetime_int/input_time_interval)
    )
  ) %>% 
    mutate(
      desk_queue_length = map_int(
        queue_length_datetime_int,
        ~ get_queue_length_at_time_point(., filter(passengers, egate_used == "desk"))
        ),
      egate_queue_length = map_int(
        queue_length_datetime_int,
        ~ get_queue_length_at_time_point(., filter(passengers, egate_used == "egate"))
        )
      )
  return(queue_lengths)
}

sim_queue_kpis <- function(passengers_after_routes,
                           bordercheck_egates,
                           egate_uptake_prop,
                           wait_time_kpis,
                           queue_length_kpis,
                           egate_failure_prop,
                           failed_egate_priority,
                           seed,
                           progress_bar = FALSE) {
  
  simulated_queues <- passengers_after_routes %>% 
    nest(route_data = -sched_aircraft_date_posix) %>% 
    mutate(year = format(sched_aircraft_date_posix, "%Y")) %>% 
    mutate(queue_data = map(route_data, 
                            ~ immigration_queue(., bordercheck_desks = bordercheck_desks, 
                                                bordercheck_egates = bordercheck_egates, 
                                                egate_uptake_prop = egate_uptake_prop, 
                                                egate_failure_prop = egate_failure_prop, 
                                                failed_egate_priority = failed_egate_priority, 
                                                seed = seed),
                            .progress = ifelse(progress_bar, "simulating queues", FALSE))) %>% 
    select(-route_data) %>% 
    unnest(queue_data) %>% 
    nest(year_data = - year) %>% 
    mutate(queue_lengths = map(year_data, get_queue_lengths, 
                               .progress = ifelse(progress_bar, "getting queue lengths", FALSE)))
  
  for (kpi in wait_time_kpis) {
    kpi_func = get(kpi)
    simulated_queues[[kpi]] <- map_dbl(simulated_queues$year_data, kpi_func)
  }
  
  for (kpi in queue_length_kpis) {
    kpi_func = get(kpi)
    simulated_queues[[kpi]]
  }
  
  simulated_queue_kpis <- simulated_queues %>% 
    select(-c(year_data, queue_lengths))
  
  return(simulated_queue_kpis)
}

mean_wait_time <- function(queue_data) {
  return(mean(queue_data$bordercheck_start_time - queue_data$route_datetime_int))
}

sla_wait_time <- function(queue_data, threshold_minutes = 60) {
  return(mean((queue_data$bordercheck_start_time - queue_data$route_datetime_int) > threshold_minutes*60))
}


# NOTE: I've muted out the wait times functions below,
# because I'm changing the strategy of how we use KPIs.
# I'm keeping them here commented as a reminder of what
# Bella thought were useful metrics to track.

# #### WAIT TIMES ####
# 
# get_wait_times_raw <- function(passengers){
#   
#   wait_times <- passengers$bordercheck_start_time - passengers$route_datetime_int
#   
#   res <- data.frame(passenger_id = passengers$passenger_id, nationality = passengers$nationality, egate_eligibility = passengers$egate_eligibility,
#                     egate_used = passengers$egate_used, egate_failed = passengers$egate_failed, wait_times = wait_times)
#   
#   return(res)
# }
# 
# 
# 
# waittimes_summary_helper <- function(waittimes){
#   
#   wt <- waittimes$wait_times
#   
#   res <- c(mean(wt), 
#            quantile(wt, c(0.5, 0.75, 0, 1)))
#   names(res)[1] <- "mean"
#   
#   return(res)
# }
# 
# 
# 
# get_wait_times_summary <- function(passengers){
#   
#   waittimes <- get_wait_times_raw(passengers)
#   
#   res <- list(used_egate = waittimes_summary_helper(waittimes[passengers$egate_used == "egate", ]),
#               used_desk = waittimes_summary_helper(waittimes[passengers$egate_used == "desk", ]),
#               egate_failed = waittimes_summary_helper(waittimes[passengers$egate_failed == "failed", ]),
#               eligibility_egate = waittimes_summary_helper(waittimes[passengers$egate_eligibility == "eligible", ]), 
#               eligibility_noegate = waittimes_summary_helper(waittimes[passengers$egate_eligibility == "not_eligible", ]), 
#               coached = waittimes_summary_helper(waittimes[passengers$coached == TRUE, ]),
#               contact = waittimes_summary_helper(waittimes[passengers$coached == FALSE, ]), 
#               total = waittimes_summary_helper(waittimes))
#   
#   return(res)
#   
#   
# }







