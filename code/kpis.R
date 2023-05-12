### WAIT TIME ###
mean_wait_time <- function(queue_data) {
  return(mean(queue_data$bordercheck_start_time - queue_data$route_datetime_int)/60)
}

wait_time_60 <- function(queue_data) {
  return(mean((queue_data$bordercheck_start_time - queue_data$route_datetime_int) < 60*60))
}

wait_time_25 <- function(queue_data) {
  return(mean((queue_data$bordercheck_start_time - queue_data$route_datetime_int) < 25*60))
}

wait_time_15 <- function(queue_data) {
  return(mean((queue_data$bordercheck_start_time - queue_data$route_datetime_int) < 15*60))
}

### QUEUE LENGTH ###

get_queue_lengths <- function(passengers,
                                  input_time_interval = 15*60) {
  passengers <- passengers %>%
    mutate(egate_used = factor(egate_used, levels = c("desk", "egate")),
           rounded_bordercheck_start_time = input_time_interval * (
             round(bordercheck_start_time / input_time_interval)),
           rounded_route_datetime_int = input_time_interval * (
             round(route_datetime_int / input_time_interval))) %>% 
    select(egate_used, rounded_route_datetime_int, rounded_bordercheck_start_time)
  
  queue_lengths <- full_join(count(passengers, egate_used, rounded_route_datetime_int),
                             count(passengers, egate_used, rounded_bordercheck_start_time),
                             by = c("rounded_route_datetime_int" = "rounded_bordercheck_start_time", "egate_used"),
                             suffix = c("_route", "_border")) %>% 
    mutate_if(is.numeric, coalesce, 0) %>% 
    arrange(rounded_route_datetime_int) %>% 
    pivot_wider(names_from = egate_used, values_from = c(n_border, n_route), values_fill = 0, names_expand = TRUE) %>% 
    mutate(desk_queue_length = cumsum(n_route_desk - n_border_desk),
           egate_queue_length = cumsum(n_route_egate - n_border_egate)) %>% 
    select(queue_length_datetime_int = rounded_route_datetime_int, desk_queue_length, egate_queue_length) %>% 
    get_datetime_alternates(c("queue_length"))
  
  return(queue_lengths)
  
}

exceeds_overflow <- function(queue_lengths, queue = "desk",
                             hall_split= c(desk = .5, egate = .5),
                             hall_size = 500,
                             overflow_size = 150,
                             mins_per_timepoint = 15) {
  joint_queue <- pmax(queue_lengths$egate_queue_length - hall_split["egate"]*hall_size, 0) + 
                 pmax(queue_lengths$desk_queue_length - hall_split["desk"]*hall_size, 0)
  if (queue == "both") {
    return(mins_per_timepoint * sum(joint_queue > overflow_size))
  }
  queue_exceeds_hall <- queue_lengths[[paste0(queue, "_queue_length")]] > hall_size*hall_split[queue]
  mins_exceeding_overflow = mins_per_timepoint * sum(
    (joint_queue > overflow_size) & queue_exceeds_hall
  )
  
  return(mins_exceeding_overflow)
}

exceeds_contingency <- function(queue_lengths, queue = "desk",
                                hall_split = c(desk = .5, egate = .5),
                                hall_size = 500,
                                contingency_size = 750,
                                mins_per_timepoint = 15) {
  joint_queue <- pmax(queue_lengths$egate_queue_length - hall_split["egate"]*hall_size, 0) + 
                 pmax(queue_lengths$desk_queue_length - hall_split["desk"]*hall_size, 0)  
  if (queue == "both") {
    return(mins_per_timepoint * sum(joint_queue > contingency_size))
  }
  queue_exceeds_hall <- queue_lengths[[paste0(queue, "_queue_length")]] > hall_size * hall_split[queue]
  mins_exceeding_contingency = mins_per_timepoint * sum(
    (joint_queue > contingency_size) & queue_exceeds_hall
  )
  
  return(mins_exceeding_contingency)
}






