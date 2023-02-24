library(tidyverse)

simulate_data <- function(seed = 1234) {
  simulated_data <- tibble(
    sim_x = rnorm(100)
  )
  write_csv(simulated_data, here("processed_data/example_simulated_data.csv"))
  return(simulated_data)
}

simple_arrivals_sim <- function(inter_arrival_rate,
                                n_customers, 
                                seed = 1234, 
                                ...) {
  set.seed(seed)
  inter_arrivals <- rexp(n_customers, inter_arrival_rate) # vector of inter-arrival times
  arrivals <- cumsum(inter_arrivals) # vector of passenger arrival times
  
  return(data.frame(arrival_time = arrivals))
}

simulate_arrivals <- function(arrival_sim_params_file) {
  arrival_sim_params <- read_csv(arrival_sim_params_file) # read_csv(here("params/sim_params/arrival_sim_params.txt")) # #
  output <- list()
  
  # For each simulation setting, run a simulation of arrivals and save the output
  for (row in seq_len(nrow(arrival_sim_params))) {
    param_vector <- as.vector(arrival_sim_params[row, ])
    names(param_vector) <- colnames(arrival_sim_params)
    output[[paste0("sim_", row)]] <- get(param_vector[["arrival_sim_fun"]])(
      inter_arrival_rate = param_vector[["inter_arrival_rate"]],
      n_customers = param_vector[["n_customers"]],
      seed = param_vector[["seed"]])
    write_csv(output[[paste0("sim_", row)]], 
              here(paste0("processed_data/arrivals_sim_", row, ".csv")))
  }
  return(output)
}

simple_queue_sim <- function(arrivals, 
                             service_rate, 
                             n_desks,
                             max_start_time,
                             seed = 1234,
                             ...) {
  # Initialize variables
  set.seed(1234)
  t <- 0 # current time
  queue <- c() # queue of customers waiting for service
  queue_length <- c() # queue length when customers join
  desks <- rep(0, n_desks) # vector of desk finish times
  
  
  # Initialize data frame for output
  output <- tibble(arrival_time = vector(),
                   start_time = vector(),
                   completion_time = vector(),
                   queued = vector())
  customer_index <- 1 # index for customers
  
  # Loop through time until max_start_time
  while (t < max_start_time) {
    # Determine the next event time and its type
    next_arrival <- min(arrivals[customer_index], max_start_time, na.rm = TRUE) # next arrival time
    next_departure <- min(desks[desks > t], max_start_time) # next desk finish time
    next_event_time <- min(next_arrival, next_departure, max_start_time) # next event time
    if (next_event_time >= max_start_time) break # if there are no events left, exit loop
    
    
    # Update queue and desks based on next event type
    if (next_event_time == next_arrival) {
      # Customer arrives
      t <- next_arrival # update current time
      if (sum(desks <= t) > 0) {
        # There is an idle desk, so the customer starts service immediately
        desk_idx <- which(desks <= t)[1] # first idle desk
        desks[desk_idx] <- t + rexp(1, service_rate) # desk finish time
        output <- output %>% 
          add_row(arrival_time = t,
                  start_time = t,
                  completion_time = desks[desk_idx],
                  queued = 0)
        
      } else {
        # All desks are busy, so the customer joins the queue
        queue <- c(queue, t)
        queue_length <- c(queue_length, length(queue))
      }
      customer_index <- customer_index + 1 # increment index for next customer
      # if (customer_index > n_customers) break # if all customers have arrived, exit loop
      # arrivals[i] <- rexp(1, lambda) # generate next inter-arrival time
    } else {
      # Server finishes serving a customer
      t <- next_departure # update current time
      desk_idx <- which(desks == next_departure)[1] # index of desk that finished
      # desks[desk_idx] <- 0 # reset desk finish time
      if (length(queue) > 0) {
        # There is a customer waiting in the queue, so they start service
        customer <- queue[1] # first customer in queue
        queued <- queue_length[1]
        if (length(queue) > 1){
          queue <- queue[-1]
          queue_length <- queue_length[-1]
        } else {
          queue <- c()
          queue_length <- c()
        }
        desks[desk_idx] <- t + rexp(1, service_rate) # desk finish time
        output <- output %>% 
          add_row(arrival_time = customer,
                  start_time = t,
                  completion_time = desks[desk_idx],
                  queued = queued)
      }
    }
  }
  
  # Return the output data frame
  return(output)
}

simulate_queue <- function(queue_sim_params_file, arrivals) {
  queue_sim_params <- read_csv(here(queue_sim_params_file)) # read_csv(here("params/sim_params/queue_sim_params.txt")) # 
  output <- list()
  
  # For each simulation setting, run a simulation of arrivals and save the output
  for (row in seq_len(nrow(queue_sim_params))) {
    param_vector <- as.vector(queue_sim_params[row, ])
    names(param_vector) <- colnames(queue_sim_params)
    arrivals <- arrivals[[param_vector[["arrivals"]]]]
    output[[paste0("sim_", row)]] <- get(param_vector[["queue_sim_fun"]])(
      arrivals = arrivals$arrival_time,
      service_rate = param_vector[["service_rate"]],
      n_desks = param_vector[["n_desks"]],
      max_start_time = param_vector[["max_start_time"]],
      seed = param_vector[["seed"]])
    write_csv(output[[paste0("sim_", row)]], 
              here(paste0("processed_data/queue_sim_", row, ".csv")))
  }
  return(output)
}