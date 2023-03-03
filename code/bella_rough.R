print("Hi, I'm Bella")

library("ggplot2")
library("here")
library("stringr")



###########################
####### 2nd Process ####### 
########## Route ##########
###########################


set.seed(4)

n_aircrafts <- 5

aircrafts <- data.frame(
  aircraft_id = paste0("A", str_pad(1:n_aircrafts, 3, pad = "0")),
  aircraft_scheduled = numeric(n_aircrafts),
  aircraft_arrival = cumsum(rexp(n = n_aircrafts, rate = 0.2)), 
  airline = rep("KLM", times = n_aircrafts),
  type = rep("Boing 737-800", times = n_aircrafts),
  capacity = rep(160, times = n_aircrafts),
  passenger_on_board = 160 - rpois(n = n_aircrafts, lambda = 10),
  coached = sample(c(TRUE, FALSE), size = n_aircrafts, replace = TRUE)
)


aircrafts$taxi_time <- get_taxi_time(aircrafts$coached)
aircrafts$walk_time <- get_walk_time(aircrafts$coached)

passengers <- get_passenger_from_aircrafts(aircrafts)

check_passengers_from_route(passengers)

route_process(passengers = passengers, seed = 4)



###########################
####### 4th process ####### 
####### Immigration ####### 
###########################


# simulate the data sets (for trials)

set.seed(4)

N_passenger <- 1000
lambda_passenger <- 5

passengers <- data.frame(passenger_id = paste0("P", str_pad(1:N_passenger, 6, pad = "0")),
                         aircraft_id = character(N_passenger),
                         nationality = character(N_passenger),
                         aircraft_arrival = numeric(N_passenger), 
                         arrival_at_hall = numeric(N_passenger),
                         arrival_at_queue = cumsum(rexp(n = N_passenger, rate = lambda_passenger)),
                         arrival_from_immigration = numeric(N_passenger),
                         queue_length = numeric(N_passenger),
                         e_gate = character(N_passenger)) 


n_checks <- 6
handling_rates <- pmax(0.5, rnorm(n_checks, mean = 1, sd = 0.2))
check_ids <- paste0("CD", str_pad(1:n_checks, 2, pad = "0"))

checks <- list(n_checks = n_checks, 
              handling_rates = handling_rates,
              check_ids = check_ids)



# check if passengers passes
check_passengers_from_immigration(passengers)


# get passengers checked
passengers <- immigration_queue(passengers = passengers, checks = checks, 
                                handling_time_func = get_check_handling_time, seed = 4)

# analysis
wait_times <- passengers$check_end_time - passengers$arrival_at_queue

plot(wait_times)
plot(density(wait_times))
summary(wait_times)

tapply(X = wait_times, INDEX = passengers$check_handled, FUN = summary)
