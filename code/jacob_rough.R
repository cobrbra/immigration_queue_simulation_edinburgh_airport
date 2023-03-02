library(tidyverse)

# Functions

get_desk_handling_time <- function(desks, desk_id, seed = NULL) {
  if (!is.null(seed)) {set.seed(seed)}
  desk_handling_time <- rexp(1, desks$desk_handling_rates[desk_id])
  
  return (desk_handling_time)
}

queue <- function(passengers, 
                  desks, 
                  handling_time_func = get_desk_handling_time,
                  seed = NULL){
  
  if (!is.null(seed)) {set.seed(seed)}
  n_desks <- desks$n_desks
  desk_times <- rep(0, times = desks$n_desks)
  
  n_passengers <- dim(passengers)[1]
  
  
  for(i in 1:n_passengers){ 
    
    next_arrival_time <- passengers$arrival_time[i]
    
    # if a desk is idle, update their time
    desk_times <- pmax(desk_times, next_arrival_time)
    
    # decide which desk to send the passenger to
    next_free_desk <- which(desk_times == min(desk_times))[1] # TODO: edit for E-Gates with conditions
    next_free_desk_time <- min(desk_times)
    
    # how long it takes to handle passenger
    handling_time <- handling_time_func(desks, next_free_desk)
    
    # update accordingly for passenger
    passengers$desk_start_time[i] <- next_free_desk_time
    passengers$desk_end_time[i] <- next_free_desk_time + handling_time
    passengers$desk_handled[i] <- desks$desk_ids[next_free_desk]
    
    # update desk 
    desk_times[next_free_desk] <- desk_times[next_free_desk] + handling_time
    
    
  }
  
  return(passengers)
  
}

# Passengers 

n_passengers <- 1000
lambda_passenger <- 5

passengers <- data.frame(passenger_id = paste0("P", str_pad(1:n_passengers, 6, pad = "0")),
                         arrival_time = cumsum(rexp(n = n_passengers, rate = lambda_passenger)),
                         desk_start_time = numeric(n_passengers),
                         desk_end_time = numeric(n_passengers),
                         desk_handled = character(n_passengers) )


# Desks

n_desks <- 5
desk_handling_rates <- pmax(0.5, rnorm(n_desks, mean = 1, sd = 0.2))
desk_ids <- paste0("D", str_pad(1:n_desks, 2, pad = "0"))

desks <- list(n_desks = n_desks, 
              desk_handling_rates = desk_handling_rates,
              desk_ids = desk_ids)

# Simulation

passengers <- queue(passengers = passengers, desks = desks, seed = 101)

# Visualisation 

wait_times <- passengers$desk_start_time - passengers$arrival_time

plot(wait_times)
plot(density(wait_times))
summary(wait_times)

tapply(X = wait_times, INDEX = passengers$desk_handled, FUN = summary)



### Suggested changes I've made:
# - I've removed some comments I would describe as 'over-commenting', e.g. "initialising variables",
# "looping over passengers", etc. The general principle being that comments are necessary when the code 
# itself isn't self-explanatory, and that if things like "I'm assigning initial variables" or "I'm looping over 
# passengers" aren't clear from code, then that's something that should be rectified in the code naming and 
# structure. The best code commenting is well-written code etc etc etc...
# - I've standardised variable naming to be snake_case. There's no particular reason for this, and in R camelCase 
# is arguably more standard, but picking a naming convention is nice.
# - I've added plurals to variables storing a plural number, e.g. "n_desk" -> "n_desks"
# - I've renamed 'mu_' variables to be more descriptive.
# - I've moved function definitions to the top of the file (since they're effectively forming a new 'library', 
# they should be with other library imports or called from an external source file)
# - I've made the desk_handling_time generator a separate function. This means: we can experiment with changes when
# we want, it neatens things up inside the queue function (code as documentation once again!), and means just in case
# we ever need seed control e.g. for testing we can have access to it.
# - The desk_handling_time change also means we don't have to unpack all of those internal desks variables inside the 
# queue function. Again, useful if we ever want to change what desks information we store, we keep it modular and in
# get_desk_handling_time.
# - I've changed "C" to "P" for customers -> passengers.
# - I've added an optional random seed to the queue function
