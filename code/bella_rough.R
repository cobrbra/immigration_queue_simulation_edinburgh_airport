print("Hi, I'm Bella")

library("ggplot2")
library("here")

plot1 <- ggplot(aes(x = Sepal.Length, y = Sepal.Width), 
                data = iris) +
  geom_point()
plot1

ggsave(here("figures/plot_1_b.png"))



### try out queuing (from arriving at the front of the queue) ###
# 4th process # 

library("stringr")

set.seed(4)


# passengers 

N_passenger <- 1000
lambda_passenger <- 5

passengers <- data.frame(Passenger.ID = paste0("C", str_pad(1:N_passenger, 6, pad = "0")),
                        Time.Arrival = cumsum(rexp(n = N_passenger, rate = lambda_passenger)),
                        Time.Desk.Start = numeric(N_passenger),
                        Time.Desk.End = numeric(N_passenger),
                        Desk.Handled = character(N_passenger) )


# Desks

N_desk <- 5
mu_desk <- pmax(0.5, rnorm(N_desk, mean = 1, sd = 0.2))
Desk.ID <- paste0("D", str_pad(1:N_desk, 2, pad = "0"))

desks <- list(N_desk = N_desk, 
              mu_desk = mu_desk,
              Desk.ID = Desk.ID)


# Function


queue <- function(passengers, desks){
  
  # Initialise variables
  
  # desks
  N_desk <- desks$N_desk 
  mu_desk <- desks$mu_desk
  Desk.ID <- desks$Desk.ID
  desk_times <- rep(0, times = N_desk)
  
  # passengers
  N_passenger <- dim(passengers)[1]
  
  
  for(i in 1:N_passenger){ #loop over all passengers
    
    next_arrival_time <- passengers$Time.Arrival[i]
    
    # if a desk is idle, update their tie
    desk_times <- pmax(desk_times, next_arrival_time)
    
    #decide which desk to send the passenger to
    next_free_desk <- which(desk_times == min(desk_times))[1] # works even when some desks have the same time, edit for E-Gates with conditions
    next_free_desk_time <- min(desk_times)
    
    # how long it takes to handle passenger
    handling_time <- rexp(1, mu_desk[next_free_desk])
    
    # update accordingly for passenger
    passengers$Time.Desk.Start[i] <- next_free_desk_time
    passengers$Time.Desk.End[i] <- next_free_desk_time + handling_time
    passengers$Desk.Handled[i] <- desks$Desk.ID[next_free_desk]
    
    # update desk 
    desk_times[next_free_desk] <- desk_times[next_free_desk] + handling_time
    
    
  }
  
  return(passengers)
  
}



passengers <- queue(passengers = passengers, desks = desks)


wait_times <- passengers$Time.Desk.Start - passengers$Time.Arrival

plot(wait_times)
plot(density(wait_times))
summary(wait_times)

tapply(X = wait_times, INDEX = passengers$Desk.Handled, FUN = summary)
