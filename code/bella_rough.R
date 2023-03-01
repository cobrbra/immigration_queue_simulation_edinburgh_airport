print("Hi, I'm Bella")

library("ggplot2")
library("here")

plot1 <- ggplot(aes(x = Sepal.Length, y = Sepal.Width), 
                data = iris) +
  geom_point()
plot1

ggsave(here("figures/plot_1_b.png"))



### try out queuing ###

library("stringr")

set.seed(4)


# customers 

N_customer <- 1000
lambda_customer <- 5

customers <- data.frame(Customer.ID = paste0("C", str_pad(1:N_customer, 6, pad = "0")),
                        Time.Arrival = cumsum(rexp(n = N_customer, rate = lambda_customer)),
                        Time.Desk.Start = numeric(N_customer),
                        Time.Desk.End = numeric(N_customer),
                        Desk.Handled = character(N_customer) )


# Desks

N_desk <- 5
mu_desk <- pmax(0.5, rnorm(N_desk, mean = 1, sd = 0.2))
Desk.ID <- paste0("D", str_pad(1:N_desk, 2, pad = "0"))

desks <- list(N_desk = N_desk, 
              mu_desk = mu_desk,
              Desk.ID = Desk.ID)


# Function


queue <- function(customers, desks){
  
  # Initialise variables
  
  # desks
  N_desk <- desks$N_desk 
  mu_desk <- desks$mu_desk
  Desk.ID <- desks$Desk.ID
  desk_times <- rep(0, times = N_desk)
  
  # customers
  N_customer <- dim(customers)[1]
  
  
  for(i in 1:N_customer){ #loop over all customers
    
    next_arrival_time <- customers$Time.Arrival[i]
    
    # if a desk is idle, update their tie
    desk_times <- pmax(desk_times, next_arrival_time)
    
    #decide which desk to send the customer to
    next_free_desk <- which(desk_times == min(desk_times))[1] # works even when some desks have the same time, edit for E-Gates with conditions
    next_free_desk_time <- min(desk_times)
    
    # how long it takes to handle customer
    handling_time <- rexp(1, mu_desk[next_free_desk])
    
    # update accordingly for customer
    customers$Time.Desk.Start[i] <- next_free_desk_time
    customers$Time.Desk.End[i] <- next_free_desk_time + handling_time
    customers$Desk.Handled[i] <- desks$Desk.ID[next_free_desk]
    
    # update desk 
    desk_times[next_free_desk] <- desk_times[next_free_desk] + handling_time
    
    
  }
  
  return(customers)
  
}



customers <- queue(customers = customers, desks = desks)


wait_times <- customers$Time.Desk.Start - customers$Time.Arrival

plot(wait_times)
plot(density(wait_times))
summary(wait_times)

tapply(X = wait_times, INDEX = customers$Desk.Handled, FUN = summary)
