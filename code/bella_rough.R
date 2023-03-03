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

passengers <- data.frame(passenger_id = paste0("P", str_pad(1:N_passenger, 6, pad = "0")),
                         aircraft_id = character(N_passenger),
                         nationality = character(N_passenger),
                         aircraft_arrival = numeric(N_passenger), 
                         arrival_at_hall = numeric(N_passenger),
                         arrival_at_queue = cumsum(rexp(n = N_passenger, rate = lambda_passenger)),
                         arrival_from_immigration = numeric(N_passenger),
                         queue_length = numeric(N_passenger),
                         e_gate = boolean(N_passenger)) 




# Checks

N_desk <- 5
mu_desk <- pmax(0.5, rnorm(N_desk, mean = 1, sd = 0.2))
Check.ID <- paste0("D", str_pad(1:N_desk, 2, pad = "0"))

desks <- list(N_desk = N_desk, 
              mu_desk = mu_desk,
              Check.ID = Check.ID)




passengers <- queue(passengers = passengers, desks = desks)


wait_times <- passengers$Time.Check.Start - passengers$Time.Arrival

plot(wait_times)
plot(density(wait_times))
summary(wait_times)

tapply(X = wait_times, INDEX = passengers$Check.Handled, FUN = summary)
