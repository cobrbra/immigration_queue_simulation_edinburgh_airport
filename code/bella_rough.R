print("Hi, I'm Bella")

library("ggplot2")
library("here")
library("stringr")
library("lubridate")
library(tidyverse)
library(scales)


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
  n_passengers = 160 - rpois(n = n_aircrafts, lambda = 10),
  coached = sample(c(TRUE, FALSE), size = n_aircrafts, replace = TRUE)
)


aircrafts$taxi_time <- get_taxi_time(aircrafts$coached)
aircrafts$walk_time <- get_walk_time(aircrafts$coached)

passengers <- get_passengers_from_aircrafts(aircrafts)

check_passengers_from_route(passengers)

arrivals_at_hall <- route_process(passengers = passengers, seed = 4)



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


###############
##### EDA #####
###############


dat1 <- targets::tar_read(aircrafts_observed_arrivals_processed)
str(dat1)

dat2 <- dat1[order(dat1$t_actual), ]


### Departure ###

# all vs relevant
dim(dat1)[1]
bool_nonUKIE <- !dat1$dep_country %in% c("ENGLAND", "ENGALND", "UK", "NORTH IRELAND", "IRELAND")
sum(bool_nonUKIE)
dat1$nonUKIE <- factor(bool_nonUKIE, levels = c("TRUE", "FALSE"), labels = c("nonUKIE", "UKIE"))

# Countries
sort(table(dat1$dep_country), decreasing = TRUE)

# England, Engalnd, UK
sort(table(dat1[dat1$dep_country %in% c("ENGLAND"), "dep_airport"]), decreasing = TRUE)
sort(table(dat1[dat1$dep_country %in% c("ENGALND"), "dep_airport"]), decreasing = TRUE)
sort(table(dat1[dat1$dep_country %in% c("UK"), "dep_airport"]), decreasing = TRUE)

# Airports
head(sort(table(dat1$dep_airport), decreasing = TRUE), 10)


### Aircraft ###

sort(table(dat1$ac_type), decreasing = TRUE)
# Boeing 737-800 is the most common

# De Havilland Canada DHC-8 (propeller airplane)
sort(table(dat1$dep_airport[dat1$ac_type == "DH8D"]), decreasing = TRUE)
# used mostly for short UK flights

# max passengers
plot(density(dat1$max_passengers[bool_nonUKIE]), main = "Max passengers", col = "blue", lwd = 2)
lines(density(dat1$max_passengers[!bool_nonUKIE]), col = "grey", lwd = 2)
legend("topright", legend = c("non UKIE", "UKIE"), col = c("blue", "grey"), lwd = 2)
# 70 is exactly DH8D
# 162 is the B738


### Time ###

# UKIE vs non UKIE
plot(density(dat1$actual_arrival_time[bool_nonUKIE]), main = "actual_arrival_time", col = "blue", lwd = 2)
lines(density(dat1$actual_arrival_time[!bool_nonUKIE]), col = "grey", lwd = 2)

# actual vs scheduled
plot(density(dat1$sched_arrival_time[bool_nonUKIE]), main = "Scheduled vs actual", col = "blue", lwd = 2, lty = 2)
lines(density(dat1$actual_arrival_time[bool_nonUKIE]), col = "blue", lwd = 2)

plot(density(dat1$sched_arrival_time[!bool_nonUKIE]), main = "Scheduled vs actual", col = "grey", lwd = 2, lty = 2)
lines(density(dat1$actual_arrival_time[!bool_nonUKIE]), col = "grey", lwd = 2)

# inter arrival times
interarrivals <- diff(dat2$t_actual)
summary(interarrivals) # most common: 180 = 3 minutes
plot(density(interarrivals), main = "Interarrivals")

# plot the log
plot(density(log(interarrivals + 1)), main = "log(Interarrivals + 1)", lwd = 2)

xx <- seq(from = 0, to = 10, length.out = 200)
yy <- dnorm(x = xx, mean = 5.7, sd = 1)
lines(x = xx, y = yy, col = "green", lty = 3, lwd = 1.5)

# date
plot(table(dat1$actual_arrival_date), type = "l")
min(table(dat1$actual_arrival_date))
which.min(table(dat1$actual_arrival_date)) # 2019-12-25 

# day of the week (count)
weekdays <- weekdays(dat1$actual_arrival_date, abbreviate = TRUE)
weekdays <- factor(weekdays, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ordered = TRUE)
plot(table(weekdays))
dat1$weekdays <- weekdays
weekend <- ifelse(weekdays %in% c("Sat", "Sun"), weekdays, "Weekday")
dat1$weekend <- factor(weekend, levels = c("Weekday", "6", "7"), labels = c("Weekday", "Sat", "Sun"), ordered = TRUE)

# day of the week (density over time)
p_weekday <- ggplot(data = dat1, aes(x = actual_arrival_time, col = weekdays)) +
  geom_density()
p_weekday


# weekday vs Sat Sun
p_weekend <- ggplot(data = dat1, aes(x = actual_arrival_time, col = weekend )) +
  geom_density() + facet_wrap(~ nonUKIE)
p_weekend

# weekday vs Sat Sun
p_weekend_alt <- dat1 %>%
  mutate(actual_arrival_time = as.POSIXct(actual_arrival_time, origin = "1970-01-01", tz = "GMT")) %>%
  ggplot(aes(x = actual_arrival_time, col = weekend )) +
  geom_density() + 
  facet_wrap(~ nonUKIE) + 
  scale_x_datetime(labels = date_format('%H:%M'), 
                   breaks = as.POSIXct(c("1970-01-01 06:00", 
                                         "1970-01-01 12:00",
                                         "1970-01-01 18:00"), tz = "GMT"))
p_weekend_alt


plot(table(dat1$weekdays, dat1$nonUKIE), main = "Split per Weekday")
plot(table(dat1$weekend, dat1$nonUKIE), main = "Split per Weekday")



     