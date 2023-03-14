library(tidyverse)
library(scales)

set.seed(2)

n_aircrafts <- 5

aircrafts <- data.frame(
  aircraft_id = paste0("A", str_pad(1:n_aircrafts, 3, pad = "0")),
  dep_country = "UK",
  dep_airport = c("LGW", rep("AMS", n_aircrafts - 1)),
  ac_type = "A320",
  des_rwy = 1,
  t_sched = cumsum(rexp(n = n_aircrafts, rate = 1e-4)),
  max_passengers = 150,
  n_passengers = runif(n_aircrafts, 100, 150),
  coached = sample(c(TRUE, FALSE), size = n_aircrafts, replace = TRUE),
  taxi_time = NA,
  walk_time = NA
) %>% mutate(
  t_actual = t_sched + rexp(n = n_aircrafts, rate = 5e-4)
) %>% mutate(
  sched_arrival_datetime = as.POSIXct(t_sched, origin = '2019-03-10 00:00:00'),
  actual_arrival_datetime = as.POSIXct(t_actual, origin = '2019-03-10 00:00:00'),
  sched_arrival_time = t_sched,
  actual_arrival_time = t_actual,
) %>% mutate(
  sched_arrival_date = as.Date(sched_arrival_datetime),
  actual_arrival_date = as.Date(actual_arrival_datetime)
) 

aircrafts %>% 
  ggplot(aes(x = sched_arrival_datetime, y = actual_arrival_datetime, colour = coached)) + geom_point() 

aircrafts %>% 
  mutate(taxi_time = get_taxi_time(coached),
         walk_time = get_walk_time(coached),
         n_passengers = get_n_passengers(max_passengers, load_factor_mean = .8, load_factor_sd = .2),
         airport_classification = get_airport_classification(dep_country, 
                                                             dep_airport, 
                                                             tar_read(EU_hubs), 
                                                             other_hubs = tar_read(other_hubs), 
                                                             ))

aircrafts %>% 
  mutate(sched_arrival_time = as.POSIXct(sched_arrival_time, origin = "1970-01-01")) %>% 
  ggplot(aes(x = sched_arrival_time, y = n_passengers)) + 
  geom_point() +
  scale_x_datetime(breaks = date_breaks('1 hour'),
                   labels = date_format('%H:%M'))

coached_levels <- aircrafts %>% 
  arrange(desc(coached)) %>% 
  pull(aircraft_id)

arrivals_at_hall %>% 
  mutate(`Aircraft ID` = aircraft_id,
         coached = if_else(coached, "Coached", "Contact"),
         stack_group = factor(aircraft_id, levels = coached_levels)) %>% 
  ggplot(aes(x = arrival_at_hall, 
             fill = `Aircraft ID`,
             group = stack_group,
             linetype = coached)) + 
  geom_density(position = "stack") +
  labs(x= "Arrival time in Hall", 
       y = "Arrival Density",
       title = "Coached arrivals occur in bulk; contact passengers are spread out") + 
  theme_light() +
  scale_linetype_discrete(name = "")
