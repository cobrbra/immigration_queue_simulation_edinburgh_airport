library(tidyverse)
library(here) 
library(scales)
library(readxl)

n_desks <- 9
desk_means <- pmax(70, rnorm(n_desks, mean = 90, sd = 5))
desk_ids <- paste0("D", str_pad(1:n_desks, 2, pad = "0"))

bordercheck_desks <- list(n_borderchecks = n_desks, 
                          bordercheck_means = desk_means,
                          bordercheck_sd = 14,
                          bordercheck_ids = desk_ids)

n_egates <- 10
eGate_mean <- 45
eGate_ids <- paste0("E", str_pad(1:n_egates, 2, pad = "0"))

bordercheck_egates <- list(n_borderchecks = n_egates, 
                           bordercheck_mean = eGate_mean,
                           bordercheck_sd = 5,
                           bordercheck_ids = eGate_ids)

seed <- 1234
simulate_future_passengers <- tar_read(future_aircrafts_arrivals) %>% 
  mutate(aircraft_datetime_int = simulate_delay_times(flight_id, .21, .58, .21, 50*60, 21*60, seed = seed) + sched_aircraft_datetime_int) %>% 
  get_datetime_alternates(column_prefixes = c("aircraft")) %>% 
  mutate(Year = format(aircraft_date_posix, format = "%Y")) %>% 
  group_by(Year) %>% 
  nest() %>% 
  inner_join(tar_read(future_coached_levels)) %>% 
  filter(coached_status == "Coached") %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(coached = get_coached_status(flight_id, prob_coached = Percent), seed = seed) %>% 
  simulate_future_airport_classification(tar_read(n_passenger_quantiles), seed = seed) %>% 
  get_passengers_after_aircrafts(tar_read(EU_plus_hubs), tar_read(other_hubs),
                                 tar_read(prop_nationality), tar_read(UK_plus_countries),
                                 tar_read(EU_plus_countries), tar_read(load_factor_mean), 
                                 tar_read(load_factor_sd), seed = seed) 

simulate_future_routes <- simulate_future_passengers %>% 
  get_passengers_after_route(seed = seed) #%>% 
  group_by(route_date_posix) %>% 
  nest() %>% 
  mutate(queue_data = map(data, 
                          ~ immigration_queue(., bordercheck_desks = bordercheck_desks, 
                                              bordercheck_egates = bordercheck_egates, 
                                              egate_uptake_prop = .8, 
                                              egate_failure_prop = tar_read(egate_failure_prop), 
                                              failed_egate_priority = tar_read(failed_egate_priority), 
                                              seed = seed))) %>% 
  mutate(prop_failed_queue_time = unlist(map(queue_data, ~ mean((.$bordercheck_start_time - .$route_datetime_int) >= 15*60)))) %>% 
  select(-data) %>% 
  unnest(queue_data)

simulate_future %>% 
  mutate(wait_time = bordercheck_start_time - route_datetime_int,
         Year = format(route_date_posix, format = "%Y")) %>% 
  group_by(Year, egate_used) %>% 
  nest() %>% 
  mutate(prop_failing_sla = unlist(map(data, ~ mean(.$wait_time >= 15*60)))) %>% 
  ggplot(aes(x = Year, y = prop_failing_sla, fill = egate_used)) + 
    geom_col(position = "dodge") + 
    scale_fill_manual(values = edi_airport_colours) +
    # theme_edi_airport() +
    labs(y = "Proportion of passengers with wait times breaching SLA")
  

input_times <- seq(from = as.numeric(as.POSIXct("2023-07-10 00:00:00")) , to =  as.numeric(as.POSIXct("2023-07-11 00:00:00")), by = 900)
queue_lengths <- get_queue_length(passengers = ex_day_2023_immigration, input_times = input_times) %>% 
  mutate(input_datetime_int = input_times) %>% 
  get_datetime_alternates(column_prefixes = c("input"))

  
ex_day_2023_passengers %>% 
  mutate(coached = factor(if_else(coached, "Coached", "Contact"),
                          levels = c("Contact", "Coached"))) %>% 
  ggplot() + 
    geom_histogram(
      mapping = aes(x = aircraft_datetime_posix, fill = coached),
      position = position_stack(reverse = TRUE)
    ) + 
    labs(x = "Date and Time") + 
    # theme_edi_airport()0 +
    theme(legend.title = element_blank(),
          legend.position = "top") +
    scale_fill_manual(values = edi_airport_colours) 

queue_lengths %>% 
  pivot_longer(cols = c(desk, egate), names_to = "Check Type", values_to = "Queue Length") %>%   
  ggplot(aes(x = input_datetime_posix, y = `Queue Length`, colour = `Check Type`)) + 
  geom_point() + 
  geom_hline(yintercept = 650, linetype = 2, colour = "red") +
  scale_colour_manual(values = edi_airport_colours) + 
  labs(x = "Date and Time") + 
  theme(legend.position = "bottom")
ggsave(filename = here("figures/rough_arrivals_queue_comp.png"), rough_arrivals_queue_comp, width = 7, height = 7)

ex_week_2023_passengers %>% 
  ggplot(aes(x = aircraft_datetime_posix)) + geom_histogram()



delay_times <- tar_read(aircrafts_observed_arrivals) %>% 
  mutate(Year = format(sched_aircraft_datetime_posix, format = "%Y")) %>% 
  filter(Year == 2019,
         sched_aircraft_date_posix >= as.Date("2019-06-01"),
         sched_aircraft_date_posix <= as.Date("2019-08-31")) %>% 
  mutate(actual_delay = as.numeric(aircraft_datetime_posix - sched_aircraft_datetime_posix)) %>% 
  select(flight_id, sched_aircraft_datetime_posix, actual_delay) %>% 
  mutate(simulated_delay = simulate_delay_times(flight_id, prop_flights_delayed = .21, 
                                                prop_flights_on_time = .58, prop_flights_early = .21, 
                                                mean_delay_time = 50*60,
                                                mean_early_time = 21*60)) %>% 
  pivot_longer(col = c(simulated_delay, actual_delay), names_to = "delay_type", values_to = "delay_time")

delay_times %>% 
  ggplot(aes(x = delay_time, colour = delay_type)) + geom_density() + #+ facet_wrap(~delay_type, scales = "free")
    theme_minimal()
