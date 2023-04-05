library(tidyverse)
library(here) 
library(scales)
library(readxl)

ex_week_2023_aircrafts <- tar_read(future_aircrafts_arrivals) %>% 
  filter(format(sched_aircraft_datetime_posix, format = "%Y") == 2023) %>%
  mutate(aircraft_datetime_int = simulate_delay_times(flight_id, .21, .58, .21, 50*60, 21*60) + sched_aircraft_datetime_int) %>% 
  get_datetime_alternates(column_prefixes = c("aircraft", "sched_aircraft")) %>% 
  mutate(coached = get_coached_status(flight_id)) %>% 
  simulate_future_airport_classification(tar_read(n_passenger_quantiles))

ex_week_2023_passengers <- get_passengers_after_aircrafts(ex_week_2023_aircrafts,
                                                      tar_read(EU_plus_hubs), tar_read(other_hubs),
                                                      tar_read(prop_nationality), tar_read(UK_plus_countries),
                                                      tar_read(EU_plus_countries), tar_read(load_factor_mean), 
                                                      tar_read(load_factor_sd))


ex_week_2023_routes <- get_passengers_after_route(ex_week_2023_passengers)

n_desks <- 9
# desk_rates <- pmax(1/200, rnorm(n_desks, mean = 1/60, sd = 0.01))
desk_rates <- pmax(40, rnorm(n_desks, mean = 45, sd = 10))
desk_ids <- paste0("D", str_pad(1:n_desks, 2, pad = "0"))

bordercheck_desks <- list(n_borderchecks = n_desks, 
                          bordercheck_rates = desk_rates,
                          bordercheck_ids = desk_ids)

n_egates <- 10
# eGate_rates <- pmax(1/120, rnorm(n_egates, mean = 1/30, sd = 0.01))
eGate_rates <- rep(45, n_egates)#pmax(30, rnorm(n_egates, mean = 25, sd = 5))
eGate_ids <- paste0("E", str_pad(1:n_egates, 2, pad = "0"))

bordercheck_egates <- list(n_borderchecks = n_egates, 
                           bordercheck_rates = eGate_rates,
                           bordercheck_ids = eGate_ids)


ex_week_2023_immigration <- immigration_queue(ex_week_2023_routes, bordercheck_desks = bordercheck_desks, 
                                              bordercheck_egates = bordercheck_egates, egate_uptake_prop = 0.8, 
                                              egate_failure_prop = 0.05, egate_failed_passenger_next = 0.75, seed = 6)

input_times <- seq(from = as.numeric(as.POSIXct("2023-07-10 00:00:00")) , to =  as.numeric(as.POSIXct("2023-07-17 00:00:00")), by = 900)
ql <- get_queue_length(passengers = ex_week_2023_immigration, input_times = input_times) %>% 
  mutate(input_datetime_int = input_times) %>% 
  get_datetime_alternates(column_prefixes = c("input"))

ql %>% 
  pivot_longer(cols = c(desk, egate), names_to = "check_type", values_to = "queue_length") %>%  
  ggplot(aes(x = input_datetime_posix, y = queue_length, colour = check_type)) + geom_point()
  
rel_scales <- 5
rough_arrivals_queue_comp <- ggplot() + 
  geom_histogram(
    data = ex_week_2023_passengers,
    mapping = aes(x = aircraft_datetime_posix),
    alpha = 0.5,
    fill = edi_airport_colours[3]
  ) + 
  geom_point(
    data = ql %>% 
      pivot_longer(cols = c(desk, egate), names_to = "check_type", values_to = "queue_length"),
    mapping = aes(x = input_datetime_posix, y = rel_scales*queue_length, colour = check_type)
  ) + 
  scale_y_continuous(
      name = "Queue Length",
      sec.axis = sec_axis( trans=~./rel_scales, name="Passengers arriving")
  ) +
  labs(x = "Date and Time") + 
  theme_edi_airport() +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = edi_airport_colours)
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
