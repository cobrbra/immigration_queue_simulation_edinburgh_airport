library(tidyverse)
library(here) 
library(scales)
library(readxl)


seed = 1234
n_egates = 10
egate_uptake_prop = .8

if (!is.null(seed)) {set.seed(seed)}

# generate desks
n_desks <- 9
desk_means <- pmax(0, rnorm(n_desks, mean = 90, sd = 5))
desk_ids <- paste0("D", str_pad(1:n_desks, 2, pad = "0"))
bordercheck_desks <- list(n_borderchecks = n_desks, 
                          bordercheck_means = desk_means,
                          bordercheck_sd = 14,
                          bordercheck_ids = desk_ids)

# generate egates
egate_mean <- 45
egate_ids <- paste0("E", str_pad(1:n_egates, 2, pad = "0"))
bordercheck_egates <- list(n_borderchecks = n_egates, 
                           bordercheck_mean = egate_mean,
                           bordercheck_sd = 5,
                           bordercheck_ids = egate_ids)

simulated_aircrafts <- tar_read(future_aircrafts_arrivals) %>% 
  complete_aircrafts_arrivals(tar_read(hubs), tar_read(countries), tar_read(prop_nationality), 
                              delay_dist = tar_read(delay_dist),
                              n_passengers_quantiles = tar_read(n_passengers_quantiles),
                              coached_levels = tar_read(future_coached_levels),
                              seed = seed) 

simulated_passengers_before_routes <- simulated_aircrafts %>% 
  get_passengers_after_aircrafts(seed = seed) 

simulated_passengers <- simulated_passengers_before_routes %>% 
  get_passengers_after_routes(tar_read(coach_dist),
                              tar_read(walk_dist),
                              tar_read(base_walk_dist),
                              seed = seed)


sim_queue_kpis <- function(passengers_after_routes,
                                bordercheck_egates,
                                egate_uptake_prop,
                                kpis,
                                egate_failure_prop,
                                failed_egate_priority,
                                seed,
                                progress_bar = FALSE) {
  
  simulated_queues <- passengers_after_routes %>% 
    nest(route_data = -sched_aircraft_date_posix) %>% 
    mutate(year = format(sched_aircraft_date_posix, "%Y")) %>% 
    mutate(queue_data = map(route_data, 
                            ~ immigration_queue(., bordercheck_desks = bordercheck_desks, 
                                                bordercheck_egates = bordercheck_egates, 
                                                egate_uptake_prop = egate_uptake_prop, 
                                                egate_failure_prop = egate_failure_prop, 
                                                failed_egate_priority = failed_egate_priority, 
                                                seed = seed),
                            .progress = ifelse(progress_bar, "simming queues", FALSE))) %>% 
    select(-route_data) %>% 
    unnest(queue_data) %>% 
    nest(year_data = - year)

  for (kpi in kpis) {
    kpi_func = get(kpi)
    simulated_queues[[kpi]] <- map_dbl(simulated_queues$year_data, kpi_func)
  }
  
  simulated_queue_kpis <- simulated_queues %>% 
    select(-year_data)
  
  return(simulated_queue_kpis)
}



simulated_kpi <- simulated_passengers %>% 
  sim_queue_kpis(bordercheck_egates,
                 egate_uptake_prop,
                 kpis = list("mean_wait_time", "sla_wait_time"),
                 egate_failure_prop = tar_read(egate_failure_prop),
                 failed_egate_priority = tar_read(failed_egate_priority),
                 seed = seed,
                 progress_bar = TRUE)
  



  

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



delay_times <- tar_read(observed_aircrafts_arrivals) %>% 
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
