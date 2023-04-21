library(here)
library(targets)
library(tidyverse)
library(showtext)
library(cowplot)

tar_source(files = c(
  here("code/process_data.R"), 
  here("code/step_1_aircraft.R"),
  here("code/step_2_route.R"),
  here("code/step_3_immigration.R"),
  here("code/kpis.R"),
  here("code/analysis.R"),
  here("code/get_figures.R"),
  here("code/get_tables.R")
))


##### EXAMPLE SINGLE DATASET

# generate desks
n_desks <- 9
desk_means <- pmax(0, rep(90, n_desks))#rnorm(n_desks, mean = 90, sd = 5))
desk_ids <- seq_len(n_desks)
bordercheck_desks <- list(n_borderchecks = n_desks, 
                          bordercheck_means = desk_means,
                          bordercheck_sd = 14,
                          bordercheck_ids = desk_ids)

n_egates <- 10
bordercheck_egates = list(
  n_borderchecks = n_egates, 
  bordercheck_mean = 45,
  bordercheck_sd = 5,
  bordercheck_ids = seq_len(n_egates))

simulated_arrivals <- tar_read(future_aircrafts_arrivals) %>% 
  complete_aircrafts_arrivals(tar_read(hubs), tar_read(countries), tar_read(prop_nationality), 
                              tar_read(delay_dist), tar_read(n_passengers_quantiles), 
                              tar_read(load_factor), tar_read(future_coached_levels)) %>% 
  get_passengers_after_aircrafts() %>% 
  get_passengers_after_routes(tar_read(coach_dist), tar_read(walk_dist), tar_read(base_walk_dist))

simulated_queue <- simulated_arrivals %>% 
  sim_queues(bordercheck_desks = bordercheck_desks, 
             bordercheck_egates = bordercheck_egates, 
             egate_uptake_prop = .7, target_eligibility = .85, 
             egate_failure_prop = tar_read(egate_failure_prop), 
             failed_egate_priority = tar_read(failed_egate_priority),
             seed = 1) 

simulated_queue %>% 
  count(year, base_egate_eligibility, egate_eligibility) %>% 
  group_by(year) %>% 
  mutate(n = n / sum(n))

simulated_queue %>% 
  mutate(queue_time = bordercheck_start_time - route_datetime_int) %>% 
  slice_sample(n = 2000) %>% 
  # filter(year == "2023") %>% 
  ggplot(aes(x = route_datetime_posix, y = queue_time / 3600, colour = egate_used)) +
  geom_point() + 
  ylim(0, NA) + 
  facet_wrap(~year, nrow = 1, scales = "free_x")

sim_settings <- generate_sim_settings()
sim_results <- sim_analysis_data(sim_settings,
                                 tar_read(future_aircrafts_arrivals), 
                                 hubs = tar_read(hubs), 
                                 countries = tar_read(countries), 
                                 prop_nationality = tar_read(prop_nationality), 
                                 delay_dist = tar_read(delay_dist), 
                                 n_passengers_quantiles = tar_read(n_passengers_quantiles), 
                                 egate_failure_prop = tar_read(egate_failure_prop),
                                 failed_egate_priority = tar_read(failed_egate_priority),
                                 coached_levels = tar_read(future_coached_levels), 
                                 coach_dist = tar_read(coach_dist), 
                                 walk_dist = tar_read(walk_dist), 
                                 base_walk_dist = tar_read(base_walk_dist),
                                 wait_time_kpis = c("mean_wait_time", "wait_time_60", "wait_time_15"),
                                 save_data = TRUE)

sim_results$queue_length_data[[1]] %>% 
  pivot_longer(c(desk_queue_length, egate_queue_length), names_to = "Check Type", values_to = "Queue Length") %>% 
  mutate(`Check Type` = if_else(`Check Type` == "desk_queue_length", "Desk", "eGate")) %>% 
  ggplot(aes(x = queue_length_datetime_posix, y = `Queue Length`, colour = `Check Type`)) + 
  geom_point() + 
  facet_wrap(~year, scales = "free_x", nrow = 1) +
  theme_minimal()




















