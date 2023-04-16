library(here)
library(targets)
library(tidyverse)
library(showtext)

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

sim_settings <- generate_sim_settings()
sim_results <- sim_analysis_data(sim_settings,
                                 tar_read(future_aircrafts_arrivals), 
                                 hubs = tar_read(hubs), 
                                 countries = tar_read(countries), 
                                 prop_nationality = tar_read(prop_nationality), 
                                 delay_dist = tar_read(delay_dist), 
                                 n_passengers_quantiles = tar_read(n_passengers_quantiles), 
                                 coached_levels = tar_read(coached_levels), 
                                 coach_dist = tar_read(coach_dist), 
                                 walk_dist = tar_read(walk_dist), 
                                 base_walk_dist = tar_read(base_walk_dist))

sim_results$queue_length_data[[1]] %>% 
  pivot_longer(c(desk_queue_length, egate_queue_length), names_to = "Check Type", values_to = "Queue Length") %>% 
  mutate(`Check Type` = if_else(`Check Type` == "desk_queue_length", "Desk", "eGate")) %>% 
  ggplot(aes(x = queue_length_datetime_posix, y = `Queue Length`, colour = `Check Type`)) + 
  geom_point() + 
  facet_wrap(~year, scales = "free_x", nrow = 1) +
  theme_minimal()






























