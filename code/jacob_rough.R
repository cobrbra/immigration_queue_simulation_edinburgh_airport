library(here)
library(targets)
library(tidyverse)
library(showtext)
library(cowplot)
library(lubridate)

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
desk_means <- pmax(0, rep(90, n_desks))
bordercheck_desks <- list(n_borderchecks = n_desks, 
                          bordercheck_means = desk_means,
                          bordercheck_sd = 14)

n_egates <- 10
bordercheck_egates = list(
  n_borderchecks = n_egates, 
  bordercheck_mean = 45,
  bordercheck_sd = 5)


simulated_arrivals <- tar_read(future_aircrafts_arrivals) %>% 
  complete_aircrafts_arrivals(tar_read(hubs), tar_read(countries), tar_read(prop_nationality), 
                              tar_read(delay_dist), tar_read(n_passengers_quantiles), 
                              tar_read(load_factor), tar_read(future_coached_levels)) %>% 
  get_passengers_after_aircrafts() %>% 
  get_passengers_after_routes(tar_read(coach_dist), tar_read(walk_dist), tar_read(base_walk_dist))


simulated_queue <- simulated_arrivals %>% 
  immigration_queue(bordercheck_desks = bordercheck_desks, 
             bordercheck_egates = bordercheck_egates, 
             egate_uptake_prop = .7, target_eligibility = .8, 
             egate_failure_prop = tar_read(egate_failure_prop), 
             failed_egate_priority = tar_read(failed_egate_priority))   



# EXAMPLE MULTI-SIM EXPERIMENT

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
                                 save_data = TRUE,
                                 save_dir = here("raw_data/"))


# EXAMPLE MULTI-SIM EXPERIMENT SPECIFYING TRAJECTORY

sim_settings <- specify_sim_settings(n_egates = c(10, 13, 16, 20, 25),
                                    n_gen_arrivals = 2,
                                    n_gen_queues = 2)

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
                                 queue_length_kpis = c("exceeds_overflow", "exceeds_contingency"),
                                 hall_desk_splits = seq(10,90,10),
                                 save_data = TRUE)



#### DELAY DIST FIG

sigma <- 15

bind_rows(
  tar_read(future_aircrafts_arrivals) %>% 
    complete_aircrafts_arrivals(tar_read(hubs), tar_read(countries), tar_read(prop_nationality), 
                                tar_read(delay_dist), tar_read(n_passengers_quantiles), tar_read(load_factor),
                                tar_read(future_coached_levels), seed = 1) %>% 
    select(sched_aircraft_datetime_int,
           aircraft_datetime_int) %>% 
    mutate(actual = "Simulated Future",
           delay = (aircraft_datetime_int - sched_aircraft_datetime_int)/60) %>% 
    mutate(delay_density = approx(x = density(delay)$x,
                                  y = density(delay)$y,
                                  xout = delay)$y,
           delay_status = factor(
             case_when(delay < -15 ~ "Early",
                       delay > 15 ~ "Late",
                       TRUE ~ "On Time"),
             levels = c("Early", "On Time", "Late"))),
  tar_read(filtered_observed_aircrafts_arrivals) %>%
    filter(aircraft_datetime_int != 1657536060) %>% 
    select(sched_aircraft_datetime_int,
           aircraft_datetime_int) %>% 
    mutate(actual = "Historical",
           delay = (aircraft_datetime_int - sched_aircraft_datetime_int)/60) %>% 
    mutate(delay_density = approx(x = density(delay)$x,
                                  y = density(delay)$y,
                                  xout = delay)$y,
           delay_status = factor(
             case_when(delay < -15 ~ "Early",
                       delay > 15 ~ "Late",
                       TRUE ~ "On Time"),
             levels = c("Early", "On Time", "Late")))
) %>% 
  ggplot(aes(x = delay, y = delay_density, colour = actual)) +
  geom_line() +
  facet_wrap(~delay_status, scales = "free_x") +
  theme_bw() +
  theme(legend.title = element_blank()) + 
  labs(x = "Delay (mins)", y = "Density") + 
  # scale_colour_manual(values = edi_airport_colours[c(4,7)]) + 
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = sigma),
                     breaks = round(pseudo_log_trans(sigma = sigma)$inverse(c(-3:-1, -0.25, 0, 0.25, 1:4))))#c(early_breaks, on_time_breaks, late_breaks))




#### EXPERIMENTING TO SET RECOMMENDATIONS AND ASSUMPTIONS


rec_fig_sim_settings <- specify_sim_settings(n_egates_range = c(15, 19, 21, 23, 23), 
                                             egate_uptake_range = seq(.75, .99, length.out = 7)[2:6],
                                             target_eligibility_range = seq(.8, .96, length.out = 7)[2:6],
                                             n_gen_arrivals = 5,
                                             n_gen_queues = 5)
rec_fig_sim_data <- sim_analysis_data(
                             rec_fig_sim_settings,
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
                             wait_time_kpis = c("mean_wait_time", "wait_time_60", "wait_time_15", "wait_time_25"),
                             queue_length_kpis = c("exceeds_overflow", "exceeds_contingency"),
                             hall_desk_splits = seq(25, 75, 25),
                             save_data = FALSE)

rec_fig_sim_data %>% 
  summarise(mean(wait_time_60_egate), .by = ("year"))

rec_fig_sim_data %>% 
  summarise(mean(wait_time_15_egate), .by = ("year"))

rec_fig_sim_data %>% 
  summarise(mean(exceeds_overflow_egate)/60, .by = ("year"))

rec_fig_sim_data %>% 
  summarise(mean(hall_split), .by = ("year"))

minus_rec_fig_sim_settings <- specify_sim_settings(n_egates_range = c(10, 15, 19, 21, 23), 
                                             egate_uptake_range = seq(.75, .99, length.out = 7)[2:6],
                                             target_eligibility_range = seq(.8, .96, length.out = 7)[2:6],
                                             n_gen_arrivals = 5,
                                             n_gen_queues = 5)
minus_rec_fig_sim_data <- sim_analysis_data(
  minus_rec_fig_sim_settings,
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
  wait_time_kpis = c("mean_wait_time", "wait_time_60", "wait_time_15", "wait_time_25"),
  queue_length_kpis = c("exceeds_overflow", "exceeds_contingency"),
  hall_desk_splits = seq(25, 75, 25),
  save_data = FALSE)

minus_rec_fig_sim_data %>% 
  summarise(mean(wait_time_60_egate), .by = ("year"))

minus_rec_fig_sim_data %>% 
  summarise(mean(wait_time_15_egate), .by = ("year"))

minus_rec_fig_sim_data %>% 
  summarise(mean(exceeds_overflow_egate)/60, .by = ("year"))

minus_rec_fig_sim_data %>% 
  summarise(mean(hall_split), .by = ("year"))
