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


tar_make('prop_nationality')
tar_read(prop_nationality)
  

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

count(simulated_queue, egate_eligibility) %>% 
  mutate(n = n / sum(n))

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
                                    n_gen_arrivals = 10,
                                    n_gen_queues = 10)

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
                                 queue_length_kpis = c("queue_length_650", "queue_length_1250"),
                                 save_data = FALSE)

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

hall_split_desk = 0.6
simulated_queue %>% 
  get_queue_lengths() %>% 
  mutate(year = lubridate::year(queue_length_date_posix)) %>% 
  select(-c(queue_length_datetime_int, queue_length_date_posix)) %>% 
  mutate(desk_queue_exceeds_hall = desk_queue_length >= 500*0.6) %>% 
  mutate(egate_queue_exceeds_hall = egate_queue_length >= 500*0.4) %>% 
  mutate(joint_queue_exceeds_overflow = desk_queue_length + egate_queue_length >= 650) %>% 
  count(year, 
        desk_queue_exceeds_hall&joint_queue_exceeds_overflow, 
        egate_queue_exceeds_hall&joint_queue_exceeds_overflow)

