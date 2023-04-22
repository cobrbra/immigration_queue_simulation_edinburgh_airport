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
             egate_uptake_prop = 1, target_eligibility = 1, 
             egate_failure_prop = tar_read(egate_failure_prop), 
             failed_egate_priority = tar_read(failed_egate_priority),
             seed = 1)   


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
                                 save_data = FALSE)


# EXAMPLE MULTI-SIM EXPERIMENT SPECIFYING TRAJECTORY

specify_sim_settings <- function(n_egates_range,
                                 egate_uptake_range = seq(.7, .99, length.out = 5),
                                 target_eligibility_range = seq(.85, .96, length.out = 5),
                                 year_range = as.character(2023:2027),
                                 n_gen_arrivals = 1,
                                 n_gen_queues = 1,
                                 seed = 1234) {
  set.seed(seed)
  gen_arrivals_seeds <- sample(1:100000, size = n_gen_arrivals)
  gen_queue_seeds <- sample(1:100000, size = n_gen_queues)
  
  specificied_settings <- data.frame(
    year = year_range,
    n_egates = n_egates_range,
    egate_uptake = egate_uptake_range,
    target_eligibility = target_eligibility_range
  )
  
  generated_settings <- crossing(
    gen_arrivals_seed = gen_arrivals_seeds,
    gen_queue_seed = gen_queue_seeds
  )
  
  return(cross_join(specificied_settings, generated_settings))

}

generate_sim_settings <- function(n_egates_range = seq(10, 14, by = 2),
                                  egate_uptake_range = seq(.6, 1., by = .2),
                                  target_eligibility_range = seq(.75, 1., .05),
                                  year_range = as.character(2023:2027),
                                  n_gen_arrivals = 1,
                                  n_gen_queues = 1,
                                  seed = 1234) {
  set.seed(seed)
  gen_arrivals_seeds <- sample(1:100000, size = n_gen_arrivals)
  gen_queue_seeds <- sample(1:100000, size = n_gen_queues)
  sim_settings <- crossing(gen_arrivals_seed = gen_arrivals_seeds, 
                           gen_queue_seed = gen_queue_seeds, 
                           n_egates = n_egates_range, 
                           egate_uptake = egate_uptake_range, 
                           target_eligibility = target_eligibility_range,
                           year = year_range)
  return(sim_settings)
}





















