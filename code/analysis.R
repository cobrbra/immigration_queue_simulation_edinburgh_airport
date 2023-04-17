generate_sim_settings <- function(seed = 1234,
                                  n_gen_arrivals = 1,
                                  n_gen_queues = 1,
                                  n_egates_range = seq(10, 14, by = 2),
                                  egate_uptake_range = seq(.6, 1., by = .2),
                                  elig_boost_range = seq(0., 1., .5)) {
  set.seed(seed)
  gen_arrivals_seeds <- sample(1:100000, size = n_gen_arrivals)
  gen_queue_seeds <- sample(1:100000, size = n_gen_queues)
  sim_settings <- crossing(gen_arrivals_seed = gen_arrivals_seeds, 
                           gen_queue_seed = gen_queue_seeds, 
                           n_egates = n_egates_range, 
                           egate_uptake = egate_uptake_range, 
                           elig_boost = elig_boost_range)
  return(sim_settings)
}


sim_analysis_data <- function(sim_settings,
                              aircrafts_arrivals,
                              hubs,
                              countries,
                              prop_nationality,
                              delay_dist,
                              n_passengers_quantiles,
                              coached_levels,
                              coach_dist,
                              walk_dist,
                              base_walk_dist,
                              egate_failure_prop,
                              failed_egate_priority,
                              queue_sample_size = 2500) {
  # generate desks
  n_desks <- 9
  desk_means <- pmax(0, rep(90, n_desks))#rnorm(n_desks, mean = 90, sd = 5))
  desk_ids <- seq_len(n_desks)
  bordercheck_desks <- list(n_borderchecks = n_desks, 
                            bordercheck_means = desk_means,   
                            bordercheck_sd = 14,
                            bordercheck_ids = desk_ids)
  
  sim_settings <- sim_settings %>% 
    mutate(queue_length_data = vector(mode = "list", length = nrow(.)),
           sample_queue_data = vector(mode = "list", length = nrow(.))) %>% 
    nest(non_arrivals_data = -gen_arrivals_seed)
  
  progress_counter <- 0
  for (arrivals_id in seq_len(nrow(sim_settings))) {
    arrivals_seed <- sim_settings$gen_arrivals_seed[[arrivals_id]]
    simulated_passengers <- aircrafts_arrivals %>% 
      complete_aircrafts_arrivals(
        hubs, countries, prop_nationality,
        delay_dist = delay_dist, 
        n_passengers_quantiles = n_passengers_quantiles,
        coached_levels = coached_levels,
        seed = arrivals_seed) %>% 
      get_passengers_after_aircrafts(seed = arrivals_seed) %>% 
      get_passengers_after_routes(coach_dist = coach_dist, 
                                  walk_dist = walk_dist,
                                  base_walk_dist = base_walk_dist,
                                  seed = arrivals_seed)
    
    non_arrivals_id <- 1 
    for (non_arrivals_id in seq_len(nrow(sim_settings$non_arrivals_data[[arrivals_id]]))) {
      n_egates <- sim_settings$non_arrivals_data[[arrivals_id]]$n_egates[[non_arrivals_id]]
      queue_seed = sim_settings$non_arrivals_data[[arrivals_id]]$gen_queue_seed[[non_arrivals_id]]
      egate_uptake <- sim_settings$non_arrivals_data[[arrivals_id]]$egate_uptake[[non_arrivals_id]]
      elig_boost <- sim_settings$non_arrivals_data[[arrivals_id]]$elig_boost[[non_arrivals_id]]
      
      bordercheck_egates = list(
        n_borderchecks = n_egates, 
        bordercheck_mean = 45,
        bordercheck_sd = 5,
        bordercheck_ids = seq_len(n_egates))
      
      simulated_queue <- simulated_passengers %>% 
        sim_queues(bordercheck_desks = bordercheck_desks,
                   bordercheck_egates = bordercheck_egates,
                   egate_uptake_prop = egate_uptake,
                   elig_boost = elig_boost,
                   egate_failure_prop = egate_failure_prop,
                   failed_egate_priority = failed_egate_priority,
                   seed = queue_seed)
      
      simulated_queue_lengths <- simulated_queue %>% 
        get_queue_lengths(progress_bar = TRUE)
      
      sim_settings$non_arrivals_data[[arrivals_id]]$queue_length_data[[non_arrivals_id]] <- 
        simulated_queue_lengths
      
      sim_settings$non_arrivals_data[[arrivals_id]]$sample_queue_data[[non_arrivals_id]] <- 
        simulated_queue %>% 
        select(sched_aircraft_date_posix, route_datetime_int, 
               bordercheck_start_time, bordercheck_end_time,
               nationality, egate_used, egate_failed) %>% 
        mutate(wait_time = bordercheck_start_time - route_datetime_int) %>% 
        slice_sample(n = queue_sample_size)
      
      non_arrivals_id <- non_arrivals_id + 1
      progress_counter <- progress_counter + 1
      progress_prop <- progress_counter / 
        (nrow(sim_settings$non_arrivals_data[[arrivals_id]])*nrow(sim_settings))
      message(paste0(round(progress_prop*100), "%"))
    }
  }
  
  sim_settings <- unnest(sim_settings, non_arrivals_data)
  return(sim_settings)
}