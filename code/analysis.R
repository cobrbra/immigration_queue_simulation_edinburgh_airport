specify_sim_settings <- function(n_egates_range,
                                 egate_uptake_range = seq(.75, .99, length.out = 5),
                                 target_eligibility_range = seq(.8, .96, length.out = 5),
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
                              wait_time_kpis = list(),
                              queue_length_kpis = list(),
                              queue_sample_size = 500,
                              input_time_interval = 15*60,
                              save_data = FALSE,
                              save_dir = NULL) {
  # generate desks
  n_desks <- 9
  desk_means <- pmax(0, rep(90, n_desks))
  desk_ids <- seq_len(n_desks)
  bordercheck_desks <- list(n_borderchecks = n_desks, 
                            bordercheck_means = desk_means,   
                            bordercheck_sd = 14,
                            bordercheck_ids = desk_ids)
  if (save_data) {
    sim_settings <- sim_settings %>% 
      mutate(queue_length_data = vector(mode = "list", length = n()),
             sample_queue_data = vector(mode = "list", length = n()))
  }
  
  n_settings <- nrow(sim_settings)
  for (kpi in wait_time_kpis) {
    sim_settings[[paste0(kpi, "_desk")]] <- numeric(n_settings)
    sim_settings[[paste0(kpi, "_egate")]] <- numeric(n_settings)
    sim_settings[[paste0(kpi, "_both")]] <- numeric(n_settings)
  }
  for (kpi in queue_length_kpis) {
    sim_settings[[paste0(kpi, "_desk")]] <- numeric(n_settings)
    sim_settings[[paste0(kpi, "_egate")]] <- numeric(n_settings)
    sim_settings[[paste0(kpi, "_both")]] <- numeric(n_settings)
  }
  
  sim_settings <- nest(sim_settings, non_arrivals_data = -gen_arrivals_seed)
  
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
      target_eligibility <- sim_settings$non_arrivals_data[[arrivals_id]]$target_eligibility[[non_arrivals_id]]
      year <- sim_settings$non_arrivals_data[[arrivals_id]]$year[[non_arrivals_id]]
      
      bordercheck_egates = list(
        n_borderchecks = n_egates, 
        bordercheck_mean = 45,
        bordercheck_sd = 5,
        bordercheck_ids = seq_len(n_egates))
      
      simulated_queue <- simulated_passengers[
        as.character(year(simulated_passengers$sched_aircraft_date_posix)) == year,
      ] %>% 
        immigration_queue(bordercheck_desks = bordercheck_desks,
                   bordercheck_egates = bordercheck_egates,
                   egate_uptake_prop = egate_uptake,
                   target_eligibility = target_eligibility,
                   egate_failure_prop = egate_failure_prop,
                   failed_egate_priority = failed_egate_priority,
                   seed = queue_seed)
      
      simulated_queue_lengths <- simulated_queue %>% 
        get_queue_lengths(input_time_interval = input_time_interval)
      
      for (kpi in wait_time_kpis) {
        sim_settings$non_arrivals_data[[arrivals_id]][[paste0(kpi, "_desk")]][non_arrivals_id] <- 
          get(kpi)(simulated_queue[simulated_queue$egate_used == "desk", ])
        sim_settings$non_arrivals_data[[arrivals_id]][[paste0(kpi, "_egate")]][non_arrivals_id] <- 
          get(kpi)(simulated_queue[simulated_queue$egate_used == "egate", ])
        sim_settings$non_arrivals_data[[arrivals_id]][[paste0(kpi, "_both")]][non_arrivals_id] <- 
          get(kpi)(simulated_queue)
        
      }
      for (kpi in queue_length_kpis) {
        sim_settings$non_arrivals_data[[arrivals_id]][[paste0(kpi, "_desk")]][non_arrivals_id] <- 
          get(kpi)(simulated_queue_lengths, "desk")
        sim_settings$non_arrivals_data[[arrivals_id]][[paste0(kpi, "_egate")]][non_arrivals_id] <- 
          get(kpi)(simulated_queue_lengths, "egate")
        sim_settings$non_arrivals_data[[arrivals_id]][[paste0(kpi, "_both")]][non_arrivals_id] <- 
          get(kpi)(simulated_queue_lengths, "both")
      }
      
      if (save_data) {
        sim_settings$non_arrivals_data[[arrivals_id]]$queue_length_data[[non_arrivals_id]] <- 
          simulated_queue_lengths
        
        sim_settings$non_arrivals_data[[arrivals_id]]$sample_queue_data[[non_arrivals_id]] <- 
          simulated_queue %>% 
            select(sched_aircraft_date_posix, route_datetime_int, 
                   bordercheck_start_time, bordercheck_end_time,
                   nationality, egate_used, egate_failed) %>% 
            mutate(wait_time = bordercheck_start_time - route_datetime_int) %>% 
            slice_sample(n = queue_sample_size)
      }
      
      non_arrivals_id <- non_arrivals_id + 1
      progress_counter <- progress_counter + 1
      if (round(100*progress_counter / n_settings) != 
        round(100*(progress_counter - 1) / n_settings)) {
        message(paste0(round(100*progress_counter / n_settings), "%"))
      }
    }
  }
  
  sim_settings <- unnest(sim_settings, non_arrivals_data)
  if (!is.null(save_dir)) {
    if (save_data) {
      write_csv(sim_settings %>% 
                select(-queue_length_data) %>% 
                unnest(sample_queue_data),
              paste0(save_dir, "/raw_passenger_data.csv"))
      
      write_csv(sim_settings %>% 
                  select(-sample_queue_data) %>% 
                  unnest(queue_length_data),
                paste0(save_dir, "/raw_queue_length_data.csv"))
    }
    else {
      write_csv(sim_settings,
                paste0(save_dir, "kpi_data.csv"))
    }
  }
  
  return(sim_settings)
}



