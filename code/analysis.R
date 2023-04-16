generate_sim_settings <- function(seed = 1234,
                                  n_gen_arrivals = 1,
                                  n_gen_queues = 1,
                                  n_egates_range = seq(10, 14, by = 2),
                                  egate_uptake_range = seq(.6, 1., by = .2),
                                  elig_boost_range = seq(0, 0, 1)) {
  set.seed(seed)
  gen_arrivals_seeds <- sample(1:100000, size = n_gen_arrivals)
  gen_queue_seeds <- sample(1:100000, size = n_gen_queues)
  sim_settings <- tibble(
    gen_arrivals_seed = rep(gen_arrivals_seeds, 
                            each = lenght(n_egates_range) * n_gen_queues * length(egate_uptake_range) * length(elig_boost_range),
                            times = 1),
    n_egates = rep(n_egates_range, 
                   each = n_gen_arrivals * n_gen_queues * length(egate_uptake_range), 
                   times = length(elig_boost_range)),
    gen_queue_seed = rep(gen_queue_seeds, 
                         each = n_gen_arrivals * length(n_egates_range), 
                         times =  lenght(egate_uptake_range) * length(elig_boost_range)),
    egate_uptake = rep(egate_uptake_range,
                       each = n_gen_arrivals,
                       times = length(n_egates_range) * n_gen_queues * length(elig_boost_range)),
    elig_boost = rep(elig_boost_range,
                     each = 1,
                     times = n_gen_arrivals * length(n_egates_range) * n_gen_queues * length(egate_uptake_range)),
    queue_length_data = vector(mode = "list", 
                               length = n_gen_arrivals * length(n_egates_range) * n_gen_queues * length(egate_uptake_range) * length(elig_boost_range))
    )
  return(sim_settings)
}
