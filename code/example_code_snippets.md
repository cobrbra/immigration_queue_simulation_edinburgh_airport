# Example usage of Edinburgh Airport queue simulation toolkit

In this workbook we’ll introduce some of the code blocks building up our
analysis. None of these are directly used to produce results seen in the
report (these are incorporated into our `targets` workflow management)
but hopefully will provide useful context if attempting to understand,
use and adapt our codebase.

## Loading relevant libraries and code

Throughout most of our analysis we make use only of `tidyverse`
ecosystem libraries, as well as `here` for working with paths within the
project and `targets` for workflow management. We load these packages
here, as well as the import bits of

    library(tidyverse)
    library(here)
    library(targets)

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

## Example single simulation

Here we show how to produce a single simulation. Note that first we have
to make sure `targets` has access to the right objects and datasets. We
do this as follows (we’ll explain more of what’s going on here later).

    tar_make(c(
      'future_aircrafts_arrivals',
      'hubs',
      'countries',
      'prop_nationality',
      'delay_dist',
      'n_passengers_quantiles',
      'load_factor',
      'future_coached_levels',
      'coach_dist',
      'walk_dist',
      'base_walk_dist',
      'egate_failure_prop',
      'failed_egate_priority'
    ))

    ## here() starts at /Users/jacobbradley/Documents/immigration_queue_simulation_edinburgh_airport
    ## ✔ skip target load_factor
    ## ✔ skip target failed_egate_priority
    ## ✔ skip target other_hubs_raw
    ## ✔ skip target base_walk_dist
    ## ✔ skip target coach_dist
    ## ✔ skip target walk_dist
    ## ✔ skip target delay_dist
    ## ✔ skip target EU_plus_countries_raw
    ## ✔ skip target EU_plus_hubs_raw
    ## ✔ skip target aircrafts_raw
    ## ✔ skip target UKIE_countries_raw
    ## ✔ skip target prop_nationality_raw
    ## ✔ skip target observed_aircrafts_arrivals_raw
    ## ✔ skip target egate_failure_prop
    ## ✔ skip target future_aircrafts_arrivals_raw
    ## ✔ skip target airports_raw
    ## ✔ skip target hubs
    ## ✔ skip target aircrafts
    ## ✔ skip target countries
    ## ✔ skip target prop_nationality
    ## ✔ skip target future_coached_levels
    ## ✔ skip target future_aircrafts_arrivals
    ## ✔ skip target airports
    ## ✔ skip target observed_aircrafts_arrivals
    ## ✔ skip target n_passengers_quantiles
    ## ✔ skip pipeline [0.159 seconds]

We now make some desks and eGates.

    n_desks <- 9
    desk_means <- rep(90, n_desks)
    bordercheck_desks <- list(n_borderchecks = n_desks, 
                              bordercheck_means = desk_means,
                              bordercheck_sd = 14)

    n_egates <- 10
    bordercheck_egates = list(
      n_borderchecks = n_egates, 
      bordercheck_mean = 45,
      bordercheck_sd = 5)

Now we can simulate some arrivals

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
