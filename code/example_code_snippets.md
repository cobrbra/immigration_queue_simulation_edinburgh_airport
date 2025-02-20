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

    head(simulated_arrivals)

    ##   passenger_id   flight_id airport_classification coached
    ## 1          444 F0000000003         EU_plus_nonhub   FALSE
    ## 2          476 F0000000003         EU_plus_nonhub   FALSE
    ## 3          427 F0000000003         EU_plus_nonhub   FALSE
    ## 4          397 F0000000003         EU_plus_nonhub   FALSE
    ## 5          391 F0000000003         EU_plus_nonhub   FALSE
    ## 6          544 F0000000003         EU_plus_nonhub   FALSE
    ##   sched_aircraft_date_posix    nationality passenger_intervals
    ## 1                2023-07-10    nat_EU_plus            2.167929
    ## 2                2023-07-10    nat_EU_plus            1.727966
    ## 3                2023-07-10       nat_UKIE            2.820606
    ## 4                2023-07-10       nat_UKIE           10.814523
    ## 5                2023-07-10       nat_UKIE            8.321522
    ## 6                2023-07-10 nat_other_easy            8.070277
    ##   route_datetime_int route_datetime_posix route_date_posix route_time_int
    ## 1         1688950205  2023-07-10 01:50:04       2023-07-10       3004.579
    ## 2         1688950206  2023-07-10 01:50:06       2023-07-10       3006.306
    ## 3         1688950209  2023-07-10 01:50:09       2023-07-10       3009.127
    ## 4         1688950220  2023-07-10 01:50:19       2023-07-10       3019.942
    ## 5         1688950228  2023-07-10 01:50:28       2023-07-10       3028.263
    ## 6         1688950236  2023-07-10 01:50:36       2023-07-10       3036.333

and some queues

    simulated_queue <- simulated_arrivals %>% 
      immigration_queue(bordercheck_desks = bordercheck_desks, 
                 bordercheck_egates = bordercheck_egates, 
                 egate_uptake_prop = .7, target_eligibility = .8, 
                 egate_failure_prop = tar_read(egate_failure_prop), 
                 failed_egate_priority = tar_read(failed_egate_priority))  

    head(simulated_queue)

    ##   passenger_id   flight_id airport_classification coached
    ## 1          444 F0000000003         EU_plus_nonhub   FALSE
    ## 2          476 F0000000003         EU_plus_nonhub   FALSE
    ## 3          427 F0000000003         EU_plus_nonhub   FALSE
    ## 4          397 F0000000003         EU_plus_nonhub   FALSE
    ## 5          391 F0000000003         EU_plus_nonhub   FALSE
    ## 6          544 F0000000003         EU_plus_nonhub   FALSE
    ##   sched_aircraft_date_posix    nationality passenger_intervals
    ## 1                2023-07-10    nat_EU_plus            2.167929
    ## 2                2023-07-10    nat_EU_plus            1.727966
    ## 3                2023-07-10       nat_UKIE            2.820606
    ## 4                2023-07-10       nat_UKIE           10.814523
    ## 5                2023-07-10       nat_UKIE            8.321522
    ## 6                2023-07-10 nat_other_easy            8.070277
    ##   route_datetime_int route_datetime_posix route_date_posix route_time_int
    ## 1         1688950205  2023-07-10 01:50:04       2023-07-10       3004.579
    ## 2         1688950206  2023-07-10 01:50:06       2023-07-10       3006.306
    ## 3         1688950209  2023-07-10 01:50:09       2023-07-10       3009.127
    ## 4         1688950220  2023-07-10 01:50:19       2023-07-10       3019.942
    ## 5         1688950228  2023-07-10 01:50:28       2023-07-10       3028.263
    ## 6         1688950236  2023-07-10 01:50:36       2023-07-10       3036.333
    ##   base_egate_eligibility egate_eligibility bordercheck_start_time
    ## 1               eligible          eligible             1688950205
    ## 2               eligible          eligible             1688950257
    ## 3               eligible          eligible             1688950209
    ## 4               eligible          eligible             1688950220
    ## 5               eligible          eligible             1688950228
    ## 6           not_eligible      not_eligible             1688950236
    ##   bordercheck_end_time bordercheck_handled egate_used egate_failed
    ## 1           1688950257                   1      egate       passed
    ## 2           1688950350                   5      egate       failed
    ## 3           1688950255                   3      egate       passed
    ## 4           1688950305                   1       desk     no_egate
    ## 5           1688950267                   4      egate       passed
    ## 6           1688950336                   2       desk     no_egate

where we’ve shown some outputs to give an indication of the sort of
information we store.
