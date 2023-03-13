# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(here)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("here", "tidyverse", "readxl", "xtable"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = c(
  here("code/process_data.R"), 
  # here("code/simulate_data.R"),
  here("code/step_1_aircraft.R"),
  here("code/step_2_route.R"),
  here("code/step_3_immigration.R")
  # here("code/model_data.R"),
  # here("code/get_results.R"),
  # here("code/get_figures.R"),
  # here("code/get_tables.R")
  )
)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # Making the airports dataset accessible
  tar_target(airports_raw,
             here("raw_data/airports/GlobalAirportDatabase.txt"),
             format = "file"),
  tar_target(airports_processed,
             process_airports(airports_raw)),
  
  # Making the aircrafts dataset accessible
  tar_target(aircrafts_raw,
             here("raw_data/aircrafts/aircrafts_capacity.txt"),
             format = "file"),
  tar_target(aircrafts_processed,
             process_aircrafts(aircrafts_raw)),
  
  # Making the aircraft arrivals dataset accessible
  tar_target(aircrafts_observed_arrivals_raw,
         here("raw_data/aircrafts_observed_arrivals/"),
         format = "file"),
  tar_target(aircrafts_observed_arrivals_processed,
             process_aircrafts_observed_arrivals(
               aircrafts_observed_arrivals_raw,
               airports_reference = airports_processed,
               aircrafts_reference = aircrafts_processed)),
  
  # Simulating passengers getting off aircraft
  tar_target(passengers_from_aircrafts,
             data.frame(
               aircraft_id = numeric(),
               dep_country = numeric(),
               dep_airport = numeric(),
               ac_type = numeric(),
               t_sched = numeric(),
               t_actual = numeric(),
               sched_arrival_datetim = numeric(),
               actual_arrival_datetime = numeric(),
               sched_arrival_date = numeric(),
               actual_arrival_date = numeric(),
               sched_arrival_time = numeric(),
               actual_arrival_time = numeric(),
               des_rwy = numeric(),
               max_passengers = numeric(),
               n_passengers = numeric(),
               coached = numeric(),
               taxi_time = numeric(),
               walk_time = numeric())),
             # get_passengers_from_aircrafts(aircrafts_observed_arrivals_processed)),
  
  # Simulating passengers getting through coach/contact route
  tar_target(passengers_from_route,
             get_passengers_from_route(passengers_from_aircrafts)),

  # Simulating passengers getting through immigration
  tar_target(passengers_from_immigration,
             get_passengers_from_immigration(passengers_from_route)), 
  
  # For passenger count and nationality
  tar_target(EU_hubs, c("FRA", "AMS", "CDG")),
  tar_target(Other_hubs, c("IST", "ATL", "ORD", "DBX", "DFW", "DEN")),
  
  tar_target(nationality_props,
             here("params/nationality_proportions.txt"),
             format = "file"),
  
  tar_target(load_factor_mean, 0.85),
  tar_target(load_factor_sd, 0.1)
  
  
  
  
  # # Generating simulated arrivals data
  # tar_target(arrivals_sim_params,
  #            here("params/sim_params/arrival_sim_params.txt"),
  #            format = "file"),
  # tar_target(simulated_arrivals_data,
  #            simulate_arrivals(arrivals_sim_params)),
  # 
  # # Generating simulated queueing data
  # tar_target(queue_sim_params,
  #            here("params/sim_params/queue_sim_params.txt"),
  #            format = "file"),
  # tar_target(simulated_queue_data,
  #            simulate_queue(queue_sim_params, simulated_arrivals_data))#,
  # 
  # # Example data sources
  # tar_target(example_raw_data, 
  #            here("raw_data/example_raw_data.csv"), 
  #            format = "file"),
  # 
  # tar_target(example_processed_data, 
  #            get_processed_data(example_raw_data)),
  # 
  # tar_target(example_simulated_data, 
  #            simulate_data()),
  # 
  # # Example models and results
  # tar_target(example_models, 
  #            model_data(example_processed_data, example_simulated_data)),
  # tar_target(example_results, 
  #            get_results(example_processed_data, 
  #                        example_simulated_data, 
  #                        example_models)),
  # 
  # # Example figures and tables
  # tar_target(example_figures,
  #            get_figures(example_results)),
  # tar_target(example_tables,
  #            get_tables(example_results))
)
