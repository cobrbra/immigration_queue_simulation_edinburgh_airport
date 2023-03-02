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
  packages = c("here", "tidyverse", "xtable"), # packages that your targets need to run
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
  here("code/simulate_data.R"),
  here("code/step_1_aircraft.R"),
  here("code/step_2_route.R"),
  here("code/step_3_hall.R"),
  here("code/step_4_immigration.R"),
  here("code/model_data.R"),
  here("code/get_results.R"),
  here("code/get_figures.R"),
  here("code/get_tables.R")
  )
)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # Making the planes dataset accessible
  tar_target(aircraft_raw,
             here("raw_data/aircraft/GlobalAirportDatabase.txt"),
             format = "file"),
  tar_target(aircraft_processed,
             process_aircraft(aircraft_raw)),
  
  # Making the aircraft arrivals dataset accessible
  tar_target(aircraft_schedule_raw,
         here("raw_data/aircraft_schedule/aircraft_schedule.csv"),
         format = "file"),
  tar_target(aircraft_schedule_processed,
             process_aircraft_schedule(aircraft_schedule_raw)),
  
  # Simulating passengers getting off aircraft
  tar_target(passengers_from_aircraft,
             get_passengers_from_aircraft(aircraft_schedule_processed)),
  
  # Simulating passengers getting through coach/contact route
  tar_target(passengers_from_route,
             get_passengers_from_route(passengers_from_aircraft)),
  
  # Simulating passengers getting through the hall
  tar_target(passengers_from_hall,
             get_passengers_from_hall(passengers_from_route)),
  
  # Simulating passengers getting through immigration
  tar_target(passengers_from_immigration,
             get_passengers_from_immigration(passengers_from_hall)),
  
  # Generating simulated arrivals data
  tar_target(arrivals_sim_params,
             here("params/sim_params/arrival_sim_params.txt"),
             format = "file"),
  tar_target(simulated_arrivals_data,
             simulate_arrivals(arrivals_sim_params)),
  
  # Generating simulated queueing data
  tar_target(queue_sim_params,
             here("params/sim_params/queue_sim_params.txt"),
             format = "file"),
  tar_target(simulated_queue_data,
             simulate_queue(queue_sim_params, simulated_arrivals_data))#,
  
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
