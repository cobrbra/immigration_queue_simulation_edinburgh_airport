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
  packages = c("here", "readxl", "scales", "showtext", "tidyverse", "xtable"), # packages that your targets need to run
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
  here("code/step_1_aircraft.R"),
  here("code/step_2_route.R"),
  here("code/step_3_immigration.R"),
  here("code/get_figures.R"),
  here("code/get_tables.R")
  )
)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # Making the airports dataset accessible
  tar_target(airports_raw,
             here("raw_data/airports/GlobalAirportDatabase.txt"),
             format = "file"),
  tar_target(airports,
             process_airports(airports_raw)),
  
  # Making the aircrafts dataset accessible
  tar_target(aircrafts_raw,
             here("raw_data/aircrafts/aircrafts_capacity.txt"),
             format = "file"),
  tar_target(aircrafts,
             process_aircrafts(aircrafts_raw)),
  
  # Making the aircraft arrivals dataset accessible
  tar_target(aircrafts_observed_arrivals_raw,
         here("raw_data/aircrafts_observed_arrivals/"),
         format = "file"),
  tar_target(aircrafts_observed_arrivals,
             process_aircrafts_arrivals(
               aircrafts_observed_arrivals_raw,
               airports_reference = airports,
               aircrafts_reference = aircrafts)),
  tar_target(filtered_aircrafts_observed_arrivals,
             filter_arrivals_for_equivalent_weeks(aircrafts_observed_arrivals = aircrafts_observed_arrivals,
                                                  UK_plus_countries = UK_plus_countries)),
  tar_target(n_passenger_quantiles, 
             process_aircrafts_quantiles(aircrafts_observed_arrivals, 
                                         EU_plus_hubs, other_hubs, UK_plus_countries,
                                         EU_plus_countries, load_factor_mean, load_factor_sd)),
  
  # Making the future aircraft arrivals dataset accessible
  tar_target(future_aircrafts_arrivals_raw,
             here("raw_data/EAL International Arrivals Data.xlsx"),
             format = "file"),
  tar_target(future_aircrafts_arrivals,
             process_future_aircrafts_arrivals(future_aircrafts_arrivals_raw)),
  tar_target(future_coached_levels,
             process_future_coached_levels(future_aircrafts_arrivals_raw)),
  
  
  # Simulating passengers from simulated arrivals
  tar_target(example_aircraft_arrivals,
             simulate_aircrafts_arrivals(seed = 8)),
  tar_target(example_passenger_arrivals,
             get_passengers_after_aircrafts(
               aircrafts = example_aircraft_arrivals,
               EU_plus_hubs = EU_plus_hubs,
               other_hubs = other_hubs,
               prop_nationality = prop_nationality,
               UK_plus_countries = UK_plus_countries,
               EU_plus_countries = EU_plus_countries,
               load_factor_mean = load_factor_mean,
               load_factor_sd = load_factor_sd)),
  tar_target(example_passengers_after_route,
             get_passengers_after_route(example_passenger_arrivals)),
  
  # For passenger count and nationality
  tar_target(EU_plus_hubs_raw, here("params/nationality_info/EU_plus_hubs.txt"), format = "file"),
  tar_target(EU_plus_hubs, colnames(read_delim(EU_plus_hubs_raw, delim = ","))),
  
  tar_target(other_hubs_raw, here("params/nationality_info/other_hubs.txt"), format = "file"),
  tar_target(other_hubs, colnames(read_delim(other_hubs_raw, delim = ","))),
  
  tar_target(EU_plus_countries_raw,
             here("params/nationality_info/EU_plus_countries.txt"), 
             format = "file"),
  tar_target(EU_plus_countries,
             colnames(read_delim(EU_plus_countries_raw, delim = ","))),
  
  tar_target(UK_plus_countries_raw,
             here("params/nationality_info/UK_plus_countries.txt"), 
             format = "file"),
  tar_target(UK_plus_countries,
             colnames(read_delim(UK_plus_countries_raw, delim = ","))),
  
  tar_target(prop_nationality_raw,
             here("params/nationality_info/nationality_proportions.txt"),
             format = "file"),
  tar_target(prop_nationality,
             read_delim(prop_nationality_raw, delim = ";")),
  
  tar_target(load_factor_mean, .95),
  tar_target(load_factor_sd, .1),
  
  tar_target(egate_failure_prop, .05),
  tar_target(failed_egate_priority, .75),
  
  tar_target(figures,
             get_figures(future_aircrafts_arrivals = future_aircrafts_arrivals,
                         future_coached_levels = future_coached_levels,
                         filtered_aircrafts_observed_arrivals = filtered_aircrafts_observed_arrivals)),
  tar_target(tables,
             get_tables(future_aircrafts_arrivals = future_aircrafts_arrivals,
                        aircrafts_observed_arrivals = aircrafts_observed_arrivals))
)
