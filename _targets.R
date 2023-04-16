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
  here("code/kpis.R"),
  here("code/analysis.R"),
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
  
  # Making the observed aircraft arrivals dataset accessible
  tar_target(observed_aircrafts_arrivals_raw,
         here("raw_data/observed_aircrafts_arrivals/"),
         format = "file"),
  tar_target(observed_aircrafts_arrivals,
             process_aircrafts_arrivals(
               observed_aircrafts_arrivals_raw,
               airports_reference = airports,
               aircrafts_reference = aircrafts)),
  tar_target(filtered_observed_aircrafts_arrivals,
             process_filtered_observed_aircrafts_arrivals(observed_aircrafts_arrivals = observed_aircrafts_arrivals,
                                                  UKIE_countries = countries$UKIE)),
  tar_target(n_passengers_quantiles, 
             process_n_passengers_quantiles(observed_aircrafts_arrivals, 
                                         hubs = hubs, 
                                         countries = countries, 
                                         load_factor = load_factor,
                                         seed = 1234)),
  
  # Making the future aircraft arrivals dataset accessible
  tar_target(future_aircrafts_arrivals_raw,
             here("raw_data/EAL International Arrivals Data.xlsx"),
             format = "file"),
  tar_target(future_aircrafts_arrivals,
             process_future_aircrafts_arrivals(future_aircrafts_arrivals_raw)),
  
  tar_target(future_coached_levels,
             process_future_coached_levels(future_aircrafts_arrivals_raw)),
  tar_target(observed_coached_levels,
             data.frame(year = as.character(2019:2022),
                        prob_coached = .15)),
  
  
  # Example workflow for small arrivals datasets
  tar_target(example_coached_levels,
             data.frame(year = "1970",
                        prob_coached = .15)),
  tar_target(example_aircrafts_arrivals,
             sim_example_aircrafts_arrivals(seed = 8) %>% 
               complete_aircrafts_arrivals(hubs = hubs, 
                                           countries = countries, 
                                           prop_nationality = prop_nationality)),
  tar_target(example_passenger_arrivals,
             get_passengers_after_aircrafts(
               aircrafts_arrivals = example_aircrafts_arrivals,
               seed = 10)),
  tar_target(example_passengers_after_routes,
             get_passengers_after_routes(example_passenger_arrivals,
                                         coach_dist = coach_dist,
                                         walk_dist = walk_dist,
                                         base_walk_dist,
                                         seed = 1)),
  
  # Competitions=specified distributional assumptions
  tar_target(delay_dist,
             list(prop_delayed = .21,
                  prop_on_time = .58,
                  prop_early = .21,
                  mean_delay_time = 50*60, #translating to minutes
                  mean_early_time = 21*60,
                  on_time_window = 15*60*2 # window length, to seconds, double sided 
             )),
  tar_target(coach_dist,
             list(mean = 23*60,
                  sd = 6*60)),
  tar_target(walk_dist,
             list(contact_inter = 5,
                  coached_inter = 1)),
  tar_target(base_walk_dist,
             list(min = 3*60,
                  max = 12*60)),
  
  # Parameter choices: nationality, countries and airports
  tar_target(EU_plus_hubs_raw, here("params/nationality_info/EU_plus_hubs.txt"), format = "file"),
  tar_target(other_hubs_raw, here("params/nationality_info/other_hubs.txt"), format = "file"),

  tar_target(hubs,
             list(EU_plus = colnames(read_delim(EU_plus_hubs_raw, delim = ",")),
                  other = colnames(read_delim(other_hubs_raw, delim = ",")))),
  
  tar_target(EU_plus_countries_raw,
             here("params/nationality_info/EU_plus_countries.txt"), 
             format = "file"),
  tar_target(UKIE_countries_raw,
             here("params/nationality_info/UKIE_countries.txt"), 
             format = "file"),

  tar_target(countries,
             list(UKIE = colnames(read_delim(UKIE_countries_raw, delim = ",")),
                  EU_plus = colnames(read_delim(EU_plus_countries_raw, delim = ",")))),
  
  tar_target(prop_nationality_raw,
             here("params/nationality_info/nationality_proportions.txt"),
             format = "file"),
  tar_target(prop_nationality,
             read_delim(prop_nationality_raw, delim = ";")),
  
  # Parameter choices: miscellaneous
  tar_target(load_factor, list(mean = .95, sd = .1)),

  tar_target(egate_failure_prop, .05),
  tar_target(failed_egate_priority, .75),
  
  # Generation of shiny data
  tar_target(shiny_sim_settings,
             generate_sim_settings()),
  tar_target(shiny_sim_data,
             sim_analysis_data(sim_settings = shiny_sim_settings,
                               aircrafts_arrivals = future_aircrafts_arrivals,
                               hubs = hubs,
                               countries = countries,
                               prop_nationality = prop_nationality,
                               delay_dist = delay_dist,
                               n_passengers_quantiles = n_passengers_quantiles, 
                               coached_levels = future_coached_levels, 
                               coach_dist = coach_dist,
                               walk_dist = walk_dist,
                               base_walk_dist = base_walk_dist)),
  
  # Trackable report outputs
  tar_target(figures,
             get_figures(future_aircrafts_arrivals = future_aircrafts_arrivals,
                         future_coached_levels = future_coached_levels,
                         filtered_observed_aircrafts_arrivals = filtered_observed_aircrafts_arrivals)),
  tar_target(tables,
             get_tables(future_aircrafts_arrivals = future_aircrafts_arrivals,
                        observed_aircrafts_arrivals = observed_aircrafts_arrivals))
)
