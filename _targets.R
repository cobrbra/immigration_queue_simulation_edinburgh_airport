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
  here("code/model_data.R"),
  here("code/get_results.R"),
  here("code/get_figures.R"),
  here("code/get_tables.R")
  )
)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(example_raw_data, 
             here("raw_data/example_raw_data.csv"), 
             format = "file"),
  tar_target(example_processed_data, 
             get_processed_data(example_raw_data)),
  tar_target(example_simulated_data, 
             simulate_data()),
  tar_target(example_models, 
             model_data(example_processed_data, example_simulated_data)),
  tar_target(example_results, 
             get_results(example_processed_data, 
                         example_simulated_data, 
                         example_models)),
  tar_target(example_figures,
             get_figures(example_results)),
  tar_target(example_tables,
             get_tables(example_results))
)
