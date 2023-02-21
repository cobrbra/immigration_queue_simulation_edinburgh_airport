library(tidyverse)

simulate_data <- function(seed = 1234) {
  simulated_data <- tibble(
    sim_x = rnorm(100)
  )
  write_csv(simulated_data, here("processed_data/example_simulated_data.csv"))
  return(simulated_data)
}