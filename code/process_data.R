library(tidyverse)
library(here)

get_processed_data <- function(file) {
  
  
  processed_data <- read_csv(file) #%>% 
    # filter(mpg < 20)
  write_csv(processed_data, here("processed_data/example_processed_data.csv"))
  
  
  return(processed_data)
}