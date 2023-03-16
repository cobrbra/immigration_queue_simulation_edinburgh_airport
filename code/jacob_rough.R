library(tidyverse)
library(scales)
library(readxl)

future_aircraft_arrivals <- 3:7 %>% 
  map(~read_xlsx(here("raw_data/EAL International Arrivals Data.xlsx"), 
                 sheet = .)) %>% 
  bind_rows() %>% 
  mutate(flight_date = format(`flight date`, format = "%Y-%m-%d"),
         arrival_time = format(`arrival time`, format  = "%H:%M:%S"),
         n_passengers = passengers) %>% 
  mutate(sched_aircraft_datetime_posix = as.POSIXct(paste(flight_date, arrival_time))) %>% 
  mutate(sched_aircraft_datetime_int = as.numeric(sched_aircraft_datetime_posix)) %>%
  mutate()
  select(sched_aircraft_datetime_posix, sched_aircraft_datetime_int, n_passengers) %>% 
  mutate()

future_aircraft_arrivals %>% 
  mutate(Year = format(sched_aircraft_datetime_posix, format = "%Y")) %>% 
  group_by(Year) %>% 
  summarise(`Total Passengers` = sum(n_passengers)) %>% 
  ggplot(aes(x = Year, y = `Total Passengers`)) + 
  geom_col() +
  theme_minimal()
  
future_aircraft_arrivals %>% 
  mutate(Year = format(sched_aircraft_datetime_posix, format = "%Y"),
         time_of_day = as.POSIXct(paste("1970-01-01", 
                                        format(sched_aircraft_datetime_posix, 
                                               format = "%H:%M:%S")))) %>% 
  ggplot(aes(x = time_of_day, fill = Year)) + geom_density(alpha = 0.2) + 
  theme_minimal() + 
  labs(x = "", y = "Density ") + 
  scale_x_datetime(labels = date_format('%H:%M'), 
                   breaks = as.POSIXct(c("1970-01-01 00:00", 
                                         "1970-01-01 06:00", 
                                         "1970-01-01 12:00",
                                         "1970-01-01 18:00"), tz = "GMT"))

future_aircraft_arrivals %>% 
  mutate(Year = format(sched_aircraft_datetime_posix, format = "%Y"),
         time_of_day = as.POSIXct(paste0("1970", 
                                        format(sched_aircraft_datetime_posix, 
                                               format = "-%m-%d %H:%M:%S")))) %>% 
  ggplot(aes(x = time_of_day)) + geom_density(alpha = 0.8) + 
  facet_wrap(~Year, ncol = 1) +
  theme_minimal() + 
  labs(x = "", y = "Number of Aircraft") + 
  scale_x_datetime(labels = date_format('%b %d, %H:%M'))

future_aircraft_arrivals %>% 
  ggplot(aes(x = sched_aircraft_datetime_posix)) + geom_histogram() + 
  labs(x = "", y = "Number of flights", title = "Flights ")
  theme_minimal()


sla_targets <- read_xlsx(here("raw_data/EAL International Arrivals Data.xlsx"), 
                      range = "assumptions!B3:D16", 
                      col_names = c("quantity", "value", "notes")) %>% 
  mutate(quantity = tolower(quantity)) %>% 
  mutate(quantity = str_replace_all(quantity, "# ", "n_")) %>% 
  mutate(quantity = str_replace_all(quantity, "% ", "percent_")) %>% 
  mutate(quantity = str_replace_all(quantity, " ", "_"))

tar_read(example_passengers_after_route) %>% 
  mutate(`Flight ID` = flight_id) %>% 
  ggplot(aes(x = route_datetime_posix, fill = `Flight ID`)) + geom_histogram(binwidth = 200, position = "stack") +
  scale_x_datetime(labels = date_format('%H:%M')) + 
  theme_minimal() + 
  labs(x = "", 
       y = "Number of passengers arriving at immigration hall", 
       title = "Coached passengers produce stronger peaks")

coached_levels <- aircrafts %>% 
  arrange(desc(coached)) %>% 
  pull(aircraft_id)

arrivals_at_hall %>% 
  mutate(`Aircraft ID` = aircraft_id,
         coached = if_else(coached, "Coached", "Contact"),
         stack_group = factor(aircraft_id, levels = coached_levels)) %>% 
  ggplot(aes(x = arrival_at_hall, 
             fill = `Aircraft ID`,
             group = stack_group,
             linetype = coached)) + 
  geom_density(position = "stack") +
  labs(x= "Arrival time in Hall", 
       y = "Arrival Density",
       title = "Coached arrivals occur in bulk; contact passengers are spread out") + 
  theme_light() +
  scale_linetype_discrete(name = "")
