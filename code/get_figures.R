library(tidyverse)
library(here)
library(showtext)

theme_edi_airport <- function() {
  font <- "Lato"
  theme_minimal() %+replace%
    theme(
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 40,                #set font size
        # face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 1),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 30),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 20,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 30),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis family
        size = 30),                #font size
      
      legend.text = element_text(
        family = font,
        size = 30
      ),
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
    ) 
}

get_figures <- function(future_aircrafts_arrivals, future_coached_levels, aircrafts_observed_arrivals, UK_plus_countries, ...) {
  # results <- targets::tar_read(example_results) # Use for debugging, COMMENT WHEN RUNNING TARGETS
  font_add_google("Lato")
  showtext_auto()
  edi_airport_colours <- c("#7D0C6E", "#A19384", "#F2D6EA", "#2D0255", "#","#")
  
  figures <- list()
  figure_sizes <- list()
  
  observed_max_passengers_per_year <- aircrafts_observed_arrivals %>% 
    mutate(Year = format(sched_aircraft_datetime_posix, format = "%Y")) %>% 
    filter(((Year == 2022) & 
              (sched_aircraft_date_posix >= as.Date("2022-07-11")) &
              (sched_aircraft_date_posix <= as.Date("2022-07-17"))) |
             ((Year == 2019) &
                (sched_aircraft_date_posix >= as.Date("2019-07-08")) &
                (sched_aircraft_date_posix <= as.Date("2019-07-14")))) %>% 
    filter(!(dep_country %in% UK_plus_countries)) %>% 
    group_by(Year) %>% 
    summarise(`Total Passengers` = sum(max_passengers)) %>% 
    mutate(coached_status = "Unknown") %>% 
    bind_rows(data.frame(Year = c("2020", "2021"),
                         coached_status = "Unknown") %>% 
                mutate(`Total Passengers` = 0))
  
  figures$future_passenger_burden_fig <- future_aircrafts_arrivals %>% # future_aircrafts_arrivals %>% # 
    mutate(Year = format(sched_aircraft_datetime_posix, format = "%Y")) %>% 
    group_by(Year) %>% 
    summarise(`Total Passengers` = sum(n_passengers)) %>% 
    inner_join(future_coached_levels, by = "Year") %>% 
    mutate(`Total Passengers` = `Total Passengers` * Percent) %>% 
    select(Year, `Total Passengers`, coached_status) %>% 
    bind_rows(observed_max_passengers_per_year) %>% 
    mutate(coached_status = factor(coached_status, levels = c("Contact", "Coached", "Unknown"))) %>%
    ggplot(aes(x = Year, y = `Total Passengers`, fill = coached_status)) + 
    geom_col(position = position_stack(reverse = TRUE)) +
    labs(title = "Future passenger pressure forecasted to increase") + 
    theme_edi_airport() +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = edi_airport_colours) +
    scale_y_continuous(labels = scales::comma)
  figure_sizes$future_passenger_burden_fig <- c(7, 3)
  
  
  # figures$figure_1 <- ...  + 
    # theme_edi_airport() +
    # scale_fill_manual(values = edi_airport_colours)
  # insert more code that generates figures here
  
  
  for (figure_index in seq_len(length(figures))) {
    figure_name <- names(figures)[figure_index[]]
    if (is.null(figure_sizes[[figure_name]])) {
      figure_sizes[[figure_name]] <- c(7,7)
    }
    ggsave(paste0(here("figures/"), figure_name, ".png"), 
           figures[[figure_name]],
           width = figure_sizes[[figure_name]][1],
           height = figure_sizes[[figure_name]][2])
  }
  return(figures)
}