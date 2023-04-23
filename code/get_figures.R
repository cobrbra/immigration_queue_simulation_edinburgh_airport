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
      
      strip.text = element_text(
        family = font, 
        size = 30),
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
    ) 
}

get_figures <- function(future_aircrafts_arrivals, future_coached_levels, 
                        filtered_observed_aircrafts_arrivals, 
                        window_aircrafts_arrivals,
                        window_queue,
                        coach_dist, walk_dist, base_walk_dist, 
                        rec_fig_sim_data, ...) {
  # results <- targets::tar_read(example_results) # Use for debugging, COMMENT WHEN RUNNING TARGETS
  font_add_google("Lato")
  showtext_auto()
  edi_airport_colours <- c("#7D0C6E", "#A19384", "#F2D6EA",
                           "#6E7D0C", "#84A193", "#EAF2D6",
                           "#0C6E7D", "#9384A1", "#D6EAF2",
                           "#2D0255", "#511166", "#B41788")
  
  figures <- list()
  figure_sizes <- list()
  
  observed_max_passengers_per_year <- (filtered_observed_aircrafts_arrivals) %>% 
    group_by(Year) %>% 
    summarise(`Total Passengers` = sum(max_passengers)) %>% 
    mutate(coached_status = "Unknown") 
  
  figures$future_passenger_burden_fig <- (future_aircrafts_arrivals) %>%
    mutate(year = format(sched_aircraft_datetime_posix, format = "%Y")) %>% 
    group_by(year) %>% 
    summarise(total_passengers = sum(n_passengers)) %>% 
    inner_join((future_coached_levels), by = "year") %>% 
    mutate(Coached = total_passengers * prob_coached,
           Contact = total_passengers * (1-prob_coached)) %>% 
    pivot_longer(c(Coached, Contact), names_to = "coached_status", values_to = "Total Passengers") %>% 
    select(Year = year, `Total Passengers`, coached_status) %>% 
    bind_rows(observed_max_passengers_per_year) %>% 
    mutate(coached_status = factor(coached_status, levels = c("Contact", "Coached", "Unknown"))) %>%
    ggplot(aes(x = Year, y = `Total Passengers`, fill = coached_status)) + 
    geom_col(position = position_stack(reverse = TRUE)) +
    # labs(title = "Passenger pressure forecasted to increase beyond pre-Covid levels") + 
    theme_edi_airport() +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = edi_airport_colours[c(4,5,6)]) +
    scale_y_continuous(labels = scales::comma)
  figure_sizes$future_passenger_burden_fig <- c(7, 3)
  
  panel_1 <- window_aircrafts_arrivals %>% 
    mutate(coached = factor(if_else(coached, "Coached", "Contact"),
                            levels = c("Contact", "Coached"))) %>% 
    ggplot(aes(x = sched_aircraft_datetime_posix, y = n_passengers)) + 
    geom_segment(aes(xend = aircraft_datetime_posix,
                     yend = n_passengers),
                 arrow = arrow(length = unit(0.2, "cm"))) + 
    geom_point(size = 2, shape = 15) + 
    theme_edi_airport() + 
    theme(plot.title = element_text(size = 30, hjust = .5, margin = margin(0,0,10,0)),
          axis.title.y = element_text(margin = margin(0, 5, 0, 0))) +
    labs(x = "", y = "Number of Passengers", title = "Aircraft Arrivals") +
    xlim(as.POSIXct("2023-07-11 08:00:00"), as.POSIXct("2023-07-11 10:00:00")) + 
    ylim(0,200) 
  
  panel_2 <- window_aircrafts_arrivals %>% 
    get_passengers_after_aircrafts(seed = 123) %>% 
    get_passengers_after_routes(coach_dist, walk_dist, base_walk_dist,
                                seed = 123) %>% 
    
    ggplot(aes(x = route_datetime_posix, 
               fill = flight_id)) + 
    geom_histogram(position = position_stack(reverse = TRUE)) + 
    theme_edi_airport() + 
    theme(legend.position = "none",
          plot.title = element_text(size = 30, hjust = .5, margin = margin(0, 0, 10, 0)),
          axis.title.y = element_text(margin = margin(0, 5, 0, 0))) + 
    labs(x = "", y = "Number of Passengers", title = "Passenger Arrivals") + 
    scale_fill_manual(values = rep(edi_airport_colours[-(1:2)], 3)) +
    xlim(as.POSIXct("2023-07-11 08:00:00"), as.POSIXct("2023-07-11 10:00:00")) + 
    ylim(0,200) 
  
  panel_3 <- bind_rows(
    window_queue %>% 
      mutate(kpi = (bordercheck_start_time - route_datetime_int)/60,
             check = if_else(egate_used == "desk", "Desk", "eGate"),
             fac = "Wait Time (mins)") %>% 
      select(route_datetime_posix, check, kpi, fac),
    window_queue %>%
      get_queue_lengths(input_time_interval = 60) %>% 
      pivot_longer(c(desk_queue_length, egate_queue_length), 
                   names_to = "check", values_to = "kpi") %>% 
      mutate(check = if_else(check == "desk_queue_length", "Desk", "eGate"),
             fac = "Queue Length") %>% 
      select(route_datetime_posix = queue_length_datetime_posix, check, kpi, fac)
  ) %>% 
    ggplot(aes(x = route_datetime_posix, y = kpi, colour = check)) + 
    geom_point() + 
    theme_edi_airport() + 
    theme(legend.title = element_blank(), axis.title = element_blank()) + 
    scale_colour_manual(values = edi_airport_colours[2:1]) + 
    facet_wrap(~fac, ncol = 1, scales = "free_y") +
    xlim(as.POSIXct("2023-07-11 08:00:00"), as.POSIXct("2023-07-11 10:00:00")) 
  
  figures$workflow_fig <- plot_grid(
    plot_grid(NULL, 
              plot_grid(panel_1, panel_2, ncol = 2), 
              NULL, 
              ncol = 1, rel_heights = c(.15, 1, .15)),
    NULL,
    panel_3,
    nrow = 1,
    rel_widths = c(1, -.08, .8))
  figure_sizes$workflow_fig <- c(10,4)
  
  core_recommendation_stats <- rec_fig_sim_data %>% 
    summarise(wait_time_60_egate = mean(wait_time_60_egate), 
              wait_time_60_desk = mean(wait_time_60_desk),
              wait_time_15_desk = mean(wait_time_15_desk),
              wait_time_15_egate = mean(wait_time_15_egate),
              queue_length_650_egate = mean(queue_length_650_egate),
              queue_length_650_desk = mean(queue_length_650_desk),
              queue_length_1250_egate = mean(queue_length_1250_egate),
              queue_length_1250_desk = mean(queue_length_1250_desk),
              .by = c("year", "n_egates", "egate_uptake", "target_eligibility")) %>% 
    mutate(overall_usage = egate_uptake * target_eligibility) %>% 
    pivot_longer(cols = c(n_egates, target_eligibility, egate_uptake, overall_usage,
                          wait_time_60_desk, wait_time_60_egate, wait_time_15_desk, wait_time_15_egate,
                          queue_length_650_egate, queue_length_650_desk, queue_length_1250_egate, queue_length_1250_desk),
                 names_to = "which_stat",
                 values_to = "stat") %>%
    mutate(
      fill = factor(
        case_when(
          which_stat == "n_egates" ~ "No. eGates",
          str_detect(which_stat, "_desk") ~ "Desk", 
          str_detect(which_stat, "_egate") ~ "eGate",
          which_stat == "target_eligibility" ~ "eGate Eligibility",
          which_stat == "egate_uptake" ~ "eGate Uptake",
          which_stat == "overall_usage" ~ "Overall eGate Usage"),
        levels = c("No. eGates", "eGate Eligibility", "eGate Uptake", "Overall eGate Usage", "Desk", "eGate")),
      fac = factor(
        case_when(
          str_detect(which_stat, "n_egates") ~ "Recommended Number of eGates",
          which_stat %in% c("egate_uptake", "target_eligibility", "overall_usage") ~ "Core Assumptions",
          str_detect(which_stat, "15") ~ "Proportion waits < 15mins", 
          str_detect(which_stat, "60") ~ "Proportion waits < 60mins",
          str_detect(which_stat, "650") ~ "Minutes exceeding overflow",
          str_detect(which_stat, "1250") ~ "Minutes exceeding contingency"),
        levels = c("Recommended Number of eGates",
                   "Core Assumptions",
                   "Proportion waits < 15mins", 
                   "Proportion waits < 60mins",
                   "Minutes exceeding overflow", 
                   "Minutes exceeding contingency")))
  
  figures$core_rec_fig <- core_recommendation_stats %>% 
    ggplot(aes(x = year, y = stat, fill = fill, colour = fill)) + 
    geom_col(position = "dodge", alpha = 0.9) + 
    facet_wrap(~fac, scales = "free_y", dir = "v", nrow = 2) + 
    theme_edi_airport() +
    theme(legend.title = element_blank(),
          legend.position = "bottom") + 
    labs(x = "Year", y = "")  + 
    scale_fill_manual(values = edi_airport_colours[c(7, 4:6, 2:1)]) +
    scale_colour_manual(values = edi_airport_colours[c(7, 4:6, 2:1)])
  figure_sizes$core_rec_fig <- c(12, 5)
  
  # figures$figure_1 <- ...  + 
    # theme_edi_airport() +
    # scale_fill_manual(values = edi_airport_colours)
  # insert more code that generates figures here
  
  
  for (figure_index in seq_len(length(figures))) {
    figure_name <- names(figures)[figure_index[]]
    if (is.null(figure_sizes[[figure_name]])) {
      figure_sizes[[figure_name]] <- c(7,7)
    }
    ggsave(here(paste0("figures/", figure_name, ".png")), 
           figures[[figure_name]],
           width = figure_sizes[[figure_name]][1],
           height = figure_sizes[[figure_name]][2])
  }
  return(figures)
}