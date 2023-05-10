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
                        rec_fig_sim_data, 
                        rec_minus_fig_sim_data, 
                        robustness_sim_data, ...) {
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
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) +
    scale_fill_manual(values = edi_airport_colours[c(4,5,6)]) +
    scale_y_continuous(labels = scales::comma)
  figure_sizes$future_passenger_burden_fig <- c(7, 3)
  
  panel_1 <- (window_aircrafts_arrivals) %>% 
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
  
  panel_2 <- (window_aircrafts_arrivals) %>% 
    get_passengers_after_aircrafts(seed = 123) %>% 
    get_passengers_after_routes((coach_dist), (walk_dist), (base_walk_dist),
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
    (window_queue) %>% 
      mutate(wait = (bordercheck_start_time - route_datetime_int)/60,
             check = if_else(egate_used == "desk", "Desk", "eGate"),
             fac = "Wait Time (mins)",
             length = NA) %>% 
      select(route_datetime_posix, check, wait, length, fac),
    (window_queue) %>%
      get_queue_lengths(input_time_interval = 60) %>% 
      pivot_longer(c(desk_queue_length, egate_queue_length), 
                   names_to = "check", values_to = "length") %>% 
      mutate(check = if_else(check == "desk_queue_length", "Desk", "eGate"),
             fac = "Queue Length (num. passengers)",
             wait = NA) %>% 
      select(route_datetime_posix = queue_length_datetime_posix, check, wait, length, fac)
  ) %>% 
    ggplot(aes(x = route_datetime_posix, colour = check)) + 
    geom_point(aes(y = wait), size = 0.75) +
    geom_line(aes(y = length), size = 1) + 
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
  
  core_recommendation_stats <- (rec_fig_sim_data) %>%
    summarise(wait_time_60_egate = mean(wait_time_60_egate), 
              wait_time_60_desk = mean(wait_time_60_desk),
              wait_time_15_desk = mean(wait_time_15_desk),
              wait_time_15_egate = mean(wait_time_15_egate),
              exceeds_contingency_egate = mean(exceeds_contingency_egate)/60,
              exceeds_contingency_desk = mean(exceeds_contingency_desk)/60,
              exceeds_overflow_egate = mean(exceeds_overflow_egate)/60,
              exceeds_overflow_desk = mean(exceeds_overflow_desk)/60,
              .by = c("year", "n_egates", "egate_uptake", "target_eligibility")) %>% 
    mutate(overall_usage = egate_uptake * target_eligibility) %>% 
    pivot_longer(cols = c(n_egates, target_eligibility, egate_uptake, overall_usage,
                          wait_time_60_desk, wait_time_60_egate, wait_time_15_desk, wait_time_15_egate,
                          exceeds_contingency_egate, exceeds_contingency_desk, exceeds_overflow_egate, exceeds_overflow_desk),
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
          str_detect(which_stat, "overflow") ~ "Contingency in use (hrs)",
          str_detect(which_stat, "contingency") ~ "Exceeding contingency (hrs)"),
        levels = c("Recommended Number of eGates",
                   "Core Assumptions",
                   "Proportion waits < 15mins", 
                   "Proportion waits < 60mins",
                   "Contingency in use (hrs)", 
                   "Exceeding contingency (hrs)")))
  
  figures$core_rec_fig <- core_recommendation_stats %>%
    ggplot(aes(x = year, y = stat, fill = fill, colour = fill)) + 
    geom_col(position = "dodge", alpha = 0.9) + 
    facet_wrap(~fac, scales = "free_y", dir = "v", nrow = 2) + 
    theme_edi_airport() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.title = element_blank()) + 
    guides(fill = guide_legend(nrow = 1)) +
    # labs(x = "Year", y = "")  + 
    scale_fill_manual(values = edi_airport_colours[c(7, 4:6, 2:1)]) +
    scale_colour_manual(values = edi_airport_colours[c(7, 4:6, 2:1)]) +
    scale_y_continuous(labels = scales::comma)
  figure_sizes$core_rec_fig <- c(8, 4)
  
  minus_core_recommendation_stats <- (rec_minus_fig_sim_data) %>% 
    summarise(wait_time_60_egate = mean(wait_time_60_egate), 
              wait_time_60_desk = mean(wait_time_60_desk),
              wait_time_15_desk = mean(wait_time_15_desk),
              wait_time_15_egate = mean(wait_time_15_egate),
              exceeds_contingency_egate = mean(exceeds_contingency_egate)/60,
              exceeds_contingency_desk = mean(exceeds_contingency_desk)/60,
              exceeds_overflow_egate = mean(exceeds_overflow_egate)/60,
              exceeds_overflow_desk = mean(exceeds_overflow_desk)/60,
              .by = c("year", "n_egates", "egate_uptake", "target_eligibility")) %>% 
    mutate(overall_usage = egate_uptake * target_eligibility) %>% 
    pivot_longer(cols = c(n_egates, target_eligibility, egate_uptake, overall_usage,
                          wait_time_60_desk, wait_time_60_egate, wait_time_15_desk, wait_time_15_egate,
                          exceeds_contingency_egate, exceeds_contingency_desk, exceeds_overflow_egate, exceeds_overflow_desk),
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
          str_detect(which_stat, "n_egates") ~ "Number of eGates",
          which_stat %in% c("egate_uptake", "target_eligibility", "overall_usage") ~ "Core Assumptions",
          str_detect(which_stat, "15") ~ "Proportion waits < 15mins", 
          str_detect(which_stat, "60") ~ "Proportion waits < 60mins",
          str_detect(which_stat, "overflow") ~ "Contingency in use (hrs)",
          str_detect(which_stat, "contingency") ~ "Exceeding contingency (hrs)"),
        levels = c("Number of eGates",
                   "Core Assumptions",
                   "Proportion waits < 15mins", 
                   "Proportion waits < 60mins",
                   "Contingency in use (hrs)", 
                   "Exceeding contingency (hrs)")))
  
  figures$minus_core_rec_fig <- minus_core_recommendation_stats %>%
    ggplot(aes(x = year, y = stat, fill = fill, colour = fill)) + 
    geom_col_pattern(aes(pattern = fill), pattern_colour = "#B80000", position = "dodge", alpha = 0.9) + 
    facet_wrap(~fac, scales = "free_y", dir = "v", nrow = 2) + 
    theme_edi_airport() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.title = element_blank()) +
    labs(x = "", y = "")  +
    scale_fill_manual(values = edi_airport_colours[c(7, 4:6, 2:1)]) +
    scale_pattern_manual(values = c("stripe", "none", "none", "none", "none", "none")) + 
    scale_colour_manual(values = edi_airport_colours[c(7, 4:6, 2:1)]) + 
    scale_y_continuous(labels = scales::comma) + 
    guides(fill = guide_legend(nrow = 1),
           pattern_colour = guide_legend(nrow = 1))
  figure_sizes$minus_core_rec_fig <- c(8, 4)
  
  figures$robustness_fig <- (robustness_sim_data) %>% 
    summarise(wait_time_60_egate = mean(wait_time_60_egate),
              wait_time_60_desk = mean(wait_time_60_desk),
              exceeds_contingency_egate = mean(exceeds_contingency_egate)/60,
              exceeds_contingency_desk = mean(exceeds_contingency_desk)/60,
              .by = c("egate_uptake", "target_eligibility", "year", "n_egates")) %>% 
    pivot_longer(c(wait_time_60_desk, wait_time_60_egate,
                   exceeds_contingency_egate, exceeds_contingency_desk), 
                 names_to = "which_stat", values_to = "stat") %>% 
    mutate(
      col = case_when(
        str_detect(which_stat, "_desk") ~ "Desk",
        str_detect(which_stat, "_egate") ~ "eGate"),
      target_eligibility = paste0("Eligibility: ", round(100*target_eligibility), "%"),
      which_kpi = if_else(str_detect(which_stat, "exceeds"),
                          "Contingency use (hrs)",
                          "Prop. waits < 1hr")) %>% 
    ggplot(aes(x = egate_uptake, y = stat, colour = col)) + 
    geom_point(size = 2) + 
    facet_grid(which_kpi~target_eligibility, scales = "free_y") +
    theme_edi_airport() + 
    theme(legend.title = element_blank(),
          panel.spacing = unit(2, "lines"),
          strip.text = element_text(margin = margin(0,0,.2,.2, unit = "cm")),
          axis.text.x = element_text(angle = 90)) + 
    labs(x = "eGate Uptake", y = "") + 
    scale_colour_manual(values = edi_airport_colours[2:1]) + 
    scale_y_continuous(labels = scales::comma)
  figure_sizes$robustness_fig <- c(8, 4.2)
  
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
