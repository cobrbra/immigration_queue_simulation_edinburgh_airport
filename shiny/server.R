library(here)
library(tidyverse)
# library(showtext)
library(vroom)

server <- function(input, output) {
  source("credentials/credentials.R")
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # font_add_google("Lato", "lato")
  # showtext_auto()
  
  # input <- list(n_egates = 10, target_eligibility = .8, egate_uptake = .6, select_kpi = "Mean wait (mins)", check_filter = "Desks and eGates")
  check_filters <- list("Desks" = c("Desk"),
                        "eGates" = c("eGate"), 
                        "Desks and eGates" = c("Desk", "eGate"))
  edi_airport_colours <- c("#7D0C6E", "#A19384", "#F2D6EA",
                           "#6E7D0C", "#84A193", "#EAF2D6",
                           "#0C6E7D", "#9384A1", "#D6EAF2",
                           "#2D0255", "#511166", "#B41788")
  kpis <- list(
    "Mean wait (mins)" = "mean_wait_time", 
    "Proportion waits < 1hr" = "wait_time_60", 
    "Proportion waits < 25mins" = "wait_time_25", 
    "Proportion waits < 15mins" = "wait_time_15",
    "Minutes exceeding overflow" = "exceeds_overflow",
    "Minutes exceeding contingency" = "exceeds_contingency"
  )
  
  shiny_raw_passenger_data <- vroom("shiny_data/raw_passenger_data.csv") %>% 
    select(-c(nationality, egate_failed, gen_arrivals_seed, gen_queue_seed, bordercheck_start_time, bordercheck_end_time, sched_aircraft_date_posix))
  shiny_raw_queue_length_data <- vroom("shiny_data/raw_queue_length_data.csv") %>% 
    select(-c(gen_arrivals_seed, gen_queue_seed, queue_length_datetime_int, queue_length_date_posix, queue_length_time_int))
  shiny_sim_kpi_data <- vroom("shiny_data/kpi_data.csv")
  
  queue_length_data <- shiny_raw_queue_length_data %>% 
    pivot_longer(cols = c(desk_queue_length, egate_queue_length), 
                 names_to = "Bordercheck type",
                 values_to = "Queue length") %>% 
    mutate(`Bordercheck type` = factor(if_else(`Bordercheck type` == "desk_queue_length", "Desk", "eGate"),
                                       levels = c("Desk", "eGate")))
  
  sample_queue_data <- shiny_raw_passenger_data %>% 
    mutate(route_datetime_posix = as.POSIXct(route_datetime_int, origin = "1970-01-01 00:00:00")) %>% 
    mutate(`Bordercheck type` = factor(if_else(egate_used == "desk", "Desk", "eGate"),
                                       levels = c("Desk", "eGate")))
  
  output$queue_length_plot <- renderPlot({
    queue_length_data %>% 
      filter(`Bordercheck type` %in% check_filters[[input$check_filter]],
             n_egates == input$n_egates,
             round(egate_uptake, 8) == round(input$egate_uptake, 8),
             round(target_eligibility, 8) == round(input$target_eligibility, 8)) %>% 
      ggplot(aes(x = queue_length_datetime_posix, 
                 y = `Queue length`,
                 colour = `Bordercheck type`)) + 
      geom_line(size = 2) + 
      facet_wrap(~year, nrow = 1, scales = "free_x") +
      theme_minimal() + 
      theme(plot.title = element_text(#family = "Lato",            
                                      size = 40),
            axis.title = element_text(#family = "Lato",
                                      size = 30,
                                      margin = margin(2,2,2,2)),
            axis.text = element_text(#family = "Lato",
                                     size = 20),
            axis.text.x = element_text(angle = 90),
            legend.text = element_text(#family = "Lato", 
                                       size = 30),
            strip.text = element_text(#family = "Lato",
                                      size = 30),
            legend.title = element_blank()) + 
      labs(x = "Date and time",
           y = "Queue length") + 
      scale_colour_manual(labels = c("Desk", "eGate"), 
                          values = edi_airport_colours[2:1],
                          drop = FALSE) + 
      scale_y_continuous(labels = scales::comma)
  })
  
  output$queue_data_plot <- renderPlot({
    sample_queue_data %>% 
      filter(`Bordercheck type` %in% check_filters[[input$check_filter]],
             n_egates == input$n_egates,
             round(egate_uptake, 8) == round(input$egate_uptake, 8),
             round(target_eligibility, 8) == round(input$target_eligibility, 8)) %>% 
      ggplot(aes(x = route_datetime_posix, y = wait_time / 60, colour = `Bordercheck type`)) +
      geom_point() + 
      facet_wrap(~year, nrow = 1, scales = "free_x") + 
      theme_minimal() + 
      theme(plot.title = element_text(#family = "Lato",            
                                      size = 40),
            axis.title = element_text(#family = "Lato",
                                      size = 30,
                                      margin = margin(2,2,2,2)),
            axis.text = element_text(#family = "Lato",
                                     size = 20),
            axis.text.x = element_text(angle = 90),
            legend.text = element_text(#family = "Lato", 
                                       size = 30),
            strip.text = element_text(#family = "Lato",
                                      size = 30),
            legend.title = element_blank()) + 
      labs(x = "Date and time",
           y = "Wait time (mins)") + 
      scale_colour_manual(labels = c("Desk", "eGate"), 
                          values = edi_airport_colours[2:1],
                          drop = FALSE) + 
      scale_y_continuous(labels = scales::comma)
  })
  
  
  output$kpi_plot <- renderPlot({
    (shiny_sim_kpi_data) %>%
      filter(n_egates == input$n_egates,
             round(egate_uptake, 8) == round(input$egate_uptake, 8),
             round(target_eligibility, 8) == round(input$target_eligibility, 8)) %>% 
      pivot_longer(cols = paste0(kpis[[input$select_kpi]], c("_desk", "_egate")),
                   names_to = "check", values_to = "kpi") %>%
      summarise(kpi = mean(kpi), .by = c("year", "check")) %>%
      mutate(check = factor(if_else(str_detect(check, "desk"), "Desk", "eGate"),
                            levels = c("Desk", "eGate"))) %>%
      filter(check %in% check_filters[[input$check_filter]]) %>% 
      ggplot(aes(x = year, y = kpi, fill = check)) + 
      geom_col(position = "dodge", alpha = 0.9) + 
      theme_minimal() + 
      theme(plot.title = element_text(#family = "Lato",            
                                      size = 40),
            axis.title = element_text(#family = "Lato",
                                      size = 30,
                                      margin = margin(10,10,10,10)),
            axis.text = element_text(#family = "Lato",
                                     size = 20),
            legend.text = element_text(#family = "Lato", 
                                       size = 30),
            strip.text = element_text(#family = "Lato",
                                      size = 30),
            legend.title = element_blank()) + 
      labs(x = "Year", y = input$select_kpi) + 
      scale_fill_manual(labels = c("Desk", "eGate"), 
                        values = edi_airport_colours[2:1],
                        drop = FALSE) + 
      scale_y_continuous(labels = scales::comma)
  })
  
  output$joint_kpi_plot <- renderPlot({
    (shiny_sim_kpi_data) %>%
      filter(n_egates == input$n_egates,
             round(egate_uptake, 8) == round(input$egate_uptake, 8),
             round(target_eligibility, 8) == round(input$target_eligibility, 8)) %>% 
      select(year, kpi = all_of(paste0(kpis[[input$select_kpi]], "_both"))) %>%
      summarise(kpi = mean(kpi), .by = "year") %>% 
      ggplot(aes(x = year, y = kpi, fill = "Both")) + 
      geom_col(alpha = 0.9) + 
      theme_minimal() + 
      theme(plot.title = element_text(#family = "Lato",            
                                      size = 40),
            axis.title = element_text(#family = "Lato",
                                      size = 30,
                                      margin = margin(10,10,10,10)),
            axis.text = element_text(#family = "Lato",
                                     size = 20),
            legend.text = element_text(#family = "Lato", 
                                       size = 30),
            strip.text = element_text(#family = "Lato",
                                      size = 30),
            legend.title = element_blank()) + 
      labs(x = "Year", y = input$select_kpi) + 
      scale_fill_manual(values = edi_airport_colours[3]) + 
      scale_y_continuous(labels = scales::comma)
    
  })
  
  output$bottom_text <- renderText("Note that queue lengths (upper panel) are queried once ever fifteen minutes, \n
                                   and to ease congestion/rendering sample wait times (lower panel) are presented \n
                                   for 2,500 passengers.")
  output$author_statement <- renderText("Shiny app authored by Isabella Deutsch and Jacob Bradley.")
}