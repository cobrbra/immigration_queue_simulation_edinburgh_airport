library(shiny)
library(here)
library(targets)
library(tidyverse)
library(showtext)

tar_source(files = c(
  here("code/process_data.R"), 
  here("code/step_1_aircraft.R"),
  here("code/step_2_route.R"),
  here("code/step_3_immigration.R"),
  here("code/kpis.R"),
  here("code/analysis.R"),
  here("code/get_figures.R"),
  here("code/get_tables.R")
))

ui <- fluidPage(
  
  titlePanel("Edinburgh Airport Queue Demand Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "n_egates",
        label = "Number of eGates:",
        min = 10,
        max = 20,
        value = 10, 
        step = 2
      ),
      sliderInput(
        inputId = "elig_boost",
        label = "Increased eGate eligibility:",
        min = 0.0,
        max = 1,
        value = 0.0,
        step = .25
      ),
      sliderInput(
        inputId = "egate_uptake",
        label = "eGate uptake:",
        min = .6,
        max = 1.,
        value = 0.8,
        step = .1
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Raw KPI Data",
          plotOutput(outputId = "queue_length_plot"),
          plotOutput(outputId = "queue_data_plot")
          ),
        tabPanel(
          "KPI Summaries",
          selectInput("select_kpi", "Select KPI:", 
                      choices = list("Wait time" = 
                                       list("Mean wait (mins)", 
                                            "Proportion waits < 1hr", 
                                            "Proportion waits < 25mins", 
                                            "Proportion waits < 15mins"), 
                                     "Queue Length" = 
                                       list("Prob of minor incident (queue > 500) per day"), 
                                            "Prob of major incident (queue > 650) per day")),
          plotOutput(outputId = "kpi_plot")
          )
        )
      )
    )
  )

server <- function(input, output) {
  queue_length_data <- tar_read(shiny_sim_data) %>% 
    select(-sample_queue_data) %>% 
    unnest(queue_length_data) %>% 
    pivot_longer(cols = c(desk_queue_length, egate_queue_length), 
                 names_to = "Check Type",
                 values_to = "Queue Length") %>% 
    mutate(`Check Type` = if_else(`Check Type` == "desk_queue_length", "Desk", "eGate"))
  
   sample_queue_data <- tar_read(shiny_sim_data) %>% 
    unnest(sample_queue_data) %>% 
    select(-queue_length_data) %>% 
    get_datetime_alternates(c("route")) %>% 
    mutate(year = format(sched_aircraft_date_posix, format = "%Y"),
           `Check Type` = factor(if_else(egate_used == "desk", "Desk", "eGate"),
                                 levels = c("Desk", "eGate")))
    
  output$queue_length_plot <- renderPlot({
    queue_length_data %>% 
      filter(n_egates == input$n_egates,
             egate_uptake == input$egate_uptake,
             elig_boost == input$elig_boost,
             gen_arrivals_seed == gen_arrivals_seed[1],
             gen_queue_seed == gen_queue_seed[1]) %>% 
      ggplot(aes(x = queue_length_datetime_posix, 
                 y = `Queue Length`,
                 colour = `Check Type`)) + 
        geom_point() + 
        facet_wrap(~year, nrow = 1, scales = "free_x") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 60)) + 
        labs(x = "Date and Time",
             y = "Queue Length")

  })
  
  output$queue_data_plot <- renderPlot({
    sample_queue_data %>% 
      filter(n_egates == input$n_egates,
             egate_uptake == input$egate_uptake,
             elig_boost == input$elig_boost,
             gen_arrivals_seed == gen_arrivals_seed[1],
             gen_queue_seed == gen_queue_seed[1]) %>% 
      ggplot(aes(x = route_datetime_posix, y = wait_time / 3600, colour = `Check Type`)) +
        geom_point() + 
        facet_wrap(~year, nrow = 1, scales = "free_x") + 
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 60)) + 
        labs(x = "Date and Time",
             y = "Wait Time (Hours)")
  })
  
  output$kpi_plot <- renderPlot({
    tar_read(shiny_sim_data) %>%
      select(-queue_length_data) %>% 
      filter(n_egates == input$n_egates,
             egate_uptake == input$egate_uptake,
             elig_boost == input$elig_boost) %>% 
      unnest(sample_queue_data) %>% 
      mutate(year = format(sched_aircraft_date_posix, format = "%Y"),
             `Check Type` = factor(if_else(egate_used == "desk", "Desk", "eGate"),
                                   levels = c("Desk", "eGate"))) %>% 
      nest(year_data = - c(year)) %>% 
      mutate(kpi = map_dbl(year_data, ~queue_data_kpis[[input$select_kpi]](.))) %>%      
      ggplot(aes(x = year, y = kpi)) + 
        geom_col(position = "dodge") + 
        theme_minimal() + 
        labs(x = "Year", y = input$select_kpi)
      
  })
}

shinyApp(ui = ui, server = server)
