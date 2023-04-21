library(shiny)
library(shinyWidgets)
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

font_add_google("Lato")
showtext_auto()

ui <- fluidPage(
  tags$head(tags$style(
    HTML('

         @import url("https://fonts.googleapis.com/css2?family=Lato:wght@300&display=swap");

         #sidebar {
         background-color: white;
         }

         * {
         font-family: "Lato"; font-size: 20px;
         }')
  )),
  titlePanel("Edinburgh Airport Queue Demand Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      setSliderColor(c("#7D0C6E", "#7D0C6E", "#7D0C6E"), c(1, 2, 3)),
      tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
      sliderInput(
        inputId = "n_egates",
        label = "Number of eGates:",
        min = 10,
        max = 30,
        value = 10, 
        step = 2
      ),
      sliderInput(
        inputId = "target_eligibility",
        label = "Targeted eGate eligibility:",
        min = 0.75,
        max = 1,
        value = 0.85,
        step = .05
      ),
      sliderInput(
        inputId = "egate_uptake",
        label = "eGate uptake:",
        min = .6,
        max = 1.,
        value = 0.8,
        step = .1
      ),
      selectInput("check_filter", "Select bordercheck type(s)",
                  choices = list("Desks", "eGates", "Desks and eGates"),
                  selected = "Desks and eGates"),
      textOutput("author_statement")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Sample KPI Data",
          plotOutput(outputId = "queue_length_plot"),
          plotOutput(outputId = "queue_data_plot"),
          textOutput("bottom_text")
          ),
        tabPanel(
          "By-Year KPI Summaries",
          selectInput("select_kpi", "Select KPI:", 
                      choices = list("Wait time" = 
                                       list("Mean wait (mins)", 
                                            "Proportion waits < 1hr", 
                                            "Proportion waits < 25mins", 
                                            "Proportion waits < 15mins"), 
                                     "Queue length" = 
                                       list("Prob of minor incident (queue > 500) per day"), 
                                            "Prob of major incident (queue > 650) per day")),
          plotOutput(outputId = "kpi_plot"),
          plotOutput(outputId = "joint_kpi_plot")
          )
        )
      )
    )
  )

server <- function(input, output) {
  
  font_add_google("Lato", "lato")
  showtext_auto()
  # input <- list(n_egates = 12, target_eligibility = .85, egate_uptake = .8, select_kpi = "Mean wait (mins)", check_filters = "Desks and eGates")
  check_filters <- list("Desks" = c("Desk"),
                        "eGates" = c("eGate"), 
                        "Desks and eGates" = c("Desk", "eGate"))
  edi_airport_colours <- c("#7D0C6E", "#A19384", "#F2D6EA",
                           "#6E7D0C", "#84A193", "#EAF2D6",
                           "#0C6E7D", "#9384A1", "#D6EAF2",
                           "#2D0255", "#511166", "#B41788")
  queue_length_data <- tar_read(shiny_sim_raw_data) %>% 
    select(-sample_queue_data) %>% 
    unnest(queue_length_data) %>% 
    pivot_longer(cols = c(desk_queue_length, egate_queue_length), 
                 names_to = "Bordercheck type",
                 values_to = "Queue length") %>% 
    mutate(`Bordercheck type` = factor(if_else(`Bordercheck type` == "desk_queue_length", "Desk", "eGate"),
                                 levels = c("Desk", "eGate")))
  
   sample_queue_data <- tar_read(shiny_sim_raw_data) %>% 
    unnest(sample_queue_data) %>% 
    select(-queue_length_data) %>% 
    get_datetime_alternates(c("route")) %>% 
    mutate(year = format(sched_aircraft_date_posix, format = "%Y"),
           `Bordercheck type` = factor(if_else(egate_used == "desk", "Desk", "eGate"),
                                 levels = c("Desk", "eGate")))
    
  output$queue_length_plot <- renderPlot({
    queue_length_data %>% 
      filter(`Bordercheck type` %in% check_filters[[input$check_filter]],
             n_egates == input$n_egates,
             round(egate_uptake, 8) == round(input$egate_uptake, 8),
             round(target_eligibility, 8) == round(input$target_eligibility, 8),
             gen_arrivals_seed == gen_arrivals_seed[1],
             gen_queue_seed == gen_queue_seed[1]) %>% 
      ggplot(aes(x = queue_length_datetime_posix, 
                 y = `Queue length`,
                 colour = `Bordercheck type`)) + 
        geom_point() + 
        facet_wrap(~year, nrow = 1, scales = "free_x") +
        theme_minimal() + 
        theme(plot.title = element_text(family = "Lato",            
                                        size = 40),
              axis.title = element_text(family = "Lato",
                                        size = 30,
                                        margin = margin(2,2,2,2)),
              axis.text = element_text(family = "Lato",
                                       size = 20),
              axis.text.x = element_text(angle = 90),
              legend.text = element_text(family = "Lato", 
                                         size = 30),
              strip.text = element_text(family = "Lato",
                                        size = 30),
              legend.title = element_blank()) + 
        labs(x = "Date and time",
             y = "Queue length") + 
        scale_colour_manual(labels = c("Desk", "eGate"), 
                            values = edi_airport_colours[2:1],
                            drop = FALSE)

  })
  
  output$queue_data_plot <- renderPlot({
    sample_queue_data %>% 
      filter(`Bordercheck type` %in% check_filters[[input$check_filter]],
             n_egates == input$n_egates,
             round(egate_uptake, 8) == round(input$egate_uptake, 8),
             round(target_eligibility, 8) == round(input$target_eligibility, 8),
             gen_arrivals_seed == gen_arrivals_seed[1],
             gen_queue_seed == gen_queue_seed[1]) %>% 
      ggplot(aes(x = route_datetime_posix, y = wait_time / 60, colour = `Bordercheck type`)) +
        geom_point() + 
        facet_wrap(~year, nrow = 1, scales = "free_x") + 
        theme_minimal() + 
        theme(plot.title = element_text(family = "Lato",            
                                        size = 40),
              axis.title = element_text(family = "Lato",
                                        size = 30,
                                        margin = margin(2,2,2,2)),
              axis.text = element_text(family = "Lato",
                                       size = 20),
              axis.text.x = element_text(angle = 90),
              legend.text = element_text(family = "Lato", 
                                         size = 30),
              strip.text = element_text(family = "Lato",
                                        size = 30),
              legend.title = element_blank()) + 
        labs(x = "Date and time",
             y = "Wait time (mins)") + 
        scale_colour_manual(labels = c("Desk", "eGate"), 
                            values = edi_airport_colours[2:1],
                            drop = FALSE)
  })
  
  
  output$kpi_plot <- renderPlot({
    sample_queue_data %>%
      filter(n_egates == input$n_egates,
             round(egate_uptake, 8) == round(input$egate_uptake, 8),
             round(target_eligibility, 8) == round(input$target_eligibility, 8)) %>% 
      nest(year_data = - c("year", "Bordercheck type")) %>% 
      filter(`Bordercheck type` %in% check_filters[[input$check_filter]]) %>% 
      mutate(kpi = map_dbl(year_data, ~queue_data_kpis[[input$select_kpi]](.))) %>%      
      ggplot(aes(x = year, y = kpi, fill = `Bordercheck type`)) + 
        geom_col(position = "dodge", alpha = 0.9) + 
        theme_minimal() + 
        theme(plot.title = element_text(family = "Lato",            
                                        size = 40),
              axis.title = element_text(family = "Lato",
                                        size = 30,
                                        margin = margin(10,10,10,10)),
              axis.text = element_text(family = "Lato",
                                       size = 20),
              legend.text = element_text(family = "Lato", 
                                         size = 30),
              strip.text = element_text(family = "Lato",
                                        size = 30),
              legend.title = element_blank()) + 
          labs(x = "Year", y = input$select_kpi) + 
          scale_fill_manual(labels = c("Desk", "eGate"), 
                            values = edi_airport_colours[2:1],
                            drop = FALSE)
      
  })
  output$joint_kpi_plot <- renderPlot({
    sample_queue_data %>%
      filter(n_egates == input$n_egates,
             round(egate_uptake, 8) == round(input$egate_uptake, 8),
             round(target_eligibility, 8) == round(input$target_eligibility, 8)) %>% 
      nest(year_data = - c("year")) %>% 
      mutate(kpi = map_dbl(year_data, ~queue_data_kpis[[input$select_kpi]](.))) %>%      
      ggplot(aes(x = year, y = kpi, fill = "Both")) + 
      geom_col(alpha = 0.9) + 
      theme_minimal() + 
      theme(plot.title = element_text(family = "Lato",            
                                      size = 40),
            axis.title = element_text(family = "Lato",
                                      size = 30,
                                      margin = margin(10,10,10,10)),
            axis.text = element_text(family = "Lato",
                                     size = 20),
            legend.text = element_text(family = "Lato", 
                                       size = 30),
            strip.text = element_text(family = "Lato",
                                      size = 30),
            legend.title = element_blank()) + 
      labs(x = "Year", y = input$select_kpi) + 
      scale_fill_manual(values = edi_airport_colours[3])
    
  })
  
  output$bottom_text <- renderText("Note that queue lengths (upper panel) are queried once ever fifteen minutes, \n
                                   and to ease congestion/rendering sample wait times (lower panel) are presented \n
                                   for 2,500 passengers.")
  output$author_statement <- renderText("Shiny app authored by Isabella Deutsch and Jacob Bradley.")
}

shinyApp(ui = ui, server = server)
