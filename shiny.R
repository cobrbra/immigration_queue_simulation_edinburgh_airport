library(shiny)
library(targets)
library(tidyverse)

ui <- fluidPage(
  
  titlePanel("Edinburgh Airport Queue Demand Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "n_egates",
        label = "Number of eGates:",
        min = 10,
        max = 14,
        value = 10, 
        step = 2
      ),
      sliderInput(
        inputId = "elig_boost",
        label = "Increased eGate eligibility:",
        min = 0.0,
        max = 1,
        value = 0.0,
        step = .1
      ),
      sliderInput(
        inputId = "egate_uptake",
        label = "eGate uptake:",
        min = .6,
        max = 1.,
        value = 0.8,
        step = .2
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "queue_length_plot")
    )
  )
)

server <- function(input, output) {
  
  output$queue_length_plot <- renderPlot({
    # input <- list(n_egates = 14,
    #               egate_uptake = 1.,
    #               elig_boost = 0)
    tar_read(shiny_sim_data) %>% 
      unnest(queue_length_data) %>% 
      pivot_longer(cols = c(desk_queue_length, egate_queue_length), 
                   names_to = "Check Type",
                   values_to = "Queue Length") %>% 
      mutate(`Check Type` = if_else(`Check Type` == "desk_queue_length", "Desk", "eGate")) %>% 
      filter(n_egates == input$n_egates,
             egate_uptake == input$egate_uptake,
             elig_boost == input$elig_boost) %>% 
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
}

shinyApp(ui = ui, server = server)
