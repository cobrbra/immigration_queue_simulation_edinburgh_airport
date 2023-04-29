library(shiny)
library(shinyWidgets)

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
        min = 0.8,
        max = 1,
        value = 0.8,
        step = .05
      ),
      sliderInput(
        inputId = "egate_uptake",
        label = "eGate uptake:",
        min = .75,
        max = 1.,
        value = 0.75,
        step = .05
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
                                            "Proportion waits < 15mins"), 
                                     "Queue length" = 
                                       list("Minutes exceeding overflow"), 
                                     "Minutes exceeding contingency")),
          plotOutput(outputId = "kpi_plot"),
          plotOutput(outputId = "joint_kpi_plot")
        )
      )
    )
  )
)