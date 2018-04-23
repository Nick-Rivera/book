library(shiny)
library(dplyr)
library(ggplot2)
library(DeclareDesign)

diagnosands <- readRDS("data/07_model_for_outcomes.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(# Sidebar with a slider input for number of bins
  verticalLayout(plotOutput("distPlot"),
                 wellPanel(
                   sliderInput(
                     inputId = "rho",
                     label = "Use the slider to change the correlation between independent variables",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.25,
                     animate = TRUE
                   )
                 )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    get_diagnosands(diagnosis) %>%
      filter(round(rho, 3) == round(input$rho, 3)) %>%
      ggplot(aes(N, proportion_correct, group = estimator_label, linetype = estimator_label)) +
      geom_point() +
      geom_line() +
      facet_wrap(~ effect_size) +
      #geom_hline(yintercept = 0.9, color = "red") +
      scale_x_continuous(breaks = seq(200, 1000, 200)) +
      scale_y_continuous(breaks = seq(0, 1, .1)) +
      xlab("Number of Subjects") +
      ylab("Proportion of Successful Decisions") +
      ggtitle(
        "Model for Outcomes",
        paste0(
          "Analyze how often the correct decision is determined when the correlation between variables is ",
          input$rho,
          " random units (fix to correlations!)"
        )
      ) +
      theme_bw()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
