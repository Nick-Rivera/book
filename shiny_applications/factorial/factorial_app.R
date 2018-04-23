library(shiny)
library(dplyr)
library(ggplot2)
library(DeclareDesign)

diagnosands <- readRDS("data/factorial_forshiny.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(# Sidebar with a slider input for number of bins
  verticalLayout(plotOutput("distPlot"),
                 wellPanel(
                   sliderInput(
                     inputId = "beta_3",
                     label = "Use the slider to change the true size of the interaction effect",
                     min = 0.1,
                     max = 0.5,
                     value = 0.1,
                     step = 0.05,
                     animate = TRUE
                   )
                 )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    diagnosands$diagnosands %>%
      filter(round(beta_3, 3) == round(input$beta_3, 3)) %>%
      ggplot(aes(N, power)) +
      geom_point() +
      geom_line() +
      geom_hline(yintercept = 0.9, color = "red") +
      scale_x_continuous(breaks = seq(200, 2000, 200)) +
      scale_y_continuous(breaks = seq(0, 1, .1)) +
      xlab("Number of Subjects") +
      ylab("Power") +
      ggtitle(
        "2x2 Factorial Experiment",
        paste0(
          "Power analysis when true interaction effect is ",
          input$beta_3,
          " standard deviations"
        )
      ) +
      theme_bw()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
