#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

diagnosands <- readRDS("data/list_experiment_diagnosands.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
           radioButtons("sample_size",
                     "Sample Size:",
                     choices = c(1000, 2000),
                     selected = 2000)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
     diagnosands %>%
       filter(sample_size == input$sample_size) %>%
       select(proportion_shy, estimator_label, bias, rmse) %>%
       gather(key = diagnosand_label,
              value = diagnosand,
              bias,
              rmse) %>%
       ggplot(aes(x = proportion_shy, y = diagnosand,
                  group = estimator_label, color = estimator_label)) +
       geom_line() +
       facet_wrap(~ diagnosand_label) +
       theme_bw()
   })
}

# Run the application
shinyApp(ui = ui, server = server)

