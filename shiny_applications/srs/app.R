library(shiny)
library(dplyr)
library(ggplot2)
library(DeclareDesign)
library(gridExtra)
library(forcats)

diagnosands <- load("data/srs_template.rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(# Sidebar with a slider input for number of bins
  verticalLayout(plotOutput("distPlot"),
                 wellPanel(
                   sliderInput(
                     inputId = "n",
                     label = "Use the slider to change the sample size",
                     min = 100,
                     max = 1000,
                     value = 100,
                     step = 100,
                     animate = TRUE
                   )
                 )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
      local_sims <- diagnosis$simulations %>% filter(round(n, 3) == round(input$n, 3)) %>%
        mutate(sim_order = fct_reorder(factor(sim_ID), x = (ci_lower + ci_upper)/2),
               covers = as.numeric(ci_lower <= estimand & ci_upper >= estimand))
      local_diagnosis <- diagnosis$diagnosands %>% filter(round(n, 3) == round(input$n, 3))
      
      g1 <- 
        local_sims %>%
        ggplot(aes(est)) +
        geom_histogram(bins = 50) +
        geom_vline(data = local_diagnosis, aes(xintercept = mean_estimand), 
                   color = "red", linetype = "dashed")+
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none", 
              panel.grid.major = element_blank()) +
        coord_cartesian(xlim = c(0.25, 0.75)) +
        xlab("Sample Mean Estimates")
      
      g2 <-
        local_sims %>%
        ggplot(aes(y = sim_order, x = est, color = covers)) +
        geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
        geom_vline(data = local_diagnosis, aes(xintercept = mean_estimand), 
                   color = "red", linetype = "dashed") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none", 
              panel.grid.major = element_blank()) +
        coord_cartesian(xlim = c(0.25, 0.75)) +
        xlab("95% Confidence Intervals")
      
      
      grid.arrange(g1, g2, ncol = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
