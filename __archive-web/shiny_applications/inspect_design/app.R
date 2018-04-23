#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DeclareDesign)
library(dplyr)
library(ggplot2)
library(DT)
library(gridExtra)

options(digits = 3)

dd_theme <-
  theme_bw()  %+replace%
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = '#eeeeee'),
    strip.background = element_blank(),
    legend.position = "bottom"
  )

list_design <-  readRDS("data/list_experiments_design.rdata")
audit_design <- readRDS("data/06_audit_experiments_design.rds")
rd_design <-    readRDS("data/rd_design.rds")
srs_design <-   readRDS("data/srs_design.rds")
designs <- list("list" = list_design, "audit" = audit_design, "rd" = rd_design,
                "srs" = srs_design)

design_names <- list("list" = "List Experiment", "audit" = "Audit Experiment",
                     "rd" = "Regression Discontinuity", "srs" = "Simple Random Sample")

list <-  readRDS("data/05_list_experiments_intext.rdata")
audit <- readRDS("data/06_audit_experiments_in_text.rds")
rd <-    readRDS("data/rd_diagnosis.rds")
srs <-   readRDS("data/srs_intext.rds")

sims <- list$simulations %>% mutate(design = "list") %>%
  bind_rows(audit$simulations %>% mutate(design = "audit")) %>%
  bind_rows(rd$simulations %>% mutate(design = "rd")) %>%
  bind_rows(srs$simulations %>% mutate(design = "srs"))

diagnosands <- list$diagnosands %>% mutate(design = "list") %>%
  bind_rows(audit$diagnosands %>% mutate(design = "audit")) %>%
  bind_rows(rd$diagnosands %>% mutate(design = "rd")) %>%
  bind_rows(srs$diagnosands %>% mutate(design = "srs"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    html * {
                    font-family: 'Palatino';
                    }

                    "))
    ),

  titlePanel(textOutput("design_name_print")),
                # Sidebar with a slider input for number of bins
                              # Show a plot of the generated distribution
                              mainPanel(tabsetPanel(
                                tabPanel("Sampling Distribution\nof the Estimates",
                                         plotOutput("distPlot")) ,
                                tabPanel("Diagnosis",
                                         dataTableOutput("diagnosands")),
                                tabPanel("Mock Data",
                                         dataTableOutput("data"))
                              )))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  design_name <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    query[[which(names(query) == "design")]]
  })

  output$design_name_print <- renderText({
    paste0("Design Inspector: ", design_names[[design_name()]])
  })

  output$diagnosands <- renderDataTable({
    #matrix(sapply(diagnosands_reactive(), prettyNum), nrow = 1)
    df <- diagnosands_reactive()
    for (i in 1:ncol(df)) {
      df[, i] <- prettyNum(df[, i])
    }
    names(df)[1:2] <- c("estimand", "estimator")
    df[, "design"] <- NULL
    return(df)
  }, options = list(dom = "t"), rownames = FALSE)

  diagnosands_reactive <- reactive({
    diagnosands %>% filter(design == design_name())
    # df <- subset(diagnosands, design == design_name())
    # print(class(df))
    # df
  })

  simulations_reactive <- reactive({
    subset(sims, design == design_name())
  })

  design_reactive <- reactive({
    designs[[design_name()]]
  })

  output$data <- renderDataTable({
    draw_data(design_reactive())
  }, rownames = FALSE)


  output$distPlot <- renderPlot({

    local_sims <- simulations_reactive() %>%
      mutate(sim_order = fct_reorder(factor(sim_ID), x = (ci_lower + ci_upper)/2),
             covers = as.numeric(ci_lower <= estimand & ci_upper >= estimand))

    g1 <-
      local_sims %>%
      ggplot(aes(est)) +
      geom_histogram(bins = 50) +
      geom_vline(data = diagnosands_reactive(), aes(xintercept = mean_estimand),
                 color = "red", linetype = "dashed")+
      dd_theme +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank()) +
      #coord_cartesian(xlim = c(0.25, 0.75)) +
      xlab("Sample Mean Estimates")

    g2 <-
      local_sims %>%
      ggplot(aes(y = sim_order, x = est, color = covers)) +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
      geom_vline(data = diagnosands_reactive(),
                 aes(xintercept = mean_estimand),
                 color = "red", linetype = "dashed") +
      dd_theme +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank()) +
      #coord_cartesian(xlim = c(0.25, 0.75)) +
      xlab("95% Confidence Intervals")

      grid.arrange(g1, g2, ncol = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
