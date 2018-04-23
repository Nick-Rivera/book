#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyAce)
library(DeclareDesign)

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  headerPanel("Edit the sensitive questions design"),
  tabsetPanel(
    tabPanel("Edit",
  aceEditor("code", mode="r", value = "
proportion_shy <- .06

my_population <- declare_population(
  N = 2000,
  # declare variables for each control item
  raise_minimum_wage = draw_binary(.8, N),
  repeal_obamacare = draw_binary(.6, N),
  ban_assault_weapons = draw_binary(.5, N),
  # declare (unobserved) trump vote variable
  truthful_trump_vote = draw_binary(.45, N),
  # declare (unobserved) shy voter variable
  shy = draw_binary(proportion_shy, N)
)

my_potential_outcomes <- declare_potential_outcomes(
  # declare variables for the potential outcomes of the list experiment
  Y_list_Z_0 = raise_minimum_wage + repeal_obamacare + ban_assault_weapons,
  Y_list_Z_1 = Y_list_Z_0 + truthful_trump_vote,
  # note we define the outcomes for the direct question, but they do not
  # depend on the assignment!
  Y_direct = pmin(truthful_trump_vote - shy, 0)
)

# the estimand is the mean truthful trump vote
my_estimand <- declare_estimand(proportion_truthful_trump_vote = mean(truthful_trump_vote))

# we assign treatment with probability = .5
my_assignment <- declare_assignment(prob = .5)

# our analysis strategy for the list experiment is to take the
#   difference in means of the list outcome
my_estimator_list <- declare_estimator(Y_list ~ Z,
                                       model = difference_in_means,
                                       estimand = my_estimand,
                                       label = list)
# our strategy for the direct question is to take the mean
#   of the direct question
my_estimator_direct <- declare_estimator(
  Y_direct ~ 1,
  model = lm_robust,
  coefficient_name = '(Intercept)',
  estimand = my_estimand,
  label = direct
)

my_design <- declare_design(
  my_population,
  my_potential_outcomes,
  my_estimand,
  my_assignment,
  # we need to reveal the observed outcome for the list experiment
  reveal_outcomes(outcome_variable_names = Y_list),
  my_estimator_list,
  my_estimator_direct
)"),
           actionButton("eval", "Update Diagnose")
    ),
    tabPanel("Diagnosis",
           htmlOutput("output")
    ),
    tabPanel("Data",
             downloadButton('downloadData', 'Download'),
             dataTableOutput("designData"))
  )
)

diagnose_code <- "\n get_diagnosands(diagnose_design(my_design, sims = 5))"

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$output <- renderTable({
    input$eval
    return(isolate(eval(parse(text = paste0(input$code, diagnose_code)))))
  })

  output$output <- renderTable({
    input$eval
    return(isolate(eval(parse(text = paste0(input$code, diagnose_code)))))
  })

}

# Run the application
shinyApp(ui = ui, server = server)

