library(DeclareDesign)
library(dplyr)
library(ggplot2)
library(tidyr)

list_experiment_template <- function(proportion_shy, N) {
  population <-
    declare_population(
      N = 5000,
      # true trump vote (unobservable)
      truthful_trump_vote = draw_binary(.45, N),

      # shy voter (unobservable)
      shy = draw_binary(proportion_shy, N),

      # Direct question response (1 if Trump supporter and not shy, 0 otherwise)
      Y_direct = as.numeric(truthful_trump_vote == 1 &
                              shy == 0),

      # Nonsensitive list experiment items
      raise_minimum_wage = draw_binary(.8, N),
      repeal_obamacare = draw_binary(.6, N),
      ban_assault_weapons = draw_binary(.5, N)
    )

  potential_outcomes <-
    declare_potential_outcomes(
      Y_list_Z_0 = raise_minimum_wage + repeal_obamacare + ban_assault_weapons,
      Y_list_Z_1 = Y_list_Z_0 + truthful_trump_vote
    )

  ## ------------------------------------------------------------------------
  estimand <-
    declare_estimand(proportion_truthful_trump_vote = mean(truthful_trump_vote))

  ## ------------------------------------------------------------------------
  sampling <-
    declare_sampling(n = 500)
  assignment <-
    declare_assignment(prob = .5)

  ## ------------------------------------------------------------------------
  estimator_direct <-
    declare_estimator(
      Y_direct ~ 1,
      model = lm_robust,
      coefficient_name = "(Intercept)",
      estimand = estimand,
      label = direct
    )

  estimator_list <-
    declare_estimator(Y_list ~ Z,
                      model = difference_in_means,
                      estimand = estimand,
                      label = list)

  ## ------------------------------------------------------------------------
  design <- declare_design(
    population,
    potential_outcomes,
    sampling,
    estimand,
    assignment,
    reveal_outcomes(outcome_variable_names = Y_list),
    estimator_direct,
    estimator_list
  )

  return(design)
}

saveRDS(list_experiment_template, file = "examples_data/05_list_experiment_template.RData")

designs <- quick_design(
  template = list_experiment_template,
  proportion_shy = c(0, .1, .2, .3, .4, .5),
  N = c(1000, 2000)
)

diagnosis <- diagnose_design(designs, sims = 5)

save(diagnosis, file = "examples_data/05_list_experiments.rdata")
