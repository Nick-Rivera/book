library(dplyr)
library(DeclareDesign)

factorial_template <-
  function(N, beta_1 = 0, beta_2 = 0, beta_3) {
    population <- declare_population(noise = rnorm(N), N = N)
    potential_outcomes <-
      declare_potential_outcomes(
        Y_Z_0 = noise,
        Y_Z_1 = beta_1 + noise,
        Y_Z_2 = beta_2 + noise,
        Y_Z_3 = beta_1 + beta_2 + beta_3 + noise
      )
    assignment_factorial <-
      declare_assignment(condition_names = 0:3)

    estimand <-
      declare_estimand(interaction_effect = mean((Y_Z_3 - Y_Z_2) - (Y_Z_1 - Y_Z_0)))

    estimator <- declare_estimator(
      formula = Y ~ Z1 + Z2 + Z1 * Z2,
      model = lm_robust,
      coefficient_name = "Z1:Z2",
      estimand = estimand
    )
    declare_design(
      population,
      potential_outcomes,
      assignment_factorial,
      mutate(Z1 = as.numeric(Z %in% c(1, 3)),
             Z2 = as.numeric(Z %in% c(2, 3))),
      reveal_outcomes,
      estimator,
      estimand
    )
  }

designs <- quick_design(factorial_template,
                        N = seq(200, 2000, 200),
                        beta_3 = seq(.1, .5, .05))

diagnoses <- diagnose_design(designs, sims = 1000, bootstrap = FALSE)

saveRDS(diagnoses, file = "shiny_applications/factorial/data/factorial_forshiny.RDS")


## ---- echo=FALSE---------------------------------------------------------
library(ggplot2)
diagnoses$diagnosands %>%
  ggplot(aes(N, power)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.9, color = "red") +
  ylim(0, 1) +
  xlab("Number of Subjects") +
  ylab("Power to Detect a 0.1 SD Interaction") +
  ggtitle("2x2 Factorial Experiment", "Power Analysis") +
  facet_wrap(~beta_3) +
  theme_bw()

