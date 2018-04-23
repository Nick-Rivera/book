model_for_outcome_template <- function(N, rho = .5, effect_size = .2){
  population <- declare_population(
    N = N,
    age = rnorm(N),
    income = rnorm(N) + rho * age,
    education = rho * income + rnorm(N),
    noise = rnorm(N),
    Y = effect_size * income + effect_size * education + noise
  )

  estimand_non_zero_effects <- declare_estimand(non_zero_effects = "income, education")
  estimand_age_effect <- declare_estimand(age_effect = "null")

  estimator_right_model <- declare_estimator(
    estimator_function = function(data){
      model_fit <- lm_robust(Y ~ income + education + age, data = data)
      estimates <- tidy(model_fit)
      answer <- paste0(estimates$coefficient_name[which(estimates$p <= .05 & estimates$coefficient_name != "(Intercept)")], collapse = ", ")
      return(data.frame(answer = answer))
    },
    estimand = estimand_non_zero_effects,
    label = right_model
  )

  estimator_age_effect <- declare_estimator(
    estimator_function = function(data){
      model_fit <- lm_robust(Y ~ income + education + age, data = data)
      estimates <- tidy(model_fit)
      answer <- ifelse(estimates$p[estimates$coefficient_name == "age"] > .05,
                       "null", "significant")
      return(data.frame(answer = answer))
    },
    estimand = estimand_age_effect,
    label = age_null_effect
  )

  design <- declare_design(
    population,
    estimand_non_zero_effects, estimand_age_effect,
    estimator_right_model, estimator_age_effect
  )

  return(design)
}

designs <- quick_design(model_for_outcome_template,
                        N = c(50, 250, 500, 1000),
                        rho = seq(from = 0, to = 1, by = .25),
                        effect_size = c(.01, .1, .25, .5))

diagnosis <- diagnose_design(
  designs, diagnosands = declare_diagnosands(proportion_correct = mean(answer == estimand)),
  sims = 1000, bootstrap = FALSE)

saveRDS(diagnosis, file = "examples_data/07_model_for_outcomes.RDS")

