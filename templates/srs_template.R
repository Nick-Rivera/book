
srs_template <- function(n){

# Model -------------------------------------------------------------------
population <-
  declare_population(
    N = 5000,
    latent_ideology = rnorm(N),
    Y = draw_discrete(latent_ideology,
                      link = "probit",
                      breaks = qnorm(seq(0, 1, length.out = 8)))
  )

# Inquiry -----------------------------------------------------------------
estimand <- declare_estimand(mean(Y), label = "Ybar")


# Data Strategy -----------------------------------------------------------
sampling <- declare_sampling(n = n)

# Answer Strategy ---------------------------------------------------------
estimator <- declare_estimator(Y ~ 1,
                               model = lm_robust,
                               coefficient_name = "(Intercept)",
                               estimand = estimand)

# Design ------------------------------------------------------------------
fixed_pop <- population()
declare_design(fixed_pop, estimand, sampling, estimator)
}

library(DeclareDesign)
library(tidyverse)
library(forcats)
designs <- quick_design(template = srs_template, n = seq(100, 1000, by = 100))
diagnosis <- diagnose_design(designs, sims = 200, bootstrap = FALSE)

save(diagnosis, file = "examples_data/srs_template.rdata")
save(diagnosis, file = "shiny_applications/srs/data/srs_template.rdata")

local_sims <- diagnosis$simulations %>% filter(n == 100) %>%
  mutate(sim_order = fct_reorder(factor(sim_ID), x = (ci_lower + ci_upper)/2),
         covers = as.numeric(ci_lower <= estimand & ci_upper >= estimand))
local_diagnosis <- diagnosis$diagnosands %>% filter(n == 100)

g1 <- 
  local_sims %>%
  ggplot(aes(est)) +
  geom_histogram(bins = 50) +
  geom_vline(data = local_diagnosis, aes(xintercept = mean_estimand), 
             color = "red", linetype = "dashed")+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  xlab("Sample Mean Estimates")
g1


g2 <-
  local_sims %>%
  ggplot(aes(y = sim_order, x = est, color = covers)) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  geom_vline(data = local_diagnosis, aes(xintercept = mean_estimand), 
             color = "red", linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  xlab("95% Confidence Intervals")

