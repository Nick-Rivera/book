rm(list = ls())

# I don't know how to do this one!
# TOo hard!

library(tidyverse)

vsg <- read_csv("real_data/VOTER_Survey_December16_Release1.csv")

trump <-
  vsg %>%
  mutate(therm_muslim = muslims_t_baseline + 1) %>%
  filter(presvote16post_2016 == 2) %>%
  select(
    vote_for_against_2016,
    fav_trump_2016,
    cmatch_romn_baseline,
    taxdoug_2016,
    univhealthcov_2016,
    RIGGED_SYSTEM_3_2016,
    wealth_2016,
    RIGGED_SYSTEM_5_2016,
    immi_naturalize_2016,
    immi_makedifficult_2016,
    imiss_c_2016,
    amlived_2016,
    race_importance_2016,
    amwhite_2016,
    race_fate_2016,
    race_tryharder_2016,
    immi_muslim_2016,
    muslims_t_baseline,
    SOCIAL_CONFORMITY_3_2016,
    SOCIAL_CONFORMITY_4_2016,
    conviction_accuracy_2016,
    selfdescr_ccap_10_baseline) %>%
  na.omit


trump_cluster <- kmeans(x = trump, centers = 3, nstart = 10)
table(trump_cluster$cluster)

trump_cluster <- kmeans(x = trump, centers = 10, nstart = 10)
table(trump_cluster$cluster)

