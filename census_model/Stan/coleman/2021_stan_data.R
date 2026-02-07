library(data.table)
library(cmdstanr)

load("census_model/coleman/census2021_inputs.RData")

### Set up data ###
data_list <- list(
  K = jags.data$K,
  G = jags.data$G,
  n_telem = sum(rowSums(jags.data$y) > jags.data$K),
  y = jags.data$y,
  TelemIndicator = jags.data$TelemIndicator,
  temp = jags.data$temp,
  Ksss = jags.data$Ksss,
  V = jags.data$V,
  sssMat = jags.data$sssMat,
  sssReach = rep(3, length(jags.data$sssSurveyOcc)),
  sssSurveyOcc = jags.data$sssSurveyOcc
)


cmdstanr::write_stan_json(data_list, "cansus_model/Stan/2021_stan_data.json")
