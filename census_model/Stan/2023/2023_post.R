library(cmdstanr)
library(posterior)
library(bayesplot)

d <- jsonlite::read_json(
  "census_model/Stan/2023_stan_data.json",
  simplifyVector = TRUE
)
fit <- as_cmdstan_fit(
  list.files('~/Desktop/stan_out', pattern = "csv$", full.names = TRUE)
)

fit$diagnostic_summary()
### Nothing divergent
# $num_divergent
# [1] 0 0 0 0
#
# $num_max_treedepth
# [1] 0 0 0 0
#
# $ebfmi
# [1] 0.9505608 0.9272032 1.0466857 0.9593030

fit$summary(
  variables = "Nsuper_realized",
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)

mcmc_trace(fit$draws("p_sss"))

fit$summary(
  variables = c("M")
)
mcmc_hist(fit$draws("LambdaSuper"))


M <- fit$draws("M", format = "matrix")
ppc_dens_overlay(y = d$sssMat, yrep = U[1:25, ])
