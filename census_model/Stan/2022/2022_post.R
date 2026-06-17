library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(ggplot2)

# mod <- cmdstan_model('census_model/Stan/models/census_model_inclUUNR.stan', compile = F)
# mod$variables()

d <- jsonlite::read_json(
  "census_model/Stan/2022/censusUUNR/UUNR_stan_data.json",
  simplifyVector = TRUE
)
fit <- readRDS("~/Desktop/out2022_UUNR.RDS")

fit$diagnostic_summary() # Nothing divergent

mcmc_trace(fit$draws("LambdaSuper"))

fit$summary(
  variables = c(
    "LambdaSuper",
    "Nsuper_realized",
    "pbar_raw",
    "p_sss",
    "alpha1",
    "beta1"
    # "move_probs_LNR",
    # "move_probs_LMC",
    # "move_probs_UMC",
    # "move_probs_UNR"
  ),
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)
mcmc_hist(
  fit$draws("p_sss")
)


N <- fit$summary(
  variables = 'N',
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
) |>
  mutate(
    day = as.Date(
      as.numeric(gsub('N\\[|\\]', '', variable)) - 1,
      origin = '2021-08-15'
    )
  )

ggplot(data = N) +
  geom_pointrange(
    aes(
      x = day,
      y = q50,
      ymin = q2.5,
      ymax = q97.5
    )
  ) +
  ylim(0, 125)


theta <- fit$summary(
  variables = c("init_probs", "entry_probs"),
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)
