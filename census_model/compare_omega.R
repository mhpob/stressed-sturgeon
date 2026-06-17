library(cmdstanr)
library(posterior)
library(bayesplot)

o1_2020 <- readRDS("census_model/Stan/2020/out2020_omega1_stan.RDS")
o0_2020 <- readRDS("census_model/Stan/2020/out2020_stan_omega0.RDS")
o1_2020$summary(
  variables = c(
    "LambdaSuper",
    "Nsuper_realized"
  ),
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)
o0_2020$summary(
  variables = c(
    "LambdaSuper",
    "Nsuper_realized"
  ),
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)


o0_2021 <- readRDS("~/Desktop/out2021_stan.RDS")
o1_2021 <- readRDS("~/Desktop/out2021_omega1_stan.RDS")
o1_2021$diagnostic_summary()
o1_2021$summary(
  variables = c(
    "LambdaSuper",
    "Nsuper_realized"
  ),
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)
o0_2022 <- readRDS("~/Desktop/out2022_omega0_stan.RDS")
o0_2022_lmc <- readRDS("census_model/Stan/2022/out2022_lmc_omega0.RDS")
o0_2023 <- readRDS("~/Desktop/out2023_omega0_stan.RDS")
o0_2023_lmc <- readRDS("census_model/Stan/2023/out2023_lmc_omega0.RDS")
o0_2024 <- readRDS("census_model/Stan/2024/out2024_omega0_stan.RDS")
o0_2024_lmc <- readRDS("census_model/Stan/2024/out2024_lmc_omega0.RDS")

o0_2022$summary(
  variables = "Nsuper_realized",
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)
o0_2023$summary(
  variables = "Nsuper_realized",
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)

o1_2022 <- readRDS("~/Desktop/out2022_stan.RDS")
o1_2022_lmc <- readRDS("census_model/Stan/2022/out2022_lmc_omega1.RDS")


o1_2023 <- as_cmdstan_fit(
  list.files('~/Desktop/stan_out', pattern = "csv$", full.names = TRUE)
)
o1_2022$summary(
  variables = "Nsuper_realized",
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)
o1_2023$summary(
  variables = "Nsuper_realized",
  ~ quantile2(.x, probs = c(0.5, 0.025, 0.975))
)
o1_2023_lmc <- readRDS("census_model/Stan/2023/out2023_lmc_omega1.RDS")
o1_2024_lmc <- readRDS("census_model/Stan/2024/out2024_lmc_omega1.RDS")
o1_2024 <- readRDS("census_model/Stan/2024/out2024_omega1_stan.RDS")
