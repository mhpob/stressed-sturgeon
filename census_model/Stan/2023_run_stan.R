library(cmdstanr)
# compile model (once)
# m <- cmdstan_model(stan_file = "census_model/Stan/census_model.stan")

# load model
m <- cmdstan_model(
  stan_file = "census_model.stan",
  exe_file = "census_model"
)

#### Initial Values ####
# Initial Value Function
init_fn <- function() {
  add_jitter <- function(target, scale = 20) {
    # 1. Multiply target by scale to get Dirichlet alphas
    alphas <- target * scale

    # 2. Draw from Gamma distribution (the building block of Dirichlet)
    #    Add a tiny epsilon (0.01) to alphas to ensure no zero-crashes
    raw_draws <- rgamma(length(target), shape = alphas + 0.01, rate = 1)

    # 3. Normalize to sum to 1
    raw_draws / sum(raw_draws)
  }

  list(
    # --- Survival & Recruitment ---
    # Prior: normal(-0.1, 0.05)
    # beta1 = runif(1, -0.15, -0.02),
    beta1 = -0.01, # from JAGS code

    # # Prior: normal(0, 1.5)
    # # alpha0 = runif(1, -0.5, 0.5),
    alpha0 = 0.5, #from JAGS code

    # # Prior: normal(0.2, 0.15)
    alpha1 = 0.2, #from JAGS code

    # # Prior: beta(1, 1) -> Start neutral
    psi = runif(1, 0.6, 0.9),

    # # --- Movement Simplexes ---
    # # Flat priors (1,1,1,1)
    # # theta[1, 2:5] in Coleman et al. 2024
    init_probs = add_jitter(c(0.40, 0.40, 0.10, 0.10)),
    # init_probs = c(NULL, 0.40, 0.40, 0.10, 0.10),
    # # # theta[2, 2:5] in Coleman et al. 2024
    entry_probs = add_jitter(c(0.50, 0.30, 0.10, 0.10)),
    # entry_probs = c(0.50, 0.30, 0.10, 0.10),
    # # # pi in Coleman et al. 2024
    move_probs_LNR = add_jitter(c(0.51, 0.22, 0.05, 0.22)),
    # move_probs_LNR = c(0.51, 0.22, 0.05, 0.22),
    move_probs_LMC = add_jitter(c(0.10, 0.65, 0.10, 0.15)),
    # move_probs_LMC = c(0.10, 0.65, 0.10, 0.15),
    move_probs_UMC = add_jitter(c(.05, .50, 0.40, 0.05)),
    # move_probs_UMC = c(.05, .50, 0.40, 0.05),
    move_probs_UNR = add_jitter(c(0.3, 0.15, 0.05, 0.5)),
    # move_probs_UNR = c(0.3, 0.15, 0.05, 0.5),

    # # --- Detection ---
    # # Prior: beta(5, 1.5) -> Mean 0.77
    pbar_raw = runif(4, .7, .9),

    # # Prior: beta(10, 3) -> Mean 0.83
    # # p_sss = runif(1, 0.75, 0.99),
    p_sss = runif(1, .8, .99),

    # # --- Population Size ---
    # # CRITICAL: Must be > n_telem.
    # # LambdaSuper = runif(1, 100, 200)
    LambdaSuper = 100
  )
}

m$sample(
  data = "2023_stan_data.json",
  seed = 20688,
  refresh = 2000,
  init = init_fn,
  parallel_chains = 4,
  iter_warmup = 49000,
  iter_sampling = 1000,
  save_metric = TRUE,
  save_cmdstan_config = TRUE,
  output_dir = "./stan_out"
)

fit$save_object(file = "out2023_stan.RDS")

## To save cmdstan console output for debugging:
# con <- file("stan_debug_log.txt", open = "wt")
# sink(con, type = "output")
# sink(con, type = "message")
#
# m$sample()
#
# sink(type = "output")
# sink(type = "message")
# close(con)
