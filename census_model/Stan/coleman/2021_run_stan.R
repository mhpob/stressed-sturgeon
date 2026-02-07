library(cmdstanr)
# compile model (once)
m <- cmdstan_model(stan_file = "census_model_2021.stan")

# load model
# m <- cmdstan_model(
#   stan_file = "census_model_2021.stan",
#   exe_file = "census_model_2021"
# )

#### Initial Values ####
# Initial Value Function
init_fn <- function() {
  list(
    # NsuperK = NsuperKinit,
    # LambdaSuper = NsuperKinit,
    # Nreach = NreachInits,
    # R = Rinit,

    beta1 = -0.01,
    alpha0 = 5.29, # JAGS: alpha0 = logit(phi0), phi0 init = 0.995)
    alpha1 = 0,
    psi = runif(1, 0.6, 0.9),

    # # --- Movement Simplexes ---
    init_probs = c(0.40, 0.40, 0.10, 0.10), # JAGS: theta[1,]
    entry_probs = c(0.50, 0.30, 0.20), #JAGS: entryProb
    # move_probs_LNR uninitialized; JAGS: piTranInits[2,]
    move_probs_LMC = c(0.10, 0.65, 0.05, 0.20), #J AGS: piTranInits[3,]
    move_probs_UMC = c(.5, .5), # JAGS: piTranInits[4,]
    # move_probs_UNR uninitialized; JAGS: piTranInits[5,]

    # # --- Detection ---
    pbar_raw = runif(4, .7, .9), # JAGS: pbar[5] was not initialized, so slight change here
    p_sss = runif(1, .8, .99),

    # # --- Population Size ---
    LambdaSuper = 100
  )
}

fit <- m$sample(
  data = "2021_stan_data.json",
  seed = 20688,
  refresh = 2000,
  init = init_fn,
  parallel_chains = 4,
  iter_warmup = 24000,
  iter_sampling = 1000,
  save_metric = TRUE,
  save_cmdstan_config = TRUE,
  output_dir = "./stan_out"
)

fit$save_object(file = "out2021_stan.RDS")

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
