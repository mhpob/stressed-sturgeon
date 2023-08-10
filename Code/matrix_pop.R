# library(popbio)
library(Matrix)

### matrix-making functions
create_pop_mat <- function(vital_rates = NULL){
  if(is.null(vital_rates)){
    vital_rates <- list(
      lifespan = floor(rnorm(1, 42, 5)),
      maturity = floor(msm::rtnorm(1, 10, 2, 12)),
      surv_1p = rbeta(1, 20, 2),
      spawn_date = floor(rnorm(1, 243, 10)),
      spawn_interval = runif(1, 1, 4),
      fec_intercept = -1304704,
      fec_slope = 111909
    )
  }
  
  row1_fun <- function(age, spawn_int, spawn_date, maturity, surv_1p,
                       fec_intercept, fec_slope){
    # This is calculated from the fecundity function (Table 1),
    # divided by spawning interval (e.g., three years for shortnose sturgeon)...
    
    fecund_func <- function(age, maturity, fec_intercept, fec_slope){
      if(age < maturity){
        0
      }else{
        fec_intercept + fec_slope * age
      }
    }
    step1 <- fecund_func(age, maturity, fec_intercept, fec_slope) / spawn_int
    
    # discounted by the mortality of females between the 1 January census and the
    # spawning date...
    discount <- function(spawn_date, surv_1p){
      if(is.character(spawn_date)){
        n_days <- as.numeric(
          difftime(
            as.Date(spawn_date),
            as.Date('2023-01-01'),
            units = 'days'
          )
        )
      }else{
        n_days <- spawn_date - 1
      }
      
      pct_yr <- n_days / 365
      
      exp(log(surv_1p) * pct_yr)
    } 
    
    step2 <- step1 * discount(spawn_date, surv_1p)
    
    # and then divided by two to count only those eggs that can mature into females.
    step2 / 2
  }
  
  # Make population matrix; sparse, so the below define where non-zero values are
  pop <- sparseMatrix(
    i = c(
      # Egg production in Row 1 in cols from maturity to death
      rep(1, times = (vital_rates$lifespan - vital_rates$maturity) + 1),
      # survivorship in off diagonal
      2:(vital_rates$lifespan + 1)
    ),
    j = c(
      # Egg production in Row 1 in cols from maturity to death
      (vital_rates$maturity + 1):(vital_rates$lifespan + 1),
      # survival rate in off diagonal
      1:vital_rates$lifespan
    ),
    
    x = c(
      # fill in egg production
      sapply(vital_rates$maturity:vital_rates$lifespan,
             row1_fun,
             vital_rates$spawn_int,
             vital_rates$spawn_date,
             vital_rates$maturity,
             vital_rates$surv_1p,
             vital_rates$fec_intercept,
             vital_rates$fec_slope),
      # and survival rate
      rep(vital_rates$surv_1p, vital_rates$lifespan)
    ),
    # name the axes
    dimnames = list(0:vital_rates$lifespan, 0:vital_rates$lifespan))
  
  
  ## solve for lambda = 1
  obj_fun <- function(x, popmat, lamb = 1){
    # det(A - lambda*I) = 0
    # Find value for YOY survival that makes this the case
    popmat[2, 1] <- x
    norm(det(popmat - lamb * Diagonal(nrow(popmat))), '2')
  }
  
  res <- optimize(obj_fun, interval = c(0, 1), tol = 1e-10,
                  popmat = pop)
  
  pop[2, 1] <- res$minimum
  
  vital_rates$surv_yoy <- res$minimum
  
  list(vital_rates = vital_rates,
       pop_mat = pop)
}

elasticity <- function(pop_mat, zero = FALSE){
  ## hacking popbio sensitivity and elasticity
  ev <- eigen(pop_mat)
  lmax <- which.max(Re(ev$values))
  W <- ev$vectors
  w <- abs(Re(W[, lmax]))
  V <- try(Conj(solve(W)), silent = TRUE)
  if ('try-error' %in% class(V)) {
    message("Warning: matrix is singular")
    s <- pop_mat * NA
  }else {
    v <- abs(Re(V[lmax, ]))
    s <- v %o% w
    # if (zero) {
    #   s[pop_mat == 0] <- 0
    # }
    dimnames(s) <- dimnames(pop_mat)
  }
  
  elas <- s * pop_mat/popbio::lambda(pop_mat)
  elas
}


### possible priors
## lifespan N(45, 5)
hist(rnorm(1000, 45, 5))
## maturity; tN(10, 2, lb=12)
hist(msm::rtnorm(1000, 10, 2, 12))
## survival Beta(8,2)
hist(rbeta(1000, 20, 2))
## spawn day N(243, 10)
hist(rnorm(1000, 243, 10))
## spawn interval U(1, 4)
hist(runif(1000, 1, 4))


library(future.apply)
plan(multisession, workers = availableCores(logical = F) - 1)
pop_reps <- future_replicate(1000, create_pop_mat(), simplify = F)
pop_df <- do.call(rbind, 
                 lapply(pop_reps, function(.) data.frame(.$vital_rates))
)
pop_fec <- data.table::rbindlist(
                 lapply(pop_reps, function(.){
                   data.frame(age = as.numeric(colnames(.$pop_mat)),
                              fec = .$pop_mat[1,],
                              .$vital_rates)
                 }),
                 id = 'run'
)

pop_elas <- lapply(pop_reps, function(.) elasticity(.$pop_mat))
pop_elas <- lapply(pop_elas, function(.) data.frame(summary(.)))
pop_elas <- lapply(pop_elas, function(.) {
  data.frame(age = c(.[.$i == 1, 'j'],
                     .[.$i != 1, 'j']),
             type = c(rep('fecundity', length(.[.$i == 1, 'j'])),
                      rep('survival', length(.[.$i != 1, 'j']))),
             value = c(.[.$i == 1, 'x'],
                       .[.$i != 1, 'x'])
  )
}
)
pop_elas <- data.table::rbindlist(pop_elas, id = 'run')

elas_summ <- pop_elas[, .(lci = quantile(value, 0.025),
                          med = quantile(value, 0.5),
                          uci = quantile(value, 0.975)), by = c('age', 'type')]

library(ggplot2)
ggplot(data = pop_df) +
  geom_point(aes(x = spawn_date, y = surv_yoy))

ggplot(data = pop_df) +
  geom_boxplot(aes(y = surv_yoy)) +
  coord_cartesian(ylim = c(0, 8e-6)) +
  labs(y = 'YOY Survival') +
  geom_hline(yintercept = 7.56e-7, col = 'red') +
  geom_hline(yintercept = c(7.41e-7, 7.64e-7), col = 'red', linetype = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 45),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


ggplot(data = pop_fec) +
  geom_line(aes(x = age, y = fec/1000000, group = run,
                color = spawn_interval), alpha = 0.1) +
  scale_color_viridis_c() +
  labs(x = 'Age', y = 'Female eggs per female (millions)', color = 'Years between\nspawning') +
  theme_bw() +
  theme(legend.position = c(0.15, 0.6))



ggplot() +
  geom_pointrange(data = elas_summ, aes(x = age-1, y = med * 100,
                                        ymin = lci*100, ymax = uci*100,
                               color = type), size = 0.05) +
  geom_line(data = pop_elas, aes(x = age-1, y = value * 100, group = type),
            linetype = 'dashed') +
  labs(x = 'Age', y = 'Elasticity (%)', color = NULL) +
  # scale_fill_manual(values = c('white', 'black')) +
  # scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  # scale_x_continuous(limits = c(-0.5, 60), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8))




## Gross et al 2002 rates Atl. sturg
vital_rates <- list(
  lifespan = 60,
  maturity = 16,
  surv_1p = 0.93,
  spawn_date = '2023-06-15',
  spawn_interval = 4.5,
  fec_intercept = -1304704,
  fec_slope = 111909
)

pop <- create_pop_mat(vital_rates)
elas <- elasticity(pop$pop_mat, zero = T)


elas_df <- data.frame(summary(elas))
elas_df <- data.frame(age = c(elas_df[elas_df$i == 1, 'j'],
                              elas_df[elas_df$i != 1, 'j']),
                      type = c(rep('fecundity', length(elas_df[elas_df$i == 1, 'j'])),
                               rep('survival', length(elas_df[elas_df$i != 1, 'j']))),
                      value = c(elas_df[elas_df$i == 1, 'x'],
                                elas_df[elas_df$i != 1, 'x'])
)

ggplot() +
  geom_col(data = elas_df, aes(x = age-1, y = value * 100,
                               fill = factor(type, ordered = T,
                                             levels = c('survival', 'fecundity'))),
           color = 'black',
           position = position_dodge(preserve = 'single')) +
  labs(x = 'Age', y = 'Elasticity (%)', fill = NULL) +
  scale_fill_manual(values = c('white', 'black')) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-0.5, 60), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8))



## Gross et al shortnose sturg
vital_rates <- list(
  lifespan = 37,
  maturity = 8,
  surv_1p = 0.865,
  spawn_date = '2023-05-01',
  spawn_interval = 3,
  fec_intercept = 4091,
  fec_slope = 2864
)

pop <- create_pop_mat(vital_rates)
elas <- elasticity(pop, zero = T)

elas_df <- data.frame(
  age = rep(0:(dim(pop)[1]-1), 2),
  type = rep(c('survival', 'fecundity'), each = dim(pop)[1]),
  value = c(
    c(elas[row(elas) == (col(elas) + 1)], 0),
    elas[1,]
  )
)

ggplot() +
  geom_col(data = elas_df, aes(x = age, y = value * 100,
                               fill = type),
           position = position_dodge()) +
  labs(y = 'Elasticity (%)')




