mean.sj <- 0.3                  # Point estimate of juv. survival
se.sj.e <- 0.03                 # Uncertainty of juv. survival as SE on natural scale
mean.sa <- 0.55                 # Point estimate of ad. survival
se.sa.e <- 0.015                # Uncertainty of ad. survival as SE on natural scale
mean.f1 <- 1.3                  # Point estimate of productivity of 1y females
se.f1.e <- 0.3                  # Uncertainty of 1y productivity as SE on natural scale
mean.fa <- 1.8                  # Point estimate of productivity of ad. females
se.fa.e <- 0.1                  # Uncertainty of ad. productivity as SE on natural scale


nsim <- 100000
lambda <- R0 <- GT <- numeric(nsim)
stable.stage <- matrix(NA, ncol=2, nrow=nsim)
sensitivity <- matrix(NA, ncol=4, nrow=nsim)





for (s in 1:nsim){
  if(s %% 1000 == 0) {cat(paste("*** Simrep", s, "***\n"))}   # Counter
  
  # Transition matrix
  A <- matrix(c(sj.sim[s] * f1.sim[s], sj.sim[s] * fa.sim[s], sa.sim[s], sa.sim[s]),
              ncol=2, byrow=TRUE)
  eigenA <- eigen(A)
  
  # Asymptotic population growth rate
  lambda[s] <- max(Re(eigenA$values))
  
  # Stable stage distribution
  u <- which.max(Re(eigenA$values))
  revec <- Re(eigenA$vectors[,u])
  stable.stage[s,] <- revec / sum(revec)
  
  # Lower level sensitivities
  levec <- Re(solve(eigenA$vectors)[u,])
  senmat <- levec %*% t(revec)
  derivmat <- matrix(c(f1.sim[s], fa.sim[s], 0, 0), ncol=2, byrow=TRUE)
  sensitivity[s,1] <- sum(senmat * derivmat)
  derivmat <- matrix(c(sj.sim[s], 0, 0, 0), ncol=2, byrow=TRUE)
  sensitivity[s,2] <- sum(senmat * derivmat)
  derivmat <- matrix(c(0, sj.sim[s], 0, 0), ncol=2, byrow=TRUE)
  sensitivity[s,3] <- sum(senmat * derivmat)
  derivmat <- matrix(c(0, 0, 1, 1), ncol=2, byrow=TRUE)
  sensitivity[s,4] <- sum(senmat * derivmat)
  
  # Net reproductive rate
  i <- 1:100
  R0[s] <- sj.sim[s] * f1.sim[s] + sj.sim[s] * fa.sim[s] * sum(sa.sim[s]^i)
  
  # Generation time
  GT[s] <- log(R0[s]) / log(lambda[s])
}


# make substep nimblefunctions?

r_Re <- nimbleRcall(prototype = function(z = double(1)){},
                    Rfun = 'Re',
                    returnType = double(1))
mod_code <- nimbleCode({
  
  sj ~ dbeta(shape1 = 5, shape2 = 10)
  sa ~ dbeta(shape1 = 10, shape2 = 10)
  f1 ~ dnorm(mean = 1.3, sd = 0.3)
  fa ~ dnorm(mean = 1.8, sd = 0.1)
  
  # Transition matrix
  A[1,1] <- sj * f1
  A[1,2] <- sj * fa
  A[2,1] <- sa
  A[2,2] <- sa
  
  e_val_real[1:4] <- r_Re(eigen(A[1:2,1:2])$values)
  lambda <- max(e_val_real[1:4])
  
  # # Stable stage distribution
  u <- which(e_val_real[1:4] == max(e_val_real[1:4]))
  # revec <- Re(eigenA$vectors[,u])
  # stable.stage[s,] <- revec / sum(revec)
  
})
mod <- nimbleModel(
  code = mod_code
)
init_vals <- function() {list(sj = rbeta(1, shape1 = 5, shape2 = 10),
                            sa = rbeta(1, shape1 = 5, shape2 = 10),
                            f1 = rnorm(1, mean = 1.3, sd = 0.3),
                            fa = rnorm(1, mean = 1.8, sd = 0.1))}
parameters <- c('lambda', 'u')
out1 <- nimbleMCMC(code = mod,
                   monitors = parameters,
                   inits = init_vals(),
                   niter = 1000,
                   nchains = 3,
                   nburnin = 900)









# Schaub & Kéry (2022) Integrated Population Models
# Chapter 3 : Introduction to stage-structured population models
# ----------------------------------------------------

# library(IPMbook) ; library(jagsUI)

# 3.4 Analysis of matrix population models with Markov Chain
#     Monte Carlo (MCMC) software
# ==========================================================

# 3.4.1 Analysis of a matrix population model without stochasticity
#       and parameter uncertainty
# ------------------------------------------------------------------

# Bundle data
sj <- 0.3
sa <- 0.55
f1 <- 1.3
fa <- 1.8
test.data <- list(sj=sj, sa=sa, f1=f1, fa=fa, time=5)

library(nimble)
mod_code <- nimbleCode(
  {

    N[1,1] <- 1
    N[2,1] <- 1
    
    # Loop over time
    for (t in 1:time){
      # Population projection
      N[1, t+1] <- sj * (f1 * N[1,t] + fa * N[2,t])
      N[2, t+1] <- sa * (N[1,t] + N[2,t])
      # Calculation of population quantities
      # Annual (realized) population growth rate
      ann.growth.rate[t] <- (N[1,t+1] + N[2,t+1]) / (N[1,t] + N[2,t])
    }
    lambda <- ann.growth.rate[time] # gr in final interval is our estimate of lambda
  }
)
mod <- nimbleModel(
  code = mod_code,
  constants = list(sj= 0.3, sa=0.55, f1=1.3, fa=1.8, time=5)
)



# model {
#   # Model for initial state
#   N[1,1] <- 1
#   N[2,1] <- 1
# 
#   # Loop over time
#   for (t in 1:T){
#     # Population projection
#     N[1,t+1] <- sj * (f1 * N[1,t] + fa * N[2,t])
#     N[2,t+1] <- sa * (N[1,t] + N[2,t])
#     # Calculation of population quantities
#     # Annual (realized) population growth rate
#     ann.growth.rate[t] <- (N[1,t+1] + N[2,t+1]) / (N[1,t] + N[2,t])
#   }
#   lambda <- ann.growth.rate[T] # gr in final interval is our estimate of lambda
# }
# )

# Parameters monitored
parameters <- c("ann.growth.rate", "lambda", "N")

# MCMC settings
ni <- 1; nt <- 1; nb <- 0; nc <- 1; na <- 0

# Call JAGS (ART <1 min) and summarize results
# out1 <- jags(jags.data, NULL, parameters, "model1.txt", n.adapt=na, n.chains=nc, n.thin=nt,
#              n.iter=ni, n.burnin=nb, DIC=FALSE)

out1 <- nimbleMCMC(code = mod,
                   data = test.data,
                   # inits = list(chain1=list(0.3)),
                   # dimensions = list(ann.growth.rate = 5),
                   monitors = parameters,
                   niter = 1,
                   nchains = 1,
                   nburnin = 0)

print(out1, 3)
#                     mean sd  2.5%   50% 97.5% overlap0 f
# ann.growth.rate[1] 1.015 NA 1.015 1.015 1.015    FALSE 1
# ann.growth.rate[2] 1.021 NA 1.021 1.021 1.021    FALSE 1
# ann.growth.rate[3] 1.021 NA 1.021 1.021 1.021    FALSE 1
# ann.growth.rate[4] 1.021 NA 1.021 1.021 1.021    FALSE 1
# ann.growth.rate[5] 1.021 NA 1.021 1.021 1.021    FALSE 1
# lambda             1.021 NA 1.021 1.021 1.021    FALSE 1
# N[1,1]             1.000 NA 1.000 1.000 1.000    FALSE 1
# N[2,1]             1.000 NA 1.000 1.000 1.000    FALSE 1
# N[1,2]             0.930 NA 0.930 0.930 0.930    FALSE 1
# N[2,2]             1.100 NA 1.100 1.100 1.100    FALSE 1
# [ ...output truncated... ]
# N[1,6]             1.017 NA 1.017 1.017 1.017    FALSE 1
# N[2,6]             1.188 NA 1.188 1.188 1.188    FALSE 1


# Write JAGS model file
cat(file="model2.txt", "
model {
  # Model for initial state
  N[1,1] <- 1
  N[2,1] <- 1

  # Loop over time
  for (t in 1:T){
    # Population projection
    N[1,t+1] <- sj * (f1 * N[1,t] + fa * N[2,t])
    N[2,t+1] <- sa * (N[1,t] + N[2,t])

    # Calculation of population quantities
    # Annual (realized) population growth rate
    ann.growth.rate[t] <- (N[1,t+1] + N[2,t+1]) / (N[1,t] + N[2,t])

    # Scaled annual stage distributions
    stage.distr[1,t] <- N[1,t+1] / (N[1,t+1] + N[2,t+1])
    stage.distr[2,t] <- N[2,t+1] / (N[1,t+1] + N[2,t+1])
  }
  lambda <- ann.growth.rate[T]
  stable.stage.distr <- stage.distr[,T]

  # Sensitivity and elasticity of lambda to changes in sj
  delta <- 0.001                                              # Size of perturbation
  N.star[1,1] <- 1
  N.star[2,1] <- 1
  for (t in 1:T){
    N.star[1,t+1] <- (sj + delta) * (f1 * N.star[1,t] + fa * N.star[2,t])
    N.star[2,t+1] <- sa * (N.star[1,t] + N.star[2,t])
    ann.growth.rate.star[t] <- (N.star[1,t+1] + N.star[2,t+1]) / (N.star[1,t] + N.star[2,t])
  }
  s.sj <- (ann.growth.rate.star[T] - ann.growth.rate[T]) / delta
  e.sj <- s.sj * sj / lambda

  # Calculation of net reproductive rate (R0)
  for (i in 1:100){
    u[i] <- pow(sa, i)
  }
  R0 <- sj * f1 + sj * fa * sum(u[])

  # Calculation of generation time (GT)
  GT <- log(R0) / log(lambda)
}
")

# Parameters monitored
parameters <- c("lambda", "stable.stage.distr", "s.sj", "e.sj", "R0", "GT")

# MCMC settings
ni <- 1; nt <- 1; nb <- 0; nc <- 1; na <- 0

# Call JAGS (ART <1 min) and summarize results
out2 <- jags(jags.data, NULL, parameters, "model2.txt", n.adapt=na, n.chains=nc, n.thin=nt,
             n.iter=ni, n.burnin=nb, DIC=FALSE)
print(out2, 3)

#                        mean sd  2.5%   50% 97.5% overlap0 f
# lambda                1.021 NA 1.021 1.021 1.021    FALSE 1
# stable.stage.distr[1] 0.461 NA 0.461 0.461 0.461    FALSE 1
# stable.stage.distr[2] 0.539 NA 0.539 0.539 0.539    FALSE 1
# s.sj                  1.454 NA 1.454 1.454 1.454    FALSE 1
# e.sj                  0.427 NA 0.427 0.427 0.427    FALSE 1
# R0                    1.050 NA 1.050 1.050 1.050    FALSE 1
# GT                    2.368 NA 2.368 2.368 2.368    FALSE 1
