## Misc notes:
# had trouble with recruitment probability, used Gemini for help
# It suggested to make the initial Nsuper larger and make initial R's thinner
# After plotting, it looks like an issue might be that 2023 was an early year --
# All tagged fish had already entered by Sept 15, making probabilities hard to fit
# Shifted up start date to the earliest temperature record: Sept 4

library(data.table)
library(jagsUI)

master_data <- readRDS("census_data_master.RDS")
for (i in seq_along(master_data)) {
  assign(names(master_data)[i], master_data[[i]])
}

#### Define the study period ####
StartDate <- as.POSIXct("2023-08-04 00:00:00", tz = "UTC") # seems to be the earliest temperature
EndDate <- as.POSIXct("2023-10-31 23:59:59", tz = "UTC")
det_2023 <- detections[dateandtimeutc %between% c(StartDate, EndDate)]

K <- as.numeric(as.Date(EndDate) - as.Date(StartDate)) + 1
days <- seq(as.Date(StartDate), as.Date(EndDate), 1)
ntelem <- length(unique(det_2023$transmitter))


#### Import SSS ####
sss_2023 <- sss[year(date) == 2023]

# make survey id
sss_2023[, survey_day := as.numeric(date - as.Date(StartDate) + 1)]
sss_survey_occ <- sss_2023$survey_day
sss_reaches <- sss_2023$reach_no
Ksss = nrow(sss_2023) # number of sss survey days
V = 1 # number of sss passes. Just hardcoded for now

#### Create detection matrix ####
### NEW RULES!! ###
# 1) if in UNR, assign to UNR
# 2) if in UMC, assign to UMC
# 3) If in both UNR and UMC, use the last location

y_mat <- copy(det_2023) |>
  unique(by = c("transmitter", "date", "reach_no")) |>
  _[,
    .(
      reach = fifelse(
        all(c(4, 5) %in% reach_no),
        .SD[which.max(dateandtimeutc)]$reach_no,
        max(reach_no)
      )
    ),
    by = c("transmitter", "date")
  ]
y_mat <- y_mat[
  expand.grid(transmitter = unique(y_mat$transmitter), date = days),
  on = c("transmitter", "date")
]
y_mat[is.na(reach), 'reach'] <- 1
y_mat <- dcast(y_mat, transmitter ~ date, value.var = 'reach')


#### Create tagging indicator ####
# matrix with 0 if th fish wasnt tagged and 1 if it was
# Rows are fish, columns are dates
cap_23 <- ats[
  DateCaptured %between% c(as.Date(StartDate), "2023-12-31"),
  .(
    date = (DateCaptured - 1), # -1 in order to match tagged in the overlap
    transmitter = TransmitterNumber,
    start = as.Date(StartDate)
  )
]


t_mat <- unique(det_2023, by = c("transmitter", "date")) |>
  _[, .(transmitter, date)] |>
  _[
    expand.grid(transmitter = unique(det_2023$transmitter), date = days),
    on = c("transmitter", "date"),
  ] |>
  _[, date2 := date]

setkey(cap_23, transmitter, start, date)
setkey(t_mat, transmitter, date, date2)

telem_indicator <- foverlaps(t_mat, cap_23) |>
  _[, tagged := as.numeric(is.na(start))] |>
  dcast(transmitter ~ i.date, value.var = 'tagged')


#### Import temperature ####
temp <- temp[date %between% c(as.Date(StartDate), as.Date(EndDate))]


#### data augmentation stuff ####
G <- 25 # data augmentation. set this value so that G>>Msuper
nz <- G - ntelem # number of extra individuals
w <- c(rep(1, ntelem), rep(NA, nz)) # marked individuals we know entered the system
yAug <- rbind(y_mat |> as.matrix(rownames = T), matrix(1, nz, K))
TelemIndicatorAug <- rbind(
  as.matrix(telem_indicator, rownames = T),
  matrix(1, nz, K)
)
J = 6 # states (not entered, 4 reaches, exited)


jags.data <- list(
  #SSS data
  sssMat = as.matrix(sss_2023[, 3]), # SSS counts; need as.numeric since this is only 1 col
  Ksss = Ksss, # number of SSS surveyed days
  sssSurveyOcc = sss_survey_occ, # survey dates
  V = V, # number of passes
  sssReaches = sss_reaches, # SSS reaches

  # telemetry data
  K = K,
  G = G,
  w = w,
  y = yAug,
  TelemIndicator = TelemIndicatorAug,
  a = rep(1, 5),
  J = J,
  temp = round(temp$temp, 1)
) # data

params <- c(
  "LambdaSuper",
  "N",
  "NsuperK",
  "Nreach",
  "R",
  "Nsuper",
  "Nmc",
  "Nnan",
  "p_sss",
  "pbar",
  "psi",
  "theta",
  "beta1",
  "piTran",
  "xi",
  "phi",
  "phi0",
  "alpha1",
  "b",
  "eta",
  "Mnan",
  "Mlnr",
  "Mlmc",
  "Mumc",
  "Mmc",
  "M",
  "Msuper",
  "mR",
  "expectedProbabilities",
  "moveProb2",
  "moveProb5",
  "entryProb"
)


## Due to all the interrelated components in this model, we need to find starting values that work
zInits <- matrix(1, G, K)
# minus 1 to account for the row name column (transmitters) in the data.frame
startTelem <- apply(telem_indicator, 1, \(.) min(which(. == 1)) - 1)
lastTelem <- apply(y_mat, 1, \(.) max(which(. > 1)) - 1)

for (i in 1:ntelem) {
  zInits[i, startTelem[i]:lastTelem[i]] <- yAug[i, startTelem[i]:lastTelem[i]] # in the system here
  if (lastTelem[i] < K) {
    zInits[i, (lastTelem[i] + 1):K] <- 6
  } # just have them leave the system to start
  if (1 %in% zInits[i, startTelem[i]:lastTelem[i]]) {
    zInits[i, startTelem[i]:lastTelem[i]][which(
      zInits[i, startTelem[i]:lastTelem[i]] == 1
    )] <- 3
  }
}

zInits[(ntelem + 1):G, ] <- 2

theta <- matrix(NA, 2, 5)
theta[1, ] <- c(NA, 0.40, 0.40, 0.10, 0.10) # can enter reaches 2-5 on day 1
entryProb <- c(0.50, 0.30, 0.10, 0.10) # can only enter reaches 2-3 on days 2:K

piTranInits <- matrix(NA, 5, 5)
piTranInits[2, 2:5] <- c(0.51, 0.22, 0.05, 0.22) #LNR to...
piTranInits[3, 2:5] <- c(0.10, 0.65, 0.10, 0.15) #LMC to...
piTranInits[4, 2:5] <- c(.05, .50, 0.40, 0.05) #UMC to...
piTranInits[5, 2:5] <- #UNR to..
  NsuperKinit <- 200 # start the population somewhere reasonable. Too large is better than too small.

NreachInits <- matrix(NA, K, 6)
# NreachInits[1, 1:5] <- as.vector(rmultinom(
#   1,
#   NsuperKinit,
#   c(.55, .15, .1, .05, 0.15)
# ))
#### GEMINI SUGGESTIONS
# Fill it with small, safe integers (e.g., 2 fish in every state)
# This ensures that for every day k, sum(Nreach[k, 1:6]) is small.
# If NsuperKinit is 500, then sum(Nreach[k, 1:5]) is only 10.
# This gives the sampler 490 "slots" of breathing room to
# move fish around without hitting a negative trial error.
for (k in 1:K) {
  NreachInits[k, 1:5] <- 2
}


# This is different than in 2022 as there are multiple reaches
for (k in 1:length(sss_survey_occ)) {
  NreachInits[sss_survey_occ[k], sss_reaches[k]] <- max(as.matrix(sss_2023[
    k,
    3
  ])) +
    2 # the only big restrictions is that abundance in lmc and unr on sss survey days must be >=max count
}

# Recruitment
# Rinit <- as.vector(rmultinom(1, NsuperKinit, c(.3, .2, rep(.05, K - 2)))) # again, something reasonable
# Rinit[K] <- NA
#### GEMINI SUGGESTIONS
####  Keep cumulative sum of R below NsuperK, don't assign NA to the last
Rinit <- rep(2, K - 1)


inits_list <- list(
  ## total population and shared stuff
  NsuperK = NsuperKinit,
  LambdaSuper = NsuperKinit,
  Nreach = NreachInits,
  R = Rinit,
  phi0 = .995,
  alpha1 = 0,
  beta1 = -0.01,
  theta = theta,
  piTran = piTranInits,

  ## sss parameters
  p_sss = runif(1, .8, .99),

  ## telemetry
  psi = runif(1, .6, .9),
  pbar = c(NA, runif(3, .7, .9), NA, NA),
  z = zInits
)

inits <- function() {
  inits_list
}


# MCMC settings
# nc <- 2
# nAdapt = 50
# nb <- 1c(0.3, 0.15, 0.05, 0.5)
# ni <- 50 + nb
# nt <- 1

nc <- 4
nAdapt <- 35000
nb <- 20000
ni <- 60000 + nb
nt <- 100

# Call JAGS from R
out2023 <- jags(
  jags.data,
  inits,
  params,
  "census_model_2023.jags",
  n.adapt = nAdapt,
  n.chains = nc,
  n.thin = nt,
  n.iter = ni,
  n.burnin = nb,
  parallel = T
)

saveRDS(out2023, "out2023.RDS")
cat("Rhat range:\n", range(out2023$Rhat, na.rm = T))
# out2023 <- readRDS('census_model/out2023.RDS')
