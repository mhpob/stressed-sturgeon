library(data.table)
library(jagsUI)

master_data <- readRDS("census_data_master.RDS")
for (i in seq_along(master_data)) {
  assign(names(master_data)[i], master_data[[i]])
}

#### Define the study period ####
StartDate <- as.POSIXct("2022-08-15 00:00:00", tz = "UTC")
EndDate <- as.POSIXct("2022-10-31 23:59:59", tz = "UTC")
det_2022 <- detections[dateandtimeutc %between% c(StartDate, EndDate)]

K <- as.numeric(as.Date(EndDate) - as.Date(StartDate)) + 1
days <- seq(as.Date(StartDate), as.Date(EndDate), 1)
ntelem <- length(unique(det_2022$transmitter))

#### Import SSS ####
sss_2022 <- sss[year(date) == 2022]

# make survey id
sss_2022[, survey_day := as.numeric(date - as.Date(StartDate) + 1)]
sss_survey_occ <- sss_2022$survey_day
sss_reaches <- sss_2022$reach_no
Ksss = nrow(sss_2022) # number of sss survey days
V = length(grep("\\d", names(sss_2022[`2` != 0]))) # number of sss passes


#### Create detection matrix ####
### NEW RULES!! ###
# 1) if in UNR, assign to UNR
# 2) if in UMC, assign to UMC
# 3) If in both UNR and UMC, use the last location
# Check with using this code:
# unique(y_mat, by = c("transmitter", "date", "reach_no")) |>
#   _[, .SD[all(c(4, 5) %in% reach_no)], by = c("transmitter", "date")] |>
#   (\(.) {
#     nrow(.) != 0
#   })() |>
#   `if`(cond = _, expr = stop("Clash in rules of detection matrix!"))

y_mat <- copy(det_2022) |>
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
cap_22 <- ats[
  DateCaptured %between% c(as.Date(StartDate), "2022-12-31"),
  .(
    date = (DateCaptured - 1), # -1 in order to match tagged in the overlap
    transmitter = TransmitterNumber,
    start = as.Date(StartDate)
  )
]

t_mat <- unique(det_2022, by = c("transmitter", "date")) |>
  _[, .(transmitter, date)] |>
  _[
    expand.grid(transmitter = unique(det_2022$transmitter), date = days),
    on = c("transmitter", "date"),
  ] |>
  _[, date2 := date]

setkey(cap_22, transmitter, start, date)
setkey(t_mat, transmitter, date, date2)

telem_indicator <- foverlaps(t_mat, cap_22) |>
  _[, tagged := as.numeric(is.na(start))] |>
  dcast(transmitter ~ i.date, value.var = 'tagged')

#### Import temperature ####
# vr <- fread("VR2AR_546463_20221213_1.csv-fathom-split/TEMP.csv")
# exo <- readxl::read_excel(
#   "Data/MISC/8.29.22-11.29.22 Marshyhope EXO.xlsx",
#   sheet = 2
# )
# ggplot2::ggplot() +
#   ggplot2::geom_line(
#     data = vr,
#     ggplot2::aes(x = `Device Time (UTC)`, y = `Ambient (deg C)`)
#   ) +
#   ggplot2::geom_line(
#     data = exo,
#     ggplot2::aes(x = `Date (MM/DD/YYYY)`, y = `Temp °C`)
#   )

# Using a VR2AR over the sonde -- will need to explain this (sonde was deployed after start date)

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
  sssMat = as.matrix(sss_2022[, 3:4]), # SSS counts. 2022 IS SPECIAL! (2 passes)
  Ksss = Ksss, # number of SSS surveyed days
  sssSurveyOcc = sss_survey_occ, # survey dates
  V = V, # number of passes

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

# I'm getting yelled at for z values. fudge initial values for now
#   Issue seems to be that things are breaking the rules by skipping LMC
# zInits[6:7, 36] <- 3
# zInits[13, 39] <- 3
# yAug[6:7, 36] <- 3
# yAug[13, 39] <- 3

theta <- matrix(NA, 2, 5)
theta[1, ] <- c(NA, 0.40, 0.40, 0.10, 0.10) # can enter reaches 2-5 on day 1
entryProb <- c(0.50, 0.30, 0.10, 0.10) # can only enter reaches 2-3 on days 2:K

piTranInits <- matrix(NA, 5, 5)
piTranInits[2, 2:5] <- c(0.51, 0.22, 0.05, 0.22) #LNR to...
piTranInits[3, 2:5] <- c(0.10, 0.65, 0.10, 0.15) #LMC to...
piTranInits[4, 2:5] <- c(.05, .50, 0.40, 0.05) #UMC to...
piTranInits[5, 2:5] <- c(0.3, 0.15, 0.05, 0.5) #UNR to..

NsuperKinit <- 100 # start the population somewhere reasonable. Too large is better than too small.

NreachInits <- matrix(NA, K, 6)
NreachInits[1, 1:5] <- as.vector(rmultinom(
  1,
  NsuperKinit,
  c(.55, .15, .1, .05, 0.15)
))

for (k in 1:length(sss_survey_occ)) {
  NreachInits[sss_survey_occ[k], 3] <- max(sss_2022[k, 3:4]) + 2 # the only big restrictions is that abundance in lmc on sss survey days must be >=max count
}

# Recruitment
Rinit <- as.vector(rmultinom(1, NsuperKinit, c(.3, .2, rep(.05, K - 2)))) # again, something reasonable
Rinit[K] <- NA


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
# nb <- 1
# ni <- 50 + nb
# nt <- 1

nc <- 4
nAdapt <- 35000
nb <- 30000
ni <- 60000 + nb
nt <- 100

# Call JAGS from R
out2022 <- jags(
  jags.data,
  inits,
  params,
  "census_model_2022.jags",
  n.adapt = nAdapt,
  n.chains = nc,
  n.thin = nt,
  n.iter = ni,
  n.burnin = nb,
  parallel = T
)

saveRDS(out2022, "out2022.RDS")
cat(
  "Rhat range:\n",
  range(out2022$Rhat, na.rm = T)
)
# out2022 <- readRDS('census_model/out2022.RDS')
