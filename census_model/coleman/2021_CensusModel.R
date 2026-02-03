#2021 Model
rm(list = ls())
gc()
library(jagsUI)
library(mgcv)
# library(lubridate)
# library(ggplot2)
# library(dplyr)
library(data.table)

## Define the study period
StartDate <- as.POSIXct("2021-08-15")
EndDate <- as.POSIXct("2021-10-31")
K <- as.numeric(difftime(EndDate, StartDate, units = "days")) + 1
days <- seq(as.Date(StartDate), as.Date(EndDate), 1)


###----------------------------------------###
## load array detections
# setwd("Q:\\.shortcut-targets-by-id\\1NBRbAnU7UmBUkXwlN8CtbZdBRNrUNJrh\\2020 Sturgeon Census Model")
dat <- data.table::fread("Data/Detections/old/sturgeon_detections.gz")
#unique(dat$transmitter)
unique(dat$station)

# subset to 2021 data
dat2021 <- dat[year(date.local) == "2021", ]

# make key (MOB)
# k <- unique(dat2021, by = c('station')) |>
#   _[, c('station', 'long', 'lat')] |>
#   sf::st_as_sf(coords = c('long', 'lat'), crs = 4326)

river_segments <- fread('census_model/2021_receiver_key.csv')
river_segments[,
  river := fcase(
    river == 'LNR' , 2 ,
    river == 'UNR' , 5 ,
    river == 'LMC' , 3 ,
    river == 'UMC' , 4
  )
]
dat2021 <- dat2021[river_segments, , on = 'station']

# transciever locations in 2021
#locs2021<- read.csv("2021_receivers.csv")
##locs <- uniquecombs(dat2020[,c("lat","long","station")],ordered=FALSE)
##locs$River<-c(3,3,2,2,2,3,3,3,3,4,4,4,2,2,2,2,3,3,3,3,3,3,3,4,4,2,2,4,4) # assign each location to a river segment

# add in 21909 and combine with dat
#fish21909 <- read.csv("mbalazik_21909.csv")
#fish21909$datetime<- mdy_hms(fish21909$datetime,tz=Sys.timezone())
#fish21909$date.utc <- fish21909$datetime
#fish21909merge <- fish21909[,c("date.utc", "datetime", "transmitter", "Station", "receiver","latitude","longitude")]

# merge back into dat
#dat2020<- rbind(dat2020, fish21909merge, use.names=FALSE)

# convert to dataframe to play nice with later analyses
# dat2021<- data.frame(dat2021)
dat2021 <- dat2021[transmitter != 'A69-9001-21060', ] #Removing fish that died
dat2021 <- dat2021[transmitter != 'A69-9001-10158', ] #Removing fish that came into system before August 15 and was not detected again

# ID'd fish in 2020
ID2021 <- unique(dat2021$transmitter)
ntelem <- length(ID2021)
# assign detections to a river segement
# dat2021$reach <- locs2021$reach[match(dat2021$station, locs2021$station)]
setnames(dat2021, 'river', 'reach')

###----------------------------------------###
### load VPS detections
VPSdat <- fread("census_model/2021_VPS_Detections.csv")
VPSdat$FishTag <- VPSdat$FullId %in% ID2021
VPSdat <- VPSdat[VPSdat$FishTag == "TRUE", ] # reduced to actual fish tags (what's up with 21909?)
VPSdat$DATETIME <- lubridate::ymd_hms(VPSdat$Time, tz = Sys.timezone())
VPSid <- unique(VPSdat$FullId)
match(unique(VPSdat$FullId), ID2021) # all VPS tags should have a match in array data


# looking okay so far
# plot(locs2020[,c("long","lat")], col=locs2020$River, cex=1)
# points(VPSdat[,c("LON","LAT")], col=rgb(0,0,1,.1), pch=16,cex=.75)

# # transcievers
# plot(locs2020[,c("long","lat")], col=locs2020$River, pch=16,cex=1)
# text(locs2020[,c("long","lat")], labels=locs2020$station)

# plot the detections of any individual
# plot(locs2021[,c("long","lat")], col="red", cex=1, main="A69-9001-18980")
# points(dat2021[dat2021$transmitter=="A69-9001-18980", c("long","lat")], col="blue", pch=16,
#        cex=table(dat2021[dat2021$transmitter=="A69-9001-18980", c("station")])/max(table(dat2021[dat2021$transmitter=="A69-9001-18980", c("station")]))*2)
#
# #transmitter detections per reciever
# t(table(dat2021[,"transmitter"], dat2021[,"station"]))

# if(1==0){
#   # plot the detections of any individual
#   plot(locs2021[,c("long","lat")], col=locs2021$reach, cex=1)
#   i = 7
#   tmp <- dat2021[dat2021$transmitter==ID2021[i], ]
#   tmp <- tmp[order(tmp$date.local), ]
#   for(k in 1:dim(tmp)[1]){
#     #points(tmp [k, c("long","lat")], col=rgb(1,0,1,.25), pch=16,cex=1.)
#     plot(locs2021[,c("long","lat")], col=locs2021$River, cex=1, main=tmp$date.local[k])
#     points(tmp [k, c("long","lat")], col=rgb(1,0,1,1), pch=16,cex=2)
#     Sys.sleep(.00001)
#   }
# }

###----------------------------------------###
## create encounter history matrix by day
# ntelem is the number of transmitters; K is number of days
y <- matrix(1, ntelem, K) # 1 is not detected

dat2021[, date := as.Date(date.local)] #MOB
VPSdat[, date := as.Date(Time)] #MOB

# For each fish
for (i in 1:ntelem) {
  tmp <- dat2021[dat2021$transmitter == ID2021[i], ]

  # for each day
  for (k in 1:K) {
    tmp_date <- tmp[date == days[k]]

    # If there are detections
    if (length(tmp_date$reach) > 0) {
      # LMC if was detected in LMC, else max reach
      y[i, k] <- ifelse(3 %in% tmp_date$reach, 3, max(tmp_date$reach))
    }
  }

  ## add in vps detections
  # if the fish was detected in the VPS, it mus be assigned LMC (river section 3)
  if (ID2021[i] %in% VPSid) {
    tmpVPS <- VPSdat[FullId == ID2021[i], ]

    # For each day detected in the VPS assign LMC
    for (k in 1:K) {
      if (length(tmpVPS[date == days[k]]$reach) > 0) {
        y[i, k] <- 3 ## if detected in VPS array it must be in river section 3
      }
    }
  }
}

plot(y[1, ])

## date of first marking
SturgeonCapDat <- fread("census_model/sturgeon_capture_data.csv")
firstTelem <- as.Date(x = integer(0), origin = "1970-01-01")
startTelem <- lastTelem <- NA

## for each fish
## (minus 1 as 21909 has a different history (tagged in another system))
for (i in 1:(ntelem - 1)) {
  firstTelem[i] <- SturgeonCapDat[
    TransmitterNumber == ID2021[i],
    as.Date(DateCaptured, format = '%m/%d/%Y')
  ]
  startTelem[i] <- ifelse(
    firstTelem[i] < StartDate,
    1,
    as.numeric(
      difftime(firstTelem[i], as.Date(StartDate), units = "days")
    ) +
      1
  )
  lastTelem[i] <- max(which(y[i, ] > 1)) # last occasion an individual was seen
}
startTelem[ntelem] <- 1
lastTelem[ntelem] <- max(which(y[ntelem, ] > 1))


# create indicator variable for when an individual has an active tag
TelemIndicator <- matrix(0, ntelem, K)
for (i in 1:ntelem) {
  TelemIndicator[i, startTelem[i]:K] <- 1 # days where an individual could be detected (1) or not (0)
} #i

###----------------------------------------###
### Load 2021 SSS data
sssDat <- fread("census_model/2021_SSS_Total.csv")
sssDat$Date.Collected <- as.Date(sssDat$SonarDateTime, format = "%m/%d/%Y")
sssDates <- cbind(unique(sssDat$Date.Collected), 1:5)
sssDat$SurveyID <- as.numeric(
  sssDates[, 2][match(as.numeric(sssDat$Date.Collected), sssDates[, 1])]
)

sssMat <- table(sssDat$SurveyID, sssDat$Pass)
sssSurveyOcc <- as.numeric(
  unique(sssDat$Date.Collected) - as.Date(StartDate) + 1
) # occasion of survey relative to StartDate
Ksss = dim(sssMat)[1] # number of sss survey days
V = dim(sssMat)[2] # number of sss passes


plot(dat2021[, c("long", "lat")], col = dat2021$reach, cex = 1, pch = 16)
points(sssDat[, c("Lon", "Lat")], col = "magenta", pch = 16, cex = .75) # SSS target

###----------------------------------------###
### load temp data
temp2021 <- fread('census_model/2021_Temp.csv')
temp2021[, temp_diff := Temp - shift(Temp)]
temp2021$Date <- as.Date(temp2021$Date, "%d-%b")
SeafordTemp2021 <- ggplot() +
  geom_point(data = temp2021, aes(x = Date, y = Temp)) +
  xlab("") +
  ggtitle("2021") +
  ylab("Seaford Temperature (\u00B0C)") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, ),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(hjust = 1)
  )
temp2021

###----------------------------------------###
## Start building the JAGS model

sink("census_model/Sturgeon_SSS_Telem_MS_ExpectedProb_test.txt")

cat(
  "
model {


  ###########################
## PART I. Jolly-Seber for marked individuals 

## Jolly-seber (superpopulation formulation) for marked individuals
for(i in 1:G){                       # G is fixed and G>>Msuper
  w[i] ~ dbern (psi)                  # inclusion in superpop
  z[i,1] ~ dcat(xi[1:6])              # starting state (this now includes 6)       
  u[i,1] <- z[i,1]*w[i]               # latent process of interest
  y[i,1] ~ dcat(Rho[z[i,1],i,1,1:5])  # capture history data at k=1 (now 1-5)
  
  
  LNR[i,1] <- u[i,1]==2               # 1 if in Nanticoke at k. 0 otherwise
  LMC[i,1] <- u[i,1]==3               # 1 if in LMC at k. 0 otherwise
  UMC[i,1] <- u[i,1]==4               # 1 if in UMC at k. 0 otherwise
  UNR[i,1] <- u[i,1]==5               # 1 if in UNR at k. 0 otherwise
  inSystem[i,1] <- LNR[i,1]+LMC[i,1]+UMC[i,1]+UNR[i,1] 
  ever.alive[i,1] <- u[i,1]>1         # 1 if ever alive by k
  recruit[i,1] <- u[i,1]==2           # Recruit on day 1
  
  for(k in 2:K){
    z[i,k] ~ dcat(Omega[z[i,k-1], k-1, 1:6]) # survives if alive, recruits if not yet entered (now includes 6?)
    u[i,k] <- z[i,k]*w[i]               # latent process of interest
    y[i,k] ~ dcat(Rho[z[i,k],i,k,1:5])   # prob. detected if alive (now 1-5)
    LNR[i,k] <- u[i,k]==2               # 1 if in Nanticoke at k. 0 otherwise
    LMC[i,k] <- u[i,k]==3               # 1 if in LMC at k. 0 otherwise
    UMC[i,k] <- u[i,k]==4               # 1 if in UMC at k. 0 otherwise
    UNR[i,k] <- u[i,k]==5               # 1 if in UNR at k. 0 otherwise
    inSystem[i,k] <- LNR[i,k]+LMC[i,k]+UMC[i,k]+UNR[i,k]
    ever.alive[i,k] <- u[i,k]>1         # 1 if ever alive by k
    recruit[i,k] <-(u[i,k-1]==1)*(u[i,k]>1)    # Recruit on day k
  }#K
}#i

### PART Ia. Distribution of individuals on day 1
# Recruitment from superpopulation (gamma parameterization of dirichlet prior)
for(k in 1:K){
  eb[k] <- exp(beta1*k) # entry prob varies as a linear function of day.
  b[k] <- eb[k]/sum(eb[1:K]) # recruitment probability on day k
}

# JS recruitment (derived) conditional on data augmentation value
eta[1] <- b[1]
for(k in 2:K){
  eta[k] <- b[k]/(1 - sum(b[1:(k-1)]) )
}

# start distirbution of superpopulation (not entered, LNR, LMC, UMC,UNR)
xi <-c(1-eta[1], eta[1]*theta[1,2], eta[1]*theta[1,3], eta[1]*theta[1,4],eta[1]*theta[1,5],0)

### PART Ib. Movement among reaches (come back to)
# If enter system by day one, prob of in reach 2:5
theta[1,1] <- 0
theta[1,2:5] ~ ddirch(a[2:5]) # LNR, LMC, UMC, UNR

# If enter in system after day one, prob of in reach 2:3
entryProb[1:3] ~ ddirch(a[1:3]) 
theta[2,1] <- 0
theta[2,2] <- entryProb[1]
theta[2,3] <- entryProb[2]
theta[2,4] <- 0 # UMC Cannot recruit into UMC from the ocean as you would be assinged as LMC
theta[2,5] <- entryProb[3] # UNR Cannot recruit into UNR from the ocean as you would be assinged LNR
# transitions among river reaches conditional on being in the system
piTran[1,1:5] <- rep(0,5)                # not entered, so transition probablities are irrelavant.
 

# LNR fish may move to
moveProb2[1:3] ~ ddirch(a[1:3]) 
piTran[2,1] <- 0                  # not entered
piTran[2,2] <- moveProb2[1] # LNR
piTran[2,3] <- moveProb2[2] # LMC
piTran[2,4] <- 0                  # Can't enter the UMC without passing the LMC
piTran[2,5] <- moveProb2[3] # UNR


# LMC fish may move to 
piTran[3,1] <- 0                  # not entered
piTran[3,2:5] ~ ddirch(a[2:5]) # LNR, LMC, UMC, UNR

# UMC fish may move to
piTran[4,1] <- 0                  # not entered 
piTran[4,2] <- 0                  # Can't go from UMC to LNR without passing LMC
piTran[4,3:4] ~ ddirch(a[3:4])    # LMC, UMC
piTran[4,5] <- 0                  # Can't go from UMC to UNR without passing LMC

# UNR fish may move to
moveProb5[1:3] ~ ddirch(a[1:3]) 
piTran[5,1] <- 0                  # not entered
piTran[5,2] <- moveProb5[1] # LNR
piTran[5,3] <- moveProb5[2] # LMC
piTran[5,4] <- 0                  # Can't enter the UMC without passing the LMC
piTran[5,5] <- moveProb5[3] # UNR


## Part Ic. State matrix
for(k in 2:K){
  ## not yet entered (state=1) to ...
  Omega[1,k-1,1] <- (1-eta[k])           # not yet entered
  Omega[1,k-1,2] <- eta[k]*theta[2,2]   # entered LNR
  Omega[1,k-1,3] <- eta[k]*theta[2,3]   # entered LMC
  Omega[1,k-1,4] <- eta[k]*theta[2,4]   # entered UMC
  Omega[1,k-1,5] <- eta[k]*theta[2,5]   # entered UNR
  Omega[1,k-1,6] <- 0                    # exited system
  
  ## in LNR (state=2) to ...
  Omega[2,k-1,1] <- 0                      # not yet entered
  Omega[2,k-1,2] <- phi[k-1]*piTran[2,2]   # remain in LNR
  Omega[2,k-1,3] <- phi[k-1]*piTran[2,3]   # move to LMC
  Omega[2,k-1,4] <- phi[k-1]*piTran[2,4]   # move to UMC
  Omega[2,k-1,5] <- phi[k-1]*piTran[2,5]   # move to UNR
  Omega[2,k-1,6] <- 1-phi[k-1]             # exit system
  
  ## in LMC (state=3) to ...
  Omega[3,k-1,1] <- 0                      # not yet entered
  Omega[3,k-1,2] <- phi[k-1]*piTran[3,2]   # move to UNR
  Omega[3,k-1,3] <- phi[k-1]*piTran[3,3]   # remain in LMC
  Omega[3,k-1,4] <- phi[k-1]*piTran[3,4]   # move to UMC
  Omega[3,k-1,5] <- phi[k-1]*piTran[3,5]   # move to UNR
  Omega[3,k-1,6] <- 1-phi[k-1]             # exit system
  
  
  ## in UMC (state=4) to ...
  Omega[4,k-1,1] <- 0                      # not yet entered
  Omega[4,k-1,2] <- phi[k-1]*piTran[4,2]   # move to UNR
  Omega[4,k-1,3] <- phi[k-1]*piTran[4,3]   # move to LMC
  Omega[4,k-1,4] <- phi[k-1]*piTran[4,4]   # remain in UMC
  Omega[4,k-1,5] <- phi[k-1]*piTran[4,5]   # move to UNR
  Omega[4,k-1,6] <- 1-phi[k-1]             # exit system
  
  ## in UNR (state=5) to ...
  Omega[5,k-1,1] <- 0                      # not yet entered
  Omega[5,k-1,2] <- phi[k-1]*piTran[5,2]   # move to UNR
  Omega[5,k-1,3] <- phi[k-1]*piTran[5,3]   # move to LMC
  Omega[5,k-1,4] <- phi[k-1]*piTran[5,4]   # remain in UMC
  Omega[5,k-1,5] <- phi[k-1]*piTran[5,5]   # move to UNR
  Omega[5,k-1,6] <- 1-phi[k-1]             # exit system
  
  
  ## in exited system (state=6) to ...
  Omega[6,k-1,1] <- 0                    # not yet entered
  Omega[6,k-1,2] <- 0                    # move to LNR
  Omega[6,k-1,3] <- 0                    # move to LMC
  Omega[6,k-1,4] <- 0                    # move to UMC
  Omega[6,k-1,5] <- 0                    # move to UNR
  Omega[6,k-1,6] <- 1                    # exited system
}

## Part Id. telem observation matrix
for(i in 1:G){
  for(k in 1:K){
    Rho[1, i,k, 1] <- 1    # not detected
    Rho[1, i,k, 2] <- 0    # UNR
    Rho[1, i,k, 3] <- 0    # LMC
    Rho[1, i,k, 4] <- 0    # UMC
    Rho[1, i,k, 5] <- 0    # UNR
    
    ## in LNR (state=2) to ...
    Rho[2, i,k, 1] <- 1-pbar[2]*w[i]*TelemIndicator[i, k]    # not detected
    Rho[2, i,k, 2] <- pbar[2]*w[i]*TelemIndicator[i, k]      # LNR
    Rho[2, i,k, 3] <- 0    # LMC
    Rho[2, i,k, 4] <- 0    # UMC
    Rho[2, i,k, 5] <- 0    # UNR
    
    ## in LMC (state=3) to ...
    Rho[3, i,k, 1] <- 1-pbar[3]*w[i]*TelemIndicator[i, k]    # not detected
    Rho[3, i,k, 2] <- 0      # LNR
    Rho[3, i,k, 3] <- pbar[3]*w[i]*TelemIndicator[i, k]      # LMC
    Rho[3, i,k, 4] <- 0    # UMC
    Rho[3, i,k, 5] <- 0    # UNR
    
    ## in UMC (state=4) to ...
    Rho[4, i,k, 1] <- 1-pbar[4]*w[i]*TelemIndicator[i, k]    # not detected
    Rho[4, i,k, 2] <- 0    # LNR
    Rho[4, i,k, 3] <- 0    # LMC
    Rho[4, i,k, 4] <- pbar[4]*w[i]*TelemIndicator[i, k]      # UMC
    Rho[4, i,k, 5] <- 0    # UNR
    
    ## in UNR (state=5) to ...
    Rho[5, i,k, 1] <- 1-pbar[5]*w[i]*TelemIndicator[i, k]    # not detected
    Rho[5, i,k, 2] <- 0    # LNR
    Rho[5, i,k, 3] <- 0    # LMC
    Rho[5, i,k, 4] <- 0    # UMC
    Rho[5, i,k, 5] <- pbar[5]*w[i]*TelemIndicator[i, k]      # UNR
    
    ## Exited (state=6) to ...
    Rho[6, i,k, 1] <- 1    # not detected
    Rho[6, i,k, 2] <- 0    # LNR
    Rho[6, i,k, 3] <- 0    # LMC
    Rho[6, i,k, 4] <- 0    # UMC
    Rho[6, i,k, 5] <- 0    # UNR
  }
}
###########################
## PART II. SSS count data that occurs in LMC

for(k in 1:Ksss){  # number of surveyed days
  for(v in 1:V){ # number of passes per surveyed day
    sssMat[k,v] ~ dbin(p_sss, Nreach[sssSurveyOcc[k],3]) # abundance references reach 3 (LMC)
  }
}



###########################
## PART III.  Population (system-wide, marked+unmarked) level recruitment, persistence, and abundance
NsuperK ~ dpois(LambdaSuper)  # super population of all individuals that used study area

# distribute individuals according to those processes
# Nreach[1:6,1]~dumulti(xi[1:6], N[1])
# JAGS does not allow stochastic N in dmulti. 
# Instead, Nreach[s,k] is estimated from the expected probablities for each day (conditional on NsuperK, starting distribution (pi1), staying probability (phi[k]), and transition probabilties (piTrans)
# In theory this could also be done using custom samplers (e.g., Link et al. 2018) or a series of conditional binomials
# the former is not possibe in JAGS and we explored the latter but mixing was terrible.

# Occasion 1
expectedProbabilities[1,1:6] <- xi[1:6] # starting probablities
for(k in 2:K){
  expectedProbabilities[k,1:6] <- expectedProbabilities[k-1,1:6]%*%Omega[1:6,k-1,1:6]
}

# number of individuals in each state (1=not entered, 2=Nant, ... 6=exited) on each day
Nreach[1,1] ~ dbin(expectedProbabilities[1,1], NsuperK)  # not yet entered on day 1 if in superpopulation
for(j in 2:(J-1)){
  Nreach[1,j] ~ dbin(min(0.99999, expectedProbabilities[1,j]/(1-sum(expectedProbabilities[1,1:(j-1)]))), NsuperK-sum(Nreach[1,1:(j-1)]) )
  #min(.99999) is required as sometimes this values goes to 1.00000000 and throws an error
}#s
Nreach[1,J] <- NsuperK-sum(Nreach[1,1:(J-1)])

# for occasions 2:(K-1)
for(k in 2:(K-1)){
  Nreach[k,1] ~ dbin(expectedProbabilities[k,1], NsuperK)  # not yet entered on day k if in superpopulation
  for(j in 2:(J-1)){
    Nreach[k,j] ~ dbin(min(.99999, expectedProbabilities[k,j]/(1-sum(expectedProbabilities[k,1:(j-1)]))), NsuperK-sum(Nreach[k,1:(j-1)]) )
  }#s
  Nreach[k,J] <- NsuperK-sum(Nreach[k,1:(J-1)])
}#k

# for occasions K,all individuals must have entered
for(k in K:K){
  Nreach[k,1] <-0  # no yet entered on day K if in superpopulation.
  for(j in 2:(J-1)){
    Nreach[k,j] ~ dbin(min(.99999, expectedProbabilities[k,j]/(1-sum(expectedProbabilities[k,1:(j-1)]))), NsuperK-sum(Nreach[k,1:(j-1)]) )
  }#s
  Nreach[k,J] <- NsuperK-sum(Nreach[k,1:(J-1)])
}#k

###########################
## Part IV Derived abundances 
for(k in 1:K){
  # marked population
  M[k] <- sum(inSystem[1:G,k])                 # total marked individuals in system on day k
  Mlnr[k] <- sum(LNR[1:G,k])                   # total marked individuals in LNR on day k
  Mlmc[k] <- sum(LMC[1:G,k])                   # total marked individuals in LMC on day k
  Mumc[k] <- sum(UMC[1:G,k])                   # total marked individuals in UMC on day k
  Mmc[k] <- Mlmc[k]+Mumc[k]                    # total marked individuals in mc on day k
  Munr[k] <- sum(UNR[1:G,k])                   # total marked individuals in UNR on day k
  Mnan[k] <- Mlnr[k]+Munr[k]                   # total marked individuals in NR on day k
  
  Msuper[k] <- sum(ever.alive[1:G,k])          # number of marked individuals that used study area on or before k
  mR[k] <- sum(recruit[1:G,k])                 # number of new marked recruits on day k
  
  # total superpopulation by day (i.e., unique individuals that used system by day k)
  Nsuper[k] <- NsuperK-Nreach[k,1]                # total individuals that recruited into study area on or before k. This is the total superpopulation minus those that have not yet recruited. 
  N[k] <- sum(Nreach[k,2:5])                      # number of fish in the system
  Nmc[k] <- sum(Nreach[k,3:4])                    # number in MC (lmc+umc). 
  #Nnan[k] <- sum(Nreach[k,c(2,5)])                # number in NR (lnr+unr). 
  Nnan[k] <- sum(Nreach[k,2]) + sum(Nreach[k,5])               # number in NR (lnr+unr). 
}

# expected Recruits - number of fish that enter the system. i.e., from state not yet entered to one of the entered states 
R[1] ~ dbin(eta[1], NsuperK)
for(k in 2:(K-1)){
  R[k] ~ dbin(eta[k], NsuperK-sum(R[1:(k-1)]) ) # note that this is not by reach.             
}
R[K] <- NsuperK-sum(R[1:(K-1)])  # note that this is not by reach.     

###########################
## Part V. Priors and constraints 

# JS marked popultion priors
psi ~ dbeta(0.0001,1)  # inclusion in Nanticoke basin marked superpopulaiton 

# Telemetry detection probablity
pbar[1] <- 0          # telemetry detection prob. if in state: not-entered
pbar[2]~dbeta(1,1)    # telemetry detection prob. if in state: lnr
pbar[3]~dbeta(1,1)    # telemetry detection prob. if in state: lmc
pbar[4]~dbeta(1,1)    # telemetry detection prob. if in state: umc
pbar[5]~dbeta(1,1)    # telemetry detection prob. if in state: unr
pbar[6]<-0            # telemetry detection prob. if in state: exited system

# recruitment priors - relationship of day and recruitment prob. 
beta1~dnorm(0,.1)

# prob of staying in the system is a function of temp on day k
for(k in 1:K){
  logit(phi[k]) <- alpha0 + alpha1*temp[k]
}
alpha0 <- logit(phi0)
phi0~dbeta(1,1)         # prob. of staying in system
alpha1~dnorm(0,.1)       # relationship between prob of staying and time

# sss detection prior
p_sss ~dbeta(1,1) # side-scan sonar detection prob. 

# super population of all individuals that used study area prior 
LambdaSuper ~ dgamma(0.1,0.1) 

}#model
",
  fill = TRUE
)
sink()


## data augmentation stuff
G <- 25 # data augmentation. set this value so that G>>Msuper
nz <- G - ntelem # number of extra individuals
w <- c(rep(1, ntelem), rep(NA, nz)) # marked individuals we know entered the system
yAug <- rbind(y, matrix(1, nz, K))
TelemIndicatorAug <- rbind(TelemIndicator, matrix(1, nz, K))
J = 6 # states (not entered, 4 reaches, exited)


jags.data2 <- list(
  #SSS data
  sssMat = as.matrix(sssMat), # SSS counts
  Ksss = Ksss, # number of SSS surveyed days
  sssSurveyOcc = sssSurveyOcc, # survey dates
  V = V, # number of passes

  # telemetry data
  K = K,
  G = G,
  w = w,
  y = yAug,
  TelemIndicator = TelemIndicatorAug,
  a = rep(1, 5),
  J = J,
  temp = temp2021$Temp[5:82]
  #tempDat$Seaford[11:83] #Change this for 2021 temp data
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
  "Mnanticoke",
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
for (i in 1:ntelem) {
  zInits[i, startTelem[i]:lastTelem[i]] <- yAug[i, startTelem[i]:lastTelem[i]] # in the system here
  if (lastTelem[i] < K) {
    zInits[i, (lastTelem[i] + 1):K] <- 6
  } # just have them leave the system to start
  if (1 %in% zInits[i, startTelem[i]:lastTelem[i]]) {
    zInits[i, startTelem[i]:lastTelem[i]][which(
      zInits[i, startTelem[i]:lastTelem[i]] == 1
    )] <- 2
  }
}

zInits[(ntelem + 1):G, ] <- 2

theta <- matrix(NA, 2, 5)
theta[1, ] <- c(NA, 0.40, 0.40, 0.10, 0.10) # can enter reaches 2-5 on day 1
entryProb <- c(0.50, 0.30, 0.20) # can only enter reaches 2-3 on days 2:K (cannot enter UMC as it requires being assigned to LMC)

piTranInits <- matrix(NA, 5, 5)
#piTranInits[2,2:3] <- c(0.90, 0.10)
piTranInits[3, 2:5] <- c(0.10, 0.65, 0.05, 0.20)
piTranInits[4, 3:4] <- c(.5, .5)
#piTranInits[5,c(2,3,5)] <- c(0.3,0.2,0.5)

NsuperKinit <- 100 # start the population somewhere reasonable. Too large is better than too small.

NreachInits <- matrix(NA, K, 6)
NreachInits[1, 1:5] <- as.vector(rmultinom(
  1,
  NsuperKinit,
  c(.55, .15, .1, .05, 0.15)
))
for (k in 1:length(sssSurveyOcc)) {
  NreachInits[sssSurveyOcc[k], 3] <- max(sssMat[k, ]) + 2 # the only big restrictions is that abundance in lmc on sss survey days must be >=max count
}

Rinit <- as.vector(rmultinom(1, NsuperKinit, c(.3, .2, rep(.05, K - 2)))) # again, something reasonable
Rinit[K] <- NA

# initial values
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
#nc <- 2; nAdapt=50; nb <- 1; ni <- 50+nb; nt <- 1
nc <- 5
nAdapt = 25000
nb <- 10000
ni <- 50000 + nb
nt <- 10

# Call JAGS from R
out2021 <- jags(
  jags.data,
  inits,
  params,
  "census_model/Sturgeon_SSS_Telem_MS_ExpectedProb_test.txt",
  n.adapt = nAdapt,
  n.chains = nc,
  n.thin = nt,
  n.iter = ni,
  n.burnin = nb,
  parallel = T
)


# Summarize posteriors
print(out2021, dig = 2, )


# save output as a matrix to make plots and summarize data
outmat2021 <- as.matrix(out2021$samples)


## start of some plotting ideas (very draft and currently ugly!!)

#plot abundance and distribution of marked individuals
plot(
  out2021$q50$N,
  ylim = c(0, 80),
  xaxt = "n",
  xlab = "Date",
  ylab = "Sturgeon (marked+unmarked)",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2021$q2.5$N, 1:K, out2021$q97.5$N)
axis(1, at = 1:K, format(days, '%m-%d'))

points(1:K - .2, out2021$q50$Nnan, col = "red", pch = 16) # total in nanticoke by day
segments(1:K - .2, out2021$q2.5$Nnan, 1:K - .2, out2021$q97.5$Nnan, col = "red")

points(1:K + .2, out2021$q50$Nmc, col = "blue", pch = 16) # total in mc by day
segments(1:K + .2, out2021$q2.5$Nmc, 1:K + .2, out2021$q97.5$Nmc, col = "blue")

legend(
  "topright",
  legend = c("Nanticoke system", "Nanticoke reaches", "MC reaches"),
  text.col = c("black", "red", "blue"),
  bty = 'n',
  cex = 1.1
)

# plot daily abundance and cumulative abundance of individuals
plot(
  out2021$q50$Nsuper,
  ylim = c(0, 110),
  pch = 16,
  ,
  xaxt = "n",
  xlab = "Date",
  ylab = "Sturgeon (marked+unmarked)",
  las = 1
) # total marked in system by day
segments(1:K, out2021$q2.5$Nsuper, 1:K, out2021$q97.5$Nsuper)
axis(1, at = 1:K, format(days, '%m-%d'))

points(1:K - .25, out2021$q50$N, col = "red", pch = 16) # total marked in system by day
segments(1:K - .25, out2021$q2.5$N, 1:K - .25, out2021$q97.5$N, col = "red")
legend(
  "topleft",
  legend = c("Cumulative", "Daily"),
  text.col = c("black", "red"),
  bty = 'n',
  cex = 1.2
)

#Idea: plot persistence probs.
plot(
  out2021$q50$phi,
  ylim = c(0.80, 1.00),
  xaxt = "n",
  xlab = "Date",
  ylab = "Daily persistence probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2021$q2.5$phi, 1:K, out2021$q97.5$phi)
axis(1, at = 1:K, format(days, '%m-%d'))

plot(
  jags.data$temp,
  out2021$q50$phi,
  ylim = c(0.80, 1.00),
  xlab = "Temp",
  ylab = "Daily persistence probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(jags.data$temp, out2021$q2.5$phi, jags.data$temp, out2021$q97.5$phi)

#Idea: plot recruitment probs.
plot(
  out2021$q50$b,
  ylim = c(0.00, 0.26),
  xaxt = "n",
  xlab = "Date",
  ylab = "Daily recruitment probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2021$q2.5$b, 1:K, out2021$q97.5$b)
axis(1, at = 1:K, format(days, '%m-%d'))

#Temperature overlaying Persistence
plot(
  out2021$q50$phi,
  xaxt = "n",
  ylim = c(0.80, 1.00),
  xlab = "Date",
  ylab = "Daily persistence probability",
  las = 1,
  pch = 16
)
segments(1:K, out2021$q2.5$phi, 1:K, out2021$q97.5$phi)
axis(1, at = 1:K, format(days, '%m-%d'))
par(new = TRUE)
plot(
  jags.data$temp,
  pch = 16,
  col = "red",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(12, 30)
)
axis(side = 4, xlab = "", ylab = "")
mtext("Temperature (\u00B0C)", side = 4, line = 3)


#Flow overlaying Persistence
plot(
  out2021$q50$phi,
  xaxt = "n",
  ylim = c(0.80, 1.00),
  xlab = "Date",
  ylab = "Daily persistence probability",
  las = 1,
  pch = 16
)
segments(1:K, out2021$q2.5$phi, 1:K, out2021$q97.5$phi)
axis(1, at = 1:K, format(days, '%m-%d'))
par(new = TRUE)
plot(
  flow$average,
  pch = 16,
  col = "blue",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(0, 400)
)
axis(side = 4, xlab = "", ylab = "")
mtext("Flow (ft^3)/s)", side = 4, line = 3)

#Temperature overlaying Recruitment
plot(
  out2021$q50$b,
  ylim = c(0.00, 0.26),
  xaxt = "n",
  xlab = "Date",
  ylab = "Daily recruitment probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2021$q2.5$b, 1:K, out2021$q97.5$b)
axis(1, at = 1:K, format(days, '%m-%d'))
par(new = TRUE)
plot(
  jags.data$temp,
  pch = 16,
  col = "red",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(12, 30)
)
axis(side = 4, xlab = "", ylab = "")
mtext("Temperature (\u00B0C)", side = 4, line = 3)

#Flow overlaying Recruitment
plot(
  out2021$q50$b,
  ylim = c(0.00, 0.26),
  xaxt = "n",
  xlab = "Date",
  ylab = "Daily recruitment probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2021$q2.5$b, 1:K, out2021$q97.5$b)
axis(1, at = 1:K, format(days, '%m-%d'))
par(new = TRUE)
plot(
  flow$average,
  pch = 16,
  col = "blue",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(0, 400)
)
axis(side = 4, xlab = "", ylab = "")
mtext("Flow (ft^3)/s)", side = 4, line = 3)

#MC vs UNR
plot(
  out2021$q50$Nmc,
  ylim = c(0, 50),
  xaxt = "n",
  xlab = "Date",
  ylab = "Sturgeon (marked+unmarked)",
  las = 1,
  pch = 16
)
segments(1:K, out2021$q2.5$Nmc, 1:K, out2021$q97.5$Nmc)
axis(1, at = 1:K, format(days, '%m-%d'))

points(1:K - .2, UNR_2021$UNR, col = "blue", pch = 16) # total in UNR by day
segments(1:K - .2, UNR_2021$UNR_2.5, 1:K - .2, UNR_2021$UNR_97.5, col = "blue")
legend(
  "topright",
  legend = c("Marshyhope Creek Reaches", "Upper Nanticoke Reach"),
  text.col = c("black", "blue"),
  bty = 'n',
  cex = 1.1
)
