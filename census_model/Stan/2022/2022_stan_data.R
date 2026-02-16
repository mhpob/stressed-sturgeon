library(data.table)
library(cmdstanr)

master_data <- readRDS("census_model/census_data_master.RDS")
for (i in seq_along(master_data)) {
  assign(names(master_data)[i], master_data[[i]])
}

#### Define the study period ####
StartDate <- as.POSIXct("2022-08-15 00:00:00", tz = "UTC") # seems to be the earliest temperature
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
V = 2 # number of sss passes. Just hardcoded for now

#### Create detection matrix ####
### OLD RULES!! (LMC preference) ###
# 1) if in LMC, assign to LMC
# 2) if in both UNR and UMC, assign to LMC
# 3) if in both LNR and UMC, assigne to LMC
# 3) else max reach
y_mat <- copy(det_2022) |>
  unique(by = c("transmitter", "date", "reach_no")) |>
  _[,
    .(
      reach = fifelse(
        3 %in%
          reach_no |
          all(c(4, 5) %in% reach_no) |
          all(c(2, 4) %in% reach_no),
        3,
        max(reach_no)
      )
    ),
    by = c("transmitter", "date")
  ]

### NEW RULES!! ###
# 1) if in UNR, assign to UNR
# 2) if in UMC, assign to UMC
# 3) If in both UNR and UMC, use the last location

# y_mat2 <- copy(det_2022) |>
#   unique(by = c("transmitter", "date", "reach_no")) |>
#   _[,
#     .(
#       reach = fifelse(
#         all(c(4, 5) %in% reach_no),
#         .SD[which.max(dateandtimeutc)]$reach_no,
#         max(reach_no)
#       )
#     ),
#     by = c("transmitter", "date")
#   ]

## compare the two if desired:
# y_mat[y_mat2, on = c("transmitter", "date")][reach != i.reach]

y_mat <- y_mat[
  expand.grid(transmitter = unique(y_mat$transmitter), date = days),
  on = c("transmitter", "date")
]
y_mat[is.na(reach), 'reach'] <- 1
y_mat <- dcast(y_mat, transmitter ~ date, value.var = 'reach')


#### Create tagging indicator ####
# matrix with 0 if th fish wasnt tagged and 1 if it was
# Rows are fish, columns are dates
cap <- ats[
  DateCaptured %between% c(as.Date(StartDate), "2023-12-31"),
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

setkey(cap, transmitter, start, date)
setkey(t_mat, transmitter, date, date2)

telem_indicator <- foverlaps(t_mat, cap) |>
  _[, tagged := as.numeric(is.na(start))] |>
  dcast(transmitter ~ i.date, value.var = 'tagged')


#### Import temperature ####
temp <- temp[date %between% c(as.Date(StartDate), as.Date(EndDate))]


#### Data Augmentation ####
G <- 25 # Super population of tagged individuals. Make sure this is bigger than ntelem
yAug <- rbind(y_mat |> as.matrix(rownames = T), matrix(1, G - ntelem, K))
TelemIndicatorAug <- rbind(
  as.matrix(telem_indicator, rownames = T),
  matrix(1, G - ntelem, K)
)

### Set up data ###
data_list <- list(
  K = K,
  G = G,
  n_telem = ntelem,
  y = yAug,
  TelemIndicator = TelemIndicatorAug,
  temp = round(temp$temp, 1),
  Ksss = Ksss,
  V = V,
  sssMat = as.matrix(sss_2022[, 3:4]),
  sssReach = as.array(sss_reaches), #as.array is needed if there's only one number
  sssSurveyOcc = as.array(sss_survey_occ) #same as above
)


cmdstanr::write_stan_json(
  data_list,
  "census_model/Stan/2022/2022lmc_stan_data.json"
)
