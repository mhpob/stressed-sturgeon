library(data.table)
library(cmdstanr)

master_data <- readRDS("census_model/census_data_master.RDS")
for (i in seq_along(master_data)) {
  assign(names(master_data)[i], master_data[[i]])
}

#### Define the study period ####
StartDate <- as.POSIXct("2024-08-13 00:00:00", tz = "UTC") # seems to be the earliest temperature
EndDate <- as.POSIXct("2024-10-30 23:59:59", tz = "UTC")
det_2024 <- detections[dateandtimeutc %between% c(StartDate, EndDate)]

K <- as.numeric(as.Date(EndDate) - as.Date(StartDate)) + 1
days <- seq(as.Date(StartDate), as.Date(EndDate), 1)
ntelem <- length(unique(det_2024$transmitter))


#### Import SSS ####
sss_2024 <- sss[year(date) == 2024]

# make survey id
sss_2024[, survey_day := as.numeric(date - as.Date(StartDate) + 1)]

# Convert to long format in order to handle any "NA" passes
sss_2024 <- melt(
  sss_2024,
  id.vars = c("date", "reach_no", "survey_day"),
  measure.vars = c("1", "2"),
  variable.name = "pass",
  value.name = "count"
) |>
  _[!is.na(count)]

#### Create detection matrix ####
# 'LNR' = 2
# 'UNR' = 5
# 'LMC' = 3
# 'UMC' = 4
# "BC"  = 6
### OLD RULES!! (LMC preference) ###
# 1) if in LMC, assign to LMC
# 2) if in both UNR and UMC, assign to LMC
# 3) if in both LNR and UMC, assigne to LMC
# 3) else max reach
# y_mat <- copy(det_2022) |>
#   unique(by = c("transmitter", "date", "reach_no")) |>
#   _[,
#     .(
#       reach = fifelse(
#         3 %in%
#           reach_no |
#           all(c(4, 5) %in% reach_no) |
#           all(c(2, 4) %in% reach_no),
#         3,
#         max(reach_no)
#       )
#     ),
#     by = c("transmitter", "date")
#   ]

### NEW RULES!! ###
# 1) if in UNR, assign to UNR
# 2) if in UMC, assign to UMC
# 3) If in both UNR and UMC, use the last location
# 4) If both UNR and BC, use UNR

y_mat <- det_2024 |>
  _[, .(dt = max(dateandtimeutc)), by = .(transmitter, date, reach_no)] |>
  _[,
    .(
      reach = fcase(
        # Rule 1: Reach 6 AND another reach exist -> pick the other reach
        uniqueN(reach_no) > 1 & 6 %in% reach_no ,
        max(reach_no[reach_no != 6])            ,

        # Rule 2: Both 4 and 5 exist -> pick the one with the later max timestamp
        all(c(4, 5) %in% reach_no)              ,
        reach_no[which.max(dt)]                 ,

        # Default Rule: Just take the max reach
        default = max(reach_no)
      )
    ),
    by = c("transmitter", "date")
  ]

## call the above y_mat2 compare the two if desired:
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
  N_sss_obs = nrow(sss_2022),
  sss_counts = sss_2022$count,
  sss_day = sss_2022$survey_day,
  sss_reach = sss_2022$reach_no
)


cmdstanr::write_stan_json(
  data_list,
  "census_model/Stan/2022/2022BC_stan_data.json"
)
