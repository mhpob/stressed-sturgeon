library(data.table)
library(cmdstanr)

generate_stan_data <- function(start_date, end_date, out_file) {
  master_data <- readRDS("census_model/census_data_master.RDS")
  for (i in seq_along(master_data)) {
    assign(names(master_data)[i], master_data[[i]])
  }

  start_date <- as.POSIXct(paste0(as.Date(start_date), " 00:00:00"), tz = "UTC")
  end_date <- as.POSIXct(paste0(as.Date(end_date), " 23:59:59"), tz = "UTC")

  yr <- year(start_date)

  det <- detections[dateandtimeutc %between% c(start_date, end_date)]

  K <- as.numeric(as.Date(end_date) - as.Date(start_date)) + 1
  days <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  ntelem <- length(unique(det$transmitter))

  ### Create Y matrix
  setorder(det, transmitter, dateandtimeutc)
  reaches <- det[,
    .SD[
      reach_no != shift(reach_no) | is.na(shift(reach_no)) | seq_len(.N) == .N,
      .(
        reach_no,
        start_time = dateandtimeutc,
        end_time = shift(dateandtimeutc, type = "lead")
      )
    ],
    by = transmitter
  ][!is.na(end_time)]
  setkey(reaches, transmitter, start_time, end_time)

  time_grid <- CJ(
    transmitter = unique(det$transmitter),
    start_time = seq(start_date, end_date, by = "day")
  ) |>
    _[, end_time := start_time + 86399]
  setkey(time_grid, transmitter, start_time, end_time)

  overlaps <- foverlaps(time_grid, reaches) |>
    _[
      !is.na(reach_no),
      duration := as.numeric(difftime(
        time1 = fifelse(end_time < i.end_time, end_time, i.end_time),
        time2 = fifelse(start_time > i.start_time, start_time, i.start_time),
        units = "secs"
      ))
    ] |>
    _[,
      .(total_seconds = sum(duration)),
      by = .(transmitter, date = as.Date(i.start_time), reach_no)
    ]
  setorder(overlaps, transmitter, date, -total_seconds, na.last = TRUE)

  y_mat <- overlaps[, .SD[1], by = .(transmitter, date)] |>
    _[is.na(reach_no), reach_no := 1] |>
    dcast(transmitter ~ date, value.var = "reach_no")

  ### Import SSS
  sss_sub <- sss[year(date) == yr]
  sss_sub[, survey_day := as.numeric(date - as.Date(start_date) + 1)]

  sss_sub <- melt(
    sss_sub,
    id.vars = c("date", "reach_no", "survey_day"),
    measure.vars = c("1", "2"),
    variable.name = "pass",
    value.name = "count"
  ) |>
    _[!is.na(count)]

  #### Create tagging indicator ####
  # matrix with 0 if th fish wasnt tagged and 1 if it was
  # Rows are fish, columns are dates
  cap <- ats[
    DateCaptured %between%
      c(as.Date(start_date), as.Date(paste0(yr + 1, "-12-31"))),
    .(
      date = (DateCaptured - 1),
      transmitter = TransmitterNumber,
      start = as.Date(start_date)
    )
  ]

  t_mat <- unique(det, by = c("transmitter", "date")) |>
    _[, .(transmitter, date)] |>
    _[
      expand.grid(transmitter = unique(det$transmitter), date = days),
      on = c("transmitter", "date")
    ] |>
    _[, date2 := date]

  setkey(cap, transmitter, start, date)
  setkey(t_mat, transmitter, date, date2)

  telem_indicator <- foverlaps(t_mat, cap) |>
    _[, tagged := as.numeric(is.na(start))] |>
    dcast(transmitter ~ i.date, value.var = 'tagged')

  #### Import temperature ####
  temp_sub <- temp[date %between% c(as.Date(start_date), as.Date(end_date))]

  #### Data Augmentation ####
  # G is superpopulation of tagged individuals. Make sure this is bigger than ntelem
  G <- 25
  yAug <- rbind(as.matrix(y_mat, rownames = TRUE), matrix(1, G - ntelem, K))
  TelemIndicatorAug <- rbind(
    as.matrix(telem_indicator, rownames = TRUE),
    matrix(1, G - ntelem, K)
  )

  ### Set up data ###
  data_list <- list(
    K = K,
    G = G,
    n_telem = ntelem,
    y = yAug,
    TelemIndicator = TelemIndicatorAug,
    temp = round(temp_sub$temp, 1),
    N_sss_obs = nrow(sss_sub),
    sss_counts = sss_sub$count,
    sss_day = sss_sub$survey_day,
    sss_reach = sss_sub$reach_no
  )

  cmdstanr::write_stan_json(data_list, out_file)
}


generate_stan_data(
  start_date = "2022-08-15",
  end_date = "2022-10-31",
  out_file = "census_model/Stan/2022/UUNR_stan_data.json"
)
