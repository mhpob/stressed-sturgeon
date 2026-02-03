library(data.table)

dat <-
  "/run/media/obrien/OS/Users/darpa2/Desktop/db_parsed.csv" |>
  data.table::fread()
dat[, detections := gsub('""', '\"', detections)]
dat[,
  detections := sapply(detections, function(.) {
    as.data.table(eval(parse(text = .)))
  })
]

detections <- rbindlist(dat$detections)
detections[, datetimeutc := as.POSIXct(datetimeutc)]
setorder(detections, datetimeutc)

library(mgcv)

d_hr <- dat[,
  .(temp = mean(T)),
  by = (hr = lubridate::floor_date(receiver_time, 'hour'))
]
d_day <- dat[,
  .(temp = mean(T)),
  by = (day = as.numeric(as.Date(receiver_time)))
]

library(gratia)
k <- bam(temp ~ s(as.numeric(hr), k = 20), data = d_hr, method = 'REML')

library(survival)
colon
?colon


k <- gam(temp ~ s(day, k = 15), data = d_day, method = 'REML')

gam(PA ~ te(T, day) + s(indiv, bs = 're'))

# sept1 - oct15
dat[
  receiver_time %between% c('2024-09-01', '2025-10-15'),
  .(temp = mean(T)),
  by = (date = as.Date(receiver_time))
]

mod_dat <- unique(
  detections[, date := as.Date(datetimeutc)],
  by = c('tag', 'date')
) |>
  _[, .N, by = 'date'] |>
  _[
    dat[
      receiver_time %between% c('2024-09-01', '2025-10-15'),
      .(temp = mean(T)),
      by = (date = as.Date(receiver_time))
    ],
    on = 'date'
  ] |>
  _[, N := fifelse(is.na(N), 0, N)]

library(brms)
k1 <- gam(N ~ te(as.numeric(date), temp), data = mod_dat[1:8], method = 'NCV')

j <- numeric(length = 45 - 6)
for (i in 7:45) {
  j[i - 6] <- gam(
    N ~ te(as.numeric(date), temp),
    data = mod_dat[1:i],
    family = 'poisson',
    method = 'NCV'
  ) |>
    summary() |>
    _$sp.criterion[1]
}


mod_dat$freq <- mod_dat$N / uniqueN(detections, 'tag')
j <- numeric(length = 45 - 6)
for (i in 7:45) {
  j[i - 6] <- gam(
    cbind(N, 6 - N) ~ s(temp, k = 5),
    data = mod_dat[1:i],
    family = 'binomial',
    method = 'QNCV'
  ) |>
    summary() |>
    _$sp.criterion[1]
}


###########

library(ggsurvfit)
k <- detections[,
  .(
    time = (as.numeric(
      min(datetimeutc) -
        as.POSIXct('2024-09-01 00:00:00')
    ) /
      (60 * 60 * 24)) |>
      ceiling(),
    status = 1
  ),
  by = 'tag'
]


survfit(Surv(time, status) ~ 1, data = k[1:4, ]) |>
  summary(times = seq(0, 45, 1)) |>
  str()

# j <- k[data.table(time=seq(0, 44, 1)), , on = 'time'] |>
#   _[d_day[, .(time = day - as.numeric(as.Date('2024-09-01')), temp)][time >=0], , on = 'time']
# j[is.na(status), status := 0]

j_list <- list()
d <- copy(k)

for (i in seq(1, 45, 1)) {
  d[, status := fifelse(time <= i - 1, 1, 0)]

  j_list[[i]] <- survfit(Surv(time, status) ~ 1, data = d) |>
    summary(times = seq(0, i - 1, 1)) |>
    _[c('time', 'surv', 'lower', 'upper')] |>
    data.frame()
}

j_list <- rbindlist(j_list, idcol = 'run')

library(ggplot2)
ggplot(j_list, aes(x = time, group = run, color = run, fill = run)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.01) +
  geom_line(aes(y = surv, alpha = run)) +
  scale_color_viridis_c(direction = -1) +
  scale_fill_viridis_c(direction = -1)


jjj <- fread('Data/Detections/sturgeon_22_23.gz') |>
  _[stationname == 'Hatchery'] |>
  _[, let(yr = year(dateandtimeutc), yday = yday(dateandtimeutc))] |>
  _[yday >= 244] |>
  _[, yday := yday - 244] |>
  _[,
    .(tag = transmitter, time = min(yday), status = 1),
    by = c('transmitter', 'yr')
  ] |>
  _[, transmitter := NULL]


jj2 <- fread('Data/Detections/old/sturgeon_detections.gz') |>
  _[station == 'Hatchery'] |>
  _[, let(yr = year(date.utc), yday = yday(date.utc))] |>
  _[, yday := fifelse(yr == 2020, yday - 245, yday - 244)] |>
  _[yday >= 0] |>
  _[,
    .(tag = transmitter, time = min(yday), status = 1),
    by = c('transmitter', 'yr')
  ] |>
  _[, transmitter := NULL]


k[, yr := 2024]

jj <- rbind(
  jj2,
  jjj,
  k
)

survfit2(Surv(time, status) ~ 1, data = jj) |>
  plot()


j_list <- list()
d <- copy(jj)

for (i in seq(1, 45, 1)) {
  d[, status := fifelse((time <= i - 1 & yr == 2024) | yr != 2024, 1, 0)]

  j_list[[i]] <- survfit(Surv(time, status) ~ 1, data = d) |>
    summary(times = seq(0, 44, 1)) |>
    _[c('time', 'surv', 'lower', 'upper')] |>
    data.frame()
}


j_list <- rbindlist(j_list, idcol = 'run')

library(ggplot2)
ggplot(
  j_list[!is.na(lower)],
  aes(x = time, group = run, color = run, fill = run)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.01, color = NA) +
  geom_line(aes(y = surv, alpha = run), show.legend = FALSE) +
  labs(
    fill = "Day model was fit",
    x = "Day since Sept 1",
    y = "Prob. of Spawning Run"
  ) +
  scale_color_viridis_c(option = 'turbo') +
  scale_fill_viridis_c(option = 'turbo') +
  theme_minimal() +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.75, 0.75),
    legend.title.position = 'top',
    legend.text = element_text(size = 30),
    legend.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    axis.title = element_text(size = 35),
    legend.key.width = unit(1, "inch"),
    legend.direction = 'horizontal'
  )

ragg::agg_png(
  'survival_plot.png',
  width = 1046,
  height = 732
)

dev.off()
