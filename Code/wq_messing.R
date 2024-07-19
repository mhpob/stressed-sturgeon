library(readxl)
library(data.table)

# MDNR 2019, 2022

# DNREC 2019, 2020, 2021


## MDNR
mdnr19 <- read_excel('data/misc/8.22-11.18 Marshyhope EXO.xlsx',
           sheet = 2)
setDT(mdnr19)
mdnr19[, dttm := as.POSIXct(paste(
  format(Date, '%Y-%m-%d'),
  format(Time, '%H:%M:%S')))
]
mdnr19 <- mdnr19[, c(1,6)]
setnames(mdnr19, c('date', 'temp'))
mdnr19 <- mdnr19[date < '2019-11-04']

mdnr22 <- read_excel('data/misc/8.29.22-11.29.22 Marshyhope EXO.xlsx',
                     sheet = 2)
setDT(mdnr22)
mdnr22[, dttm := as.POSIXct(paste(
  format(`Date (MM/DD/YYYY)`, '%Y-%m-%d'),
  format(`Time (HH:mm:ss)`, '%H:%M:%S')))
]
mdnr22 <- mdnr22[, c(1, 18)]  
setnames(mdnr22, c('date', 'temp'))

mdnr <- rbind(mdnr19, mdnr22)

mdnr[, let(year = year(date), doy = yday(date))]

mdnr <- mdnr[, .(median_temp = median(temp)), by = c('doy', 'year')]
mdnr <- mdnr[, doy_date := as.Date(doy)]

prevmdnr <- fread('s:/nansturg-analysis/manuscript/data/sonde_data_marshyhope.csv')
prevmdnr <- prevmdnr[, c(1, 3)]
setnames(prevmdnr, c('date', 'median_temp'))
prevmdnr[, let(year = year(date), doy = yday(date))]
prevmdnr[, doy_date := as.Date(doy)]

mdnr <- rbind(mdnr, prevmdnr[,-1])
mdnr[, body := 'Marshyhope']

library(ggplot2)
mdnr_plot <- ggplot(data = mdnr) + 
  annotate('rect', xmin = as.Date(-Inf), xmax = as.Date(Inf),
           ymin = 28, ymax = Inf, fill = 'red') +
  annotate('rect', xmin = as.Date(-Inf), xmax =as.Date(Inf),
           ymin = 20, ymax = 25, fill = 'green') +
  geom_line(aes(x = doy_date, y = median_temp)) + facet_wrap(~year) +
  labs(x = NULL, y = 'Water temperature (C)') +
  theme_minimal() 

# k <- mdnr[between(median_temp, 20, 25)][, l := doy_date-shift(doy_date), by = year]
k <- mdnr[between(median_temp, 20, 25)] |> 
  _[doy_date > as.Date(yday('2015-08-15'))] |> 
  _[, .(min = min(doy_date), max = max(doy_date), site = 'marshyhope'), by = year] |> 
  _[, n_days := max - min]

## DNREC
# hatch19 <- read_excel('data/misc/Hatchery pH 2019.xlsx', sheet = 1,
#                       skip = 1)
# setDT(hatch19)
# hatch19 <- hatch19[,-1]
# setnames(hatch19, c('dttm', 'temp', 'ph', 'ph_mv'))
# hatch19[, let(year = 2019, body = 'Nanticoke')]
# hatch21 <- read_excel('data/misc/Hatchery_D.O._2021_Final.xlsx', sheet = 1,
#                       skip = 1)
# setDT(hatch21)
# hatch21 <- hatch21[,-1]
# setnames(hatch21, c('dttm', 'temp', 'do_mgl', 'do_sat'))
# hatch21[, let(year = 2021, body = 'Nanticoke')]
# hatch20 <- read_excel('data/misc/Master 2020.xlsx', sheet = 3,
#                       range = cell_cols("A:G"))
# setDT(hatch20)
# hatch20 <- hatch20[,c(3,5)]
# setnames(hatch20, c('dttm', 'temp'))
# hatch20[, let(year = 2020, body = 'Nanticoke')]
# 
# 
# hatch <- rbind(hatch19, hatch20, hatch21, fill = T)
# 
# hatch[, doy := yday(dttm)]
# hatch[, tempC := (temp - 32) * 5/9]


seaf19 <- read_excel('data/misc/Seaford pH 2019.xlsx', sheet = 1,
                     skip = 1)
setDT(seaf19)
seaf19 <- seaf19[,-1]
setnames(seaf19, c('dttm', 'temp', 'ph', 'ph_mv'))
seaf19[, let(year = 2019, body = 'Nanticoke')]


seaf21 <- read_excel('data/misc/Seaford_D.O._2021.xlsx', sheet = 1,
                     skip = 1)
setDT(seaf21)
seaf21 <- seaf21[,c(2,3)]
setnames(seaf21, c('dttm', 'temp'))
seaf21[, let(year = 2021, body = 'Nanticoke')]


seaf20 <- read_excel('data/misc/Master 2020.xlsx', sheet = 3,
                      range = cell_cols("J:N"))
setDT(seaf20)
seaf20 <- seaf20[,c(1,3)]
setnames(seaf20, c('dttm', 'temp'))
seaf20[, let(year = 2020, body = 'Nanticoke')]


seaford <- rbind(seaf19, seaf20, seaf21, fill = T)

seaford[, doy := yday(dttm)]
seaford[, tempC := (temp - 32) * 5/9]

seaford_summ <- seaford[, .(med_temp = median(tempC)), by = c('doy', 'year', 'body')]


prev <- 's:/nansturg-analysis/manuscript/data_derived/dnrec_wq_aggregated.csv' |> 
  fread()

prev <- prev[station == 'seaford', 2:3]
prev[, let(year = year(date), body = 'Nanticoke', doy = yday(date))]
setnames(prev, c('date', 'med_temp', 'year', 'body', 'doy'))

seaford_summ <- rbind(prev, seaford_summ, fill = T)
seaford_summ[, doy_date := as.Date(doy)]

seaford <- ggplot(data = seaford_summ) + 
  annotate('rect', xmin = as.Date(-Inf), xmax = as.Date(Inf),
           ymin = 28, ymax = Inf, fill = 'red') +
  annotate('rect', xmin = as.Date(-Inf), xmax =as.Date(Inf),
           ymin = 20, ymax = 25, fill = 'green') +
  geom_line(aes(x = doy_date, y = med_temp)) + facet_wrap(~year) +
  labs(x = NULL, y = 'Water temperature (C)') +
  theme_minimal() 


# k <- seaford_summ[between(med_temp, 20, 25)][, l := doy_date-shift(doy_date), by = year]
k_sf <- seaford_summ[between(med_temp, 20, 25)] |> 
  _[doy_date > as.Date(yday('2015-08-15'))] |> 
  _[, .(min = min(doy_date), max = max(doy_date), site = 'seaford'), by = year] |> 
  _[, n_days := max - min]


k <- rbind(k, k_sf)

ggplot(data = k) +
  geom_line(aes(x = year, y = n_days, color =site))


j <- "site	year	start	end	n_days
seaford	2015	2015-09-14	2015-10-01	18
seaford	2016	2016-09-16	2016-09-29	14
seaford	2017	2017-08-27	2017-09-24	30
seaford	2018	2018-09-21	2018-10-13	23
seaford	2019	2019-09-18	2019-10-08	23
seaford	2020	2020-09-14	2020-09-21	8
seaford	2021	2021-09-19	2021-10-18	30
marshyhope	2015	2015-09-14	2015-10-01	18
marshyhope	2016	2016-09-18	2016-09-30	13
marshyhope	2017	2017-09-13	2017-09-27	15
marshyhope	2018	2018-09-11	2018-10-01	21
marshyhope	2019	2019-09-19	2018-10-09	21
marshyhope	2022	2022-09-16	2022-09-29	14" |>
  fread(text = _)

ggplot(data = k) +
  geom_line(aes(x = year, y = yday(max), color = site))

library(gamlss)

k[, st_day := yday(min)]
k[, end_day := yday(max)]
k_mod <- gamlss(st_day ~ year + site, data = k, family = 'IG')


seaford_summ[, yr_fac := as.factor(year)]
library(mgcv)
m <- gam(med_temp ~ yr_fac + s(doy, by = yr_fac), data = seaford_summ[between(doy, 214, 306)])

library(gratia)
derivatives(m, order = 2, eps = 1) |> draw()


names(mdnr)
setnames(mdnr, 'median_temp', 'med_temp')

wq <- rbind(seaford_summ, mdnr, fill = T)
wq[, yr_fac := as.factor(year)]

m <- gam(med_temp ~ s(doy, by = yr_fac, m=4) + yr_fac + body, data = wq,
    method = 'REML')
 
