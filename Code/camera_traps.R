library(data.table)
library(readxl)

trap_base <- 's:/Stressed Sturgeon_2022-2024/Camera traps'
trap_import <- function(up_down){
  xl_file <- read_excel(
    file.path(trap_base, 
              paste0(up_down, ' nanticoke 11-9-23'),
              paste0('nan_',
                     gsub(' ', '', up_down),
                     '_camera_trap_data.xlsx')
    ),
    sheet = 1)
  
  setDT(xl_file)
  xl_file[, c('st_frame', 'end_frame') :=
            lapply(.SD, function(.) sprintf('%05d', .)),
          .SDcols = c('st_frame', 'end_frame')]
  
  xl_file[, st_time := file.mtime(
    sprintf('s:/Stressed Sturgeon_2022-2024/Camera traps/%s Nanticoke 11-9-23/PHOTO/IM_%s.jpg', up_down, st_frame)
  )]
  xl_file[, end_time := file.mtime(
    sprintf('s:/Stressed Sturgeon_2022-2024/Camera traps/%s Nanticoke 11-9-23/PHOTO/IM_%s.jpg', up_down, end_frame)
  )]
  
  xl_file[, end_time := fifelse(is.na(end_time), st_time, end_time)]
  
  setattr(xl_file$st_time, 'tzone', 'America/New_York')
  setattr(xl_file$end_time, 'tzone', 'America/New_York')
  
  xl_file[, location := up_down]
}

## Upriver trap
### 1st
ur1 <- read_excel('s:/Stressed Sturgeon_2022-2024/Camera traps/Up River Nanticoke 9-5-23/camera_trap_data.xlsx')
setDT(ur1)

ur1[, c('st_frame', 'end_frame') :=
          lapply(.SD, function(.) sprintf('%05d', .)),
        .SDcols = c('st_frame', 'end_frame')]

ur1[, st_time := file.mtime(
  sprintf('s:/Stressed Sturgeon_2022-2024/Camera traps/Up River Nanticoke 9-5-23/PHOTO/IM_%s.jpg', st_frame)
)]
ur1[, end_time := file.mtime(
  sprintf('s:/Stressed Sturgeon_2022-2024/Camera traps/Up River Nanticoke 9-5-23/PHOTO/IM_%s.jpg', end_frame)
)]

ur1[, end_time := fifelse(is.na(end_time), st_time, end_time)]

ur1[, location := 'up river']

ur1[st_frame == 13681, st_time := as.POSIXct('2023-08-27 13:01:22', tz = 'America/New_York')]
ur1[end_frame == 13681, end_time := as.POSIXct('2023-08-27 13:01:22', tz = 'America/New_York')]


### 2nd
upriver <- trap_import('up river') 

upriver[, st_hr := as.POSIXct(trunc(st_time, 'hours'))]

k <- upriver[, .N, by = st_hr]
plot(N ~st_hr, data = k)


## Downriver trap
### 1st
dr1 <- read_excel('s:/Stressed Sturgeon_2022-2024/Camera traps/Down River Nanticoke 9-5-23/camera_trap_data_dr_nan.xlsx')
setDT(dr1)
dr1 <- dr1[st_frame != 'NA']

dr1[, c('st_frame', 'end_frame') :=
      lapply(.SD, function(.) sprintf('%05d', .)),
    .SDcols = c('st_frame', 'end_frame')]

dr1[, st_time := file.mtime(
  sprintf('s:/Stressed Sturgeon_2022-2024/Camera traps/Down River Nanticoke 9-5-23/PHOTO/IM_%s.jpg', st_frame)
)]
dr1[, end_time := file.mtime(
  sprintf('s:/Stressed Sturgeon_2022-2024/Camera traps/Down River Nanticoke 9-5-23/PHOTO/IM_%s.jpg', end_frame)
)]

dr1[, end_time := fifelse(is.na(end_time), st_time, end_time)]

dr1[, location := 'down river']


### 2nd
downriver <- trap_import('down river')
downriver <- downriver[!grepl('\\s+NA', st_frame)]

downriver[, st_hr := as.POSIXct(trunc(st_time, 'hours'))]

k <- downriver[, .N, by = st_hr]
plot(N ~st_hr, data = k)


## Both
trap_data <- rbind(upriver, downriver)
trap_data[, st_hr := as.POSIXct(trunc(st_time, 'hours'))]
trap_data[, st_20m := lubridate::floor_date(st_time, '20 minutes')]

## looking through data, when there are paired detections the upriver camera is often 22seconds behind upriver
trap_data[, st_m := lubridate::floor_date(st_time, 'minute')]

trap_hourly <- trap_data[, .N, by = .(st_hr, location)]

library(ggplot2)
ggplot(data = trap_hourly) +
  geom_line(aes(x = st_hr, y = N)) +
  facet_wrap(~location, ncol = 1)

ggplot(data = trap_data) +
  geom_rect(aes(xmin = st_time, xmax = end_time, ymin=0, ymax=1,
                fill = type),
            alpha = 0.5) +
  facet_wrap(~location, ncol = 1)


hrly <- dcast(trap_data[, .(st_hr, location)], st_hr ~ location)

lm(`up river` ~ `down river`, data = hrly) |> summary()
cor.test(x = hrly$`down river`, y=hrly$`up river`)
hrly_plot <- ggplot(hrly) + 
  geom_jitter(aes(x = `down river`, y = `up river`),
              width=0.2, height = 0.2) +
  geom_smooth(aes(x = `down river`, y = `up river`),
              method = 'lm',
              formula = y~x)+
  geom_abline(slope = 1) +
  coord_fixed()


m20 <- dcast(trap_data, st_20m~ location)
lm(`up river` ~ `down river`, data = m20) |> summary()
cor.test(m20$`down river`, m20$`up river`)


m20_plot <- ggplot(m20) + 
  geom_jitter(aes(x = `down river`, y = `up river`),
              width = 0.2, height = 0.2) +
  geom_smooth(aes(x = `down river`, y = `up river`),
              method = 'lm',
              formula = y~x)+
  geom_abline(slope = 1) +
  coord_fixed()

m <- dcast(trap_data, st_m~ location)
lm(`up river` ~ `down river`, data = m) |> summary()
cor.test(m$`down river`, m$`up river`)


m_plot <- ggplot(m) + 
  geom_jitter(aes(x = `down river`, y = `up river`),
              width = 0.2, height = 0.2) +
  geom_smooth(aes(x = `down river`, y = `up river`),
              method = 'lm',
              formula = y~x)+
  geom_abline(slope = 1) +
  coord_fixed()

library(patchwork)
hrly_plot / m20_plot / m2_plot

## pres/abs confusion matrix?

## sturgeon
dets <- list.files('s:/Stressed Sturgeon_2022-2024/Nanticoke Vemco data 11-9-23/',
                   pattern = '\\.csv', full.names = T) |>
  lapply(fread,
         fill = TRUE) |> 
  rbindlist()

dets <- dets[grepl('9001', Transmitter)]

ggplot() +
  
  geom_vline(data = dets[grepl('9001', Transmitter)],
             aes(xintercept = `Date and Time (UTC)`),
             color = 'lightgray') +
  # geom_rect(data = trap_data,
  #           aes(xmin = st_time, xmax = end_time, ymin=0, ymax=1,
  #               fill = type),
  #           alpha = 0.5) +
  geom_rug(data = trap_data, aes(x = st_time)) +
  facet_wrap(~location, ncol = 1) +
  theme_minimal()







###
# upriver, first tending
imgs <- list.files('s:/Stressed Sturgeon_2022-2024/Camera traps/Up River Nanticoke 9-5-23/PHOTO',
                   full.names = T)

image_time <- numeric(length(imgs))
image_time <- as.POSIXct(image_time, origin = '1970-01-01 00:00:00')

for(i in seq_along(imgs)){
  image_time[i] <- file.mtime(imgs[i])
}

imgs <- data.frame(file_path = imgs,
                   file_time = image_time,
                   image_time = image_time)

# qs::qsave(imgs, '2023-08-08_09-05.qs')


# upriver, second tending
imgs <- list.files('s:/Stressed Sturgeon_2022-2024/Camera traps/Up River Nanticoke 11-9-23/PHOTO',
                   full.names = T)

image_time <- numeric(length(imgs))
image_time <- as.POSIXct(image_time, origin = '1970-01-01 00:00:00')

for(i in seq_along(imgs)){
  image_time[i] <- file.mtime(imgs[i])
}

imgs <- data.frame(file_path = imgs,
                   file_time = image_time,
                   image_time = image_time - 60*60)

# qs::qsave(imgs, 'data/2023 camera traps/ur_2023-09-05_09-27.qs')




# downriver, first tending
imgs <- list.files('s:/Stressed Sturgeon_2022-2024/Camera traps/Down River Nanticoke 9-5-23/PHOTO',
                   full.names = T)

image_time <- numeric(length(imgs))
image_time <- as.POSIXct(image_time, origin = '1970-01-01 00:00:00')

for(i in seq_along(imgs)){
  image_time[i] <- file.mtime(imgs[i])
}

imgs <- data.frame(file_path = imgs,
                   file_time = image_time,
                   image_time = image_time)

# qs::qsave(imgs, 'dr_2023-08-08_09-05.qs')


# downriver, second tending
imgs <- list.files('s:/Stressed Sturgeon_2022-2024/Camera traps/Down River Nanticoke 11-9-23/PHOTO',
                   full.names = T)

image_time <- numeric(length(imgs))
image_time <- as.POSIXct(image_time, origin = '1970-01-01 00:00:00')

for(i in seq_along(imgs)){
  image_time[i] <- file.mtime(imgs[i])
}

imgs <- data.frame(file_path = imgs,
                   file_time = image_time,
                   image_time = image_time - 60*60)

# qs::qsave(imgs, 'dr_2023-09-05_09-27.qs')

# dr1 <- qs::qread('data/2023 camera traps/dr_2023-08-08_09-05.qs')
# dr2 <- qs::qread('data/2023 camera traps/dr_2023-09-05_09-27.qs')
# ur1 <- qs::qread('data/2023 camera traps/ur_2023-08-08_09-05.qs')
# ur2 <- qs::qread('data/2023 camera traps/ur_2023-09-05_09-27.qs')
# 
# all_img_metadata <- Reduce(function(...) merge(..., all = T), list(dr1, dr2, ur1, ur2))
# 
# qs::qsave(all_img_metadata, 'data/2023 camera traps/all_image_metadata.qs')

img_metadata <- qs::qread('data/2023 camera traps/all_image_metadata.qs')

setDT(img_metadata)
img_metadata[grepl('_13681\\.', file_path)
             & grepl('Up River Nanticoke 9-5-23', file_path),
             ':='(file_time = as.POSIXct('2023-08-27 13:01:22', tz = 'America/New_York'),
                  image_time = as.POSIXct('2023-08-27 13:01:22', tz = 'America/New_York'))]
img_metadata[, frame := gsub('\\.JPG$|^.*_', '', file_path)]
img_metadata[, deployment := tstrsplit(file_path, '/', keep = 4)]
img_metadata[, river_section := gsub(' River.*', '', deployment)]

img_metadata[, deployment := gsub('^.*e ', '', deployment)]

library(ggplot2)
ggplot() +
  geom_line(data = img_metadata,
            aes(x = file_time, y = river_section, color = deployment),
            linewidth = 10) +
  geom_vline(data = dets, aes(xintercept = `Date and Time (UTC)`), color = 'red') +
  annotate('rect',
           xmin = as.POSIXct(
             c('2023-08-08 13:01:24',
               '2023-09-12 00:01:43'), 
               tz = 'America/New_York'),
           xmax = as.POSIXct(
             c('2023-08-28 23:59:24',
               '2023-09-25 07:39:34'),
             tz = 'America/New_York'),
           ymin = 1.75, ymax = 2.25,
           fill = NA, color = 'green', linewidth = 3) +
  annotate('rect',
           xmin = as.POSIXct(
             c('2023-08-08 12:30:18',
               '2023-09-12 14:01:22'), 
             tz = 'America/New_York'),
           xmax = as.POSIXct(
             c('2023-08-11 09:50:40',
               '2023-09-25 05:07:22'),
             tz = 'America/New_York'),
           ymin = 0.75, ymax = 1.25,
           fill = NA, color = 'green', linewidth = 3) +
  geom_rect(data = ur1,
            aes(xmin = st_time, xmax = end_time, ymin = 1.75, ymax = 2.25)) +
  geom_rect(data = upriver,
            aes(xmin = st_time, xmax = end_time, ymin = 1.75, ymax = 2.25)) +
  geom_rect(data = dr1,
            aes(xmin = st_time, xmax = end_time, ymin = 0.75, ymax = 1.25)) +
  geom_rect(data = downriver,
            aes(xmin = st_time, xmax = end_time, ymin = 0.75, ymax = 1.25)) +
  labs(x = NULL, y = 'Array Section', color = 'Deployment Period') +
  theme_minimal()

## Audits as of 2024-01-25
# UR 1st deployment audit complete
# 1
# 2023-08-08 13:01:24 EDT
# 14730
# 2023-08-28 23:59:24

# Upriver second deployment audit complete
# 4652
# 2023-09-12T00:01:43-4
# 
# 2023-09-25T07:39:34-4
# 14241

# DR  1st deployment audit complete 
# 1
# 2023-08-08 12:30:18
# 2081
# 2023-08-11 09:50:40

# DR  2nd deployment audit complete 
# 4940
# 2023-09-12 14:01:22
# 13984 
# 2023-09-25 05:07:22


setkey(upriver, st_time, end_time)

dets[, dt_local := `Date and Time (UTC)`]
setattr(dets$dt_local, 'tzone', 'America/New_York')
dets[, dummy := dt_local]
setkey(dets, dt_local, dummy)
foverlaps(dets, upriver, nomatch = 0) |> nrow()
foverlaps(dets, upriver[!grepl('tug', type)], nomatch = 0) |> nrow()



upriver[, end_pl2 := end_time + 2*60]
upriver[, start_pl2 := st_time - 2*60]
setkey(upriver, start_pl2, end_pl2)
foverlaps(dets, upriver, nomatch = 0) |>
  nrow()
foverlaps(dets, upriver[grepl('tug', type)], nomatch = 0) |>
  View()
foverlaps(dets, upriver[!grepl('tug', type)], nomatch = 0) |>
  nrow()

dr_trim <- downriver[!is.na(st_time) & !is.na(end_time)]
setkey(dr_trim, st_time, end_time)
foverlaps(dets, dr_trim, nomatch = 0) |> nrow()
foverlaps(dets, dr_trim[grepl('dock', type) & grepl('tug', type)], nomatch = 0) |> nrow()
foverlaps(dets, dr_trim[!grepl('tug', type)], nomatch = 0) |> nrow()

dr_trim[, end_pl2 := end_time + 2*60]
dr_trim[, start_pl2 := st_time - 2*60]
setkey(dr_trim, start_pl2, end_pl2)
foverlaps(dets, dr_trim, nomatch = 0) |>
  nrow()
foverlaps(dets, dr_trim[grepl('tug', type) & grepl('dock', type)], nomatch = 0) |>
  nrow()
foverlaps(dets, dr_trim[!grepl('tug', type)], nomatch = 0) |>
  nrow()
