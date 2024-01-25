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
  
  xl_file[, location := up_down]
}

## Upriver trap
upriver <- trap_import('up river') 

upriver[, st_hr := as.POSIXct(trunc(st_time, 'hours'))]

k <- upriver[, .N, by = st_hr]
plot(N ~st_hr, data = k)


## Downriver trap
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

dets[grepl('9001', Transmitter)]

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

# qs::qsave(imgs, '2023-09-05_09-27.qs')




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
