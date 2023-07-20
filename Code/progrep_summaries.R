library(data.table)

new_recs <- lapply(
  list.files('data/detections/umces', full.names = T, pattern = '\\.csv$'),
  fread, fill = T, col.names = function(.) tolower(gsub('[ \\(\\)]', '', .))
)

new_recs <- rbindlist(new_recs)


library(ggplot2)
new_recs[, day := as.Date(dateandtimeutc)]
ggplot(data = new_recs[grepl('A69-1601', transmitter) & day %between% c('2022-08-11', '2022-10-31'), .N, by = c('transmitter', 'day', 'receiver')]) +
  geom_line(aes(x = day, y = N)) +
  facet_grid(transmitter ~ receiver)

library(readxl)
deployment <- list.files('data', pattern = 'otn_metadata', full.names = T)
deployment <- read_xlsx(deployment, sheet = 2, skip = 3)
setDT(deployment)
setnames(deployment, function(.) tolower(gsub(' (.*)', '', .)))         
deployment[, ':='(deploy_date_time = as.POSIXct(deploy_date_time,
                                                format = '%Y-%m-%dT%H:%M:%S',
                                                tz = 'UTC'),
                  recover_date_time = as.POSIXct(recover_date_time,
                                                 format = '%Y-%m-%dT%H:%M:%S',
                                                 tz = 'UTC'))]

deployment[, receiver := paste(ins_model_no, ins_serial_no, sep = '-')]

setkey(deployment, receiver, deploy_date_time, recover_date_time)
new_recs[, dummy_date := dateandtimeutc]
setkey(new_recs, receiver, dateandtimeutc, dummy_date)

dets <- foverlaps(deployment, new_recs)
dets[, ':='(stationname = station_no,
            latitude = deploy_lat,
            longitude = deploy_long)]
anims <- dets[!grepl('1601-609', transmitter) & !is.na(transmitter),
              1:12]

ggplot(data = dets) +
  geom_line(aes(x = dateandtimeutc ))


## DNR receivers
new_sites <- read_excel('data/stressed_sturgeon_otn_metadata_deployment.xlsx',
                        sheet = 2, skip = 3)

new_sites <- st_as_sf(new_sites,
                      coords = c('DEPLOY_LONG', 'DEPLOY_LAT'),
                      crs = st_crs(4326))

mdnr2021 <- read.csv('data/mdnr_2021receivers.csv') |> 
  st_as_sf(coords = c('Longitude', 'Latitude'),
           crs = 4326)


dummy <- data.frame(site = c('Idwild Rd', 'Deep Hole', 'North Federalsburg',
                             'VFW', 'Federalsburg'),
                    long = c(-75.77332, -75.77426, -75.77364, -75.77431, -75.77214),
                    lat = c(38.70683,  38.70406,  38.70108, 38.67868,38.69393)) |> 
  st_as_sf(coords = c('long', 'lat'),
           crs = 4326)

dnr <- list.files('data/detections/dnr', full.names = T)

dnr <- lapply(dnr, read_xlsx)
dnr <- lapply(dnr, setDT)
dnr <- rbindlist(dnr)
setnames(dnr, function(.) tolower(gsub('[ \\(\\)]', '', .)))

dnr[, latitude := as.numeric(latitude)]

dnr[!grepl('1601-609', transmitter), .N, stationname][grepl('9001-', transmitter), .N, by = c('stationname', 'latitude', 'longitude', 'transmitter')]

library(sf)
fbg_det <- rbind(dnr[!grepl('1601-609', transmitter)], anims)
fbg_det_N <- fbg_det[grepl('9001-', transmitter), .N, by = c('stationname', 'latitude', 'longitude', 'transmitter')]  |> 
  st_as_sf(coords = c('longitude', 'latitude'),
           crs = 4326)

ggplot(data = fbg_det_N) +
  geom_sf(aes(size = N)) +
  facet_wrap(~transmitter)

# A69-9001-18980	1844	1720		M	MDNR
# A69-9001-18983	1750	1585		M	MDNR
# A69-9001-27543	1918	1692		M	MDNR

fburg <- st_read('data/Maryland_Property_Data_-_Tax_Map_Grids',
                 query = "SELECT * FROM \"Maryland_Property_Data_-_Tax_Map_Grids\" WHERE JURSCODE = 'CARO'") %>%
  st_transform(4326) %>%
  st_union()
nan <- st_read('data/NHD_H_0208_HU4_GDB.gdb',
               query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan <- st_read('data/NHD_H_0208_HU4_GDB.gdb',
               layer = 'nhdarea',
               wkt_filter = nan$wkt)

dnr_plot <- dnr[!grepl('1601-609', transmitter) &grepl('9001-', transmitter), .N, by = c('stationname', 'latitude', 'longitude', 'transmitter')] |> 
  st_as_sf(coords = c('longitude', 'latitude'),
           crs = 4326)
umces_plot <-  anims[!grepl('1601-609', transmitter) &grepl('9001-', transmitter), .N, by = c('stationname', 'latitude', 'longitude', 'transmitter')] |> 
  st_as_sf(coords = c('longitude', 'latitude'),
           crs = 4326)

ggplot() +
  geom_sf(data = fburg, fill = 'yellow') +
  geom_sf(data = nan) +
  geom_sf(data = new_sites, size = 0, color = 'red') +
  geom_sf(data = mdnr2021, size = 0) +
  geom_sf(data = dummy, size = 0)+
  geom_sf(data = umces_plot, aes(size = N), color = 'red') +
  geom_sf(data = dnr_plot, aes(size = N)) +

    facet_wrap(~transmitter) +
  annotate('point', x = -75.76966, y = 38.68742,
           shape = 'triangle down filled',
           size = 5) +
  annotate('point', x = -75.76219, y = 38.68911, shape = 'cross',
           size = 5) +
  coord_sf(label_axes = '--EN',
           xlim = c(-75.785, -75.75), ylim = c(38.679, 38.706)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = c(0.95, 0.75))
