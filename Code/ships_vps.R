library(data.table)

ships <- fread('data/2023 camera traps/trap_data.csv')

dets <- file.path(
  'S:\\Stressed Sturgeon_2022-2024\\VPS 2023',
  'VPS-NanticokeRiver-Seaford-01-Results-20240515\\results\\animal'
) |> 
  list.files(full.names = T) |> 
  lapply(fread) |> 
  rbindlist()

# prep for conservative foverlaps
dets[, time_dummy := Time]
setkey(dets, Time, time_dummy)

setkey(ships, st_time, end_time)

cons_ovr <- foverlaps(dets, ships,
                          nomatch = 0)
cons_ovr[, .N, type]

# prep for max foverlaps
ships[, let(
  st_max = st_time - 2*60,
  end_max = end_time + 2*60
)]
setkey(ships, st_max, end_max)

max_ovr <- foverlaps(dets, ships,
                          nomatch = 0)
max_ovr[, .N, type]


####
setorder(max_ovr, location, st_frame)

max_ovr[, event:= 1:.N]
kk <- max_ovr[, .SD[, .(frame = seq(st_frame, end_frame),
              location)], .I] |> 
  _[max_ovr, , on = c(I='event', 'location')]


library(sf)
cam <- st_read('s:/Stressed Sturgeon_2022-2024/VPS_Camera_deploym_08-AUG-23.gpx',
          layer = 'waypoints') |> 
  _[3:4,] |>
  st_transform('+proj=aeqd +lat_0=38.63198 +lon_0=-75.617847 +x_0=800 +y_0=800 +datum=WGS84 +units=m')

ship_loc <- st_read('s:/Stressed Sturgeon_2022-2024/Camera traps/ship locations.gpkg')

mapview::mapview(ship_loc, zcol = 'camera')
setDT(ship_loc)

k <- kk[ship_loc, , on = c('frame', location = 'camera'), nomatch = 0] |> 
  unique(by = c('I', 'frame', 'Transmitter', 'Time'))

k <- cbind(k, k[, .SD[, .(k=st_sfc(st_point(c(Longitude, Latitude)), crs = 4326))], .I][, I:=NULL])
dists <- data.frame(
  dists = st_distance(k$geometry, k$k, by_element = T) |> 
  as.numeric() 
)

library(ggplot2)
a <- ggplot(data = dists) +
  geom_histogram(aes(x = dists), binwidth = 20) +
  labs(x = 'Distance between ship and sturgeon (m)', y = 'Number of possible interactions') +
  theme_minimal()

mapview::mapview(st_as_sf(k, sf_column_name = 'geometry'), zcol = 'location') +
  mapview::mapview(st_as_sf(k, sf_column_name = 'k'))

k[, line := st_union(geometry, k, by_feature = T) |> st_cast('LINESTRING')]

mapview::mapview(st_as_sf(k, sf_column_name = 'geometry'), zcol = 'location') +
  mapview::mapview(st_as_sf(k, sf_column_name = 'k')) +
  mapview::mapview(st_as_sf(k, sf_column_name = 'line'))


b <- ggplot() +
  geom_sf(data = st_as_sf(k, sf_column_name = 'line'), alpha = .5) +
  geom_sf(data = st_as_sf(k, sf_column_name = 'k'), color = 'red') +
  geom_sf(data = st_as_sf(k, sf_column_name = 'geometry'),
          aes(color = location)) +
  labs(color = 'Camera location') +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.9))


library(patchwork)
a+b
