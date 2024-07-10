library(data.table)
library(sf)

dets <- file.path(
  'S:\\Stressed Sturgeon_2022-2024\\VPS 2023',
  'VPS-NanticokeRiver-Seaford-01-Results-20240515\\results\\animal'
) |> 
  list.files(full.names = T) |> 
  lapply(fread) |> 
  rbindlist() |>
  setorder(Transmitter, Time) |> 
  _[, tdiff := difftime(Time, shift(Time), units = 'mins'), by = Transmitter] |> 
  _[tdiff > 30 | is.na(tdiff),
    track := paste(Id, rleid(tdiff), sep = "-"),
    by = Transmitter] |> 
  _[tdiff > 30 | is.na(tdiff),
    track_id := 1:.N] |> 
  _[, track_id := nafill(track_id, 'locf')] |> 
  _[, let(
    track = unique(track)[!is.na(unique(track))][track_id],
    track_id = NULL
  )] |> 
  
  st_as_sf(
    coords = c('Longitude', 'Latitude'),
    crs = 4326
  )

bare_pts <- dets |>
  data.table() |> 
  _[, if(.N == 1) .SD, by = 'track']
tracks_pts <- dets |>
  data.table() |> 
  _[, if(.N > 1) .SD, by = 'track']
tracks <- tracks_pts |> 
  _[, .(geometry = st_combine(geometry)), by = 'track'] |> 
  _[, geometry := st_cast(geometry, 'LINESTRING')]

seaford_shoal <- '/vsizip/Data/MD_37_37S_20170323_CS.zip/MD_37_37S_20170323_CS.gdb' |> 
  st_read(layer = 'ShoalPolygon')
seaford_bathy <- '/vsizip/Data/MD_37_37S_20170323_CS.zip/MD_37_37S_20170323_CS.gdb' |> 
  st_read(layer = 'Bathymetry_Vector')

library(leaflet)
library(leaflet.extras2)
tracks_sf <- st_as_sf(tracks)
tracks_pts_sf <- st_as_sf(tracks_pts)
bare_pts_sf <- st_as_sf(bare_pts)

bathy_pal <- leaflet::colorNumeric('plasma', seaford_bathy$depthMean)


# map <- 
# leaflet() |> 
#   setView(
#     lng = -75.617847,
#     lat = 38.63198,
#     zoom = 16
#   ) |> 
#   addProviderTiles('Esri.WorldImagery', group = 'base')  |>
#   addPolygons(data = st_transform(seaford_bathy, 4326),
#               fillColor = ~ bathy_pal(depthMean),
#               color = ~bathy_pal(depthMean),
#               fillOpacity = 1) |>
#   addPolygons(data = st_transform(seaford_shoal, 4326),
#               fillColor = 'white',
#               color = 'red',
#               fillOpacity = 0.5) |> 
#   addLegend(data = st_transform(seaford_bathy, 4326),
#             pal = bathy_pal, values = ~depthMean) |> 
#   addCircles(data = dets,
#              radius = 3,
#              color = NA,
#              fillOpacity = 0.5,
#              fillColor = 'white')


{
  map <- leaflet() |> 
    setView(
      lng = -75.617847,
      lat = 38.63198,
      zoom = 16
    ) |> 
    addProviderTiles('Esri.WorldImagery', group = 'base')  |>
    addPolygons(data = st_transform(seaford_bathy, 4326),
                fillColor = ~ bathy_pal(depthMean),
                color = ~bathy_pal(depthMean),
                fillOpacity = 1) |>
    addPolygons(data = st_transform(seaford_shoal, 4326),
                fillColor = 'white',
                color = 'red',
                fillOpacity = 0.5) |> 
    addLegend(data = st_transform(seaford_bathy, 4326),
              pal = bathy_pal, values = ~depthMean)
  
  for (i in 1:nrow(tracks_sf)){
    temp_dat <- tracks_pts_sf[tracks_pts_sf$track == tracks_sf$track[i],]
    
    map <- map |> 
      # addPolylines(data = tracks_sf[i,], group = ~ track)
      addAntpath(data = tracks_sf[i,],
                 group = ~ track,
                 options = antpathOptions(delay = 5000)) |> 
      addCircleMarkers(
        data = temp_dat,
        group = ~track,
        label = paste(
          'Transmitter:', temp_dat$FullId, '<br>',
          'Time:', temp_dat$Time
        ) |> 
          lapply(htmltools::HTML)
      )
  }
  
  for (i in 1:nrow(bare_pts_sf)) {
    map <- map |> 
      addCircleMarkers(
        data = bare_pts_sf[i,],
        group = ~track,
        label = paste(
          'Transmitter:', bare_pts_sf[i,]$FullId, '<br>',
          'Time:', bare_pts_sf[i,]$Time
        ) |> 
          htmltools::HTML()
      )
  }
  
  map |>
    addLayersControl(overlayGroups = unique(dets$track)) |> 
    hideGroup(group = unique(dets$track))
  
}
