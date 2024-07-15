library(dplyr); library(sf)

# Read and manipulate flowline data ---
flowline <- st_read('s:/nansturg-analysis/manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
                    layer = 'NHDFlowline',
                    query = "select *
                    from NHDFlowline
                    where (ReachCode like '02080109%')
                    and ((GNIS_Name like '%Nanticoke%') or
                    (GNIS_Name like 'Marshyhope Creek') or
                    (GNIS_Name like 'Broad Creek') or
                    (GNIS_Name like 'Deep Creek'))")

names(flowline) <- tolower(names(flowline))
st_geometry(flowline) <- 'shape'


##  Merge different within-river secctions into one.
flowline <- flowline %>%
  st_zm() %>%

  # Combine different sections into one object
  group_by(gnis_name) %>%
  summarize(shape = st_combine(shape), .groups = 'keep') %>%

  # Merge within-object lines together
  summarize(shape = st_line_merge(shape)) %>%
  st_transform(32618)



# Calculate RKM of Creek mouths ----
## Find where the creeks meet the mainstem
mouths <- flowline %>%
  st_intersection() %>%
  filter(n.overlaps == 2)

## Find RKM of Creek mouths
nan_split <- flowline %>%
  filter(gnis_name == 'Nanticoke River') %>%

  # Split Nanticoke by mouth locations
  lwgeom::st_split(mouths) %>%

  # Pull out the split sections
  st_collection_extract('LINESTRING') %>%

  # Find their lengths in km
  st_length() %>%
  units::set_units(km)

## Return RKMs
mouths <- data.frame(body = c('Marshyhope Creek', 'Broad Creek', 'Deep Creek'),
                     rkm_nan_mouth = as.numeric(cumsum(nan_split)[1:3]))



# Sites ----
##DNREC
dnrec <- read.csv('manuscript/data/detections/past receiver locations.csv') %>%
  arrange(body) %>%
  st_as_sf(coords = c('long', 'lat'),
           remove = F,
           crs = 4326) %>%
  st_transform(32618)


# Break the line that makes up the flowline into points 1 m apart from each other ----
flowline_pts <- flowline %>%
  st_segmentize(1) %>%
  st_cast('MULTIPOINT')


pts <- lapply(flowline_pts$gnis_name, function(.){
  st_nearest_points(flowline_pts[flowline_pts$gnis_name == .,],
                               dnrec[dnrec$body == .,])
})
pts <- lapply(pts, st_as_sf)

pts <- bind_rows(pts)

st_geometry(dnrec) <- st_geometry(pts)


# Cast flowline from MULTIPOINT into simplified LINESTRING ---
flowline_simp <- st_cast(flowline_pts, 'LINESTRING')

dnrec$rkm_body_mouth <- NA


for(i in 1:nrow(dnrec)){
  # Split flowline in half according to location of receiver
  flowline_split <- flowline_simp %>%
    filter(gnis_name == dnrec[i,]$body) %>%
    lwgeom::st_split(dnrec[i,]) %>%
    st_collection_extract('LINESTRING')

  # Find the length of the flowline between locations
  lengths <- flowline_split %>%
    st_length() %>%

    # convert to KM
    units::set_units(km) %>%
    # Choose the down-river section (flowlines are measured from up- to down-river)
    .[[2]]

  dnrec[i,]$rkm_body_mouth <- lengths
}


dnrec <- dnrec %>%
  left_join(mouths) %>%
  mutate(rkm_nan_mouth = ifelse(is.na(rkm_nan_mouth), 0, rkm_nan_mouth),
         rkm_gross = rkm_body_mouth + rkm_nan_mouth,
         error_m = as.numeric(st_length(.)),
         rkm_nan_mouth = NULL) %>%
  data.frame %>%
  select(-geometry)

write.csv(dnrec, 'manuscript/dnrec_rkm.csv', row.names = F)


## MDNR
dets_old <- data.table::fread('data/detections/old/sturgeon_detections.gz')
dets <- data.table::fread('data/detections/sturgeon_22_23.gz')
dets <- dets[, .(date.utc = dateandtimeutc,
                             date.local = dateandtimeutc,
                             transmitter,
                             station = stationname,
                             receiver,
                             lat = latitude,
                             long = longitude)]
data.table::setattr(dets$date.local, 'tzone', 'America/New_York')

dets <- rbind(dets_old, dets)
# Remove non-nanticoke stations
dets <- dets[!grepl('Pocomoke|Shelltown', station)]

mdnr <- dets %>%
  tibble() %>%
  distinct(station, lat, long) %>%
  arrange(station) %>%
  st_as_sf(coords = c('long', 'lat'),
           remove = F,
           crs = 4326) %>%
  st_transform(32618)


mdnr <-  st_nearest_points(flowline_pts,
                           mdnr) %>%
  st_as_sf(station = rep(mdnr$station, times = nrow(flowline_pts)),
           body = rep(flowline_pts$gnis_name, each = nrow(mdnr)),
           lat = rep(mdnr$lat, times = nrow(flowline_pts)),
           long = rep(mdnr$long, times = nrow(flowline_pts)),
           error_m = st_length(.)) %>%
  group_by(station) %>%
  slice(which.min(error_m)) %>%
  ungroup()

mdnr$rkm_body_mouth <- units::set_units(1, 'km')

for(i in 1:nrow(mdnr)){
  # Split flowline in half according to location of receiver
  flowline_split <- flowline_simp %>%
    filter(gnis_name == mdnr[i,]$body) %>%
    lwgeom::st_split(mdnr[i,]) %>%
    st_collection_extract('LINESTRING')

  # Find the length of the flowline between locations
  lengths <- flowline_split %>%
    st_length() %>%

    # convert to KM
    units::set_units(km) %>%
    # Choose the down-river section (flowlines are measured from up- to down-river)
    .[[2]]

  mdnr[i,]$rkm_body_mouth <- lengths
}

mdnr <- mdnr %>%
  left_join(mouths) %>%
  mutate(rkm_nan_mouth = ifelse(is.na(rkm_nan_mouth), 0, rkm_nan_mouth),
         rkm_gross = as.numeric(rkm_body_mouth) + rkm_nan_mouth) %>%
  tibble() %>%
  select(-x, -rkm_nan_mouth)

write.csv(mdnr, 'receiver_rkm.csv', row.names = F)
