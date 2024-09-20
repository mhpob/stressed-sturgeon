library(duckdb)
library(sf)
library(dplyr)

con <- dbConnect(duckdb(), ':memory:')

# From the 12th to the 24th of Sept
for ( i in 12:24 ) {
  filename <- paste0(
    'AIS_2023_09_',
    i,
    '.zip'
  )
  dest <- file.path(
    tempdir(),
    filename
  )
  url <- paste0(
    'https://coast.noaa.gov/htdata/CMSP/AISDataHandler/2023/',
    filename
  )
  
  download.file(
    url,
    destfile = dest
  )
  unzip(dest, exdir = tempdir())
  
  
  vessels <- dbGetQuery(
    con,
    paste0(
      "SELECT *
  FROM read_csv('",
      gsub('zip', 'csv', dest),
      "')
  WHERE (LAT > 38.224) AND 
        (LON > -75.9699) AND
        (LAT < 38.6505) AND
        (LON < -75.605)"
    )
  )
  
  write.csv(vessels,
            file.path('Data','AIS', gsub('zip', 'csv', filename)))
  
  file.remove(dest)
  file.remove(gsub('zip', 'csv', dest))
}

dbDisconnect(con)

ais <- dbGetQuery(
  con,
  "SELECT *
    FROM read_csv('Data/AIS/*.csv')"
)

ais <- st_as_sf(
  ais,
  coords = c('LON', 'LAT'),
  crs = 4326
)



nan <- st_read('s:/nansturg-analysis/manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
               layer = 'wbdhu10',
               query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan <- st_read('s:/nansturg-analysis/manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
               layer = 'nhdarea',
               wkt_filter = nan$wkt) |> 
  st_zm(nan)

ais <- st_transform(ais, st_crs(nan))


nanticoke_ais <- st_intersection(
  ais,
  nan
)


nanticoke_ais <- nanticoke_ais |> 
  select(MMSI:TransceiverClass)

# Code reference: https://www.navcen.uscg.gov/sites/default/files/pdf/AIS/AISGuide.pdf
