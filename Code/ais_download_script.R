
library(parallel)
library(sf)


td <- tempdir()
ais_files <- NULL



for(i in 2015:2024){
  download.file(
    file.path(
      'https://coast.noaa.gov/htdata/CMSP/AISDataHandler',
      i,
      'index.html'
    ),
    file.path(td, 'index.txt')
  )
  
  ais_files <- c(ais_files,
                 file.path(td, 'index.txt') |>
                   readLines() |> 
                   grep('AIS_', x = _, value = T) |> 
                   strsplit(split = 'href=\"') |>
                   unlist() |>
                   gsub('^(AIS_.{10}).zip.*$', '\\1', x = _) |>
                   grep('AIS_.{4}_.[890]', x = _, value = TRUE)
  )
}



# for (i in seq_along(ais_files)){
mclapply(ais_files,
         function(x){
           yr <- gsub('AIS_(.{4}).*', '\\1', x)
           
           ais <- read_sf(
             file.path(
               '/vsizip/vsicurl/https://coast.noaa.gov/htdata/CMSP/AISDataHandler',
               yr,
               paste0(x, '.zip'),
               paste0(x,'.csv')
             ),
             query = paste0(
               'select * from ',
               x,
               ' where (CAST(LAT AS float(8)) between 38.215 and 38.655)
        AND (CAST(LON AS float(8)) between -75.969 and -75.578)'
             ),
           )
           
           write.csv(ais, paste0(x, '.csv'), row.names = FALSE)
           print(x)
         }
)
