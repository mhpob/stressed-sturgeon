get_tides <- function(station = 8571858, begin_date, end_date){
  url <- paste0('https://tidesandcurrents.noaa.gov/api/datagetter?product=predictions&application=NOS.COOPS.TAC.WL&begin_date=',
                begin_date,
                '&end_date=',
                end_date,
                '&datum=MLLW&station=',
                station,
                '&time_zone=lst_ldt&units=english&interval=hilo&format=csv')
  fread(url)
  
  # Sharptown: 8571858
}
