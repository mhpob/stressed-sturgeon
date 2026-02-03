library(sf)
ne <- '/vsizip/c:/users/darpa2/downloads/ne_110m_land.zip/ne_110m_land.shp' |>
  st_read() |>
  st_coordinates()
x_mecca <- lambda0 <- 39.8173
y_mecca <- phi1 <- 21.4241

# pg 226 https://pubs.usgs.gov/pp/1453/report.pdf
#phi - north latitude (if latitude is south, a minus sign is used) 
#phi1 - single standard parallel on cylindrical or conic projections;
#       latitude of central point on azimuthal projections. Its choice normally affects the
#       appearance of the projection. 
#lambda - longitude east of Greenwich (for longitude west of Greenwich, a minus sign is used) 
#lambda0 - longitude east of Greenwich of the central meridian of the map or of the origin of
#         the rectangular coordinates (for west longitude,a minus sign is used).
#         If 1/>1 is a pole, >-.0 is the longitude of the meridian extending down
#         on the map from the North Pole or up from the South Pole. On an
#         interrupted projection, )..0 is the central meridian of each section

craig_fwd <- function(coords, lambda0, phi1) {
  R <- 6378.1
  lambda <- coords[,1]
  phi <- coords[,2]
  
  lambda_diff <- lambda - lambda0
  
  x <- ifelse(
    lambda_diff == 0,
    0,
    R * (lambda_diff)
  )
  y <- ifelse(
    lambda_diff == 0,
    R * (sin(phi * pi/180) - cos(phi * pi/180)*tan(phi1 * pi/180)),
    R * lambda_diff * 
      (sin(phi * pi/180)*cos(lambda_diff * pi/180) - 
         cos(phi * pi/180) * tan(phi1 * pi/180)) /
      sin(lambda_diff * pi/180)
  )
  
}


plot(x[abs(y) < 1e6], y[abs(y) < 1e6], ylim = c(-1e6, 1e6), type = 'l')
