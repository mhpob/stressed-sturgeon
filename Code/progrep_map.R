library(sf)

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

library(ggplot2)
ggplot() +
  geom_sf(data = fburg) +
  geom_sf(data = nan) +
  coord_sf(label_axes = '--EN',
         xlim = c(-75.8, -75.74), ylim = c(38.67, 38.71))


library(readxl)
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

library(ragg)
agg_png(width = 500, height = 475, scaling = 1.75)
ggplot() +
  geom_sf(data = fburg, fill = 'yellow') +
  geom_sf(data = nan) +
  geom_sf(data = new_sites, color = 'red') +
  geom_sf(data = mdnr2021) +
  geom_sf(data=dummy)+
  annotate('point', x = -75.76966, y = 38.68742,
           shape = 'triangle down filled',
           size = 5) +
  annotate('point', x = -75.76219, y = 38.68911, shape = 'cross',
           size = 5) +
  coord_sf(label_axes = '--EN',
           xlim = c(-75.785, -75.75), ylim = c(38.679, 38.706)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()
