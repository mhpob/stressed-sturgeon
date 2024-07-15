library(data.table)
library(duckdb)

con <- dbConnect(
  duckdb(),
  ':memory:',
  read_only = TRUE
)

# dets <- dbGetQuery(
#   con,
#   "SELECT transmitter, count(transmitter) AS N
#   FROM read_csv('data/detections/sturgeon_22_23.gz')
#   WHERE transmitter
#   SIMILAR TO '.*(6275|21066|18010|21065)$'
#   GROUP BY transmitter"
# )

dets <- dbGetQuery(
  con,
  "SELECT dateandtimeutc, transmitter, stationname
  FROM read_csv('data/detections/sturgeon_22_23.gz')
  WHERE transmitter
    SIMILAR TO '.*(6275|21066|18010|21065)$'
    AND dateandtimeutc > '2023-01-01'
  ORDER BY transmitter, dateandtimeutc"
)

setDT(dets)
dets <- unique(dets, by = c('dateandtimeutc', 'transmitter', 'stationname'))

dets <- dets[, .(min = min(dateandtimeutc), max = max(dateandtimeutc)),
         .(transmitter, stationname, rleid(stationname))]

rkm <- fread('data/receiver_rkm.csv')


k <- dets[rkm, , on = 'stationname == station', nomatch = 0] 
j <- copy(k)[, body := factor(body,
                              levels = c('Broad Creek', 'Nanticoke River',
                                         'Marshyhope Creek'),
                              ordered = T)]
j <- unique(setorder(j, body, rkm_body_mouth), by = 'stationname')
j[body == 'Broad Creek'] <- setorder(j[body == 'Broad Creek'], by = -rkm_body_mouth)
k[, station_fac := factor(stationname,
                          levels = j$stationname,
                          ordered = T)]


library(ggplot2)
library(ragg)


agg_png('test.png', width = 750, height = 900)
ggplot(data = k) + 
  annotate('rect', xmin = min(k$min), xmax = max(k$max),
           ymax = '0.5 Federalsburg', ymin = '4.9 Marshyhope Confluence',
           fill = 'pink', alpha = 0.5) +
  annotate('rect', xmin = min(k$min), xmax = max(k$max),
           ymax = 'Short Cut', ymin = '10.0 Roaring Point',
           fill = 'lightgray', alpha = 0.5) +
  annotate('rect', xmin = min(k$min), xmax = max(k$max),
           ymax = 'Broad Creek G3', ymin = 'Bethel Bridge',
           fill = 'blue', alpha = 0.5) +
  geom_segment(aes(x = min, xend = max, y = station_fac),
               lwd = 2) +
  facet_wrap(~transmitter, ncol = 1) +
  theme_minimal() +
  labs(x = NULL, y = NULL)
dev.off()
