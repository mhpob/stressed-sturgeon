library(data.table)
library(lubridate)

#### prep detections: ####
# 1. Assign daily reach per fish using established rules.
#   a. rules may need to be adjusted as previous code emphasized LMC due to
#     1) VPS and 2) SSS locatons

#### Read detections ####
#### NEED 2024 and 2025 from MDNR, 2025 from DNREC ####
## MDNR
dets_22 <- lapply(
  list.files("Data//Detections//DNR//DNR2022/DL", full.names = T),
  readxl::read_xlsx
) |>
  rbindlist()

dets_23 <- lapply(
  list.files("Data//Detections//DNR//DNR2023/DL", full.names = T),
  readxl::read_xlsx
) |>
  rbindlist()

## DNREC
dnrec <- list.files(
  "./Data/Detections/DNREC",
  full.names = T,
  pattern = '2[23].*\\.csv'
) |>
  lapply(fread) |>
  rbindlist()
dnrec_24 <- data.table::fread("Data//Detections/DNREC/2024_VUE_Export.csv") |>
  _[,
    `Date and Time (UTC)` := as.POSIXct(
      `Date and Time (UTC)`,
      format = "%m/%d/%Y %H:%M"
    )
  ]

det_all <- rbindlist(list(dets_22, dets_23, dnrec, dnrec_24))

setnames(det_all, function(x) {
  tolower(gsub('[\\( \\)]', '', x))
})

det_all <- unique(det_all, by = c("dateandtimeutc", "transmitter", "receiver"))

# extract ATS
ats <- fread("./census_model/sturgeon_capture_data.csv", fill = T) |>
  _[, DateCaptured := as.Date(DateCaptured, "%m/%d/%Y")]

det_all <- det_all[transmitter %in% ats$TransmitterNumber]

# latitudes are character class
det_all[, latitude := as.numeric(latitude)]
det_all[, stationname := gsub('\\d+\\.\\d |\'', '', stationname)]
det_all <- det_all[
  !stationname %in% c("Shelltown", "Pocomoke City")
]
det_all[, date := as.Date(lubridate::floor_date(dateandtimeutc, "day"))]

# Change Walnut Landing Road -> Walnut Landing to match 2021 receiver key
# This is for _both_ "Walnut Landing Road" AND "Above Walnut Landing Road"
det_all[,
  stationname := gsub("Walnut Landing Road", "Walnut Landing", stationname)
]

#### Assign reaches ####
det_all <- det_all[transmitter != '']

# there are some detections from VR2W-134624/station G67 that have no lat/lon
coords <- unique(
  det_all[stationname == "G67" & !is.na(longitude), ],
  by = c("longitude")
)[, c(longitude, latitude)]
det_all[stationname == "G67" & is.na(longitude), 'longitude'] <- coords[1]
det_all[stationname == "G67" & is.na(longitude), 'latitude'] <- coords[2]

rec_pts <- unique(det_all, by = c('stationname')) |>
  _[, c('stationname', 'longitude', 'latitude')] |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)
setDT(rec_pts)

river_segments <- fread('census_model/2021_receiver_key.csv')

# Assign reach_no
river_segments[,
  reach_no := fcase(
    river == 'LNR' , 2 ,
    river == 'UNR' , 5 ,
    river == 'LMC' , 3 ,
    river == 'UMC' , 4
  )
]

key <- rec_pts[river_segments, , on = c('stationname' = 'station')]

det_all <- key[det_all, , on = 'stationname', nomatch = NA]


#### Prep SSS ####
sss <- fread("Data/MISC/SSS_22-24.csv", col.names = \(.) {
  tolower(gsub(" ", "_", gsub("[\\(\\)]", "", .)))
}) |>
  ## Assign reach
  _[, let(
    date = as.Date(date, format = "%m/%d/%Y"),
    reach_no = fifelse(
      grepl("NR", target_name),
      5, # currently seems that all NR SSS are UNR
      3 # and seems that all MC are LMC
    )
  )]

## Make survey id
setorder(sss, reach_no, date)
sss <- sss[, survey := rleid(date)] |>
  dcast(
    date + reach_no ~ transect,
    function(.) {
      x <- length(.)
      ifelse(x == 0, NA, x)
    },
    value.var = 'survey'
  )


#### Import temperature ####
# Add 2024 and 2025 somewhere
temp <- c(
  "VR2AR_546463_20221213_1.csv-fathom-split/TEMP.csv",
  "Data/VPS Events/VR2AR_546306_20231110_1.csv-fathom-split/TEMP.csv"
) |>
  lapply(fread) |>
  rbindlist() |>
  _[, date := as.Date(`Device Time (UTC)`)] |>
  _[,
    .(temp = mean(`Ambient (deg C)`)),
    by = 'date'
  ]


saveRDS(
  list(
    detections = det_all,
    ats = ats,
    sss = sss,
    temp = temp
  ),
  "census_model/census_data_master.RDS"
)
