library(data.table)
library(lubridate)

#### prep detections: ####
# 1. Assign daily reach per fish using established rules.
#   a. rules may need to be adjusted as previous code emphasized LMC due to
#     1) VPS and 2) SSS locatons

#### Read detections ####
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

dets_24 <- lapply(
  list.files(
    'Data/Detections/DNR/DNR 2024',
    pattern = "\\.csv$",
    full.names = TRUE
  ),
  fread,
  fill = TRUE
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
dnrec_24 <- fread("Data//Detections/DNREC/2024_VUE_Export.csv") |>
  _[,
    `Date and Time (UTC)` := as.POSIXct(
      `Date and Time (UTC)`,
      format = "%m/%d/%Y %H:%M"
    )
  ]


## UMCES
### Augmented array deployed in UMC (Federalsburg, MD) in 2022
umces_22 <- list.files(
  "Data/Detections/UMCES/2022/",
  pattern = "\\.csv",
  full.names = T
) |>
  lapply(fread, fill = T) |>
  rbindlist() |>
  _[, river := "UMC"]

### VPS deployed in UUNR (Seaford, DE) in 2023 and 2024
umces_vps <- list.files(
  "Data/Detections/UMCES",
  pattern = "^DET\\.csv$",
  recursive = T,
  full.names = T
) |>
  lapply(fread, skip = 2) |>
  rbindlist() |>
  _[
    grepl("A69-9001", `Full ID`),
    .(
      `Date and Time (UTC)` = `Device Time (UTC)`,
      Receiver = `Serial Number`,
      Transmitter = `Full ID`,
      river = "UUNR"
    )
  ]

### Live receiver deployed in UNR (DE fish hatchery) in 2024
umces_live <- 'Data/Detections/UMCES/real time 2024/db_parsed.csv' |>
  fread() |>
  _[detections != '', "detections"] |>
  _[, detections := gsub('""', '\"', detections)] |>
  _[,
    detections := lapply(detections, function(.) {
      as.data.table(eval(parse(text = .)))
    })
  ][[1]] |>
  rbindlist() |>
  _[, .(
    `Date and Time (UTC)` = as.POSIXct(datetimeutc),
    Receiver = receiver,
    Transmitter = paste(codespace, tag, sep = "-"),
    river = "UNR"
  )] |>
  rbind({
    'Data/Detections/UMCES/real time 2024/detections.csv' |>
      fread(
        col.names = c(
          'receiver',
          "datetimeutc",
          "codespace",
          "tag",
          "status",
          "checksum"
        )
      ) |>
      _[,
        .(
          `Date and Time (UTC)` = datetimeutc,
          Receiver = gsub("\\\"\\*|\\..*", "", receiver),
          Transmitter = paste(codespace, tag, sep = "-"),
          river = "UNR"
        )
      ]
  })


det_all <- rbindlist(
  list(
    dets_22,
    dets_23,
    dets_24,
    dnrec,
    dnrec_24,
    umces_22,
    umces_vps,
    umces_live
  ),
  fill = T
)

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


# there are some detections from VR2W-134624/station G67 that have no lat/lon
coords <- unique(
  det_all[stationname == "G67" & !is.na(longitude), ],
  by = c("longitude")
)[, c(longitude, latitude)]
det_all[stationname == "G67" & is.na(longitude), 'longitude'] <- coords[1]
det_all[stationname == "G67" & is.na(latitude), 'latitude'] <- coords[2]

# Broad Creek G3 has incorrect lat
det_all[
  stationname == "Broad Creek G3" & latitude < 38,
  latitude := latitude * 10
]

# Fix positive lon
det_all[longitude > 0, longitude := longitude * -1]

rec_pts <- unique(det_all, by = c('stationname')) |>
  _[
    !is.na(longitude) & is.na(river),
    c('stationname', 'longitude', 'latitude')
  ] |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)
setDT(rec_pts)

river_segments <- fread('census_model/coleman/2021_receiver_key.csv')

# If including Broad Creek separately
# river_segments[
#   station %in%
#     c("Dead Fall", "Bethel Bridge", "Speed Boat", "R16") |
#     grepl("Broad Creek|Dead *[Ff]all", station),
#   river := "BC"
# ]

# If splitting out UUNR
river_segments[
  station %in%
    c("G67", "Confluence", "Deep Creek", "Short Cut") |
    grepl("Seaford|ROFO|Mainstem", station),
  river := "UUNR"
]


# Assign reach_no
river_segments[,
  reach_no := fcase(
    river == 'LNR'  , 2 ,
    river == 'UNR'  , 5 ,
    river == 'UUNR' , 6 ,
    river == 'LMC'  , 3 ,
    river == 'UMC'  , 4
  )
]

key <- rec_pts[river_segments, , on = c('stationname' = 'station')]

det_all <- rbind(
  key[det_all[is.na(river), -"river"], , on = 'stationname', nomatch = NA],
  det_all[
    !is.na(river),
    c(
      .SD,
      .(
        reach_no = fcase(
          river == 'LNR'  , 2 ,
          river == 'UNR'  , 5 ,
          river == 'UUNR' , 6 ,
          river == 'LMC'  , 3 ,
          river == 'UMC'  , 4
        )
      )
    )
  ],
  fill = T
)


#### Prep SSS ####
sss <- fread("Data/MISC/SSS_22-24.csv", col.names = \(.) {
  tolower(gsub(" ", "_", gsub("[\\(\\)]", "", .)))
}) |>
  ## Assign reach
  _[, let(
    date = as.Date(date, format = "%m/%d/%Y"),
    reach_no = fcase(
      n_y > 4274753 & e_x > 445265   , 6 ,
      grepl("N[RD]|BC", target_name) , 5 , # seems all but 3 NR are UNR; grab ND typo
      grepl("MC", target_name)       , 3 # and seems that all MC are LMC
    )
  )]

# Fix the few NR in LNR
#  RETURN TO THIS: Actually, not going to do this as it overly confuses the sss matrix
# sss[
#   target_name %in%
#     c("ATS_NR_090622_017_20", "ATS_NR_091322_013_20", "	ATS_NR_090122_042_17"),
#   reach_no := 2
# ]

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
temp <- c(
  "census_model/VR2AR_546463_20221213_TEMP.csv", # UMC, 10 Sept - 13 Dec
  "census_model/VR2AR_546306_20231110_TEMP.csv", # UNR, 4 Sept - 9 Nov
  "census_model/VR2AR_550737_20241030_TEMP.csv" #UNR, 13 Sept - 30 Oct
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
