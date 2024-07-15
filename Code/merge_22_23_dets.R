library(data.table)

library(readxl)

mdnr22 <- list.files('data/detections/dnr/dnr2022/dl', pattern = "\\.xlsx",
           full.names = T) |> 
  lapply(read_excel) |> 
  do.call(rbind, args = _)

setDT(mdnr22)

mdnr23 <- list.files('data/detections/dnr/dnr2023/dl', pattern = "\\.xlsx",
                     full.names = T) |> 
  lapply(read_excel) |> 
  do.call(rbind, args = _)

setDT(mdnr23)

mdnr <- rbind(
  mdnr22,
  mdnr23
)

mdnr[, Latitude := as.numeric(gsub("+", "", Latitude))]
setnames(mdnr, function(.) tolower(gsub("[\\(\\) ]", "", .)))

dnrec <- list.files('data/detections/dnrec', full.names = TRUE) |> 
  lapply(fread, col.names = function(.) tolower(gsub("[\\(\\) ]", "", .))) |>
  rbindlist() |> 
  _[!is.na(dateandtimeutc)]


dets_22_23 <- rbind(
  mdnr,
  dnrec
)


fwrite(dets_22_23, "data/detections/sturgeon_22_23.gz")
