library(rvdat)

dir.create(file.path(tempdir(), 'cde'))
list.files('data/detections/dnr/dnr2022', full.names = T) |> 
  lapply(vdat_to_folder, outdir = file.path(tempdir(), 'cde'))
fls <- list.files(file.path(tempdir(), 'cde'), pattern = 'DET.csv', recursive = T, full.names = T)
fls_names <- sapply(strsplit(fls, '/'), function(x) gsub('-.*', '', x[[3]]))

file.rename(fls, file.path('data/detections/dnr/dnr2022', fls_names))
