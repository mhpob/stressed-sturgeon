# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c('dplyr', 'data.table', 'readxl', 'ggplot2', 'ragg', 'lubridate', 'gt', 
               'sf', 'geoarrow', 'arrow'), 
  # default storage format
  format = "feather" 
  # Set other options as needed.
)

# tar_make_future() configuration:
plan(multisession,
     workers = availableCores(logical = F))


# Load the R scripts with your custom functions:
for (file in list.files("code/targets", full.names = TRUE)) source(file)
rm(file)

# Replace the target list below with your own:
list(
  tar_target(
    name = data,
    command = tibble(x = rnorm(100), y = rnorm(100))
#   format = "feather" # efficient storage of large data frames # nolint
  ),
  tar_target(
    name = model,
    command = coefficients(lm(y ~ x, data = data))
  )
)
