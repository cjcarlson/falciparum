################################################################################
# Use the following code to extract ERA5 data for temperature and precipitation
# from the dataset:
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  here,
  terra,
  future,
  tidyverse,
  progressr,
  parallelly,
  data.table,
  future.apply,
  exactextractr
)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

overwrite <- TRUE

################################################################################
# Logging ----
################################################################################

log_msg <- create_logger(file.path(logs_dir, "B05_extract_ERA5_ADM1.log"))

log_msg("Starting script `B05 - Extract ERA5 tmp and prc data ADM1.R`")

################################################################################
# ADM1 data ----
################################################################################

cont <- sf::read_sf(ADM1_fp)

################################################################################
# Extract temperature ----
################################################################################

temp_rast <- temp_fp |>
  terra::rast() |>
  terra::crop(terra::ext(cont))

temp_rast = temp_rast - 273.15

rast_times = as.character(terra::time(temp_rast))

names(temp_rast) <- rast_times

temp_dt <- extract_long(
  rast = temp_rast,
  polygons = cont,
  rast_times = rast_times,
  value_name = "temp"
)

temp2_dt <- extract_long(
  rast = temp_rast * temp_rast,
  polygons = cont,
  rast_times = rast_times,
  value_name = "temp2"
)
temp_dt[, temp2 := temp2_dt$temp2]
rm(temp2_dt)

################################################################################
# Extract precipitation ----
################################################################################

precip_rast <- prec_fp |>
  terra::rast() |>
  terra::crop(terra::ext(cont))

rast_times <- as.character(terra::time(precip_rast))
names(precip_rast) <- rast_times

precip_dt <- extract_long(
  rast = precip_rast,
  polygons = cont,
  rast_times = names(precip_rast) <- rast_times,
  value_name = "ppt"
)
temp_dt[, ppt := precip_dt$ppt]
rm(precip_dt)

################################################################################
# Save intermediate climate data ----
################################################################################

setcolorder(temp_dt, c("OBJECTID", "year", "month", "temp", "temp2", "ppt"))

data.table::fwrite(temp_dt, intermediate_ERA_adm1_fp)

log_msg(
  "Script `B05 - Extract ERA5 tmp and prc data ADM1.R` completed successfully"
)

################################################################################
# End of file ----
################################################################################
