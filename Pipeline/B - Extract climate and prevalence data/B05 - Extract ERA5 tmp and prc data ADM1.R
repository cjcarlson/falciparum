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

pacman::p_load(sf, here, terra, tidyverse, data.table, exactextractr)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

overwrite <- TRUE

################################################################################
# Logging ----
################################################################################

# log_msg <- create_logger(file.path(logs_dir, "B05_extract_ERA5_ADM1.log"))

log_msg <- create_logger()

log_msg("Starting script `B05 - Extract ERA5 tmp and prc data ADM1.R`")

################################################################################
# ADM1 data ----
################################################################################

log_msg("Loading administrative data")

admin_regions <- sf::read_sf(ADM1_fp)

################################################################################
# Temperature data ----
################################################################################

log_msg("Load temperature data")

temp_rast <- era5_temp_fp |>
  terra::rast() |>
  terra::crop(terra::ext(admin_regions))

temp_rast = temp_rast - 273.15

time_names = as.character(terra::time(temp_rast))

names(temp_rast) <- time_names

################################################################################
# Extract temperature ----
################################################################################

log_msg("Extract temperature data")

temp_extract_list <- purrr::map(1:2, \(p) {
  extract_clim_data_polygons(
    rast = temp_rast,
    polygons = admin_regions,
    rast_times = time_names,
    value_name = paste0("temp", if (p == 1) "" else p),
    power = p
  )
})

temp_df <- purrr::reduce(
  temp_extract_list,
  \(x, y) merge(x, y, by = c("OBJECTID", "year", "month"), all.x = TRUE)
)

################################################################################
# Precipitation data ----
################################################################################

log_msg("Load precipitation data")

precip_rast <- era5_prec_fp |>
  terra::rast() |>
  terra::crop(terra::ext(admin_regions))

terra::time(precip_rast) <- seq(
  as.Date("1940-01-01"),
  by = "month",
  length.out = terra::nlyr(precip_rast)
)

rast_times <- as.character(terra::time(precip_rast))
names(precip_rast) <- rast_times

################################################################################
# Extract precipitation ----
################################################################################

log_msg("Extract precipitation data")

pre_df <- extract_clim_data_polygons(
  rast = precip_rast,
  polygons = admin_regions,
  rast_times = rast_times,
  value_name = "ppt"
)

# temp_df[, ppt := precip_dt$ppt]
# rm(precip_dt)

################################################################################
# Save intermediate climate data ----
################################################################################

climate_df <- merge(
  temp_df,
  pre_df,
  by = c("OBJECTID", "year", "month"),
  all = FALSE
)

log_msg(paste0("Save data to: ", intermediate_ERA_adm1_fp))

arrow::write_feather(climate_df, intermediate_ERA_adm1_fp)

log_msg(
  "Script `B05 - Extract ERA5 tmp and prc data ADM1.R` completed successfully"
)

################################################################################
# End of file ----
################################################################################
