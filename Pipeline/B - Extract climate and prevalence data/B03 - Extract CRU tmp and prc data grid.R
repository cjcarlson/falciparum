################################################################################
# Extract temperature and precipitation data from CRU-TS4.XX at point locations
# This modified version extracts climate data at specific survey lat/lon points
# rather than averaging across administrative units
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(sf, here, terra, tidyverse, arrow)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

sf::sf_use_s2(FALSE)

################################################################################
# Set up logging ----
################################################################################

# log_msg <- create_logger(file.path(logs_dir, "B03_extract_CRU_grid.log"))

log_msg <- create_logger()

log_msg("Starting script `B03 - Extract CRU tmp and prc data grid.R`")

# Read continent shapefile for cropping rasters
log_msg("Loading admin regions for raster cropping")
cont <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::select(OBJECTID, geometry)
log_msg(sprintf("  Loaded %d admin regions", nrow(cont)))

################################################################################
# Prevalence data ----
################################################################################

log_msg("Loading prevalence point locations")

# Read the prevalence CSV file first to get point locations
prev_df <- prev_DB_fp |>
  readr::read_csv(
    col_select = c(Lat, Long),
    col_types = readr::cols(Long = col_double(), Lat = col_double(), )
  ) |>
  dplyr::distinct(Lat, Long) |>
  dplyr::mutate(
    # METHOD = str_to_upper(METHOD),
    point_id = row_number() # Add unique point identifier
  ) |>
  sf::st_as_sf(
    coords = c("Long", "Lat"),
    crs = 4326,
    remove = FALSE # Keep Long and Lat columns
  )

log_msg(sprintf("  %d unique survey points loaded", nrow(prev_df)))

################################################################################
# Extract temperature data ----
################################################################################

log_msg("Loading and cropping CRU temperature raster")

# Read and process temperature raster data
tmp <- cru_tmp_fp |>
  terra::rast() |>
  terra::crop(cont) %>%
  terra::subset(grep("tmp_", names(.)))

time_names <- as.character(time(tmp))

log_msg(sprintf("  Temperature raster: %d layers", terra::nlyr(tmp)))

# Define the powers to be applied
powers <- 1:5

log_msg("Extracting temperature at survey points (powers 1-5)")

# Apply the function to all powers for temperature
temp_extract_list <- lapply(
  powers,
  extract_clim_data_points,
  clim_data = tmp,
  points_sf = prev_df,
  rast_times = time_names,
  var_name = "temp"
)

# Merge all temperature data frames into one
temp_df <- purrr::reduce(
  temp_extract_list,
  left_join,
  by = c("point_id", "Lat", "Long", 'year', 'month')
)

log_msg(sprintf("  Temperature extraction complete: %d rows", nrow(temp_df)))

rm(tmp, temp_extract_list)
gc()

################################################################################
# Extract precipitation data ----
################################################################################

log_msg("Loading and cropping CRU precipitation raster")

# Read and process precipitation raster data
pre <- cru_pre_fp |>
  terra::rast() |>
  terra::crop(cont) %>%
  terra::subset(grep("pre_", names(.)))

time_names <- as.character(time(pre))

log_msg(sprintf("  Precipitation raster: %d layers", terra::nlyr(pre)))

log_msg("Extracting precipitation at survey points (powers 1-5)")

# Apply the function to all powers for precipitation
pre_extract_list <- lapply(
  powers,
  extract_clim_data_points,
  clim_data = pre,
  points_sf = prev_df,
  rast_times = time_names,
  var_name = "ppt"
)

# Merge all precipitation data frames into one
pre_df <- purrr::reduce(
  pre_extract_list,
  left_join,
  by = c("point_id", "Lat", "Long", 'year', 'month')
)

log_msg(sprintf("  Precipitation extraction complete: %d rows", nrow(pre_df)))

rm(pre, pre_extract_list)
gc()

################################################################################
# Combine tmp and prc data ----
################################################################################

log_msg("Joining temperature and precipitation data")

# Join the temperature and precipitation data
complete_df <- dplyr::left_join(
  temp_df,
  pre_df,
  by = c("point_id", "Lat", "Long", 'year', 'month')
) |>
  dplyr::select(
    point_id,
    Lat,
    Long,
    year,
    month,
    tidyselect::starts_with("temp"),
    tidyselect::starts_with("ppt"),
  )

log_msg(sprintf("  Combined data: %d rows, %d columns", nrow(complete_df), ncol(complete_df)))

################################################################################
# Save data ----
################################################################################

log_msg(sprintf("Saving grid climate data to: %s", intermediate_CRU_grid_fp))
# readr::write_csv(complete_df, intermediate_CRU_grid_fp)
arrow::write_feather(x = complete_df, intermediate_CRU_grid_fp)

log_msg("Script `B03 - Extract CRU tmp and prc data grid.R` completed successfully")

################################################################################
# End of file ----
################################################################################
