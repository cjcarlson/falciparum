################################################################################
# Extract CRU climate data (temperature and precipitation) from CRU-TS4.03
# NetCDFs at ADM1 level. Climate model data is bias corrected to CRU 4.03, so it
# is recommended only use this version, unless redoing the bias correction.
# CRU data can be downloaded from:
# shared: crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/cruts.1905011326.v4.03
# <shared>/tmp/cru_ts4.03.1901.2018.tmp.dat.nc.gz
# <shared>/pre/cru_ts4.03.1901.2018.pre.dat.nc.gz
# - Note: Pre-extracted CRU data can be downloaded from 
#   https://zenodo.org/records/20399793
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(sf, here, tidyverse, arrow, data.table, exactextractr)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

powers <- 1:5

################################################################################
# Set up logging ----
################################################################################

log_msg <- create_logger()

log_msg("Starting script `B01 - Extract CRU tmp and prc data ADM1.R`")

################################################################################
# ADM1 data ----
################################################################################

log_msg("Loading admin regions")

admin_regions <- sf::read_sf(ADM1_fp)

log_msg(sprintf("  Loaded %d admin regions", length(admin_regions$OBJECTID)))

################################################################################
# Temperature data ----
################################################################################

tmp <- cru_tmp_fp |>
  terra::rast() |>
  terra::crop(admin_regions) %>%
  terra::subset(grep("tmp_", names(.)))

time_names <- as.character(terra::time(tmp))

################################################################################
# Extract temperature ----
################################################################################

log_msg("Extracting CRU temperature data at ADM1 level")

temp_extract_list <- purrr::map(powers, \(p) {
  extract_clim_data_polygons(
    rast = tmp,
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

log_msg("  Temperature extraction complete")

################################################################################
# Precipitation data ----
################################################################################

pre <- cru_pre_fp |>
  terra::rast() |>
  terra::crop(admin_regions) %>%
  terra::subset(grep("pre_", names(.)))

time_names <- as.character(terra::time(pre))

################################################################################
# Extract precipitation ----
################################################################################

log_msg("Extracting CRU precipitation data at ADM1 level")

pre_extract_list <- purrr::map(powers, \(p) {
  extract_clim_data_polygons(
    rast = pre,
    polygons = admin_regions,
    rast_times = time_names,
    value_name = paste0("ppt", if (p == 1) "" else p),
    power = p
  )
})

pre_df <- purrr::reduce(
  pre_extract_list,
  \(x, y) merge(x, y, by = c("OBJECTID", "year", "month"), all.x = TRUE)
)

log_msg("  Precipitation extraction complete")

################################################################################
# Merge temp and precip data ----
################################################################################

log_msg("Merging climate data")

climate_df <- merge(
  temp_df,
  pre_df,
  by = c("OBJECTID", "year", "month"),
  all = FALSE
)

################################################################################
# Save intermediate climate data ----
################################################################################

log_msg(sprintf("Saving climate data to: %s", intermediate_CRU_adm1_fp))

arrow::write_feather(climate_df, intermediate_CRU_adm1_fp)

log_msg(
  "Script `B01 - Extract CRU tmp and prc data ADM1.R` completed successfully"
)

################################################################################
# End of file ----
################################################################################
