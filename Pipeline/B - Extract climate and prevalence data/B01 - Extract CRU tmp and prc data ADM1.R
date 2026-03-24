###########################################################
# Extract CRU climate data (temperature and precipitation)
# from CRU-TS4.03 NetCDFs at ADM1 level.
#
# Note:: This script will not work in modern versions of R (> 4.2.3).
# The packages rgdal is no longer supported and Velox requires
# sp and rgdal. To replicate this script exactly, it is
# recomended to use docker to create a stable environment that
# supports these packages.
# See https://hub.docker.com/repository/docker/cmolitor/r-malaria-cru/general
# for a working container that can run this script.
############################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  sp,
  here,
  ncdf4,
  raster,
  rgdal,
  velox,
  tidyverse,
  future,
  future.apply,
  arrow
)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

options(future.globals.maxSize = 3 * 1024^3)

N_CORES <- 12L

future::plan(future::multicore, workers = N_CORES)

############################################################
# Load admin ----
############################################################

admin_regions <- rgdal::readOGR(ADM1_fp)

############################################################
# Extract temperature ----
############################################################

admin_regions <- extract_cru_variable(
  nc_filepath = cru_tmp_fp,
  nc_varname = "tmp",
  admin_sp = admin_regions,
  var_prefix = "temp",
  max_power = 5L,
  start_year = 1901L
)

############################################################
# Extract precipitation ----
############################################################

admin_regions <- extract_cru_variable(
  nc_filepath = cru_prc_fp,
  nc_varname = "pre",
  admin_sp = admin_regions,
  var_prefix = "ppt",
  max_power = 5L,
  start_year = 1901L
)

future::plan(future::sequential)

############################################################
# Save intermediate climate data ----
############################################################

climate_df <- admin_regions@data

# Drop original shapefile attribute columns (cols 2-15),
# keeping OBJECTID (col 1) plus all extracted climate columns
climate_df <- climate_df[, -c(2:15)]

# Wide -> long
climate_long <- tidyr::pivot_longer(
  climate_df,
  cols = -OBJECTID,
  names_to = "variable",
  values_to = "value"
)

# Parse "Mon.YYYY.var" naming convention
climate_long <- tidyr::separate(
  climate_long,
  col = variable,
  into = c("month", "year", "var"),
  sep = "\\."
)

# Long -> wide by variable type
climate_wide <- tidyr::pivot_wider(
  climate_long,
  names_from = "var",
  values_from = "value"
)

climate_wide$year <- as.numeric(climate_wide$year)

############################################################
# Write results ----
############################################################

readr::write_csv(climate_wide, intermediate_CRU_fp)

message(sprintf("Climate data saved to:\n  %s\n  %s", intermediate_CRU_fp))
