################################################################################
# Extract CRU climate data (temperature and precipitation) from CRU-TS4.03 
# NetCDFs at ADM1 level. Climate model data is bias corrected to CRU 4.03, so it
# is recommended only use this version, unless redoing the bias correction. 
# CRU data can be downloaded from:
# https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/cruts.1905011326.v4.03/tmp/cru_ts4.03.1901.2018.tmp.dat.nc.gz
# https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/cruts.1905011326.v4.03/pre/cru_ts4.03.1901.2018.pre.dat.nc.gz
#
# Note: This script will not work in modern versions of R (> 4.2.3).
# The packages rgdal and velox are no longer supported. To replicate
# this script exactly, it is recomended to use docker to create a
# stable environment that supports these packages.
# See https://hub.docker.com/repository/docker/cmolitor/r-malaria-cru/general
# for a working container that can run this script.
#
# Example useage with apptainer:
# apptainer pull docker://cmolitor/r-malaria-cru:4.2.3
# cd "Pipeline/B - Extract climate and prevalence data"
# apptainer exec \
#     /path/to/apptainer/r-malaria-cru_4.2.3.siff \
#     Rscript "B05 - Extract ERA5 tmp and prc data ADM1.R"
################################################################################
# Set up ----
################################################################################

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
  arrow,
  vroom
)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

options(future.globals.maxSize = 3 * 1024^3)

N_CORES <- 12L

future::plan(future::multicore, workers = N_CORES)

################################################################################
# Set up logging ----
################################################################################

log_msg <- create_logger(file.path(logs_dir, "B01_extract_CRU_ADM1.log"))

log_msg("Starting script `B01 - Extract CRU tmp and prc data ADM1.R`")
log_msg(sprintf("  Using %d cores for parallel processing", N_CORES))

################################################################################
# ADM1 data ----
################################################################################

log_msg("Loading admin regions")

admin_regions <- rgdal::readOGR(ADM1_fp)

log_msg(sprintf("  Loaded %d admin regions", length(admin_regions)))

################################################################################
# Extract temperature ----
################################################################################

log_msg("Extracting CRU temperature data at ADM1 level")

admin_regions <- extract_cru_variable(
  nc_filepath = cru_tmp_fp,
  nc_varname = "tmp",
  admin_sp = admin_regions,
  var_prefix = "temp",
  max_power = 5L,
  start_year = 1901L
)

log_msg("  Temperature extraction complete")

################################################################################
# Extract precipitation ----
################################################################################

log_msg("Extracting CRU precipitation data at ADM1 level")

admin_regions <- extract_cru_variable(
  nc_filepath = cru_pre_fp,
  nc_varname = "pre",
  admin_sp = admin_regions,
  var_prefix = "ppt",
  max_power = 5L,
  start_year = 1901L
)

log_msg("  Precipitation extraction complete")

future::plan(future::sequential)

################################################################################
# Save intermediate climate data ----
################################################################################

log_msg("Reshaping extracted data to long format")

climate_df <- admin_regions@data

# Drop original shapefile attribute columns (cols 2-15),
# keeping OBJECTID (col 1) plus all extracted climate columns
climate_df <- climate_df[, -c(2:15)]

log_msg("Pivot longer")

# Wide -> long
climate_long <- tidyr::pivot_longer(
  climate_df,
  cols = -OBJECTID,
  names_to = "variable",
  values_to = "value"
)

log_msg("Create time")

# Parse "Mon.YYYY.var" naming convention
climate_long <- tidyr::separate(
  climate_long,
  col = variable,
  into = c("month", "year", "var"),
  sep = "\\."
)

log_msg("Pivot wider")

# Long -> wide by variable type
climate_wide <- tidyr::pivot_wider(
  climate_long,
  names_from = "var",
  values_from = "value"
)

climate_wide$year <- as.numeric(climate_wide$year)

log_msg(
  sprintf(
    "  Reshaped data: %d rows, %d columns",
    nrow(climate_wide),
    ncol(climate_wide)
  )
)

################################################################################
# Save intermediate climate data ----
################################################################################

log_msg(sprintf("Saving climate data to: %s", intermediate_CRU_adm1_fp))

readr::write_csv(climate_wide, intermediate_CRU_adm1_fp)

log_msg(
  "Script `B01 - Extract CRU tmp and prc data ADM1.R` completed successfully"
)

################################################################################
# End of file ----
################################################################################
