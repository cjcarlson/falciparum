#############################################################################-
# Use the following code to extract GCM data for temperature and precipitation
# from the CRU-TS4.03 dataset. The code extracts the data for 5 climate scenarios
# and 10 climate models. The code extracts the data for each month from 1901 to
# 2100. The code saves the data in CSV format for each month and each model. The
# code also consolidates the data into a single CSV file for each clim_var and
# model. The code uses N cores to parallelize, which is chosen by the user.
# Finally, the code saves the data in the 'Climate' directory.
#############################################################################-

############################################################
# Set up ----
############################################################

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

############################################################
# Logging ----
############################################################

log_msg <- create_logger(file.path(logs_dir, "B05_extract_ERA5_ADM1.log"))

log_msg("Starting script `B05 - Extract ERA5 tmp and prc data ADM1.R`")

############################################################
# ADM1 data ----
############################################################

cont <- sf::read_sf(ADM1_fp)

############################################################
# Extract temperature ----
############################################################

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

############################################################
# Extract precipitation ----
############################################################

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

############################################################
# Save intermediate climate data ----
############################################################

setcolorder(temp_dt, c("OBJECTID", "year", "month", "temp", "temp2", "ppt"))

data.table::fwrite(temp_dt, intermediate_ERA_adm1_fp)

log_msg(
  "Script `B05 - Extract ERA5 tmp and prc data ADM1.R` completed successfully"
)

############################################################
# End of file ----
############################################################



# cru <- intermediate_CRU_adm1_fp |> 
#   data.table::fread()


# # align types for the join
# cru[, `:=`(OBJECTID = as.character(OBJECTID), year = as.character(year))]

# # merge
# merged <- cru[temp_dt, on = .(OBJECTID, year, month), nomatch = 0]

# # scatter
# library(ggplot2)
# ggplot(merged, aes(x = temp, y = i.temp)) +
#   geom_point(alpha = 0.05, size = 0.3) +
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
#   labs(x = "CRU Temp (°C)", y = "ERA5 Temp (°C)") +
#   theme_minimal()

