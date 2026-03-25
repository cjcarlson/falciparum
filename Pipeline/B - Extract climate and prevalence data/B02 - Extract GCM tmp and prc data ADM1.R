#############################################################################-
# Use the following code to extract GCM data for temperature and precipitation
# from the CRU-TS4.03 dataset. The code extracts the data for 5 climate scenarios
# and 10 climate models. The code extracts the data for each month from 1901 to
# 2100. The code saves the data in CSV format for each month and each model. The
# code also consolidates the data into a single CSV file for each scenario and
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
  tictoc,
  foreach,
  tidyverse,
  doParallel,
  exactextractr,
  data.table
)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

overwrite <- FALSE

tictoc::tic()

############################################################
# Set up logging ----
############################################################

log_msg <- create_logger(file.path(logs_dir, "B02_extract_GCM_ADM1.log"))

log_msg("Starting script B02 - Extract GCM tmp and prc data ADM1")

############################################################
# Make cluster----
############################################################

numCores <- 12

cl <- makeCluster(numCores, outfile = "")

registerDoParallel(cl)

log_msg(sprintf("  Registered parallel cluster with %d cores", numCores))

############################################################
# Loop over scenarios ----
############################################################

for (scenario in scenarios) {
  # scenario = "historical"
  tictoc::tic()
  log_msg(sprintf("Starting scenario: %s", scenario))

  date_range <- dplyr::case_when(
    scenario %in% names(historical_scenario_names) ~ "_190101-201412_",
    scenario %in% names(future_scenario_names) ~ "_201501-210012_"
  )

  year_mon <- ifelse(scenario %in% names(historical_scenario_names), 1368, 1032)
  year_start <- ifelse(
    scenario %in% names(historical_scenario_names),
    1901,
    2015
  )

  ############################################################
  # Loop over Models ----
  ############################################################

  cont <- sf::read_sf(here::here(data_dir, 'Data', 'AfricaADM1.shp'))
  for (model in models) {
    # model = "ACCESS-CM2"
    tictoc::tic()
    log_msg(sprintf("  Starting model: %s (%d months)", model, year_mon))

    grid <- dplyr::case_when(
      model == "GFDL-ESM4" ~ "gr1",
      model == "IPSL-CM6A-LR" ~ "gr",
      TRUE ~ "gn"
    )
    prc_fn <- make_filename("pr", model, scenario, grid, date_range)
    tmp_fn <- make_filename("tas", model, scenario, grid, date_range)

    ############################################################
    # Extraction ----
    ############################################################

    output_path <- file.path(
      data_dir,
      "int",
      scenario,
      paste0(model, ".csv")
    )

    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

    if (!file.exists(output_path) | overwrite) {
      temp_rast <- file.path(bc_cruts_output_dir, scenario, tmp_fn) |>
        terra::rast()

      rast_times <- as.character(time(temp_rast))

      ############################################################
      # Extract temp ----
      ############################################################

      temp_dt <- extract_long(
        rast = temp_rast,
        polygons = cont,
        rast_times = rast_times,
        value_name = "temp"
      )

      ############################################################
      # Extract temp^2 ----
      ############################################################

      temp2_dt <- extract_long(
        rast = temp_rast * temp_rast,
        polygons = cont,
        rast_times = rast_times,
        value_name = "temp2"
      )
      temp_dt[, temp2 := temp2_dt$temp2]
      rm(temp2_dt) # free immediately

      precip_rast <- file.path(bc_cruts_output_dir, scenario, prc_fn) |>
        terra::rast()

      ############################################################
      # Extract precip ----
      ############################################################

      precip_dt <- extract_long(
        rast = precip_rast,
        polygons = cont,
        rast_times = as.character(time(precip_rast)),
        value_name = "ppt"
      )
      temp_dt[, ppt := precip_dt$ppt]
      rm(precip_dt)

      ############################################################
      # Save results ----
      ############################################################

      data.table::fwrite(temp_dt, output_path)
    }
    log_msg(sprintf("  Finished model: %s", model))

    tictoc::toc()
  }
  log_msg(sprintf("Finished scenario: %s", scenario))
  tictoc::toc()
}
tictoc::toc()
stopCluster(cl)

log_msg("Script B02 completed successfully")

############################################################
# End of file ----
############################################################
