################################################################################
# Extracts GCM data (temperature and precipitation) for 5 climate scenarios and
# 10 climate models for each month and year from 1901 to 2100. The data is
# saved in CSV format for each scenario and model.
# Coupled Model Intercomparison Project (CMIP6) data can be downloaded from:
# https://cds.climate.copernicus.eu/datasets/projections-cmip6
# - The following ten models are used:
#   `ACCESS-CM2`, `ACCESS-ESM1-5`, `BCC-CSM2-MR`, `CanESM5`, `FGOALS-g3`,
#   `GFDL-ESM4`, `IPSL-CM6A-LR`, `MIROC6`, `MRI-ESM2-0`, and `NorESM2-LM`
# - Under 5 climate scenarios:
#   `historical`, `historical-natural`, `SSP1-2.6`, `SSP2-4.5`, and `SSP5-8.5`
# - Note: CMIP6 data have gone through a bias correction procedure to calibrate
#   values to CRU 4.03. Pre-extracted BC CMIP6 data can be downloaded from
#   https://zenodo.org/records/20399793
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
# Set up logging ----
################################################################################

log_msg <- create_logger()

log_msg("Starting script `B02 - Extract GCM tmp and prc data ADM1.R`")

################################################################################
# Make cluster ----
################################################################################

n_cores <- min(10, availableCores())
future::plan(multicore, workers = n_cores)

################################################################################
# Loop over scenarios ----
################################################################################

for (scenario in scenarios) {
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

  ##############################################################################
  # Loop over Models ----
  ##############################################################################

  future_lapply(
    models,
    function(model) {
      # ---- Load cont fresh on each worker (avoids sf serialization) ----
      cont <- sf::read_sf(ADM1_fp)

      grid <- dplyr::case_when(
        model == "GFDL-ESM4" ~ "gr1",
        model == "IPSL-CM6A-LR" ~ "gr",
        TRUE ~ "gn"
      )
      prc_fn <- make_filename("pr", model, scenario, grid, date_range)
      tmp_fn <- make_filename("tas", model, scenario, grid, date_range)

      output_path <- file.path(
        inter_cmip6_ext_dir,
        scenario,
        paste0(model, ".csv")
      )
      output_path |>
        dirname() |>
        dir.create(showWarnings = FALSE, recursive = TRUE)

      if (!file.exists(output_path) | overwrite) {
        temp_rast <- terra::rast(file.path(
          climate_bc_cmip6_dir,
          scenario,
          tmp_fn
        ))
        rast_times <- as.character(terra::time(temp_rast))

        temp_dt <- extract_clim_data_polygons(
          rast = temp_rast,
          polygons = cont,
          rast_times = rast_times,
          value_name = "temp"
        )

        temp2_dt <- extract_clim_data_polygons(
          rast = temp_rast * temp_rast,
          polygons = cont,
          rast_times = rast_times,
          value_name = "temp2"
        )
        temp_dt[, temp2 := temp2_dt$temp2]
        rm(temp2_dt)

        precip_rast <- terra::rast(file.path(
          climate_bc_cmip6_dir,
          scenario,
          prc_fn
        ))

        precip_dt <- extract_clim_data_polygons(
          rast = precip_rast,
          polygons = cont,
          rast_times = as.character(terra::time(precip_rast)),
          value_name = "ppt"
        )
        temp_dt[, ppt := precip_dt$ppt]
        rm(precip_dt)

        data.table::fwrite(temp_dt, output_path)
      }

      return(NULL)
    },
    future.seed = NULL
  )
  log_msg(sprintf("Finished scenario: %s", scenario))
}
future::plan(sequential)

log_msg(
  "Script `B02 - Extract GCM tmp and prc data ADM1.R` completed successfully"
)

################################################################################
# End of file ----
################################################################################
