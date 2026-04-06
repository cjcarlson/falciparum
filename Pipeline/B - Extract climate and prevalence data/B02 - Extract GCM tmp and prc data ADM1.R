################################################################################
# Use the following code to extract GCM data for temperature and precipitation
# from the CRU-TS4.03 dataset. The code extracts the data for 5 climate 
# scenarios and 10 climate models. The code extracts the data for each month 
# from 1901 to 2100. The code saves the data in CSV format for each month and 
# each model. The code also consolidates the data into a single CSV file for 
# each scenario and model. The code uses N cores to parallelize, which is chosen 
# by the user. Finally, the code saves the data in the 'Climate' directory.
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
# Set up logging ----
################################################################################

log_msg <- create_logger(file.path(logs_dir, "B02_extract_GCM_ADM1.log"))

log_msg("Starting script `B02 - Extract GCM tmp and prc data ADM1.R`")

################################################################################
# Make cluster ----
################################################################################

n_cores <- future::availableCores() 
# n_cores <- min(length(models), availableCores())
future::plan(multicore, workers = n_cores)

handlers(handler_progress(
  format = ":spin :current/:total [:bar] :percent :message"
))

################################################################################
# Loop over scenarios ----
################################################################################

for (scenario in scenarios) {
  # scenario = "historical"
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

  progressr::with_progress({
    p <- progressr::progressor(along = models)

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

          precip_rast <- terra::rast(file.path(
            climate_bc_cmip6_dir,
            scenario,
            prc_fn
          ))

          precip_dt <- extract_long(
            rast = precip_rast,
            polygons = cont,
            rast_times = as.character(terra::time(precip_rast)),
            value_name = "ppt"
          )
          temp_dt[, ppt := precip_dt$ppt]
          rm(precip_dt)

          data.table::fwrite(temp_dt, output_path)
        }

        p(message = sprintf("%s done", model))
        return(NULL)
      },
      future.seed = NULL
    ) 
  })
  log_msg(sprintf("Finished scenario: %s", scenario))
}
future::plan(sequential)

log_msg("Script `B02 - Extract GCM tmp and prc data ADM1.R` completed successfully")

################################################################################
# End of file ----
################################################################################
