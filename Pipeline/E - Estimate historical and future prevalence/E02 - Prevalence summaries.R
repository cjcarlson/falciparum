################################################################################
#### Create time series summaries of historic and future prevalence estimates.
#### Extract the overall (scenario, model, year) and regional (scenario, model,
#### region, year) medians. Save the files to the TempFiles directory. These
#### summaries are used to create the time series in figures 2, 3, and 4.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  here,
  arrow,
  future,
  lubridate,
  tidyverse,
  progressr,
  data.table,
  future.apply
  # sf,
  # foreach,
)

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

n_cores <- min(30, future::availableCores())
options(future.globals.maxSize = 6 * 1024^3)
future::plan(future::multicore, workers = n_cores)

progressr::handlers(
  progressr::handler_progress(
    format = ":spin :current/:total (:percent) [:bar] ETA: :eta",
    width = 60
  )
)

################################################################################
# Set up logging ----
################################################################################

log_file_path <- file.path(logs_dir, "E02_summarise_prev.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script `E02 - Prevalence summaries.R`")

################################################################################
# Summarize prediction data ----
################################################################################

for (mode in c("historical", "future")) {
  # mode <- "historical"
  log_msg(paste0("Starting: ", mode))

  ##############################################################################
  # Directories and files ----
  ##############################################################################

  if (mode == "historical") {
    start_date <- lubridate::ymd("1900-01-01")
    prediction_dir <- hist_pred_dir
    summary_dir <- hist_sum_dir
    scen_subset <- names(historical_scenario_names)
    row_years <- c(yr_bins[["1901"]], yr_bins[["2014"]])
  } else {
    start_date <- lubridate::ymd("2015-01-01")
    prediction_dir <- fut_pred_dir
    summary_dir <- fut_sum_dir
    scen_subset <- names(future_scenario_names)
    row_years <- c(yr_bins[["2015"]], yr_bins[["2050"]], yr_bins[["2100"]])
  }

  log_msg("Loading metadata")

  meta <- file.path(prediction_dir, "RowMetadata.feather") |>
    arrow::read_feather()

  log_msg("Defining rows for the ADM1 level summaries used in maps")

  rows <- which((meta$scenario %in% scen_subset) & (meta$year %in% row_years))

  ##############################################################################
  # Data summaries ----
  ##############################################################################

  log_msg(paste0("Computing data summaries using: ", n_cores, " CPUs"))

  # iter.list <- progressr::with_progress({
  # i <- 1
  # p <- progressr::progressor(steps = 1000)
  iter.list <- future_lapply(
    1:1001,
    function(i) {
      pred_data <- file.path(
        prediction_dir,
        paste0("iter_", i, ".feather")
      ) |>
        arrow::read_feather() |>
        data.table::as.data.table()

      pred_data[, names(meta) := meta]
      pred_data[, run := as.character(run)]

      maps_iter <- pred_data[rows, ]
      maps_iter$year[maps_iter$year %in% yr_1901] <- 1901
      maps_iter$year[maps_iter$year %in% yr_2014] <- 2014
      maps_iter$year[maps_iter$year %in% yr_2015] <- 2015
      maps_iter$year[maps_iter$year %in% yr_2050] <- 2050
      maps_iter$year[maps_iter$year %in% yr_2100] <- 2100

      # Group meana - scenario, model, and year ----
      scen_mod_yr <- pred_data[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, run)
      ]
      # Group meana - scenario, model, year, and region ----
      scen_mod_yr_reg <- pred_data[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, region, run)
      ]
      # Group meana - scenario, model, year, month, and region ----
      scen_mod_yr_mon_reg <- pred_data[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, month, region, run)
      ]
      # Group meana - scenario, model, year, and country ----
      scen_mod_yr_obj <- maps_iter[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, OBJECTID, run)
      ]

      # scen_mod_yr$run <- i
      # scen_mod_yr_reg$run <- i
      # scen_mod_yr_mon_reg$run <- i
      # scen_mod_yr_obj$run <- i

      # p(sprintf("iter %d", i))

      if (i %% 100 == 0) {
        log_msg(paste0("Completed iteration: ", i))
      }

      return(
        list(
          scen_mod_yr = scen_mod_yr,
          scen_mod_yr_reg = scen_mod_yr_reg,
          scen_mod_yr_mon_reg = scen_mod_yr_mon_reg,
          scen_mod_yr_obj = scen_mod_yr_obj
        )
      )
    },
    future.seed = TRUE
  )
  # })

  ## Compile and save summaries ----
  log_msg("Compile results and save summary files")

  summaries <- c(
    "scen_mod_yr",
    "scen_mod_yr_reg",
    "scen_mod_yr_mon_reg",
    "scen_mod_yr_obj"
  )

  for (sum_type in summaries) {
    out_path <- file.path(
      summary_dir,
      paste0(mode, "_pred_sum_", sum_type, ".feather")
    )
    compiled <- rbindlist(lapply(iter.list, `[[`, sum_type))
    arrow::write_feather(compiled, out_path)
    log_msg(sprintf("Wrote %s: %d rows\n", out_path, nrow(compiled)))
  }
}

future::plan(future::sequential)

log_msg("Script `E02 - Prevalence summaries.R` completed successfully")

################################################################################
# End of file ----
################################################################################
