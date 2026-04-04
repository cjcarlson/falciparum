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

numCores <- min(12, future::availableCores())
options(future.globals.maxSize = 6 * 1024^3)
future::plan(future::multicore, workers = numCores)

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

  log_msg(paste0("Computing data summaries using: ", numCores, " CPUs"))

  iter.list <- progressr::with_progress({
    # i <- 1
    p <- progressr::progressor(steps = 1000)
    future_lapply(
      1:1000,
      function(i) {
        pred_data <- file.path(
          prediction_dir,
          paste0("iter_", i, ".feather")
        ) |>
          arrow::read_feather() |>
          data.table::as.data.table()

        pred_data[, names(meta) := meta]

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
          by = .(scenario, model, year)
        ]
        # Group meana - scenario, model, year, and region ----
        scen_mod_yr_reg <- pred_data[,
          list(
            Pred = mean(Pred, na.rm = TRUE),
            Pf.temp = mean(Pf.temp, na.rm = TRUE),
            Pf.flood = mean(Pf.flood, na.rm = TRUE),
            Pf.drought = mean(Pf.drought, na.rm = TRUE)
          ),
          by = .(scenario, model, year, region)
        ]
        # Group meana - scenario, model, year, month, and region ----
        scen_mod_yr_mon_reg <- pred_data[,
          list(
            Pred = mean(Pred, na.rm = TRUE),
            Pf.temp = mean(Pf.temp, na.rm = TRUE),
            Pf.flood = mean(Pf.flood, na.rm = TRUE),
            Pf.drought = mean(Pf.drought, na.rm = TRUE)
          ),
          by = .(scenario, model, year, month, region)
        ]
        # Group meana - scenario, model, year, and country ----
        scen_mod_yr_obj <- maps_iter[,
          list(
            Pred = mean(Pred, na.rm = TRUE),
            Pf.temp = mean(Pf.temp, na.rm = TRUE),
            Pf.flood = mean(Pf.flood, na.rm = TRUE),
            Pf.drought = mean(Pf.drought, na.rm = TRUE)
          ),
          by = .(scenario, model, year, OBJECTID)
        ]

        scen_mod_yr$run <- i
        scen_mod_yr_reg$run <- i
        scen_mod_yr_mon_reg$run <- i
        scen_mod_yr_obj$run <- i

        p(sprintf("iter %d", i))

        return(list(
          scen_mod_yr = scen_mod_yr,
          scen_mod_yr_reg = scen_mod_yr_reg,
          scen_mod_yr_mon_reg = scen_mod_yr_mon_reg,
          scen_mod_yr_obj = scen_mod_yr_obj
        ))
      },
      future.seed = TRUE
    )
  })

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

# ##############################################################################
# ## Overall medians ----
# ##############################################################################

# scen_yr_mean <- bind_rows(lapply(iter.list, function(x) x[[1]])) |>
#   tibble::as_tibble()

# scen_yr_mean |>
#   dplyr::filter(year %in% overall_yr_filter) |>
#   dplyr::group_by(model, run, scenario) |>
#   dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
#   dplyr::right_join(scen_yr_mean) |>
#   dplyr::mutate(Pred = (Pred - BetaMean)) |>
#   dplyr::select(-BetaMean) -> df

# df |>
#   dplyr::group_by(scenario, year) |>
#   dplyr::summarize(
#     median = median(Pred, na.rm = TRUE),
#     upper = quantile(Pred, 0.95, na.rm = TRUE),
#     lower = quantile(Pred, 0.05, na.rm = TRUE)
#   ) -> hist.to.graph

# print(paste0("Saving: ", overall_fn))
# readr::write_csv(hist.to.graph, here::here("TempFiles", overall_fn))

# ##############################################################################
# ## Regional medians ----
# ##############################################################################

# scen_yr_reg_mean <- bind_rows(lapply(iter.list, function(x) x[[2]])) |>
#   tibble::as_tibble()

# scen_yr_reg_mean |>
#   dplyr::filter(year %in% region_yr_filter) |>
#   dplyr::group_by(scenario, model, region, run) |>
#   dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
#   dplyr::right_join(scen_yr_reg_mean) |>
#   dplyr::mutate(Pred = (Pred - BetaMean)) |>
#   dplyr::select(-BetaMean) -> df

# df |>
#   dplyr::group_by(scenario, region, year) |>
#   dplyr::summarize(
#     median = median(Pred, na.rm = TRUE),
#     upper = quantile(Pred, 0.95, na.rm = TRUE),
#     lower = quantile(Pred, 0.05, na.rm = TRUE)
#   ) -> data.to.graph

# print(paste0("Saving: ", region_fn))
# readr::write_csv(data.to.graph, here::here("TempFiles", region_fn))

# ##############################################################################
# ## ADM1 means ----
# ##############################################################################

# print(paste0("Saving: ", map_fn))
# scen_yr_adm_mean <- bind_rows(lapply(iter.list, function(x) x[[3]])) |>
#   tibble::as_tibble() |>
#   arrow::write_feather(here::here("TempFiles", map_fn))
