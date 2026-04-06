################################################################################
# Use the following code to predict prevalence based on temperature and
# precipitation data with coefficients estimated from 1,000 bootstrap models.
# This code makes historical and future predictions based on 5 climate
# scenarios and 10 climate models. The code uses N cores to parallelize, which
# is chosen by the user. The code saves the predictions in feather format for
# fast reading and writing and reduces the size of the data. Additionally,
# metadata is saved in a separate file to reduce file size.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  zoo,
  here,
  terra,
  future,
  tidyverse,
  lubridate,
  data.table,
  future.apply
)

overwrite <- TRUE
n_cores <- min(10, future::availableCores())
options(future.globals.maxSize = 6 * 1024^3)

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

################################################################################
# Set up logging ----
################################################################################

log_file_path <- file.path(logs_dir, "E01_predict_prev.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script `E01 - Predict prevalence.R`")

log_msg(paste0("Using ", n_cores, " CPUs"))

################################################################################
# Precipitation thresholds ----
################################################################################

log_msg("Loading the precipitation")

precip_dt <- precip_CRU_adm1_fp |>
  data.table::fread()

data.table::setnames(
  precip_dt,
  c("ppt_pctile0.9", "ppt_pctile0.1"),
  c("ppt.90", "ppt.10")
)

valid_ids <- unique(precip_dt$OBJECTID)

################################################################################
# Country data ----
################################################################################

log_msg("Loading ADM1 data")

country_dt <- ADM1_fp |>
  sf::read_sf() |>
  tibble::as_tibble() |>
  dplyr::select(OBJECTID, NAME_0) |>
  dplyr::distinct() |>
  dplyr::rename(country = NAME_0) |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID)) |>
  dplyr::filter(OBJECTID %in% valid_ids) |>
  data.table::as.data.table()

################################################################################
# Region data ----
################################################################################

log_msg("Loading GBD region data")

gbod_dt <- gbd_fp |>
  sf::read_sf() |>
  tibble::as_tibble() |>
  dplyr::select("ISO", "NAME_0", "SmllRgn") |>
  dplyr::group_by(ISO, NAME_0) |>
  dplyr::summarize(SmllRgn = first(SmllRgn)) |>
  dplyr::rename(country = "NAME_0", region = "SmllRgn") |>
  dplyr::mutate(country = gsub('Cote D\'Ivoire', 'Côte d\'Ivoire', country)) |>
  dplyr::filter(region %in% names(region_names)) |>
  data.table::as.data.table()

spatial_dt <- gbod_dt[country_dt, on = "country", nomatch = NULL]

################################################################################
# Bootstrap coefficients ----
################################################################################

log_msg("Loading bootstrap coeffs")

bootstrap <- boot_mod_full_fn |>
  readr::read_csv(show_col_types = FALSE)

################################################################################
# Loop over modes ----
################################################################################

future::plan(multisession, workers = n_cores)

for (mode in c("historical", "future")) {
  # mode <- "historical"

  log_msg(paste0("Starting: ", mode))

  ##############################################################################
  ## Directories and files ----
  if (mode == "historical") {
    start_date <- lubridate::ymd("1900-01-01")
    scen_subset <- names(historical_scenario_names)
    prediction_dir <- hist_pred_dir
    summary_dir <- hist_sum_dir
    row_years <- c(yr_bins[["1901"]], yr_bins[["2014"]])
  } else {
    start_date <- lubridate::ymd("2015-01-01")
    scen_subset <- names(future_scenario_names)
    prediction_dir <- fut_pred_dir
    summary_dir <- fut_sum_dir
    row_years <- c(yr_bins[["2015"]], yr_bins[["2050"]], yr_bins[["2100"]])
  }

  log_msg("Predictions will be saved to: ")
  log_msg(paste0("    ", prediction_dir))

  files <- vector("character")

  for (dir in scen_subset) {
    files <- c(
      files,
      list.files(file.path(inter_cmip6_ext_dir, dir), full.names = TRUE)
    )
  }

  log_msg("Loading climate model data")

  ##############################################################################
  ## Climate model data ----
  data <- data.table::rbindlist(
    future.apply::future_lapply(
      files,
      function(f) {
        dt <- data.table::fread(f, showProgress = FALSE)
        dt <- dt[OBJECTID %in% valid_ids]
        dt <- dt[complete.cases(dt)]

        dt[, `:=`(
          scenario = basename(dirname(f)),
          model = tools::file_path_sans_ext(basename(f))
        )]

        dt[, .(scenario, model, OBJECTID, year, month, temp, temp2, ppt)]
      },
      future.seed = NULL
    )
  )

  ##############################################################################
  ## Join to spatial data ----
  log_msg("Joining climate data to spatial data")
  data <- spatial_dt[data, on = "OBJECTID", nomatch = NULL]

  ##############################################################################
  ## Create monthyr var----
  log_msg("Creating monthyr variable")
  data[, monthyr := as.Date(zoo::as.yearmon(paste(month, year, sep = " ")))]
  data[, monthyr := as.numeric(monthyr - start_date)]

  ##############################################################################
  ## Join to precip key ----
  log_msg("Joining climate data to precipitation key")
  dt <- precip_dt[data, on = "OBJECTID", nomatch = NULL]
  data.table::setorder(dt, OBJECTID, scenario, model, monthyr)

  ##############################################################################
  ## Compute flood/drought ----
  log_msg("Computing flood and droughts")
  dt[, `:=`(
    flood = as.numeric(ppt >= ppt.90),
    drought = as.numeric(ppt <= ppt.10)
  )]

  ##############################################################################
  ## Compute flood/drought lags ----
  log_msg("Computing flood and drought lags")
  dt[,
    `:=`(
      flood.lag = data.table::shift(flood, n = 1, type = "lag"),
      flood.lag2 = data.table::shift(flood, n = 2, type = "lag"),
      flood.lag3 = data.table::shift(flood, n = 3, type = "lag"),
      drought.lag = data.table::shift(drought, n = 1, type = "lag"),
      drought.lag2 = data.table::shift(drought, n = 2, type = "lag"),
      drought.lag3 = data.table::shift(drought, n = 3, type = "lag")
    ),
    by = .(OBJECTID, scenario, model)
  ]

  ##############################################################################
  ## Save metadata ----
  meta_path <- file.path(prediction_dir, "RowMetadata.feather")
  if (!file.exists(meta_path) | overwrite) {
    log_msg("Saving metadata to save on prediction file size")
    meta <- dt[, .(
      scenario,
      model,
      region,
      ISO,
      country,
      OBJECTID,
      year,
      month,
      monthyr
    )]

    meta |> arrow::write_feather(meta_path)
    meta <- meta[, .(OBJECTID, year, month, scenario, model, region)]
  }

  ##############################################################################
  ## Apply model coefficients ----
  log_msg("Computing predictions")

  future.apply::future_lapply(
    1:1001,
    function(i) {
      file_name <- paste0("iter_", i, ".feather")
      file_path <- file.path(prediction_dir, file_name)

      if (!file.exists(file_path) | overwrite) {
        coef <- bootstrap[i, ]

        Pf.temp <- coef$temp * dt$temp + coef$temp2 * dt$temp2

        Pf.flood <- (coef[["flood"]] *
          dt$flood +
          coef[["flood.lag"]] * dt$flood.lag +
          coef[["flood.lag2"]] * dt$flood.lag2 +
          coef[["flood.lag3"]] * dt$flood.lag3)

        Pf.drought <- (coef[["drought"]] *
          dt$drought +
          coef[["drought.lag"]] * dt$drought.lag +
          coef[["drought.lag2"]] * dt$drought.lag2 +
          coef[["drought.lag3"]] * dt$drought.lag3)

        Pf.prec <- Pf.flood + Pf.drought
        Pred <- Pf.temp + Pf.prec

        pred_data <- data.table::data.table(
          Pred = Pred,
          Pf.temp = Pf.temp,
          Pf.flood = Pf.flood,
          Pf.drought = Pf.drought,
          run = coef$model
        )

        pred_data |> arrow::write_feather(file_path)

        if (i %% 100 == 0) {
          log_msg(paste0("Completed iteration: ", i))
        }

      } else {
        NULL
      }
    },
    future.seed = NULL
  )
}

future::plan(sequential)

log_msg("Script `E01 - Predict prevalence.R` completed successfully")

################################################################################
# End of file ----
################################################################################

