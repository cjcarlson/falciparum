################################################################################
# This script predicts childhood malaria prevalence based on temperature and
# precipitation data with coefficients estimated from 1,000 draws of the
# variance-covariance matrix. Predictions are made on historical and future data
# based on 5 climate scenarios and 10 climate models. The code uses N cores to
# parallelize, which is chosen by the user. Predictions are summarized at several
# levels for plots and summary statistics.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  sf,
  zoo,
  here,
  arrow,
  future,
  tidyverse,
  lubridate,
  data.table,
  future.apply
)

n_cores <- min(1, future::availableCores())
options(future.globals.maxSize = 7 * 1024^3)

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

################################################################################
# Set up logging ----
################################################################################

log_msg <- create_logger()

log_msg("Starting script `E01 - Predict prevalence.R`")

log_msg(paste0("Using ", n_cores, " CPUs"))

################################################################################
# Precipitation thresholds ----
################################################################################

log_msg("Loading the precipitation key")

precip_dt <- precip_CRU_adm1_fp |>
  data.table::fread()

data.table::setnames(
  precip_dt,
  c("ppt_pctile0.9", "ppt_pctile0.1"),
  c("ppt.90", "ppt.10")
)

valid_ids <- unique(precip_dt$OBJECTID)

################################################################################
# Elevation data ----
################################################################################

log_msg("Loading the elevation key")

elev_dt <- elevation_summary_fp |>
  data.table::fread() |>
  dplyr::select(OBJECTID, elevmn)

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
  data.table::as.data.table() |>
  dplyr::left_join(elev_dt, by = join_by("OBJECTID"))

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
# Coefficients ----
################################################################################

coeffs_complete <- vcov_sample_mod_full_fn |>
  readr::read_csv(show_col_types = FALSE)

################################################################################
# Loop over modes ----
################################################################################

future::plan(multicore, workers = n_cores)

for (mode in c("historical", "future")) {
  log_msg(paste0("Starting: ", mode))

  ##############################################################################
  ## Directories and files ----
  if (mode == "historical") {
    start_date <- lubridate::ymd("1900-01-01")
    scen_subset <- names(historical_scenario_names)
    summary_dir <- hist_sum_dir
    row_years <- c(yr_1901, yr_2014)
  } else {
    start_date <- lubridate::ymd("2015-01-01")
    scen_subset <- names(future_scenario_names)
    summary_dir <- fut_sum_dir
    row_years <- c(yr_2015, yr_2050, yr_2100)
  }

  log_msg("Predictions summaries will be saved to: ")
  log_msg(paste0("    ", summary_dir))

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
  ## Metadata ----
  meta <- dt[, .(
    scenario,
    model,
    region,
    ISO,
    country,
    OBJECTID,
    elevmn,
    year,
    month,
    monthyr
  )]

  rows <- which((meta$scenario %in% scen_subset) & (meta$year %in% row_years))

  ##############################################################################
  ## Make predictions ----
  log_msg("Computing predictions")

  iter.list <- future.apply::future_lapply(
    1:1001,
    function(i) {
      # Prediction ----
      coef <- coeffs_complete[i, ]

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

      pred_data[, names(meta) := meta]
      pred_data[, run := as.character(run)]

      # Summaries ----
      # Scenario, model, and year ----
      scen_mod_yr <- pred_data[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, run)
      ]

      # Scenario, model, year, and region ----
      scen_mod_yr_reg <- pred_data[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, region, run)
      ]

      # High elevation regions ----
      high_el_data <- pred_data[elevmn > 1000]
      # Scenario, model, year, and high elev region ----
      scen_mod_yr_high_el_reg <- high_el_data[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, region, run)
      ]
      # Scenario, model, year, month, and region ----
      scen_mod_yr_mon_reg <- pred_data[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, month, region, run)
      ]

      # Scenario, model, year, and adm1 ----
      pred_data <- pred_data[rows, ]
      pred_data$year[pred_data$year %in% yr_1901] <- 1901
      pred_data$year[pred_data$year %in% yr_2014] <- 2014
      pred_data$year[pred_data$year %in% yr_2015] <- 2015
      pred_data$year[pred_data$year %in% yr_2050] <- 2050
      pred_data$year[pred_data$year %in% yr_2100] <- 2100

      scen_mod_yr_obj <- pred_data[,
        list(
          Pred = mean(Pred, na.rm = TRUE),
          Pf.temp = mean(Pf.temp, na.rm = TRUE),
          Pf.flood = mean(Pf.flood, na.rm = TRUE),
          Pf.drought = mean(Pf.drought, na.rm = TRUE)
        ),
        by = .(scenario, model, year, OBJECTID, run)
      ]

      if (i %% 100 == 0) {
        log_msg(paste0("Completed iteration: ", i))
      }

      return(
        list(
          scen_mod_yr = scen_mod_yr,
          scen_mod_yr_reg = scen_mod_yr_reg,
          scen_mod_yr_high_el_reg = scen_mod_yr_high_el_reg,
          scen_mod_yr_mon_reg = scen_mod_yr_mon_reg,
          scen_mod_yr_obj = scen_mod_yr_obj
        )
      )
    },
    future.seed = NULL
  )

  summaries <- c(
    "scen_mod_yr",
    "scen_mod_yr_reg",
    "scen_mod_yr_high_el_reg",
    "scen_mod_yr_mon_reg",
    "scen_mod_yr_obj"
  )
  rm(data, dt, meta)
  gc()
  for (sum_type in summaries) {
    out_path <- file.path(
      summary_dir,
      paste0(mode, "_vcov_pred_sum_", sum_type, ".feather")
    )
    compiled <- rbindlist(lapply(iter.list, `[[`, sum_type))
    arrow::write_feather(compiled, out_path)
    log_msg(sprintf("Wrote %s: %d rows\n", out_path, nrow(compiled)))
    rm(compiled)
    gc()
  }
}

future::plan(sequential)

log_msg("Script `E01 - Predict prevalence.R` completed successfully")

################################################################################
# End of file ----
################################################################################
