#############################################################################-
#### Use the following code to predict prevalence based on temperature and
#### precipitation data with coefficients estimated from 1,000 bootstrap models.
#### This code makes historical and future predictions based on 5 climate
#### scenarios and 10 climate models. The code uses N cores to parallelize,
#### which is chosen by the user. The code saves the predictions in feather
#### format for fast reading and writing and reduces the size of the data.
#### Additionally, metadata is saved in a separate file to reduce file size.
#### -----------------------------------------------------------------------.
#### Written by: Cullen Molitor
#### Date: 2024-06-29
#### Email: cullen_molitor@ucsb.edu
#############################################################################-

############################################################
# Set up ----
############################################################

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  zoo,
  here,
  terra,
  tictoc,
  progressr,
  tidyverse,
  lubridate,
  data.table,
  future.apply
)

overwrite <- TRUE
numCores <- 10
options(future.globals.maxSize = 6 * 1024^3) # 4 GiB

handlers(handler_progress(
  format = ":spin :current/:total (:percent) [:bar] ETA: :eta",
  width = 60
))

tictoc::tic("Total execution time")

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(A_utils_calc_fp)

############################################################
# Country data ----
############################################################

countrydf <- ADM1_fp |>
  sf::read_sf() |>
  tibble::as_tibble() |>
  dplyr::select(OBJECTID, NAME_0) |>
  dplyr::distinct() |>
  dplyr::rename(country = NAME_0) |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

############################################################
# Region data ----
############################################################

gboddf <- world_regions_fp |>
  sf::read_sf() |>
  tibble::as_tibble() |>
  dplyr::select("ISO", "NAME_0", "SmllRgn") |>
  dplyr::group_by(ISO, NAME_0) |>
  dplyr::summarize(SmllRgn = first(SmllRgn)) |>
  dplyr::rename(country = "NAME_0", region = "SmllRgn") |>
  dplyr::mutate(country = gsub('Cote D\'Ivoire', 'Côte d\'Ivoire', country))

############################################################
# Precipitation thresholds ----
############################################################

precip.key <- precip_fp |>
  readr::read_csv(show_col_types = FALSE)

############################################################
# Bootstrap coefficients ----
############################################################

bootstrap <- boot_mod_full_fn |>
  readRDS() |>
  tibble::as_tibble()

############################################################
# Loop over modes ----
############################################################

for (mode in c("future")) {
  # for (mode in c("historical", "future")) {

  # mode <- "historical"

  print(paste0("Starting: ", mode))
  tictoc::tic(paste0("Finished: ", mode))

  ## Directories and files ----
  if (mode == "historical") {
    idx <- c(1, 2)
    start_date <- lubridate::ymd("1900-01-01")
    results_dir <- iter_hist_dir
    summary_dir <- summ_hist_dir
    scen_subset <- scenarios[1:2]
    row_years <- c(yr_bins[["1901"]], yr_bins[["2014"]])
  } else {
    idx <- c(3, 4, 5)
    start_date <- lubridate::ymd("2015-01-01")
    results_dir <- iter_futu_dir
    summary_dir <- summ_futu_dir
    scen_subset <- scenarios[3:5]
    row_years <- c(yr_bins[["2015"]], yr_bins[["2050"]], yr_bins[["2100"]])
  }

  files <- vector("character")

  for (dir in scenarios[idx]) {
    files <- c(
      files,
      list.files(file.path(data_dir, "Climate", dir), full.names = TRUE)
    )
  }

  plan(multisession, workers = numCores)

  ## Climate Scenario data ----

  # Pre-convert join tables
  countrydt <- as.data.table(countrydf)
  gboddt <- as.data.table(gboddf)
  valid_ids <- unique(precip.key$OBJECTID)
  exclude_regions <- c("Asia (Southeast)", "North Africa & Middle East")

  data <- rbindlist(
    with_progress({
      p <- progressor(along = files)
      future_lapply(
        files,
        function(f) {
          dt <- fread(f)
          dt[, `:=`(
            scenario = basename(dirname(f)),
            model = tools::file_path_sans_ext(basename(f))
          )]

          # Drop NAs early to reduce size before join
          dt <- dt[complete.cases(dt)]

          # Filter to valid IDs early
          dt <- dt[OBJECTID %in% valid_ids]

          # Joins
          dt <- countrydt[dt, on = "OBJECTID", nomatch = NULL]
          dt <- gboddt[dt, on = "country", nomatch = NULL]

          # Filter regions
          dt <- dt[!region %in% exclude_regions]

          # Date computation
          dt[,
            monthyr := as.Date(zoo::as.yearmon(paste(month, year, sep = " ")))
          ]
          dt[, monthyr := as.numeric(monthyr - start_date)]

          p(sprintf("Read %s", basename(f)))

          # Select columns to minimize memory for rbindlist
          dt[, .(
            scenario,
            model,
            region,
            ISO,
            country,
            OBJECTID,
            month,
            year,
            monthyr,
            temp,
            temp2,
            ppt
          )]
        },
        future.seed = NULL
      )
    })
  )

  ## Prepare data for predictions ----
  precipdt <- as.data.table(precip.key)
  setnames(precipdt, c("ppt_pctile0.9", "ppt_pctile0.1"), c("ppt.90", "ppt.10"))

  dt <- precipdt[data, on = "OBJECTID", nomatch = NULL]
  setorder(dt, OBJECTID, scenario, model, monthyr)

  # Compute lagged flood/drought indicators (shared across all iterations)
  dt[, `:=`(
    flood = as.numeric(ppt >= ppt.90),
    drought = as.numeric(ppt <= ppt.10)
  )]

  dt[,
    `:=`(
      flood.lag = shift(flood, n = 1, type = "lag"),
      flood.lag2 = shift(flood, n = 2, type = "lag"),
      flood.lag3 = shift(flood, n = 3, type = "lag"),
      drought.lag = shift(drought, n = 1, type = "lag"),
      drought.lag2 = shift(drought, n = 2, type = "lag"),
      drought.lag3 = shift(drought, n = 3, type = "lag")
    ),
    by = .(OBJECTID, scenario, model)
  ]

  ### Save metadata ----
  meta_path <- file.path(results_dir, "RowMetadata.feather")
  if (!file.exists(meta_path) | overwrite) {
    meta <- dt[, .(
      OBJECTID,
      monthyr,
      month,
      year,
      scenario,
      model,
      country,
      ISO,
      region
    )]

    meta |> arrow::write_feather(meta_path)
    meta <- meta[, .(OBJECTID, year, month, scenario, model, region)]
  }

  ## Apply model coefficients ----
  results <- with_progress({
    p <- progressor(steps = 1000)
    future_lapply(
      1:1000,
      function(i) {
        file_name <- paste0("iter_", i, ".feather")
        file_path <- file.path(results_dir, file_name)

        if (!file.exists(file_path) | overwrite) {
          tictoc::tic(i)
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

          pred_data <- data.table(
            Pred = Pred,
            Pf.temp = Pf.temp,
            Pf.flood = Pf.flood,
            Pf.drought = Pf.drought
          )

          pred_data |> arrow::write_feather(file_path)

          pred_data[, names(meta) := meta]

          ## Scen, mod, yr summary
          scen_yr_mean <- pred_data[,
            .(Pred = mean(Pred, na.rm = TRUE)),
            by = .(scenario, model, year)
          ]
          scen_yr_mean[, run := i]

          ## Scen, mod, yr, reg summary
          scen_mod_yr_reg_mean <- pred_data[,
            .(Pred = mean(Pred, na.rm = TRUE)),
            by = .(scenario, model, year, region)
          ]
          scen_mod_yr_reg_mean[, run := i]

          ## Scen, mod, yr, obj summary
          map_row_idx <- which(
            meta$scenario %in% scen_subset & meta$year %in% row_years
          )
          maps_dt <- pred_data[map_row_idx]
          maps_dt[, year := yr_lookup[as.character(year)]]

          scen_yr_adm_mean <- maps_dt[,
            .(Pred = mean(Pred, na.rm = TRUE)),
            by = .(scenario, model, year, OBJECTID)
          ]
          scen_yr_adm_mean[, run := i]

          tictoc::toc()

          p(sprintf("iter_%d", i))

          list(
            scen_mod_yr = scen_yr_mean,
            scen_mod_yr_reg = scen_mod_yr_reg_mean,
            scen_mod_yr_obj = scen_yr_adm_mean
          )
        } else {
          p(sprintf("iter_%d (skipped)", i))
          NULL
        }
      },
      future.seed = NULL
    )
  })

  plan(sequential)

  ## Compile and save summaries ----
  # Drop NULLs (skipped iterations)
  results <- results[!vapply(results, is.null, logical(1))]

  for (stype in c("scen_mod_yr", "scen_mod_yr_reg", "scen_mod_yr_obj")) {
    compiled <- rbindlist(lapply(results, `[[`, stype))
    out_path <- file.path(summary_dir, paste0(stype, ".feather"))
    dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
    arrow::write_feather(compiled, out_path)
    cat(sprintf("Wrote %s: %d rows\n", out_path, nrow(compiled)))
  }

  tictoc::toc()
  
  # ## Apply model coefficients ----
  # with_progress({
  #   p <- progressor(steps = 1000)
  #   future_lapply(
  #     1:1000,
  #     function(i) {
  #       # i <- 1
  #       file_name <- paste0("iter_", i, ".feather")
  #       file_path <- file.path(results_dir, file_name)

  #       if (!file.exists(file_path) | overwrite) {
  #         tictoc::tic(i)
  #         coef <- bootstrap[i, ]

  #         Pf.temp <- coef$temp * dt$temp + coef$temp2 * dt$temp2

  #         Pf.flood <- (coef[["flood"]] *
  #           dt$flood +
  #           coef[["flood.lag"]] * dt$flood.lag +
  #           coef[["flood.lag2"]] * dt$flood.lag2 +
  #           coef[["flood.lag3"]] * dt$flood.lag3)

  #         Pf.drought <- (coef[["drought"]] *
  #           dt$drought +
  #           coef[["drought.lag"]] * dt$drought.lag +
  #           coef[["drought.lag2"]] * dt$drought.lag2 +
  #           coef[["drought.lag3"]] * dt$drought.lag3)

  #         Pf.prec <- Pf.flood + Pf.drought
  #         Pred <- Pf.temp + Pf.prec

  #         pred_data <- data.table(
  #           Pred = Pred,
  #           Pf.temp = Pf.temp,
  #           Pf.flood = Pf.flood,
  #           Pf.drought = Pf.drought
  #         )

  #         sprintf("Writing: %s", file_name)
  #         pred_data |> arrow::write_feather(file_path)

  #         pred_data[, names(meta) := meta]

  #         ############################################################
  #         # Scen, mod, yr summary ----
  #         ############################################################

  #         scen_yr_mean <- pred_data[,
  #           .(Pred = mean(Pred, na.rm = TRUE)),
  #           by = .(scenario, model, year)
  #         ]
  #         scen_yr_mean[, run := i]

  #         sum_s_m_y_file_name <- paste0("sum_iter_", i, ".feather")
  #         sum_s_m_y_file_path <- file.path(
  #           summary_dir,
  #           "scen_mod_yr",
  #           sum_s_m_y_file_name
  #         )
  #         dir.create(
  #           dirname(sum_s_m_y_file_path),
  #           showWarnings = FALSE,
  #           recursive = TRUE
  #         )

  #         sprintf("Writing: %s", sum_s_m_y_file_name)
  #         scen_yr_mean |> arrow::write_feather(sum_s_m_y_file_path)

  #         ############################################################
  #         # Scen, mod, yr, reg summary ----
  #         ############################################################

  #         scen_mod_yr_reg_mean <- pred_data[,
  #           .(Pred = mean(Pred, na.rm = TRUE)),
  #           by = .(scenario, model, year, region)
  #         ]
  #         scen_mod_yr_reg_mean[, run := i]

  #         sum_s_m_y_r_file_name <- paste0("sum_iter_", i, ".feather")
  #         sum_s_m_y_r_file_path <- file.path(
  #           summary_dir,
  #           "scen_mod_yr_reg",
  #           sum_s_m_y_r_file_name
  #         )
  #         dir.create(
  #           dirname(sum_s_m_y_r_file_path),
  #           showWarnings = FALSE,
  #           recursive = TRUE
  #         )

  #         sprintf("Writing: %s", sum_s_m_y_r_file_name)
  #         scen_mod_yr_reg_mean |> arrow::write_feather(sum_s_m_y_r_file_path)

  #         ############################################################
  #         # Scen, mod, yr, obj summary ----
  #         ############################################################

  #         map_row_idx <- which(
  #           meta$scenario %in% scen_subset & meta$year %in% row_years
  #         )
  #         maps_dt <- pred_data[map_row_idx]
  #         maps_dt[, year := yr_lookup[as.character(year)]]

  #         scen_yr_adm_mean <- maps_dt[,
  #           .(Pred = mean(Pred, na.rm = TRUE)),
  #           by = .(scenario, model, year, OBJECTID)
  #         ]
  #         scen_yr_adm_mean[, run := i]

  #         sum_s_m_y_o_file_name <- paste0("sum_iter_", i, ".feather")
  #         sum_s_y_m_o_file_path <- file.path(
  #           summary_dir,
  #           "scen_mod_yr_obj",
  #           sum_s_m_y_o_file_name
  #         )
  #         dir.create(
  #           dirname(sum_s_y_m_o_file_path),
  #           showWarnings = FALSE,
  #           recursive = TRUE
  #         )

  #         sprintf("Writing: %s", sum_s_m_y_o_file_name)
  #         scen_yr_adm_mean |> arrow::write_feather(sum_s_y_m_o_file_path)
  #         tictoc::toc()
  #       }

  #       sprintf("iter_%d", i)
  #       return(NULL)
  #     },
  #     future.seed = NULL
  #   )
  # })

  # plan(sequential)
  # tictoc::toc()
}
tictoc::toc()

############################################################
# End of file ----
############################################################
