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
  foreach,
  tidyverse,
  lubridate,
  data.table,
  doParallel,
  future.apply
)

overwrite <- FALSE
numCores <- 10

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

for (mode in c("historical", "future")) {
  # mode <- "historical"
  print(paste0("Starting: ", mode))
  tictoc::tic(paste0("Finished: ", mode))

  ## Directories and files ----
  if (mode == "historical") {
    idx <- c(1, 2)
    start_date <- lubridate::ymd("1900-01-01")
    results_dir <- iter_hist_dir
  } else {
    idx <- c(3, 4, 5)
    start_date <- lubridate::ymd("2015-01-01")
    results_dir <- iter_futu_dir
  }

  files <- vector("character")

  for (dir in scenarios[idx]) {
    files <- c(
      files,
      list.files(file.path(data_dir, "Climate", dir), full.names = TRUE)
    )
  }

  plan(multisession, workers = 12)

  ## Climate Scenario data ----

  # Pre-convert join tables
  countrydt <- as.data.table(countrydf)
  gboddt <- as.data.table(gboddf)
  valid_ids <- unique(precip.key$OBJECTID)
  exclude_regions <- c("Asia (Southeast)", "North Africa & Middle East")

  data <- rbindlist(
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
        dt[, monthyr := as.Date(zoo::as.yearmon(paste(month, year, sep = " ")))]
        dt[, monthyr := as.numeric(monthyr - start_date)]

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
    dt[, .(
      OBJECTID,
      monthyr,
      month,
      year,
      scenario,
      model,
      country,
      ISO,
      region
    )] |>
      arrow::write_feather(meta_path)
  }

  plan(sequential)

  ## Make cluster ----
  cl <- makeCluster(numCores, outfile = "")
  registerDoParallel(cl)

  ## Apply model coefficients ----
  # for (i in c(1:1000)) { # i <- 1
  foreach(
    i = 1:1000,
    .export = c("results_dir", "dt", "bootstrap")
  ) %dopar%
    {
      tictoc::tic(i)

      file_name <- paste0("iter_", i, ".feather")
      file_path <- file.path(results_dir, file_name)
      # meta_path <- file.path(results_dir, "RowMetadata.feather")

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

        data.table(
          Pred = Pred,
          Pf.temp = Pf.temp,
          Pf.flood = Pf.flood,
          Pf.drought = Pf.drought
        ) |>
          arrow::write_feather(file_path)
      }
      #   coef <- bootstrap[i, ]

      #   iter.df <- data |>
      #     dplyr::left_join(precip.key, by = "OBJECTID") |>
      #     dplyr::rename(ppt.90 = ppt_pctile0.9, ppt.10 = ppt_pctile0.1) |>
      #     dplyr::group_by(OBJECTID, scenario, model) |>
      #     dplyr::mutate(
      #       Pf.temp = coef$temp * temp + coef$temp2 * temp2,
      #       flood = as.numeric(ppt >= ppt.90),
      #       drought = as.numeric(ppt <= ppt.10),
      #       flood.lag = dplyr::lag(flood, order_by = monthyr),
      #       flood.lag2 = dplyr::lag(flood, order_by = monthyr, n = 2),
      #       flood.lag3 = dplyr::lag(flood, order_by = monthyr, n = 3),
      #       drought.lag = dplyr::lag(drought, order_by = monthyr),
      #       drought.lag2 = dplyr::lag(drought, order_by = monthyr, n = 2),
      #       drought.lag3 = dplyr::lag(drought, order_by = monthyr, n = 3),
      #       Pf.flood = (coef[['flood']] *
      #         flood +
      #         coef[['flood.lag']] * flood.lag +
      #         coef[['flood.lag2']] * flood.lag2 +
      #         coef[['flood.lag3']] * flood.lag3),
      #       Pf.drought = (coef[['drought']] *
      #         drought +
      #         coef[['drought.lag']] * drought.lag +
      #         coef[['drought.lag2']] * drought.lag2 +
      #         coef[['drought.lag3']] * drought.lag3),
      #       Pf.prec = (Pf.flood + Pf.drought),
      #       Pred = (Pf.temp + Pf.prec)
      #     ) |>
      #     dplyr::ungroup()

      #   ### Save metadata ----
      #   if (i == 1) {
      #     iter.df |>
      #       dplyr::select(
      #         OBJECTID,
      #         monthyr,
      #         month,
      #         year,
      #         scenario,
      #         model,
      #         country,
      #         ISO,
      #         region
      #       ) |>
      #       arrow::write_feather(meta_path)
      #   }
      #   ### Save predictions ----
      #   iter.df |>
      #     dplyr::select(
      #       Pred,
      #       Pf.temp,
      #       Pf.flood,
      #       Pf.drought
      #     ) |>
      #     arrow::write_feather(file_path)

      #   tictoc::toc()
      # } else {
      #   tictoc::toc()
      #   return(NULL)
      # }
    }
  tictoc::toc()
  stopCluster(cl)
}
tictoc::toc()

############################################################
# End of file ----
############################################################
