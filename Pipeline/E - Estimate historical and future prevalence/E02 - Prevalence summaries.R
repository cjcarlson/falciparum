#############################################################################-
#### Create time series summaries of historic and future prevalence estimates.
#### Extract the overall (scenario, model, year) and regional (scenario, model,
#### region, year) medians. Save the files to the TempFiles directory. These
#### summaries are used to create the time series in figures 2, 3, and 4.
#### -----------------------------------------------------------------------.
#### Written by: Cullen Molitor
#### Date: 2024-06-29
#### Email: cullen_molitor@ucsb.edu
#############################################################################-

####
#### Setup ----
####

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  sf,
  here,
  tictoc,
  foreach,
  tidyverse,
  lubridate,
  doParallel,
  data.table
)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

for (mode in c("historical", "future")) {
  # mode <- "historical"
  print(paste0("Starting: ", mode))
  # tictoc::tic(paste0("Finished: ", mode))

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

  summary_files <- list.files(list.dirs(summary_dir)[2:4], full.names = T)
  
  tictoc::tic()
  summary_data <- data.table::rbindlist(lapply(summary_files[1:1000], arrow::read_feather))
  tictoc::toc()

  for (i in seq_along(summary_files)){
    # i <- 1
    sum_file <- summary_files[i]
    sum <- arrow::read_feather(sum_file)
    file_list <- list(file_list, sum)
  }

  data.table::rbindlist(file_list)

  ####
  #### Overall medians ----
  ####

  # iter.list <-
    scen_yr_mean <- bind_rows(lapply(iter.list, function(x) x[[1]])) |>
      tibble::as_tibble()

  scen_yr_mean |>
    dplyr::filter(year %in% overall_yr_filter) |>
    dplyr::group_by(model, run, scenario) |>
    dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
    dplyr::right_join(scen_yr_mean) |>
    dplyr::mutate(Pred = (Pred - BetaMean)) |>
    dplyr::select(-BetaMean) -> df

  df |>
    dplyr::group_by(scenario, year) |>
    dplyr::summarize(
      median = median(Pred, na.rm = TRUE),
      upper = quantile(Pred, 0.95, na.rm = TRUE),
      lower = quantile(Pred, 0.05, na.rm = TRUE)
    ) -> hist.to.graph

  print(paste0("Saving: ", overall_fn))
  readr::write_csv(hist.to.graph, here::here("TempFiles", overall_fn))

  ####
  #### Regional medians ----
  ####

  scen_yr_reg_mean <- bind_rows(lapply(iter.list, function(x) x[[2]])) |>
    tibble::as_tibble()

  scen_yr_reg_mean |>
    dplyr::filter(year %in% region_yr_filter) |>
    dplyr::group_by(scenario, model, region, run) |>
    dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
    dplyr::right_join(scen_yr_reg_mean) |>
    dplyr::mutate(Pred = (Pred - BetaMean)) |>
    dplyr::select(-BetaMean) -> df

  df |>
    dplyr::group_by(scenario, region, year) |>
    dplyr::summarize(
      median = median(Pred, na.rm = TRUE),
      upper = quantile(Pred, 0.95, na.rm = TRUE),
      lower = quantile(Pred, 0.05, na.rm = TRUE)
    ) -> data.to.graph

  print(paste0("Saving: ", region_fn))
  readr::write_csv(data.to.graph, here::here("TempFiles", region_fn))

  ####
  #### ADM1 means ----
  ####

  print(paste0("Saving: ", map_fn))
  scen_yr_adm_mean <- bind_rows(lapply(iter.list, function(x) x[[3]])) |>
    tibble::as_tibble() |>
    arrow::write_feather(here::here("TempFiles", map_fn))
}


# # #############################################################################-
# #### Create time series summaries of historic and future prevalence estimates.
# #### Extract the overall (scenario, model, year) and regional (scenario, model,
# #### region, year) medians. Save the files to the TempFiles directory. These
# #### summaries are used to create the time series in figures 2, 3, and 4.
# #### -----------------------------------------------------------------------.
# #### Written by: Cullen Molitor
# #### Date: 2024-06-29
# #### Email: cullen_molitor@ucsb.edu
# #############################################################################-

# ####
# #### Setup ----
# ####

# rm(list = ls())

# # packages
# if (!require("pacman")) {
#   install.packages("pacman")
# }

# pacman::p_load(
#   sf,
#   here,
#   tictoc,
#   foreach,
#   tidyverse,
#   lubridate,
#   doParallel,
#   data.table
# )

# tictoc::tic("Total execution time")

# source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

# yr_1901 <- 1901:1905
# yr_2014 <- 2010:2014
# yr_2015 <- 2015:2019
# yr_2050 <- 2048:2052
# yr_2100 <- 2096:2100

# num_cores <- 10
# log_file <- here::here("TempFiles", "parallel_log.txt")
# cl <- makeCluster(num_cores, type = "FORK", outfile=log_file)
# registerDoParallel(cl)

# for (mode in c("historical", "future")) { # mode <- "historical"
#   print(paste0("Starting: ", mode))

#   if (mode == "historical") {
#     iter_dir <- iter_hist_dir
#     overall_fn <- "test_Fig2Hist.csv"
#     region_fn <- "test_Fig3Regionals.csv"
#     map_fn <- "test_Fig3Big.feather"
#     overall_yr_filter <- c(1900:1930) # This is actually 1901 to 1930 in practice
#     region_yr_filter <- c(1900:1930) # This is actually 1901 to 1930 in practice
#   } else {
#     iter_dir <- iter_futu_dir
#     overall_fn <- "test_Fig2Future.csv"
#     region_fn <- "test_Fig4Regionals.csv"
#     map_fn <- "test_Fig4Big.feather"
#     overall_yr_filter <- c(2015:2019)
#     region_yr_filter <- c(2015:2020)
#   }

#   meta <- file.path(iter_dir, "RowMetadata.feather") |>
#     arrow::read_feather(col_select = c(OBJECTID, year, scenario, model, country, region))

#   if (mode == "historical") {
#     rows <- which(
#       (meta$scenario %in% scenarios[1:2])
#       & (meta$year %in% c(yr_1901, yr_2014))
#     )
#   } else {
#     rows <- which(
#       (meta$scenario %in% scenarios[3:5])
#       & (meta$year %in% c(yr_2015, yr_2050, yr_2100))
#     )
#   }

#   ####
#   #### Data summaries ----
#   ####

#   iter.list <- foreach(i = 1:1000) %dopar% { # i <- 1
#     tictoc::tic()

#     iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |>
#       arrow::read_feather(col_select = "Pred") |>
#       data.table::as.data.table() |>
#       dplyr::bind_cols(meta)

#     maps_iter <- iter[rows,]
#     maps_iter$year[maps_iter$year %in% yr_1901] <- 1901
#     maps_iter$year[maps_iter$year %in% yr_2014] <- 2014
#     maps_iter$year[maps_iter$year %in% yr_2015] <- 2015
#     maps_iter$year[maps_iter$year %in% yr_2050] <- 2050
#     maps_iter$year[maps_iter$year %in% yr_2100] <- 2100

#     scen_yr_mean <- iter[
#       , list(Pred = mean(Pred, na.rm = TRUE)),
#       by = 'scenario,model,year'
#     ]
#     scen_yr_reg_mean <- iter[
#       , list(Pred = mean(Pred, na.rm = TRUE)),
#       by = 'scenario,model,year,region'
#     ]
#     scen_yr_adm_mean <- maps_iter[
#       , list(Pred = mean(Pred, na.rm = TRUE)),
#       by = 'scenario,model,year,OBJECTID'
#     ]

#     scen_yr_mean$run <- i
#     scen_yr_reg_mean$run <- i
#     scen_yr_adm_mean$run <- i

#     tictoc::toc()

#     return(list(scen_yr_mean, scen_yr_reg_mean, scen_yr_adm_mean))
#   }

#   ####
#   #### Overall medians ----
#   ####

#   scen_yr_mean <- bind_rows(lapply(iter.list, function(x) x[[1]])) |>
#     tibble::as_tibble()

#   scen_yr_mean |>
#     dplyr::filter(year %in% overall_yr_filter) |>
#     dplyr::group_by(model, run, scenario) |>
#     dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
#     dplyr::right_join(scen_yr_mean) |>
#     dplyr::mutate(Pred = (Pred-BetaMean))  |>
#     dplyr::select(-BetaMean) ->
#     df

#   df |>
#     dplyr::group_by(scenario, year) |>
#     dplyr::summarize(
#       median = median(Pred, na.rm = TRUE),
#       upper = quantile(Pred, 0.95, na.rm = TRUE),
#       lower = quantile(Pred, 0.05, na.rm = TRUE)
#     ) ->
#     hist.to.graph

#   print(paste0("Saving: ", overall_fn))
#   readr::write_csv(hist.to.graph, here::here("TempFiles", overall_fn))

#   ####
#   #### Regional medians ----
#   ####

#   scen_yr_reg_mean <- bind_rows(lapply(iter.list, function(x) x[[2]])) |>
#     tibble::as_tibble()

#   scen_yr_reg_mean |>
#     dplyr::filter(year %in% region_yr_filter) |>
#     dplyr::group_by(scenario, model, region, run) |>
#     dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
#     dplyr::right_join(scen_yr_reg_mean) |>
#     dplyr::mutate(Pred = (Pred-BetaMean)) |>
#     dplyr::select(-BetaMean) ->
#     df

#   df |>
#     dplyr::group_by(scenario, region, year) |>
#     dplyr::summarize(
#       median = median(Pred, na.rm = TRUE),
#       upper = quantile(Pred, 0.95, na.rm = TRUE),
#       lower = quantile(Pred, 0.05, na.rm = TRUE)
#     ) ->
#     data.to.graph

#   print(paste0("Saving: ", region_fn))
#   readr::write_csv(data.to.graph, here::here("TempFiles", region_fn))

#   ####
#   #### ADM1 means ----
#   ####

#   print(paste0("Saving: ", map_fn))
#   scen_yr_adm_mean <- bind_rows(lapply(iter.list, function(x) x[[3]])) |>
#     tibble::as_tibble() |>
#     arrow::write_feather(here::here("TempFiles", map_fn))

# }
# stopCluster(cl)
# tictoc::toc()

#############################################################################-
#### Create time series summaries of historic and future prevalence estimates.
#### Extract the overall (scenario, model, year) and regional (scenario, model,
#### region, year) medians. Save the files to the TempFiles directory. These
#### summaries are used to create the time series in figures 2, 3, and 4.
#### -----------------------------------------------------------------------.
#### Written by: Cullen Molitor
#### Date: 2024-06-29
#### Email: cullen_molitor@ucsb.edu
#### Revised: 2026-03-15 — performance rewrite
#############################################################################-

####
#### Setup ----
####

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  sf,
  here,
  data.table,
  arrow,
  future,
  future.apply,
  progressr
)

# ---------------------------------------------------------------------------
# Logging helper — replaces tictoc inside parallel workers.
# Writes timestamped messages to a log file AND to the console.
# ---------------------------------------------------------------------------
log_file <- here::here("TempFiles", "parallel_log.txt")

log_msg <- function(..., file = log_file) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
  message(msg)
  cat(msg, "\n", file = file, append = TRUE)
}

t_start <- proc.time()
log_msg("Script started")

options(future.globals.maxSize = 2 * 1024^3) # 2 GiB

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))


# ---------------------------------------------------------------------------
# Parallelism via future (multicore fork, same semantics as FORK cluster)
# ---------------------------------------------------------------------------
num_cores <- 10L
plan(multisession, workers = num_cores)

# Enable progress reporting (prints to console; works with future.apply)
handlers(handler_progress(
  format = ":spin [:bar] :percent | iter :current/:total | elapsed: :elapsed | eta: :eta",
  clear = FALSE
))

for (mode in c("historical", "future")) {
  # mode <- "historical"
  log_msg("Starting mode: ", mode)

  if (mode == "historical") {
    iter_dir <- iter_hist_dir
    temp_files <- "test_HistoricalTempFiles"
    overall_fn <- "test_Fig2Hist.csv"
    region_fn <- "test_Fig3Regionals.csv"
    map_fn <- "test_Fig3Big.feather"
    overall_yr_filter <- 1900:1930
    region_yr_filter <- 1900:1930
    scen_subset <- scenarios[1:2]
    row_years <- c(yr_bins[["1901"]], yr_bins[["2014"]])
  } else {
    iter_dir <- iter_futu_dir
    temp_files <- "test_FutureTempFiles"
    overall_fn <- "test_Fig2Future.csv"
    region_fn <- "test_Fig4Regionals.csv"
    map_fn <- "test_Fig4Big.feather"
    overall_yr_filter <- 2015:2019
    region_yr_filter <- 2015:2020
    scen_subset <- scenarios[3:5]
    row_years <- c(yr_bins[["2015"]], yr_bins[["2050"]], yr_bins[["2100"]])
  }

  log_msg("Reading meta data")
  # Read metadata once — keep as data.table from the start
  meta <- arrow::read_feather(file.path(iter_dir, "RowMetadata.feather"))

  # Pre-compute the row mask for the maps subset (avoids recomputing 1000×)
  map_row_idx <- which(
    meta$scenario %in% scen_subset & meta$year %in% row_years
  )

  ####
  #### Data summaries (parallel across 1000 iterations) ----
  ####

  log_msg("Processing 1000 iterations in parallel (", num_cores, " cores)...")

  iter_list <- with_progress({
    p <- progressor(steps = 1000L)

    future_lapply(
      1:1000,
      function(i) {
        # i <- 1

        log_msg("Reading raw prediction data for iter: ", i)
        # Read only the Pred column as an Arrow array, then to data.table
        pred <- arrow::read_feather(
          file.path(iter_dir, paste0("iter_", i, ".feather")),
          col_select = "Pred"
        ) |>
          data.table::as.data.table()

        # Attach metadata columns by reference (no copy)
        pred[, names(meta) := meta]

        # log_msg("mean Pred by scenario, model, year")
        # --- Overall summary: mean Pred by (scenario, model, year) ---
        scen_yr_mean <- pred[,
          .(Pred = mean(Pred, na.rm = TRUE)),
          by = .(scenario, model, year)
        ]
        scen_yr_mean[, run := i]

        # log_msg("mean Pred by scenario, model, year, region")
        # --- Regional summary: mean Pred by (scenario, model, year, region) ---
        scen_yr_reg_mean <- pred[,
          .(Pred = mean(Pred, na.rm = TRUE)),
          by = .(scenario, model, year, region)
        ]
        scen_yr_reg_mean[, run := i]

        # log_msg("mean Pred by scenario, model, year, OBJECTID")
        # --- Map summary: subset rows, recode years, mean by (scenario, model, year, OBJECTID) ---
        maps_dt <- pred[map_row_idx]
        maps_dt[, year := yr_lookup[as.character(year)]]

        scen_yr_adm_mean <- maps_dt[,
          .(Pred = mean(Pred, na.rm = TRUE)),
          by = .(scenario, model, year, OBJECTID)
        ]
        scen_yr_adm_mean[, run := i]

        # p(sprintf("iter %d done", i))

        list(scen_yr_mean, scen_yr_reg_mean, scen_yr_adm_mean)
      },
      future.seed = NULL,
      future.scheduling = 2.0
    )
  }) # iter_list <- list(list(scen_yr_mean, scen_yr_reg_mean, scen_yr_adm_mean))

  log_msg("Parallel loop complete — combining results")

  # ---------------------------------------------------------------------------
  # Combine results with rbindlist (much faster than bind_rows for data.tables)
  # ---------------------------------------------------------------------------
  scen_yr_mean <- rbindlist(lapply(iter_list, `[[`, 1L))
  scen_yr_reg_mean <- rbindlist(lapply(iter_list, `[[`, 2L))
  scen_yr_adm_mean <- rbindlist(lapply(iter_list, `[[`, 3L))

  # Free the big list
  rm(iter_list)
  gc()

  ####
  #### Overall medians ----
  ####

  # Baseline mean per (model, run, scenario), then demean
  baseline_overall <- scen_yr_mean[
    year %in% overall_yr_filter,
    .(BetaMean = mean(Pred, na.rm = TRUE)),
    by = .(model, run, scenario)
  ]
  scen_yr_mean[
    baseline_overall,
    Pred := Pred - BetaMean,
    on = .(model, run, scenario)
  ]

  hist_to_graph <- scen_yr_mean[,
    .(
      median = median(Pred, na.rm = TRUE),
      upper = quantile(Pred, 0.95, na.rm = TRUE),
      lower = quantile(Pred, 0.05, na.rm = TRUE)
    ),
    by = .(scenario, year)
  ]

  log_msg("Saving: ", overall_fn)
  fwrite(hist_to_graph, here::here("TempFiles", overall_fn))

  ####
  #### Regional medians ----
  ####

  baseline_regional <- scen_yr_reg_mean[
    year %in% region_yr_filter,
    .(BetaMean = mean(Pred, na.rm = TRUE)),
    by = .(scenario, model, region, run)
  ]
  scen_yr_reg_mean[
    baseline_regional,
    Pred := Pred - BetaMean,
    on = .(scenario, model, region, run)
  ]

  data_to_graph <- scen_yr_reg_mean[,
    .(
      median = median(Pred, na.rm = TRUE),
      upper = quantile(Pred, 0.95, na.rm = TRUE),
      lower = quantile(Pred, 0.05, na.rm = TRUE)
    ),
    by = .(scenario, region, year)
  ]

  log_msg("Saving: ", region_fn)
  fwrite(data_to_graph, here::here("TempFiles", region_fn))

  ####
  #### ADM1 means ----
  ####

  log_msg("Saving: ", map_fn)
  arrow::write_feather(scen_yr_adm_mean, here::here("TempFiles", map_fn))

  # Clean up before next mode
  rm(scen_yr_mean, scen_yr_reg_mean, scen_yr_adm_mean, meta)
  gc()
}

plan(sequential) # reset future plan

elapsed <- (proc.time() - t_start)["elapsed"]
log_msg(sprintf(
  "Total execution time: %.1f seconds (%.1f minutes)",
  elapsed,
  elapsed / 60
))
