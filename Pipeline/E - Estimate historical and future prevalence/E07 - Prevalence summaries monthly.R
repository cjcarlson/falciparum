#############################################################################-
#### Create time series summaries of historic and future prevalence estimates.
#### Extract the overall (scenario, model, year, and month). Save the files to
#### the TempFiles directory. These summaries are used to create the time series
#### for the supplemental figure of monthly prevalence estimates.
#### -----------------------------------------------------------------------.
#### Written by: Cullen Molitor
#### Date: 2025-06-29
#### Email: cullen_molitor@ucsb.edu
#############################################################################-

####
#### Setup ----
####

library(sf)
library(here)
library(tictoc)
library(foreach)
library(tidyverse)
library(lubridate)
library(parallel)
library(doParallel)
library(data.table)

tictoc::tic("Total execution time")

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

num_cores <- 50
log_file <- here::here("TempFiles", "parallel_log.txt")
cl <- parallel::makeCluster(num_cores, type = "PSOCK", outfile = log_file)
# cl <- parallel::makeCluster(num_cores, type = "PSOCK", outfile = "")
doParallel::registerDoParallel(cl)

mode <- "historical"
print(paste0("Starting: ", mode))

region_fn <- "Fig3Regionals-monthly.csv"
region_mod_fn <- "Fig3Regionals-model-monthly.csv"

adm1_fn <- "Fig3ADM1-monthly.csv"

iter_dir <- file.path(data_dir, "IterationFiles", "HistoricalTempFiles")

meta <- file.path(iter_dir, "RowMetadata.feather") |>
  arrow::read_feather(
    col_select = c(OBJECTID, year, scenario, model, country, region, month)
  )

####
#### Data summaries ----
####

iter.list <- foreach(i = 1:1000) %dopar%
  {
    tictoc::tic(i)

    cat(i, "Reading data\n")
    iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |>
      arrow::read_feather(col_select = "Pred") |>
      data.table::as.data.table() |>
      dplyr::bind_cols(meta) |>
      dplyr::filter(year %in% 2010:2014)

    cat(i, "Cleaning data\n")

    scen_yr_reg_mean <- iter[,
      list(Pred = mean(Pred, na.rm = TRUE)),
      by = .(scenario, model, year, month, region)
    ]

    scen_yr_adm1_mean <- iter[,
      list(Pred = mean(Pred, na.rm = TRUE)),
      by = .(OBJECTID, scenario, model, year, month, region)
    ]

    scen_yr_reg_mean$run <- i
    scen_yr_adm1_mean$run <- i

    tictoc::toc()

    return(list(scen_yr_reg_mean, scen_yr_adm1_mean)) # iter.list <- list(scen_yr_reg_mean)
  }

parallel::stopCluster(cl)

####
#### Regional medians ----
####

cat("Regional medians\n")

diff_df <- bind_rows(lapply(iter.list, function(x) x[[1]])) |>
# diff_df <- bind_rows(iter.list) |>
  tibble::as_tibble() |>
  tidyr::pivot_wider(
    id_cols = c(month, year, model, region, run),
    names_from = scenario,
    values_from = Pred
  ) |>
  dplyr::mutate(diff = historical - `hist-nat`) |>
  mutate(
    month_num = match(month, month.abb),
    date = make_date(year = year, month = month_num, day = 1)
  )

rgn_mean_diff_df <- diff_df |>
  dplyr::group_by(region, month_num) |>
  dplyr::summarize(
    mean = mean(diff, na.rm = TRUE),
    median = median(diff, na.rm = TRUE),
    upper = quantile(diff, 0.95, na.rm = TRUE),
    lower = quantile(diff, 0.05, na.rm = TRUE)
  )

print(paste0("Saving: ", region_fn))
readr::write_csv(
  x = rgn_mean_diff_df,
  file = here::here("TempFiles", region_fn)
)

rgn_mod_mean_diff_df <- diff_df |>
  dplyr::group_by(region, model, month_num) |>
  dplyr::summarize(
    mean = mean(diff, na.rm = TRUE),
    median = median(diff, na.rm = TRUE),
    upper = quantile(diff, 0.95, na.rm = TRUE),
    lower = quantile(diff, 0.05, na.rm = TRUE)
  )

print(paste0("Saving: ", region_mod_fn))
readr::write_csv(
  x = rgn_mod_mean_diff_df,
  file = here::here("TempFiles", region_mod_fn)
)

####
#### ADM1 medians ----
####

cat("ADM1 medians\n")

diff_adm1_df <- bind_rows(lapply(iter.list, function(x) x[[2]])) |>
  tibble::as_tibble() |>
  tidyr::pivot_wider(
    id_cols = c(OBJECTID, month, year, model, region, run),
    names_from = scenario,
    values_from = Pred
  ) |>
  dplyr::mutate(diff = historical - `hist-nat`) |>
  mutate(
    month_num = match(month, month.abb),
    date = make_date(year = year, month = month_num, day = 1)
  )

adm1_mean_diff_df <- diff_adm1_df |>
  dplyr::group_by(region, OBJECTID, month_num) |>
  dplyr::summarize(
    mean = mean(diff, na.rm = TRUE),
    median = median(diff, na.rm = TRUE),
    upper = quantile(diff, 0.95, na.rm = TRUE),
    lower = quantile(diff, 0.05, na.rm = TRUE)
  )

print(paste0("Saving: ", adm1_fn))
readr::write_csv(
  x = adm1_mean_diff_df,
  file = here::here("TempFiles", adm1_fn)
)

tictoc::toc()
