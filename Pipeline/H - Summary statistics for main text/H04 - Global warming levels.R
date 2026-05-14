############################################################
# This script makes all
############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(terra, here, ncdf4, tidyverse)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))

################################################################################
# Summarize global warming levels ----
################################################################################

scenarios <- c("hist-ssp126", "hist-ssp245", "hist-ssp585")
gwl_time_int <- c("GWL05", "GWL10", "GWL20")

result_list <- list()

for (scenario in scenarios) {
  for (gwl in gwl_time_int) {
    scen_gwl_files <- file.path(climate_gwl_dir, scenario, gwl) |>
      list.files(full.names = TRUE)

    for (file in scen_gwl_files) {
      gwl_tmp <- ncdf4::nc_open(file)
      tas <- ncdf4::ncvar_get(gwl_tmp, "tas")
      time <- ncdf4::ncvar_get(gwl_tmp, "time")

      origin <- stringr::str_replace(
        gwl_tmp$dim$time$units,
        pattern = "days since ",
        ""
      )
      origin <- stringr::str_replace(origin, pattern = " 00:00:00", "")

      dates <- as.Date(time, origin = origin)
      years <- lubridate::year(dates)

      df <- data.frame(year = years)

      model <- basename(file) |>
        stringr::str_extract("_([^_]+)_hist-ssp\\d+") |>
        stringr::str_remove("^_") |>
        stringr::str_remove("_hist-ssp\\d+$")

      df$gwl <- gwl
      df$scenario <- scenario
      df$model <- model
      df$tas <- tas

      result_list[[length(result_list) + 1]] <- df

      ncdf4::nc_close(gwl_tmp)
    }
  }
}

################################################################################
# Combine results and save data ----
################################################################################

final_df <- dplyr::bind_rows(result_list) |>
  dplyr::group_by(year, gwl, scenario) |>
  dplyr::summarize(mean_tas = mean(tas) |> round(3)) |>
  dplyr::ungroup() |>
  dplyr::filter(year %in% c(2048, 2096), gwl == "GWL05") |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = mean_tas,
    names_glue = "{year}-{year+4}"
  )

readr::write_csv(final_df, file.path(fut_sum_dir, "global_warming_levels.csv"))

################################################################################
# End of file ----
################################################################################
