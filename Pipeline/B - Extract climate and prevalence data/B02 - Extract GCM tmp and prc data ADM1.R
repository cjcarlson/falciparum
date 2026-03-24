#############################################################################-
# Use the following code to extract GCM data for temperature and precipitation
# from the CRU-TS4.03 dataset. The code extracts the data for 5 climate scenarios
# and 10 climate models. The code extracts the data for each month from 1901 to
# 2100. The code saves the data in CSV format for each month and each model. The
# code also consolidates the data into a single CSV file for each scenario and
# model. The code uses N cores to parallelize, which is chosen by the user.
# Finally, the code saves the data in the 'Climate' directory.
#############################################################################-

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  here,
  terra,
  tictoc,
  foreach,
  tidyverse,
  doParallel,
  exactextractr
)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

overwrite <- FALSE

tictoc::tic()

############################################################
# Make cluster----
############################################################

numCores <- 100

cl <- makeCluster(numCores, outfile = "")

registerDoParallel(cl)

############################################################
# Loop over scenarios ----
############################################################

for (scenario in scenarios) {
  tictoc::tic()
  print(paste0("Starting: ", scenario))

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

  ############################################################
  # Loop over Models ----
  ############################################################

  for (model in models) {
    tictoc::tic()
    print(paste0("Starting: ", model))

    grid <- dplyr::case_when(
      model == "GFDL-ESM4" ~ "gr1",
      model == "IPSL-CM6A-LR" ~ "gr",
      TRUE ~ "gn"
    )
    prc_fn <- make_filename("pr", model, scenario, grid, date_range)
    tmp_fn <- make_filename("tas", model, scenario, grid, date_range)

    ############################################################
    # Parallel extraction ----
    ############################################################

    foreach(
      i = 1:year_mon,
      .export = c(
        "data_dir",
        "bc_cruts_output_dir",
        "scenario",
        "prc_fn",
        "tmp_fn"
      )
    ) %dopar%
      {
        month_year <- paste(
          month.abb[(i - 1) %% 12 + 1],
          ((i - 1 - (i - 1) %% 12) / 12) + year_start,
          sep = '.'
        )

        output_path <- file.path(
          data_dir,
          "int",
          scenario,
          paste0(model, "_", i, ".csv")
        )
        output_dir <- dirname(output_path)

        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }

        if (!file.exists(output_path) | overwrite) {
          # Does not serialize well, easy to read in every time
          cont <- sf::read_sf(here::here(data_dir, 'Data', 'AfricaADM1.shp'))

          nct <- terra::rast(file.path(bc_cruts_output_dir, scenario, tmp_fn))
          ncp <- terra::rast(file.path(bc_cruts_output_dir, scenario, prc_fn))

          ############################################################
          # Extract temp ----
          ############################################################

          temp <- nct[[i]]
          temp_ex <- exactextractr::exact_extract(
            x = temp,
            y = cont,
            fun = 'mean',
            progress = FALSE
          )

          temp2 <- temp * temp
          temp2_ex <- exactextractr::exact_extract(
            x = temp2,
            y = cont,
            fun = 'mean',
            progress = FALSE
          )

          ############################################################
          # Extract precip ----
          ############################################################

          ppt <- ncp[[i]]
          ppt_ex <- exactextractr::exact_extract(
            x = ppt,
            y = cont,
            fun = 'mean',
            progress = FALSE
          )

          ############################################################
          # Save outputs ----
          ############################################################

          dummy_df <- tibble::tibble(
            OBJECTID = cont$OBJECTID,
            !!paste0(month_year, '.temp') := temp_ex,
            !!paste0(month_year, '.temp2') := temp2_ex,
            !!paste0(month_year, '.ppt') := ppt_ex
          ) |>
            tidyr::pivot_longer(
              -OBJECTID,
              names_to = c('month', 'year', 'var'),
              names_sep = '\\.'
            ) |>
            tidyr::pivot_wider(names_from = 'var', values_from = 'value') |>
            dplyr::mutate(year = as.numeric(year)) |>
            readr::write_csv(output_path)
        }
      }
    print(paste0("Finished: ", model, "\nConsilidating intermediate files"))

    files <- list.files(
      file.path(data_dir, "int", scenario),
      pattern = model,
      full.names = TRUE
    )
    file_name <- file.path(data_dir, "Climate", scenario, paste0(model, ".csv"))
    file_dir <- dirname(file_name)

    if (!dir.exists(file_dir)) {
      dir.create(file_dir, recursive = TRUE)
    }

    if (!file.exists(file_name) | overwrite) {
      results <- readr::read_csv(files, show_col_types = FALSE) |>
        dplyr::mutate(month = factor(month, levels = month.abb)) |>
        dplyr::arrange(year, month) |>
        readr::write_csv(file_name)
      tictoc::toc()
    }
  }
  print(paste0("Finished: ", scenario))
  tictoc::toc()
}
tictoc::toc()
stopCluster(cl)

############################################################
# End of file ----
############################################################
