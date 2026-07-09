################################################################################
# This script generates summary statistics for the historical predictions.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  sf,
  here,
  vroom,
  tidyverse,
  data.table
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

### Check Historical Predictions that can be attributed to climate change
### Greater Rift Valley countries
# grv_countries <- c(
#   "Burundi", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Malawi",
#   "Mozambique", "Rwanda", "Somalia", "South Sudan", "Sudan", "Tanzania",
#   "Uganda", "Zambia", "Zimbabwe"
# )

################################################################################
# Hist delta data ----
################################################################################

boots_2010_2014 <- file.path(
  hist_sum_dir,
  "historical_vcov_pred_sum_scen_mod_yr_obj.feather"
) |>
  arrow::read_feather() |>
  dplyr::mutate(
    model = stringr::str_replace_all(model, 'BCC-CSM2-MR', 'BCC-CSM2')
  ) |>
  dplyr::select(scenario, model, year, OBJECTID, Pred, run) |>
  dplyr::filter(run != "main", year == 2014) |>
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
  dplyr::mutate(diff = (historical - `hist-nat`), ) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(
    mean.diff = mean(diff),
    runs.diff = sum(diff > 0),
    lower.diff = quantile(diff, 0.05, na.rm = TRUE),
    upper.diff = quantile(diff, 0.95, na.rm = TRUE),
    prop_positive_diff = mean(diff > 0)
  ) |>
  dplyr::mutate(
    OBJECTID = factor(OBJECTID),
    moe = 1 - abs(runs.diff - 5000) / 5000
  )

################################################################################
# Hist delta high elevation data ----
################################################################################

boots_2010_2014_high_el_reg <- file.path(
  hist_sum_dir,
  "historical_vcov_pred_sum_scen_mod_yr_high_el_reg.feather"
) |>
  arrow::read_feather() |>
  dplyr::mutate(
    model = stringr::str_replace_all(model, 'BCC-CSM2-MR', 'BCC-CSM2')
  ) |>
  dplyr::select(scenario, model, year, region, Pred, run) |>
  dplyr::filter(run != "main", year == 2014) |>
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
  dplyr::mutate(diff = (historical - `hist-nat`), ) |>
  dplyr::group_by(region) |>
  dplyr::summarize(
    mean.diff = mean(diff),
    runs.diff = sum(diff > 0),
    lower.diff = quantile(diff, 0.05, na.rm = TRUE),
    upper.diff = quantile(diff, 0.95, na.rm = TRUE),
    prop_positive_diff = mean(diff > 0)
  ) |>
  dplyr::mutate(moe = 1 - abs(runs.diff - 5000) / 5000) |>
  dplyr::filter(
    region %in% c("Sub-Saharan Africa (East)", "Sub-Saharan Africa (Southern)")
  )

################################################################################
# Join to elev and ADM1 data ----
################################################################################

elev <- elevation_summary_fp |>
  readr::read_csv(show_col_types = FALSE)

sfcont <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::left_join(boots_2010_2014, by = join_by(OBJECTID)) |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID)) |>
  dplyr::mutate(moe = 1 - abs(runs.diff - 5000) / 5000) |>
  dplyr::left_join(elev, by = join_by(OBJECTID)) |>
  tibble::as_tibble() |>
  dplyr::select(
    OBJECTID,
    NAME_0,
    NAME_1,
    elevmin,
    elevmn,
    elevmax,
    mean.diff,
    prop_positive_diff
  ) |>
  tidyr::drop_na()

sfcont |>
  dplyr::group_by(country = NAME_0) |>
  dplyr::summarise(
    historical = mean(mean.diff, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::drop_na() |>
  readr::write_csv(here::here("Results", "Tables", "historical_country_summary.csv"))

################################################################################
# Check countries ----
################################################################################

## Ethiopia
eth <- sfcont |>
  dplyr::filter(NAME_0 == "Ethiopia") |>
  dplyr::arrange(mean.diff)

print(eth)

## Sudan and South Sudan
sud <- sfcont |>
  dplyr::filter(NAME_0 %in% c("Sudan", "South Sudan")) |>
  dplyr::arrange(mean.diff)

print(sud)

## Eritrea
eri <- sfcont |>
  dplyr::filter(NAME_0 == "Eritrea") |>
  dplyr::arrange(mean.diff)

print(eri)

## Djibouti
dji <- sfcont |>
  dplyr::filter(NAME_0 == "Djibouti") |>
  dplyr::arrange(mean.diff)

print(dji)

################################################################################
# End of file ----
################################################################################
