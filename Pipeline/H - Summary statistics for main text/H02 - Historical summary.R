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

# log_msg("Loading historical_cru_pred_sum_scen_mod_yr_obj.feather")

hist_scen_mod_yr_adm1_pred <- file.path(
  hist_sum_dir,
  "historical_cru_pred_sum_scen_mod_yr_obj.feather"
) |>
  arrow::read_feather() |>
  dplyr::mutate(
    model = stringr::str_replace_all(model, 'BCC-CSM2-MR', 'BCC-CSM2')
  ) |>
  dplyr::select(scenario, model, year, OBJECTID, Pred, run) |>
  dplyr::filter(year == 2014)

# log_msg("Calculating ADM1 mean difference")

main_2010_2014 <- hist_scen_mod_yr_adm1_pred |>
  dplyr::filter(run == "main", ) |>
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
  dplyr::mutate(diff = (historical - `hist-nat`)) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(mean.diff = mean(diff))

# log_msg("Calculating ADM1 confidence interval")

boots_2010_2014 <- hist_scen_mod_yr_adm1_pred |>
  dplyr::filter(run != "main") |>
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
  dplyr::mutate(diff = (historical - `hist-nat`), ) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(
    runs.diff = sum(diff > 0),
    lower.diff = quantile(diff, 0.05, na.rm = TRUE),
    upper.diff = quantile(diff, 0.95, na.rm = TRUE),
  ) |>
  dplyr::left_join(main_2010_2014) |>
  dplyr::mutate(
    OBJECTID = factor(OBJECTID),
    moe = 1 - abs(runs.diff - 5000) / 5000
  )

################################################################################
# Join to elev and ADM1 data ----
################################################################################

elev <- elevation_fp |>
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
    mean.diff
  ) |>
  tidyr::drop_na()

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
