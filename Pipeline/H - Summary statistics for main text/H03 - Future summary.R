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
pacman::p_load(sf, here, vroom, tidyverse)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Future delta data ----
################################################################################

future_scen_mod_yr_pred <- file.path(
  fut_sum_dir,
  "future_vcov_pred_sum_scen_mod_yr.feather"
) |>
  arrow::read_feather() |>
  dplyr::mutate(
    model = stringr::str_replace_all(model, 'BCC-CSM2-MR', 'BCC-CSM2')
  ) |>
  dplyr::select(scenario, model, year, Pred, run)

adm1_results <- calc_future_regional_diff(future_scen_mod_yr_pred)

################################################################################
# Future regional delta data ----
################################################################################

future_scen_mod_yr_reg_pred <- file.path(
  fut_sum_dir,
  "future_vcov_pred_sum_scen_mod_yr_reg.feather"
) |>
  arrow::read_feather() |>
  dplyr::mutate(
    model = stringr::str_replace_all(model, 'BCC-CSM2-MR', 'BCC-CSM2')
  ) |>
  dplyr::select(scenario, model, region, year, Pred, run)

region_results <- names(region_names)[2:5] |>
  purrr::map(~ calc_future_regional_diff(future_scen_mod_yr_reg_pred, .x)) |>
  purrr::list_rbind()

################################################################################
# Save table ----
################################################################################

# Prepare the data for output
output_data <- adm1_results |>
  dplyr::bind_rows(region_results) |>
  dplyr::select(-n_positive, -n_total) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = c(mean, lower, upper, prop_positive),
    names_sep = "_"
  ) |>
  dplyr::select(
    Region = region,
    Scenario = scenario,
    Estimate_2048_2052 = `mean_2048-2052`,
    CI_low_2048_2052 = `lower_2048-2052`,
    CI_high_2048_2052 = `upper_2048-2052`,
    PropPositive_2048_2052 = `prop_positive_2048-2052`,
    Estimate_2096_2100 = `mean_2096-2100`,
    CI_low_2096_2100 = `lower_2096-2100`,
    CI_high_2096_2100 = `upper_2096-2100`,
    PropPositive_2096_2100 = `prop_positive_2096-2100`
  ) |>
  dplyr::mutate(
    Region = dplyr::case_match(Region, !!!region_formulas),
    Scenario = dplyr::case_match(Scenario, !!!future_scenario_formulas)
  ) |>
  dplyr::arrange(factor(Region, levels = unname(region_names))) |>
  dplyr::mutate(
    Estimate_2048_2052 = round(Estimate_2048_2052, 3),
    Estimate_2096_2100 = round(Estimate_2096_2100, 3),
    CI_2048_2052 = sprintf("(%.3f, %.3f)", CI_low_2048_2052, CI_high_2048_2052),
    CI_2096_2100 = sprintf("(%.3f, %.3f)", CI_low_2096_2100, CI_high_2096_2100),
    PropPositive_2048_2052 = round(PropPositive_2048_2052, 3),
    PropPositive_2096_2100 = round(PropPositive_2096_2100, 3)
  ) |>
  dplyr::select(
    Region,
    Scenario,
    Estimate_2048_2052,
    CI_2048_2052,
    PropPositive_2048_2052,
    Estimate_2096_2100,
    CI_2096_2100,
    PropPositive_2096_2100
  )

writeLines(
  generate_future_latex(output_data),
  con = here::here("Results", "Tables", "Future.tex")
)

cat(generate_future_latex(output_data))

# readr::write_csv(
#   output_data,
#   # file.path(fut_sum_dir, "supp_future_regions_summary.csv")
#   here::here("Results", "Tables", "supp_future_regions_summary.csv")
# )

################################################################################
# Compare ssp126 to ssp245 P+ ----
# Calculate proportion of positive bootstrap runs by limiting to ssp126 compared
# to ssp245 for each region.
################################################################################

bm <- future_scen_mod_yr_reg_pred |>
  filter(year %in% 2015:2020) |>
  group_by(model, scenario, region, run) |>
  summarize(BetaMean = mean(Pred, na.rm = TRUE), .groups = "drop")

df <- future_scen_mod_yr_reg_pred |>
  left_join(bm, by = c("model", "scenario", "region", "run")) |>
  mutate(Pred = Pred - BetaMean) |>
  select(-BetaMean)

results <- bind_rows(
  df |>
    filter(year %in% 2048:2052) |>
    mutate(period = "2048-2052"),
  df |>
    filter(year %in% 2096:2100) |>
    mutate(period = "2096-2100")
) |>
  group_by(run, model, scenario, region, period) |>
  summarize(Pred = mean(Pred), .groups = "drop")

################################################################################
# Mid century ----
################################################################################

boot.diff.mid.df <- results |>
  dplyr::filter(
    scenario %in% c("ssp126", "ssp245"),
    period == "2048-2052",
    run != "main"
  ) |>
  dplyr::filter() |>
  tidyr::pivot_wider(
    id_cols = c(run, model, region, period),
    names_from = scenario,
    values_from = Pred
  ) |>
  dplyr::mutate(diff = ssp245 - ssp126) |>
  dplyr::group_by(region, period) |>
  dplyr::summarise(
    mean_diff = mean(diff),
    lower_diff = quantile(diff, 0.025),
    upper_diff = quantile(diff, 0.975),
    prop_positive_diff = mean(diff > 0)
  ) |>
  dplyr::mutate(scenario_diff = "ssp245 - ssp126")

################################################################################
# End of century ----
################################################################################

boot.diff.end.df <- results |>
  dplyr::filter(
    scenario %in% c("ssp126", "ssp245"),
    period == "2096-2100",
    run != "main"
  ) |>
  tidyr::pivot_wider(
    id_cols = c(model, region, run, period),
    names_from = scenario,
    values_from = Pred
  ) |>
  dplyr::mutate(diff = ssp245 - ssp126) |>
  dplyr::group_by(region, period) |>
  dplyr::summarise(
    mean_diff = mean(diff),
    lower_diff = quantile(diff, 0.025),
    upper_diff = quantile(diff, 0.975),
    prop_positive_diff = mean(diff > 0)
  ) |>
  dplyr::mutate(scenario_diff = "ssp245 - ssp126")

################################################################################
# Join mid and end of century ----
################################################################################

diff.df <- rbind(boot.diff.mid.df, boot.diff.end.df) |>
  dplyr::mutate(
    across(
      c(lower_diff, upper_diff, prop_positive_diff, mean_diff),
      ~ round(.x, 3)
    )
  )

readr::write_csv(
  diff.df,
  here::here("Results", "Tables", "future_regional_diff_summary.csv")
)

################################################################################
# Calculate ADM1 differences ----
################################################################################

boot_diff <- file.path(
  fut_sum_dir,
  "future_vcov_pred_sum_scen_mod_yr_obj.feather"
) |>
  arrow::read_feather() |>
  dplyr::filter(run != "main", scenario == "ssp585") |>
  tidyr::pivot_wider(
    id_cols = c(scenario, model, OBJECTID, run),
    names_from = year,
    values_from = Pred
  ) |>
  dplyr::mutate(diff = (`2100` - `2015`)) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(
    mean.diff = mean(diff),
    runs.diff = sum(diff > 0),
    lower.diff = quantile(diff, 0.05, na.rm = TRUE),
    upper.diff = quantile(diff, 0.95, na.rm = TRUE),
    prop_positive_diff = mean(diff > 0)
  ) |>
  dplyr::mutate(moe = 1 - abs(runs.diff - 5500) / 5500)

################################################################################
# Join to elev and ADM1 data ----
################################################################################

elev <- elevation_summary_fp |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

sfcont <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID)) |>
  dplyr::left_join(boot_diff, by = join_by(OBJECTID)) |>
  dplyr::left_join(elev, by = join_by(OBJECTID)) |>
  tibble::as_tibble() |>
  dplyr::select(OBJECTID, NAME_0, NAME_1, elevmin, elevmn, elevmax, mean.diff)

################################################################################
# Check countries ----
# Check Future Predictions that can be attributed to climate change.
# Greater Rift Valley countries:
# grv_countries <- c(
#   "Burundi", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Malawi",
#   "Mozambique", "Rwanda", "Somalia", "South Sudan", "Sudan", "Tanzania",
#   "Uganda", "Zambia", "Zimbabwe"
# )
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
