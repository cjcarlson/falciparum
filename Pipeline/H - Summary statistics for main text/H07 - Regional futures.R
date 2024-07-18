library(tidyverse)
library(vroom)
library(here)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

process_data <- function(data, region_name = NULL) {
  
  if (!is.null(region_name)) {
    data <- data |> filter(region == region_name)
  }
  
  bm <- data |>
    filter(year %in% 2015:2020) |>
    group_by(model, scenario, iter) |>
    summarize(BetaMean = mean(Pred, na.rm = TRUE), .groups = "drop")
  
  df <- data |>
    left_join(bm, by = c("model", "scenario", "iter")) |>
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
    group_by(iter, model, scenario, period) |>
    summarize(Pred = mean(Pred), .groups = "drop") |>
    group_by(scenario, period) |>
    summarize(
      mean = mean(Pred),
      lower = quantile(Pred, 0.025),
      upper = quantile(Pred, 0.975),
      .groups = "drop"
    )  
  
  if (!is.null(region_name)) {
      results <- results |> mutate(region = region_name)
  } else {
    results <- results |> mutate(region = "Sub-Saharan Africa (continent-wide)")
  }
  
  return(results)
}

iter.df <- here::here("TempFiles", "SuppFutureBig.feather") |>
  arrow::read_feather()

global_results <- process_data(iter.df)

iter.df <- here::here("TempFiles", "SuppFutureRegions.csv") |> 
  vroom(show_col_types = FALSE)

results <- names(region_names)[2:5] |>
  map(~process_region(iter.df, .x)) |>
  list_rbind()

# Prepare the data for output
output_data <- global_results |>
  bind_rows(results) |>
  pivot_wider(
    names_from = period,
    values_from = c(mean, lower, upper),
    names_sep = "_"
  ) |>
  select(
    Region = region,
    Scenario = scenario,
    Estimate_2048_2052 = `mean_2048-2052`,
    CI_low_2048_2052 = `lower_2048-2052`,
    CI_high_2048_2052 = `upper_2048-2052`,
    Estimate_2096_2100 = `mean_2096-2100`,
    CI_low_2096_2100 = `lower_2096-2100`,
    CI_high_2096_2100 = `upper_2096-2100`
  ) |>   
  mutate(    
    Region = case_match(Region, !!!region_formulas),
    Scenario = case_match(Scenario, !!!future_scenario_formulas)
  ) |> 
  arrange(factor(Region, levels = unname(region_names))) |>
  mutate(
    CI_2048_2052 = sprintf("(%.3f, %.3f)", CI_low_2048_2052, CI_high_2048_2052),
    CI_2096_2100 = sprintf("(%.3f, %.3f)", CI_low_2096_2100, CI_high_2096_2100)
  ) |>
  select(
    Region, Scenario,
    Estimate_2048_2052, CI_2048_2052,
    Estimate_2096_2100, CI_2096_2100
  )

write_csv(output_data, here::here("TempFiles", "Supp_Future_Regions_Summary.csv"))


### Check Future Predictions that can be attributed to climate change
### Greater Rift Valley countries
# grv_countries <- c(
#   "Burundi", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Malawi", "Mozambique", 
#   "Rwanda", "Somalia", "South Sudan", "Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe"
# )

library(sf)
library(here)
library(tidyverse)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

iter.df <- here::here("TempFiles", "Fig4Big.feather") |> 
  arrow::read_feather() |> 
  dplyr::filter(scenario == "ssp585")

iter.df |> 
  tidyr::pivot_wider(names_from = year, values_from = Pred) |> 
  dplyr::mutate(diff = (`2100` - `2015`)) |>
  dplyr::select(-c(`2100`,`2050`,`2015`, scenario)) -> 
  slices.runs

slices.runs |> 
  dplyr::ungroup() |> 
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(
    mean.diff = mean(diff), runs.diff = sum(diff > 0), 
    lower.diff = quantile(diff, 0.05, na.rm = TRUE), 
    upper.diff = quantile(diff, 0.95, na.rm = TRUE)) |> 
  dplyr::mutate(
    # OBJECTID = factor(OBJECTID), 
    moe = 1 - abs(runs.diff-5500)/5500) ->
  slice.map2

elev <- file.path(datadir, "Data", "elevation", "elevation_extracted_all_ADM1.csv") |> 
  readr::read_csv(show_col_types = FALSE) |> 
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID)) 

sfcont <- file.path(datadir, 'Data', 'AfricaADM1.shp') |> 
  sf::read_sf() |> 
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID)) |>
  dplyr::left_join(slice.map2, by = join_by(OBJECTID))|> 
  dplyr::left_join(elev, by = join_by(OBJECTID)) |> 
  tibble::as_tibble() |> 
  dplyr::select(OBJECTID, NAME_0, NAME_1, elevmin, elevmn, elevmax, mean.diff) 

## Ethiopia
eth <- sfcont |> 
  dplyr::filter(NAME_0 == "Ethiopia") |> 
  dplyr::arrange(mean.diff) |>
  View()

## Sudan and South Sudan
sud <- sfcont |> 
  dplyr::filter(NAME_0 %in% c("Sudan", "South Sudan")) |>
  dplyr::arrange(mean.diff) |>
  View()

## Eritrea
eri <- sfcont |> 
  dplyr::filter(NAME_0 == "Eritrea") |> 
  dplyr::arrange(mean.diff) |>
  View()

## Djibouti
dji <- sfcont |> 
  dplyr::filter(NAME_0 == "Djibouti") |> 
  dplyr::arrange(mean.diff) |>
  View()
