library(tidyverse)
library(data.table)
library(vroom)
library(here)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

process_region <- function(data, region_name = NULL) {
  
  if (!is.null(region_name)) {
    data <- data %>% filter(region == region_name)
  }
  
  bm <- data |>
    filter(year %in% 1900:1930) |>
    group_by(model, scenario, iter) |>
    summarize(BetaMean = mean(Pred, na.rm = TRUE))
  
  df <- data |>
    left_join(bm) |>
    mutate(Pred = Pred - BetaMean) |>
    select(-BetaMean)
  
  df2 <- df |>
    filter(year %in% 2010:2014) |>
    select(model, iter, year, Pred, scenario) |>
    pivot_wider(names_from = scenario, values_from = Pred) |>
    group_by(model, iter) |>
    summarize(`hist-nat` = mean(`hist-nat`), historical = mean(historical))
  
  diff <- df2$historical - df2$`hist-nat`
  
  results <- tibble(
    MeanDifference = mean(diff),
    ScaledMeanDifference = 100000 * mean(diff) / 100,
    Quantile_025 = quantile(diff, 0.025),
    Quantile_975 = quantile(diff, 0.975),
    ProportionPositive = prop.table(table(diff > 0))["TRUE"]
  ) |> 
    mutate(across(where(is.numeric), ~round(., 4)))
  
  if (!is.null(region_name)) {
    results <- results %>% mutate(Region = region_name)
  } else {
    results <- results %>% mutate(Region = "Sub-Saharan Africa (continent-wide)")
  }
}
iter.df <- here::here("TempFiles", "SuppHistoricalBig.feather")  |> 
  arrow::read_feather() 

global_results <- process_region(iter.df)

iter.df <- here::here("TempFiles", "SuppHistoricalRegions.csv") |>
  vroom(show_col_types = FALSE) |>
  mutate(model = str_replace_all(model, 'BCC-CSM2-MR', 'BCC-CSM2'))

results <- names(region_names)[2:5] |>
  map(~process_region(iter.df, .x)) |>
  list_rbind()

results <- bind_rows(global_results, results) |>
  mutate(Region = case_match(Region, !!!region_formulas))

output_file <- here::here("TempFiles", "H04_results_summary.csv")
write_csv(results, output_file)

cat("Results have been saved to:", output_file, "\n")

print(results)


### Check Historical Predictions that can be attributed to climate change
### Greater Rift Valley countries
# grv_countries <- c(
#   "Burundi", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Malawi", "Mozambique", 
#   "Rwanda", "Somalia", "South Sudan", "Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe"
# )

library(sf)
library(here)
library(tidyverse)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

iter.df <- here::here("TempFiles", "Fig3Big.feather") |> 
  arrow::read_feather() |>
  dplyr::mutate(model = stringr::str_replace_all(model,'BCC-CSM2-MR','BCC-CSM2')) 

iter.df |> 
  dplyr::filter(year == 2014) -> 
  slices

slices |> 
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |> 
  dplyr::mutate(diff = (historical - `hist-nat`)) |>
  dplyr::select(-c(historical, `hist-nat`, year)) -> 
  slices.runs

slices.runs |> 
  dplyr::ungroup() |> 
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) |> 
  dplyr::mutate(OBJECTID = factor(OBJECTID)) ->
  slice.map1

elev <- file.path(datadir, "Data", "elevation", "elevation_extracted_all_ADM1.csv") |> 
  readr::read_csv(show_col_types = FALSE)

sfcont <- file.path(datadir, 'Data', 'AfricaADM1.shp') |> 
  sf::read_sf() |> 
  dplyr::left_join(slice.map1, by = join_by(OBJECTID)) |> 
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))  |> 
  dplyr::mutate(moe = 1 - abs(runs.diff-5000)/5000) |> 
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