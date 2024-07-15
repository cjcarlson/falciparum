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
