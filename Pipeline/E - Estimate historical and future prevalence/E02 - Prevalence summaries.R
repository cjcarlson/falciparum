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

library(sf)
library(here)
library(tictoc)
library(foreach)
library(tidyverse)
library(lubridate)
library(doParallel)
library(data.table)

tictoc::tic("Total execution time")

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

yr_1901 <- 1901:1905
yr_2014 <- 2010:2014
yr_2015 <- 2015:2019
yr_2050 <- 2048:2052
yr_2100 <- 2096:2100

num_cores <- 100
log_file <- here::here("TempFiles", "parallel_log.txt")
cl <- makeCluster(num_cores, type = "FORK", outfile=log_file)
registerDoParallel(cl)

for (mode in c("historical", "future")) { # mode <- "historical"
  print(paste0("Starting: ", mode))
  
  if (mode == "historical") {
    temp_files <- "HistoricalTempFiles"
    overall_fn <- "Fig2Hist.csv"
    region_fn <- "Fig3Regionals.csv"
    map_fn <- "Fig3Big.feather"
    overall_yr_filter <- c(1900:1930) # This is actually 1901 to 1930 in practice
    region_yr_filter <- c(1900:1930) # This is actually 1901 to 1930 in practice
  } else {
    temp_files <- "FutureTempFiles"
    overall_fn <- "Fig2Future.csv"
    region_fn <- "Fig4Regionals.csv"
    map_fn <- "Fig4Big.feather"
    overall_yr_filter <- c(2015:2019)
    region_yr_filter <- c(2015:2020)
  }
  iter_dir <- file.path(datadir, "IterationFiles", temp_files)
  
  meta <- file.path(iter_dir, "RowMetadata.feather") |> 
    arrow::read_feather(col_select = c(OBJECTID, year, scenario, model, country, region)) 
  
  if (mode == "historical") {
    rows <- which(
      (meta$scenario %in% scenarios[1:2]) 
      & (meta$year %in% c(yr_1901, yr_2014))
    )
  } else {
    rows <- which(
      (meta$scenario %in% scenarios[3:5]) 
      & (meta$year %in% c(yr_2015, yr_2050, yr_2100))
    )
  }
  
  #### 
  #### Data summaries ----
  ####
  
  iter.list <- foreach(i = 1:1000) %dopar% {
    tictoc::tic(i)
    
    iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |> 
      arrow::read_feather(col_select = "Pred") |> 
      data.table::as.data.table() |> 
      dplyr::bind_cols(meta)
    
    maps_iter <- iter[rows,]
    maps_iter$year[maps_iter$year %in% yr_1901] <- 1901
    maps_iter$year[maps_iter$year %in% yr_2014] <- 2014 
    maps_iter$year[maps_iter$year %in% yr_2015] <- 2015
    maps_iter$year[maps_iter$year %in% yr_2050] <- 2050
    maps_iter$year[maps_iter$year %in% yr_2100] <- 2100
    
    scen_yr_mean <- iter[
      , list(Pred = mean(Pred, na.rm = TRUE)),
      by = 'scenario,model,year'
    ]
    scen_yr_reg_mean <- iter[
      , list(Pred = mean(Pred, na.rm = TRUE)), 
      by = 'scenario,model,year,region'
    ]
    scen_yr_adm_mean <- maps_iter[
      , list(Pred = mean(Pred, na.rm = TRUE)), 
      by = 'scenario,model,year,OBJECTID'
    ]
    
    scen_yr_mean$run <- i
    scen_yr_reg_mean$run <- i
    scen_yr_adm_mean$run <- i
    
    tictoc::toc()
    
    return(list(scen_yr_mean, scen_yr_reg_mean, scen_yr_adm_mean))
  } 
  
  #### 
  #### Overall medians ----
  ####
  
  scen_yr_mean <- bind_rows(lapply(iter.list, function(x) x[[1]])) |> 
    tibble::as_tibble()
  
  scen_yr_mean |>
    dplyr::filter(year %in% overall_yr_filter) |> 
    dplyr::group_by(model, run, scenario) |> 
    dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
    dplyr::right_join(scen_yr_mean) |>
    dplyr::mutate(Pred = (Pred-BetaMean))  |> 
    dplyr::select(-BetaMean) -> 
    df
  
  df |>
    dplyr::group_by(scenario, year) |>
    dplyr::summarize(
      median = median(Pred, na.rm = TRUE),
      upper = quantile(Pred, 0.95, na.rm = TRUE),
      lower = quantile(Pred, 0.05, na.rm = TRUE)
    ) -> 
    hist.to.graph
  
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
    dplyr::mutate(Pred = (Pred-BetaMean)) |>
    dplyr::select(-BetaMean) ->
    df
  
  df |>
    dplyr::group_by(scenario, region, year) |>
    dplyr::summarize(
      median = median(Pred, na.rm = TRUE),
      upper = quantile(Pred, 0.95, na.rm = TRUE),
      lower = quantile(Pred, 0.05, na.rm = TRUE)
    ) ->
    data.to.graph
  
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
stopCluster(cl)
tictoc::toc()
