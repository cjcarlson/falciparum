# library(sf)
# library(lubridate)
# library(magrittr)
# library(tidyverse)
# library(data.table)
# library(tictoc)
# 
# # mode <- "historical"
# mode <- "future"
# 
# source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
# source(here::here(pipeline_A_dir, "A01 - Utility code for calculations.R"))
# 
# if (mode == "historical") {
#   temp_files <- "HistoricalTempFiles"
#   save_fn <- "Fig2Hist.csv"
#   yr_filter <- c(1900:1930) # This is actually 1901 to 1930 in practice
# } else {
#   temp_files <- "FutureTempFiles"
#   save_fn <- "Fig2Future.csv"
#   yr_filter <- c(2015:2019)
# }
# temp_dir <- file.path(datadir, "IterationFiles", temp_files)
# 
# meta <- file.path(temp_dir, "RowMetadata.feather") |> 
#   arrow::read_feather(col_select = c(year, scenario, model)) 
# 
# tictoc::tic("Finish")
# for (i in 1:1000) { # i <- 1
#   tictoc::tic(i)
#   iter <- file.path(temp_dir, paste0("iter_", i, ".feather")) |> 
#     arrow::read_feather(col_select = "Pred") |> 
#     data.table::as.data.table() |> 
#     dplyr::bind_cols(meta)
#   iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,model,year']
#   iter$run <- i
#   if(i==1) {iter.df <- iter} else {iter.df <- dplyr::bind_rows(iter.df, iter)}
#   tictoc::toc()
# } 
# tictoc::toc()
# 
# iter.df %<>% as_tibble()
# 
# iter.df |>
#   dplyr::filter(year %in% yr_filter) |> 
#   dplyr::group_by(model, run, scenario) |> 
#   dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
#   dplyr::right_join(iter.df) |>
#   dplyr::mutate(Pred = (Pred-BetaMean))  |> 
#   dplyr::select(-BetaMean) -> df
# 
# df %>%
#   dplyr::group_by(scenario, year) %>%
#   dplyr::summarize(
#     median = median(Pred, na.rm = TRUE),
#     upper = quantile(Pred, 0.95, na.rm = TRUE),
#     lower = quantile(Pred, 0.05, na.rm = TRUE)
#   ) -> 
#   hist.to.graph
# 
# readr::write_csv(hist.to.graph, here::here("TempFiles", save_fn))
