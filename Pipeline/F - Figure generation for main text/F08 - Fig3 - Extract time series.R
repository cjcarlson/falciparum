# library(tidyverse)
# library(magrittr)
# library(ggplot2)
# library(data.table)
# 
# source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
# iter_dir <- file.path(datadir, "IterationFiles", "HistoricalTempFiles")
# 
# meta <- file.path(iter_dir, "RowMetadata.feather") |> 
#   arrow::read_feather(col_select = c(region, year, scenario, model)) 
# 
# tictoc::tic("Finish")
# for (i in 1:1000) { 
#   tictoc::tic(i)
#   iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |> 
#     arrow::read_feather(col_select = "Pred") |> 
#     dplyr::bind_cols(meta) |> 
#     data.table::as.data.table() 
#   
#   iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,model,year,region']
#   iter$run <- i
#   if(i==1) {iter.df <- iter} else {iter.df <- dplyr::bind_rows(iter.df, iter)}
#   tictoc::toc()
# } 
# tictoc::toc()
# 
# iter.df <- tibble::as_tibble(iter.df)
# 
# iter.df %>% 
#   dplyr::filter(year %in% c(1900:1930)) %>%
#   dplyr::group_by(scenario, model, region, run) %>%
#   dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) %>% 
#   dplyr::right_join(iter.df) %>% 
#   dplyr::mutate(Pred = (Pred-BetaMean)) %>%
#   dplyr::select(-BetaMean) ->
#   df
# 
# df %>%
#   dplyr::group_by(scenario, region, year) %>%
#   dplyr::summarize(
#     median = median(Pred, na.rm = TRUE),
#     upper = quantile(Pred, 0.95, na.rm = TRUE),
#     lower = quantile(Pred, 0.05, na.rm = TRUE)
#   ) ->
#   data.to.graph
# 
# write_csv(data.to.graph, here::here("TempFiles", "Fig3Regionals.csv"))
# 
