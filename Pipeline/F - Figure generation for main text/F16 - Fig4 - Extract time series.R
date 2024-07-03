# library(here)
# library(tidyverse)
# library(data.table)
# library(tictoc)
# library(foreach)
# library(doParallel)
# 
# tictoc::tic("Finish")
# 
# source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
# 
# iter_dir <- file.path(datadir, "IterationFiles", "FutureTempFiles")
# 
# meta <- file.path(iter_dir, "RowMetadata.feather") |> 
#   arrow::read_feather(col_select = c(year, scenario, model, region, country)) 
# 
# log_file <- here::here("TempFiles", "parallel_log.txt")
# 
# num_cores <- 10 
# cl <- makeCluster(num_cores, type = "FORK", outfile=log_file)
# registerDoParallel(cl)
# 
# 
# iter.df <- foreach(
#   i = 1:1000, 
#   .combine = rbind
# ) %dopar% {
#   tictoc::tic(i)
#   
#   iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |> 
#     arrow::read_feather(col_select = "Pred") |> 
#     dplyr::bind_cols(meta) |> 
#     data.table::as.data.table() 
#   iter <- iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,model,year,region']
#   iter$run <- i
#   
#   tictoc::toc()
#   
#   return(iter)
# }
# stopCluster(cl)
# 
# iter.df2 <- iter.df |> 
#   tibble::as_tibble()
# 
# iter.df2 %>%
#   dplyr::filter(year %in% c(2015:2020)) %>%
#   dplyr::group_by(scenario, region, model, run) %>%
#   dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE)) %>%
#   dplyr::right_join(iter.df2) %>%
#   dplyr::mutate(Pred = (Pred-BetaMean)) %>%
#   dplyr::select(-BetaMean) -> 
#   df
# 
# df %>%
#   dplyr::group_by(scenario, region, year) %>%
#   dplyr::summarize(
#     median = median(Pred),
#     upper = quantile(Pred, 0.95),
#     lower = quantile(Pred, 0.05)) ->
#   data.to.graph
# 
# data.to.graph |>
#   readr::write_csv(here::here("TempFiles", "Fig4Regionals.csv"))
# 
# tictoc::toc()
# 
