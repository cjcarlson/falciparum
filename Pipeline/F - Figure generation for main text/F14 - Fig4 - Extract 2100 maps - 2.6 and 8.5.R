# 
# library(here)
# library(tidyverse)
# library(data.table)
# library(tictoc)
# library(foreach)
# library(doParallel)
# 
# source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
# 
# iter_dir <- file.path(datadir, "IterationFiles", "FutureTempFiles")
# 
# meta <- file.path(iter_dir, "RowMetadata.feather") |> 
#   arrow::read_feather(col_select = c(year, scenario, model, OBJECTID)) 
# 
# rows <- which((meta$scenario %in% c('ssp126','ssp245', 'ssp585')) & (meta$year %in% c(2015:2019, 2096:2100)))
# meta <- meta[rows,]
# # Yes I know the below solution is a little janky, god forgive me 
# meta$year[meta$year %in% c(2015:2019)] <- 2015
# meta$year[meta$year %in% c(2096:2100)] <- 2100
# 
# num_cores <- 20 
# log_file <- here::here("parallel_log.txt")
# cl <- makeCluster(num_cores, type = "FORK", outfile=log_file)
# registerDoParallel(cl)
# 
# tictoc::tic("Finish")
# 
# iter.df <- foreach(
#   i = 1:1000, 
#   .combine = rbind, 
#   .packages = c("arrow", "dplyr", "data.table")
# ) %dopar% {
#   tictoc::tic(i)
#   
#   iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |> 
#     arrow::read_feather(col_select = "Pred")
#   
#   iter <- iter[rows,] 
#   iter <- dplyr::bind_cols(meta, iter) |> 
#     data.table::as.data.table() 
#   iter <- iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,model,year,OBJECTID']
#   iter$run <- i
#   
#   tictoc::toc()
#   
#   return(iter)
# }
# 
# stopCluster(cl)
# 
# iter.df |> 
#   tibble::as_tibble() |> 
#   arrow::write_feather(here::here("TempFiles", "Fig4Big2100.feather"))
# 
# tictoc::toc()
