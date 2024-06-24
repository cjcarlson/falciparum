
library(tidyverse)
library(data.table)
library(here)
library(arrow)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

iter_dir <- file.path(datadir, "IterationFiles", "HistoricalTempFiles")

meta <- file.path(iter_dir, "RowMetadata.feather") |> 
  arrow::read_feather(col_select = c(OBJECTID, year, scenario, model)) 

rows <- which(meta$year %in% c(1901:1905, 2010:2014))
meta <- meta[rows,]
# Yes I know the below solution is a little janky, god forgive me 
meta$year[meta$year %in% c(1901:1905)] <- 1901
meta$year[meta$year %in% c(2010:2014)] <- 2014 

tictoc::tic("Finish")
for (i in 1:1000) { 
  tictoc::tic(i)
  iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |> 
    arrow::read_feather(col_select = "Pred")
  
  iter <- iter[rows,] 
  iter <- dplyr::bind_cols(meta, iter) |> 
    data.table::as.data.table() 
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,model,year,OBJECTID']
  iter$run <- i
  if(i==1) {iter.df <- iter} else {iter.df <- dplyr::bind_rows(iter.df, iter)}
  tictoc::toc()
} 
tictoc::toc()

arrow::write_feather(tibble::as_tibble(iter.df), here::here("TempFiles", "Fig3Big.feather"))
