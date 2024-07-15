
library(here)
library(tidyverse)
library(data.table)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

iter_dir <- file.path(datadir, "IterationFiles", "FutureTempFiles")

results_fn <- here::here("TempFiles", "SuppFutureRegions.csv")

overwrite <- FALSE

if (!file.exists(results_fn) | overwrite) {
  meta <- file.path(iter_dir, "RowMetadata.feather") |> 
    arrow::read_feather(col_select = c(year, scenario, model, region)) 
  
  for (i in 1:1000) { 
    tictoc::tic(i)
    iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |> 
      arrow::read_feather(col_select = c(Pred, Pf.temp, Pf.flood, Pf.drought)) |> 
      data.table::as.data.table() |> 
      dplyr::bind_cols(meta)
    iter <-  iter[
      , list(
        Pred = mean(Pred, na.rm = TRUE), 
        Pf.temp = mean(Pf.temp, na.rm = TRUE),
        Pf.flood = mean(Pf.flood, na.rm = TRUE),
        Pf.drought = mean(Pf.drought, na.rm = TRUE)
      ),
      by = 'scenario,model,year,region'
    ]
    iter$iter <- i
    if(i==1) {iter.df <- iter} else {iter.df <- bind_rows(iter.df, iter)}
    tictoc::toc()
  } 
  
  iter.df <- tibble::as_tibble(iter.df)
  
  readr::write_csv(iter.df, results_fn)
  
} else {
  message("File already exists. Set overwrite = TRUE to redo.")
}