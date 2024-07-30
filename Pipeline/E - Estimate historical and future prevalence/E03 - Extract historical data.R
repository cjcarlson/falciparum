#############################################################################-
#### Extract the overall (scenario, model, year) historic effects of model 
#### variables. Save the files to the TempFiles directory. These
#### summaries are used to create supplemental figures.
#### -----------------------------------------------------------------------. 
#### Written by: Colin Carson
#### Edited by: Cullen Molitor
#### Date: 2024-06-29
#### Email: cullen_molitor@ucsb.edu
#############################################################################-

library(here)
library(magrittr)
library(tidyverse)
library(data.table)
library(tictoc)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

iter_dir <- file.path(datadir, "IterationFiles", "HistoricalTempFiles")

meta <- file.path(iter_dir, "RowMetadata.feather") |> 
  arrow::read_feather(col_select = c(year, scenario, model)) 

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
    by = 'scenario,model,year'
  ]
  iter$iter <- i
  if(i==1) {iter.df <- iter} else {iter.df <- bind_rows(iter.df, iter)}
  tictoc::toc()
} 

iter.df <- tibble::as_tibble(iter.df)

arrow::write_feather(iter.df, here::here("TempFiles", "SuppHistoricalBig.feather"))
