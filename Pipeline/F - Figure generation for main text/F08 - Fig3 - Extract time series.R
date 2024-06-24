
library(tidyverse)
library(magrittr)
library(ggplot2)
library(data.table)

# setwd("D:/MalariaAfrica/HistoricalTempFiles")

# setDTthreads(1L)
# meta <- fread("RowMetadata.csv", select = c("year", "scenario", "model", "region"))

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
iter_dir <- file.path(datadir, "IterationFiles", "HistoricalTempFiles")

meta <- file.path(iter_dir, "RowMetadata.feather") |> 
  arrow::read_feather(col_select = c(region, year, scenario, model)) 

# for (i in 1:1000) { 
#   iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
#   iter <- bind_cols(meta, iter)
#   iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,model,year,region']
#   if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
# } 

tictoc::tic("Finish")
for (i in 1:1000) { 
  tictoc::tic(i)
  iter <- file.path(iter_dir, paste0("iter_", i, ".feather")) |> 
    arrow::read_feather(col_select = "Pred") |> 
    dplyr::bind_cols(meta) |> 
    data.table::as.data.table() 
  
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,model,year,region']
  iter$run <- i
  if(i==1) {iter.df <- iter} else {iter.df <- dplyr::bind_rows(iter.df, iter)}
  tictoc::toc()
} 
tictoc::toc()

iter.df %<>% as_tibble()
# iter.df %<>% pivot_longer(cols = -c(scenario, model, year, region), names_to = c("Pred"))

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(scenario, model, region, run) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) %>% 
  right_join(iter.df) %>% 
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(scenario, region, year) %>%
  summarize(median = median(Pred, na.rm = TRUE),
            upper = quantile(Pred, 0.95, na.rm = TRUE),
            lower = quantile(Pred, 0.05, na.rm = TRUE)) -> data.to.graph

write_csv(data.to.graph, here::here("TempFiles", "Fig3Regionals.csv"))

