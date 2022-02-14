
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/HistoricalTempFiles")

meta <- fread("RowMetadata.csv", select = c("year", "scenario"))

for (i in 1:50) { #Loop starts  
  setDTthreads(2L)
  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,year']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} #Loop ends

iter.df %<>% as.tibble()

iter.df %<>% pivot_longer(cols = -c(scenario, year, Region), names_to = c("Pred"))

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(scenario, Region, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df