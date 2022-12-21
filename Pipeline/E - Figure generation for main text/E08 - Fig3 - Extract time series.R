
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/HistoricalTempFiles")

setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c("year", "scenario", "GCM", "Region"))

for (i in 1:1000) { 
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,GCM,year,Region']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} 

iter.df %<>% as_tibble()
iter.df %<>% pivot_longer(cols = -c(scenario, GCM, year, Region), names_to = c("Pred"))

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(scenario, GCM, Region, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(scenario, Region, year) %>%
  summarize(median = median(value, na.rm = TRUE),
            upper = quantile(value, 0.95, na.rm = TRUE),
            lower = quantile(value, 0.05, na.rm = TRUE)) -> data.to.graph

write_csv(data.to.graph, "~/Github/falciparum/TempFiles/Fig3Regionals.csv")

            
