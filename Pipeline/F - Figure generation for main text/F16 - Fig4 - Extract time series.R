
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/FutureTempFiles")

setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c("year", "run", "Region"))

for (i in 1:1000) { 
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'run,year,Region']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} 

iter.df %<>% as_tibble()

iter.df %>%
  pivot_longer(cols = -c(run, year, Region), names_to = c("Pred")) %>%
  tidyr::extract(run, 
                 into = c('GCM','RCP'),
                 regex = "(ACCESS-CSM2|ACCESS-ESM1|BCC-CSM2-MR|CanESM5|FGOALS-g3|GFDL-ESM4|IPSL-CM6A-LR|MIROC6|MRI-ESM2-0|NorESM2-LM)-(rcp26|rcp45|rcp85)",
                 remove = FALSE) -> iter.df2

iter.df2 %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(RCP, Region, GCM, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df2) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(RCP, Region, year) %>%
  summarize(median = median(value),
            upper = quantile(value, 0.95),
            lower = quantile(value, 0.05)) -> data.to.graph

write_csv(data.to.graph, "~/Github/falciparum/TempFiles/Fig4Regionals.csv")


