
library(tidyverse); library(magrittr); library(ggplot2); library(data.table); library(vroom)

iter.df <- vroom("~/Github/falciparum/TempFiles/SuppFutureBig.csv")

iter.df %<>% 
  tidyr::extract(run, 
                 into = c('GCM','RCP'),
                 regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
                 remove = FALSE) 
  
iter.df %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(GCM, RCP, iter) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) -> bm

iter.df %>% 
  left_join(bm) %>% 
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>% 
  filter(year %in% c(2048:2052)) %>%
  group_by(RCP) %>% 
  summarize(mean = mean(Pred),
            upper = quantile(Pred, 0.95),
            lower = quantile(Pred, 0.05))

df %>% 
  filter(year %in% c(2096:2100)) %>%
  group_by(RCP) %>% 
  summarize(mean = mean(Pred),
            upper = quantile(Pred, 0.95),
            lower = quantile(Pred, 0.05))


