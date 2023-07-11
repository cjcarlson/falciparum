
library(tidyverse); library(magrittr); library(ggplot2); library(data.table); library(vroom)

iter.df <- vroom("~/Github/falciparum/TempFiles/SuppFutureBig.csv")

iter.df %<>% 
  tidyr::extract(run, 
                 into = c('GCM','RCP'),
                 regex = "(ACCESS-CSM2|ACCESS-ESM1|BCC-CSM2-MR|CanESM5|FGOALS-g3|GFDL-ESM4|IPSL-CM6A-LR|MIROC6|MRI-ESM2-0|NorESM2-LM)-(rcp26|rcp45|rcp85)",
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
  group_by(iter, GCM, RCP) %>%
  summarize(Pred = mean(Pred)) %>% 
  ungroup() %>%
  group_by(RCP) %>% 
  summarize(mean = mean(Pred),
            lower = quantile(Pred, 0.025),
            upper = quantile(Pred, 0.975))

df %>% 
  filter(year %in% c(2096:2100)) %>%
  group_by(iter, GCM, RCP) %>%
  summarize(Pred = mean(Pred)) %>% 
  ungroup() %>%
  group_by(RCP) %>% 
  summarize(mean = mean(Pred),
            lower = quantile(Pred, 0.025),
            upper = quantile(Pred, 0.975))


