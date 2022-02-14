
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/FutureTempFiles")

meta <- fread("RowMetadata.csv", select = c("year", "run"))

#### OVERALL PREVALENCE

for (i in 1:100) { #Loop stars  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'run,year']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
  print(i)
} #Loop ends

iter.df %<>% as_tibble()

iter.df %>%
  pivot_longer(cols = -c(run, year), names_to = c("Pred")) %>%
  tidyr::extract(run, 
                 into = c('GCM','RCP'),
                 regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
                 remove = FALSE) -> iter.df2

iter.df2 %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(RCP, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df2) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

# summary and t-test

df %>% 
  filter(year == 2050, RCP == 'rcp26') %>% 
  pull(value) %>% t.test()

(df %>% 
  filter(year == 2050, RCP == 'rcp26') %>% 
  pull(value) > 0) %>% table() %>% prop.table()
