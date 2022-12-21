
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/HistoricalTempFiles")

setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c("year", "scenario", "GCM"))

# meta %<>%
#   tidyr::extract(run, 
#                  into = c('GCM','RCP'),
#                  regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
#                  remove = FALSE)  

# Doesn't need GCM because the averages are the same? 

for (i in 1:100) { #Loop starts  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,GCM,year']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} #Loop ends


iter.df %<>% as.tibble()

iter.df %<>% pivot_longer(cols = -c(scenario, GCM, year), names_to = c("Pred"))

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(scenario, GCM, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(diff = (hist - nat)) %>%
  group_by(year) %>% 
  summarize(prop = sum(diff < 0)/length(diff < 0)) %>%
  ggplot(aes(x = year, y = prop)) + geom_line() + geom_hline(yintercept = 0.05, col = 'red')


df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(diff = (hist - nat)) %>%
  group_by(year) %>% 
  summarize(prop = sum(diff < 0)/length(diff < 0), n = length(diff)) %>%
  ggplot(aes(x = year, y = prop)) + geom_line() + geom_hline(yintercept = 0.05, col = 'red')


df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(diff = (hist - nat)) %>%
  group_by(year) %>% 
  summarize(sum = sum(diff < 0), n = length(diff)) -> df2

df2$p <- 0

for (i in 1:nrow(df2)) {
    df2$p[i] = prop.test(df2$sum[i], df2$n[i], 0.5)$p.value
}

df2

# df %>%
#   pivot_wider(names_from = scenario, values_from = value) %>%
#   mutate(diff = (hist - nat)) %>%
#   group_by(year) %>% 
#   summarize(p = t.test(diff ~ 1)$p.value) %>% 
#   ggplot(aes(x = year, y = p)) + geom_line() + geom_hline(yintercept = 0.05, col = 'red')
