
library(tidyverse); library(magrittr); library(ggplot2); library(data.table); library(vroom)

iter.df <- vroom("~/Github/falciparum/TempFiles/SuppHistoricalBig.csv")

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) -> bm

iter.df %>% 
  left_join(bm) %>% 
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>% 
  select(GCM, iter, year, Pred, scenario) %>% 
  filter(year %in% c(2010:2014)) %>%
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

100000*mean(df2$hist - df2$nat)/100
t.test(df2$hist, df2$nat, paired = TRUE)

table((df2$hist - df2$nat) > 0) %>% prop.table() 

