
library(tidyverse); library(magrittr); library(ggplot2); library(data.table); library(vroom)

iter.df <- vroom("~/Github/falciparum/TempFiles/SuppHistoricalRegions.csv")

iter.df %>%
  mutate(GCM = str_replace_all(GCM,'./Historical/','')) %>%
  mutate(GCM = str_replace_all(GCM,'./Future/','')) %>%
  mutate(GCM = str_replace_all(GCM,'BCC-CSM2-MR','BCC-CSM2')) -> iter.df

###########################################################################
###########################################################################
###########################################################################

iter.df %>% 
  filter(Region == "Sub-Saharan Africa (West)",
         year %in% c(1900:1930)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) -> bm

iter.df %>% 
  filter(Region == "Sub-Saharan Africa (West)") %>% 
  left_join(bm) %>% 
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>% 
  filter(year %in% c(2010:2014)) %>%
  select(GCM, iter, year, Pred, scenario) %>% 
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

df2 %>%
  group_by(GCM, iter) %>%
  summarize(nat = mean(nat), hist = mean(hist)) -> df2

mean(df2$hist - df2$nat)
100000*mean(df2$hist - df2$nat)/100
quantile((df2$hist - df2$nat), 0.025)
quantile((df2$hist - df2$nat), 0.975)
table((df2$hist - df2$nat) > 0) %>% prop.table() 




###########################################################################
###########################################################################
###########################################################################

iter.df %>% 
  filter(Region == "Sub-Saharan Africa (Central)",
         year %in% c(1900:1930)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) -> bm

iter.df %>% 
  filter(Region == "Sub-Saharan Africa (Central)") %>% 
  left_join(bm) %>% 
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>% 
  filter(year %in% c(2010:2014)) %>%
  select(GCM, iter, year, Pred, scenario) %>% 
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

df2 %>%
  group_by(GCM, iter) %>%
  summarize(nat = mean(nat), hist = mean(hist)) -> df2

mean(df2$hist - df2$nat)
100000*mean(df2$hist - df2$nat)/100
quantile((df2$hist - df2$nat), 0.025)
quantile((df2$hist - df2$nat), 0.975)
table((df2$hist - df2$nat) > 0) %>% prop.table() 


###########################################################################
###########################################################################
###########################################################################

iter.df %>% 
  filter(Region == "Sub-Saharan Africa (East)",
         year %in% c(1900:1930)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) -> bm

iter.df %>% 
  filter(Region == "Sub-Saharan Africa (East)") %>% 
  left_join(bm) %>% 
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>% 
  filter(year %in% c(2010:2014)) %>%
  select(GCM, iter, year, Pred, scenario) %>% 
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

df2 %>%
  group_by(GCM, iter) %>%
  summarize(nat = mean(nat), hist = mean(hist)) -> df2

mean(df2$hist - df2$nat)
100000*mean(df2$hist - df2$nat)/100
quantile((df2$hist - df2$nat), 0.025)
quantile((df2$hist - df2$nat), 0.975)
table((df2$hist - df2$nat) > 0) %>% prop.table() 



###########################################################################
###########################################################################
###########################################################################

iter.df %>% 
  filter(Region == "Sub-Saharan Africa (Southern)",
         year %in% c(1900:1930)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) -> bm

iter.df %>% 
  filter(Region == "Sub-Saharan Africa (Southern)") %>% 
  left_join(bm) %>% 
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>% 
  filter(year %in% c(2010:2014)) %>%
  select(GCM, iter, year, Pred, scenario) %>% 
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

df2 %>%
  group_by(GCM, iter) %>%
  summarize(nat = mean(nat), hist = mean(hist)) -> df2

mean(df2$hist - df2$nat)
100000*mean(df2$hist - df2$nat)/100
quantile((df2$hist - df2$nat), 0.025)
quantile((df2$hist - df2$nat), 0.975)
table((df2$hist - df2$nat) > 0) %>% prop.table() 

