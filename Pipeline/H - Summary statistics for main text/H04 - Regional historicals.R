
library(tidyverse); library(magrittr); library(ggplot2); library(data.table); library(vroom)

iter.df <- vroom("~/Github/falciparum/TempFiles/SuppHistoricalRegions.csv")

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
  select(GCM, iter, year, Pred, scenario) %>% 
  filter(year %in% c(2010:2014)) %>%
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

# 100000*mean(df2$hist - df2$nat)/100
t.test(df2$hist, df2$nat, paired = TRUE)

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
  select(GCM, iter, year, Pred, scenario) %>% 
  filter(year %in% c(2010:2014)) %>%
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

# 100000*mean(df2$hist - df2$nat)/100
t.test(df2$hist, df2$nat, paired = TRUE)

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
  select(GCM, iter, year, Pred, scenario) %>% 
  filter(year %in% c(2010:2014)) %>%
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

# 100000*mean(df2$hist - df2$nat)/100
t.test(df2$hist, df2$nat, paired = TRUE)

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
  select(GCM, iter, year, Pred, scenario) %>% 
  filter(year %in% c(2010:2014)) %>%
  pivot_wider(names_from = scenario, values_from = Pred) -> df2

# 100000*mean(df2$hist - df2$nat)/100
t.test(df2$hist, df2$nat, paired = TRUE)

table((df2$hist - df2$nat) > 0) %>% prop.table() 