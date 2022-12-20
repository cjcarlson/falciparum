
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/HistoricalTempFiles")

####################### GLOBAL 

meta <- fread("RowMetadata.csv", select = c("year", "GCM", "scenario"))

for (i in 1:1000) { #Loop starts  
  #setDTthreads(1L)
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'GCM,scenario,year']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} #Loop ends

iter.df %<>% as_tibble()

iter.df %<>% pivot_longer(cols = -c(GCM, scenario, year), names_to = c("Pred"))

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(GCM, scenario, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) -> bm

iter.df %>% 
  left_join(bm) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

# A test you can do to prove to yourself the data are arranged right
# > iter.df %>% 
#   +   filter(scenario == 'hist') %>% 
#   +   arrange(year, Pred) %>%
#   +   pull(value) -> end1 
# > iter.df %>% 
#   +   filter(scenario == 'nat') %>% 
#   +   arrange(year, Pred) %>%
#   +   pull(value) -> end2
# > cor(end1, end2)
# [1] 0.9999878

# Sense check: this should be 11000
# length(nat) # nat is below

df %>% 
  filter(year == 2014, scenario == 'hist') %>% 
  group_by(GCM, Pred) %>% 
  summarize(value = mean(value)) %>% # Turns the daily into one data point for the year 
  pull(value) -> hist 
df %>% 
  filter(year == 2014, scenario == 'nat') %>% 
  group_by(GCM, Pred) %>% 
  summarize(value = mean(value)) %>% # Turns the daily into one data point for the year 
  pull(value) -> nat
t.test(hist, nat, paired = TRUE)

table((hist - nat) > 0) %>% prop.table() # IS THIS SET UP RIGHT????????????????

####################### REGIONAL

meta <- fread("RowMetadata.csv", select = c("Region", "year", "GCM", "scenario"))

for (i in 1:1000) { #Loop starts  
  #setDTthreads(1L)
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'Region,GCM,scenario,year']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} #Loop ends

iter.df %<>% as_tibble()

iter.df %<>% pivot_longer(cols = -c(Region, GCM, scenario, year), names_to = c("Pred"))

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(Region, GCM, scenario, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) -> bm

iter.df %>% 
  left_join(bm) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

# A test you can do to prove to yourself the data are arranged right
# > iter.df %>% 
#   +   filter(scenario == 'hist') %>% 
#   +   arrange(year, Pred) %>%
#   +   pull(value) -> end1 
# > iter.df %>% 
#   +   filter(scenario == 'nat') %>% 
#   +   arrange(year, Pred) %>%
#   +   pull(value) -> end2
# > cor(end1, end2)
# [1] 0.9999878

# Sense check: this should be 11000
# length(nat) # nat is below

df %>% 
  filter(Region=='Sub-Saharan Africa (East)', year == 2014, scenario == 'hist') %>% 
  group_by(GCM, Pred) %>% 
  summarize(value = mean(value)) %>% # Turns the daily into one data point for the year 
  pull(value) -> hist 
df %>% 
  filter(Region=='Sub-Saharan Africa (East)', year == 2014, scenario == 'nat') %>% 
  group_by(GCM, Pred) %>% 
  summarize(value = mean(value)) %>% # Turns the daily into one data point for the year 
  pull(value) -> nat
t.test(hist, nat, paired = TRUE)

table((hist - nat) > 0) %>% prop.table() 