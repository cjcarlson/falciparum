
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/FutureTempFiles")

#### OVERALL PREVALENCE

meta <- fread("RowMetadata.csv", select = c("year", "run"))

for (i in 1:1000) { #Loop stars  
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
  group_by(GCM, RCP, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) -> bm

iter.df2 %>% 
  left_join(bm) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

# do the exploratory stuff blah blah blah 

df %>% 
  filter(year == 2050, RCP == 'rcp26') %>% 
  pull(value) %>% t.test()

(df %>% 
  filter(year == 2050, RCP == 'rcp26') %>% 
  pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2050, RCP == 'rcp45') %>% 
  pull(value) %>% t.test()

(df %>% 
    filter(year == 2050, RCP == 'rcp45') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2050, RCP == 'rcp85') %>% 
  pull(value) %>% t.test()

(df %>% 
    filter(year == 2050, RCP == 'rcp85') %>% 
    pull(value) > 0) %>% table() %>% prop.table()





df %>% 
  filter(year == 2100, RCP == 'rcp26') %>% 
  pull(value) %>% t.test()

(df %>% 
    filter(year == 2100, RCP == 'rcp26') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp45') %>% 
  pull(value) %>% t.test()

(df %>% 
    filter(year == 2100, RCP == 'rcp45') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp85') %>% 
  pull(value) %>% t.test()

(df %>% 
    filter(year == 2100, RCP == 'rcp85') %>% 
    pull(value) > 0) %>% table() %>% prop.table()







#### REGIONAL PREVALENCE

meta <- fread("RowMetadata.csv", select = c("Region", "year", "run"))

for (i in 1:1000) { #Loop stars  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'Region,run,year']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
  print(i)
} #Loop ends

iter.df %<>% as_tibble()

iter.df %>%
  pivot_longer(cols = -c(Region, run, year), names_to = c("Pred")) %>%
  tidyr::extract(run, 
                 into = c('GCM','RCP'),
                 regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
                 remove = FALSE) -> iter.df2

iter.df2 %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(GCM, RCP, Pred, Region) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) -> bm

iter.df2 %>% 
  left_join(bm) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  filter(RCP == 'rcp85', Region == 'Sub-Saharan Africa (West)') %>%
  group_by(year) %>%
  summarize(median = median(value)) %>%
  ggplot(aes(x = year, y = median)) + geom_line()

# ^ THIS IS A CHECK OF THE NORMALIZATION



# do the exploratory stuff blah blah blah 

df %>% 
  filter(year == 2100, RCP == 'rcp26', Region == 'Sub-Saharan Africa (Central)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp26', Region == 'Sub-Saharan Africa (Central)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp45', Region == 'Sub-Saharan Africa (Central)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp45', Region == 'Sub-Saharan Africa (Central)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp85', Region == 'Sub-Saharan Africa (Central)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp85', Region == 'Sub-Saharan Africa (Central)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()




df %>% 
  filter(year == 2100, RCP == 'rcp26', Region == 'Sub-Saharan Africa (East)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp26', Region == 'Sub-Saharan Africa (East)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp45', Region == 'Sub-Saharan Africa (East)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp45', Region == 'Sub-Saharan Africa (East)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp85', Region == 'Sub-Saharan Africa (East)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp85', Region == 'Sub-Saharan Africa (East)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()




df %>% 
  filter(year == 2100, RCP == 'rcp26', Region == 'Sub-Saharan Africa (West)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp26', Region == 'Sub-Saharan Africa (West)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp45', Region == 'Sub-Saharan Africa (West)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp45', Region == 'Sub-Saharan Africa (West)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp85', Region == 'Sub-Saharan Africa (West)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp85', Region == 'Sub-Saharan Africa (West)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()







df %>% 
  filter(year == 2060, RCP == 'rcp26', Region == 'Sub-Saharan Africa (Southern)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2060, RCP == 'rcp26', Region == 'Sub-Saharan Africa (Southern)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2060, RCP == 'rcp45', Region == 'Sub-Saharan Africa (Southern)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2060, RCP == 'rcp45', Region == 'Sub-Saharan Africa (Southern)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2060, RCP == 'rcp85', Region == 'Sub-Saharan Africa (Southern)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2060, RCP == 'rcp85', Region == 'Sub-Saharan Africa (Southern)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()




df %>% 
  filter(year == 2100, RCP == 'rcp26', Region == 'Sub-Saharan Africa (Southern)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp26', Region == 'Sub-Saharan Africa (Southern)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp45', Region == 'Sub-Saharan Africa (Southern)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp45', Region == 'Sub-Saharan Africa (Southern)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()

df %>% 
  filter(year == 2100, RCP == 'rcp85', Region == 'Sub-Saharan Africa (Southern)') %>% 
  pull(value) %>% t.test()
(df %>% 
    filter(year == 2100, RCP == 'rcp85', Region == 'Sub-Saharan Africa (Southern)') %>% 
    pull(value) > 0) %>% table() %>% prop.table()
