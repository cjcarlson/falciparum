
setwd("C:/Users/cjcar/Dropbox/MalariaAttribution")

library(tidyverse)
library(magrittr)
library(rgdal)
library(zoo)
library(lubridate)
library(vroom)

###########

# 1. Get the climate data - every file with a future projection 
# (Note: Some are currently missing and need to be restored)

future.files <- list.files('./ClimateCSVs', pattern="rcp", full.names = TRUE)

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(run = flnm)
}

future.files %>% 
  map_df(~read_plus(.)) %>% 
  mutate(run = gsub('./ClimateCSVs/','', run)) %>%
  mutate(run = gsub('.csv','', run)) %>% 
  select(-X1) -> 
  future.df

# Grab the RCPs more systematically

future.df %<>%
  tidyr::extract(run, 
          into = c('GCM','RCP'),
          regex = "(BCC-CSM2|BCC-CSM2|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
          remove = FALSE)

###########

# 2. Load the spatial information 

# First, bind country information to the OBJECTID's

read_csv('./Dataframe backups/shapefile-backup.csv') %>%
  select(OBJECTID, NAME_0) %>%
  unique() %>% 
  dplyr::rename(Country = NAME_0) -> 
  countrydf

future.df %<>% left_join(countrydf)

# Next, add the GBD regions back in

gbod <- readOGR(file.path("Data", "OriginalGBD", "WorldRegions.shp"))

gbod@data %>%
  as_tibble() %>%
  select("ISO", "NAME_0", "SmllRgn") %>% 
  group_by(ISO, NAME_0) %>% 
  summarize(SmllRgn = first(SmllRgn)) %>%
  rename(Country = "NAME_0", Region = "SmllRgn") %>%
  mutate(Country = gsub('Cote D\'Ivoire','Côte d\'Ivoire', Country)) -> gboddf

future.df %<>% left_join(gboddf); rm(gbod, gboddf) # Clean up a bit as you go

# future.df %>% filter(Region == 'Asia (Southeast)') %>% select(Country) %>% unique()
# future.df %>% filter(Region == 'North Africa & Middle East') %>% select(Country) %>% unique()
# future.df %>% filter(is.na(Region)) %>% select(Country) %>% unique()

future.df %<>% filter(!(Region %in% c('Asia (Southeast)', 'North Africa & Middle East')))

# Finally: create the flow of linear time. You are like a god to this dataset

future.df %>% 
  unite("monthyr", month:year, sep=' ', remove=FALSE) %>% 
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>% 
  mutate(monthyr = as.numeric(ymd(monthyr)-ymd("2015-01-01"))) -> future.df

###########

# 3. Load the bootstraps, and gather coefficients

bootstrap <- readRDS("Results/Models/block_bootstrap_cXt2intrXm.rds")
bootstrap <- as_tibble(bootstrap)

# Load the precip thresholds

precip.key <- read_csv('~/Github/falciparum/precipkey.csv')

# Scaffolding: only use the first bootstrap, and reset

setwd("D:/MalariaAfrica/FutureTempFiles")

for (i in c(686:1000)) {
  
coef <- bootstrap[i,]
iter.df <- future.df
 
# Effect of temperature

iter.df %<>%
  mutate(Pf.temp = coef$temp*temp + coef$temp2*temp2)

# # Detour: What does temperature effect look like into the future?
# iter.df %>%
#   select(GCM, RCP, year, Pf.temp) %>%
#   group_by(GCM, RCP, year) %>%
#   summarize(Pf.temp = mean(Pf.temp, na.rm = TRUE)) -> temp.df
# temp.df %>%
#   unite(GCMxRCP, GCM:RCP, remove = FALSE) %>%
#   ggplot(aes(x = year, y = Pf.temp, group = GCMxRCP, color = RCP)) + geom_line()

# Effect of drought and floods - load the benchmarks

iter.df %>% 
  left_join(precip.key) %>% 
  rename(ppt.90 = ppt_pctile0.9, 
         ppt.10 = ppt_pctile0.1) -> iter.df

# Flood and lags

iter.df %>%
  mutate(flood = as.numeric(ppt >= ppt.90)) %>%
  group_by(OBJECTID) %>%
  mutate(flood.lag = lag(flood, order_by = monthyr),
         flood.lag2 = lag(flood, order_by = monthyr, n=2),
         flood.lag3 = lag(flood, order_by = monthyr, n=3)) -> iter.df

# Drought and lags

iter.df %>%
  mutate(drought = as.numeric(ppt <= ppt.10)) %>% 
  group_by(OBJECTID) %>% 
  mutate(drought.lag = lag(drought, order_by = monthyr),
         drought.lag2 = lag(drought, order_by = monthyr, n=2),
         drought.lag3 = lag(drought, order_by = monthyr, n=3)) -> iter.df

# Calculate the effecst of floods and droughts 

iter.df %>% 
  mutate(Pf.flood = (coef[['flood']]*flood + 
                     coef[['flood.lag']]*flood.lag + 
                     coef[['flood.lag2']]*flood.lag2 + 
                     coef[['flood.lag3']]*flood.lag3),
         Pf.drought = (coef[['drought']]*drought + 
                       coef[['drought.lag']]*drought.lag + 
                       coef[['drought.lag2']]*drought.lag2 + 
                       coef[['drought.lag3']]*drought.lag3)) %>%
  mutate(Pf.prec = (Pf.flood + Pf.drought)) %>%
  mutate(Pred = (Pf.temp + Pf.prec)) -> iter.df

iter.df %>% select(OBJECTID, monthyr, run, Pred, Pf.temp, Pf.flood, Pf.drought)

# iter.df %>% 
#   select(GCM, RCP, year, Pf.prec) %>%
#   group_by(GCM, RCP, year) %>%
#   summarize(Pf.prec = mean(Pf.prec, na.rm = TRUE)) -> prec.df
# prec.df %>%
#   unite(GCMxRCP, GCM:RCP, remove = FALSE) %>%
#   ggplot(aes(x = year, y = Pf.prec, group = GCMxRCP, color = RCP)) + geom_line()

if(i == 1) {
  iter.df %>% 
    select(OBJECTID, monthyr, month, year, run, Country, ISO, Region) %>%
    vroom_write("RowMetadata.csv")
}

iter.df %>% 
  ungroup %>% 
  select(Pred, Pf.temp, Pf.flood, Pf.drought) %>%
  vroom_write(paste(paste("iter", i, sep=""), ".csv", sep = ""))

}
