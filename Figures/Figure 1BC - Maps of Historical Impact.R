
library(magrittr)
library(tidyverse)

setwd('C:/Users/cjcar/Dropbox/MalariaAttribution')

coef <- readRDS("./Results/Models/coefficients_cXt2intrXm.rds")
coef.t1 <- coef[1,]
coef.t2 <- coef[2,]

all.files <- list.files('./ClimateCSVs', full.names = TRUE)
nat.files <- list.files('./ClimateCSVs', pattern="nat.csv", full.names = TRUE)
hist.files <- all.files[!(all.files %in% nat.files)]

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(run = flnm)
}

nat.files %>% 
  map_df(~read_plus(.)) %>% 
  mutate(run = gsub('./ClimateCSVs/','', run)) %>%
  mutate(run = gsub('-nat.csv','', run)) %>% 
  mutate(scenario = 'nat') -> 
  natdf

hist.files %>% 
  map_df(~read_plus(.)) %>% 
  mutate(run = gsub('./ClimateCSVs/','', run)) %>%
  mutate(run = gsub('.csv','', run)) %>% 
  mutate(scenario = 'hist') -> 
  histdf

all <- bind_rows(natdf, histdf)

all %<>% mutate(Pf.temp = (temp*coef.t1 + temp2*coef.t2))

all %>% select(year, Pf.temp, run, scenario) %>%
  group_by(year, run, scenario) %>% 
  summarize(Pf.temp = mean(Pf.temp, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Pf.temp, 
             color = scenario,
             group = interaction(run,scenario))) + 
  theme_bw() + geom_line()

# Break it down by region

read_csv('./Dataframe backups/shapefile-backup.csv') %>%
  select(OBJECTID, NAME_0) %>%
  unique() %>% 
  dplyr::rename(Country = NAME_0) -> 
  countrydf

all %<>% left_join(countrydf)

# A specific country

all %>% filter(Country == 'Ethiopia') %>%
  select(year, Pf.temp, run, scenario) %>%
  group_by(year, run, scenario) %>% 
  summarize(Pf.temp = mean(Pf.temp, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Pf.temp)) + 
  theme_bw() + 
  geom_smooth(aes(color = scenario, group = scenario)) + 
  geom_line(aes(color = scenario,
                group = interaction(run,scenario)),
            alpha = 0.3)

# Region read-in

library(rgdal)
gbod <- readOGR(file.path("Data", "OriginalGBD", "WorldRegions.shp"))
gboddf = as.data.frame(gbod@data)
gboddf %<>% dplyr::select("ISO", "NAME_0", "SmllRgn")
gboddf %>% group_by(ISO, NAME_0) %>% summarize(SmllRgn = first(SmllRgn)) -> gboddf # note that the small regions are homogenous within country
colnames(gboddf) = c("ISO", "country", "smllrgn")

gboddf %>% mutate(country = as.character(country)) %>%
  mutate(country = gsub('Cote D\'Ivoire','Côte d\'Ivoire', country)) -> gboddf

gboddf$smllrgn <- as.character(gboddf$smllrgn)

gboddf %<>% dplyr::rename(Country = country)

all %<>% left_join(gboddf)

all %>% dplyr::rename(Region = smllrgn) -> all
rm(gbod, gboddf)

# Clean?

all %>% filter(Region == 'Asia (Southeast)') %>% select(Country) %>% unique()
all %>% filter(Region == 'North Africa & Middle East') %>% select(Country) %>% unique()
all %>% filter(is.na(Region)) %>% select(Country) %>% unique()

all %<>% filter(!(Region %in% c('Asia (Southeast)', 'North Africa & Middle East')))

################################

precip.key <- read_csv('~/Github/falciparum/precipkey.csv')
# This produces something called precip.key 
# That has an adm1 level cutoff for 90/10 

all %>% left_join(precip.key) %>% 
  dplyr::rename(ppt.90 = ppt_pctile0.9, 
                ppt.10 = ppt_pctile0.1) -> all

# 
library(zoo)
library(lubridate)

all %>% unite("monthyr", month:year, sep=' ', remove=FALSE) %>% 
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>% 
  mutate(monthyr = as.numeric(ymd(monthyr)-ymd("1900-01-01"))) -> all

# flood and lags
all$flood <- as.numeric(all$ppt >= all$ppt.90)
all %>% group_by(OBJECTID) %>% 
  mutate(flood.lag = lag(flood, order_by = monthyr),
         flood.lag2 = lag(flood, order_by = monthyr, n=2),
         flood.lag3 = lag(flood, order_by = monthyr, n=3)) -> all

# drought lags
all$drought <- as.numeric(all$ppt <= all$ppt.10)
all %>% group_by(OBJECTID) %>% 
  mutate(drought.lag = lag(drought, order_by = monthyr),
         drought.lag2 = lag(drought, order_by = monthyr, n=2),
         drought.lag3 = lag(drought, order_by = monthyr, n=3)) -> all


all %>% mutate(Pf.flood = (coef['flood',]*flood + 
                             coef['flood.lag',]*flood.lag + 
                             coef['flood.lag2',]*flood.lag2 + 
                             coef['flood.lag3',]*flood.lag3),
               Pf.drought = (coef['drought',]*drought + 
                               coef['drought.lag',]*drought.lag + 
                               coef['drought.lag2',]*drought.lag2 + 
                               coef['drought.lag3',]*drought.lag3)) %>%
  mutate(Pf.prec = (Pf.flood + Pf.drought)) -> all

all$Pred <- all$Pf.temp + all$Pf.prec

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

all %>% select(run, scenario, OBJECTID, year, Pred) %>% filter(year == 2014) %>% 
  group_by(run, scenario, OBJECTID, year) %>% summarize(Pred = mean(Pred, na.rm = TRUE)) -> slices

slices %>% pivot_wider(names_from = scenario, values_from = Pred) %>% 
  mutate(diff = (hist - nat)) %>%
  select(-c(hist,nat,year)) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) ->
  slice.map

### ADD THE MAP

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))

cont <- readOGR('./Data/AfricaADM1.shp')

cont@data <- left_join(cont@data, slice.map)

library(sf)

# output a plot using ggplot
p1 <- ggplot() + 
  theme_void() + 
  geom_sf(aes(fill=mean.diff), data = st_as_sf(cont), color=NA) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                       high="red", space ="Lab") + 
  labs(fill = expression(Delta*"PfPR"["2-10"]))

p2 <- ggplot() + 
  theme_void() + 
  geom_sf(aes(fill=runs.diff), data = st_as_sf(cont), color=NA) +
  scale_fill_gradient2(midpoint=6, low="blue", mid="white",
                       high="red", space ="Lab") + 
  labs(fill = "+Runs")

all %>% # filter(Country == 'Ethiopia') %>%
  select(year, Pred, run, scenario) %>%
  group_by(year, run, scenario) %>% 
  summarize(Pred = mean(Pred, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Pred)) + 
  theme_bw() + 
  labs(y = "Climate-attributable prevalence") + 
  geom_smooth(aes(color = scenario, group = scenario)) + 
  geom_line(aes(color = scenario,
                group = interaction(run,scenario)),
            alpha = 0.3) -> p3

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

all %>% select(run, scenario, OBJECTID, year, Pred) %>% filter(scenario == 'hist', year %in% c(1901, 2014)) %>% 
  group_by(run, scenario, OBJECTID, year) %>% summarize(Pred = mean(Pred, na.rm = TRUE)) -> slices

slices %>% pivot_wider(names_from = year, values_from = Pred) %>% 
  mutate(diff = (`2014` - `1901`)) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) ->
  slice.map

### ADD THE MAP

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))

cont <- readOGR('./Data/AfricaADM1.shp')

cont@data <- left_join(cont@data, slice.map)

library(sf)

# output a plot using ggplot
p4 <- ggplot() + 
  theme_void() + 
  geom_sf(aes(fill=mean.diff), data = st_as_sf(cont), color=NA) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                       high="red", space ="Lab") + 
  labs(fill = expression(Delta*"PfPR"["2-10"]))

p5 <- ggplot() + 
  theme_void() + 
  geom_sf(aes(fill=runs.diff), data = st_as_sf(cont), color=NA) +
  scale_fill_gradient2(midpoint=6, low="blue", mid="white",
                       high="red", space ="Lab") + 
  labs(fill = "+Runs")

library(patchwork)
p3 / (p4 + p1) / (p5 + p2) + plot_annotation(tag_levels = c('A','B','C','D','E'))
