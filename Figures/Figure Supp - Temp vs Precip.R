
library(sf)
library(lubridate)
library(magrittr)
library(rgdal)
library(tidyverse)
library(patchwork)

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
rm(natdf); rm(histdf)

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
# 
# all %>% filter(Country == 'Ethiopia') %>%
#   select(year, Pf.temp, run, scenario) %>%
#   group_by(year, run, scenario) %>% 
#   summarize(Pf.temp = mean(Pf.temp, na.rm = TRUE)) %>% 
#   ggplot(aes(x = year, y = Pf.temp)) + 
#   theme_bw() + 
#   geom_smooth(aes(color = scenario, group = scenario)) + 
#   geom_line(aes(color = scenario,
#                 group = interaction(run,scenario)),
#             alpha = 0.3)

# Region read-in

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

all %<>% unite("monthyr", month:year, sep=' 1 ', remove=FALSE) 
all %<>% mutate(monthyr = mdy(monthyr))
all %<>% mutate(monthyr = monthyr - ymd("1900-01-01"))
all %<>% mutate(monthyr = as.numeric(monthyr))

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
# 
# all %>% # filter(Country == 'Ethiopia') %>%
#   select(year, Pred, run, scenario) %>%
#   group_by(year, run, scenario) %>%
#   summarize(Pred = mean(Pred, na.rm = TRUE)) %>%
#   ggplot(aes(x = year, y = Pred, group = interaction(run,scenario))) +
#   theme_bw() +
#   labs(y = "Climate-attributable prevalence") +
#   geom_line(aes(color = scenario,
#                 group = interaction(run,scenario)),
#             alpha = 0.3)

small <- all # %>% filter(Region == 'Sub-Saharan Africa (East)')

small %<>% 
  ungroup() %>%
  select(year, Pf.temp, run, scenario) %>%
  group_by(year, run, scenario) %>% 
  summarize(Pf.temp = mean(Pf.temp, na.rm = TRUE))
  
small %<>% 
  separate(run, c("gcm", "rcp"), sep = "-rcp") %>% 
  unite(line, c(scenario, rcp), sep='') %>%
  mutate(line = recode(line, !!!c('histNA' = 'hist',
                                  'natNA' = 'nat',
                                  'hist26' = 'rcp26',
                                  'hist45' = 'rcp45',
                                  'hist85' = 'rcp85')))

smallpast <- small %>% filter(line %in% c('hist','nat'))
smallfuture <- small %>% filter(line %in% c('rcp26', 'rcp45', 'rcp85'))

smallpast %<>% 
  ungroup() %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(gcm, line) %>%
  summarize(BetaMean = mean(Pf.temp, na.rm = TRUE)) %>% 
  right_join(smallpast) %>% 
  mutate(Pf.temp = (Pf.temp-BetaMean)) %>%
  select(-BetaMean) 

smallfuture %<>% 
  ungroup() %>%
  filter(year %in% c(2015)) %>% 
  select(gcm, line, Pf.temp) %>%
  dplyr::rename(Pf.temp2015 = Pf.temp) %>%
  right_join(smallfuture) %>%
  mutate(Pf.temp = (Pf.temp - Pf.temp2015)) %>%
  select(-Pf.temp2015) 

aligner <- smallpast %>% 
  filter(year == 2014, line == 'hist') %>%
  select(gcm, Pf.temp) %>%
  dplyr::rename(Pf.temp2014 = Pf.temp)

smallfuture %<>%
  left_join(aligner) %>%
  mutate(Pf.temp = Pf.temp + Pf.temp2014) %>%
  select(-Pf.temp2014) 

small <- bind_rows(smallpast, smallfuture)

small %>% 
  group_by(year, line) %>%
  summarize(mean = mean(Pf.temp)) -> binddata

small %<>% left_join(binddata)

small %>%
  mutate(line = factor(line, levels = c('nat', 'hist', 'rcp26', 'rcp45', 'rcp85'))) %>%
  ggplot(aes(x = year, y = Pf.temp, 
             color = line)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  geom_line(aes(group = interaction(gcm,line)), lwd = 0.65, alpha = 0.25) + 
  scale_color_manual(values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Historical counterfactual', 'Historical climate', 'Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                     name = '') + 
  geom_vline(xintercept = 2014.5, linetype = 'dashed') + 
  geom_line(aes(x = year, y = mean), lwd = 1.3) + 
  #geom_smooth(lwd = 1.3, se = FALSE) + 
  xlab("Year") + ylab("Change in prevalence (%)") + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 6),
        plot.margin = unit(c(0.5,0.5,1,1), "cm"), 
        legend.position = c(0.13, 0.35)) -> s1

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
# 
# all %>% # filter(Country == 'Ethiopia') %>%
#   select(year, Pred, run, scenario) %>%
#   group_by(year, run, scenario) %>%
#   summarize(Pred = mean(Pred, na.rm = TRUE)) %>%
#   ggplot(aes(x = year, y = Pred, group = interaction(run,scenario))) +
#   theme_bw() +
#   labs(y = "Climate-attributable prevalence") +
#   geom_line(aes(color = scenario,
#                 group = interaction(run,scenario)),
#             alpha = 0.3)

small <- all # %>% filter(Region == 'Sub-Saharan Africa (East)')

small %<>% 
  ungroup() %>%
  select(year, Pf.flood, run, scenario) %>%
  group_by(year, run, scenario) %>% 
  summarize(Pf.flood = mean(Pf.flood, na.rm = TRUE))

small %<>% 
  separate(run, c("gcm", "rcp"), sep = "-rcp") %>% 
  unite(line, c(scenario, rcp), sep='') %>%
  mutate(line = recode(line, !!!c('histNA' = 'hist',
                                  'natNA' = 'nat',
                                  'hist26' = 'rcp26',
                                  'hist45' = 'rcp45',
                                  'hist85' = 'rcp85')))

smallpast <- small %>% filter(line %in% c('hist','nat'))
smallfuture <- small %>% filter(line %in% c('rcp26', 'rcp45', 'rcp85'))

smallpast %<>% 
  ungroup() %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(gcm, line) %>%
  summarize(BetaMean = mean(Pf.flood, na.rm = TRUE)) %>% 
  right_join(smallpast) %>% 
  mutate(Pf.flood = (Pf.flood-BetaMean)) %>%
  select(-BetaMean) 

smallfuture %<>% 
  ungroup() %>%
  filter(year %in% c(2015)) %>% 
  select(gcm, line, Pf.flood) %>%
  dplyr::rename(Pf.flood2015 = Pf.flood) %>%
  right_join(smallfuture) %>%
  mutate(Pf.flood = (Pf.flood - Pf.flood2015)) %>%
  select(-Pf.flood2015) 

aligner <- smallpast %>% 
  filter(year == 2014, line == 'hist') %>%
  select(gcm, Pf.flood) %>%
  dplyr::rename(Pf.flood2014 = Pf.flood)

smallfuture %<>%
  left_join(aligner) %>%
  mutate(Pf.flood = Pf.flood + Pf.flood2014) %>%
  select(-Pf.flood2014) 

small <- bind_rows(smallpast, smallfuture)

small %>% 
  group_by(year, line) %>%
  summarize(mean = mean(Pf.flood)) -> binddata

small %<>% left_join(binddata)

small %>%
  mutate(line = factor(line, levels = c('nat', 'hist', 'rcp26', 'rcp45', 'rcp85'))) %>%
  ggplot(aes(x = year, y = Pf.flood, 
             color = line)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  geom_line(aes(group = interaction(gcm,line)), lwd = 0.65, alpha = 0.25) + 
  scale_color_manual(values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Historical counterfactual', 'Historical climate', 'Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                     name = '') + 
  geom_vline(xintercept = 2014.5, linetype = 'dashed') + 
  geom_line(aes(x = year, y = mean), lwd = 1.3) + 
  #geom_smooth(lwd = 1.3, se = FALSE) + 
  xlab("Year") + ylab("Change in prevalence (%)") + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 6),
        plot.margin = unit(c(0.5,0.5,1,1), "cm"), 
        legend.position = 'none') -> s2


###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
# 
# all %>% # filter(Country == 'Ethiopia') %>%
#   select(year, Pred, run, scenario) %>%
#   group_by(year, run, scenario) %>%
#   summarize(Pred = mean(Pred, na.rm = TRUE)) %>%
#   ggplot(aes(x = year, y = Pred, group = interaction(run,scenario))) +
#   theme_bw() +
#   labs(y = "Climate-attributable prevalence") +
#   geom_line(aes(color = scenario,
#                 group = interaction(run,scenario)),
#             alpha = 0.3)

small <- all # %>% filter(Region == 'Sub-Saharan Africa (East)')

small %<>% 
  ungroup() %>%
  select(year, Pf.drought, run, scenario) %>%
  group_by(year, run, scenario) %>% 
  summarize(Pf.drought = mean(Pf.drought, na.rm = TRUE))

small %<>% 
  separate(run, c("gcm", "rcp"), sep = "-rcp") %>% 
  unite(line, c(scenario, rcp), sep='') %>%
  mutate(line = recode(line, !!!c('histNA' = 'hist',
                                  'natNA' = 'nat',
                                  'hist26' = 'rcp26',
                                  'hist45' = 'rcp45',
                                  'hist85' = 'rcp85')))

smallpast <- small %>% filter(line %in% c('hist','nat'))
smallfuture <- small %>% filter(line %in% c('rcp26', 'rcp45', 'rcp85'))

smallpast %<>% 
  ungroup() %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(gcm, line) %>%
  summarize(BetaMean = mean(Pf.drought, na.rm = TRUE)) %>% 
  right_join(smallpast) %>% 
  mutate(Pf.drought = (Pf.drought-BetaMean)) %>%
  select(-BetaMean) 

smallfuture %<>% 
  ungroup() %>%
  filter(year %in% c(2015)) %>% 
  select(gcm, line, Pf.drought) %>%
  dplyr::rename(Pf.drought2015 = Pf.drought) %>%
  right_join(smallfuture) %>%
  mutate(Pf.drought = (Pf.drought - Pf.drought2015)) %>%
  select(-Pf.drought2015) 

aligner <- smallpast %>% 
  filter(year == 2014, line == 'hist') %>%
  select(gcm, Pf.drought) %>%
  dplyr::rename(Pf.drought2014 = Pf.drought)

smallfuture %<>%
  left_join(aligner) %>%
  mutate(Pf.drought = Pf.drought + Pf.drought2014) %>%
  select(-Pf.drought2014) 

small <- bind_rows(smallpast, smallfuture)

small %>% 
  group_by(year, line) %>%
  summarize(mean = mean(Pf.drought)) -> binddata

small %<>% left_join(binddata)

small %>%
  mutate(line = factor(line, levels = c('nat', 'hist', 'rcp26', 'rcp45', 'rcp85'))) %>%
  ggplot(aes(x = year, y = Pf.drought, 
             color = line)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  geom_line(aes(group = interaction(gcm,line)), lwd = 0.65, alpha = 0.25) + 
  scale_color_manual(values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Historical counterfactual', 'Historical climate', 'Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                     name = '') + 
  geom_vline(xintercept = 2014.5, linetype = 'dashed') + 
  geom_line(aes(x = year, y = mean), lwd = 1.3) + 
  #geom_smooth(lwd = 1.3, se = FALSE) + 
  xlab("Year") + ylab("Change in prevalence (%)") + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 6),
        plot.margin = unit(c(0.5,0.5,1,1), "cm"), 
        legend.position = 'none') -> s3

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

library(patchwork)
s1 / s2 / s3 + plot_annotation(tag_levels = 'A')

