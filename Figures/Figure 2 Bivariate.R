
library(sf)
library(lubridate)
library(magrittr)
library(rgdal)
library(tidyverse)
library(patchwork)
library(multiscales)

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

# # A specific country
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

sfcont <- st_as_sf(cont)
sfcont %<>% mutate(moe = 1 - abs(sfcont$runs.diff-5.5)/5.5)

colors <- scales::colour_ramp(
  colors = c(red = "#AC202F", purple = "#740280", blue = "#2265A3")
)((0:7)/7)

colors <- rev(colors)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.1) +
  coord_sf(datum = NA) +
  bivariate_scale("fill",
                  pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
                  name = c("Change in prevalence (%)", "sign uncertainty"),
                  limits = list(c(-1, 1), c(0, 1)),
                  breaks = list(c(-1, -0.5, 0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1)),
                  labels = list(waiver(), scales::percent),
                  guide = "colourfan") +
  theme_void() +
  theme(
    legend.key.size = grid::unit(1, "cm"),
    legend.title.align = 0.5 #,
    #plot.margin = margin(5.5, 12, 5.5, 5.5)
  ) -> map.diff

legend <- cowplot::get_legend(map.diff)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.1) +
  coord_sf(datum = NA, 
           xlim = c(-19, 53)) + 
  bivariate_scale("fill",
                  pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
                  name = c("Change in prevalence (%)", "sign uncertainty"),
                  limits = list(c(-1, 1), c(0, 1)),
                  breaks = list(c(-1, -0.5, 0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1)),
                  labels = list(waiver(), scales::percent)) +
  theme_void() +
  theme(
    #legend.key.size = grid::unit(1, "cm"),
    #legend.title.align = 0.5,
    plot.margin = margin(0, 0, 0, 0)
  ) + ggtitle('B') + 
  theme(plot.title = element_text(size = 25)) -> map.diff.no.legend 


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

sfcont <- st_as_sf(cont)
sfcont %<>% mutate(moe = 1 - abs(sfcont$runs.diff-5.5)/5.5)

library(multiscales)
ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.1) +
  coord_sf(datum = NA, 
           xlim = c(-19, 53)) + 
  bivariate_scale("fill",
                  pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
                  name = c("Change in prevalence (%)", "sign uncertainty"),
                  limits = list(c(-1, 1), c(0, 1)),
                  breaks = list(c(-1, -0.5, 0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1)),
                  labels = list(waiver(), scales::percent)) +
  theme_void() +
  theme(
    #legend.key.size = grid::unit(1, "cm"),
    #legend.title.align = 0.5,
    plot.margin = margin(0, 0, 0, 0)
  ) + ggtitle('A') + 
  theme(plot.title = element_text(size = 25))  -> map.mean.no.legend

map.mean.no.legend + map.diff.no.legend + legend + plot_layout(widths = c(4, 4, 2)) -> top
rm(all)