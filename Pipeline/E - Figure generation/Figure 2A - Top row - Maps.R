
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(sf)
library(lubridate)
library(rgdal)
library(patchwork)
library(multiscales)

setwd("D:/MalariaAfrica/HistoricalTempFiles")
setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c('scenario','GCM','year','OBJECTID'))
rows <- which(meta$year %in% c(1901:1905, 2010:2014))
meta <- meta[rows,]
# Yes I know the below solution is a little janky, god forgive me 
meta$year[meta$year %in% c(1901:1905)] <- 1901
meta$year[meta$year %in% c(2010:2014)] <- 2014 

for (i in 1:1000) {  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- iter[rows,]
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,GCM,year,OBJECTID']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} 

iter.df %<>% as_tibble()
iter.df %<>% pivot_longer(cols = -c(scenario, GCM, year, OBJECTID), names_to = c("Pred"))

vroom::vroom_write(iter.df, "~/Github/falciparum/TempFiles/Fig2Big.csv")

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

iter.df %>% 
  filter(year == 2014) -> slices

slices %>% pivot_wider(names_from = scenario, values_from = value) %>% 
  mutate(diff = (hist - nat)) %>%
  select(-c(hist,nat,year)) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) ->
  slice.map1

### ADD THE MAP

slice.map1 %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- readOGR('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')
cont@data <- left_join(cont@data, slice.map1)

sfcont <- st_as_sf(cont)
sfcont %<>% mutate(moe = 1 - abs(sfcont$runs.diff-5500)/5500) # make 55000 when 110000 runs

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

leg <- cowplot::get_legend(map.diff)

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

iter.df %>% 
  filter(scenario == 'hist') -> slices

slices %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2014` - `1901`)) %>%
  select(-c(`2014`,`1901`,'scenario')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) ->
  slice.map2

### ADD THE MAP

slice.map2 %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- readOGR('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')
cont@data <- left_join(cont@data, slice.map2)

sfcont <- st_as_sf(cont)
sfcont %<>% mutate(moe = 1 - abs(sfcont$runs.diff-5500)/5500)

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

map.mean.no.legend + map.diff.no.legend + leg + plot_layout(widths = c(4, 4, 2)) -> top
#rm(all)