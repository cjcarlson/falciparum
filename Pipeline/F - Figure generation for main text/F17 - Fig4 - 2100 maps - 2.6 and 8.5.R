
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(sf)
library(lubridate)
library(rgdal)
library(patchwork)
library(multiscales)
library(vroom)

iter.df <- vroom("~/Github/falciparum/TempFiles/Fig4Big2100.csv")

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

iter.df %>% 
  filter(RCP == 'rcp26') -> slices

slices %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2100` - `2015`)) %>%
  select(-c(`2100`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) ->
  slice.map

### ADD THE MAP

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- readOGR('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')
cont@data <- left_join(cont@data, slice.map)

sfcont <- st_as_sf(cont)
sfcont %<>% mutate(moe = 1 - abs(sfcont$runs.diff-5500)/5500)

colors <- scales::colour_ramp(
  colors = c(red = "#AC202F", purple = "#740280", blue = "#2265A3")
)((0:7)/7)

colors <- rev(colors)

library(multiscales)
ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.1) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  bivariate_scale("fill",
                  pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
                  name = c("Change in prevalence (%)", "sign uncertainty"),
                  limits = list(c(-4.5, 4.5), c(0, 1)),
                  breaks = list(c(-4.5, -2, 0, 2, 4.5), c(0, 0.25, 0.5, 0.75, 1)),
                  labels = list(waiver(), scales::percent),
                  guide = "colourfan") +
  theme_void() +
  theme(
    legend.key.size = grid::unit(1, "cm"),
    legend.title.align = 0.5,
    plot.margin = margin(0, 0, 0, 0)
  ) + ggtitle('C') + 
  theme(plot.title = element_text(size = 25))  -> map.rcp26

legend <- cowplot::get_legend(map.rcp26)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.1) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  bivariate_scale("fill",
                  pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
                  name = c("Change in prevalence (%)", "sign uncertainty"),
                  limits = list(c(-4.5, 4.5), c(0, 1)),
                  breaks = list(c(-4.5, -2, 0, 2, 4.5), c(0, 0.25, 0.5, 0.75, 1)),
                  labels = list(waiver(), scales::percent)) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18),
    legend.key.size = grid::unit(1, "cm"),
    legend.title.align = 0.5,
    plot.margin = margin(0, 0, 0, 0)
  ) + ggtitle('D') -> map.rcp26.2100

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

iter.df %>% 
  filter(RCP == 'rcp85') -> slices

slices %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2100` - `2015`)) %>%
  select(-c(`2100`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) ->
  slice.map2

### ADD THE MAP

slice.map2 %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- readOGR('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')
cont@data <- left_join(cont@data, slice.map2)

sfcont <- st_as_sf(cont)
sfcont %<>% mutate(moe = 1 - abs(sfcont$runs.diff-5500)/5500)

colors <- scales::colour_ramp(
  colors = c(red = "#AC202F", purple = "#740280", blue = "#2265A3")
)((0:7)/7)

colors <- rev(colors)

library(multiscales)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.1) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  bivariate_scale("fill",
                  pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
                  name = c("Change in prevalence (%)", "sign uncertainty"),
                  limits = list(c(-4.5, 4.5), c(0, 1)),
                  breaks = list(c(-4.5, -2, 0, 2, 4.5), c(0, 0.25, 0.5, 0.75, 1)),
                  labels = list(waiver(), scales::percent)) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18),
    legend.key.size = grid::unit(1, "cm"),
    legend.title.align = 0.5,
    plot.margin = margin(0, 0, 0, 0)
  ) + ggtitle('F')  -> map.rcp85.2100
