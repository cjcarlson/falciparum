
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(sf)
library(lubridate)
library(rgdal)
library(patchwork)
library(multiscales)
library(vroom)
library(colorspace)

cont <- read_sf('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')

iter.df <- vroom("~/Github/falciparum/TempFiles/Fig4Big2050.csv")

iter.df %>% 
  filter(RCP == 'rcp26') -> slices

slices %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2050` - `2015`)) %>%
  select(-c(`2050`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff.205026 = mean(diff)) ->
  slice.map

### ADD 

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- left_join(cont, slice.map)

##############

iter.df %>% 
  filter(RCP == 'rcp85') -> slices

slices %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2050` - `2015`)) %>%
  select(-c(`2050`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff.205085 = mean(diff)) ->
  slice.map 

### ADD 

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- left_join(cont, slice.map)

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

iter.df <- vroom("~/Github/falciparum/TempFiles/Fig4Big2100.csv")

iter.df %>% 
  filter(RCP == 'rcp26') -> slices

slices %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2100` - `2015`)) %>%
  select(-c(`2100`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff.210026 = mean(diff)) ->
  slice.map

### ADD 

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- left_join(cont, slice.map)

##############

iter.df %>% 
  filter(RCP == 'rcp85') -> slices

slices %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2100` - `2015`)) %>%
  select(-c(`2100`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff.210085 = mean(diff)) ->
  slice.map

### ADD 

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- left_join(cont, slice.map)

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

iter.df <- vroom("~/Github/falciparum/TempFiles/Fig4BigRCP45.csv")


iter.df %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2050` - `2015`)) %>%
  select(-c(`2100`,`2050`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff.205045 = mean(diff)) ->
  slice.map

### ADD 

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- left_join(cont, slice.map)

##############

iter.df %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2100` - `2015`)) %>%
  select(-c(`2100`,`2050`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff.210045 = mean(diff)) ->
  slice.map

### ADD 

slice.map %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- left_join(cont, slice.map)

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff.205026), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white",
                                   limits = range(cont$mean.diff.210026, na.rm = TRUE)) +
  labs(fill = "Change (%)") -> g1

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff.210026), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white",
                                   limits = range(cont$mean.diff.210026, na.rm = TRUE)) +
  labs(fill = "Change (%)") -> g2

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff.205045), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white",
                                   limits = range(cont$mean.diff.210045, na.rm = TRUE)) +
  labs(fill = "Change (%)") -> g3

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff.210045), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white",
                                   limits = range(cont$mean.diff.210045, na.rm = TRUE)) +
  labs(fill = "Change (%)") -> g4

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff.205085), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white",
                                   limits = range(cont$mean.diff.210085, na.rm = TRUE)) +
  labs(fill = "Change (%)") -> g5

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff.210085), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white",
                                   limits = range(cont$mean.diff.210085, na.rm = TRUE)) +
  labs(fill = "Change (%)") -> g6

((g1 + g2 + plot_layout(guides = 'collect')) / 
  (g3 + g4 + plot_layout(guides = 'collect')) / 
  (g5 + g6 + plot_layout(guides = 'collect'))) + plot_annotation(tag_levels = 'A')
  