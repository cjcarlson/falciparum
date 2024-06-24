
library(sf)
library(lubridate)
library(magrittr)
# library(rgdal)
library(tidyverse)
library(patchwork)
library(multiscales)

# iter.df <- vroom("~/Github/falciparum/TempFiles/Fig3Big.csv")
# 
# iter.df %>%
#   mutate(GCM = str_replace_all(GCM,'./Historical/','')) %>%
#   mutate(GCM = str_replace_all(GCM,'./Future/','')) %>%
#   mutate(GCM = str_replace_all(GCM,'BCC-CSM2-MR','BCC-CSM2')) -> iter.df

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

iter.df <- here::here("TempFiles", "Fig3Big.feather") |> 
  arrow::read_feather() |>
  dplyr::mutate(model = stringr::str_replace_all(model,'BCC-CSM2-MR','BCC-CSM2')) 

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

iter.df %>% 
  filter(year == 2014) -> slices

slices %>% pivot_wider(names_from = scenario, values_from = Pred) %>% 
  mutate(diff = (historical - `hist-nat`)) %>%
  select(-c(historical, `hist-nat`, year)) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff = mean(diff, na.rm = TRUE), 
            lower.diff = quantile(diff, 0.05, na.rm = TRUE), 
            upper.diff = quantile(diff, 0.95, na.rm = TRUE)) ->
  slice.map1

###########################
###########################
###########################
###########################

# Get the stuff 
elev <- file.path(datadir, "Data", "elevation", "elevation_extracted_all_ADM1.csv") |> 
  readr::read_csv(show_col_types = FALSE) 

cont <- file.path(datadir, 'Data', 'AfricaADM1.shp') |> 
  sf::read_sf()

latlon <- cont %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
latlon %>%
  as.data.frame() %>%
  select(OBJECTID, lat) %>%
  mutate(OBJECTID = as.numeric(OBJECTID)) -> lat

temp <- file.path(datadir, "Data", "CRU-Reextraction-Aug2022.csv") |> 
  readr::read_csv(show_col_types = FALSE) 
temp %>% 
  filter(year %in% c(1901:1930)) %>%
  group_by(OBJECTID) %>% 
  summarize(t = mean(temp, na.rm = TRUE)) -> tmean

###########################
###########################
###########################
###########################
###########################
###########################

slice.map1 %>%
  left_join(elev %>% select(OBJECTID, elevmn)) %>%
  left_join(lat) %>%
  left_join(tmean) -> df

# Generate a nice little significance color scheme
df %>%
  mutate(sign = as.numeric(lower.diff > 0) + -1*as.numeric(upper.diff < 0)) %>%
  mutate(sign = factor(sign)) -> df
  
df %>%
  na.omit() %>%
  ggplot(aes(x = mean.diff, y = elevmn, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Elevation (m)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,10,0)) + 
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) -> g1

df %>%
  na.omit() %>%
  ggplot(aes(x = mean.diff, y = lat, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Latitude") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,0,0)) + 
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) -> g2

df %>%
  na.omit() %>%
  ggplot(aes(x = mean.diff, y = t, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Mean temperature (1901-1930)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,0,0)) + 
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) -> g3
