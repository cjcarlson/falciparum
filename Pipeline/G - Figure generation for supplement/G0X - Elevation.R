
library(sf)
library(lubridate)
library(magrittr)
library(rgdal)
library(tidyverse)
library(patchwork)
library(multiscales)

iter.df <- vroom("~/Github/falciparum/TempFiles/Fig3Big.csv")

iter.df %>%
  mutate(GCM = str_replace_all(GCM,'./Historical/','')) %>%
  mutate(GCM = str_replace_all(GCM,'./Future/','')) %>%
  mutate(GCM = str_replace_all(GCM,'BCC-CSM2-MR','BCC-CSM2')) -> iter.df

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
  summarize(mean.diff = mean(diff, na.rm = TRUE), 
            lower.diff = quantile(diff, 0.05, na.rm = TRUE), 
            upper.diff = quantile(diff, 0.95, na.rm = TRUE)) ->
  slice.map1

###########################
###########################
###########################
###########################

# Get the stuff 

elev <- read_csv("C:/Users/cjcar/Dropbox/MalariaAttribution/Data/elevation/elevation_extracted_all_ADM1.csv")

cont <- read_sf('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')
latlon <- cont %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
latlon %>%
  as.data.frame() %>%
  select(OBJECTID, lat) %>%
  mutate(OBJECTID = as.numeric(OBJECTID)) -> lat

temp <- read_csv("C:/Users/cjcar/Dropbox/MalariaAttribution/Data/CRU-Reextraction-Aug2022.csv")
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
  ggplot(aes(x = mean.diff, y = elevmn, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Estimated change in prevalence") + 
  ylab("Elevation (m)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n') + 
  scale_color_manual(values = c("#00AFBB","grey80","#fa5340")) -> g1

df %>%
  ggplot(aes(x = mean.diff, y = lat, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Estimated change in prevalence") + 
  ylab("Latitude") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n') + 
  scale_color_manual(values = c("#00AFBB","grey80","#fa5340")) -> g2

df %>%
  ggplot(aes(x = mean.diff, y = t, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Estimated change in prevalence") + 
  ylab("Mean temperature (1901-1930)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n') + 
  scale_color_manual(values = c("#00AFBB","grey80","#fa5340")) -> g3

g3 + g1 + g2

        