
library(sf)
library(lubridate)
library(magrittr)
library(rgdal)
library(tidyverse)
library(patchwork)
library(multiscales)
library(vroom)
library(colorspace)

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

cont <- read_sf('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')

cont %<>% left_join(slice.map1 %>% mutate(OBJECTID = as.character(OBJECTID)))

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white") +
  labs(fill = "Change (%)") + 
  theme(legend.position = c(0.15, 0.25)) -> g1

cont %>%
  mutate(sign = as.numeric(lower.diff > 0) + -1*as.numeric(upper.diff < 0)) %>%
  mutate(sign = factor(sign)) -> cont

ggplot(cont) + 
  geom_sf(aes(fill = sign), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_manual(values = c("#00AFBB","grey80","#fa5340"), na.value = "white", na.translate = F,
                    labels = c('Decline','Insignificant','Increase')) +
  labs(fill = "Significance") + 
  theme(legend.position = c(0.15, 0.25))  -> g2 

g1 + g2 

### Scratch space

cont %>%
  mutate(sign = as.numeric(lower.diff > 0) + as.numeric(upper.diff < 0)) %>% 
  mutate(sign = replace_na(sign, 0)) %>%
  arrange(-sign) %>%
  mutate(sign = as.factor(sign)) -> cont

cont %>% select(sign) %>%
  filter(sign == 1) %>% 
  st_make_valid() %>% 
  st_union() %>% 
  st_make_valid() %>% 
  st_union() -> top

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff), color = 'grey70', size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  theme_void() + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white") +
  labs(fill = "Prevalence (%)") + 
  theme(legend.position = c(0.25, 0.35)) + 
  geom_sf(data = top, color = 'black', size = 0.25, fill = NA)
