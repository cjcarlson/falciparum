
library(sf)
library(rgdal)
library(tidyverse)
library(patchwork)
library(viridisLite)
library(readr)
library(magrittr)

user = "Colin" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/'
  repo = "" #to fill in
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/Data'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else {
  wd = NA
  print('Script not configured for this user!')
}

setwd(wd)

cont <- readOGR('./Data/AfricaADM1.shp')

fe <- read_rds("./Results/Models/fixed_effects_cXt2intrXm.rds")

fe %<>%
  as_tibble() %>%
  dplyr::select(idx, effect) %>%
  rename(OBJECTID = 'idx')

cont@data %<>% left_join(fe)

sfcont <- st_as_sf(cont)

ggplot() + 
  geom_sf(data = sfcont, aes(fill = effect), color = NA) +
  coord_sf(datum = NA, 
           xlim = c(-19, 53)) + 
  theme_void() +
  theme(legend.position = c(0.2,0.3)) + 
  # theme(legend.key.height = unit(2,'cm'),
  #       legend.key.width = unit(0.4, 'cm'),
  #       plot.margin = margin(0, 1, 0, 0)) + 
  scale_fill_gradientn('Effect', 
                       colours = mako(100))  + 
  guides(fill = guide_colourbar(#title.position = "top",
    #title.vjust = 1.5,
    ticks = FALSE)) -> map.n
