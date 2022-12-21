
library(sf)
library(rgdal)
library(tidyverse)
library(patchwork)
library(viridisLite)

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

prev<- read.csv('./Data/dataverse_files/00 Africa 1900-2015 SSA PR database (260617).csv')
prev <- SpatialPointsDataFrame(coords=prev[,c('Long','Lat')], data=prev[,c('MM','YY','Pf','PfPR2.10')])
prev@proj4string <- cont@proj4string

cont$meanprev <- over(cont, prev, fn = mean)[,4]
# cont$meanprev19 <- over(cont, prev[prev$YY %in% c(1900:1999),], fn = mean)[,4]
# cont$meanprev20 <- over(cont, prev[prev$YY %in% c(2000:2015),], fn = mean)[,4]
cont$npts <- unlist(lapply(over(cont, prev, returnList = TRUE), nrow))

sfcont <- st_as_sf(cont)

ggplot() + 
  geom_sf(data = sfcont, aes(fill = npts), color = NA) +
  coord_sf(datum = NA, 
           xlim = c(-19, 53)) + 
  theme_void() +
  theme(legend.position = c(0.2,0.3)) + 
  # theme(legend.key.height = unit(2,'cm'),
  #       legend.key.width = unit(0.4, 'cm'),
  #       plot.margin = margin(0, 1, 0, 0)) + 
  scale_fill_gradientn('Samples', 
                       colours = mako(100),
                       trans = "log10")  + 
  guides(fill = guide_colourbar(#title.position = "top",
                                #title.vjust = 1.5,
                                ticks = FALSE)) -> map.n

ggplot(sfcont) + 
  geom_sf(aes(fill = meanprev), color = NA) +
  coord_sf(datum = NA, 
           xlim = c(-19, 53)) + 
  theme_void() +
  theme(legend.position = c(0.2,0.3)) + 
  # theme(legend.key.height = unit(1.5,'cm'),
  #       legend.key.width = unit(0.4, 'cm'),
  #       plot.margin = margin(0, 1, 0, 0)) + 
  scale_fill_gradientn('Prevalence (%)', 
                       colours = mako(100), 
                       limits = c(0,85))  + 
  guides(fill = guide_colourbar(#title.position = "top",
                                #title.vjust = 0.5,
                                ticks = FALSE))  -> map.p

library(patchwork)

map.n + map.p-> top

