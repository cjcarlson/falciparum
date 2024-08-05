
library(sf)
# library(rgdal)
library(tidyverse)
library(patchwork)
library(viridisLite)

sf::sf_use_s2(FALSE)

cont <- st_read(file.path(datadir, 'Data', 'AfricaADM1.shp')) |> 
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

prev_sf <- file.path(datadir, 'Data', 'dataverse_files', '00 Africa 1900-2015 SSA PR database (260617).csv') |> 
  read.csv() |> 
 st_as_sf(coords = c("Long", "Lat"), crs = st_crs(cont))


cont$meanprev <- st_join(cont, prev_sf) %>%
  group_by(OBJECTID) %>%
  summarise(meanprev = mean(PfPR2.10, na.rm = TRUE)) %>%
  pull(meanprev)


cont$npts <- st_intersects(cont, prev_sf) %>%
  lengths()


ggplot() + 
  geom_sf(data = cont, aes(fill = npts), color = NA) +
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

ggplot(cont) + 
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

