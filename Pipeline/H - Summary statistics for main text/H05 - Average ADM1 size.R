library(tidyverse)
library(here)
library(sf)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

cont <- sf::read_sf(here::here(datadir, 'Data', 'AfricaADM1.shp')) |> 
  sf::st_transform("ESRI:54009") %>%
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(.)) / 1e+06) |> 
  sf::st_transform("EPSG:4326")

cat(
  "Mean area: ", mean(cont$area_km2), "km2\n",
  "Median area: ", median(cont$area_km2), "km2\n", sep = ""
)

ggplot(data = cont, aes(x = area_km2)) +
  geom_histogram(bins = 100) +
  theme_classic()

ggplot(data = cont, aes(fill = area_km2)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "inside", legend.position.inside = c(0.15, 0.25))
