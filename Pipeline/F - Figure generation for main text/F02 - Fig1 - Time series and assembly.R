
library(sf)
library(lubridate)
library(magrittr)
# library(rgdal)
library(tidyverse)
library(patchwork)

# setwd('C:/Users/cjcar/Dropbox/MalariaAttribution')

gbod <- sf::read_sf(file.path(datadir, "Data", "OriginalGBD", "WorldRegions.shp"))
# gbod@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')

# prev<- read.csv('./Data/dataverse_files/00 Africa 1900-2015 SSA PR database (260617).csv')
# prev <- SpatialPointsDataFrame(coords=prev[,c('Long','Lat')], data=prev[,c('MM','YY','Pf','PfPR2.10')])
# prev@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')

# o <- over(prev, gbod)

o <- st_join(prev_sf, gbod)

df <- tibble(prev_sf) 
df$region <- o$SmllRgn

df %<>% unite("monthyr", MM:YY, sep=' 1 ', remove=FALSE) 
df %<>% mutate(monthyr = mdy(monthyr))

df2 <- df
df2$region <- 'Continent-wide'
df <- bind_rows(df2, df)

df %>% 
  filter(!is.na(region)) %>%
  mutate(region = recode(region, !!!c('Sub-Saharan Africa (Central)' = 'Central Africa',
                                      'Sub-Saharan Africa (East)' = 'East Africa',
                                      'Sub-Saharan Africa (Southern)' = 'Southern Africa',
                                      'Sub-Saharan Africa (West)' = 'West Africa'))) %>%
  mutate(region = factor(region, levels = c('Continent-wide', 'Central Africa', 'East Africa', 'Southern Africa', 'West Africa'))) %>%
  ggplot(aes(x = monthyr, y = PfPR2.10)) + 
  theme_bw() + 
  geom_rect(aes(xmin=ymd('1955-01-01'),
                xmax = ymd('1969-01-06'),
                ymin = 100,
                ymax = 0), fill = 'pink', alpha = 0.05) + 
  geom_rect(aes(xmin=ymd('2000-01-01'),
                xmax = ymd('2015-01-01'),
                ymin = 100,
                ymax = 0), fill = 'pink', alpha = 0.05) + 
  geom_point(alpha = 0.03, col = "#214d65",
             shape = 16, stroke = 0) + 
  geom_smooth(method = 'gam', col =  "#287DAB") + 
  facet_wrap(~region, nrow = 5) + 
  xlab(NULL) + ylab(expression(paste(italic("falciparum")," malaria prevalence, ages 2-10 (%)"))) -> ts

ts

# Run Figure 1 Draft map summary.R first 

library(patchwork)

((map.n / map.p) | ts)  + plot_layout(widths = c(1.5, 1)) + plot_annotation(tag_levels = 'A')


ggsave(
  filename = "Figure1.pdf",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 9,
  height = 10,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure1.jpg",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 9,
  height = 10,
  units = "in"
)
