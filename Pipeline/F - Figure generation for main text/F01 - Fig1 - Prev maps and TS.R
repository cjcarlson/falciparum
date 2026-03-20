############################################################
# This script produces Figure 1 of the paper
############################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  sf,
  here,
  tidyverse,
  lubridate,
  magrittr,
  patchwork,
  viridisLite
)

sf::sf_use_s2(FALSE)

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))

############################################################
# Load data ----
############################################################

cont <- st_read(file.path(data_dir, 'Data', 'AfricaADM1.shp')) |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

gbod <- sf::read_sf(file.path(
  data_dir,
  "Data",
  "OriginalGBD",
  "WorldRegions.shp"
))

prev_sf <- file.path(
  data_dir,
  'Data',
  'dataverse_files',
  '00 Africa 1900-2015 SSA PR database (260617).csv'
) |>
  read.csv() |>
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(cont))

cont$meanprev <- st_join(cont, prev_sf) %>%
  group_by(OBJECTID) %>%
  summarise(meanprev = mean(PfPR2.10, na.rm = TRUE)) %>%
  pull(meanprev)

cont$npts <- st_intersects(cont, prev_sf) %>%
  lengths()

############################################################
# Top row of plot ----
############################################################

map.n <- ggplot() +
  geom_sf(data = cont, aes(fill = npts), color = NA) +
  coord_sf(datum = NA, xlim = c(-19, 53)) +
  theme_void() +
  theme(legend.position = c(0.2, 0.3)) +
  scale_fill_gradientn('Samples', colours = mako(100), trans = "log10") +
  guides(fill = guide_colourbar(ticks = FALSE)) 

map.p <- ggplot(cont) +
  geom_sf(aes(fill = meanprev), color = NA) +
  coord_sf(datum = NA, xlim = c(-19, 53)) +
  theme_void() +
  theme(legend.position = c(0.2, 0.3)) +
  scale_fill_gradientn(
    'Prevalence (%)',
    colours = mako(100),
    limits = c(0, 85)
  ) +
  guides(fill = guide_colourbar(ticks = FALSE))

top <- map.n + map.p

############################################################
# Bottom row of plot ----
############################################################

o <- st_join(prev_sf, gbod)

df <- tibble(prev_sf)
df$region <- o$SmllRgn

df %<>% unite("monthyr", MM:YY, sep = ' 1 ', remove = FALSE)
df %<>% mutate(monthyr = mdy(monthyr))

df2 <- df
df2$region <- 'Continent-wide'
df <- bind_rows(df2, df)

df <- df %>%
  filter(!is.na(region)) %>%
  mutate(
    region = recode(
      region,
      !!!c(
        'Sub-Saharan Africa (Central)' = 'Central Africa',
        'Sub-Saharan Africa (East)' = 'East Africa',
        'Sub-Saharan Africa (Southern)' = 'Southern Africa',
        'Sub-Saharan Africa (West)' = 'West Africa'
      )
    )
  ) %>%
  mutate(
    region = factor(
      region,
      levels = c(
        'Continent-wide',
        'Central Africa',
        'East Africa',
        'Southern Africa',
        'West Africa'
      )
    )
  ) 

ts <- df %>%
  ggplot(aes(x = monthyr, y = PfPR2.10)) +
  theme_bw() +
  geom_rect(
    aes(
      xmin = ymd('1955-01-01'),
      xmax = ymd('1969-01-06'),
      ymin = 100,
      ymax = 0
    ),
    fill = 'pink',
    alpha = 0.05
  ) +
  geom_rect(
    aes(
      xmin = ymd('2000-01-01'),
      xmax = ymd('2015-01-01'),
      ymin = 100,
      ymax = 0
    ),
    fill = 'pink',
    alpha = 0.05
  ) +
  geom_point(alpha = 0.03, col = "#214d65", shape = 16, stroke = 0) +
  geom_smooth(method = 'gam', col = "#287DAB") +
  facet_wrap(~region, nrow = 5) +
  xlab(NULL) +
  ylab(expression(paste(
    italic("falciparum"),
    " malaria prevalence, ages 2-10 (%)"
  ))) 

ts

############################################################
# Save plot ----
############################################################

p1 <- ((map.n / map.p) | ts) +
  plot_layout(widths = c(1.5, 1)) +
  plot_annotation(tag_levels = 'A')


ggsave(
  filename = "Figure1.pdf",
  plot = p1,
  path = here::here("Figures"),
  width = 9,
  height = 10,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure1.jpg",
  plot = p1,
  path = here::here("Figures"),
  width = 9,
  height = 10,
  units = "in"
)
