################################################################################
# This script produces Figure 1 of the paper
################################################################################
# Set up ----
################################################################################

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
  patchwork,
  viridisLite
)

sf::sf_use_s2(FALSE)

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

################################################################################
# Set up logging ----
################################################################################

log_file_path <- file.path(logs_dir, "F01_prev_map_ts.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script `F01 - Prev maps and TS.R`")

################################################################################
# Load data ----
################################################################################

log_msg("Load the spatial data")

cont <- ADM1_fp |>
  sf::st_read() |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

gbod <- sf::read_sf(gbd_fp)

log_msg("Load the prevalence data")

prev_sf <- prev_DB_fp |>
  readr::read_csv() |>
  sf::st_as_sf(coords = c("Long", "Lat"), crs = st_crs(cont))

log_msg("Join the prevalence and spatial data")

cont$meanprev <- sf::st_join(cont, prev_sf) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarise(meanprev = mean(`PfPR2-10`, na.rm = TRUE)) |>
  dplyr::pull(meanprev)

cont$npts <- sf::st_intersects(cont, prev_sf) |>
  lengths()

################################################################################
# Left side of plot ----
################################################################################

log_msg("Plot the number of samples map")

map_n_samples_plot <- ggplot() +
  geom_sf(data = cont, aes(fill = npts), color = NA) +
  coord_sf(datum = NA, xlim = c(-19, 53)) +
  theme_void() +
  theme(legend.position = c(0.2, 0.3)) +
  scale_fill_gradientn(
    'Samples',
    colours = viridisLite::mako(100),
    trans = "log10"
  ) +
  guides(fill = guide_colourbar(ticks = FALSE))

log_msg("Plot the mean prevalence map")

map_mean_prev_plot <- ggplot(cont) +
  geom_sf(aes(fill = meanprev), color = NA) +
  coord_sf(datum = NA, xlim = c(-19, 53)) +
  theme_void() +
  theme(legend.position = c(0.2, 0.3)) +
  scale_fill_gradientn(
    'Prevalence (%)',
    colours = viridisLite::mako(100),
    limits = c(0, 85)
  ) +
  guides(fill = guide_colourbar(ticks = FALSE))

top <- map_n_samples_plot + map_mean_prev_plot

################################################################################
# Right side of plot ----
################################################################################

log_msg("Plot the regional time series of prevalence")

o <- sf::st_join(prev_sf, gbod)

df <- tibble::tibble(prev_sf)
df$region <- o$SmllRgn

df <- df |>
  tidyr::unite("monthyr", MM:YY, sep = " 1 ", remove = FALSE) |>
  dplyr::mutate(monthyr = lubridate::mdy(monthyr))

df2 <- df
df2$region <- 'Continent-wide'
df <- bind_rows(df2, df)

df <- df |>
  dplyr::filter(!is.na(region)) |>
  dplyr::mutate(region = dplyr::recode(region, !!!region_names[2:5])) |>
  dplyr::mutate(
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

ts <- df |>
  ggplot(aes(x = monthyr, y = `PfPR2-10`)) +
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

################################################################################
# Save plot ----
################################################################################

log_msg("Save the plot")

p1 <- ((map_n_samples_plot / map_mean_prev_plot) | ts) +
  patchwork::plot_layout(widths = c(1.5, 1)) +
  patchwork::plot_annotation(tag_levels = 'A')

ggplot2::ggsave(
  filename = "Figure1_prev_maps_and_TS.jpg",
  plot = p1,
  path = here::here("Results", "Figures"),
  width = 9,
  height = 10,
  units = "in"
)

log_msg("Script `F01 - Prev maps and TS.R` completed successfully")

################################################################################
# End of file ----
################################################################################
