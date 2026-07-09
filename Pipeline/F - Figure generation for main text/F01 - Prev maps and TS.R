################################################################################
# This script produces Figure 1 of the paper.
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

# log_msg <- create_logger(file.path(logs_dir, "F01_prev_map_ts.log"))

log_msg <- create_logger()

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
  geom_sf(data = cont, aes(fill = npts), color = "gray30", linewidth = 0.05) +
  coord_sf(datum = NA, xlim = c(-18, 51.5), ylim = c(-35, 37), expand = FALSE) +
  scale_fill_gradientn(
    'Samples',
    colours = viridisLite::mako(100),
    trans = "log10",
    na.value = "white"
  ) +
  guides(fill = guide_colourbar(ticks = FALSE))+
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.3),
    legend.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0),
    plot.tag.location = "panel",
    plot.tag.position = c(0, 1.03)
  ) 

log_msg("Plot the mean prevalence map")

map_mean_prev_plot <- ggplot(cont) +
  geom_sf(aes(fill = meanprev), color = "gray30", linewidth = 0.05) +
  coord_sf(datum = NA, xlim = c(-18, 51.5), ylim = c(-35, 37), expand = FALSE) +
  scale_fill_gradientn(
    'Prevalence (%)',
    colours = viridisLite::mako(100),
    limits = c(0, 85),
    na.value = "white"
  ) +
  guides(fill = guide_colourbar(ticks = FALSE))+
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.3),
    legend.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0),
    plot.tag.location = "panel",
    plot.tag.position = c(0, 1.03),
  ) 

# top <- map_n_samples_plot / map_mean_prev_plot

################################################################################
# Right side of plot ----
################################################################################

log_msg("Plot the regional time series of prevalence")


samples <- sf::st_join(prev_sf, gbod)

df <- tibble::tibble(prev_sf)
df$region <- samples$SmllRgn

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

region_n <- df |>
  dplyr::filter(!is.na(region)) |>
  dplyr::count(region) |>
  dplyr::mutate(
    label = sprintf(
      'italic(n) * " = %s"',
      format(n, big.mark = ",", trim = TRUE)
    )
  )

ts <- df |>
  ggplot(aes(x = monthyr, y = `PfPR2-10`)) +
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
  geom_text(
    data = region_n,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.15,
    vjust = 1.5,
    parse = TRUE,
    inherit.aes = FALSE,
    size = 3.5
  ) +
  facet_wrap(
    ~region,
    nrow = 5
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(add = c(1, 1))) +
  xlab(NULL) +
  ylab(expression(paste(
    italic("falciparum"),
    " malaria prevalence, ages 2-10 (%)"
  ))) +
  theme_classic() +
  theme(
    plot.tag.location = "panel",
    plot.tag.position = c(-0.2, 1.015),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "white")
  )

ts

################################################################################
# Save plot ----
################################################################################

log_msg("Save the plot")

top <- map_n_samples_plot / map_mean_prev_plot + plot_layout(heights = c(1, 1))

p1 <- ((top) | ts) +
  patchwork::plot_layout(widths = c(1.5, 1)) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 23),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(3, 3, 3, 3, unit = "mm")
  )

ggplot2::ggsave(
  filename = paste0("Figure1_prev_maps_and_TS.", fig_file_type),
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