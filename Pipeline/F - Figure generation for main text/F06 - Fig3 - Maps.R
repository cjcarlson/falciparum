# install.packages("colorspace", repos = "http://R-Forge.R-project.org")
# devtools::install_github("clauswilke/multiscales")

library(sf)
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)
library(multiscales)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

iter.df <- here::here("TempFiles", "Fig3Big.feather") |>
  arrow::read_feather() |>
  dplyr::mutate(
    model = stringr::str_replace_all(model, 'BCC-CSM2-MR', 'BCC-CSM2')
  )

iter.df |>
  dplyr::filter(year == 2014) -> slices

slices |>
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
  dplyr::mutate(diff = (historical - `hist-nat`)) |>
  dplyr::select(-c(historical, `hist-nat`, year)) -> slices.runs

slices.runs |>
  dplyr::ungroup() |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) |>
  dplyr::mutate(OBJECTID = factor(OBJECTID)) -> slice.map1

### ADD THE MAP
sfcont <- file.path(datadir, 'Data', 'AfricaADM1.shp') |>
  sf::read_sf() |>
  dplyr::left_join(slice.map1, by = join_by(OBJECTID)) |>
  dplyr::mutate(moe = 1 - abs(runs.diff - 5000) / 5000)

colors <- scales::colour_ramp(
  colors = c(red = "#AC202F", purple = "#740280", blue = "#2265A3")
)((0:7) / 7)

colors <- rev(colors)

ggplot(sfcont) +
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.05) +
  scale_x_continuous(limits = c(-17, 52), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-36, 38), expand = c(0, 0)) +
  coord_sf(datum = NA) +
  bivariate_scale(
    "fill",
    pal_vsup(
      values = colors,
      max_desat = 0.8,
      pow_desat = 0.2,
      max_light = 0.7,
      pow_light = 1
    ),
    name = c("Prevalence (%)", "sign uncertainty"),
    limits = list(c(-2.1, 2.1), c(0, 1)),
    breaks = list(c(-2, -1, 0, 1, 2), c(0, 0.25, 0.5, 0.75, 1)),
    labels = list(waiver(), scales::percent),
    guide = "colourfan"
  ) +
  labs(
    title = "Historical impact of anthropogenic climate change on prevalence",
    subtitle = "(2010-2014)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(r = 10)),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.3),
    legend.key.size = grid::unit(0.8, "cm"),
    # legend.title.align = 0.5,
    plot.margin = margin(0, 0, 0, 0)
  ) -> map.diff

