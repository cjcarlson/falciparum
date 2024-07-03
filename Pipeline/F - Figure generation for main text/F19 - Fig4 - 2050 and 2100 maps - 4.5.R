library(here)
library(sf)
library(tidyverse)
library(data.table)
library(patchwork)
library(multiscales)


iter.df <- here::here("TempFiles", "Fig4Big.feather") |> 
  arrow::read_feather() |> 
  dplyr::filter(
    scenario == "ssp245",
  )

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

iter.df |> 
  tidyr::pivot_wider(names_from = year, values_from = Pred) |> 
  dplyr::mutate(diff = (`2050` - `2015`)) |>
  dplyr::select(-c(`2100`,`2050`,`2015`, scenario)) -> 
  slices.runs

slices.runs |> 
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) |> 
  # dplyr::ungroup() |>
  dplyr::mutate(OBJECTID = factor(OBJECTID), moe = 1 - abs(runs.diff-5500)/5500) ->
  slice.map

### ADD THE MAP
sfcont <- file.path(datadir, 'Data', 'AfricaADM1.shp') |>
  sf::read_sf() |>
  dplyr::left_join(slice.map, by = join_by(OBJECTID))

colors <- scales::colour_ramp(
  colors = c(red = "#AC202F", purple = "#740280", blue = "#2265A3")
)((0:7)/7)

colors <- rev(colors)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  bivariate_scale(
    "fill",
    pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
    name = c("Change in prevalence (%)", "sign uncert."),
    limits = list(c(-8, 8), c(0, 1)),
    breaks = list(c(-8, -4, 0, 4, 8), c(0, 0.25, 0.5, 0.75, 1)),
    labels = list(waiver(), scales::percent),
    guide = "colourfan") +
  theme_void() +
  theme(
    legend.key.size = grid::unit(1, "cm"),
    plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(size = 25),
    legend.title = element_text(hjust= 0.5)
  ) + 
  ggtitle('A')  ->
  map.rcp26

legend <- cowplot::get_legend(map.rcp26)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  bivariate_scale(
    "fill",
    pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
    name = c("Change in prevalence (%)", "sign uncert."),
    limits = list(c(-4.5, 4.5), c(0, 1)),
    breaks = list(c(-4.5, -2, 0, 2, 4.5), c(0, 0.25, 0.5, 0.75, 1)),
    labels = list(waiver(), scales::percent)) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18),
    legend.key.size = grid::unit(1, "cm"),
    # legend.title.align = 0.5,
    legend.title = element_text(hjust= 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) + 
  ggtitle('B') -> 
  map.rcp45.2050

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

iter.df |> 
  tidyr::pivot_wider(names_from = year, values_from = Pred) |> 
  dplyr::mutate(diff = (`2100` - `2015`)) |>
  dplyr::select(-c(`2100`,`2050`,`2015`, scenario)) -> 
  slices.runs

slices.runs |> 
  dplyr::ungroup() |> 
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0)) |> 
  dplyr::mutate(
    OBJECTID = factor(OBJECTID), 
    moe = 1 - abs(runs.diff-5500)/5500) ->
  slice.map2

### ADD THE MAP
sfcont <- file.path(datadir, 'Data', 'AfricaADM1.shp') |>
  sf::read_sf() |>
  dplyr::left_join(slice.map2, by = join_by(OBJECTID))

colors <- scales::colour_ramp(
  colors = c(red = "#AC202F", purple = "#740280", blue = "#2265A3")
)((0:7)/7)

colors <- rev(colors)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, 
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) + 
  bivariate_scale(
    "fill",
    pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
    name = c("Change in prevalence (%)", "sign uncert."),
    # limits = list(c(-8, 8), c(0, 1)),
    # breaks = list(c(-8, -4, 0, 4, 8), c(0, 0.25, 0.5, 0.75, 1)),
    labels = list(waiver(), scales::percent)) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18),
    legend.key.size = grid::unit(1, "cm"),
    # legend.title.align = 0.5,
    legend.title = element_text(hjust= 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) + 
  ggtitle('E') ->
  map.rcp45.2100
