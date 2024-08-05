library(here)
library(sf)
library(tidyverse)
library(data.table)
library(patchwork)
library(multiscales)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

iter.df <- here::here("TempFiles", "Fig4Big.feather") |> 
  arrow::read_feather() |> 
  dplyr::filter(scenario == "ssp245")

iter.df |> 
  tidyr::pivot_wider(names_from = year, values_from = Pred) |> 
  dplyr::mutate(diff = (`2100` - `2015`)) |>
  dplyr::select(-c(`2100`,`2050`,`2015`, scenario)) -> 
  slices.runs

slices.runs |> 
  dplyr::ungroup() |> 
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(
    mean.diff = mean(diff), runs.diff = sum(diff > 0), 
    lower.diff.90 = quantile(diff, 0.05, na.rm = TRUE), 
    upper.diff.90 = quantile(diff, 0.95, na.rm = TRUE),
    lower.diff.95 = quantile(diff, 0.025, na.rm = TRUE),
    upper.diff.95 = quantile(diff, 0.975, na.rm = TRUE)
    ) |> 
  dplyr::mutate(
    OBJECTID = factor(OBJECTID), 
    moe = 1 - abs(runs.diff-5500)/5500) ->
  slice.map2

sfcont <- file.path(datadir, 'Data', 'AfricaADM1.shp') |>
  sf::read_sf() |>
  dplyr::left_join(slice.map2, by = join_by(OBJECTID))

colors <- scales::colour_ramp(
  colors = c(red = "#AC202F", purple = "#740280", blue = "#2265A3")
)((0:7)/7)

colors <- rev(colors)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.05) +
  scale_x_continuous(limits = c(-17,52), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-36, 38), expand = c(0, 0)) +
  coord_sf(datum = NA) +
  bivariate_scale(
    "fill",
    pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
    name = c("Prevalence (%)", "sign uncertainty"),
    limits = list(c(-3, 3), c(0, 1)),
    breaks = list(c(-3, -1.5, 0, 1.5, 3), c(0, 0.25, 0.5, 0.75, 1)),
    labels = list(waiver(), scales::percent),
    guide = "colourfan") +
  labs(
    title = "Future impact of anthropogenic climate change on prevalence",
    subtitle = "(2096-2100; SSP2-RPC4.5)") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "inside",
    legend.position.inside = c(0.18,0.3),
    legend.key.size = grid::unit(0.8, "cm"),
    legend.title = element_text(hjust= 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) ->
  map.rcp45.2100
