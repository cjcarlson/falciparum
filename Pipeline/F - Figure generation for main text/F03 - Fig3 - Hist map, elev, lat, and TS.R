########################################################################
# This script plots the 
########################################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# install.packages("colorspace", repos = "http://R-Forge.R-project.org")
# devtools::install_github("clauswilke/multiscales")

# packages
pacman::p_load(
  sf,
  here,
  reshape,
  tidyverse,
  lubridate,
  patchwork,
  multiscales
)

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)


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
sfcont <- file.path(data_dir, 'Data', 'AfricaADM1.shp') |>
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


############################################################
# XXXXXXXXX ----
############################################################

iter.df <- here::here("TempFiles", "Fig3Big.feather") |> 
  arrow::read_feather() |>
  dplyr::mutate(model = stringr::str_replace_all(model,'BCC-CSM2-MR','BCC-CSM2')) 

iter.df |> 
  filter(year == 2014) ->
  slices

slices |> 
  pivot_wider(names_from = scenario, values_from = Pred) |> 
  mutate(diff = (historical - `hist-nat`)) |>
  select(-c(historical, `hist-nat`, year)) -> 
  slices.runs

slices.runs |> 
  group_by(OBJECTID) |>
  summarize(
    mean.diff = mean(diff, na.rm = TRUE), 
    lower.diff.90 = quantile(diff, 0.05, na.rm = TRUE), 
    upper.diff.90 = quantile(diff, 0.95, na.rm = TRUE),
    lower.diff.95 = quantile(diff, 0.025, na.rm = TRUE), 
    upper.diff.95 = quantile(diff, 0.975, na.rm = TRUE)
  ) ->
  slice.map1


# Get the stuff 
elev <- file.path(data_dir, "Data", "elevation", "elevation_extracted_all_ADM1.csv") |> 
  readr::read_csv(show_col_types = FALSE) |> 
  dplyr::select(OBJECTID, elevmn)

cont <- file.path(data_dir, 'Data', 'AfricaADM1.shp') |> 
  sf::read_sf()

latlon <- cont |> 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
latlon |>
  as.data.frame() |>
  select(OBJECTID, lat) |>
  mutate(OBJECTID = as.numeric(OBJECTID)) -> lat

temp <- file.path(data_dir, "Data", "CRU-Reextraction-Aug2022.csv") |> 
  readr::read_csv(show_col_types = FALSE) 
temp |> 
  filter(year %in% c(1901:1930)) |>
  group_by(OBJECTID) |> 
  summarize(t = mean(temp, na.rm = TRUE)) -> 
  tmean


############################################################
# XXXXXXXXX ----
############################################################


slice.map1 |>
  left_join(elev) |>
  left_join(lat) |>
  left_join(tmean) -> 
  df

# Generate a nice little significance color scheme
df |>
  mutate(
    sign = as.numeric(lower.diff.90 > 0) + -1*as.numeric(upper.diff.90 < 0),
    sign = factor(sign)
  ) ->
  df

# After creating your df dataframe, split it into two based on significance
df_non_sig <- df %>% filter(sign == 0)
df_sig <- df %>% filter(sign != 0)

#### This version orders the colors such that the grey lines are plotted first
#### then the red and blue lines are plotted on top.

g1 <- ggplot() +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = elevmn, xmin = lower.diff.95, xmax = upper.diff.95), 
    color = "grey80", alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = elevmn, xmin = lower.diff.90, xmax = upper.diff.90), 
    color = "grey80", alpha = 0.5, size = 0.7) +
  geom_point(
    data = df_non_sig, aes(x = mean.diff, y = elevmn), 
    color = "grey80") +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = elevmn, xmin = lower.diff.95, xmax = upper.diff.95, color = sign), 
    alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = elevmn, xmin = lower.diff.90, xmax = upper.diff.90, color = sign), 
    alpha = 0.5, size = 0.7) +
  geom_point(data = df_sig, aes(x = mean.diff, y = elevmn, color = sign)) +
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Elevation (m)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,10,0)) +
  scale_color_manual(values = c("-1" = "#2265A3", "1" = "#AC202F"))



g2 <- ggplot() +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = lat, xmin = lower.diff.95, xmax = upper.diff.95), 
    color = "grey80", alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = lat,  xmin = lower.diff.90, xmax = upper.diff.90), 
    color = "grey80", alpha = 0.5, size = 0.7) +
  geom_point(
    data = df_non_sig, aes(x = mean.diff, y = lat), 
    color = "grey80") +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = lat, xmin = lower.diff.95, xmax = upper.diff.95, color = sign), 
    alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = lat, xmin = lower.diff.90, xmax = upper.diff.90, color = sign), 
    alpha = 0.5, size = 0.7) +
  geom_point(data = df_sig, aes(x = mean.diff, y = lat, color = sign)) +
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Latitude") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,0,0)) +
  scale_color_manual(values = c("-1" = "#2265A3", "1" = "#AC202F"))


g3 <- ggplot() +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = t, xmin = lower.diff.95, xmax = upper.diff.95), 
    color = "grey80", alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = t,  xmin = lower.diff.90, xmax = upper.diff.90), 
    color = "grey80", alpha = 0.5, size = 0.7) +
  geom_point(
    data = df_non_sig, aes(x = mean.diff, y = t), 
    color = "grey80") +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = t,  xmin = lower.diff.95, xmax = upper.diff.95, color = sign), 
    alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = t, xmin = lower.diff.90, xmax = upper.diff.90, color = sign), 
    alpha = 0.5, size = 0.7) +
  geom_point(data = df_sig, aes(x = mean.diff, y = t, color = sign)) +
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Mean temperature (1901-1930)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,0,0)) +
  scale_color_manual(values = c("-1" = "#2265A3", "1" = "#AC202F"))


############################################################
# XXXXXXXXX ----
############################################################


data.to.graph <- here::here("TempFiles", "Fig3Regionals.csv") |> 
  readr::read_csv(show_col_types = FALSE) 

data.to.graph |> 
  mutate(scenario = factor(scenario, levels = c('hist-nat', 'historical'))) |>
  mutate(
    region = recode(
      region, !!!c(
        'Sub-Saharan Africa (Central)' = 'Central Africa',
        'Sub-Saharan Africa (East)' = 'East Africa',
        'Sub-Saharan Africa (Southern)' = 'Southern Africa',
        'Sub-Saharan Africa (West)' = 'West Africa'))) |>
  ### Start plotting in 1902 because it's the first full year with lags incorporated right.
  filter(year > 1901) |> 
  ############ radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT LOOKING CLOSELY
  ############ this is a way of hard coding the CI's to still plot thanks to how ggplot does CI's
  ############ this is for plotting purposes ONLY and text stats give full CI's
  mutate(lower = pmax(lower, -0.6), upper = pmin(upper, 1.0)) |>
    ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) + 
  geom_line(aes(y=median), lwd = 1.25) + 
  # geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) + 
  geom_ribbon(
    aes(ymin = lower, ymax = upper, colour = scenario),
    fill = NA,
    linewidth = 0.1,
    show.legend = FALSE,
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = scenario),
    color = NA,
    alpha = 0.1
  ) +
  theme_classic() + 
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) + 
  facet_wrap(region ~ ., nrow = 1) + 
  xlab(NULL) + 
  ylab("Prevalence (%)") + 
  ylim(-0.6, 1) +
  scale_color_manual(
    values = c("grey50", "#287DAB"), 
    labels = c('Historical counterfactual', 'Historical climate'),
    name = '') + 
  scale_fill_manual(
    values = c("grey50", "#287DAB"), 
    labels = c('Historical counterfactual', 'Historical climate'),
    name = '') + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(size = 20)) -> 
  bottom


############################################################
# XXXXXXXXX ----
############################################################

map.diff +
  g3 +
  g1 +
  bottom +
  plot_layout(design = fig_3_4_layout) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 23))

ggsave(
  filename = "Figure3.pdf",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 11.63,
  height = 10.07,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure3.jpg",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 11.63,
  height = 10.07,
  units = "in",
)

