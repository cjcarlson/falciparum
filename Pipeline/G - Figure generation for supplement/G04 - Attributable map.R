################################################################################
# This script makes all four panels of Extended Data Figure 7.
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
  patchwork,
  tidyverse,
  colorspace,
  data.table
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

################################################################################
# Hist delta map data ----
################################################################################

boots_2010_2014 <- file.path(
  hist_sum_dir,
  "historical_vcov_pred_sum_scen_mod_yr_obj.feather"
) |>
  arrow::read_feather() |>
  dplyr::filter(run != "main", year == 2014) |>
  dplyr::select(scenario, model, year, OBJECTID, Pred, run) |>
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
  dplyr::mutate(diff = (historical - `hist-nat`), ) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(
    mean.diff = mean(diff),
    runs.diff = sum(diff > 0),
    lower.diff = quantile(diff, 0.05, na.rm = TRUE),
    upper.diff = quantile(diff, 0.95, na.rm = TRUE),
  ) |>
  dplyr::mutate(
    OBJECTID = factor(OBJECTID),
    moe = 1 - abs(runs.diff - 5000) / 5000
  )

cont <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::left_join(boots_2010_2014, by = join_by(OBJECTID))

g1 <- ggplot(cont) +
  geom_sf(aes(fill = mean.diff), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, xlim = c(-18, 51.5), ylim = c(-35, 37), expand = FALSE) +
  colorspace::scale_fill_continuous_divergingx(
    palette = "Geyser",
    na.value = "white"
  ) +
  labs(fill = "Change (%)") +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.25),
    legend.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  )

cont <- cont |>
  dplyr::mutate(
    sign = as.numeric(lower.diff > 0) + -1 * as.numeric(upper.diff < 0)
  ) |>
  dplyr::mutate(sign = factor(sign))

g2 <- ggplot(cont) +
  geom_sf(aes(fill = sign), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, xlim = c(-18, 51.5), ylim = c(-35, 37), expand = FALSE) +
  scale_fill_manual(
    values = c("#00AFBB", "grey80", "#fa5340"),
    labels = c('Decline', 'Insignificant', 'Increase'),
    na.value = "white",
    na.translate = F
  ) +
  labs(fill = "Significance") +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.25),
    legend.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  )

cont <- cont |>
  mutate(sign = as.numeric(lower.diff > 0) + as.numeric(upper.diff < 0)) |>
  mutate(sign = replace_na(sign, 0)) |>
  arrange(-sign) |>
  mutate(sign = as.factor(sign))

top <- cont |>
  dplyr::select(sign) |>
  dplyr::filter(sign == 1) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_make_valid() |>
  sf::st_union()

supp_2 <- ggplot(cont) +
  geom_sf(aes(fill = mean.diff), color = 'grey30', size = 0.05) +
  colorspace::scale_fill_continuous_divergingx(
    palette = "Geyser",
    na.value = "white"
  ) +
  labs(fill = "Prevalence (%)") +
  geom_sf(data = top, color = 'black', size = 0.25, fill = NA) +
  coord_sf(datum = NA, xlim = c(-18, 51.5), ylim = c(-35, 37), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.25, 0.35),
    legend.title = element_text(size = 14, hjust = 0.5),
    legend.text = element_text(size = 12),
    plot.margin = margin(3, 3, 3, 3, unit = "mm"),
    legend.key.width = unit(8, "mm"),
    legend.key.height = unit(8, "mm")
  )

ggplot2::ggsave(
  filename = paste0("ED_Figure_attributable_map.", fig_file_type),
  plot = supp_2,
  path = here::here("Results", "Figures"),
  width = 9.53,
  height = 10.07,
  units = "in"
)

################################################################################
# End of file ----
################################################################################
