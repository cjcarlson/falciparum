################################################################################
# This script plots Figure 3, Historical changes in malaria prevalence
# attributable to anthropogenic climate change from 1901 to 2014.
# apptainer exec --cleanenv --contain \
#   --bind /global/scratch/projects/co_carleton:/global/scratch/projects/co_carleton \
#   --bind /global/home/users/cmolitor/falciparum:/global/home/users/cmolitor/falciparum \
#   --pwd /global/home/users/cmolitor/falciparum \
#   /global/scratch/projects/co_carleton/carleton_colab/software/apptainers/r-malaria-cru_4.2.3.sif \
#   Rscript "Pipeline/F - Figure generation for main text/F03 - Hist map, temp, elev, and TS.R"
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
  # reshape,
  tidyverse,
  lubridate,
  patchwork
)

pacman::p_load_gh("clauswilke/multiscales")

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Set up logging ----
################################################################################

log_file_path <- file.path(logs_dir, "F03_hist_map_temp_el_ts.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script `F03 - Hist map, temp, elev, and TS.R`")

################################################################################
# Hist delta map data ----
################################################################################

log_msg("Loading historical_vcov_pred_sum_scen_mod_yr_obj.feather")

hist_scen_mod_yr_adm1_pred <- file.path(
  hist_sum_dir,
  "historical_vcov_pred_sum_scen_mod_yr_obj.feather"
) |>
  arrow::read_feather() |>
  dplyr::mutate(
    model = stringr::str_replace_all(model, 'BCC-CSM2-MR', 'BCC-CSM2')
  ) |>
  dplyr::select(scenario, model, year, OBJECTID, Pred, run) |>
  dplyr::filter(year == 2014)

log_msg("Calculating ADM1 mean difference")

# main_2010_2014 <- hist_scen_mod_yr_adm1_pred |>
#   dplyr::filter(run == "main",) |>
#   tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
#   dplyr::mutate(diff = (historical - `hist-nat`)) |>
#   dplyr::group_by(OBJECTID) |>
#   dplyr::summarize(mean.diff = mean(diff))

log_msg("Calculating ADM1 confidence interval")

boots_2010_2014 <- hist_scen_mod_yr_adm1_pred |>
  dplyr::filter(run != "main") |>
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
  dplyr::mutate(diff = (historical - `hist-nat`), ) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(
    mean.diff = mean(diff),
    runs.diff = sum(diff > 0),
    lower.diff.90 = quantile(diff, 0.05, na.rm = TRUE),
    upper.diff.90 = quantile(diff, 0.95, na.rm = TRUE),
    lower.diff.95 = quantile(diff, 0.025, na.rm = TRUE),
    upper.diff.95 = quantile(diff, 0.975, na.rm = TRUE)
  ) |>
  # dplyr::left_join(main_2010_2014) |>
  dplyr::mutate(
    OBJECTID = factor(OBJECTID),
    moe = 1 - abs(runs.diff - 5000) / 5000
  )

log_msg("Add summarized data to ADM1 map data")

sfcont <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::left_join(boots_2010_2014, by = join_by(OBJECTID))

################################################################################
# End of hist (2010-2014) delta plot ----
################################################################################

log_msg("Plot the historical map")

colors <- scales::colour_ramp(
  colors = c(
    red = "#AC202F",
    purple = "#740280",
    blue = "#2265A3"
  )
)((0:7) / 7) |>
  rev()

map.diff <- ggplot(sfcont) +
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.05) +
  scale_x_continuous(limits = c(-17, 52), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-36, 38), expand = c(0, 0)) +
  coord_sf(datum = NA) +
  multiscales::bivariate_scale(
    "fill",
    multiscales::pal_vsup(
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
    legend.position = c(0.18, 0.3),
    legend.key.size = grid::unit(0.8, "cm"),
    plot.margin = margin(0, 0, 0, 0)
  )

################################################################################
# Elevation and temperature ----
################################################################################

log_msg("Load elevation and CRU temperature data")

elev <- elevation_fp |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::select(OBJECTID, elevmn) |>
  dplyr::mutate(OBJECTID = factor(OBJECTID))

tmean <- intermediate_CRU_adm1_fp |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::filter(year %in% c(1901:1930)) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(t = mean(temp, na.rm = TRUE)) |>
  dplyr::mutate(OBJECTID = factor(OBJECTID))

################################################################################
# Significance ----
################################################################################

# Generate a nice little significance color scheme
df <- boots_2010_2014 |>
  dplyr::left_join(elev) |>
  dplyr::left_join(tmean) |>
  dplyr::mutate(
    sign = as.numeric(lower.diff.90 > 0) + -1 * as.numeric(upper.diff.90 < 0),
    sign = factor(sign)
  )

# After creating your df dataframe, split it into two based on significance
df_non_sig <- df %>% filter(sign == 0)
df_sig <- df %>% filter(sign != 0)

################################################################################
# Temperature plot ----
# This version orders the colors such that the grey lines are plotted first
# then the red and blue lines are plotted on top.
################################################################################

log_msg("Plot the average temperature")

temp_plot <- ggplot() +
  geom_errorbar(
    data = df_non_sig,
    aes(x = mean.diff, y = t, xmin = lower.diff.95, xmax = upper.diff.95),
    color = "grey80",
    alpha = 0.3,
    linewidth = 0.5
  ) +
  geom_errorbar(
    data = df_non_sig,
    aes(x = mean.diff, y = t, xmin = lower.diff.90, xmax = upper.diff.90),
    color = "grey80",
    alpha = 0.5,
    linewidth = 0.7
  ) +
  geom_point(
    data = df_non_sig,
    aes(x = mean.diff, y = t),
    color = "grey80"
  ) +
  geom_errorbar(
    data = df_sig,
    aes(
      x = mean.diff,
      y = t,
      xmin = lower.diff.95,
      xmax = upper.diff.95,
      color = sign
    ),
    alpha = 0.3,
    linewidth = 0.5
  ) +
  geom_errorbar(
    data = df_sig,
    aes(
      x = mean.diff,
      y = t,
      xmin = lower.diff.90,
      xmax = upper.diff.90,
      color = sign
    ),
    alpha = 0.5,
    linewidth = 0.7
  ) +
  geom_point(data = df_sig, aes(x = mean.diff, y = t, color = sign)) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_classic() +
  xlab("Prevalence (%)") +
  ylab("Mean temperature (1901-1930)") +
  theme(
    axis.title.x = element_text(margin = margin(t = 20, b = 10)),
    axis.title.y = element_text(margin = margin(r = 20, l = 10)),
    legend.position = 'n',
    plot.margin = margin(0, 0, 0, 0)
  ) +
  scale_color_manual(values = c("-1" = "#2265A3", "1" = "#AC202F"))

################################################################################
# Elevation plot ----
# This version orders the colors such that the grey lines are plotted first
# then the red and blue lines are plotted on top.
################################################################################

log_msg("Plot the elevation")

elev_plot <- ggplot() +
  geom_errorbar(
    data = df_non_sig,
    aes(x = mean.diff, y = elevmn, xmin = lower.diff.95, xmax = upper.diff.95),
    color = "grey80",
    alpha = 0.3,
    linewidth = 0.5
  ) +
  geom_errorbar(
    data = df_non_sig,
    aes(x = mean.diff, y = elevmn, xmin = lower.diff.90, xmax = upper.diff.90),
    color = "grey80",
    alpha = 0.5,
    linewidth = 0.7
  ) +
  geom_point(
    data = df_non_sig,
    aes(x = mean.diff, y = elevmn),
    color = "grey80"
  ) +
  geom_errorbar(
    data = df_sig,
    aes(
      x = mean.diff,
      y = elevmn,
      xmin = lower.diff.95,
      xmax = upper.diff.95,
      color = sign
    ),
    alpha = 0.3,
    linewidth = 0.5
  ) +
  geom_errorbar(
    data = df_sig,
    aes(
      x = mean.diff,
      y = elevmn,
      xmin = lower.diff.90,
      xmax = upper.diff.90,
      color = sign
    ),
    alpha = 0.5,
    linewidth = 0.7
  ) +
  geom_point(data = df_sig, aes(x = mean.diff, y = elevmn, color = sign)) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_classic() +
  xlab("Prevalence (%)") +
  ylab("Elevation (m)") +
  theme(
    axis.title.x = element_text(margin = margin(t = 20, b = 10)),
    axis.title.y = element_text(margin = margin(r = 20, l = 10)),
    legend.position = 'n',
    plot.margin = margin(0, 0, 10, 0)
  ) +
  scale_color_manual(values = c("-1" = "#2265A3", "1" = "#AC202F"))

################################################################################
# Regional time series data ----
################################################################################

log_msg("load the regional time series data")

historical_pred <- file.path(
  hist_sum_dir,
  "historical_vcov_pred_sum_scen_mod_yr_reg.feather"
) |>
  arrow::read_feather() |>
  data.table::as.data.table()

# hist_main <- historical_pred[run == "main"] |>
#   baseline_adjust_summarize(
#     variable = "Pred",
#     baseline_group = c("model", "scenario", "region", "run"),
#     adjusted_group = c("scenario", "region", "year"),
#     baseline_years = 1900:1930,
#     confidence_level = 0.90
#   ) |>
#   dplyr::filter(year > 1901) |>
#   dplyr::select(-c(upper, lower))

hist_boot <- historical_pred[run != "main"] |>
  baseline_adjust_summarize(
    variable = "Pred",
    baseline_group = c("model", "scenario", "region", "run"),
    adjusted_group = c("scenario", "region", "year"),
    baseline_years = 1900:1930,
    confidence_level = 0.90
  ) |>
  dplyr::filter(year > 1901) |>
  # dplyr::select(-c(median, mean)) |>
  # dplyr::left_join(hist_main) |>
  dplyr::mutate(
    scenario = factor(scenario, levels = names(historical_scenario_names))
  ) |>
  # radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT
  # LOOKING CLOSELY this is a way of hard coding the CI's to still plot thanks
  # to how ggplot does CI's this is for plotting purposes ONLY and text stats
  # give full CI's
  dplyr::mutate(lower = pmax(lower, -0.6), upper = pmin(upper, 1.0))

################################################################################
# Regional time series plot ----
################################################################################

log_msg("Plot the regional time series data")

regional_ts_plot <- ggplot(
  data = hist_boot,
  aes(x = year, group = scenario, color = scenario, fill = scenario)
) +
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
  geom_line(aes(y = mean), lwd = 1.25) +
  theme_classic() +
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) +
  facet_wrap(region ~ ., nrow = 1) +
  xlab(NULL) +
  ylab("Prevalence (%)") +
  ylim(-0.6, 1) +
  scale_color_manual(
    values = c("grey50", "#287DAB"),
    labels = c('Historical counterfactual', 'Historical climate'),
    name = ''
  ) +
  scale_fill_manual(
    values = c("grey50", "#287DAB"),
    labels = c('Historical counterfactual', 'Historical climate'),
    name = ''
  ) +
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(size = 20))

################################################################################
# Compile and save plot ----
################################################################################

log_msg("Compile plots and save")

fig3 <- (map.diff + temp_plot + elev_plot + regional_ts_plot) +
  patchwork::plot_layout(design = fig_3_4_layout) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 23))

ggsave(
  filename = "Figure3_hist_map_tmp_el_and_TS.jpg",
  plot = last_plot(),
  path = here::here("Results", "Figures"),
  width = 11.63,
  height = 10.07,
  units = "in",
)

log_msg("Script `F03 - Hist map, temp, elev, and TS.R` completed successfully")

################################################################################
# End of file ----
################################################################################
