################################################################################
# This script plots the main prevalence-temperature dose-response function
# as well as its uncertainty over 1,000 bootstrap samples
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  lfe,
  here,
  reshape,
  tidyverse,
  lubridate,
  cowplot,
  zoo,
  patchwork,
  stringr
)

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Load data ----
################################################################################

print("Loading clean data")

complete <- analysis_ready_CRU_adm1_fp |>
  readr::read_rds()

################################################################################
# Model coefficients ----
################################################################################

all_mods <- boot_mod_full_fn |>
  readr::read_csv(show_col_types = FALSE)

main <- all_mods |>
  dplyr::filter(model == "main")

bootstraps <- all_mods |>
  dplyr::filter(model != "main")

################################################################################
# Spaghetti plot of estimated T response functions ----
################################################################################

conf_level <- 0.90

Tref = 24
Tmin = 10
Tmax = 40
int = 0.1
plotXtemp = cbind(seq(Tmin, Tmax, by = int), seq(Tmin, Tmax, by = int)^2)
xValsT = genRecenteredXVals_polynomial(plotXtemp, Tref, 2)

# point estimate
b <- as.matrix(c(main$temp, main$temp2))
response <- as.matrix(xValsT) %*% b

plotData <- data.frame(
  x = xValsT[, 1] + Tref,
  model = "main",
  response = as.numeric(response),
  n = length(complete$OBJECTID)
)

# collect bootstrap results as a list, then row-bind once
boot_list <- vector("list", nrow(bootstraps))

for (mod in seq_len(nrow(bootstraps))) {
  sub <- bootstraps[mod, ]
  b <- as.matrix(c(sub$temp, sub$temp2))
  boot_response <- as.numeric(as.matrix(xValsT) %*% b)

  boot_list[[mod]] <- data.frame(
    x = xValsT[, 1] + Tref,
    model = sub$model,
    response = boot_response,
    n = sub$n
  )

  if (mod %% 100 == 0) {
    print(paste0("--------- DONE WITH ITERATION ", mod, " of 1000 --------"))
  }
}

plotData <- rbind(plotData, do.call(rbind, boot_list))

percentile_data <- plotData |>
  dplyr::filter(model != "main") |>
  dplyr::group_by(x) |>
  dplyr::summarize(
    lower_bound = quantile(response, 0 + ((1 - conf_level) / 2)),
    upper_bound = quantile(response, 1 - ((1 - conf_level) / 2))
  )

################################################################################
# Temperature response ----
################################################################################

median_temps <- complete |>
  dplyr::group_by(smllrgn) |>
  dplyr::summarise(median_temp = median(temp, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    smllrgn = str_remove_all(smllrgn, "Sub-Saharan Africa \\("),
    smllrgn = str_remove_all(smllrgn, "\\)")
  )

g <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgrey", alpha = .5) +
  geom_segment(
    data = median_temps,
    aes(
      x = median_temp, # start & end x at the median
      xend = median_temp,
      y = 0, # start just above the curves
      yend = min(subset(plotData, model != "main")$response, na.rm = T)
    ),
    linewidth = .5,
    linetype = "solid",
    colour = "black"
  ) +
  geom_line(
    data = subset(plotData, model != "main"),
    aes(x = x, y = response, group = model),
    color = "#C1657C",
    alpha = .1
  ) +
  geom_line(
    data = subset(plotData, model == "main"),
    mapping = aes(x = x, y = response),
    color = "black",
    linewidth = .5
  ) +
  geom_line(
    data = percentile_data,
    aes(x = x, y = lower_bound),
    color = "black",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_line(
    data = percentile_data,
    aes(x = x, y = upper_bound),
    color = "black",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_text(
    data = median_temps,
    aes(x = median_temp, y = -30, label = smllrgn),
    angle = 90, # letters run alongside the line
    vjust = -0.3, # a little above the line tip
    hjust = 0, # left‑aligned
    size = 2
  ) +
  labs(
    x = NULL,
    y = "Prevalence (%)"
  ) +
  scale_x_continuous(
    limits = c(Tmin, Tmax),
    breaks = seq(Tmin, Tmax, by = 10),
    labels = as.character(seq(Tmin, Tmax, by = 10)),
    expand = expansion(mult = c(0.0, 0.0))
  ) +
  scale_y_continuous(
    breaks = seq(0, -40, -10),
    labels = as.character(seq(0, -40, -10)),
    expand = expansion(mult = c(0.0, 0.01))
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0, 1), units = "cm")
  )

################################################################################
# Temperature histogram inset ----
################################################################################

# Create the histogram as a separate plot
h_inset <- ggplot() +
  geom_histogram(
    data = complete,
    aes(x = temp),
    fill = "#8B3A4A",
    alpha = 1,
    bins = 30,
    colour = "black"
  ) +
  theme_void() +
  scale_x_continuous(
    limits = c(Tmin, Tmax),
    breaks = seq(Tmin, Tmax, by = 10),
    labels = as.character(seq(Tmin, Tmax, by = 10)),
    expand = expansion(mult = c(0.0, 0.0))
  ) +
  scale_y_continuous(
    breaks = seq(0, -40, -10),
    labels = as.character(seq(0, -40, -10)),
    expand = expansion(mult = c(0.0, 0.01))
  )

# Convert histogram to grob
h_grob <- ggplotGrob(h_inset)

# Add histogram to main plot
g_with_hist <- g +
  annotation_custom(
    h_grob,
    xmin = Tmin,
    xmax = Tmax, # Match x-axis range
    ymin = min(subset(plotData, model != "main")$response, na.rm = T), # Position at bottom
    ymax = -35 # Height of histogram
  ) +
  labs(x = "Mean temperature (\u00B0C)") +
  theme(
    axis.title.x = element_text(vjust = -0.5),
    plot.title.position = "plot",
    axis.text.x = element_text(),
    plot.margin = unit(c(0.0, 0.0, 1, 0), units = "cm"),
  )

g_with_hist

################################################################################
# Drought and flood responses ----
################################################################################

# reformat: want a dataset of lag x var x model for flood and drought
# subset to flood and drought
mycols = c(
  colnames(all_mods)[grep("flood", colnames(all_mods))],
  colnames(all_mods)[grep("drought", colnames(all_mods))]
)

rain <- all_mods |>
  dplyr::select(dplyr::all_of(mycols), model) |>
  dplyr::mutate(
    # calculate cumulative effect
    flood.cumu = rowSums(pick(flood:flood.lag3), na.rm = TRUE),
    drought.cumu = rowSums(pick(drought:drought.lag3), na.rm = TRUE)
  ) |>
  dplyr::relocate(model) |>
  tidyr::pivot_longer(
    cols = flood:drought.cumu,
    names_to = "var",
    values_to = "response"
  ) |>
  dplyr::mutate(
    lagst = str_extract(var, "(?<=\\.)\\w+") |> replace_na("cont"),
    lag = case_match(
      lagst,
      "lag3" ~ 3L,
      "lag2" ~ 2L,
      "lag" ~ 1L,
      "cumu" ~ -1L,
      .default = 0L
    ),
    var = case_when(
      str_detect(var, "flood") ~ "flood",
      str_detect(var, "drought") ~ "drought"
    )
  )

min_max <- c(
  min(
    subset(rain, model != "main" & var == "flood")$response,
    subset(rain, model != "main" & var == "drought")$response
  ),
  max(
    subset(rain, model != "main" & var == "flood")$response,
    subset(rain, model != "main" & var == "drought")$response
  )
)

custom_stats <- rain |>
  filter(model != "main") |>
  group_by(lag = factor(lag), var) |>
  summarise(
    ymin = quantile(response, 0.05, na.rm = TRUE),
    lower = quantile(response, 0.25, na.rm = TRUE),
    middle = quantile(response, 0.50, na.rm = TRUE),
    upper = quantile(response, 0.75, na.rm = TRUE),
    ymax = quantile(response, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# Flood plot ----
################################################################################

f = ggplot() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "darkgrey",
    alpha = 0.5
  ) +
  geom_boxplot(
    data = filter(custom_stats, var == "flood"),
    aes(
      x = lag,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    color = "#43A7BA",
    fill = "#43A7BA",
    alpha = 0.35,
    size = 0.5,
    width = 0.3
  ) +
  geom_point(
    data = subset(rain, model == "main" & var == "flood"),
    aes(x = factor(lag), y = response),
    color = "black",
    alpha = 1,
    size = 0.5
  ) +
  # geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Flood (month lags)", y = NULL) +
  scale_x_discrete(
    breaks = c("-1", "0", "1", "2", "3"),
    labels = c("cumulative\neffect", "0", "1", "2", "3")
  ) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 5),
    plot.margin = unit(c(0.3, 0.3, 1, 0), units = "cm")
  ) +
  ylim(min_max)
f

################################################################################
# Drought plot ----
################################################################################

d = ggplot() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "darkgrey",
    alpha = .5
  ) +
  geom_boxplot(
    data = filter(custom_stats, var == "drought"),
    aes(
      x = lag,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    color = "#C99776",
    fill = "#C99776",
    alpha = 0.35,
    size = 0.5,
    width = 0.3
  ) +
  geom_point(
    data = subset(rain, model == "main" & var == "drought"),
    aes(x = factor(lag), y = response),
    color = "black",
    alpha = 1,
    size = .5
  ) +
  # geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Drought (month lags)", y = NULL) +
  scale_x_discrete(
    breaks = c("-1", "0", "1", "2", "3"),
    labels = c("cumulative\neffect", "0", "1", "2", "3")
  ) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 0),
    plot.margin = unit(c(0.0, 0.0, 1, 0.2), units = "cm"),
    # plot.margin = unit(c(0, 0, 0, 0), units = "cm")
  ) +
  # ylim(-5, 5) +
  scale_y_continuous(
    limits = c(-7, 5),
    expand = expansion(c(0, 0)),
    breaks = seq(-6, 4, by = 2)
  )
d

################################################################################
# Intervention plot ----
################################################################################

mycols <- c(colnames(all_mods)[grep("intervention", colnames(all_mods))])

inter <- all_mods |>
  dplyr::select(dplyr::all_of(mycols), model) |>
  tidyr::pivot_longer(
    cols = -model,
    names_to = "var",
    values_to = "response"
  ) |>
  dplyr::mutate(
    var = stringr::str_replace_all(var, "I\\(intervention", "Int. "),
    var = stringr::str_replace_all(var, "\\)", "")
  )

inter_stats <- inter |>
  dplyr::filter(model != "main") |>
  dplyr::group_by(var) |>
  dplyr::summarise(
    ymin = quantile(response, 0.05, na.rm = TRUE),
    lower = quantile(response, 0.25, na.rm = TRUE),
    middle = quantile(response, 0.50, na.rm = TRUE),
    upper = quantile(response, 0.75, na.rm = TRUE),
    ymax = quantile(response, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

intervention_fig <- ggplot() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "darkgrey",
    alpha = .5
  ) +
  geom_boxplot(
    data = inter_stats,
    aes(
      x = var,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    color = "pink",
    fill = "pink",
    alpha = 0.35,
    size = 0.5,
    width = 0.3
  ) +
  geom_point(
    data = subset(inter, model == "main"),
    aes(x = factor(var), y = response),
    color = "black",
    alpha = 1,
    size = .5
  ) +
  # geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Interventions", y = NULL) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 0),
    plot.margin = unit(c(0.0, 0.0, 1, 0.2), units = "cm"),
    # plot.margin = unit(c(0, 0, 0, 0), units = "cm")
  ) +
  scale_y_continuous(
    limits = c(-7, 5),
    expand = expansion(c(0, 0)),
    breaks = seq(-6, 4, by = 2)
  )

intervention_fig

################################################################################
# Global time series ----
################################################################################

hist_meta <- file.path(hist_pred_dir, "RowMetadata.feather") |>
  arrow::read_feather()

hist_pred <- file.path(hist_pred_dir, "iter_1.feather") |>
  arrow::read_feather() |>
  data.table::as.data.table()

hist_pred[, names(hist_meta) := hist_meta]
hist_pred[, run := as.character(run)]

# Group meana - scenario, model, and year ----
hist_summary_main <- hist_pred[,
  list(
    Pred = mean(Pred, na.rm = TRUE),
    Pf.temp = mean(Pf.temp, na.rm = TRUE),
    Pf.flood = mean(Pf.flood, na.rm = TRUE),
    Pf.drought = mean(Pf.drought, na.rm = TRUE)
  ),
  by = .(scenario, model, year, run)
] |>
  baseline_adjust_summarize(
    variable = "Pred",
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 1900:1930,
    confidence_level = 0.90
  ) |>
  dplyr::mutate(
    scenario = factor(scenario, levels = names(historical_scenario_names))
  ) |>
  dplyr::filter(year > 1901)


hist.to.graph <- file.path(
  hist_sum_dir,
  "historical_pred_sum_scen_mod_yr.feather"
) |>
  arrow::read_feather() |>
  baseline_adjust_summarize(
    variable = "Pred",
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 1900:1930,
    confidence_level = 0.90
  ) |>
  dplyr::mutate(
    scenario = factor(scenario, levels = names(historical_scenario_names))
  ) |>
  dplyr::filter(year > 1901)

future.to.graph <- file.path(
  fut_sum_dir,
  "future_pred_sum_scen_mod_yr.feather"
) |>
  arrow::read_feather() |>
  baseline_adjust_summarize(
    variable = "Pred",
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 2015:2020,
    confidence_level = 0.90
  ) |>
  dplyr::mutate(
    scenario = factor(scenario, levels = names(future_scenario_names))
  ) |>
  dplyr::filter(year > 2016)

base <- hist.to.graph |>
  dplyr::filter(scenario == 'historical', year %in% c(2010:2014)) |>
  dplyr::pull(median) |>
  mean()

future.to.graph <- future.to.graph |>
  dplyr::mutate(
    median = median + base,
    upper = upper + base,
    lower = lower + base
  )

graph.data <- hist.to.graph |>
  dplyr::bind_rows(future.to.graph) |>
  dplyr::mutate(scenario = factor(scenario, levels = scenarios)) |>
  # Start plotting in 1902 and 2016 because it's the first full year with lags
  # incorporated right.
  dplyr::filter(!(year %in% c(1901, 2015))) |>
  # radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT
  # LOOKING CLOSELY this is a way of hard coding the CI's to still plot thanks
  # to how ggplot does CI's this is for plotting purposes ONLY and text stats
  # give full CI's
  dplyr::mutate(lower = pmax(lower, -2.2))

# ggplot() +
#   geom_line(
#     data = hist_summary_main,
#     aes(x = year, y = median, group = scenario, color = scenario),
#     lwd = 1.3
#   )

s <- graph.data |>
  ggplot(aes(x = year, y = median, group = scenario, color = scenario)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) +
  scale_color_manual(
    values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"),
    labels = scenario_labels,
    name = ''
  ) +
  scale_fill_manual(
    values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"),
    labels = scenario_labels,
    name = ''
  ) +
  geom_vline(xintercept = 2014.5, linetype = 'dashed') +
  geom_line(aes(x = year, y = median), lwd = 1.3) +
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
  labs(x = 'Year', y = 'Prevalence (%)') +
  scale_x_continuous(
    breaks = seq(1900, 2100, by = 50),
    labels = as.character(seq(1900, 2100, by = 50)),
    expand = expansion(mult = c(0.02, 0.01))
  ) +
  theme(
    axis.title.x = element_text(vjust = -3),
    # axis.title.y = element_text(vjust = 6),
    plot.margin = unit(c(0.0, 0.5, 0.5, 0), "cm"),
    legend.position = "inside",
    legend.position.inside = c(0.13, 0.29),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_blank(),
    # plot.margin = unit(c(0, 0, 0, 0), units = "cm")
  )
s

################################################################################
# Plot assembly ----
################################################################################

top_row <- (g_with_hist + f + d + intervention_fig) +
  plot_layout(ncol = 4, widths = c(5, 5, 5, 2))

f2 <- top_row / s + plot_annotation(tag_levels = 'A')

ggsave(
  filename = "Figure2.pdf",
  plot = f2,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure2.jpg",
  plot = f2,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in"
)

################################################################################
# End of file ----
################################################################################
