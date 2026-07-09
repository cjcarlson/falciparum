################################################################################
# This script makes all four panels of Extended Data Figure 6.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  here,
  reshape,
  patchwork,
  tidyverse,
  data.table
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)


################################################################################
# Historical time series data ----
################################################################################

hist_boot <- file.path(
  hist_sum_dir,
  "historical_vcov_pred_sum_scen_mod_yr.feather"
) |>
  arrow::read_feather() |>
  dplyr::filter(run != "main")

variables <- list(
  list(name = "Pred", label = "Prevalence (%)"),
  list(name = "Pf.temp", label = "Partial effect of temperature"),
  list(name = "Pf.flood", label = "Partial effect of floods"),
  list(name = "Pf.drought", label = "Partial effect of droughts")
)

plots <- list()

for (i in seq_along(variables)) {
  var <- variables[[i]]

  boot <- baseline_adjust_summarize(
    df = hist_boot,
    variable = var$name,
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 1900:1930,
    confidence_level = 0.90
  ) |>
    dplyr::filter(year > 1901)

  plots[[i]] <- partials_plot(
    boot,
    var$label,
    i == 1,
    legend_position = c(0.17, 0.86),
    scen_colors = hist_scenario_colors,
    scen_labels = hist_scenario_labels
  )
}

combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]] &
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.title = element_blank()
  ) &
  scale_x_continuous(
    limits = c(1900, 2014),
    breaks = c(1900, 1925, 1950, 1975, 2000), 
    expand = expansion(add = c(2,2))
  )
combined_plot

ggplot2::ggsave(
  filename = paste0("ED_Figure_hist_partials.", fig_file_type),
  plot = combined_plot,
  path = here::here("Results", "Figures"),
  width = 7.42,
  height = 10.07,
  units = "in"
)

################################################################################
# End of file ----
################################################################################
