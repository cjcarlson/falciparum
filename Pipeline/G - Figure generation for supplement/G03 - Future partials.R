################################################################################
# This script makes all four panels of Extended Data Figure 9.
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
  patchwork,
  tidyverse,
  data.table
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Future time series data ----
################################################################################

future_boot <- file.path(
  fut_sum_dir,
  "future_vcov_pred_sum_scen_mod_yr.feather"
) |>
  arrow::read_feather() |>
  dplyr::filter(run != "main")

################################################################################
# Create partial time series plots ----
################################################################################

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
    df = future_boot,
    variable = var$name,
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 2015:2020,
    confidence_level = 0.90
  ) |>
    dplyr::filter(year > 2016)

  plots[[i]] <- partials_plot(
    boot,
    var$label,
    i == 1,
    legend_position = c(0.2, 0.22),
    scen_colors = fut_scenario_colors,
    scen_labels = fut_scenario_labels
  )
}

################################################################################
# Combine plots and save ----
################################################################################

combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]] &
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.title = element_blank()
  )  &
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = c(2020, 2040, 2060, 2080, 2100), 
    expand = expansion(add = c(0,2))
  )
combined_plot

ggplot2::ggsave(
  filename = paste0("ED_Figure_fut_partials.", fig_file_type),
  plot = combined_plot,
  path = here::here("Results", "Figures"),
  width = 7.42,
  height = 10.07,
  units = "in"
)

################################################################################
# End of file ----
################################################################################
