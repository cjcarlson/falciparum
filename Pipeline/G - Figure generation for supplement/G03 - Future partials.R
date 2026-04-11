############################################################
# This script makes all four panels of Figure S1.
############################################################
# Set up ----
############################################################

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

# log_msg("Load and prepare future projections")

future_pred <- file.path(
  fut_sum_dir,
  "future_cru_pred_sum_scen_mod_yr.feather"
) |>
  arrow::read_feather()

future_main <- future_pred[run == "main"]

future_boot <- future_pred[run != "main"]

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

  main <- baseline_adjust_summarize(
    df = future_main,
    variable = var$name,
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 2015:2020,
    confidence_level = 0.90
  ) |>
    dplyr::select(-c(upper, lower))

  boot <- baseline_adjust_summarize(
    df = future_boot,
    variable = var$name,
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 2015:2020,
    confidence_level = 0.90
  ) |>
    dplyr::select(-c(median, mean))

  data <- dplyr::left_join(main, boot) |>
    dplyr::filter(year > 2016)

  plots[[i]] <- partials_plot(
    data,
    var$label,
    i == 1,
    legend_position = c(0.175, 0.22)
  )
}

################################################################################
# Combine plots and save ----
################################################################################

combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
combined_plot

ggplot2::ggsave(
  filename = "Supp_Figure_fut_partials.jpg",
  plot = combined_plot,
  path = here::here("Results", "Figures"),
  width = 7.42,
  height = 10.07,
  units = "in"
)

################################################################################
# End of file ----
################################################################################
