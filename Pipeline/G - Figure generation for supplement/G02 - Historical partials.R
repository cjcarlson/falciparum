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

# log_msg("Load and prepare historical projections")

historical_pred <- file.path(
  hist_sum_dir,
  "historical_cru_pred_sum_scen_mod_yr.feather"
) |>
  arrow::read_feather()

hist_main <- historical_pred[run == "main"]

hist_boot <- historical_pred[run != "main"]

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
    df = hist_main,
    variable = var$name,
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 1900:1930,
    confidence_level = 0.90
  ) |>
    dplyr::filter(year > 1901) |>
    dplyr::select(-c(upper, lower))

  boot <- baseline_adjust_summarize(
    df = hist_boot,
    variable = var$name,
    baseline_group = c("model", "scenario", "run"),
    adjusted_group = c("scenario", "year"),
    baseline_years = 1900:1930,
    confidence_level = 0.90
  ) |>
    dplyr::filter(year > 1901) |>
    dplyr::select(-c(median, mean))

  data <- dplyr::left_join(boot, main)

  plots[[i]] <- partials_plot(
    data,
    var$label,
    i == 1,
    legend_position = c(0.15, 0.16)
  )
}

combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
combined_plot

ggplot2::ggsave(
  filename = "Supp_Figure_hist_partials.jpg",
  plot = combined_plot,
  path = here::here("Results", "Figures"),
  width = 7.42,
  height = 10.07,
  units = "in"
)

################################################################################
# End of file ----
################################################################################
