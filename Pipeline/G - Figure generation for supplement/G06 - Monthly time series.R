############################################################
# This script makes 
############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  zoo,
  here,
  broom,
  knitr,
  cowplot,
  patchwork,
  tidyverse,
  lubridate,
  kableExtra
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Load monthly regional hist predictions ----
################################################################################

hist_scen_mod_yr_adm1_pred <- file.path(
  hist_sum_dir,
  "historical_cru_pred_sum_scen_mod_yr_mon_reg.feather"
) |>
  arrow::read_feather() |>
  dplyr::mutate(
    region = stringr::str_remove_all(region, "Sub-Saharan Africa \\("),
    region = stringr::str_replace_all(region, "\\)", " Africa")
  )

################################################################################
# Regional monthly diff ----
################################################################################

diff_df <- hist_scen_mod_yr_adm1_pred |>
  tidyr::pivot_wider(
    id_cols = c(month, year, model, region, run),
    names_from = scenario,
    values_from = Pred
  ) |>
  dplyr::mutate(
    diff = historical - `hist-nat`,
    month_num = match(month, month.abb),
    date = make_date(year = year, month = month_num, day = 1)
  )

################################################################################
# Regional mean monthly diff ----
################################################################################

main_rgn_mean_diff_df <- diff_df |>
  dplyr::filter(run == "main") |>
  dplyr::group_by(region, month_num) |>
  dplyr::summarize(
    mean = mean(diff, na.rm = TRUE),
    median = median(diff, na.rm = TRUE),
  )

boot_rgn_mean_diff_df <- diff_df |>
  dplyr::filter(run != "main") |>
  dplyr::group_by(region, month_num) |>
  dplyr::summarize(
    upper = quantile(diff, 0.95, na.rm = TRUE),
    lower = quantile(diff, 0.05, na.rm = TRUE)
  )

rgn_mean_diff_df <- dplyr::left_join(
  main_rgn_mean_diff_df,
  boot_rgn_mean_diff_df
)

################################################################################
# Regional model mean monthly diff ----
################################################################################

main_rgn_mod_mean_diff_df <- diff_df |>
  dplyr::filter(run == "main") |>
  dplyr::group_by(region, model, month_num) |>
  dplyr::summarize(
    mean = mean(diff, na.rm = TRUE),
    median = median(diff, na.rm = TRUE)
  )

boot_rgn_mod_mean_diff_df <- diff_df |>
  dplyr::filter(run != "main") |>
  dplyr::group_by(region, model, month_num) |>
  dplyr::summarize(
    upper = quantile(diff, 0.95, na.rm = TRUE),
    lower = quantile(diff, 0.05, na.rm = TRUE)
  )

rgn_mod_mean_diff_df <- dplyr::left_join(
  main_rgn_mod_mean_diff_df,
  boot_rgn_mod_mean_diff_df
)

################################################################################
# Monthly diff plot ----
################################################################################

monthly_diff <- ggplot() +
  geom_hline(data = NULL, yintercept = 0, colour = "black", linewidth = 0.3) +
  geom_line(
    data = rgn_mod_mean_diff_df,
    aes(
      x = month_num,
      y = mean,
      # color = region,
      group = model
    ),
    alpha = 0.3,
    linewidth = 1.25,
    color = "grey60"
  ) +
  geom_line(
    data = rgn_mean_diff_df,
    aes(
      x = month_num,
      y = mean,
      group = region
    ),
    linewidth = 1.5, # mean trace
    colour = "#287DAB"
  ) +
  facet_wrap(~region, ncol = 1) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    expand = c(0.02, 0) # small left/right padding
  ) +
  # scale_y_continuous(
  #   limits = c(-10, 10)
  # ) +
  labs(
    x = NULL,
    y = "Change in prevalence (%)",
    # title = "Avg Monthly difference (2010-2014) between ‘historical’ and ‘hist-nat’ scenarios",
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -2),
    plot.margin = unit(c(0.5, 0.5, 1, 1), "cm"),
    legend.position = "none",
    # legend.position.inside = c(0.13, 0.85),
  )

monthly_diff

ggplot2::ggsave(
  filename = "Supp_Figure_monthly-diff.jpg",
  plot = monthly_diff,
  path = here::here("Results", "Figures"),
  width = 7.42,
  height = 10.07,
  units = "in"
)

################################################################################
# Global time series data ----
################################################################################

# log_msg("load the regional time series data")

hist_scen_mod_yr <- file.path(
  hist_sum_dir,
  "historical_cru_pred_sum_scen_mod_yr.feather"
) |>
  arrow::read_feather() |>
  data.table::as.data.table()

global_results <- calc_hist_regional_diff(hist_scen_mod_yr)

################################################################################
# Regional time series data ----
################################################################################

hist_scen_mod_yr_reg <- file.path(
  hist_sum_dir,
  "historical_cru_pred_sum_scen_mod_yr_reg.feather"
) |>
  arrow::read_feather() |>
  data.table::as.data.table()

region_results <- names(region_names)[2:5] |>
  purrr::map(~ calc_hist_regional_diff(hist_scen_mod_yr_reg, .x)) |>
  purrr::list_rbind()

region_results <- bind_rows(global_results, region_results) |>
  dplyr::mutate(Region = case_match(Region, !!!region_formulas))

print(region_results)

output_file <- file.path(hist_sum_dir, "historical_regional_diffs.csv")

readr::write_csv(region_results, output_file)

cat("Results have been saved to:", output_file, "\n")

################################################################################
# Regional peak effect ----
################################################################################

regional_avg_effect <- region_results |>
  # here::here("TempFiles", "H04_results_summary.csv") |>
  # vroom::vroom(show_col_types = FALSE) |>
  dplyr::filter(Region != "Sub-Saharan Africa (continent-wide)") |>
  dplyr::rename(`Average Annual Impact (% points)` = MeanDifference)

region_peak_effect <- rgn_mean_diff_df |>
  dplyr::group_by(region) |>
  dplyr::slice_max(abs(mean), n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Region = region,
    `Month of Peak Impact` = factor(month.abb[month_num], levels = month.abb),
    `Impact Size (% points)` = mean,
    mon_Quantile_025 = lower,
    mon_Quantile_975 = upper,
    .keep = "none"
  ) |>
  dplyr::left_join(regional_avg_effect, by = join_by(Region)) |>
  dplyr::arrange(Region) |>
  dplyr::select(
    Region,
    `Month of Peak Impact`,
    `Impact Size (% points)`,
    mon_Quantile_025,
    mon_Quantile_975,
    `Average Annual Impact (% points)`,
    Quantile_025,
    Quantile_975
  )

################################################################################
# Format table ----
################################################################################

region_peak_effect <- rgn_mean_diff_df |>
  dplyr::group_by(region) |>
  dplyr::mutate(
    max_month_val = max(mean, na.rm = TRUE),
    min_month_val = min(mean, na.rm = TRUE),
    monthly_range = max_month_val - min_month_val
  ) |>
  dplyr::slice_max(abs(mean), n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Region = region,
    `Month of Peak Impact` = factor(month.abb[month_num], levels = month.abb),
    `Impact Size (% points)` = mean,
    mon_Quantile_025 = lower,
    mon_Quantile_975 = upper,
    `Monthly Range (% points)` = monthly_range,
    .keep = "none"
  ) |>
  dplyr::left_join(regional_avg_effect) |>
  dplyr::arrange(Region) |>
  dplyr::select(
    Region,
    `Month of Peak Impact`,
    `Impact Size (% points)`,
    mon_Quantile_025,
    mon_Quantile_975,
    `Monthly Range (% points)`,
    `Average Annual Impact (% points)`,
    Quantile_025,
    Quantile_975
  ) |>
  transmute(
    Region,
    `Month of Peak Impact`,
    `Impact Size (\\% points)` = sprintf(
      "%.2f (%.2f, %.2f)",
      `Impact Size (% points)`,
      mon_Quantile_025,
      mon_Quantile_975
    ),
    `Average Annual Impact (\\% points)` = sprintf(
      "%.2f (%.2f, %.2f)",
      `Average Annual Impact (% points)`,
      Quantile_025,
      Quantile_975
    )
  ) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c"),
    col.names = c(
      "Region",
      "\\makecell{Month of \\\\ Peak Impact}",
      "\\makecell{Impact Size \\\\ (\\% points)}",
      "\\makecell{Average Annual \\\\ Impact (\\% points)}"
    ),
    escape = FALSE
  ) |>
  kable_styling(latex_options = c("hold_position"))

region_peak_effect


kableExtra::save_kable(
  region_peak_effect,
  # file = file.path(hist_sum_dir, "monthly_differences.tex")
  file = here::here("Results", "Tables", "monthly_differences.tex")
)

################################################################################
# End of file ----
################################################################################
