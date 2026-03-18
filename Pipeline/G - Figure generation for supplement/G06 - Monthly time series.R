library(zoo)
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)
library(cowplot)
library(knitr)
library(kableExtra)
library(broom)

source(
  here::here(
    "Pipeline",
    "A - Utility functions",
    "A00 - Configuration.R"
  )
)
source(
  here::here(
    "Pipeline",
    "A - Utility functions",
    "A01 - Utility code for calculations.R"
  )
)
source(
  here::here(
    "Pipeline",
    "A - Utility functions",
    "A02 - Utility code for plotting.R"
  )
)

###### seasonal difference

rgn_mod_mon_df <- here::here("TempFiles", "Fig3Regionals-model-monthly.csv") |>
  vroom::vroom(show_col_types = FALSE) |>
  dplyr::mutate(
    region = stringr::str_remove_all(region, "Sub-Saharan Africa \\("),
    region = stringr::str_replace_all(region, "\\)", " Africa")
  )

rgn_mon_df <- here::here("TempFiles", "Fig3Regionals-monthly.csv") |>
  vroom::vroom(show_col_types = FALSE) |>
  dplyr::mutate(
    region = stringr::str_remove_all(region, "Sub-Saharan Africa \\("),
    region = stringr::str_replace_all(region, "\\)", " Africa")
  )

monthly_diff <- ggplot() +
  geom_hline(data = NULL, yintercept = 0, colour = "black", linewidth = 0.3) +
  geom_line(
    data = rgn_mod_mon_df,
    aes(
      x = month_num,
      y = median,
      # color = region,
      group = model
    ),
    alpha = 0.3,
    linewidth = 1.25,
    color = "grey60"
  ) +
  geom_line(
    data = rgn_mon_df,
    aes(
      x = month_num,
      y = median,
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
  scale_y_continuous(
    limits = c(-10, 10)
  ) +
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
  filename = "FigureS-monthly-diff.pdf",
  plot = monthly_diff,
  path = here::here("Figures"),
  width = 7.42,
  height = 10.07,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)


regional_avg_effect <- here::here("TempFiles", "H04_results_summary.csv") |>
  vroom::vroom(show_col_types = FALSE) |>
  dplyr::filter(Region != "Sub-Saharan Africa (continent-wide)") |>
  dplyr::rename(`Average Annual Impact (% points)` = MeanDifference)


region_peak_effect <- rgn_mon_df |>
  dplyr::group_by(region) |>
  dplyr::slice_max(abs(median), n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Region = region,
    `Month of Peak Impact` = factor(month.abb[month_num], levels = month.abb),
    `Impact Size (% points)` = median,
    mon_Quantile_025 = lower,
    mon_Quantile_975 = upper,
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
    `Average Annual Impact (% points)`,
    Quantile_025,
    Quantile_975
  )


region_peak_effect <- rgn_mon_df |>
  dplyr::group_by(region) |>
  dplyr::mutate(
    # find the month with the max and min median values
    max_month_val = max(median, na.rm = TRUE),
    min_month_val = min(median, na.rm = TRUE),
    # compute the range
    monthly_range = max_month_val - min_month_val
  ) |>
  dplyr::slice_max(abs(median), n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Region = region,
    `Month of Peak Impact` = factor(month.abb[month_num], levels = month.abb),
    `Impact Size (% points)` = median,
    mon_Quantile_025 = lower,
    mon_Quantile_975 = upper,
    `Monthly Range (% points)` = monthly_range, # new column here
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
  )

# # LaTeX table (booktabs + makecell-style headers)
# kableExtra::kable(
#   region_peak_effect,
#   format = "latex",
#   digits = 4,
#   booktabs = TRUE,
#   align = c("l", "c", "c", "c"),
#   col.names = c(
#     "Region",
#     "\\makecell{Month of \\\\ Peak Impact}",
#     "\\makecell{Impact Size \\\\ (\\% points)}",
#     "\\makecell{Average Annual Impact \\\\ (\\% points)}"
#   ),
#   escape = FALSE
# ) |>
#   kableExtra::kable_styling(latex_options = c("hold_position"))
