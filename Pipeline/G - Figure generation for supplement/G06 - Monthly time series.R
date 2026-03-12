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

####################################################################
####################################################################
###### time series plot - full monthly time series - VERY UGLY
####################################################################
####################################################################

# hist.to.graph <- here::here("TempFiles", "Fig2Hist-monthly.csv") |>
#   vroom::vroom(show_col_types = FALSE)

# hist.to.graph |>
#   filter(scenario == 'historical', year %in% c(2010:2014)) |>
#   pull(median) |>
#   mean() -> base

# graph.data <- hist.to.graph |>
#   mutate(
#     scenario = factor(
#       scenario,
#       levels = c('hist-nat', 'historical', 'ssp126', 'ssp245', 'ssp585')
#     )
#   ) |>
#   filter(!(year %in% c(1901, 2015))) |>
#   mutate(
#     month_num = match(month, month.abb),
#     date = make_date(year = year, month = month_num, day = 1),
#     lower = pmax(lower, -1.75),
#   )

# ggplot(
#   data = graph.data,
#   aes(x = date, y = median, group = scenario, color = scenario)
# ) +
#   theme_bw() +
#   geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) +
#   scale_color_manual(
#     values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"),
#     labels = scenario_labels,
#     name = ''
#   ) +
#   scale_fill_manual(
#     values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"),
#     labels = scenario_labels,
#     name = ''
#   ) +
#   # geom_vline(xintercept = 2014.5, linetype = 'dashed') +
#   geom_line(aes(x = date, y = median), lwd = 1) +
#   # geom_ribbon(
#   #   aes(ymin = lower, ymax = upper, colour = scenario),
#   #   fill = NA,
#   #   linewidth = 0.1,
#   #   show.legend = FALSE,
#   # ) +
#   # geom_ribbon(
#   #   aes(ymin = lower, ymax = upper, fill = scenario),
#   #   color = NA,
#   #   alpha = 0.1
#   # ) +
#   scale_x_date(
#     breaks = c(
#       as.Date("1900-01-01"),
#       as.Date("1925-01-01"),
#       as.Date("1950-01-01"),
#       as.Date("2000-01-01")
#     ),
#     # date_breaks = "25 years",
#     date_labels = "%Y",
#     expand = expansion(0.02, 0.1),
#   ) +
#   labs(x = 'Year', y = 'Prevalence (%)') +
#   theme(
#     axis.title.x = element_text(vjust = -3),
#     axis.title.y = element_text(vjust = 6),
#     plot.margin = unit(c(0.5, 0.5, 1, 1), "cm"),
#     legend.position = "inside",
#     legend.position.inside = c(0.1, 0.05),
#     legend.margin = margin(0, 0, 0, 0),
#     legend.text = element_text(size = rel(0.8)),
#     legend.title = element_blank()
#   ) -> s
# s

# ggsave(
#   filename = "FigureS-mn-time-series.jpg",
#   plot = last_plot(),
#   path = here::here("Figures"),
#   width = 12,
#   height = 6,
#   units = "in"
# )

###### seasonal sub-series - Last 5 years of data, averaged each month
###### to show prevelence pattern at the monthly level across a year

# yr_order <- graph.data |>
#   distinct(year) |>
#   arrange(year) |>
#   pull(year)

# last5 <- tail(yr_order, 5)

# test <- graph.data |>
#   filter(year %in% last5) |>
#   mutate(month_num = match(month, month.abb)) |>
#   group_by(scenario, month_num) |>
#   summarise(mean_month = mean(median, na.rm = TRUE)) |>
#   ungroup()

# ggplot(
#   data = test,
#   aes(
#     month_num,
#     mean_month,
#     group =  scenario,
#     color = scenario
#   )
# ) +
#   geom_line(alpha = 0.25) +
#   geom_line(aes(y = mean_month), linewidth = .5) +
#   scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   scale_color_manual(
#     values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"),
#     labels = scenario_labels,
#     name = ''
#   ) +
#   labs(x = NULL, y = "Prevalence (%)", title = "Seasonal sub-series") +
#   theme_bw()
