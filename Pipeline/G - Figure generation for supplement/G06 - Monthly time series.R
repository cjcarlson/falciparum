library(zoo)
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)
library(cowplot)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(
  "Pipeline",
  "A - Utility functions",
  "A01 - Utility code for calculations.R"
))
source(here::here(
  "Pipeline",
  "A - Utility functions",
  "A02 - Utility code for plotting.R"
))

hist.to.graph <- here::here("TempFiles", "Fig2Hist-monthly.csv") |>
  vroom::vroom(show_col_types = FALSE)

hist.to.graph |>
  filter(scenario == 'historical', year %in% c(2010:2014)) |>
  pull(median) |>
  mean() -> base

graph.data <- hist.to.graph |>
  mutate(
    scenario = factor(
      scenario,
      levels = c('hist-nat', 'historical', 'ssp126', 'ssp245', 'ssp585')
    )
  ) |>
  filter(!(year %in% c(1901, 2015))) |>
  mutate(
    month_num = match(month, month.abb),
    date = make_date(year = year, month = month_num, day = 1),
    lower = pmax(lower, -1.75),
  )

ggplot(
  data = graph.data,
  aes(x = date, y = median, group = scenario, color = scenario)
) +
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
  # geom_vline(xintercept = 2014.5, linetype = 'dashed') +
  geom_line(aes(x = date, y = median), lwd = 1) +
  # geom_ribbon(
  #   aes(ymin = lower, ymax = upper, colour = scenario),
  #   fill = NA,
  #   linewidth = 0.1,
  #   show.legend = FALSE,
  # ) +
  # geom_ribbon(
  #   aes(ymin = lower, ymax = upper, fill = scenario),
  #   color = NA,
  #   alpha = 0.1
  # ) +
  scale_x_date(
    breaks = c(
      as.Date("1900-01-01"),
      as.Date("1925-01-01"),
      as.Date("1950-01-01"),
      as.Date("2000-01-01")
    ),
    # date_breaks = "25 years",
    date_labels = "%Y",
    expand = expansion(0.02, 0.1),
  ) +
  labs(x = 'Year', y = 'Prevalence (%)') +
  theme(
    axis.title.x = element_text(vjust = -3),
    axis.title.y = element_text(vjust = 6),
    plot.margin = unit(c(0.5, 0.5, 1, 1), "cm"),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.05),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_blank()
  ) -> s
s

ggsave(
  filename = "FigureS-mn-time-series.jpg",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 12,
  height = 6,
  units = "in"
)

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

###### seasonal difference

hist.to.graph <- here::here("TempFiles", "Fig3Regionals-model-monthly.csv") |>
  vroom::vroom(show_col_types = FALSE) |>
  dplyr::mutate(
    region = stringr::str_remove_all(region, "Sub-Saharan Africa \\("),
    region = stringr::str_replace_all(region, "\\)", " Africa")
  )

graph.data <- hist.to.graph |>
  mutate(
    scenario = factor(
      scenario,
      levels = c('hist-nat', 'historical', 'ssp126', 'ssp245', 'ssp585')
    )
  ) |>
  ### Start plotting in 1902 and 2016 because it's the first full year with lags incorporated right.
  filter(!(year %in% c(1901, 2015))) |>
  mutate(
    month_num = match(month, month.abb),
    date = make_date(year = year, month = month_num, day = 1),
    lower = pmax(lower, -1.75),
  )

yr_order <- graph.data |>
  distinct(year) |>
  arrange(year) |>
  pull(year)

last5 <- tail(yr_order, 5)

diff.data <- graph.data |>
  filter(year %in% last5) |>
  mutate(month_num = match(month, month.abb)) |>
  pivot_wider(
    id_cols = c(model, region, year, month, month_num),
    names_from = scenario,
    values_from = median
  ) |>
  ## calculate the difference:  historical − hist-nat
  mutate(diff = historical - `hist-nat`)


test <- diff.data |>
  dplyr::group_by(region, model, month, month_num) |>
  dplyr::summarise(
    diff = mean(diff, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

test2 <- test |>
  dplyr::group_by(region, month_num) |>
  dplyr::summarise(
    mean_month = mean(diff, na.rm = TRUE)
  )

monthly_diff <- ggplot() +
  geom_hline(data = NULL, yintercept = 0, colour = "black", linewidth = 0.3) +
  geom_line(
    data = test,
    aes(
      x = month_num,
      y = diff,
      # color = region,
      group = model
    ),
    alpha = 0.3,
    linewidth = 1.25,
    color = "grey60"
  ) +
  geom_line(
    data = test2,
    aes(x = month_num, y = mean_month),
    linewidth = 1.5, # mean trace
    colour = "#287DAB"
  ) +
  facet_wrap(~region, ncol = 1) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    expand = c(0.02, 0) # small left/right padding
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
