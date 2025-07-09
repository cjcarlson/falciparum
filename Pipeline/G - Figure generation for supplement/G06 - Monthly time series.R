library(zoo)
library(here)
# library(reshape)
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

# future.to.graph <- here::here("TempFiles", "Fig2Future-monthly.csv") |>
#   vroom::vroom(show_col_types = FALSE)

hist.to.graph |>
  filter(scenario == 'historical', year %in% c(2010:2014)) |>
  pull(median) |>
  mean() -> base

# future.to.graph <- future.to.graph |>
#   mutate(median = median + base, upper = upper + base, lower = lower + base)

graph.data <- hist.to.graph

# graph.data <- bind_rows(hist.to.graph, future.to.graph) #|> dplyr::rename(scenario = RCP))

graph.data |>
  mutate(
    scenario = factor(
      scenario,
      levels = c('hist-nat', 'historical', 'ssp126', 'ssp245', 'ssp585')
    )
  ) |>

  ### Start plotting in 1902 and 2016 because it's the first full year with lags incorporated right.
  filter(!(year %in% c(1901, 2015))) |>

  ############ radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT LOOKING CLOSELY
  ############ this is a way of hard coding the CI's to still plot thanks to how ggplot does CI's
  ############ this is for plotting purposes ONLY and text stats give full CI's
  mutate(
    month_num = match(month, month.abb),
    date = make_date(year = year, month = month_num, day = 1),
    lower = pmax(lower, -1.75),
  ) |>

  ggplot(aes(x = date, y = median, group = scenario, color = scenario)) +
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
  geom_line(aes(x = date, y = median), lwd = 1.3) +
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
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  labs(x = 'Year', y = 'Prevalence (%)') +
  theme(
    axis.title.x = element_text(vjust = -3),
    axis.title.y = element_text(vjust = 6),
    plot.margin = unit(c(0.5, 0.5, 1, 1), "cm"),
    legend.position = "inside",
    legend.position.inside = c(0.13, 0.29),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_blank()
  ) -> s
s

###### seasonal sub-series

yr_order <- graph.data |>
  distinct(year) |>
  arrange(year) |>
  pull(year)

first5 <- yr_order[1:5]
last5 <- tail(yr_order, 5)

test <- graph.data |>
  filter(year %in% last5) |>
  mutate(month_num = match(month, month.abb)) |>
  group_by(scenario, month_num, model) |>
  summarise(mean_month = mean(median, na.rm = TRUE)) |>
  ungroup()

ggplot(
  data = test,
  aes(
    month_num,
    mean_month,
    group = interaction(model, scenario),
    color = scenario
  )
) +
  geom_line(alpha = 0.25) +
  geom_line(aes(y = mean_month), linewidth = 1.2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(
    values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"),
    labels = scenario_labels,
    name = ''
  ) +
  labs(x = NULL, y = "Prevalence (%)", title = "Seasonal sub-series") +
  theme_bw()


###### seasonal difference

# diff.data <- graph.data |>
#   ## keep only the two baseline scenarios
#   filter(scenario %in% c("historical", "hist-nat"), year %in% last5) |>
#   ## convert Jan → 1, … Dec → 12  (needed for x-axis order)
#   mutate(month_num = match(month, month.abb)) |>
#   ## reshape so each scenario is a separate column
#   pivot_wider(
#     id_cols = c(year, month_num),
#     names_from = scenario,
#     values_from = median
#   ) |>
#   ## calculate the difference:  historical − hist-nat
#   mutate(diff = historical - `hist-nat`) |>
#   ## climatological (all-years) monthly mean of the difference
#   group_by(month_num) |>
#   mutate(mean_month = mean(diff, na.rm = TRUE)) |>
#   ungroup()

diff.data <- test |>
  ## keep only the two baseline scenarios
  # filter(scenario %in% c("historical", "hist-nat"), year %in% last5) |>
  # ## convert Jan → 1, … Dec → 12  (needed for x-axis order)
  # mutate(month_num = match(month, month.abb)) |>
  ## reshape so each scenario is a separate column
  pivot_wider(
    id_cols = c(month_num, model),
    names_from = scenario,
    values_from = mean_month
  ) |>
  ## calculate the difference:  historical − hist-nat
  mutate(diff = historical - `hist-nat`) |>
  ## climatological (all-years) monthly mean of the difference
  group_by(month_num) |>
  mutate(mean_month = mean(diff, na.rm = TRUE)) |>
  ungroup()

ggplot(diff.data, aes(month_num, diff, group = model)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(alpha = 0.1) + # yearly traces
  geom_line(
    aes(y = mean_month),
    linewidth = 1.3, # mean trace
    colour = "#287DAB"
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    expand = c(0.02, 0) # small left/right padding
  ) +
  labs(
    x = NULL,
    y = "Historical − Historical-natural (prevalence, %)",
    title = "Monthly difference between ‘historical’ and ‘hist-nat’ scenarios",
    subtitle = "Grey = individual models · Blue = last five years monthly mean"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -2),
    plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")
  )
