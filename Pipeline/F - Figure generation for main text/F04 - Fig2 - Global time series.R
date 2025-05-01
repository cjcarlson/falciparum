hist.to.graph <- here::here("TempFiles", "Fig2Hist-revision.csv") |>
  vroom::vroom(show_col_types = FALSE)

# future.to.graph <- here::here("TempFiles", "Fig2Future-revision.csv") |>
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

graph.data |>
  mutate(month_num = match(month, month.abb)) |>
  group_by(scenario, month_num) |>
  mutate(mean_month = mean(median, na.rm = TRUE)) |>
  ungroup() |>
  filter(year %in% last5) |>
  ggplot(aes(
    month_num,
    median,
    group = interaction(year, scenario),
    color = scenario
  )) +
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

diff.data <- graph.data |>
  ## keep only the two baseline scenarios
  filter(scenario %in% c("historical", "hist-nat"), year %in% last5) |>
  ## convert Jan → 1, … Dec → 12  (needed for x-axis order)
  mutate(month_num = match(month, month.abb)) |>
  ## reshape so each scenario is a separate column
  pivot_wider(
    id_cols = c(year, month_num),
    names_from = scenario,
    values_from = median
  ) |>
  ## calculate the difference:  historical − hist-nat
  mutate(diff = historical - `hist-nat`) |>
  ## climatological (all-years) monthly mean of the difference
  group_by(month_num) |>
  mutate(mean_month = mean(diff, na.rm = TRUE)) |>
  ungroup()

ggplot(diff.data, aes(month_num, diff, group = year)) +
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
    subtitle = "Grey = last five years years · Blue = last five years monthly mean"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -2),
    plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")
  )

# yr_order <- diff.data |>
#   distinct(year) |>
#   arrange(year) |>
#   pull(year)

# first5 <- yr_order[1:5]
# last5  <- tail(yr_order, 5)

# ## ------------------------------------------------------------------ ##
# ## 2.  Average the monthly difference within those periods
# ## ------------------------------------------------------------------ ##
# delta_df <- diff.data |>
#   filter(year %in% c(first5, last5)) |>
#   mutate(period = if_else(year %in% first5, "first", "last")) |>
#   group_by(period, month_num) |>
#   summarise(mean_diff = mean(diff, na.rm = TRUE), .groups = "drop") |>
#   pivot_wider(names_from = period, values_from = mean_diff) |>
#   mutate(delta = last - first,
#          month_lab = factor(month.abb[month_num], levels = month.abb))

# ## ------------------------------------------------------------------ ##
# ## 3.  Plot a single seasonal line
# ## ------------------------------------------------------------------ ##
# ggplot(delta_df, aes(month_num, delta)) +
#   geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
#   geom_line(linewidth = 1.3, colour = "#287DAB") +
#   geom_point(size = 2, colour = "#287DAB") +
#   scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   labs(
#     x = NULL,
#     y = "Δ prevalence (%): last 5 yrs – first 5 yrs",
#     title = "Shift in historical – hist-nat difference\n(last five years minus first five years)"
#   ) +
#   theme_bw() +
#   theme(
#     axis.title.y = element_text(vjust = 3),
#     axis.title.x = element_text(vjust = -2),
#     plot.title   = element_text(face = "bold", hjust = 0.5),
#     plot.margin  = unit(c(0.5, 0.5, 1, 1), "cm")
#   )

#   diff.data <- graph.data |>
#     filter(scenario %in% c("historical", "hist-nat")) |>
#     mutate(month_num = match(month, month.abb)) |>
#     pivot_wider(
#       id_cols   = c(year, month_num),
#       names_from  = scenario,
#       values_from = median
#     ) |>
#     mutate(diff = historical - `hist-nat`)               # key column

#   # --------------------------------------------------------------------------- #
#   # 2.  Identify the first and last five distinct calendar years
#   # --------------------------------------------------------------------------- #
#   yr_sorted <- sort(unique(diff.data$year))
#   first5    <- yr_sorted[1:5]
#   last5     <- tail(yr_sorted, 5)

#   # --------------------------------------------------------------------------- #
#   # 3.  Average the difference within each 5-year block, month by month
#   # --------------------------------------------------------------------------- #
#   period_means <- diff.data |>
#     filter(year %in% c(first5, last5)) |>
#     mutate(period = if_else(year %in% first5, "First 5 yrs", "Last 5 yrs")) |>
#     group_by(period, month_num) |>
#     summarise(mean_diff = mean(diff, na.rm = TRUE), .groups = "drop")

#   # --------------------------------------------------------------------------- #
#   # 4.  Plot the two seasonal lines
#   # --------------------------------------------------------------------------- #
#   ggplot(period_means, aes(month_num, mean_diff, colour = period, group = period)) +
#     geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.3) +
#     geom_line(linewidth = 1.3) +
#     geom_point(size = 2) +
#     scale_x_continuous(breaks = 1:12, labels = month.abb) +
#     scale_colour_manual(
#       values = c("First 5 yrs" = "#4d5f8e", "Last 5 yrs" = "#C582B2"),
#       name = NULL
#     ) +
#     labs(
#       x = NULL,
#       y = "Historical – Hist-nat (% prevalence)",
#       title = "Monthly gap between scenarios: first vs last five years"
#     ) +
#     theme_bw() +
#     theme(
#       axis.title.y = element_text(vjust = 3),
#       axis.title.x = element_text(vjust = -2),
#       plot.title   = element_text(face = "bold", hjust = 0.5),
#       legend.position = "bottom"
#     )
