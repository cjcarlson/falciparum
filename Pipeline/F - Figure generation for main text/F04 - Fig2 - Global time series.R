
hist.to.graph <- here::here("TempFiles", "Fig2Hist.csv") |>
  vroom::vroom(show_col_types = FALSE)

future.to.graph <- here::here("TempFiles", "Fig2Future.csv") |> 
  vroom::vroom(show_col_types = FALSE)

hist.to.graph |>
  filter(scenario == 'historical', year %in% c(2010:2014)) |>
  pull(median) |>
  mean() -> 
  base

future.to.graph <- future.to.graph |> 
  mutate(median = median + base,
         upper = upper + base,
         lower = lower + base)

graph.data <- bind_rows(hist.to.graph, future.to.graph) #|> dplyr::rename(scenario = RCP))

graph.data |>
  mutate(scenario = factor(scenario, levels = c('hist-nat', 'historical', 'ssp126', 'ssp245', 'ssp585'))) |>
  
  ### Start plotting in 1902 and 2016 because it's the first full year with lags incorporated right.
  filter(!(year %in% c(1901, 2015))) |> 
  
  ############ radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT LOOKING CLOSELY
  ############ this is a way of hard coding the CI's to still plot thanks to how ggplot does CI's
  ############ this is for plotting purposes ONLY and text stats give full CI's
  mutate(lower = pmax(lower, -1.75)) |>
  
  ggplot(aes(x = year, y = median, group = scenario, color = scenario)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  scale_color_manual(
    values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"), 
    labels = scenario_labels,
    name = '') + 
  scale_fill_manual(
    values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"), 
    labels = scenario_labels,
    name = '') + 
  geom_vline(xintercept = 2014.5, linetype = 'dashed') + 
  geom_line(aes(x = year, y = median), lwd = 1.3) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), color = NA, alpha = 0.1) +
  labs(x = 'Year', y = 'Prevalence (%)') +
  theme(
    axis.title.x = element_text(vjust = -3),
    axis.title.y = element_text(vjust = 6),
    plot.margin = unit(c(0.5,0.5,1,1), "cm"), 
    legend.position = "inside",
    legend.position.inside = c(0.13, 0.29),
    legend.margin = margin(0, 0, 0, 0),
    legend.text=element_text(size=rel(0.8)),
    legend.title=element_blank()) -> 
  s
s
 