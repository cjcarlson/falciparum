
data.to.graph <- here::here("TempFiles", "Fig3Regionals.csv") |> 
  readr::read_csv(show_col_types = FALSE) 

data.to.graph |> 
  mutate(scenario = factor(scenario, levels = c('hist-nat', 'historical'))) |>
  mutate(
    region = recode(
      region, !!!c(
        'Sub-Saharan Africa (Central)' = 'Central Africa',
        'Sub-Saharan Africa (East)' = 'East Africa',
        'Sub-Saharan Africa (Southern)' = 'Southern Africa',
        'Sub-Saharan Africa (West)' = 'West Africa'))) |>
  ### Start plotting in 1902 because it's the first full year with lags incorporated right.
  filter(year > 1901) |> 
  ############ radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT LOOKING CLOSELY
  ############ this is a way of hard coding the CI's to still plot thanks to how ggplot does CI's
  ############ this is for plotting purposes ONLY and text stats give full CI's
  mutate(lower = pmax(lower, -0.6), upper = pmin(upper, 1.0)) |>
    ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) + 
  geom_line(aes(y=median), lwd = 1.25) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) + 
  theme_classic() + 
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) + 
  facet_wrap(region ~ ., nrow = 1) + 
  xlab(NULL) + 
  ylab("Prevalence (%)") + 
  ylim(-0.6, 1) +
  scale_color_manual(
    values = c("grey50", "#287DAB"), 
    labels = c('Historical counterfactual', 'Historical climate'),
    name = '') + 
  scale_fill_manual(
    values = c("grey50", "#287DAB"), 
    labels = c('Historical counterfactual', 'Historical climate'),
    name = '') + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(size = 20)) -> 
  bottom
