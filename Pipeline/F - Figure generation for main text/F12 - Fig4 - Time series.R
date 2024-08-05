
data.to.graph <- here::here("TempFiles", "Fig4Regionals.csv") |> 
  readr::read_csv(show_col_types = FALSE)

data.to.graph %>% 
  mutate(
    region = recode(
      region, !!!c(
        'Sub-Saharan Africa (Central)' = 'Central Africa',
        'Sub-Saharan Africa (East)' = 'East Africa',
        'Sub-Saharan Africa (Southern)' = 'Southern Africa',
        'Sub-Saharan Africa (West)' = 'West Africa'))) %>%
  
  ### Start plotting in 2016 because it's the first full year with lags incorporated right.
  filter(year > 2015) %>% 
  ############ radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT LOOKING CLOSELY
  ############ this is a way of hard coding the CI's to still plot thanks to how ggplot does CI's
  ############ this is for plotting purposes ONLY and text stats give full CI's
  mutate(lower = pmax(lower, -4.3), upper = pmin(upper, 2.1)) %>%
  
  ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) + 
  geom_line(aes(y=median), lwd = 1.25) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) + 
  scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                    labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                    name = '') + 
  theme_classic() +
  theme(plot.title = element_text(size = 18)) + 
  xlab(NULL) + 
  ylab("Prevalence (%)") + 
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) + 
  facet_wrap(region ~ ., ncol = 4) + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(size = 20)) -> 
  bottom

