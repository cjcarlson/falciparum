
library(tidyverse)
library(data.table)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

data.to.graph <- here::here("TempFiles", "Fig4Regionals.feather") |> 
  arrow::read_feather()

data.to.graph |>  
  drop_na() |> 
  mutate(
    region = recode(
      region, !!!c(
        'Sub-Saharan Africa (Central)' = 'Central Africa',
        'Sub-Saharan Africa (East)' = 'East Africa',
        'Sub-Saharan Africa (Southern)' = 'Southern Africa',
        'Sub-Saharan Africa (West)' = 'West Africa'))) %>%
  ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) + 
  geom_line(aes(y=median), lwd = 1.25) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) + 
  scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                    labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                    name = '') + 
  theme_classic() +
  theme(plot.title = element_text(size = 18)) + 
  xlab(NULL) + 
  ylab("Predicted change in prevalence (%)") + 
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) + 
  facet_wrap(region ~ ., ncol = 4) + 
  ggtitle('G') + 
  theme(legend.position = 'bottom') -> 
  bottom

