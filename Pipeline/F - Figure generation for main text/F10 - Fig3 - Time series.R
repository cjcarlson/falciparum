
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(vroom)

data.to.graph <- vroom("~/Github/falciparum/TempFiles/Fig3Regionals.csv")

data.to.graph %>% 
  mutate(scenario = factor(scenario, levels = c('nat', 'hist'))) %>%
  mutate(Region = recode(Region, !!!c('Sub-Saharan Africa (Central)' = 'Central Africa',
                                      'Sub-Saharan Africa (East)' = 'East Africa',
                                      'Sub-Saharan Africa (Southern)' = 'Southern Africa',
                                      'Sub-Saharan Africa (West)' = 'West Africa'))) %>%
  ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) + 
  geom_line(aes(y=median), lwd = 1.25) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) + 
  theme_classic() + 
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) + 
  facet_wrap(Region ~ ., nrow = 1) + 
  xlab(NULL) + 
  ylab("Predicted change in prevalence (%)") + 
  scale_color_manual(values = c("grey50", "#287DAB"), 
                     labels = c('Historical counterfactual', 'Historical climate'),
                     name = '') + 
  scale_fill_manual(values = c("grey50", "#287DAB"), 
                    labels = c('Historical counterfactual', 'Historical climate'),
                    name = '') + 
  theme(legend.position = 'bottom') + ggtitle('C') + 
  theme(plot.title = element_text(size = 20)) -> bottom
