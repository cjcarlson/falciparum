
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(vroom)

data.to.graph <- vroom("~/Github/falciparum/TempFiles/Fig4Regionals.csv")

data.to.graph %>% 
  ggplot(aes(x = year, group = RCP, color = RCP, fill = RCP)) + 
  geom_line(aes(y=median), lwd = 1.25) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = RCP), color = NA, alpha = 0.1) + 
  scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                    labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                    name = '') + 
  theme_classic() + 
  xlab(NULL) + 
  ylab("Predicted change in prevalence (%)") + 
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) + 
  facet_wrap(Region ~ ., ncol = 4) + 
  ggtitle('E') + 
  theme(legend.position = 'bottom') -> bottom

