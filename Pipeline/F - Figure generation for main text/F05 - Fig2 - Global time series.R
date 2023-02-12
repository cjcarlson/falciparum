
library(sf)
library(lubridate)
library(magrittr)
library(rgdal)
library(tidyverse)
library(patchwork)
library(data.table)
library(vroom)

hist.to.graph <- vroom("~/Github/falciparum/TempFiles/Fig2Hist.csv")
future.to.graph <- vroom("~/Github/falciparum/TempFiles/Fig2Future.csv")

hist.to.graph %>%
  filter(scenario == 'hist', year %in% c(2010:2014)) %>%
  pull(median) %>% mean() -> base

future.to.graph %<>%
  mutate(median = median + base,
         upper = upper + base,
         lower = lower + base)

graph.data <- bind_rows(hist.to.graph, future.to.graph %>% rename(scenario = RCP))

graph.data %>%
  mutate(scenario = factor(scenario, levels = c('nat', 'hist', 'rcp26', 'rcp45', 'rcp85'))) %>%
  ggplot(aes(x = year, y = median, group = scenario, color = scenario)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  scale_color_manual(values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Historical counterfactual', 'Historical climate', 'Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"), 
                    labels = c('Historical counterfactual', 'Historical climate', 'Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
                    name = '') + 
  geom_vline(xintercept = 2014.5, linetype = 'dashed') + 
  geom_line(aes(x = year, y = median), lwd = 1.3) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), color = NA, alpha = 0.1) +
  xlab("Year") + ylab("Change in prevalence (%)") + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 6),
        plot.margin = unit(c(0.5,0.5,1,1), "cm"), 
        legend.position = c(0.13, 0.29),
        legend.margin = margin(0, 0, 0, 0),
        legend.text=element_text(size=rel(0.8)),
        legend.title=element_blank()) -> s; s
