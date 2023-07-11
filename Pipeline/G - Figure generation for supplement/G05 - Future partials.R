
library(tidyverse); library(magrittr); library(ggplot2); library(data.table); library(vroom)

iter.df <- vroom("~/Github/falciparum/TempFiles/SuppFutureBig.csv")

iter.df %<>% 
  tidyr::extract(run, 
                 into = c('GCM','scenario'),
                 regex = "(ACCESS-CSM2|ACCESS-ESM1|BCC-CSM2-MR|CanESM5|FGOALS-g3|GFDL-ESM4|IPSL-CM6A-LR|MIROC6|MRI-ESM2-0|NorESM2-LM)-(rcp26|rcp45|rcp85)",
                 remove = FALSE) 

iter.df %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) -> bm
iter.df %>% 
  left_join(bm) %>% 
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  mutate(scenario = factor(scenario)) %>% 
  group_by(scenario, year) %>%
  summarize(median = median(Pred, na.rm = TRUE),
            upper = quantile(Pred, 0.95, na.rm = TRUE),
            lower = quantile(Pred, 0.05, na.rm = TRUE)) %>%
  mutate(scenario = factor(scenario, levels = c('nat', 'hist', 'rcp26', 'rcp45', 'rcp85'))) %>%
  
  # plots start in 2016 the first full year
  filter(year > 2016) %>% 
  
  ggplot(aes(x = year, y = median, group = scenario, color = scenario)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                     name = '') + 
  geom_line(aes(x = year, y = median), lwd = 1.3) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), color = NA, alpha = 0.1) +
  xlab(NULL) + ylab("Prevalence (%)") + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 6),
        plot.margin = unit(c(0.2,0.5,0.2,1), "cm"), 
        legend.position = c(0.2, 0.27),
        legend.title = element_blank()) -> s1; s1

iter.df %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pf.temp, na.rm = TRUE)) -> bm
iter.df %>% 
  left_join(bm) %>% 
  mutate(Pf.temp = (Pf.temp-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(scenario, year) %>%
  summarize(median = median(Pf.temp, na.rm = TRUE),
            upper = quantile(Pf.temp, 0.95, na.rm = TRUE),
            lower = quantile(Pf.temp, 0.05, na.rm = TRUE)) %>%
  mutate(scenario = factor(scenario, levels = c('nat', 'hist', 'rcp26', 'rcp45', 'rcp85'))) %>%
  
  # plots start in 2016 the first full year
  filter(year > 2016) %>% 
  
  
  ggplot(aes(x = year, y = median, group = scenario, color = scenario)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                    labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                    name = '') + 
  geom_line(aes(x = year, y = median), lwd = 1.3) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), color = NA, alpha = 0.1) +
  xlab(NULL) + ylab("Partial effect of temperature") + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 6),
        plot.margin = unit(c(0.2,0.5,0.2,1), "cm"), 
        legend.position = "none",
        legend.title = element_blank()) -> s2; s2

iter.df %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pf.flood, na.rm = TRUE)) -> bm
iter.df %>% 
  left_join(bm) %>% 
  mutate(Pf.flood = (Pf.flood-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(scenario, year) %>%
  summarize(median = median(Pf.flood, na.rm = TRUE),
            upper = quantile(Pf.flood, 0.95, na.rm = TRUE),
            lower = quantile(Pf.flood, 0.05, na.rm = TRUE)) %>%
  mutate(scenario = factor(scenario, levels = c('nat', 'hist', 'rcp26', 'rcp45', 'rcp85'))) %>%
  
  # plots start in 2016 the first full year
  filter(year > 2016) %>% 
  
  
  ggplot(aes(x = year, y = median, group = scenario, color = scenario)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                    labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                    name = '') + 
  geom_line(aes(x = year, y = median), lwd = 1.3) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), color = NA, alpha = 0.1) +
  xlab(NULL) + ylab("Partial effect of floods") + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 6),
        plot.margin = unit(c(0.2,0.5,0.2,1), "cm"), 
        legend.position = "none",
        legend.title = element_blank()) -> s3; s3


iter.df %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(GCM, scenario, iter) %>%
  summarize(BetaMean = mean(Pf.drought, na.rm = TRUE)) -> bm
iter.df %>% 
  left_join(bm) %>% 
  mutate(Pf.drought = (Pf.drought-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(scenario, year) %>%
  summarize(median = median(Pf.drought, na.rm = TRUE),
            upper = quantile(Pf.drought, 0.95, na.rm = TRUE),
            lower = quantile(Pf.drought, 0.05, na.rm = TRUE)) %>%
  mutate(scenario = factor(scenario, levels = c('nat', 'hist', 'rcp26', 'rcp45', 'rcp85'))) %>%
  
  # plots start in 2016 the first full year
  filter(year > 2016) %>% 
  
  
  ggplot(aes(x = year, y = median, group = scenario, color = scenario)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) + 
  scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                    labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                    name = '') + 
  geom_line(aes(x = year, y = median), lwd = 1.3) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), color = NA, alpha = 0.1) +
  xlab(NULL) + ylab("Partial effect of droughts") + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 6),
        plot.margin = unit(c(0.2,0.5,0.2,1), "cm"), 
        legend.position = "none",
        legend.title = element_blank()) -> s4; s4

library(patchwork)

s1 / s2 / s3 / s4
