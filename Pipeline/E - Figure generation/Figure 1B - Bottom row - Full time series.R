
library(sf)
library(lubridate)
library(magrittr)
library(rgdal)
library(tidyverse)
library(patchwork)
library(data.table)

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

setwd("D:/MalariaAfrica/HistoricalTempFiles")

setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c("year", "scenario", "GCM"))

for (i in 1:1000) { 
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,GCM,year']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} 

iter.df %<>% as_tibble()
iter.df %<>% pivot_longer(cols = -c(scenario, GCM, year), names_to = c("Pred"))

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(scenario, GCM, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(scenario, year) %>%
  summarize(median = median(value, na.rm = TRUE),
            upper = quantile(value, 0.95, na.rm = TRUE),
            lower = quantile(value, 0.05, na.rm = TRUE)) -> hist.to.graph

write_csv(hist.to.graph, "~/Github/falciparum/TempFiles/Fig1Hist.csv")
rm(meta)

hist.to.graph %>%
  mutate(scenario = factor(scenario, levels = c('nat', 'hist'))) %>%
  ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) +
  geom_line(aes(y=median), lwd = 1.25) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) +
  theme_classic() +
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) +
  xlab(NULL) +
  ylab("Predicted change in prevalence (%)") +
  scale_color_manual(values = c("grey50", "#287DAB"),
                     labels = c('Historical counterfactual', 'Historical climate'),
                     name = '') +
  scale_fill_manual(values = c("grey50", "#287DAB"),
                    labels = c('Historical counterfactual', 'Historical climate'),
                    name = '') +
  theme(legend.position = 'bottom')

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

setwd("D:/MalariaAfrica/FutureTempFiles")
meta <- fread("RowMetadata.csv", select = c("year", "run"))

for (i in 1:10) { 
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'run,year']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} 

iter.df %<>% as_tibble()
iter.df %>%
  pivot_longer(cols = -c(run, year), names_to = c("Pred")) %>%
  tidyr::extract(run, 
                 into = c('GCM','RCP'),
                 regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
                 remove = FALSE) -> iter.df2

iter.df2 %>% 
  filter(year %in% c(2015:2019)) %>%
  group_by(RCP, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df2) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(RCP, year) %>%
  summarize(median = median(value),
            upper = quantile(value, 0.95),
            lower = quantile(value, 0.05)) -> future.to.graph

write_csv(future.to.graph, "~/Github/falciparum/TempFiles/Fig1Future.csv")
rm(meta)

# future.to.graph %>%
#   mutate(scenario = factor(RCP, levels = c('rcp26', 'rp45', 'rcp85'))) %>%
#   ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) +
#   geom_line(aes(y=median), lwd = 1.25) +
#   geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) +
#   theme_classic() +
#   geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) +
#   xlab(NULL) +
#   ylab("Predicted change in prevalence (%)") +
#   scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
#                      labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
#                      name = '') + 
#   scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
#                     labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
#                     name = '') + 
#   theme(legend.position = 'bottom')

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

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
        legend.position = c(0.13, 0.23)) 
