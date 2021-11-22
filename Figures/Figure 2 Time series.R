
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/HistoricalTempFiles")

setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c("year", "GCM", "scenario", "Region"))

# meta %<>%
#   tidyr::extract(run, 
#                  into = c('GCM','RCP'),
#                  regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
#                  remove = FALSE)  

for (i in 1:10) { #Loop starts  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'GCM,scenario,year,Region']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} #Loop ends

iter.df %<>% as.tibble()

iter.df %<>% pivot_longer(cols = -c(GCM, scenario, year, Region), names_to = c("Pred"))

iter.df %>% 
  filter(year %in% c(1900:1930)) %>%
  group_by(GCM, scenario, Region, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(scenario, Region, year) %>%
  summarize(median = median(value, na.rm = TRUE),
            upper = quantile(value, 0.95, na.rm = TRUE),
            lower = quantile(value, 0.05, na.rm = TRUE)) -> data.to.graph

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
  theme(legend.position = 'bottom') -> bottom
            
