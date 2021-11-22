
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)

setwd("D:/MalariaAfrica/FutureTempFiles")

meta <- fread("RowMetadata.csv", select = c("year", "run", "Region"))

# meta %<>%
#   tidyr::extract(run, 
#                  into = c('GCM','RCP'),
#                  regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
#                  remove = FALSE)  

for (i in 1:10) { #Loop stars  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'run,year,Region']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} #Loop ends

iter.df %<>% as.tibble()

iter.df %>%
  pivot_longer(cols = -c(run, year, Region), names_to = c("Pred")) %>%
  tidyr::extract(run, 
                 into = c('GCM','RCP'),
                 regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
                 remove = FALSE) -> iter.df2

iter.df2 %>% 
  filter(year %in% c(2015:2020)) %>%
  group_by(GCM, RCP, Region, Pred) %>%
  summarize(BetaMean = mean(value, na.rm = TRUE)) %>% 
  right_join(iter.df2) %>% 
  mutate(value = (value-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(RCP, Region, year) %>%
  summarize(median = median(value),
            upper = quantile(value, 0.95),
            lower = quantile(value, 0.05)) -> data.to.graph

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
  theme(legend.position = 'bottom') -> bottom
            
