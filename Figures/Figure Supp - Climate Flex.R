
### SET UP
rm(list = ls())

user = "Colin" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/'
  repo = 'C:/Users/cjcar/Documents/Github/falciparum'
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else {
  wd = NA
  print('Script not configured for this user!')
}

setwd(wd)

# source functions from previous script
source(file.path(repo,'code/R_utils.R'))
source(file.path(repo,'code/utils_plotting.R'))

# packages
library(doSNOW)
library(lfe)
library(tidyverse)
library(zoo)
library(lubridate)
library(rgdal)
library(dplyr)
library(data.table)
library(tictoc)
library(vroom)

########################################################################
# A. INITIALIZING
########################################################################

#### Read in the data backup
data <- vroom('./Dataframe backups/formatted-backup.csv', 
              col_types = cols(Pf = "d", PfPR2 = "d"))

tdf <- bind_rows(data %>% select(temp, ppt) %>% mutate(sample = 'Background'),
                 data %>% filter(!is.na(PfPR2)) %>% select(temp, ppt) %>% mutate(sample = 'Sampled'))

tdf %>% 
  ggplot(aes(temp, color = NULL, fill = sample, alpha = sample)) + 
  geom_histogram(bins = 100, fill = "#C1657C") + 
  theme_bw() + scale_y_log10() +
  theme(legend.position = c(0.2, 0.9),
        legend.title = element_blank()) +
  scale_alpha_manual(values = c(0.35, 1)) + 
  xlab(expression(paste("Mean temperature (",degree,"C"))) + ylab("Count") -> thist

tdf %>% 
  ggplot(aes(ppt, color = NULL, fill = sample, alpha = sample)) + 
  geom_histogram(bins = 100, fill = "#287DAB") + 
  theme_bw() + scale_y_log10() +
  theme(legend.position = c(0.8, 0.9),
        legend.title = element_blank()) +
  scale_alpha_manual(values = c(0.35, 1)) + 
  xlab("Precipitation (mm)") + ylab("Count") -> phist


precip.key <- read_csv('~/Github/falciparum/precipkey.csv')
data %>% left_join(precip.key) %>% 
  dplyr::rename(ppt.90 = ppt_pctile0.9, 
                ppt.10 = ppt_pctile0.1) -> fd
fd$flood <- as.numeric(fd$ppt >= fd$ppt.90)
fd$drought <- as.numeric(fd$ppt <= fd$ppt.10)

library(lubridate)

fd %>% 
  #filter(!is.na(PfPR2)) %>% 
  select(month, year, OBJECTID, flood, drought) %>%
  group_by(month, year) %>% 
  summarize(flood = sum(flood, na.rm = TRUE),
            drought = sum(drought, na.rm = TRUE)) %>% 
  unite("monthyr", month:year, sep=' 1 ', remove=FALSE) %>% 
  mutate(monthyr = mdy(monthyr)) %>%
  pivot_longer(c('flood', 'drought'), 'event') %>% 
  dplyr::rename("Background" = "value") %>% 
  ggplot(aes(x = monthyr, y = event, fill = event, alpha = `Background`)) + 
  theme_bw() + 
  geom_tile() + 
  scale_fill_manual(values = c("#43A7BA", "#C99776")) + 
  scale_alpha_continuous(range = c(0.2, 1)) + 
  xlab(NULL) + ylab(NULL) + 
  guides(fill = FALSE) -> fd1

fd %>% 
  filter(!is.na(PfPR2)) %>% 
  select(month, year, OBJECTID, flood, drought) %>%
  group_by(month, year) %>% 
  summarize(flood = sum(flood, na.rm = TRUE),
            drought = sum(drought, na.rm = TRUE)) %>% 
  unite("monthyr", month:year, sep=' 1 ', remove=FALSE) %>% 
  mutate(monthyr = mdy(monthyr)) %>%
  pivot_longer(c('flood', 'drought'), 'event') %>% 
  dplyr::rename("Sampled" = "value") %>% 
  ggplot(aes(x = monthyr, y = event, fill = event, alpha = `Sampled`)) + 
  theme_bw() + 
  geom_tile() + 
  scale_fill_manual(values = c("#43A7BA", "#C99776")) + 
  scale_alpha_continuous(range = c(0.2, 1)) + 
  xlab(NULL) + ylab(NULL) + 
  guides(fill = FALSE) -> fd2


# fd %>% 
#   filter()
#   select(month, year, OBJECTID, flood, drought) %>%
#   group_by(month, year) %>% 
#   summarize(flood = sum(flood, na.rm = TRUE),
#             drought = sum(drought, na.rm = TRUE)) %>% 
#   unite("monthyr", month:year, sep=' 1 ', remove=FALSE) %>% 
#   mutate(monthyr = mdy(monthyr)) %>%
#   pivot_longer(c('flood', 'drought'), 'event') %>% 
#   ggplot(aes(x = monthyr, y = value, group = event, color = value)) +
#   theme_minimal() + 
#   geom_point(alpha = 0.1) + geom_smooth(method = 'gam') + 
#   facet_wrap(~ event, nrow = 2) + 
#   xlab(NULL) + ylab("ADM1 areas with value")

library(patchwork)
(thist + phist) / fd1 / fd2 + plot_layout(heights = c(2.3, 1, 1)) + plot_annotation(tag_levels = 'A')
