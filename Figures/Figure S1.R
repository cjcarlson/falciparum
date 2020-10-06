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
library(ggplot2)
library(lfe)
library(reshape)
library(tidyverse)
library(zoo)
library(lubridate)

############

#### Read in the data backup

# Has to be this way or it reads it as logical
data <- read.csv('./Dataframe backups/formatted-backup.csv')
data %>% na.omit() %>% as_tibble() -> data 

# Rescale to percentages
data %>% mutate(PfPR2 = PfPR2/100) -> data

# Generate the R0 curves:
data$predR0 <- sapply(data$temp, r0t)

ggplot(data = data, aes(x = temp), fill = NA) + 
  
  theme_bw() + 
  
  geom_line(aes(y = predR0, color='Predicted'), lwd=1.1) + 
  
  geom_smooth(aes(y = PfPR2*2.68, color='Observed'), lwd=1.1, fill='light grey') + 
  
  xlim(15,35) + ylab(expression('R'[0]*' predicted')) + xlab('Temperature') + 
  
  theme(text = element_text(size=15),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20, r= 0, b = 20, l = 0)),
        plot.title = element_text(margin = margin(t = 20, r= 0, b = 0, l = 0),
                                  size = 20, hjust = 0.5),
        legend.position='top',
        legend.key = element_rect(colour = NA, fill = 'white')) +
  
  #labs(title='Observed vs. theoretical temperature sensitivity', subtitle='')  +
  
  scale_y_continuous(sec.axis = sec_axis(~./2.68, name = "Observed prevalence (1900-2010)")) +
  
  scale_color_discrete(name="") + guides(color=guide_legend(override.aes=list(fill=NA)))

