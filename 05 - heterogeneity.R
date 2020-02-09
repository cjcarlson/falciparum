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
data <- read.csv('./Dataframe backups/formatted-backup.csv')
#### STANDARD FILTERING & AUGMENTING
spatial <- read.csv('./Dataframe backups/shapefile-backup.csv')
countrydf <- unique(spatial[,c('OBJECTID','NAME_0')])
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
iso = data %>% group_by(country, month, year) %>% summarize_all(mean, na.rm=T)
data_iso <- iso[complete.cases(iso),]
data$year <- factor(data$year)
data %>% unite("monthyr", month:year, sep=' ', remove=FALSE) %>% 
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>% 
  mutate(monthyr = as.numeric(ymd(monthyr)-ymd("1900-01-01"))) -> data.reset

#### A FEW DIFFERENT MODEL FORMULATIONS

data <- data.reset

data %>% group_by(OBJECTID) %>% summarize(temp.mean = mean(temp),
                                          ppt.mean = mean(ppt)) %>% 
  left_join(data, .) -> data

complete <- data[complete.cases(data),]
head(complete)

model.het <- felm(PfPR2 ~ temp + temp2 + temp:temp.mean + temp2:temp.mean | 
                          OBJECTID + country:year + month | 0 | OBJECTID, data = complete)
summary(model.het)

model.het2 <- felm(PfPR2 ~ ppt + ppt2 + ppt:ppt.mean + ppt2:ppt.mean | 
                           OBJECTID + country:year + month | 0 | OBJECTID, data = complete)
summary(model.het2)
