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
library(stargazer)
library(tidyverse)
library(zoo)
library(lubridate)

############
#### Read in the data backup
data <- read.csv('./Data/PolyBackup-CRURaw.csv')
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

complete <- data.reset[complete.cases(data.reset),]


model.5order <- felm(PfPR2 ~ temp + temp2 + temp3 + temp4 + temp5 | 
                           OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

plotXtemp = cbind(seq(0,37), seq(0,37)^2)

g1 <- plotPolynomialResponse(model.full, "temp", plotXtemp, polyOrder = 5, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "Model 1", yLim=c(-15,15), showYTitle = T)
g1
