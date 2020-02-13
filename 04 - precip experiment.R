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

#### (1) ADM-SPECIFIC SHOCKS

data <- data.reset

data %>% 
  filter(as.numeric(year) < 1950 ) %>% 
  group_by(OBJECTID) %>% 
  summarize(ppt.90 = quantile(na.omit(ppt), 0.90)) -> ppt.90

data <- left_join(data, ppt.90)

#### FLOODS

data$flood <- as.numeric(data$ppt > data$ppt.90)

# Lags
data %>% group_by(OBJECTID) %>% 
  mutate(flood.lag = lag(flood, order_by = monthyr),
         flood.lag2 = lag(flood, order_by = monthyr, n=2),
         flood.lag3 = lag(flood, order_by = monthyr, n=3)) -> data

#### DROUGHT

data %>% 
  filter(as.numeric(year) < 1950 ) %>% 
  group_by(OBJECTID) %>% 
  summarize(ppt.10 = quantile(na.omit(ppt), 0.10)) -> ppt.10

data <- left_join(data, ppt.10)

data$drought <- as.numeric(data$ppt < data$ppt.10)

# One month lag
data %>% group_by(OBJECTID) %>% 
  mutate(drought.lag = lag(drought, order_by = monthyr),
         drought.lag2 = lag(drought, order_by = monthyr, n=2),
         drought.lag3 = lag(drought, order_by = monthyr, n=3)) -> data

#### TEST SOME IDEAS ABOUT SHOCKS

complete <- data[complete.cases(data),]
head(complete)

model.adm <- felm(PfPR2 ~ temp + temp2 + 
                        flood + flood.lag + flood.lag2 + flood.lag3 + 
                        drought + drought.lag + drought.lag2 + drought.lag3 | 
                        OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

##################################################

#### (2) ADM-MONTH DEFINED SHOCKS

data <- data.reset

data %>% 
  filter(as.numeric(year) < 1950 ) %>% 
  group_by(OBJECTID, month) %>% 
  summarize(ppt.90 = quantile(na.omit(ppt), 0.90)) -> ppt.90

data <- left_join(data, ppt.90, by=c('OBJECTID'='OBJECTID', 'month'='month'))

#### FLOODS

data$flood <- as.numeric(data$ppt > data$ppt.90)

# Lags
data %>% group_by(OBJECTID) %>% 
  mutate(flood.lag = lag(flood, order_by = monthyr),
         flood.lag2 = lag(flood, order_by = monthyr, n=2),
         flood.lag3 = lag(flood, order_by = monthyr, n=3)) -> data

#### DROUGHT

data %>% 
  filter(as.numeric(year) < 1950 ) %>% 
  group_by(OBJECTID, month) %>% 
  summarize(ppt.10 = quantile(na.omit(ppt), 0.10)) -> ppt.10

data <- left_join(data, ppt.10, by=c('OBJECTID'='OBJECTID', 'month'='month'))

data$drought <- as.numeric(data$ppt < data$ppt.10)

# One month lag
data %>% group_by(OBJECTID) %>% 
  mutate(drought.lag = lag(drought, order_by = monthyr),
         drought.lag2 = lag(drought, order_by = monthyr, n=2),
         drought.lag3 = lag(drought, order_by = monthyr, n=3)) -> data

#### TEST SOME IDEAS ABOUT SHOCKS

complete <- data[complete.cases(data),]
head(complete)

model.adm_month <- felm(PfPR2 ~ temp + temp2 + 
                        flood + flood.lag + flood.lag2 + flood.lag3 + 
                        drought + drought.lag + drought.lag2 + drought.lag3 | 
                        OBJECTID + country:year + month | 0 | OBJECTID, data = complete)


##################################################

#### (3) MONTH-SPECIFIC SHOCKS

data <- data.reset

data %>% 
  filter(as.numeric(year) < 1950 ) %>% 
  group_by(month) %>% 
  summarize(ppt.90 = quantile(na.omit(ppt), 0.90)) -> ppt.90

data <- left_join(data, ppt.90)

#### FLOODS

data$flood <- as.numeric(data$ppt > data$ppt.90)

# Lags
data %>% group_by(OBJECTID) %>% 
  mutate(flood.lag = lag(flood, order_by = monthyr),
         flood.lag2 = lag(flood, order_by = monthyr, n=2),
         flood.lag3 = lag(flood, order_by = monthyr, n=3)) -> data

#### DROUGHT

data %>% 
  filter(as.numeric(year) < 1950 ) %>% 
  group_by(month) %>% 
  summarize(ppt.10 = quantile(na.omit(ppt), 0.10)) -> ppt.10

data <- left_join(data, ppt.10)

data$drought <- as.numeric(data$ppt < data$ppt.10)

# One month lag
data %>% group_by(OBJECTID) %>% 
  mutate(drought.lag = lag(drought, order_by = monthyr),
         drought.lag2 = lag(drought, order_by = monthyr, n=2),
         drought.lag3 = lag(drought, order_by = monthyr, n=3)) -> data

#### TEST SOME IDEAS ABOUT SHOCKS

complete <- data[complete.cases(data),]
head(complete)

model.month <- felm(PfPR2 ~ temp + temp2 + 
                        flood + flood.lag + flood.lag2 + flood.lag3 + 
                        drought + drought.lag + drought.lag2 + drought.lag3 | 
                        OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

#### COMPARE MODELS

summary(model.month)
summary(model.adm)
summary(model.adm_month)

library(stargazer)
stargazer(model.month, model.adm, model.adm_month)
