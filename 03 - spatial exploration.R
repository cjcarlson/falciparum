
### SETUP
rm(list = ls())

library(ggplot2)
#library(ggthemr)
library(lfe)
library(reshape)
library(zoo)
library(lubridate)
library(tidyverse)
library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(velox)

user = "Colin" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/'
  repo = 'C:/Users/cjcar/Documents/Github/falciparum'
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/Data'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else {
  wd = NA
  print('Script not configured for this user!')
}

setwd(wd)

# Read in the data backup
data <- read.csv('./Dataframe backups/formatted-backup.csv')

data$PfPR2 <- data$PfPR2/100

complete <- data[complete.cases(data),]

# Add country names in
spatial <- read.csv('./Dataframe backups/shapefile-backup.csv')
countrydf <- unique(spatial[,c('OBJECTID','NAME_0')])
complete$country <- countrydf$NAME_0[sapply(complete$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]

# country avg df (for robustness -- note: to do this right, we need area weights for each OBJECTID!)
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
iso = data %>% group_by(country, month, year) %>% summarize_all(mean, na.rm=T)
complete_iso <- iso[complete.cases(iso),]

# Year as factor instead of integer
complete$year <- factor(complete$year)

complete %>% unite("monthyr", month:year, sep=' ', remove=FALSE) %>% 
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>% 
  mutate(monthyr = as.numeric(ymd(monthyr)-ymd("1900-01-01"))) -> complete


# Test model with all specified components

full.model <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                     OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

summary(full.model)

fe <- getfe(full.model)
View(fe)

fe %>% filter(fe=='OBJECTID') -> fe.spatial

####################
# Read in Africa
#setwd('wd')
cont <- readOGR('./Data/AfricaADM1.shp')

cont@data <- left_join(cont@data, fe.spatial, c('OBJECTID'='idx'))

# output a plot using ggplot
p1 <- ggplot() + 
  theme_void() + 
  geom_sf(aes(fill=effect), data = st_as_sf(cont), color=NA) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                       high="red", space ="Lab")
#PLOT
# spplot(cont, 'effect')

View(data)

# Predict based on full.model what the expected prevalence is at each time-space point

predict.lfe <- function(model,temp,temp2,ppt,ppt2) {
  return(
    model$coefficients['temp',]*temp + 
    model$coefficients['temp2',]*temp2 + 
    model$coefficients['ppt',]*ppt + 
    model$coefficients['ppt2',]*ppt2
  )
}

data$pred.Pf <- predict.lfe(full.model, 
            data$temp,
            data$temp2,
            data$ppt,
            data$ppt2)

data %>% filter(year %in% c(1900:1905)) %>% 
  group_by(OBJECTID) %>% 
  summarize(pred.Pf1900 = mean(pred.Pf)) %>%
  mutate(OBJECTID = factor(OBJECTID)) -> Pf.1900s

data %>% filter(year %in% c(2010:2015)) %>% 
  group_by(OBJECTID) %>% 
  summarize(pred.Pf2000 = mean(pred.Pf)) %>%
  mutate(OBJECTID = factor(OBJECTID)) -> Pf.2000s

cont@data <- left_join(cont@data, Pf.1900s)
cont@data <- left_join(cont@data, Pf.2000s)

cont@data$deltaPf <- cont$pred.Pf2000 - cont$pred.Pf1900

###################################

library(sf)

# output a plot using ggplot
p1 <- ggplot() + 
  theme_void() + 
  geom_sf(aes(fill=deltaPf), data = st_as_sf(cont), color=NA) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                        high="red", space ="Lab")
