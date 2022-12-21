### SET UP
rm(list = ls())

user = "Tamma" #"Colin"
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
library(rgdal)
library(cowplot)

########################################################################
       # A. INITIALIZING
########################################################################

#### Read in the data backup
data <- read.csv('./Dataframe backups/formatted-backup.csv')
#### STANDARD FILTERING & AUGMENTING
spatial <- read.csv('./Dataframe backups/shapefile-backup.csv')
countrydf <- unique(spatial[,c('OBJECTID','NAME_0')])
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
iso = data %>% group_by(country, month, year) %>% summarize_all(mean, na.rm=T)
data_iso <- iso[complete.cases(iso),]
data$yearnum <- data$year
data$year <- factor(data$year)
data %>% unite("monthyr", month:year, sep=' ', remove=FALSE) %>% 
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>% 
  mutate(monthyr = as.numeric(ymd(monthyr)-ymd("1900-01-01"))) -> data.reset

complete <- data.reset[complete.cases(data.reset),]

########################################################################
# B. DEFINE GBOD REGIONS (use for regionXtime FE)
########################################################################

gbod <- readOGR(file.path(wd, "Data", "OriginalGBD", "WorldRegions.shp"))
head(gbod@data)

gboddf = as.data.frame(gbod@data)
gboddf = gboddf %>% dplyr::select("ISO", "NAME_0", "Region", "SmllRgn")
gboddf = gboddf %>% group_by(ISO, NAME_0) %>% summarize(Region = first(Region), SmllRgn = first(SmllRgn)) # note that the small regions are homogenous within country
colnames(gboddf) = c("ISO", "country", "region", "smllrgn")
gboddf$country = as.character(gboddf$country)

# clean to merge
gboddf$country = ifelse(gboddf$country=="Cote D'Ivoire", "Côte d'Ivoire", gboddf$country)

complete$country = as.character(complete$country)
complete = left_join(complete, gboddf, by = "country")
complete$country = as.factor(complete$country)

rm(gbod, gboddf)

########################################################################
# C. FULL SPEC CHECK ACROSS FEs 
########################################################################

##### Define flood/drought variables - need to pass the climate data separately from the merged dataset with the outcome
##### variable because we want to define climate over the whole period
complete = computePrcpExtremes(dfclimate = data.reset, dfoutcome = complete, pctdrought = 0.10, pctflood = 0.90, yearcutoff = NA)
complete = complete %>% arrange(OBJECTID, monthyr)

complete %>% select(OBJECTID, ppt_pctile0.1, ppt_pctile0.9) %>% unique() -> precip.key

write_csv(precip.key, '~/Github/falciparum/precipkey.csv')
