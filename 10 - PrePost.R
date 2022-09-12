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
data <- read.csv('./Data/CRU-Reextraction-Aug2022.csv')
#### STANDARD FILTERING & AUGMENTING
spatial <- read.csv('./Dataframe backups/shapefile-backup.csv')
countrydf <- unique(spatial[,c('OBJECTID','NAME_0')])
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

# include: contemporaneous temp, then distributed lag in flood and drought
floodvars = paste(colnames(complete)[grep("flood", colnames(complete))], collapse = " + ")
droughtvars = paste(colnames(complete)[grep("drought", colnames(complete))], collapse = " + ")

# need this for country specific quadratic trends
complete$monthyr2 = complete$monthyr^2

# define key intervention periods
complete$intervention = ifelse(complete$yearnum>=1955 & complete$yearnum<=1969, 1, 0)
complete$intervention[complete$yearnum>=2000 & complete$yearnum<=2015] = 2
complete$intervention = as.factor(complete$intervention)

# Formulas
cXt2intrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention)+ country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2rXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2intm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "+ country:monthyr + country:monthyr2 | OBJECTID  + intervention + month | 0 | OBJECTID"))
cXt2m = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:monthyr + country:monthyr2 + month | 0 | OBJECTID"))
rXyrXmcXt = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + country:monthyr | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))

complete$smllrgn <- as.factor(complete$smllrgn)
complete$month <- as.factor(complete$month)
complete %>% filter(as.numeric(as.character(year)) < 2000) -> pre
complete %>% filter(as.numeric(as.character(year)) >= 2000) -> post
complete$year <- as.factor(complete$year)

# Pre-post with simplified model due to splitting the sample (note full sample response is very similar to main model)
modellist = list()
modellist[[1]] =  felm(data = complete, formula = cXt2intrXm) #main spec
modellist[[2]] <- felm(data = complete, formula = rXyrXmcXt) #simplified spec (no quad trends by country, which are very demanding)
modellist[[3]] <- felm(data = pre, formula = rXyrXmcXt) #simplified spec
modellist[[4]] <- felm(data = post, formula = rXyrXmcXt) #simplified spec, no need for intervention dummies bc it's post period only
mycollabs = c("Full (main specification)", "Full (simplified specification)", "1900-1999 (simplified)", "2000-2015 (simplified)")


# Plots: Full model, main spec; full model, simplified spec; pre period, simplified spec; post period, simplified spec
# Plot temperature response for each model
plotXtemp = cbind(seq(10,40), seq(10,40)^2)

figList = list()
for(m in 1:4) {
  figList[[m]] =  plotPolynomialResponse(modellist[[m]], "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = 24, xLab = "Monthly avg. T [C]", 
                                         yLab = expression(paste(Delta, " % Prevalence", '')), title = mycollabs[m], yLim=c(-30,10), showYTitle = T)
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], nrow=2)
p

ggsave(file.path(wd, "Results", "Figures", "Pre_Post", "pre_vs_post_temperature_response.pdf"), plot = p, width = 7, height = 8)

