############################################################
# This script estimates the main empirical specification linking
# PfPR2 to drought, flood, and temperature.
############################################################

############################################################
# Set up
############################################################

rm(list = ls())

user = "Tamma" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/' #location for data and output
  repo = 'C:/Users/cjcar/Documents/Github/falciparum' #location for cloned repo
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else {
  wd = NA
  print('Script not configured for this user!')
}

setwd(wd)

# source functions for easy plotting and estimation
source(file.path(repo,'Pipeline/A - Utility functions/A01 - Utility code for calculations.R'))
source(file.path(repo,'Pipeline/A - Utility functions/A02 - Utility code for plotting.R'))

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
library(multcomp)
library(dplyr)

############################################################
# Plotting toggles
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tref = 24 #reference temperature - curve gets recentered to 0 here
Tmin = 10 #min T for x axis
Tmax = 40 #max T for x axis

########################################################################
# Data clean up
########################################################################

#### Climate data
data <- read.csv('./Data/CRU-Reextraction-Aug2022.csv')

#### Spatial data
spatial <- read.csv('./Dataframe backups/shapefile-backup.csv')
countrydf <- unique(spatial[,c('OBJECTID','NAME_0')])
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
iso = data %>% group_by(country, month, year) %>% summarize_all(mean, na.rm=T)
data_iso <- iso[complete.cases(iso),]

#### Dates & times
data$yearnum <- data$year
data$year <- factor(data$year)
data %>% unite("monthyr", month:year, sep=' ', remove=FALSE) %>% 
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>% 
  mutate(monthyr = as.numeric(ymd(monthyr)-ymd("1900-01-01"))) -> data.reset

### Store complete records only
complete <- data.reset[complete.cases(data.reset),]

########################################################################
# Define Global Burden of Disease regions 
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
# Define covariates:
# a) Drought/flood events
# b) Malaria intervention periods
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

########################################################################
# Estimation
########################################################################

# classes: important for ensuring felm is treating these correctly
complete$month = as.factor(complete$month)
complete$year = as.factor(complete$year)

# Formula (see other files for robustness/sensitivity checks)
cXt2intrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

# Estimation & save model results
mainmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)
bfn = file.path(wd,"Results", "Models", "reproducibility", "coefficients_cXt2intrXm.rds")
vfn = file.path(wd,"Results", "Models", "reproducibility", "vcv_cXt2intrXm.rds")
saveRDS(coeffs, file=bfn)
saveRDS(vcov, file=vfn)

# Stargazer output
mynote = "Country-specific quad. trends with intervention FE and country by month FE."
stargazer(mainmod,
          title="PfPR2 response to daily avg. temperature", align=TRUE, 
          keep = c("temp", "flood", "drought"),
          out = file.path(wd, "Results", "Tables", "main", "main_specification_cXt2intrXm.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

########################################################################
# Plot (Note: analogous to Fig 2A but with analytically derived confidence intervals 
# in place of bootstrap runs shown in Fig 2A)
########################################################################

# Temperature support
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)

coefs = summary(mainmod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
fig =  plotPolynomialResponse(mainmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = "Monthly avg. T [C]", 
                                         yLab = expression(paste(Delta, " % Prevalence", '')), title = "Main spec: cXt2intrXm", yLim=c(-30,5), showYTitle = T)

fig
ggsave(file.path(wd, "Results", "Figures", "Diagnostics", "Main_model", "temp_response_cXt2intrXm.pdf"), plot = fig, width = 7, height = 7)

