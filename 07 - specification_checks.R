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
# B. VISUALIZING SECULAR TRENDS
########################################################################

complete$datestr = paste0(as.character(complete$year), '-', as.character(complete$month), '-15')
complete$datevar = ymd(complete$datestr)

clist = unique(complete$country)
complete$yhat = NA

for(c in 1:length(clist)) {
  mydf = subset(complete, country==clist[c])
  mydf$monthyr2 = mydf$monthyr^2
  mod = lm(PfPR2 ~ monthyr+ monthyr2, data = mydf)
  complete$yhat[complete$country==clist[c]] = predict(mod, newdata = mydf)
}

# quick plot by country over time 
g = ggplot(data = complete, aes(x=datevar, PfPR2)) +
  geom_point(color="cadetblue4", size = 1) + theme_classic() +
  geom_line(aes(y = yhat), color = "red", size = 1) +
  labs(y = "PfPR2", x = "Date") + 
  facet_wrap(~ country)
g

# save
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Fixed_effects/country_quad_trends.png')
ggsave(filename = fn, plot = g, height = 6, width = 8)


########################################################################
# DEFINE GBOD REGIONS (use for regionXtime FE)
########################################################################

gbod <- readOGR(file.path(wd, "Data", "OriginalGBD", "WorldRegions.shp"))
head(gbod@data)

gboddf = as.data.frame(gbod@data)
gboddf = gboddf %>% select("ISO", "NAME_0", "Region", "SmllRgn")
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
complete$intervention = ifelse(complete$yearnum>=2004 & complete$yearnum<=2015, 2, 0)
complete$intervention = as.factor(complete$intervention)

# Formulas
cym = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + year + month | 0 | OBJECTID"))
cXym = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:year + month | 0 | OBJECTID"))
cXycXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:year + country:month | 0 | OBJECTID"))
cXt2m = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:monthyr + country:monthyr2 + month | 0 | OBJECTID"))
#cXt2ym = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:monthyr + country:monthyr2 + year + month | 0 | OBJECTID"))
cXt2intm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:monthyr + country:monthyr2 + intervention + month | 0 | OBJECTID"))
rXyrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + smllrgn:month + smllrgn:year | 0 | OBJECTID"))
myforms = c(cym, cXym, cXycXm, cXt2m, cXt2intm, rXyrXm)    

# region:year, month
# region: year, region:month

# Run all models
modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete, formula = m)
}

# Combine into a single stargazer plot 
stargazer(modellist,
          title="Quadratic temperature: FE sensitivity", align=TRUE, column.labels = c("cym","cXym","cXycXm","cXt2m",  "cXt2intm", "rXyrXm"),
          out = file.path(wd, "Results", "Tables", "panelFE_FE_sensitivity.tex"), out.header = FALSE, type = "latex", float=F)

# Plot temperature response for each model

plotXtemp = cbind(seq(0,37), seq(0,37)^2)

g1 <- plotPolynomialResponse(model.a1, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "Model 1", yLim=c(-15,15), showYTitle = T)
g2 <- plotPolynomialResponse(model.a2, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "Model 2", yLim=c(-15,15), showYTitle = T)
g3 <- plotPolynomialResponse(model.a3, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "Model 3", yLim=c(-15,15), showYTitle = T)
g4 <- plotPolynomialResponse(model.a4, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "Model 4", yLim=c(-15,15), showYTitle = T)

plotXppt = cbind(seq(0,800), seq(0,800)^2)

g5 <- plotPolynomialResponse(model.a1, "ppt", plotXppt, polyOrder = 2, cluster = T, xRef = 0, xLab = "Monthly precip. (mm)", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "", yLim=c(-15,15), showYTitle = F)
g6 <- plotPolynomialResponse(model.a2, "ppt", plotXppt, polyOrder = 2, cluster = T, xRef = 0, xLab = "Monthly precip. (mm)", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "", yLim=c(-15,15), showYTitle = F)
g7 <- plotPolynomialResponse(model.a3, "ppt", plotXppt, polyOrder = 2, cluster = T, xRef = 0, xLab = "Monthly precip. (mm)", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "", yLim=c(-15,15), showYTitle = F)
g8 <- plotPolynomialResponse(model.a4, "ppt", plotXppt, polyOrder = 2, cluster = T, xRef = 0, xLab = "Monthly precip. (mm)", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "", yLim=c(-15,15), showYTitle = F)
(g1 | g5) / (g2 | g6) / (g3 | g7) / (g4 | g8)


########################################################################
# D. RESIDUALS 
########################################################################

# plot residuals from main specifications (over time and histogram)

########################################################################
# E. F-test on country:year interactions 
########################################################################


########################################################################
# F. Lags & leads
########################################################################

# Temp lags and leads (leave drought/flood as is)
