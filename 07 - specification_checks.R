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
# C. FULL SPEC CHECK ACROSS FEs 
########################################################################

##### Define flood/drought variables
complete = computePrcpExtremes(complete, 0.10, 0.90, NA)


# include: contemporaneous temp & precip, then distributed lag in flood and drought

# Formulas
cym = as.formula(paste0("PfPR2 ~ temp + temp2 + | as.factor(fips) + as.factor(year) + as.factor(state_doy) |0|state"))
formquad = as.formula(paste0("sentiment ~ EVI + EVI2 + ", tempBins, " + ", "ppt + ppt2 | as.factor(fips) + as.factor(year) + as.factor(state_doy) |0|state"))
formbin = as.formula(paste0("sentiment ~ ", EVIBins, " + " , tempBins, " + ", "ppt + ppt2 | as.factor(fips) + as.factor(year) + as.factor(state_doy) |0|state"))
myforms = c(formlin, formquad, formbin)    


# Country, year, month

# Country:year, month


# Country*time+country*time2, month
# Year, country*time+country*time^2, month
# Dummies for interventions, country*time+country*time^2, month
# Region:year, region:month
# Region:year, country*time+country*time^2, month
# Region:year, country*time+country*time^2, region:month


# Run for each window
windows = c(100,200, 365)
modellist = list()
i = 0
for (w in windows) {
  for (m in myforms) {
    i = i+1
    modellist[[i]] = felm(data = data[data$doy<= w,], formula = m)
  }
}

# Combine into a single stargazer plot - limited window models
stargazer(modellist[1:6],
          title="EVI with early year windows", align=TRUE, column.labels = c("doy 0-100","doy 0-100","doy 0-100","doy 0-200","doy 0-200","doy 0-200"),
          out = file.path(RES, "tables", "results_panelFE_early_year_windows.tex"), out.header = FALSE, type = "latex", float=F)


model.a1 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + year | 0 | OBJECTID, data = complete)

model.a2 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + country + year | 0 | OBJECTID, data = complete)

model.a3 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + country + year + month | 0 | OBJECTID, data = complete)

model.a4 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

stargazer(model.a1, 
          model.a2,
          model.a3,
          model.a4)

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
