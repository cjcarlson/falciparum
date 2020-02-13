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

complete <- data.reset[complete.cases(data.reset),]

#### A: BUILDING OUT THE FIXED EFFECTS

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


#### B: ROBUSTNESS CHECKS F.E.'S

model.b1 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                 OBJECTID + country:monthyr + month | 0 | OBJECTID, data = complete)

model.b2 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + country:(monthyr^2) + month | 0 | OBJECTID, data = complete)

model.b3 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + country:year + country:month | 0 | OBJECTID, data = complete)

stargazer(model.b1,
          model.b2, 
          model.b3)

#### C. HETEROGENEITY

data <- data.reset

data %>% group_by(OBJECTID) %>% summarize(temp.mean = mean(temp),
                                          ppt.mean = mean(ppt)) %>% 
  left_join(data, .) -> data

complete <- data[complete.cases(data),]
head(complete)

model.c1 <- felm(PfPR2 ~ temp + temp2 + temp:temp.mean + temp2:temp.mean | 
                    OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

model.c2 <- felm(PfPR2 ~ ppt + ppt2 + ppt:ppt.mean + ppt2:ppt.mean | 
                     OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

model.c3 <- felm(PfPR2 ~ temp:temp.mean + temp2:temp.mean + ppt:ppt.mean + ppt2:ppt.mean | 
                     OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

model.c4 <- felm(PfPR2 ~ temp + temp2 + temp:temp.mean + temp2:temp.mean + 
                     ppt + ppt2 + ppt:ppt.mean + ppt2:ppt.mean | 
                     OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

stargazer(model.c1,
          model.c2,
          model.c3,
          model.c4)

model.c5 <- felm(PfPR2 ~ temp + temp2 + temp:ppt.mean + temp2:ppt.mean | 
                   OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

model.c6 <- felm(PfPR2 ~ ppt + ppt2 + ppt:temp.mean + ppt2:temp.mean | 
                   OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

model.c7 <- felm(PfPR2 ~ temp:ppt.mean + temp2:ppt.mean + ppt:temp.mean + ppt2:temp.mean | 
                   OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

model.c8 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 + 
                   temp:ppt.mean + temp2:ppt.mean + ppt:temp.mean + ppt2:temp.mean | 
                   OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

stargazer(model.c5,
          model.c6,
          model.c7,
          model.c8)


#### D. DROPPING RBM YEARS


pre.2005 <- (complete %>% filter(as.numeric(as.character(year))<2005))
post.2005 <- (complete %>% filter(as.numeric(as.character(year))>=2005))

model.a4 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

model.d1 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + country:year + month | 0 | OBJECTID, data = pre.2005)

model.d2 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                   OBJECTID + country:year + month | 0 | OBJECTID, data = post.2005)

stargazer(model.a4, model.d1, model.d2)


plotX = cbind(seq(10,40), seq(10,40)^2)


plotX = cbind(seq(10,37), seq(10,37)^2)

g1 <- plotPolynomialResponse(model.a4, "temp", plotX, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "All years", yLim=c(-15,15), showYTitle = T)

g2 <- plotPolynomialResponse(model.d1, "temp", plotX, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "1900-2004", yLim=c(-15,15), showYTitle = T)

g3 <- plotPolynomialResponse(model.d2, "temp", plotX, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = expression(paste(Delta, " % Prevalence", '')), title = "2005-2017", yLim=c(-15,15), showYTitle = T)

library(patchwork)
g1 + g2 + g3 
