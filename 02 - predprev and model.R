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
#library(cowplot)
library(ggplot2)
#library(ggthemr)
library(lfe)
library(reshape)
library(tidyverse)
library(zoo)
library(lubridate)

############
# Read in the data backup
data <- read.csv('./Dataframe backups/formatted-backup.csv')
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

############################
# Exploring data coverage to ensure sufficient data for identification of common FE we want to use

# 1) how many obs per country-year, on avg? 
isocounts = complete %>% group_by(country) %>% tally()
summary(isocounts$n) # on average, we've got 214 observations per country over the sample
isoyrcounts = complete %>% group_by(country,year) %>% tally()
summary(isoyrcounts$n) # on average, we've got 6 observations per country-year over the sample...but 25% of the data has just one obs per country-year. 
# This suggests we probably want to have the central model use trends instead of country-yr FE.

# 2) how many obs per year and month, on avg?
Yrct = complete %>% group_by(year) %>% tally()
summary(Yrct$n) # on average, we've got 80 observations per year over the sample, but some years have just one
Moct = complete %>% group_by(month) %>% tally()
summary(Moct$n) # on average, we've got 838 observations per calendar month over the sample -- monthly FE are fine, might want to move to region:month FE given differential seasonality


############################

# Test a few smaller models


model1 <- felm(PfPR2 ~ R0 | 
                     OBJECTID + country:year + month | 0 | OBJECTID, data = complete)
summary(model1)

model2 <- felm(PfPR2 ~ temp + temp2 | 
                 OBJECTID + year | 0 | OBJECTID, data = complete)
summary(model2)


model3 <- felm(PfPR2 ~ temp + temp2 | 
                 OBJECTID + year + month | 0 | OBJECTID, data = complete)
summary(model3)

# all climate variables
model4 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                     OBJECTID + country + year + month | 0 | OBJECTID, data = complete)

summary(model4)

# Plotting example for model4:
# 1. Construct the matrix of X values you want to plot over, reflecting your polynomial orders! 
plotX = cbind(seq(10,40), seq(10,40)^2)
# 2. Pass X values to plotting function, with other options reflecting the model run
p = plotPolynomialResponse(model4, "temp", plotX, polyOrder = 2, cluster = T, xRef = 30, xLab = "Monthly avg. T [C]", 
                           yLab = "Prevalence", title = "Country, year, month FE", yLim=c(-20,10), showYTitle = T)
p

# interaction effects, temp and precip
model5 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 + temp:ppt + temp2:ppt | 
                 OBJECTID + country + year + month | 0 | OBJECTID, data = complete)

summary(model5)

# check if relationship holds up at country level
complete_iso$year <- factor(complete_iso$year)
model6 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2  | 
                 country + year + month | 0 | OBJECTID, data = complete_iso)

summary(model6)

# Test model with all specified components

full.model <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

summary(full.model)

####################

# Checks on alternate temporal structures!

full.model <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                     OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

summary(full.model)

#> class(complete$year)
#[1] "factor"
#> class(complete$month)
#[1] "factor"
#> class(complete$monthyr)
#[1] "numeric"

temp1 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
               OBJECTID + country:monthyr + month | 0 | OBJECTID, data = complete)

summary(temp1)

temp2 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                  OBJECTID + country:(monthyr^2) + month | 0 | OBJECTID, data = complete)

summary(temp2)


temp2 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                OBJECTID + country:(monthyr^2) + month | 0 | OBJECTID, data = complete)

summary(temp2)

temp3 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                OBJECTID + year + country:month | 0 | OBJECTID, data = complete)

summary(temp3)

library(patchwork)

plotX = cbind(seq(10,37), seq(10,37)^2)

g1 <- plotPolynomialResponse(full.model, "temp", plotX, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                           yLab = "Prevalence", title = "Country, year, month FE", yLim=c(-15,15), showYTitle = T)

g2 <- plotPolynomialResponse(temp1, "temp", plotX, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = "Prevalence", title = "Country-linear trend", yLim=c(-15,15), showYTitle = T)

g3 <- plotPolynomialResponse(temp2, "temp", plotX, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                             yLab = "Prevalence", title = "Country-quadratic trend", yLim=c(-15,15), showYTitle = T)

g1 + g2 + g3

#####################

plotPolynomialResponse(temp1,
                       patternForPlotVars = 'temp',
                       xVals = complete[,c('temp','temp2')],
                       xLab='Temperature',
                       yLab='Effect',
                       cluster=FALSE)

#####################

# This just makes a percent
complete$Pf3 <- complete$PfPR2/100

# Calculate temperature-R0 curve
x<-seq(10, 40, by=0.2); y<-sapply(x, r0t)
tempdf <- data.frame(temp=x,response=y)

# Predict the curve from the econometric model coefficients
tempfit2 <- function(t) {full.model$coefficients['temp',]*t + full.model$coefficients['temp2',]*t*t}
x<-seq(10, 40, by=0.2); y<-sapply(x, tempfit2)
tempdf2 <- data.frame(temp=x,response=y)

theme_set(theme_classic())  # not run gg

g1 <- ggplot(complete, aes(temp, Pf3)) + #geom_point(col='light grey') + 
  geom_smooth(col='turquoise3', lwd=1.2, fill='paleturquoise2', alpha=0.5) + ylab('P. falciparum prevalence') + 
  xlab('Temperature') +
  theme(text = element_text(size=13)) + 
  labs(title = 'Observed prevalence model', subtitle='African dataset (1900-2010)'); g1

g2 <- ggplot(tempdf2,aes(temp,response)) + geom_line(col='turquoise3', lwd=1.2) + 
  ylab('Prevalence predicted') + xlab('Temperature') + 
  theme(text = element_text(size=13)) +
  labs(title='Full econometric model', subtitle='At ADM1 level (1900-2010)')

g3 <- ggplot(tempdf,aes(temp,response)) + geom_line(col='turquoise3', lwd=1.3) + 
  ylab('Estimated R0') + xlab('Temperature')  + 
  theme(text = element_text(size=13)) + theme_classic()  + 
  labs(title = 'Predicted R0', subtitle='Based on laboratory experiments') 

g4 <- ggplot(complete, aes(R0, Pf3)) + # geom_point(col='light grey') + 
  geom_smooth() + ylab('Observed prevalence of P. falciparum') + 
  xlab('Estimated scaled R0 based on temp.') + theme_classic()  + 
  theme(text = element_text(size=13)) + 
  labs(title = 'Observed prevalence model', subtitle='Using R0 estimator') 

plot_grid(g1, g2, g3, g4)

plot_grid(g3, g1, g2, nrow=1)

##################

# This part does the overlaid curves - the GAMs against the R0 curve

complete$predR0 <- r0t(complete$temp)

ggthemr('pale')
ggplot(data=complete, aes(x=temp), fill=NA) + 
  geom_line(aes(y=predR0, color='Predicted'), lwd=1.1) + 
  geom_smooth(aes(y=Pf3*3, color='Observed'), lwd=1.1, fill='light grey') + 
  xlim(15,35) + ylab('R0 predicted ') + xlab('Temperature') + 
  theme(text = element_text(size=15),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20, r= 0, b = 20, l = 0)),
        plot.title = element_text(margin = margin(t = 20, r= 0, b = 0, l = 0),
                                  size = 20, hjust = 0.5),
        legend.position='top',
        legend.key = element_rect(colour = NA, fill = 'white'))+ 
  labs(title='Observed vs. theoretical temperature sensitivity', subtitle='')  +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Observed prevalence (1900-2010)")) +
  scale_color_discrete(name="") + guides(color=guide_legend(override.aes=list(fill=NA)))


# The map curve may no lonegr exist somehow. I'll rewrite it ASAP.


###################################################################
#getfe(full.model,se=TRUE)  -> effects
#effects[effects$fe=='month',] -> monthly
#monthly$idx <- as.numeric(as.character(monthly$idx))
#monthly %>% ggplot(aes(x=idx, y=effect)) + geom_line() + 
#  geom_ribbon(aes(ymin=effect-se, ymax=effect+se), fill='light blue', alpha=0.35) + 
#  theme_bw() + xlab('Month') + ylab('Fixed effect')

