############################################################
# This script conducts a variety of robustness checks on the
# main empirical specification linking PfPR2 to drought, 
# flood, and temperature.
############################################################

############################################################
# Set up
############################################################

rm(list = ls())


# packages
library(here)
library(lfe)
library(reshape)
library(stargazer)
library(tidyverse)
library(zoo)
library(lubridate)
library(cowplot)
library(multcomp)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here("Pipeline", "A - Utility functions", "A01 - Utility code for calculations.R"))
source(here::here("Pipeline", "A - Utility functions", "A02 - Utility code for plotting.R"))

# CRUversion = "4.03" # "4.06"
if (CRUversion=="4.03") {
  resdir = file.path(datadir, "Results")
} else if (CRUversion=="4.06") {
  resdir = file.path(datadir, "Results_CRU-TS4-06")
} else {
  print('CRU version not supported! Use 4.03 or 4.06.')
}

############################################################
# Plotting toggles
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tref = 25 #reference temperature - curve gets recentered to 0 here
Tmin = 10 #min T for x axis
Tmax = 40 #max T for x axis

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
source(here::here("Pipeline", "A - Utility functions", "A03 - Prep data for estimation.R"))

########################################################################
# Estimation
########################################################################

# Formulas: all fixed effects (main spec = cXt2intrXm)
cym = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + year + month | 0 | OBJECTID"))
cXt2m = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + country:monthyr + country:monthyr2 | OBJECTID  + month | 0 | OBJECTID"))
cXt2cXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + country:monthyr + country:monthyr2 | OBJECTID + country:month | 0 | OBJECTID"))
cXt2intm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + country:monthyr + country:monthyr2 | OBJECTID  + intervention + month | 0 | OBJECTID"))
cXt2intrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2intcXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + country:month | 0 | OBJECTID"))
rXyrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))
rXycXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:month + as.factor(smllrgn):year | 0 | OBJECTID"))
rXyrXmcXt = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + country:monthyr | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))
myforms = c(cym, cXt2m, cXt2cXm, cXt2intm, cXt2intrXm, cXt2intcXm, rXyrXm, rXycXm, rXyrXmcXt) 
#mycollabs = c("cym", "cXt2m", "cXt2cXm", "cXt2intm", "cXt2intrXm", "cXt2intcXm", "rXyrXm", "rXycXm", "rXyrXmcXt")
mycollabs = c("cnty + yr + mo FEs.", "cnty trd, mo FEs.", "cnty trd, cnty-mo FEs.", "cnty trd, int + mo FEs.", "cnty trd, int + rgn-mo FEs.", "cnty trd, int + cnty-mo FEs.", "rgn-yr + rgn-mo FEs.", "rgn-yr + cnty-mo FEs.", "rgn-yr+rgn-mo FEs., cnty trd")

# Run all models
modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete, formula = m)
}

# Combine into a single stargazer plot 
mynote = "Column specifications: (1) country, year and month FE; (2) country-specific quad. trends and month FE; (3) country-specific quad. trends and country-by-month FE; (4) country-specific quad. trends and intervention year FE; (5) country-specific quad. trends, intervention year FE, GBOD region-by-month FE; (6) country-specific quad. trends with intervention FE and country by month FE; (7) GBOD region-by-year and region-by-month FE; (8) GBOD region-by-year and country-by-month FE; (9) GBOD region-by-year and region-by-month FE with country-specific linear trends."
dir.create(file.path(resdir, "Tables", "sensitivity"), showWarnings = FALSE)
stargazer(modellist,
          title="Quadratic temperature: FE sensitivity", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought", "intervention"),
          out = file.path(resdir, "Tables", "sensitivity","FixedEffects_sensitivity.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, digits=2,notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

########################################################################
# Plot temperature response functions for all fixed effects specifications
########################################################################

# Plot temperature response for each model
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)

figList = list()
for(m in 1:length(modellist)) {
  # get max of response function
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  figList[[m]] =  plotPolynomialResponse(modellist[[m]], "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                         yLab = "Prevalence (%)", title = mycollabs[m], yLim=c(-30,10), showYTitle = T) +
    theme(plot.title = element_text(size = 10))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], nrow=3)
p
dir.create(file.path(resdir, "Figures", "Diagnostics","Fixed_effects"), showWarnings = FALSE)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Fixed_effects", "panelFE_FE_sensitivity.pdf"), plot = p, width = 7, height = 7)

########################################################################
# Assessing temporal controls: At what spatial scale do we need to address long-run trends?
########################################################################

complete$datestr = paste0(as.character(complete$year), '-', as.character(complete$month), '-15')
complete$datevar = ymd(complete$datestr)

clist = unique(complete$country)
complete$yhat = NA

# 1. Show that temporal trends in PfPR2 vary by Global Burden of Disease region, suggesting at least 
# some spatially varying temporal controls are merited

rlist = unique(complete$smllrgn)
complete$yhat = NA

for(r in 1:length(rlist)) {
  mydf = subset(complete, smllrgn==rlist[r])
  mydf$monthyr2 = mydf$monthyr^2
  mod = lm(PfPR2 ~ monthyr+ monthyr2, data = mydf)
  complete$yhat[complete$smllrgn==rlist[r]] = predict(mod, newdata = mydf)
}

g = ggplot(data = complete, aes(x=datevar, PfPR2)) +
  geom_point(color="cadetblue4", size = 1) + theme_classic() +
  geom_line(aes(y = yhat), color = "red", size = 1) +
  labs(y = "PfPR2", x = "Date") + 
  facet_wrap(~ smllrgn)

fn = file.path(resdir, 'Figures', 'Diagnostics', 'Fixed_effects','region_quad_trends.png')
ggsave(filename = fn, plot = g, height = 6, width = 8)

# 2. Show that trends appear a) nonlinear; and b) heterogeneous by country within GBOD regions,
# suggesting country specific quadratic trends are preferred
clist = unique(complete$country)
complete$yhat = NA

for(c in 1:length(clist)) {
  mydf = subset(complete, country==clist[c])
  mydf$monthyr2 = mydf$monthyr^2
  mod = lm(PfPR2 ~ monthyr+ monthyr2, data = mydf)
  complete$yhat[complete$country==clist[c]] = predict(mod, newdata = mydf)
}

figList = list()
for(r in 1:length(rlist)){
  figList[[r]] = ggplot(data = subset(complete, smllrgn==rlist[r]), aes(x=datevar, PfPR2)) +
    geom_point(color="cadetblue4", size = 1) + theme_classic() +
    geom_line(aes(y = yhat), color = "red", size = 1) +
    labs(y = "PfPR2", x = "") + 
    facet_wrap(~ country)
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], figList[[4]], nrow = 2, 
              labels=c(as.character(rlist[1]), as.character(rlist[2]), as.character(rlist[3]), as.character(rlist[4])),
              label_size = 8, vjust = .5)
fn = file.path(resdir, 'Figures', 'Diagnostics', 'Fixed_effects','country_quad_trends_by_GBOD_region.pdf')
ggsave(filename = fn, plot = p, height = 10, width = 10)

# 3. Ensure we have sufficient data to identify GBOD region X year FEs,
# region X month FEs, country X month FEs, but not country X year FEs (too little coverage here).
# This implies country trends are likely more reliable to capture within region heterogeneity
# in trends, since we are underpowered to estimate country-month FEs.
regcounts = complete %>% group_by(smllrgn) %>% tally()
summary(regcounts$n) # on average, we've got >2400 observations per GBOD region over the whole sample
regyrcounts = complete %>% group_by(smllrgn,year) %>% tally()
summary(regyrcounts$n) # on average, we've got 27 observations per GBOD region per year to identify regionXyear FEs
regmocounts = complete %>% group_by(smllrgn,month) %>% tally()
summary(regmocounts$n) # on average, we've got 206 observations per GBOD region per month to identify regionXmonth FEs
isomocounts = complete %>% group_by(country,month) %>% tally()
summary(isomocounts$n) # on average, we've got 20 observations per country per month to identify countryXmonth FEs (not great...)
isoyrcounts = complete %>% group_by(country,year) %>% tally() 
summary(isoyrcounts$n) # on average, we've got just 6 observations per country per year to identify countryXyear FEs (highly insufficient)

########################################################################
# Assessing temporal controls: At what spatial scale do we need to address seasonality?
########################################################################

# Show that seasonality looks different by region
rlist = unique(complete$smllrgn)
molist = unique(complete$month)

avgbyregionmo = complete %>% group_by(smllrgn, month) %>% summarize(ymn = mean(PfPR2))
toplot = left_join(complete, avgbyregionmo, by = c("smllrgn", "month"))
toplot = toplot %>% mutate(monthnum = month(datevar))

# quick plot by GBOD region over time 
g = ggplot(data=toplot, aes(x=monthnum, y=ymn)) + theme_classic() + 
  geom_point(color="cadetblue4", size = 1) + facet_wrap(~smllrgn) + 
  labs(y = "PfPR2", x = "Month") 
g

fn = file.path(resdir, 'Figures', 'Diagnostics', 'Fixed_effects','region_seasonality.png')
ggsave(filename = fn, plot = g, height = 6, width = 8)

########################################################################
# Ensure residuals are normally distributed and uncorrelated over time
########################################################################

# plot residuals from main specifications (over time and histogram)
main = felm(data=complete, formula = cXt2intrXm)
complete$residuals = main$residuals

# residuals histogram
g = ggplot(data = complete) + geom_histogram(aes(residuals)) + theme_classic()
fn = file.path(resdir, 'Figures', 'Diagnostics', 'Main_model','residuals_cXt2intrXm.png')
ggsave(filename = fn, plot = g)

# residuals over time
g = ggplot(data=complete,aes(x=datevar, y=residuals)) + geom_point(size = 1, alpha=.3) + geom_hline(yintercept = 0, size = .5, color="red") +
  stat_smooth(method = "loess", formula = y ~ x, size = 1) +
  theme_classic()
g
fn = file.path(resdir, 'Figures', 'Diagnostics', 'Main_model','residuals_cXt2intrXm_overtime.png')
ggsave(filename = fn, plot = g)
summary(lm(residuals~datevar,data=complete)) #uncorrelated with time

# residuals over time by region
g = ggplot(data=complete,aes(x=datevar, y=residuals)) +  geom_point(size = .5, alpha=.3) + geom_hline(yintercept = 0, size = .5, color="red") +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = .5) + theme_classic() + facet_wrap(~smllrgn) 
g
fn = file.path(resdir, 'Figures', 'Diagnostics', 'Main_model','residuals_cXt2intrXm_overtime_by_region.png')
ggsave(filename = fn, plot = g)

########################################################################
# Lags and leads of temperature: Dynamic effects
########################################################################

# Temp lags and leads (leave drought/flood as in main specification)
data.reset = data.reset %>% arrange(OBJECTID, monthyr)
templags = data.reset %>% group_by(OBJECTID) %>% 
  mutate(temp.lag = lag(temp, order_by = monthyr),
         temp.lag2 = lag(temp, order_by = monthyr, n=2),
         temp.lag3 = lag(temp, order_by = monthyr, n=3),
         temp2.lag = lag(temp2, order_by = monthyr),
         temp2.lag2 = lag(temp2, order_by = monthyr, n=2),
         temp2.lag3 = lag(temp2, order_by = monthyr, n=3),
         temp.lead = lead(temp, order_by = monthyr),
         temp.lead2 = lead(temp, order_by = monthyr, n=2),
         temp.lead3 = lead(temp, order_by = monthyr, n=3),
         temp2.lead = lead(temp2, order_by = monthyr),
         temp2.lead2 = lead(temp2, order_by = monthyr, n=2),
         temp2.lead3 = lead(temp2, order_by = monthyr, n=3)) 

# merge back into main dataset 
tokeep = c("OBJECTID", "monthyr", "month", "year")
templags = templags %>% dplyr::select(tokeep, contains("lag"), contains("lead"))
complete <- left_join(complete, templags, by=c("OBJECTID", "monthyr", "month", "year"))
complete$month=as.factor(complete$month)

# Formulas
cont = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
lg1 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag +", 
                        floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
lg2 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lag2 + temp2.lag2 +", 
                        floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
lg3 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lag2 + temp2.lag2 + temp.lag3 + temp2.lag3 +", 
                        floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
ld1lg1 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lead + temp2.lead +", 
                           floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
ld2lg2 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lead + temp2.lead +  temp.lag2 + temp2.lag2 + temp.lead2 + temp2.lead2 + ", 
                           floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
ld3lg3 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lead + temp2.lead +  temp.lag2 + temp2.lag2 + temp.lead2 + temp2.lead2 + temp.lag3 + temp2.lag3 + temp.lead3 + temp2.lead3 +", 
                           floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
ld1lg3 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lead + temp2.lead +  temp.lag2 + temp2.lag2 + temp.lag3 + temp2.lag3 +", 
                           floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"))
myforms = c(cont, lg1, lg2, lg3, ld1lg1, ld2lg2, ld3lg3, ld1lg3) 
mycollabs = c("cont", "lg1", "lg2", "lg3", "ld1lg1", "ld2lg2", "ld3lg3", "ld1lg3")

# Run all models
modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete, formula = m)
}

# Combine into a single stargazer plot 
dir.create(file.path(resdir,"Tables", "sensitivity"), showWarnings = FALSE)
stargazer(modellist,
          title="Quadratic temperature: Leads and lags", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought"),
          out = file.path(resdir, "Tables", "sensitivity", "panelFE_leads_lags.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F)

# Plot main model with SEs
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)
c = plotPolynomialResponse(modellist[[1]], "temp", plotXtemp, polyOrder = 2, plotmax=T, cluster = T, xRef = Tref, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                           yLab = "Prevalence (%)", title = "contemp.", yLim=c(-30,5), showYTitle = T)

# Plot with one lag
p1 = plotPolynomialResponse(modellist[[2]], "temp", plotXtemp, polyOrder = 2, lag=1, plotmax=F, cluster = T, xRef = Tref, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                           yLab = "Prevalence (%)", title = "cumulative (1 mo.)", yLim=c(-30,5), showYTitle = T)+
  geom_vline(mapping = aes(xintercept=25), linetype = "solid", colour = "grey39") +
  annotate(geom="text", x=28, y=5, label=paste0("25 C"),
           color="grey39")

# Plot with two lags
p2 = plotPolynomialResponse(modellist[[3]], "temp", plotXtemp, polyOrder = 2, lag=2, plotmax=F, cluster = T, xRef = Tref, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                            yLab = "Prevalence (%)", title = "cumulative (2 mo.)", yLim=c(-30,5), showYTitle = T)+
  geom_vline(mapping = aes(xintercept=25), linetype = "solid", colour = "grey39") +
  annotate(geom="text", x=28, y=5, label=paste0("25 C"),
           color="grey39")

# Plot with three lags
p3 = plotPolynomialResponse(modellist[[4]], "temp", plotXtemp, polyOrder = 2, lag=3, plotmax=T, cluster = F, xRef = Tref, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                            yLab = "Prevalence (%)", title = "cumulative (3 mo.)", yLim=c(-30,5), showYTitle = T)+
  geom_vline(mapping = aes(xintercept=25), linetype = "solid", colour = "grey39") +
  annotate(geom="text", x=28, y=5, label=paste0("25 C"),
           color="grey39")

# Combine figs
p = plot_grid(c, p1, p2, p3, nrow=1)
p

dir.create(file.path(resdir, "Figures", "Diagnostics", "Temp_lags"), showWarnings = FALSE)
save_plot(file.path(resdir, "Figures", "Diagnostics", "Temp_lags", "templags_cumulative_effects.pdf"), p, ncol = 1, base_asp = 4)

########################################################################
# Sensitivity to definitions of drought and flood
########################################################################

# Loop over drought/flood function
dlist = c(0.01,0.05,0.1,0.15,0.2)
flist = c(0.85,0.9,0.95)

modellist = list()
modellabs = list()
i = 0
for (dd in dlist) {
  for (ff in flist) {
    i = i+1
    
    # compute drought and flood variables
    dropcols = grep("flood|drought|ppt_pctile", colnames(complete), value=TRUE)  
    newdf = computePrcpExtremes(dfclimate = data.reset, dfoutcome = complete[,!(names(complete) %in% dropcols)], pctdrought = dd, pctflood = ff, yearcutoff = NA)
    newdf = newdf %>% dplyr::arrange(OBJECTID, monthyr)
    newdf$month = as.factor(newdf$month)
    
    # list of variables indicating drought and flood
    floodvars = paste(colnames(complete)[grep("flood", colnames(complete))], collapse = " + ")
    droughtvars = paste(colnames(complete)[grep("drought", colnames(complete))], collapse = " + ")
    
    # regression formula (main spec)
    mymod = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
    
    # run regression, store results
    modellist[[i]] = felm(data = newdf, formula = mymod)
    modellabs[[i]] = paste0("drought:",dd," flood:",ff)
    
    print(paste0('----------- Regression run for drought pctile ', dd, ' and flood pctile ', ff, ' -----------'))
    rm(newdf)
  }
}

######## For each model, plot temperature response ########
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)
figList = list()
for(m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  figList[[m]] =  plotPolynomialResponse(modellist[[m]], "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = Tref, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                         yLab = "Prevalence (%)", title = modellabs[m], yLim=c(-30,5), showYTitle = T) +
    theme(plot.title = element_text(size = 10))
}


p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], 
              figList[[10]], figList[[11]], figList[[12]], 
              figList[[13]], figList[[14]], figList[[15]], nrow=5)
p

dir.create(file.path(resdir, "Figures", "Diagnostics", "Drought_flood_defn"), showWarnings = FALSE)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Drought_flood_defn", "temp_responses_drought_flood_sensitivity.pdf"), plot = p, width = 7, height = 10)

######## For each model, plot drought and flood coeffs ########

# All drought figures
figList = list()
for(m in 1:length(modellist)) {
  figList[[m]] =  plotLinearLags(modellist[[m]], "drought", cluster = T, laglength = 3, xLab="Monthly lag", "Coefficient", title = modellabs[[m]], yLim = c(-6,4))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], 
              figList[[10]], figList[[11]], figList[[12]], 
              figList[[13]], figList[[14]], figList[[15]], nrow=5)
p

ggsave(file.path(resdir, "Figures", "Diagnostics", "Drought_flood_defn", "drought_responses_sensitivity.pdf"), plot = p, width = 7, height = 10)

# All flood figures
figList = list()
for(m in 1:length(modellist)) {
  figList[[m]] =  plotLinearLags(modellist[[m]], "flood", cluster = T, laglength = 3, xLab="Monthly lag", "Coefficient", title = modellabs[[m]], yLim = c(-4,4))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], 
              figList[[10]], figList[[11]], figList[[12]], 
              figList[[13]], figList[[14]], figList[[15]], nrow=5)
p

ggsave(file.path(resdir, "Figures", "Diagnostics", "Drought_flood_defn", "flood_responses_sensitivity.pdf"), plot = p, width = 7, height = 10)

########################################################################
# Temperature functional form 
########################################################################

# estimate polynomial orders up to 5
modellist = list()
modellist[[1]] = felm(data = complete, formula = cXt2intrXm)
modellist[[2]] = felm(data = complete, formula = 
                        as.formula(paste0("PfPR2 ~ temp + temp2 + temp3 +", floodvars,
                                          " + ", droughtvars, 
                                          " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID")))
modellist[[3]] = felm(data = complete, formula = 
                        as.formula(paste0("PfPR2 ~ temp + temp2 + temp3 + temp4 +", floodvars,
                                          " + ", droughtvars, 
                                          " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID")))
modellist[[4]] = felm(data = complete, formula = 
                        as.formula(paste0("PfPR2 ~ temp + temp2 + temp3 + temp4 + temp5 +", floodvars,
                                          " + ", droughtvars, 
                                          " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID")))
# plot
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2, seq(Tmin,Tmax)^3,seq(Tmin,Tmax)^4,seq(Tmin,Tmax)^5)
modellabs = c("Quadratic", "Cubic", "Quartic", "Quintic")

# get ref T for quadratic model
coefs = summary(modellist[[1]])$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)

figList = list()
for(m in 1:length(modellist)) {
  end = m+1
  figList[[m]] =  plotPolynomialResponse(modellist[[m]], "temp", plotXtemp[,1:end], polyOrder = end, plotmax = F, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                         yLab = "Prevalence (%)", title = modellabs[m], yLim=c(-30,5), showYTitle = T) +
        geom_vline(mapping = aes(xintercept=myrefT), linetype = "solid", colour = "grey39") +
        annotate(geom="text", x=myrefT+3, y=5, label=paste0(myrefT," C"), color="grey39")
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], nrow=2)
p

dir.create(file.path(resdir, "Figures", "Diagnostics", "Temp_functionalForm"), showWarnings = FALSE)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Temp_functionalForm", "temperature_poly_order.pdf"), plot = p, width = 10, height = 10)

########################################################################
# Cumulative precipitation
########################################################################

# estimate polynomial orders up to 5
modellist = list()
modellist[[1]] = felm(data = complete, formula = 
                        as.formula(paste0("PfPR2 ~ temp + temp2 + ppt + ppt2 + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID")))
modellist[[2]] = felm(data = complete, formula = 
                        as.formula(paste0("PfPR2 ~ temp + temp2 + ppt + ppt2 + ppt3 + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID")))
modellist[[3]] = felm(data = complete, formula = 
                        as.formula(paste0("PfPR2 ~ temp + temp2 + ppt + ppt2 + ppt3 + ppt4 + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID")))
modellist[[4]] = felm(data = complete, formula = 
                        as.formula(paste0("PfPR2 ~ temp + temp2 + ppt + ppt2 + ppt3 + ppt4 + ppt5 + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID")))
# plot
Tmin = 0
Tmax = 600
plotXprcp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2, seq(Tmin,Tmax)^3,seq(Tmin,Tmax)^4,seq(Tmin,Tmax)^5)
modellabs = c("Quadratic", "Cubic", "Quartic", "Quintic")

# get ref P
myrefP = 0

figList = list()
for(m in 1:length(modellist)) {
  end = m+1
  figList[[m]] =  plotPolynomialResponse(modellist[[m]], "ppt", plotXprcp[,1:end], polyOrder = end, plotmax = F, cluster = T, xRef = myrefP, fillcolor= "#43A7BA", xLab = "Total precipitation (mm)", 
                                         yLab = "Prevalence (%)", title = modellabs[m], yLim=c(-10,5), showYTitle = T) 
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], nrow=2)
p

ggsave(file.path(resdir, "Figures", "Diagnostics", "Drought_flood_defn", "precipitation_poly_order.pdf"), plot = p, width = 10, height = 10)

