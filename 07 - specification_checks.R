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
library(multcomp)

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

# include: contemporaneous temp, then distributed lag in flood and drought
floodvars = paste(colnames(complete)[grep("flood", colnames(complete))], collapse = " + ")
droughtvars = paste(colnames(complete)[grep("drought", colnames(complete))], collapse = " + ")

# need this for country specific quadratic trends
complete$monthyr2 = complete$monthyr^2

# define key intervention periods
complete$intervention = ifelse(complete$yearnum>=1955 & complete$yearnum<=1969, 1, 0)
complete$intervention[complete$yearnum>=2000 & complete$yearnum<=2015] = 2
complete$intervention = as.factor(complete$intervention)

# classes: important for ensuring felm is treating these correctly
complete$month = as.factor(complete$month)

# Formulas
cym = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + year + month | 0 | OBJECTID"))
cXym = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:year + month | 0 | OBJECTID"))
cXycXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:year + country:month | 0 | OBJECTID"))
cXt2m = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:monthyr + country:monthyr2 + month | 0 | OBJECTID"))
cXt2cXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:monthyr + country:monthyr2 + country:month | 0 | OBJECTID"))
cXtym = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:monthyr  + year + month | 0 | OBJECTID"))
cXt2intm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:monthyr + country:monthyr2 + intervention + month | 0 | OBJECTID"))
cXt2intrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2intcXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + country:month | 0 | OBJECTID"))
rXyrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))
rXycXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + country:month + as.factor(smllrgn):year | 0 | OBJECTID"))
cXyrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + as.factor(smllrgn):month + country:year | 0 | OBJECTID"))
rXyrXmcXt = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + country*monthyr | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))
#myforms = c(cym, cXym, cXycXm, cXt2m, cXtym, cXt2intm, cXt2intrXm, rXyrXm, rXyrXmcXt)    
myforms = c(cym, cXym, cXycXm, cXt2m, cXt2cXm, cXt2intm, cXt2intrXm, cXt2intcXm, rXyrXm, rXycXm, cXyrXm, rXyrXmcXt) # adjusting this for now because two models are so slow to estimate
mycollabs = c("cym", "cXym", "cXycXm", "cXt2m", "cXt2cXm", "cXt2intm", "cXt2intrXm", "cXt2intcXm", "rXyrXm", "rXycXm", "cXyrXm", "rXyrXmcXt")

# look at last model in detail for a sec (note linear country trends almost all insig!)
summary(felm(data=complete, formula = rXyrXmcXt))

# look at likely preferred model for a sec 
summary(felm(data=complete, formula = cXt2intrXm))

# Run all models
modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete, formula = m)
}

# Combine into a single stargazer plot 
mynote = "Column specifications: (1) country, year and month FE; (2) country-by-year and month FE; (3) country-by-year and country-by-month Fs; (4) country-specific quad. trends and month FE; (5) country-specific quad. trends and country-by-month FE; (6) country-specific quad. trends and intervention year FE; (7) country-specific quad. trends, intervention year FE, GBOD region-by-month FE; (8) country-specific quad. trends with intervention FE and country by month FE; (9) GBOD region-by-year and region-by-month FE; (10) GBOD region-by-year and country-by-month FE; (11) country-by-year and GBOD region-by-month FE; (12) GBOD region-by-year and region-by-month FE with country-specific linear trends."
stargazer(modellist,
          title="Quadratic temperature: FE sensitivity", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought"),
          out = file.path(wd, "Results", "Tables", "panelFE_FE_sensitivity.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

# Save preferred model to use in counterfactual exercise
mainmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)
bfn = file.path(wd,"Results", "Models", "coefficients_cXt2intrXm.rds")
vfn = file.path(wd,"Results", "Models", "vcv_cXt2intrXm.rds")
saveRDS(coeffs, file=bfn)
saveRDS(vcov, file=vfn)

# Save fixed effects for the main model
print(cXt2intrXm)

# Make necessary vbls by hand 
#as.factor(smllrgn):month
complete = complete %>% mutate(smllrgnXmth = interaction(smllrgn, month))
cXt2intrXm_v2 = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + smllrgnXmth + I(country):monthyr + I(country):monthyr2 | OBJECTID | 0 | OBJECTID"))
mainmod_v2 = felm(data = complete, formula = cXt2intrXm_v2)
summary(mainmod_v2)
fes = lfe::getfe(mainmod_v2,se=FALSE)
fesfn = file.path(wd,"Results", "Models", "fixed_effects_cXt2intrXm.rds")
saveRDS(fes, file=fesfn)

########################################################################
# D. PLOT RESPONSE FUNCTIONS FOR FULL SPEC CHECK ACROSS FEs 
########################################################################

# Plot temperature response for each model
plotXtemp = cbind(seq(10,37), seq(10,37)^2)

figList = list()
for(m in 1:length(modellist)) {
  figList[[m]] =  plotPolynomialResponse(modellist[[m]], "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                                         yLab = expression(paste(Delta, " % Prevalence", '')), title = mycollabs[m], yLim=c(-15,15), showYTitle = T)
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], figList[[10]], figList[[11]], figList[[12]], nrow=4)
p

ggsave(file.path(wd, "Results", "Figures", "Diagnostics", "Fixed_effects", "panelFE_FE_sensitivity.pdf"), plot = p, width = 7, height = 9)

########################################################################
# E. KEY QUESTION #1: AT WHAT SPATIAL SCALE ARE TRENDS? 
########################################################################

########## VISUALIZING SECULAR TRENDS #########
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

########## Test: RegionXyear vs year #########
# 1) how many obs per region-year, on avg? 
regcounts = complete %>% group_by(smllrgn) %>% tally()
summary(regcounts$n) # on average, we've got 2303 observations per GBOD region over the whole sample
regyrcounts = complete %>% group_by(smllrgn,year) %>% tally()
summary(regyrcounts$n) # on average, we've got 25 observations per GBOD region per year to identify regionXyear FEs
regmocounts = complete %>% group_by(smllrgn,month) %>% tally()
summary(regmocounts$n) # on average, we've got 209 observations per GBOD region per month to identify regionXmonth FEs
isomocounts = complete %>% group_by(country,month) %>% tally()
summary(isomocounts$n)
isoyrcounts = complete %>% group_by(country,year) %>% tally()
summary(isoyrcounts$n)

# 2) run model with regionXyear effects and jointly test significance
form1 = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + year + smllrgn:month | 0 | OBJECTID"))
form2 = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + year + smllrgn:month + smllrgn:year | 0 | OBJECTID"))
form3 = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, "| OBJECTID + year + smllrgn:month + country:year | 0 | OBJECTID"))
f1 = summary(felm(data=complete, formula = form1))
f2 = summary(felm(data=complete, formula = form2))
f3 = summary(felm(data=complete, formula = form3))

fstat_fullmodel = c(f1$fstat, f2$fstat, f3$fstat)
R2_fullmodel = c(f1$r2, f2$r2, f3$r2)
fstat_fullmodel
R2_fullmodel

form4 = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(smllrgn:year) | OBJECTID + year + smllrgn:month | 0 | OBJECTID"))
f1 = felm(data=complete, formula = form1)
f4 = felm(data=complete, formula = form4)
summary(f4) # NOTE that most regionXyear effects are highly statistically significant
ftest = waldtest(f4, grep("smllrgn:year",rownames(f4$coefficients)))

# plot regions over time 
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
g

# save
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Fixed_effects/region_quad_trends.png')
ggsave(filename = fn, plot = g, height = 6, width = 8)

# plot region by year FEs
rlist = unique(complete$smllrgn)
ylist = unique(complete$yearnum)
complete$ymn = NA

avgbyregionyr = complete %>% group_by(smllrgn, yearnum) %>% summarize(ymn = mean(PfPR2))
toplot = left_join(complete, avgbyregionyr, by = c("smllrgn", "yearnum"))

# quick plot by country over time 
g = ggplot(data = toplot, aes(x=datevar, PfPR2)) +
  geom_point(color="cadetblue4", size = 1) + theme_classic() +
  geom_line(aes(y = ymn), color = "red", size = 1) +
  labs(y = "PfPR2", x = "Date") + 
  facet_wrap(~ smllrgn)
g

# save
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Fixed_effects/region_FEs.png')
ggsave(filename = fn, plot = g, height = 6, width = 8)
rm(toplot)


########## VISUALIZING SECULAR TRENDS WITHIN EACH REGION #########
clist = unique(complete$country)
complete$yhat = NA

for(c in 1:length(clist)) {
  mydf = subset(complete, country==clist[c])
  mydf$monthyr2 = mydf$monthyr^2
  mod = lm(PfPR2 ~ monthyr+ monthyr2, data = mydf)
  complete$yhat[complete$country==clist[c]] = predict(mod, newdata = mydf)
}

# quick plot by country over time 
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
p

# save
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Fixed_effects/country_quad_trends_by_GBOD_region.png')
ggsave(filename = fn, plot = p, height = 8, width = 10)

########################################################################
# G. KEY QUESTION #2: AT WHAT SPATIAL SCALE IS SEASONALITY? 
########################################################################

# seasonality by region
rlist = unique(complete$smllrgn)
molist = unique(complete$month)

avgbyregionmo = complete %>% group_by(smllrgn, month) %>% summarize(ymn = mean(PfPR2))
toplot = left_join(complete, avgbyregionmo, by = c("smllrgn", "month"))

# quick plot by country over time 
g = ggplot(data=toplot, aes(x=month, y=ymn)) + theme_classic() + 
    geom_point(color="cadetblue4", size = 1) + facet_wrap(~smllrgn) + 
    labs(y = "PfPR2", x = "Month") 
g
# save
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Fixed_effects/region_seasonality.png')
ggsave(filename = fn, plot = g, height = 6, width = 8)

g = ggplot(data=toplot, aes(x=month, y=PfPR2)) + theme_classic() + 
  geom_boxplot() + facet_wrap(~smllrgn) + 
  labs(y = "PfPR2", x = "Month") 
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Fixed_effects/region_seasonality_boxplot.png')
ggsave(filename = fn, plot = g, height = 6, width = 8)


## Look at regionXmonth fe in the regression and see if they are sig
cXt2intrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(smllrgn)*I(month)  | OBJECTID  + country:monthyr + country:monthyr2 + intervention | 0 | OBJECTID"))
cXt2intcXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(country)*I(month)  | OBJECTID  + country:monthyr + country:monthyr2 + intervention | 0 | OBJECTID"))
rXyrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(smllrgn)*I(month) | OBJECTID + smllrgn:year | 0 | OBJECTID"))
summary(felm(data=complete, formula = cXt2intrXm))
summary(felm(data=complete, formula = rXyrXm))
summary(felm(data=complete, formula = cXt2intcXm))

tzn = subset(complete, country=="Tanzania")
tznmo = tzn %>% group_by(month) %>% summarize(mn = mean(PfPR2))
plot(tznmo$month, tznmo$mn, type='l')

########################################################################
# H. RESIDUALS 
########################################################################

# plot residuals from main specifications (over time and histogram)
myform = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " | OBJECTID  + country:monthyr + country:monthyr2 + intervention + smllrgn:month | 0 | OBJECTID"))
main = felm(data=complete, formula = myform)
complete$residuals = main$residuals

# residuals histogram
g = ggplot(data = complete) + geom_histogram(aes(residuals)) + theme_classic()
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Main_model/residuals_cXt2intrXm.png')
ggsave(filename = fn, plot = g)

# residuals over time
g = ggplot(data=complete,aes(x=datevar, y=residuals)) + geom_point(size = 1, alpha=.3) + geom_hline(yintercept = 0, size = .5, color="red") +
  stat_smooth(method = "loess", formula = y ~ x, size = 1) +
  theme_classic()
g
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Main_model/residuals_cXt2intrXm_overtime.png')
ggsave(filename = fn, plot = g)

# residuals over time by region
g = ggplot(data=complete,aes(x=datevar, y=residuals)) +  geom_point(size = .5, alpha=.3) + geom_hline(yintercept = 0, size = .5, color="red") +
    stat_smooth(method = "lm", formula = y ~ poly(x,3), size = .5) + theme_classic() + facet_wrap(~country) 
g
fn = file.path(wd, 'Results', 'Figures', 'Diagnostics', 'Main_model/residuals_cXt2intrXm_overtime_by_country.png')
ggsave(filename = fn, plot = g)

########################################################################
# I. Lags & leads
########################################################################

# Temp lags and leads (leave drought/flood as is)
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
templags = templags %>% select(tokeep, contains("lag"), contains("lead"))
complete <- left_join(complete, templags, by=c("OBJECTID", "monthyr", "month", "year"))
complete$month=as.factor(complete$month)

# Formulas
cont = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
lg1 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag +", 
                           floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
lg2 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lag2 + temp2.lag2 +", 
                        floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
lg3 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lag2 + temp2.lag2 + temp.lag3 + temp2.lag3 +", 
                        floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
ld1lg1 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lead + temp2.lead +", 
                           floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
ld2lg2 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lead + temp2.lead +  temp.lag2 + temp2.lag2 + temp.lead2 + temp2.lead2 + ", 
                           floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
ld3lg3 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lead + temp2.lead +  temp.lag2 + temp2.lag2 + temp.lead2 + temp2.lead2 + temp.lag3 + temp2.lag3 + temp.lead3 + temp2.lead3 +", 
                           floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
ld1lg3 = as.formula(paste0("PfPR2 ~ temp + temp2 + temp.lag + temp2.lag + temp.lead + temp2.lead +  temp.lag2 + temp2.lag2 + temp.lag3 + temp2.lag3 +", 
                           floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | 0 | OBJECTID"))
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
stargazer(modellist,
          title="Quadratic temperature: Leads and lags", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought"),
          out = file.path(wd, "Results", "Tables", "panelFE_leads_lags.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F)

# Plot main model with SEs
plotXtemp = cbind(seq(10,37), seq(10,37)^2)
c = plotPolynomialResponse(modellist[[1]], "temp", plotXtemp, polyOrder = 2, plotmax=T, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                           yLab = expression(paste(Delta, " % Prevalence", '')), title = "contemp.", yLim=c(-15,15), showYTitle = T)

# Plot cumulative effect at different lag lengths (w/o conf intervals for now)
# lag 1
coefs = c(summary(glht(modellist[[2]], linfct = c("temp + temp.lag = 0")))$test$coefficients, summary(glht(modellist[[2]], linfct = c("temp2 + temp2.lag = 0")))$test$coefficients)
p1 = plotPolynomialResponseSimple(coefs, plotXtemp, polyOrder = 2, plotmax = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                                yLab = expression(paste(Delta, " % Prevalence", '')), title = "cumulative (1 mo.)", yLim=c(-15,15), showYTitle = T)
# lag 2
coefs = c(summary(glht(modellist[[3]], linfct = c("temp + temp.lag + temp.lag2 = 0")))$test$coefficients, summary(glht(modellist[[3]], linfct = c("temp2 + temp2.lag + temp2.lag2 = 0")))$test$coefficients)
p2 = plotPolynomialResponseSimple(coefs, plotXtemp, polyOrder = 2, plotmax = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                                  yLab = expression(paste(Delta, " % Prevalence", '')), title = "cumulative (2 mos.)", yLim=c(-15,15), showYTitle = T)

# lag 3
coefs = c(summary(glht(modellist[[4]], linfct = c("temp + temp.lag + temp.lag2 + temp.lag3 = 0")))$test$coefficients, summary(glht(modellist[[4]], 
                                            linfct = c("temp2 + temp2.lag + temp2.lag2 + temp2.lag3 = 0")))$test$coefficients)
p3 = plotPolynomialResponseSimple(coefs, plotXtemp, polyOrder = 2, plotmax = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                                  yLab = expression(paste(Delta, " % Prevalence", '')), title = "cumulative (3 mos.)", yLim=c(-15,15), showYTitle = T)

p = plot_grid(c, p1, p2, p3, nrow=1)
p

dir.create(file.path(wd, "Results", "Figures", "Diagnostics", "Temp_lags"), showWarnings = FALSE)
save_plot(file.path(wd, "Results", "Figures", "Diagnostics", "Temp_lags", "templags_cumulative_effects.pdf"), p, ncol = 1, base_asp = 3)


########################################################################
# J. Robustness on drought/flood definitions
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
    newdf = newdf %>% arrange(OBJECTID, monthyr)
    
    # list of variables indicating drought and flood
    floodvars = paste(colnames(complete)[grep("flood", colnames(complete))], collapse = " + ")
    droughtvars = paste(colnames(complete)[grep("drought", colnames(complete))], collapse = " + ")
    
    # regression formula (main spec)
    mymod = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + smllrgn:month | 0 | OBJECTID"))
    
    # run regression, store results
    modellist[[i]] = felm(data = newdf, formula = mymod)
    modellabs[[i]] = paste0("drought:",dd," flood:",ff)
    
    print(paste0('----------- Regression run for drought pctile ', dd, ' and flood pctile ', ff, ' -----------'))
    rm(newdf)
  }
}

######## For each model, plot temperature response ########
plotXtemp = cbind(seq(10,37), seq(10,37)^2)
figList = list()
for(m in 1:length(modellist)) {
  figList[[m]] =  plotPolynomialResponse(modellist[[m]], "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = 32.6, xLab = "Monthly avg. T [C]", 
                                         yLab = expression(paste(Delta, " % Prevalence", '')), title = modellabs[m], yLim=c(-15,15), showYTitle = T)
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], 
              figList[[10]], figList[[11]], figList[[12]], 
              figList[[13]], figList[[14]], figList[[15]], nrow=5)
p

ggsave(file.path(wd, "Results", "Figures", "Diagnostics", "Main_model", "temp_responses_drought_flood_sensitivity.pdf"), plot = p, width = 7, height = 10)

######## For each model, plot drought and flood coeffs ########

# All drought figures
figList = list()
for(m in 1:length(modellist)) {
  figList[[m]] =  plotLinearLags(modellist[[m]], "drought", cluster = T, laglength = 3, xLab="Lag", "Coefficient", title = modellabs[[m]], yLim = c(-6,4))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], 
              figList[[10]], figList[[11]], figList[[12]], 
              figList[[13]], figList[[14]], figList[[15]], nrow=5)
p

ggsave(file.path(wd, "Results", "Figures", "Diagnostics", "Main_model", "drought_responses_sensitivity.pdf"), plot = p, width = 7, height = 10)

# All flood figures
figList = list()
for(m in 1:length(modellist)) {
  figList[[m]] =  plotLinearLags(modellist[[m]], "flood", cluster = T, laglength = 3, xLab="Lag", "Coefficient", title = modellabs[[m]], yLim = c(-4,4))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], 
              figList[[10]], figList[[11]], figList[[12]], 
              figList[[13]], figList[[14]], figList[[15]], nrow=5)
p

ggsave(file.path(wd, "Results", "Figures", "Diagnostics", "Main_model", "flood_responses_sensitivity.pdf"), plot = p, width = 7, height = 10)

########################################################################
# K. GBoD region sensitivity analysis
########################################################################

#to run a set of tests on the GBD regions (see if seasonality changes over time and see if east africa needs country X month FEs)

### Q1: does seasonality in the GBoD regions change over time? 

## Full sample
mymod = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention)  + I(smllrgn)*month | OBJECTID + country:monthyr + country:monthyr2  | 0 | OBJECTID"))
out = felm(data = complete, formula = mymod)
summary(out)

## Break up time series
complete$period = ifelse(complete$yearnum<=1960, 1, 0)
complete$period[complete$yearnum>1960 & complete$yearnum<=2000] = 2
complete$period[complete$yearnum>2000] = 3
complete$period = as.factor(complete$period)

# Re run interacting seasonality with time period...not really seeing much here 
mymod = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention)  + I(smllrgn)*month*period | OBJECTID + country:monthyr + country:monthyr2  | 0 | OBJECTID"))
out = felm(data = complete, formula = mymod)
summary(out)

### Q2: Does East Africa X month FE change overall main result?
# Create a countryXmo factor only for east african region
complete$isomo = with(complete, interaction(country,  month), drop = TRUE )
complete$factorvar = ifelse(complete$smllrgn=="Sub-Saharan Africa (East)", complete$isomo, 0)
complete$factorvar = as.factor(complete$factorvar)

mymodmain = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention)  | OBJECTID + country:monthyr + country:monthyr2 + smllrgn:month | 0 | OBJECTID"))
out_main = felm(data = complete, formula = mymodmain)
mymodsens = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + I(factorvar) | OBJECTID + country:monthyr + country:monthyr2 + smllrgn:month | 0 | OBJECTID"))
out_sens = felm(data = complete, formula = mymodsens)

# Combine into a single stargazer plot 
stargazer(out_main, out_sens,
          title="Quadratic temperature: Sensitivity to ISOxMO in E. Africa", align=TRUE, column.labels = c("Main", "ISOxMo in E.A."),
          keep = c("temp", "flood", "drought"),
          out = file.path(wd, "Results", "Tables", "panelFE_ISOxMO_East_Africa.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F)

