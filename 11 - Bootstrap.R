
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
library(doSNOW)
library(lfe)
library(tidyverse)
library(zoo)
library(lubridate)
library(rgdal)
library(dplyr)
library(data.table)
library(tictoc)

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
# C. CONSTRUCT KEY VARIABLES IN MAIN REGRESSION
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
complete$intervention[complete$yearnum>=2004 & complete$yearnum<=2015] = 2
complete$intervention = as.factor(complete$intervention)

rm(data.reset) # remove from memory

########################################################################
# D. BOOTSTRAP
########################################################################

# MAIN SPEC: cXt2intrXm (country specific quadratic trends, intervention dummies, GBOD region by month FE)
myform = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
                           " + I(intervention) | OBJECTID + country:monthyr + country:monthyr2 + smllrgn:month | 0 | OBJECTID"))

# BLOCK BOOTSTRAP by ADM1s:
adm1s = unique(complete$OBJECTID)

# Set number of bootstrap simulations. 
S = 1000

# Set number of cores to parallelize over:
n_cores = 4

## Bootstrap, sampling by ADM1
# Store in the first row of the output the regression run with all observations
# parallelize
set.seed(11235)
clus <- makeCluster(n_cores)
registerDoSNOW(clus)
pb <- txtProgressBar(max = S, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
result <- foreach (i = 1:(S+1),
                   .packages = c("lfe"),
                   .options.snow = opts) %dopar% 
                   {
                     cl  <- sample(adm1s, size = length(adm1s), replace=T)
                     df.bs <- sapply(cl, function(x) which(complete[,'OBJECTID']==x))
                     complete.boot <- if(i==1) complete else{complete[unlist(df.bs),]} 
                     mod <- felm(myform, data = complete.boot)
                     out <- t(mod$coefficients[c(1:10)]) # only keep the climate coefficients, not the intervention dummies
                     colnames(out) <- c("temp", "temp2", colnames(complete)[grep("flood", colnames(complete))], colnames(complete)[grep("drought", colnames(complete))])
                     fn <- if(i==1) 'full_sample_cXt2intrXm.rds' else{fn = paste0('block_bootstrap_', i, '_cXt2intrXm.rds')}
                     saveRDS(out, file = paste0('Results/Models/bootstrap/', fn))
                   }
close(pb)
stopCluster(clus) 

########################################################################
# E. PULL IN BOOTSTRAPPED SAMPLES AND SAVE
########################################################################

# all bootstraps, including full sample
boot.files = list.files('Results/Models/bootstrap/')

# append
boots = array(dim = c(S+1,10))
colnames(boots) = c("temp", "temp2", colnames(complete)[grep("flood", colnames(complete))], colnames(complete)[grep("drought", colnames(complete))])
for (f in 1:length(boot.files)) {
 boots[f,] = readRDS(paste0('Results/Models/bootstrap/',boot.files[f]))
}

# save
saveRDS(boots, file = 'Results/Models/block_bootstrap_cXt2intrXm.rds')



