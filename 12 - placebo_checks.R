### This script conducts two placebo checks. One in which we scramble the time series of weather within each panel unit (time),
### the other in which we reassign full weather time series to random panel units (space).
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
library(lfe)
library(tidyverse)
library(zoo)
library(lubridate)
library(rgdal)
library(dplyr)
library(data.table)
library(doParallel)

########################################################################
# A. INITIALIZING
########################################################################

# this loads, cleans, and preps our main dataset
source(file.path(repo,'code/prep_data.R'))

# MAIN SPEC: cXt2intrXm (country specific quadratic trends, intervention dummies, GBOD region by month FE)
myform = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
                           " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"))

########################################################################
# B. Randomly scramble weather within each panel unit
########################################################################

# drop if there is only one observation per ADM1 (these are dropped in regression anyway)
scram = complete %>% group_by(OBJECTID) %>%
  mutate(length=length(!is.na(PfPR2))) %>%
  filter(length>1)

# Which variables do you want to store?
pattern = c("temp", "flood", "drought")

# Run simulation many times
S = 1000

# Set number of cores to parallelize over:
n_cores = 4
registerDoParallel(n_cores)
set.seed(7812)

placebo_out=foreach(i=1:S,
                    .combine = rbind) %dopar%
  {
    
    # create scrambled climate
    scramS = scram %>% group_by(OBJECTID) %>% 
      mutate(temp = sample(temp),
             drought = sample(drought),
             flood = sample(flood))
    
    # obtain lags and squared term of scrambled climate
    scramS = scramS %>% 
      mutate(temp2 = temp^2,
             flood.lag=lag(flood, order_by=monthyr),
             flood.lag2 = lag(flood, order_by = monthyr, n=2),
             flood.lag3 = lag(flood, order_by = monthyr, n=3),
             drought.lag = lag(drought, order_by = monthyr),
             drought.lag2 = lag(drought, order_by = monthyr, n=2),
             drought.lag3 = lag(drought, order_by = monthyr, n=3))
    
    # run regression
    mod = felm(data=scramS, formula=myform)
    
    # store coefficients, their p values, and the result of an F test on the effect at 35C
    Xvars = rownames(mod$coefficients)[grepl(paste(pattern, collapse="|"), x = rownames(mod$coefficients))]
    Xcoefs = as.matrix(mod$coefficients[rownames(mod$coefficients) %in% Xvars])
    Xpvals = as.matrix(summary(mod)$coefficients[rownames(mod$coefficients) %in% Xvars,4])
    
    out = cbind(t(Xcoefs), t(Xpvals))
    
    # Print so we can track progress
      print(paste0("-------- Done with sample ", i, " of ", S, " ---------"))

    return(out)
  }


placebo_out=data.frame(placebo_out, stringsAsFactors = F)

# get column names right
mod = felm(data=complete, formula=myform)
Xvars = rownames(mod$coefficients)[grepl(paste(pattern, collapse="|"), x = rownames(mod$coefficients))]
pnames = list()
for(x in Xvars){
  pnames[[x]] = paste0(x, "_p")
}
colnames(placebo_out) = c(Xvars, unlist(pnames))
stopImplicitCluster()

# Save
fn <- "scramble_time_placebo.rds"
saveRDS(placebo_out, file = paste0('Results/Models/', fn))

print("-------- Saved Rds of coefficients and p-values from randomization test with scrambled climate data --------")

########################################################################
# Investigate p-vals relative to standard model
########################################################################

placebo = readRDS(file.path("Results", "Models", "scramble_time_placebo.rds"))

# matrix of graphs: histogram of coefficients from placebo, compared to coef in true (vertical line in red) 

# matrix of graphs: histogram of pvals from placebo, compared to pval in true (vertical line in red)