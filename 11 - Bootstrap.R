
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

# this loads, cleans, and preps our main dataset
source(file.path(repo,'code/prep_data.R'))

# MAIN SPEC: cXt2intrXm (country specific quadratic trends, intervention dummies, GBOD region by month FE)
myform = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
                           " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"))


########################################################################
# D. BOOTSTRAP
########################################################################

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



