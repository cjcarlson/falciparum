############################################################
# This script plots a distribution of peak temperatures
# for the bootstrapped set of prevalentce~temperature
# relationships.
############################################################

############################################################
# Set up
############################################################

rm(list = ls())

user = "Tamma" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution' #location for data and output
  repo = 'C:/Users/cjcar/Documents/Github/falciparum' #location for cloned repo
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else {
  wd = NA
  print('Script not configured for this user!')
}

CRUversion = "4.03" # "4.06"
if (CRUversion=="4.03") {
  resdir = file.path(wd, "Results")
} else if (CRUversion=="4.06") {
  resdir = file.path(wd, "Results_CRU-TS4-06")
} else {
  print('CRU version not supported! Use 4.03 or 4.06.')
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
# Load bootstraps
############################################################

boots = readRDS(file.path(resdir,'Models','block_bootstrap_cXt2intrXm.rds'))

############################################################
# Compute peak temp for each run
############################################################

boots = as.data.frame(boots)
peakT = function(coef1, coef2){
  out = -1*(coef1)/(2*coef2)
  return(out)
}

boots = boots %>% mutate(peakT = peakT(temp,temp2))


############################################################
# Make histogram
############################################################

meanpeak = mean(boots$peakT)

g = ggplot(data=boots) + geom_histogram(aes(x=peakT, y=..density..,color="#C1657C",fill="#C1657C"),
                                        show.legend = FALSE) + 
  geom_vline(xintercept = meanpeak, color= "grey88") + theme_classic() 
g

ggsave(file.path(resdir, "Figures", "Diagnostics", "Main_model", "histogram_peakT.pdf"), 
       plot = g, width = 7, height = 6)
