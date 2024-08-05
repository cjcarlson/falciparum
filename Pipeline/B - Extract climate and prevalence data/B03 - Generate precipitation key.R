
rm(list = ls())

user = "Colin" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/' #location for data and output
  repo = 'C:/Users/cjcar/Documents/Github/falciparum' #location for cloned repo
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else {
  wd = NA
  print('Script not configured for this user!')
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

#### Call external script for data cleaning
source(file.path(repo,'Pipeline/A - Utility functions/A03 - Prep data for estimation.R'))
