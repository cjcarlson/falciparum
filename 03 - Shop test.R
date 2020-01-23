rm(list = ls())

library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(velox)

user = "Shop" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/Data'
  repo = "" #to fill in
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/Data'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else if (user == "Shop") {
  wd = '/Users/bublai/Dropbox/ruins_remote_sensing/malaria'
  repo = '/Users/bublai/Desktop/research/falciparum/'
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

