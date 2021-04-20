
########################################################################
# This script plots the main prevalence-temperature dose-response function 
# as well as its uncertainty over 1,000 bootstrap samples 
########################################################################

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
library(tidyverse)
library(cowplot)

########################################################################
# A. Read in saved regression results
########################################################################

# load main model (full sample)
main <- readRDS(file.path("Results","Models","bootstrap","full_sample_cXt2intrXm.rds"))

# function to compute optimal temp for each run
optT <- function(beta1, beta2){
  opt = -beta1/(2*beta2)
  return(opt)
}

# load bootstraps into one dataframe
df = as.data.frame(main)
df$model = "main"

files = list.files(file.path("Results","Models","bootstrap"))
stop = length(files)-1
files=files[1:stop]

for(f in 1:length(files)){
  tmp = readRDS(file.path("Results","Models","bootstrap",files[f]))
  tmp = as.data.frame(tmp)
  tmp$model = paste0("boot",f)
  df = df %>% add_row(as.data.frame(tmp))
}

# calculate optimal temperature
df$optT = optT(df$temp,df$temp2)

# density plot
medoptT = median(df$optT)
g = ggplot(data=df, aes(optT)) + 
  geom_density(col="grey", 
                 fill="grey", 
                 alpha = .2) + 
  labs(title="Histogram of optimal temperatures", x="Optimal T (C)", y="count") + 
  geom_vline(xintercept=24, color="red", size=.5) + 
  geom_vline(xintercept=medoptT, color="blue", size=.5) + theme_classic()
g
fileout = file.path("Results", "Figures", "Fig1","full_density_optimalT.pdf")
ggsave(fileout)

# limit to middle 99% of the data
quants = quantile(df$optT, probs=c(.005,.995))
dfsub = subset(df, optT>=quants[1] & optT<=quants[2])
medoptT = median(dfsub$optT)
g = ggplot(data=dfsub, aes(optT)) + 
  geom_density(col="grey", 
               fill="grey", 
               alpha = .2) + 
  labs(title="Histogram of optimal temperatures", x="Optimal T (C)", y="count") + 
  geom_vline(xintercept=24, color="red", size=.5) + 
  geom_vline(xintercept=medoptT, color="blue", size=.5) + theme_classic()
g
fileout = file.path("Results", "Figures", "Fig1","clipped_99_density_optimalT.pdf")
ggsave(fileout)



  