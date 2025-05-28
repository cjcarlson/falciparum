############################################################
# This script investigates correlation in the model
# residuals and assesses alternative methods of clustering
# or accounting for spatiotemporal correlations in errors.
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

Tref = 25 # reference temperature - curve gets recentered to 0 here
Tmin = 10 # min T for x axis
Tmax = 40 # max T for x axis

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
source(here::here("Pipeline", "A - Utility functions", "A03 - Prep data for estimation.R"))



########################################################################
# R2: Correlated errors
########################################################################

# Are residuals correlated within countries? 
resCntry = felm(res ~ monthyr + I(country), data=complete)
pvals = summary(resCntry)$coefficients[,"Pr(>|t|)"]
ph = ggplot() + 
  geom_histogram(aes(x=pvals), color= "seagreen", fill = "seagreen") + 
  xlab("p-values by country") +
  theme_classic()
ph

ggsave(file.path(resdir, "Figures", "Diagnostics", "Reviewer_comments", "res_corr_countries.pdf"), plot = ph, width = 7, height = 7)

#boxplot of residuals by country
g= ggplot(complete, aes(x=country, y=res)) + 
  geom_boxplot() + 
  theme_classic() + ylab("residuals") + theme( axis.text.x=element_blank(),axis.ticks.x=element_blank())
g
ggsave(file.path(resdir, "Figures", "Diagnostics", "Reviewer_comments", "res_boxplot_countries.pdf"), plot = g, width = 9, height = 5)


