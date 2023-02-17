############################################################
# This script estimates the main empirical specification linking
# PfPR2 to drought, flood, and temperature.
############################################################

############################################################
# Set up
############################################################

rm(list = ls())

user = "Tamma" #"Colin"
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

############################################################
# Plotting toggles
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tref = 24 #reference temperature - curve gets recentered to 0 here
Tmin = 10 #min T for x axis
Tmax = 40 #max T for x axis

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
source(file.path(repo,'Pipeline/A - Utility functions/A03 - Prep data for estimation.R'))

########################################################################
# Estimation
########################################################################

# Formula (see other files for robustness/sensitivity checks)
cXt2intrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

# Estimation & save model results
mainmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)
bfn = file.path(wd,"Results", "Models", "reproducibility", "coefficients_cXt2intrXm.rds")
vfn = file.path(wd,"Results", "Models", "reproducibility", "vcv_cXt2intrXm.rds")
saveRDS(coeffs, file=bfn)
saveRDS(vcov, file=vfn)

# Stargazer output
mynote = "Country-specific quad. trends with intervention FE and country by month FE."
stargazer(mainmod,
          title="PfPR2 response to daily avg. temperature", align=TRUE, 
          keep = c("temp", "flood", "drought"),
          out = file.path(wd, "Results", "Tables", "main", "main_specification_cXt2intrXm.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

########################################################################
# Plot (Note: analogous to Fig 2A but with analytically derived confidence intervals 
# in place of bootstrap runs shown in Fig 2A)
########################################################################

# Temperature support
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)

coefs = summary(mainmod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
fig =  plotPolynomialResponse(mainmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                         yLab = "Prevalence (%)", title = "Main spec: cXt2intrXm", yLim=c(-30,5), showYTitle = T)

fig
ggsave(file.path(wd, "Results", "Figures", "Diagnostics", "Main_model", "temp_response_cXt2intrXm.pdf"), plot = fig, width = 7, height = 7)

