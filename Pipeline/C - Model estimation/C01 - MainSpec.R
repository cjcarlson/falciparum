############################################################
# This script estimates the main empirical specification linking
# PfPR2 to drought, flood, and temperature.
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

Tref = 24 #reference temperature - curve gets recentered to 0 here
Tmin = 10 #min T for x axis
Tmax = 40 #max T for x axis

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
source(here::here("Pipeline", "A - Utility functions", "A03 - Prep data for estimation.R"))

#### Create necessary subfolders
dir.create(file.path(resdir, "Tables"), showWarnings = FALSE)
dir.create(file.path(resdir, "Figures"), showWarnings = FALSE)
dir.create(file.path(resdir, "Models"), showWarnings = FALSE)

########################################################################
# Estimation
########################################################################

# Formula (see other files for robustness/sensitivity checks)
cXt2intrXm = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)

# Estimation & save model results
mainmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)
dir.create(file.path(resdir, "Models", "reproducibility"), showWarnings = FALSE)
bfn = file.path(resdir, "Models", "reproducibility", "coefficients_cXt2intrXm.rds")
vfn = file.path(resdir, "Models", "reproducibility", "vcv_cXt2intrXm.rds")
saveRDS(coeffs, file=bfn)
saveRDS(vcov, file=vfn)

# Stargazer output
mynote = "Country-specific quad. trends with intervention FE and country by month FE."
dir.create(file.path(resdir,"Tables", "main"), showWarnings = FALSE)
stargazer(
  mainmod,
  title="PfPR2 response to daily avg. temperature", align=TRUE, 
  keep = c("temp", "flood", "drought"),
  out = file.path(resdir, "Tables", "main", "main_specification_cXt2intrXm.tex"), 
  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
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
dir.create(file.path(resdir,"Figures","Diagnostics","Main_model"), showWarnings = FALSE)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Main_model", "temp_response_cXt2intrXm.pdf"), plot = fig, width = 7, height = 7)

