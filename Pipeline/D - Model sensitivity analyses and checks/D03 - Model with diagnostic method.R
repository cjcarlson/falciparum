############################################################
# This script conducts a variety of robustness checks on the
# main empirical specification linking PfPR2 to drought, 
# flood, and temperature.
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

complete <- filter(complete, dominant_METHOD != "LAMP")

########################################################################
# Estimation
########################################################################

# Formulas: all fixed effects (main spec = cXt2intrXm)

# Formula (see other files for robustness/sensitivity checks)

common <- paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars)
country_time <- "country:monthyr + country:monthyr2"

cXt2intrXm = as.formula(
  paste0(common, " + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2intrXmDM = as.formula(
  paste0(common, " + dominant_METHOD + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2intrXmSM = as.formula(
  paste0(common, " + simplified_METHOD + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

myforms = c(cXt2intrXm, cXt2intrXmDM, cXt2intrXmSM) 
mycollabs = c(
  "cnty trd, int + rgn-mo FEs.", # Main Spec
  "cnty trd, int + rgn-mo FEs. + DM", # Main Spec with dominant method
  "cnty trd, int + rgn-mo FEs. + SM" # Main Spec with simplified method
)

# Run all models
modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete, formula = m)
}

# Combine into a single stargazer plot 
mynote = "Column specifications: (1) country, year and month FE; (2) country-specific quad. trends and month FE; (3) country-specific quad. trends and country-by-month FE; (4) country-specific quad. trends and intervention year FE; (5) country-specific quad. trends, intervention year FE, GBOD region-by-month FE; (6) country-specific quad. trends with intervention FE and country by month FE; (7) GBOD region-by-year and region-by-month FE; (8) GBOD region-by-year and country-by-month FE; (9) GBOD region-by-year and region-by-month FE with country-specific linear trends."
dir.create(file.path(resdir, "Tables", "sensitivity"), showWarnings = FALSE)
stargazer(modellist,
          title="Quadratic temperature: FE sensitivity", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought", "intervention", "METHOD"),
          out = file.path(resdir, "Tables", "sensitivity","FixedEffects_sensitivity.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, digits=2,notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

########################################################################
# Plot temperature response functions for all fixed effects specifications
########################################################################

# Plot temperature response for each model
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)

figList = list()
for(m in 1:length(modellist)) {
  # get max of response function
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]], "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
    yLab = "Prevalence (%)", title = mycollabs[m], yLim=c(-30,10), showYTitle = T) +
    theme(plot.title = element_text(size = 10))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], nrow=1)
p

fe_fig_dir <- file.path(resdir, "Figures", "Diagnostics","Fixed_effects")
dir.create(fe_fig_dir, showWarnings = FALSE)
ggsave(
  filename = "diagnostic_method_sensitivity.pdf",
  path = fe_fig_dir, 
  plot = p, 
  width = 9, 
  height = 3
)
