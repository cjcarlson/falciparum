############################################################
# This script conducts additional robustness checks requested
# by reviewers in round 1 of Nature submission 
# Date: November 2024
# This script should be incorporated into D01 when a subset of 
# tests are included in the main text and/or Supplement.
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
# R1, Comment 1: Do count of surveys respond to T and P shocks? 
# Implementable immediately: Does diagnostic method change with T and P shocks? 
########################################################################

# common variables in all regs
common <- paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars)
country_time <- "country:monthyr + country:monthyr2"

# main spec: cXt2intrXm
cXt2intrXm = as.formula(paste0(common, " + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

# new outcome var: Probability method is XX
complete <- complete |> mutate(microscopy = (simplified_METHOD=="MICROSCOPY"))
PrMicro = as.formula(paste0("microscopy ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

Micromod = felm(data = complete, formula = PrMicro)
mainmod = felm(data = complete, formula = cXt2intrXm)

# stargazer
dir.create(file.path(resdir, "Tables", "sensitivity"), showWarnings = FALSE)
stargazer(Micromod,
          title="Microscopy method test", align=TRUE,
          keep = c("temp", "flood", "drought"),
          out = file.path(resdir, "Tables", "sensitivity","Microscopy_sample_sensitivity.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F)

########################################################################
# R1: Overdispersion?
########################################################################

# Plot model residuals
complete <- complete |> mutate(res = c(residuals(mainmod)))
g <- ggplot(data=complete) + 
  geom_histogram(aes(x=res), color= "seagreen", fill = "seagreen") + 
  xlab("model residuals") +
  theme_classic()
g

dir.create(file.path(resdir, "Figures", "Diagnostics","Reviewer_comments"), showWarnings = FALSE)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Reviewer_comments", "model_residuals.pdf"), plot = g, width = 7, height = 7)

########################################################################
# R1: Control for diagnostic method?
########################################################################

###### Estimate regressions #######
complete_dm <- filter(complete, dominant_METHOD != "LAMP")

cXt2intrXmDM = as.formula(paste0(common, " + dominant_METHOD + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2intrXmSM = as.formula(paste0(common, " + simplified_METHOD + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

myforms = c(cXt2intrXm, cXt2intrXmDM, cXt2intrXmSM) 

mycollabs = c(
  "main specification", # Main Spec
  "+ diag. method (full set)", # Main Spec with dominant method
  "+ diag. method (small set)" # Main Spec with simplified method
)

modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete_dm, formula = m)
}

mynote = "Column specifications: (1) country-specific quad. trends, intervention year FE, GBOD region-by-month FE; (2) same as (1), but with additional controls for diagnostic method: Microscopy, Microcscopy/PCR Confirmed, PCR, RDT, RDT/PCR Confirmed, and RDT/SLIDE Confirmed; (3) same as (2) but using simplified diagnostic method control set: Microscopy, PCR, RDT."
dir.create(file.path(resdir, "Tables", "sensitivity"), showWarnings = FALSE)
stargazer(
  modellist,
  title = "Sensitivity to controlling for diagnostic method", 
  align = TRUE, 
  column.labels = mycollabs,
  keep = c("temp", "flood", "drought", "METHOD"),
  out = file.path(resdir, "Tables", "sensitivity","DiagMethod_sensitivity.tex"),  
  omit.stat = c("f", "ser"), 
  out.header = FALSE,
  type = "latex", 
  float = F,
  notes.append = TRUE, 
  digits = 2,
  notes.align = "l", 
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}")
)

####### Plot temperature responses #######

plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)

figList = list()
for(m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]], "temp", plotXtemp,
    polyOrder = 2, cluster = T, xRef = myrefT,
    xLab = expression(paste("Mean temperature (",degree,"C)")), 
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

ggsave(file.path(resdir, "Figures", "Diagnostics", "Reviewer_comments", "res_corr_countries.pdf"), plot = g, width = 7, height = 7)

#boxplot of residuals by country
g= ggplot(complete, aes(x=country, y=res)) + 
  geom_boxplot() + 
  theme_classic() + ylab("residuals") + theme( axis.text.x=element_blank(),axis.ticks.x=element_blank())
g
ggsave(file.path(resdir, "Figures", "Diagnostics", "Reviewer_comments", "res_boxplot_countries.pdf"), plot = g, width = 9, height = 5)

########################################################################
# R2: Data imbalance: responses on different subsamples? 
########################################################################

complete = complete |> mutate(yearnum = as.numeric(as.character(year)))
g = ggplot(complete) + geom_histogram(aes(x=yearnum), color= "seagreen", fill = "seagreen") + 
  xlab("year") + ylab("count of observations") + theme_classic()
g

# obs by group
complete = complete |> mutate(post1995 = (yearnum>=1995))
complete %>% count(post1995)

# formula (remove intervention dummies for temporal subsampling)
cXt2rXm = as.formula(paste0(common, " +  ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

pre1995 = felm(data = subset(complete, post1995==FALSE), formula = cXt2rXm)
post1995 = felm(data = subset(complete, post1995==TRUE), formula = cXt2rXm)

# plot temperature responses
modellist = list(pre1995,post1995)
mycollabs = c(
  "Early sample (1901-1994)",
  "Late sample (1995-2016)"
)

plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)
figList = list()
for(m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]], "temp", plotXtemp,
    polyOrder = 2, cluster = T, xRef = myrefT,
    xLab = expression(paste("Mean temperature (",degree,"C)")), 
    yLab = "Prevalence (%)", title = mycollabs[m], yLim=c(-30,10), showYTitle = T) +
    theme(plot.title = element_text(size = 10))
}

p = plot_grid(figList[[1]], figList[[2]], nrow=1)
p

ggsave(file.path(resdir, "Figures", "Diagnostics", "Reviewer_comments", "split_sample_1995.pdf"), plot = p, width = 9, height = 5)




