############################################################
# This script makes all four panels of Figure S1.
############################################################

# rm(list = ls())
# 
# user = "Tamma" #"Colin"
# if (user == "Colin") {
#   wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/'
#   repo = 'C:/Users/cjcar/Documents/Github/falciparum'
# } else if (user == "Tamma") {
#   wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/'
#   repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
# } else {
#   wd = NA
#   print('Script not configured for this user!')
# }
# 
# setwd(wd)
# 
# # source functions from previous script
# source(file.path(repo,'Pipeline/A - Utility functions/A01 - Utility code for calculations.R'))
# source(file.path(repo,'Pipeline/A - Utility functions/A02 - Utility code for plotting.R'))

# packages
library(lfe)
library(zoo)
library(here)
library(reshape)
library(tidyverse)
library(lubridate)
library(patchwork)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here("Pipeline", "A - Utility functions", "A01 - Utility code for calculations.R"))
source(here::here("Pipeline", "A - Utility functions", "A02 - Utility code for plotting.R"))

#######################################################################
# S1A: Theoretical
#######################################################################

#### Read in the data backup

data <- file.path(datadir,"Data", "CRU-Reextraction-Aug2022.csv") |> 
  read.csv()
# Keep an eye out for logical data parse issue

# Generate the R0 curves:
data$predR0 <- sapply(data$temp, r0t)

data %>%
  ggplot(aes(x = temp, y = predR0)) + 
  geom_vline(xintercept = 25.56, color = 'dark grey', lwd = 1, linetype = 'longdash') +
  geom_line(color = "black", lwd = 0.7) + 
  xlim(c(15,35)) + 
  labs(x = expression(paste("Temperature (",degree,"C)")),
       y = expression('R'[0]*' predicted')) + 
  theme_bw() -> 
  g1


#######################################################################
# S1B: GAMS on raw data
#######################################################################

data %>%
  ggplot(aes(x = temp, y = predR0)) + 
  geom_vline(xintercept = 25.56, color = 'dark grey', lwd = 0.7, linetype = 'longdash') +
  #  geom_vline(xintercept = optg2, color = "#C1657C", lwd = 0.7, linetype = 'longdash') +
  geom_smooth(aes(y = PfPR2), lwd=1, color = "#C1657C", fill='light grey') + 
  xlim(c(15,35)) +
  labs(x = expression(paste("Temperature (",degree,"C)")),
       y = "Prevalence (%, raw data)")  + 
  theme_bw() -> 
  g2 

ggplot_build(g2)$data[[2]] -> sm

sm$x[sm$y == max(sm$y)] -> optg2

data %>%
  ggplot(aes(x = temp, y = predR0)) + 
  geom_vline(xintercept = 25.56, color = 'dark grey', lwd = 0.7, linetype = 'longdash') +
  geom_vline(xintercept = optg2, color = "#C1657C", lwd = 0.7, linetype = 'longdash') +
  geom_smooth(aes(y = PfPR2), lwd=1, color = "#C1657C", fill='light grey') + 
  xlim(c(15,35)) + 
  labs(x = expression(paste("Temperature (",degree,"C)")),
       y = "Prevalence (%, raw data)") + 
  theme_bw() -> 
  g2 

#######################################################################
# S1C: Econometric model + uncertainty
#######################################################################

# make space
rm(data)

#### Call external script for data cleaning
CRUversion = "4.03"
source(file.path(repo,'Pipeline/A - Utility functions/A03 - Prep data for estimation.R'))

# Formula & estimation
cXt2intrXm = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars,
    " + I(intervention) + country:monthyr + country:monthyr2",
    "| OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)
mainmod = felm(data = complete, formula = cXt2intrXm)
beta = mainmod$coefficients ##See if this works
vars = rownames(beta)
plotVars = vars[grepl(pattern = "temp", x = vars)] 

rm(complete,countrydf,data,data_iso,data.reset)

# plot setup 
Tref = 24
Tmin = 10
Tmax = 40
int = 0.1
plotXtemp = cbind(seq(Tmin,Tmax,by=int), seq(Tmin,Tmax,by=int)^2)
myrefT = max(round(-1*beta[1]/(2*beta[2]), digits = 0), 10) # plot relative to max of quadratic function
optg3 = -1*beta[1]/(2*beta[2])
xValsT = genRecenteredXVals_polynomial(plotXtemp,myrefT,2,NA)
vcov = getVcov(mainmod$clustervcv, plotVars)
b = as.matrix(beta[rownames(beta) %in% plotVars])

response = as.matrix(xValsT) %*% b #Prediction
length = 1.96 * sqrt(apply(X = xValsT, FUN = calcVariance, MARGIN = 1, vcov))
lb = response - length
ub = response + length

# add back in the reference temperature so it's centered at xRef
plotData = data.frame(x = xValsT[,1] + myrefT, response = response, lb = lb, ub = ub)

g3 = ggplot(data = plotData) + #geom_ribbon(aes(x, ymin = lb, ymax = ub), alpha = 0.4, fill = 'light grey') +
  geom_vline(xintercept = 25.56, color = 'dark grey', lwd = 0.7, linetype = 'longdash') +
  geom_vline(xintercept = optg3, color = "#C1657C", lwd = 0.7, linetype = 'longdash') +
  geom_line(aes(x=x, y = response),color = "#C1657C", lwd = 1) + 
  xlim(c(15,35)) + 
  ylim(c(-9,0.2)) + 
  labs(x = expression(paste("Temperature (",degree,"C)")),
       y = "Prevalence (%, modeled)") + 
  theme_bw()
g3

#######################################################################
# S1D: Distribution of peak temperatures in econometric model
#######################################################################

# function to compute optimal temp for each run
optT <- function(beta1, beta2){
  opt = -beta1/(2*beta2)
  return(opt)
}

# upload bootstraps 
boots = as.data.frame(readRDS(file.path(wd, "Results",'Models','block_bootstrap_cXt2intrXm.rds')))
boots = boots %>% mutate(peakT = optT(temp,temp2))

meanpeak = mean(boots$peakT)

g4 = ggplot(data=boots) + geom_histogram(aes(x=peakT, y=..density..),color="light grey",fill="light grey",
                                         show.legend = FALSE) + 
  labs(x = expression(paste("Optimum temperature (",degree,"C)")),
       y = "Density") +
  geom_vline(xintercept = meanpeak, color= "#C1657C",lwd = 0.7, linetype = 'longdash') + theme_bw() 
g4


#######################################################################
# Combine and save
#######################################################################

p = plot_grid(g1, g2, g3, g4, nrow = 2, 
              label_size = 12, labels = c('A', 'B', 'C','D'))
p
fn = file.path(repo, 'Figures', 'FigureS1.pdf')
ggsave(filename = fn, plot = p, height = 10, width = 10)
