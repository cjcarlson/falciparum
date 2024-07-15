
########################################################################
# This script plots the main prevalence-temperature dose-response function 
# as well as its uncertainty over 1,000 bootstrap samples 
########################################################################

# packages
library(lfe)
library(zoo)
library(here)
library(reshape)
library(cowplot)
library(tidyverse)
library(lubridate)

# source functions from previous script
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here("Pipeline", "A - Utility functions", "A02 - Utility code for plotting.R"))

########################################################################
# A. Read in saved regression results
########################################################################

# load main model (full sample)
main <- file.path(datadir, "Results", "Models", "coefficients_cXt2intrXm.rds") |>
  readRDS()
coefs <- main[,1]
names(coefs) <- rownames(main)
main <- as.data.frame(t(as.matrix(coefs)))

# function to compute optimal temp for each run
optT <- function(beta1, beta2){
  opt = -beta1/(2*beta2)
  return(opt)
}

# load bootstraps into one dataframe
df = as.data.frame(main)
df$model = "main"

files = list.files(file.path(datadir, "Results","Models","bootstrap"))
stop = length(files)-1
files=files[1:stop]

for(f in 1:length(files)){
  tmp = readRDS(file.path(datadir, "Results","Models","bootstrap",files[f]))
  tmp = as.data.frame(tmp)
  tmp$model = paste0("boot",f)
  df = df %>% add_row(as.data.frame(tmp))
} 

########################################################################
# C. Spaghetti plot of estimated T response functions
########################################################################

#setup 
Tref = 24
Tmin = 10
Tmax = 40
int = 0.1
plotXtemp = cbind(seq(Tmin,Tmax,by=int), seq(Tmin,Tmax,by=int)^2)
xValsT = genRecenteredXVals_polynomial(plotXtemp,Tref,2)

# point estimate
main = subset(df, model=="main")
b = as.matrix(c(main$temp, main$temp2))
response = as.matrix(xValsT) %*% b #Prediction

plotData = data.frame(x = xValsT[,1] + Tref, response = response)

#plotData$model = "main"    

# loop over all bootstraps, add to dataframe

for(mod in 1:dim(df)[1]){
  sub = df[mod,]
  b = as.matrix(c(sub$temp,sub$temp2))
  boot = as.data.frame(as.matrix(xValsT) %*% b)
  colnames(boot) = paste0("boot",mod)
  plotData = cbind(plotData, boot)
  
  if(mod %% 100 == 0) {
    message(sprintf('--------- DONE WITH ITERATION %d of 1000 --------', mod))
  }
}

# reshape
colnames(plotData)[3] = "boot1"
plotData = plotData %>% gather(plotData, response, boot1:boot1001)  
colnames(plotData) = c("temp", "model","response")  

########################################################################
########################################################################
########################################################################

plotData %>% 
  group_by(model) %>%
  filter(response == max(response)) -> 
  temps

print(temps)

temps %>% 
  filter(!(model=='boot1')) %>% 
  pull(temp) -> 
  temps

print(fivenum(temps))

print(quantile(temps, 0.025))
print(quantile(temps, 0.975))
