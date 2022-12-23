### SET UP
#rm(list = ls())

user = "Colin" #"Colin"
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
source(file.path(repo,'Pipeline/A - Utility functions/A01 - Utility code for calculations.R'))
source(file.path(repo,'Pipeline/A - Utility functions/A02 - Utility code for plotting.R'))

# packages
library(ggplot2)
library(lfe)
library(reshape)
library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)

############

# load main model (full sample)
main <- readRDS("./Results/Models/coefficients_cXt2intrXm.rds")
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

files = list.files(file.path("Results","Models","bootstrap"))
stop = length(files)-1
files=files[1:stop]

for(f in 1:length(files)){
  tmp = readRDS(file.path("Results","Models","bootstrap",files[f]))
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
for(mod in 1:dim(df)[1]){ #dim(df)[1]
  sub = df[mod,]
  b = as.matrix(c(sub$temp,sub$temp2))
  boot = as.data.frame(as.matrix(xValsT) %*% b) #Prediction
  colnames(boot) = paste0("boot",mod)
  plotData = cbind(plotData, boot)
  
  # progress
  if(mod/100==round(mod/100)) {
    print(paste0('--------- DONE WITH ITERATION ', mod, ' of 1000 --------'))
  }
}

# reshape
colnames(plotData)[3] = "boot1"
plotData = plotData %>% gather(plotData, response, boot1:boot1001)  
colnames(plotData) = c("x", "model","response")  

#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

#### Read in the data backup

data <- read.csv(file.path(wd,"Data/CRU-Reextraction-Aug2022.csv"))
# Keep an eye out for logical data parse issue

# Generate the R0 curves:
data$predR0 <- sapply(data$temp, r0t)

#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

data %>%
  ggplot(aes(x = temp, y = predR0)) + 
  geom_vline(xintercept = 25.56, color = 'dark grey', lwd = 1, linetype = 'longdash') +
  geom_line(color = "black", lwd = 0.7) + 
  xlim(c(15,35)) + 
  labs(x = expression(paste("Temperature (",degree,"C)")),
       y = expression('R'[0]*' predicted')) + 
  theme_bw() -> g1


data %>%
  ggplot(aes(x = temp, y = predR0)) + 
  geom_vline(xintercept = 25.56, color = 'dark grey', lwd = 0.7, linetype = 'longdash') +
#  geom_vline(xintercept = optg2, color = "#C1657C", lwd = 0.7, linetype = 'longdash') +
  geom_smooth(aes(y = PfPR2), lwd=1, color = "#C1657C", fill='light grey') + 
  xlim(c(15,35)) +
  labs(x = expression(paste("Temperature (",degree,"C)")),
       y = "Prevalence (%, raw data)")  + 
  theme_bw() -> g2 
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
  theme_bw() -> g2 

plotData[(plotData$model=='boot1'),] -> sm
sm$x[sm$response == max(sm$response)] -> optg3
plotData[plotData$model=='boot1',] %>%
  ggplot(aes(x = x, y = response)) + 
  geom_vline(xintercept = 25.56, color = 'dark grey', lwd = 0.7, linetype = 'longdash') +
  geom_vline(xintercept = optg3, color = "#C1657C", lwd = 0.7, linetype = 'longdash') +
  geom_line(color = "#C1657C", lwd = 1) + 
  xlim(c(15,35)) + 
  ylim(c(-9,0.2)) + 
  labs(x = expression(paste("Temperature (",degree,"C)")),
       y = "Prevalence (%, modeled)") + 
  theme_bw() -> g3

g1 + g2 + g3 + plot_annotation(tag_levels = 'A')
