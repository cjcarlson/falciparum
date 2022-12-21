
########################################################################
# This script plots the main prevalence-temperature dose-response function 
# as well as its uncertainty over 1,000 bootstrap samples 
########################################################################

user = "Colin" # "Tamma" #
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/'
  repo = 'C:/Users/cjcar/Documents/Github/falciparum/'
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else {
  wd = NA
  print('Script not configured for this user!')
}

setwd(wd)

# source functions from previous script
source(file.path(repo,'Pipeline/A - Utility functions/A02 - Utility code for plotting.R'))

# packages
library(ggplot2)
library(lfe)
library(reshape)
library(tidyverse)
library(cowplot)
library(tidyr)
library(zoo)
library(lubridate)

########################################################################
# A. Read in saved regression results
########################################################################

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
for(mod in 2:dim(df)[1]){ #dim(df)[1]
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

# plot
g = ggplot()  +
  geom_hline(yintercept = 0, color="darkgrey",alpha=.5) + 
  geom_line(data = subset(plotData,model!="boot1"),
            aes(x = x, y = response, group = model), color = "#C1657C", alpha=.1) +
  geom_line(data = subset(plotData,model=="boot1"), 
            mapping = aes(x = x, y = response), color = "black", size = 1) +
  theme_bw() + 
  labs(x = expression(paste("Mean temperature (",degree,"C")), y = "Effect on prevalence (%)") + 
  xlim(Tmin,Tmax) + 
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 5),
        plot.margin = unit(c(0.3,0.3,1,1), units = "cm"))
g

########################################################################
# D. Lagged drought and flood responses
########################################################################

# reformat: want a dataset of lag x var x model for flood and drought 
# subset to flood and drought
mycols = c(colnames(df)[grep("flood", colnames(df))], colnames(df)[grep("drought", colnames(df))])
rain = df %>% select(all_of(mycols),model)

# calculate cumulative effect 
rain %>% select(flood:flood.lag3) %>% rowSums(na.rm=TRUE) -> rain$flood.cumu
rain %>% select(drought:drought.lag3) %>% rowSums(na.rm=TRUE) -> rain$drought.cumu
rain = rain %>% relocate(model)

# reshape long & format lags
rain = rain %>% gather(rain, response, flood:drought.cumu) 
colnames(rain) = c("model","var","response")
rain$lagst = sapply(strsplit(as.character(rain$var), "\\."), `[`, 2)
rain$lag = ifelse(rain$lagst=="lag3",3,NA)
rain$lag = ifelse(rain$lagst=="lag2",2, rain$lag)
rain$lag = ifelse(rain$lagst=="lag",1,rain$lag)
rain$lag = ifelse(rain$lagst=="cumu",-1,rain$lag)
rain$lag = ifelse(is.na(rain$lag),0,rain$lag)

rain$lagst = ifelse(is.na(rain$lagst), "cont", rain$lagst)
rain$var = ifelse(grepl("flood",rain$var), "flood",rain$var)
rain$var = ifelse(grepl("drought",rain$var), "drought",rain$var)

# plot - flood
f = ggplot() + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype="solid", color="darkgrey",alpha=.5) +
  geom_point(data = subset(rain,model!="main" & var=="flood"), 
             aes(x=factor(lag),y=response), 
             color = "#43A7BA", alpha = .1, size = 1.75)  +
  geom_point(data = subset(rain,model=="main" & var=="flood"), 
             aes(x=factor(lag),y=response), 
             color = "black", alpha = 1, size = 4) +
  geom_vline(xintercept = -0.5, linetype="dashed") + 
  ylab(NULL) + xlab("Flood (month lags)") + 
  scale_y_continuous(breaks=c(-8, -4,  0, 4)) + 
  scale_x_discrete(breaks=c("-1","0","1","2","3"),
                   labels=c("cumulative", "0", "1", "2", "3")) +
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 5),
        plot.margin = unit(c(0.3,0.3,1,0), units = "cm")) + ylim(-8,4)
f

# plot- drought
d = ggplot() + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype="solid", color="darkgrey",alpha=.5) +
  geom_point(data=subset(rain,model!="main" & var=="drought"), 
             aes(x=factor(lag),y=response), color = "#C99776", 
             alpha = .05, size = 1.75)  +
  geom_point(data = subset(rain,model=="main" & var=="drought"), 
             aes(x=factor(lag),y=response), color = "black", 
             alpha = 1, size = 4) +
  geom_vline(xintercept = -0.5, linetype="dashed") + 
  ylab(NULL) + xlab("Drought (month lags)") +
  scale_x_discrete(breaks=c("-1","0","1","2","3"),
                   labels=c("cumulative", "0", "1", "2", "3")) +
  theme(axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 0),
        plot.margin = unit(c(0.3,0.3,1,0), units = "cm")) + ylim(-8,4)
d

########################################################################
# Middle row multipanel
########################################################################

library(patchwork)
g + f + d
