
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
library(tidyr)
library(zoo)
library(lubridate)

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

########################################################################
# B. Histogram of optimal temperatures
########################################################################

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

# limit to middle 99.5% of the data
quants = quantile(df$optT, probs=c(.01,.9925))
dfsub = subset(df, optT>=quants[1] & optT<=quants[2])
medoptT = median(dfsub$optT)
g = ggplot(data=dfsub, aes(optT)) + 
  geom_density(col="grey", 
               fill="grey", 
               alpha = .2) + 
  labs(title="Histogram of optimal temperatures", x="Optimal T (C)", y="density") + 
  geom_vline(xintercept=24, color="red", size=.5) + 
  geom_vline(xintercept=medoptT, color="blue", size=.5) + theme_classic() +
  annotate(geom="text", x=18, y=0.2, label="Theoretical benchmark",
           color="red") +
  annotate(geom="segment", x=22, xend=23.8, y=0.2, yend=0.2, arrow = arrow(length = unit(.2,"cm")),
           color="red") +
annotate(geom="text", x=17.5, y=0.22, label="Empirical median",
         color="blue") +
  annotate(geom="segment", x=20.6, xend=22.4, y=0.22, yend=0.22, arrow = arrow(length = unit(.2,"cm")),
           color="blue") +
  theme(text = element_text(size=18))
g
fileout = file.path("Results", "Figures", "Fig1","clipped_01_9925_density_optimalT.pdf")
ggsave(fileout, plot=g)

########################################################################
# C. Spaghetti plot of estimated T response functions
########################################################################

#setup 
Tref = 24
Tmin = 10
Tmax = 40
int = 1
plotXtemp = cbind(seq(Tmin,Tmax,by=int), seq(Tmin,Tmax,by=int)^2)
xValsT = genRecenteredXVals_polynomial(plotXtemp,Tref,2)

# point estimate
main = subset(df, model=="main")
b = as.matrix(c(main$temp, main$temp2))
response = as.matrix(xValsT) %*% b #Prediction

plotData = data.frame(x = xValsT[,1] + Tref, response = response)

  g = ggplot(data = plotData) + geom_line(mapping = aes(x = x, y = response), color = "black", size=1.5) +
    theme_classic() + geom_hline(yintercept = 0) + labs(x = "Daily average temperature (C)" , y = "Prevalence") +
    theme(text = element_text(size=16))
  g

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
colnames(plotData)[2] = "boot1"
plotData = plotData %>% gather(plotData, response, boot1:boot996)  
colnames(plotData) = c("x", "model","response")  

# plot
g = ggplot()  +
  geom_line(data=subset(plotData,model!="boot1"),aes(x = x, y = response, group = model), color = "coral3", alpha=.05) +
  geom_line(data = subset(plotData,model=="boot1"), mapping = aes(x = x, y = response), color = "black", size=1.5) +
  theme_cowplot() + geom_hline(yintercept = 0) + labs(x = "Daily average temperature (C)" , y = "Prevalence") 
g

fileout = file.path("Results", "Figures", "Fig1","Tresponse_bootstrapped.pdf")
ggsave(fileout, plot=g)

# Add temperature histogram underneath
data <- read.csv('./Dataframe backups/formatted-backup.csv')
#### STANDARD FILTERING & AUGMENTING
spatial <- read.csv('./Dataframe backups/shapefile-backup.csv')
countrydf <- unique(spatial[,c('OBJECTID','NAME_0')])
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
iso = data %>% group_by(country, month, year) %>% summarize_all(mean, na.rm=T)
data_iso <- iso[complete.cases(iso),]
data$yearnum <- data$year
data$year <- factor(data$year)
data %>% unite("monthyr", month:year, sep=' ', remove=FALSE) %>% 
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>% 
  mutate(monthyr = as.numeric(ymd(monthyr)-ymd("1900-01-01"))) -> data.reset

complete <- data.reset[complete.cases(data.reset),]
rm(data,spatial,countrydf,iso)

h = ggplot(data=complete, aes(temp)) + geom_histogram(aes(y=..density..),bins=50, colour="darkgrey", fill="grey", alpha=.5) +
  theme_cowplot() + labs(x="Daily average temperature (C)", y = "Density") +  scale_y_continuous(position = "right") #+
  theme(axis.text.x = element_blank())

# combine
#p = plot_grid(g,h,ncol=1, rel_heights = c(4,1), align = "h")
#p
# this seems to work more nicely
aligned_plots <- align_plots(h, g, align="hv", axis="tblr")
p = ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
p
fileout = file.path("Results", "Figures", "Fig1","Tresponse_bootstrapped.pdf")
ggsave(fileout, plot=p)
########################################################################
# D. Lagged drought and flood responses
########################################################################

  