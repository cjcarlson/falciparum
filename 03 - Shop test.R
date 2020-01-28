rm(list = ls())

library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(velox)
# packages
#library(cowplot)
library(ggplot2)
#library(ggthemr)
library(lfe)
library(reshape)
library(tidyverse)
library(zoo)
library(lubridate)
library(readr)
library(sp)
library(rgeos)

user = "Shop" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution/Data'
  repo = "" #to fill in
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution/Data'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else if (user == "Shop") {
  wd = '/Users/bublai/Dropbox/ruins_remote_sensing/malaria'
  repo = '/Users/bublai/Desktop/research/falciparum/'
  } else {
  wd = NA
  print('Script not configured for this user!')
  }

setwd(wd)

# source functions from previous script
source(file.path(repo,'code/R_utils.R'))
source(file.path(repo,'code/utils_plotting.R'))
############
# Read in the data backup
data <- read_csv('./formatted-backup.csv')
#complete <- data[complete.cases(data),]
cont <- readOGR('AfricaADM1.shp')
#cont@data$OBJECTID = as.numeric(cont@data$OBJECTID)

month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

first <- data.frame(R0 = cont$Jan.1900.R0,
                    Pf = cont$Jan.1900.Pf,
                    Pf2 = cont$Jan.1900.PfPR2,
                    temp = cont$Jan.1900.temp,
                    temp2 = cont$Jan.1900.temp2,
                    ppt = cont$Jan.1900.ppt,
                    ppt2 = cont$Jan.1900.ppt2)

meancovs = c('temp','ppt')
avgdf = data %>% group_by(OBJECTID) %>% summarize_at(meancovs, mean, na.rm = T)
avgdf$OBJECTID <- factor(avgdf$OBJECTID)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#cont <- cont[cont@data$OBJECTID %in% avgdf$OBJECTID,]
cont@data <- left_join(cont@data, avgdf, by = c("OBJECTID" = "OBJECTID"))
#cont@data <- completeFun(cont@data,'ppt')

cont@data$id = rownames(cont@data)
cont.points = fortify(cont, region ="id" )
cont.df = merge(cont.points, cont@data, by="id")

to_plot = cont.df[cont.df$NAME_0 == 'Algeria',]
gg <- ggplot() + geom_polygon(data = to_plot, aes(x=long, y=lat, group = group,
                                                                      fill = ppt,colour="black")) +
  geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = "yellow", mid="red", high = "blue", midpoint = mean(to_plot[,'ppt']), guide = guide_colorbar(title="Precipitation")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= "Test graph",
       x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
gg

gg <- ggplot() + geom_polygon(data = cont.df[seq(1, 4251569,5),], aes(x=long, y=lat, group = group,
                                                                      fill = ppt)) +
  coord_fixed()+
  scale_fill_gradient2(low = "yellow", mid="red", high = "blue", midpoint = 100, guide = guide_colorbar(title="Precipitation")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= "Test graph",
       x="Longitude", y= "Latitude") 

