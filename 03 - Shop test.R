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
library(plyr)
library(broom)
library(data.table)
library(lme4)

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
temp <- readOGR('AfricaADM1.shp')
temp2 <- gSimplify(temp,tol=0.05, topologyPreserve=TRUE)
temp2 <- spTransform(temp2, center=FALSE) 
temp2 <- gBuffer(temp2, byid=TRUE, width=0)
cont = SpatialPolygonsDataFrame(temp2, data=temp@data)
#cont@data$OBJECTID = as.numeric(cont@data$OBJECTID)

month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

first <- data.frame(R0 = cont$Jan.1900.R0,
                    Pf = cont$Jan.1900.Pf,
                    Pf2 = cont$Jan.1900.PfPR2,
                    temp = cont$Jan.1900.temp,
                    temp2 = cont$Jan.1900.temp2,
                    ppt = cont$Jan.1900.ppt,
                    ppt2 = cont$Jan.1900.ppt2)
slope2 <- function(x, y){
  return(cov(x, y)/var(x))
}

drops = c("Pf","PfPR2")
edited = data[,!(names(data) %in% drops)]
edited2 = edited[complete.cases(edited), ]
grouped = edited2 %>% group_by(OBJECTID)
#lms = grouped %>% do(model = summary(lm(temp ~ year, data = .)))
#below line to be taken out later
#models = lms$model

#models <- edited2 %>% group_by(OBJECTID) %>% summarise(mod = list(lm(temp ~ year)))

#dat <- data.table(x=edited2$year, y=edited2$temp, grp=edited2$OBJECTID)
#model = dat[,coef(lm(temp~year)),by=grp]

model = lmList(temp ~ year | OBJECTID, data=edited2)
rows = rownames(coef(model))
coefs = coef(model)$year
df <- data.frame(matrix(ncol = 2, nrow = 847))
x <- c("OBJECTID", "cors")
colnames(df) <- x
df$OBJECTID = rows
df$cors = coefs
df$OBJECTID <- factor(df$OBJECTID)

meancovs = c('temp','ppt')
avgdf = data %>% group_by(OBJECTID) %>% summarize_at(meancovs, mean, na.rm = T)
avgdf$OBJECTID <- factor(avgdf$OBJECTID)

#completeFun <- function(data, desiredCols) {
#  completeVec <- complete.cases(data[, desiredCols])
#  return(data[completeVec, ])
#}

#cont <- cont[cont@data$OBJECTID %in% avgdf$OBJECTID,]
##changed df from avgdf to df
cont@data <- left_join(cont@data, df, by = c("OBJECTID" = "OBJECTID"))
cont@data <- left_join(cont@data, avgdf, by = c("OBJECTID" = "OBJECTID"))
#cont@data <- completeFun(cont@data,'ppt')

cont@data$id = rownames(cont@data)
cont.points = fortify(cont, region ="id" )
cont.df = merge(cont.points, cont@data, by="id")


to_plot = cont.df[cont.df$NAME_0 == 'Nigeria',]
#to_plot = cont.df[seq(1, nrow(cont.df), 5), ]
#to_plot = cont.df

plot_list <- list("temp","cors","ppt")

for (value in plot_list) {
gg <- ggplot() + geom_polygon(data = to_plot, aes(x=long, y=lat, group = group,
                                                                      fill = value),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = "yellow", mid="red", high = "blue", midpoint = median(to_plot[,value], na.rm = TRUE), guide = guide_colorbar(title=value)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= paste(value,"graph",sep = " ", collapse = NULL),
       x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
gg
ggsave(paste(value,'.png',sep="",collapse=NULL))}
