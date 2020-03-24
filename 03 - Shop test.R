rm(list = ls())

library(RColorBrewer)
library(colorRamps) 
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
#cont@data$OBJECTID = as.numeric(as.character(cont@data$OBJECTID))
#cont@data$OBJECTID = as.numeric(cont@data$OBJECTID)


drops = c("Pf","PfPR2")
edited = data[,!(names(data) %in% drops)]
edited2 = edited[complete.cases(edited), ]
pre_edited2 = edited2[edited2$year < 1980,]
post_edited2 = edited2[edited2$year >= 1980,]
#lms = grouped %>% do(model = summary(lm(temp ~ year, data = .)))
#below line to be taken out later
#models = lms$model

#models <- edited2 %>% group_by(OBJECTID) %>% summarise(mod = list(lm(temp ~ year)))

#dat <- data.table(x=edited2$year, y=edited2$temp, grp=edited2$OBJECTID)
#model = dat[,coef(lm(temp~year)),by=grp]

full_temp_model = lmList(temp ~ year | OBJECTID, data=edited2)
pre_temp_model = lmList(temp ~ year | OBJECTID, data=pre_edited2)
post_temp_model = lmList(temp ~ year | OBJECTID, data=post_edited2)
full_ppt_model = lmList(ppt ~ year | OBJECTID, data=edited2)
pre_ppt_model = lmList(ppt ~ year | OBJECTID, data=pre_edited2)
post_ppt_model = lmList(ppt ~ year | OBJECTID, data=post_edited2)
full_temp_coefs = coef(full_temp_model)$year
pre_temp_coefs = coef(pre_temp_model)$year
post_temp_coefs = coef(post_temp_model)$year
full_ppt_coefs = coef(full_ppt_model)$year
pre_ppt_coefs = coef(pre_ppt_model)$year
post_ppt_coefs = coef(post_ppt_model)$year
rows = rownames(coef(full_temp_model))
df <- data.frame(matrix(ncol = 2, nrow = 847))
x <- c("OBJECTID", "full_temp_cors","pre_temp_cors","post_temp_cors","full_ppt_cors","pre_ppt_cors","post_ppt_cors")
colnames(df) <- x
df$OBJECTID = rows
df$full_temp_cors = full_temp_coefs
df$pre_temp_cors = pre_temp_coefs
df$post_temp_cors = post_temp_coefs
df$full_ppt_cors = full_ppt_coefs
df$pre_ppt_cors = pre_ppt_coefs
df$post_ppt_cors = post_ppt_coefs
#df$OBJECTID = as.numeric(as.character(df$OBJECTID))
#df$OBJECTID <- factor(df$OBJECTID)

meancovs = c('temp','ppt')
avgdf = data %>% group_by(OBJECTID) %>% summarize_at(meancovs, mean, na.rm = T)
#avgdf$OBJECTID = as.numeric(as.character(avgdf$OBJECTID))
avgdf$OBJECTID <- factor(avgdf$OBJECTID)

#completeFun <- function(data, desiredCols) {
#  completeVec <- complete.cases(data[, desiredCols])
#  return(data[completeVec, ])
#}

##changed df from avgdf to df
cont@data <- left_join(cont@data, df, by = c("OBJECTID" = "OBJECTID"))
cont@data <- left_join(cont@data, avgdf, by = c("OBJECTID" = "OBJECTID"))

cont@data$id = rownames(cont@data)
cont.points = fortify(cont, region ="id" )
cont.df = merge(cont.points, cont@data, by="id")

#to_plot = cont.df[cont.df$NAME_0 == 'Nigeria',]
#to_plot = cont.df[seq(1, nrow(cont.df), 5), ]
to_plot = cont.df

# plot_list <- list("temp","cors","ppt")
# plot_list2 <- list("temp")
# 
# for (value in plot_list) {
# gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
#                                                                       fill = as.numeric(to_plot[,value])),color=NA) +
#   guides(colour="none") +
#   #geom_line(linetype="dotted", color="black", size=5) +
#   coord_fixed()+
#   scale_fill_gradient2(low = '#e6e6ff', mid="#0000ff", high = "#000080", midpoint = median(to_plot[,value], na.rm = TRUE), guide = guide_colorbar(title=value)) +
#   #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
#   #scale_fill_brewer(palette="RdYlBu") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
#   labs(title= paste(value,"graph",sep = " ", collapse = NULL),x="Longitude", y= "Latitude")
# ## cut out formatting fluff for debugging
# #gg
# ggsave(paste(value,'.png',sep="",collapse=NULL))}

# TEMP CORRELATIONS
gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
                                                              fill = as.numeric(to_plot[,'full_temp_cors'])),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = '#ffd9b3', mid="#ff8000", high = "#804000", midpoint = median(to_plot[,'full_temp_cors'], na.rm = TRUE), guide = guide_colorbar(title='Correlation Coefficients (ºC/year)')) +
  #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
  #scale_fill_brewer(palette="RdYlBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= "Temperature Increase per Year 1900 - 2017",x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
ggsave('full_temp_cors.png')

gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
                                                              fill = as.numeric(to_plot[,'pre_temp_cors'])),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = '#ffd9b3', mid="#ff8000", high = "#804000", midpoint = median(to_plot[,'pre_temp_cors'], na.rm = TRUE), guide = guide_colorbar(title='Correlation Coefficients (ºC/year)')) +
  #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
  #scale_fill_brewer(palette="RdYlBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= "Temperature Increase per Year 1900 - 1979",x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
ggsave('pre_temp_cors.png')

gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
                                                              fill = as.numeric(to_plot[,'post_temp_cors'])),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = '#ffd9b3', mid="#ff8000", high = "#804000", midpoint = median(to_plot[,'post_temp_cors'], na.rm = TRUE), guide = guide_colorbar(title='Correlation Coefficients (ºC/year)')) +
  #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
  #scale_fill_brewer(palette="RdYlBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= "Temperature Increase per Year 1980 - 2017",x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
ggsave('post_temp_cors.png')

# PPT CORRELATIONS
gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
                                                              fill = as.numeric(to_plot[,'full_ppt_cors'])),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = '#e6e6ff', mid="#0000ff", high = "#000080", midpoint = median(to_plot[,'full_ppt_cors'], na.rm = TRUE), guide = guide_colorbar(title='Correlation Coefficients (cm/year)')) +
  #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
  #scale_fill_brewer(palette="RdYlBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= "Precipitation Increase per Year 1900 - 2017",x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
ggsave('full_ppt_cors.png')

gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
                                                              fill = as.numeric(to_plot[,'post_ppt_cors'])),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = '#e6e6ff', mid="#0000ff", high = "#000080", midpoint = median(to_plot[,'post_ppt_cors'], na.rm = TRUE), guide = guide_colorbar(title='Correlation Coefficients (cm/year)')) +
  #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
  #scale_fill_brewer(palette="RdYlBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= "Precipitation Increase per Year 1980 - 2017",x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
ggsave('post_ppt_cors.png')

gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
                                                              fill = as.numeric(to_plot[,'pre_ppt_cors'])),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = '#e6e6ff', mid="#0000ff", high = "#000080", midpoint = median(to_plot[,'pre_ppt_cors'], na.rm = TRUE), guide = guide_colorbar(title='Correlation Coefficients (cm/year)')) +
  #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
  #scale_fill_brewer(palette="RdYlBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= "Precipitation Increase per Year 1900 - 1979",x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
ggsave('pre_ppt_cors.png')

#TEMPERATURE
gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
                                                              fill = as.numeric(to_plot[,'temp'])),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = '#e6e6ff', mid="#0000ff", high = "#000080", midpoint = median(to_plot[,'temp'], na.rm = TRUE), guide = guide_colorbar(title='Degrees ºC')) +
  #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
  #scale_fill_brewer(palette="RdYlBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= 'Average Temperature',x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
#gg
ggsave('temp.png')

#PRECIPITATION
gg <- ggplot() + geom_polygon(data = to_plot, na.rm=TRUE, aes(x=long, y=lat, group = group,
                                                              fill = as.numeric(to_plot[,'ppt'])),color=NA) +
  guides(colour="none") + 
  #geom_line(linetype="dotted", color="black", size=5) +
  coord_fixed()+
  scale_fill_gradient2(low = '#e6e6ff', mid="#0000ff", high = "#000080", midpoint = median(to_plot[,'ppt'], na.rm = TRUE), guide = guide_colorbar(title='Rainfall (CM)')) +
  #scale_color_gradientn(colours=rev(brewer.pal(4,"RdYlBu")),guide = guide_colorbar(title='value')) +
  #scale_fill_brewer(palette="RdYlBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey92", colour = NA),title= element_text(size=10,hjust = 0.5,vjust = 1,face= c("bold"))) +
  labs(title= 'Average Precipitation',x="Longitude", y= "Latitude") 
## cut out formatting fluff for debugging
#gg
ggsave('ppt.png')
