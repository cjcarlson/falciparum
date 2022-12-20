
library(ncdf4)
library(raster)
library(reshape)
library(rgdal)
library(sp)
library(tidyverse)
library(velox)

swirl <- function(input.raster) {
  input.raster <- flip(t(input.raster),direction='y')
  extent(input.raster) <- c(-180,180,-90,90)
  input.raster = crop(input.raster, c(-30,60,-37,40))
  return(input.raster)
}



nct <- nc_open("C:/Users/cjcar/Dropbox/MalariaAttribution/Data/CRU_TS403_data/tmp/cru_ts4.03.1901.2018.tmp.dat.nc/cru_ts4.03.1901.2018.tmp.dat.nc")

g <- ncvar_get(nct, 'tmp')

#############

setwd('C:/Users/cjcar/Dropbox/MalariaAttribution/Data')
cont <- readOGR('AfricaADM1.shp')
month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

########################### R0

for (i in 1:1416) {
  
  r <- swirl(raster(g[,,i]))
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'temp',sep='.')
  cont@data[,name] <- unlist(lapply(velox(r)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r*r
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'temp2',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  
  c <- r^3
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'temp3',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r^4
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'temp4',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r^5
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'temp5',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  print(i)
}

####################


ncp <- nc_open("C:/Users/cjcar/Dropbox/MalariaAttribution/Data/CRU_TS403_data/pr/cru_ts4.03.1901.2018.pre.dat.nc/cru_ts4.03.1901.2018.pre.dat.nc")
g <- ncvar_get(ncp, 'pre')

for (i in 1:1416) {
  r <- swirl(raster(g[,,i]))
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt',sep='.')
  cont@data[,name] <- unlist(lapply(velox(r)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r*r
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt2',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r^3
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt3',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r^4
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt4',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r^5
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt5',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  print(i)
}

###########

library(reshape)
contdf <- cont@data
contdf <- contdf[,-c(2:15)]
contdf <- melt(contdf, id='OBJECTID')

dffix <- separate(contdf,  variable, into=c('month','year','var'), sep='\\.')
dffix <- cast(dffix, OBJECTID+month+year ~ var)

write.csv(dffix, '~/Github/falciparum/TempFiles/ADM1-CRU.csv.gz')
