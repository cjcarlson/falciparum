
library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(velox)


swirl <- function(input.raster) {
  input.raster <- flip(t(input.raster),direction='y')
  extent(input.raster) <- c(-180,180,-90,90)
  input.raster = crop(input.raster, c(-30,60,-37,40))
  return(input.raster)
}


r0t <- function(T, na.rm=TRUE) {
  suppressWarnings(if(is.na(T)) return(NA))
  if(T<=14) {return(0)}
  a = 0.000203*T*(T-11.7)*((42.3-T)^0.5)
  bc = -0.54*T*T + 25.2*T - 206
  p = -0.000828*T*T + 0.0367*T + 0.522 # e^-mu
  mu = -1*log(p)
  PDR = 0.000111*T*(T-14.7)*((34.4-T)^0.5) # 1/EIP
  pEA = -0.00924*T*T + 0.453*T - 4.77
  MDR = 0.000111*T*(T-14.7)*((34-T)^0.5) # 1/tauEA
  EFD = -0.153*T*T + 8.61*T - 97.7
  
  R0 = (((a^2)*bc*(p^(1/PDR))*EFD*pEA*MDR)/(mu^3))^(1/2)
  if(is.nan(R0)){return(0)}
  return(R0/87.13333) # that's the max
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
  print(i)
  
  c <- r^3
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt3',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  print(i)
  
  c <- r^4
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt4',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  print(i)
  
  c <- r^5
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt5',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  print(i)
}







############

prev<- read.csv('./dataverse_files/00 Africa 1900-2015 SSA PR database (260617).csv')
prev <- SpatialPointsDataFrame(coords=prev[,c('Long','Lat')], data=prev[,c('MM','YY','Pf','PfPR2.10')])
prev@proj4string <- cont@proj4string

overtest <- over(cont, prev, returnList = TRUE)

library(tidyr)
library(dplyr)

for (i in 1:1416) {
  mon <- 1+(i-1)%%12
  yr <- ((i-1 - (i-1)%%12)/12)+1900
  
  pf <- function(x) {x<-x[x$MM==mon,]
                      x<-x[x$YY==yr,]
                      mean(x$Pf)}
  pfr <- function(x) {x<-x[x$MM==mon,]
                      x<-x[x$YY==yr,]
                      mean(x$PfPR2.10)}
  
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'PfPR2',sep='.')
  cont@data[,name] <- unlist(lapply(overtest, pfr))
  
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'Pf',sep='.')
  cont@data[,name] <- unlist(lapply(overtest, pf))
  
  print(i)
}

###########

library(reshape)
contdf <- cont@data
contdf <- contdf[,-c(2:15)]
contdf <- melt(contdf, id='OBJECTID')

dffix <- separate(contdf,  variable, into=c('month','year','var'), sep='\\.')
#dffix <- cast(dffix, OBJECTID+month+year ~ var) # Why did I do it this way originally 
dffix <- dffix %>% pivot_wider(names_from = 'var', values_from = 'value')
dffix$year <- as.numeric(dffix$year)

readr::write_csv(dffix, '~/Github/falciparum/Climate/CRU-Reextraction-Aug2022.csv')
