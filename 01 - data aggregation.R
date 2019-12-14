
### SETUP

library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(velox)

# Some basic functions that do important work below

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

# Some old code that made the Africa shapefile:
#setwd('C:/Users/cjcar/Dropbox/continents/gadm28_adm0')
#cont <- readOGR('gadm28_adm0.shp')
#setwd('C:/Users/cjcar/Dropbox/continents/World')
#cont2 <- readOGR('gadm28_adm1.shp')
#cont3 <- cont2[cont2$NAME_0 %in% unique(cont$NAME_ENGLI[cont$UNREGION2=='Africa']),]
#writeOGR(cont3, 
#         dsn='C:/Users/cjcar/Dropbox/MalariaAttribution/Data',
#         layer='AfricaADM1',
#         driver='ESRI Shapefile')

# Read in Africa

setwd('C:/Users/cjcar/Dropbox/MalariaAttribution/Data')
cont <- readOGR('AfricaADM1.shp')
month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')


### PULL OUT RELEVANT VARIABLES

# Extract temperature and R0 variables


# Read in temperature 
nct <- nc_open("C:/Users/cjcar/Dropbox/MalariaAttribution/Data/CRU_TS403_data/tmp/cru_ts4.03.1901.2018.tmp.dat.nc/cru_ts4.03.1901.2018.tmp.dat.nc")

g <- ncvar_get(nct, 'tmp')

for (i in 1:1416) {
  r <- swirl(raster(g[,,i]))
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'temp',sep='.')
  cont@data[,name] <- unlist(lapply(velox(r)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r*r
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'temp2',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  
  c <- calc(r, r0t) #disaggregate,10
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'R0',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  print(i)
}

####################

# Extract precipitation

ncp <- nc_open("C:/Users/cjcar/Dropbox/MalariaAttribution/Data/CRU_TS403_data/pr/cru_ts4.03.1901.2018.pre.dat.nc/cru_ts4.03.1901.2018.pre.dat.nc")
g <- ncvar_get(ncp, 'pre')


for (i in 1:1416) {
  r <- swirl(raster(g[,,i]))
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'ppt',sep='.')
  cont@data[,name] <- unlist(lapply(velox(r)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  c <- r*r
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'ppt2',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  print(i)
}

### PREVALENCE

prev<- read.csv('./dataverse_files/00 Africa 1900-2015 SSA PR database (260617).csv')
prev <- SpatialPointsDataFrame(coords=prev[,c('Long','Lat')], data=prev[,c('MM','YY','Pf','PfPR2.10')])
prev@proj4string <- cont@proj4string

overtest <- over(cont, prev, returnList = TRUE)

library(tidyr)
library(dplyr)

# Assign a date to each set of prevalence estimates averaged for a given shapefile poly

for (i in 1:1416) {
  mon <- i%%12
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

### Basic GAM checks

# This builds a little dataframe for doing some graphic checks, it isn't used further on really

first <- data.frame(R0 = cont$Jan.1900.R0,
                    Pf = cont$Jan.1900.Pf,
                    Pf2 = cont$Jan.1900.PfPR2,
                    temp = cont$Jan.1900.temp,
                    temp2 = cont$Jan.1900.temp2,
                    ppt = cont$Jan.1900.ppt,
                    ppt2 = cont$Jan.1900.ppt2)
for (i in 2:1416) {
  mon <- i%%12
  yr <- ((i-1 - (i-1)%%12)/12)+1900
  df <- data.frame(R0 = cont[,paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'R0',sep='.')],
                   Pf = cont[,paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'Pf',sep='.')],
                   Pf2 = cont[,paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'PfPR2',sep='.')],
                   temp = cont[,paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'temp',sep='.')],
                   temp2 = cont[,paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'temp2',sep='.')],
                   ppt = cont[,paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'ppt',sep='.')],
                   ppt = cont[,paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.'),'ppt2',sep='.')])
  colnames(df) <- colnames(first)
  first <- rbind(first, df)
  print(i)
}

setwd("C:/Users/cjcar/Dropbox/MalariaAttribution/Dataframe backups")
write.csv(first, 'unformatted-backup.csv')
write.csv(data.frame(cont@data), 'shapefile-backup.csv')

firstcomplete <- first[firstcomplete.cases(first),]
colnames(firstcomplete) <- c('R0',"Pf","Pf2","temp","temp2","ppt","ppt2")

library(ggplot2)

# Some basic data check plots

firstcomplete$Pf3 <- firstcomplete$Pf2/100
ggplot(firstcomplete, aes(R0, Pf3)) + geom_point(col='light grey') + 
  geom_smooth(col='red') + ylab('Observed prevalence of P. falciparum') + 
  xlab('Estimated scaled R0 based on temp.') + theme_classic() 

ggplot(firstcomplete, aes(temp, Pf3)) + #geom_point(col='light grey') + 
  geom_smooth(col='red') + ylab('Observed prevalence of P. falciparum') + 
  xlab('Temperature') + theme_classic() 

ggplot(firstcomplete, aes(ppt, Pf3)) + #geom_point(col='light grey') + 
  geom_smooth(col='red') + ylab('Observed prevalence of P. falciparum') + 
  xlab('Precipitation') + theme_classic() 


### Save out data backups

# Reconstitute the spatial data frame, "cont", without running the previous script
setwd('C:/Users/cjcar/Dropbox/MalariaAttribution')
cont <- readOGR('./Data/AfricaADM1.shp')
cont@data <- read.csv('./Dataframe backups/shapefile-backup.csv')

# Pull out the dataframe, remove all extraneous metadata, make a dataset
contdf <- cont@data
contdf <- contdf[,-c(1,3:16)]
contdf <- melt(contdf, id='OBJECTID')

#Separate the time component back out and save out
dffix <- separate(contdf,  variable, into=c('month','year','var'), sep='\\.')
dffix <- cast(dffix, OBJECTID+month+year ~ var)
dffix$year <- as.numeric(dffix$year)
write.csv(dffix, './Dataframe backups/formatted-backup.csv')
