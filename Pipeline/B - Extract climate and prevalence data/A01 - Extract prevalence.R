
library(ncdf4)
library(raster)
library(reshape)
library(rgdal)
library(sp)
library(tidyverse)

swirl <- function(input.raster) {
  input.raster <- flip(t(input.raster),direction='y')
  extent(input.raster) <- c(-180,180,-90,90)
  input.raster = crop(input.raster, c(-30,60,-37,40))
  return(input.raster)
}

setwd("C:/Users/cjcar/Dropbox/MalariaAttribution")


cont <- readOGR('Data/AfricaADM1.shp')

month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

prev <- read_csv('Data/dataverse_files/00 Africa 1900-2015 SSA PR database (260617).csv')
prev <- SpatialPointsDataFrame(coords=prev[,c('Long','Lat')], data=prev[,c('MM','YY','Pf','PfPR2-10')])
prev@proj4string <- cont@proj4string

### SPOT CHECK
range(prev$YY)
###

overtest <- over(cont, prev, returnList = TRUE)

for (i in 1:1404) {
  
  mon <- (i-1)%%12 + 1
  yr <- ((i-1 - (i-1)%%12)/12)+1900
  
  pfr <- function(x) {x<-x[x$MM==mon,]
                      x<-x[x$YY==yr,]
                      mean(x$`PfPR2-10`, na.rm = TRUE)}
  
  name =paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1900,sep='.')
  
  cont@data[,name] <- unlist(lapply(overtest, pfr))
  
  print(i)
  
}

###########

contdf <- cont@data
contdf <- contdf[,-c(2:15)]
contdf <- melt(contdf, id='OBJECTID')

dffix <- separate(contdf,  variable, into=c('month','year'), sep='\\.')
dffix %>% 
  rename(PfPR2 = value) %>%
  filter(!is.na(PfPR2)) -> dffix

write.csv(dffix, '~/Github/falciparum/TempFiles/ADM1-Prevalence.csv')
