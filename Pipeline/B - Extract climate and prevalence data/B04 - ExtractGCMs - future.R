

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

g <- ncvar_get(nct, 'tas1') # It's tas5 in all the others but switched to tas1 in the RCP2.6 redo

#############

cont <- readOGR(dsn = 'C:/Users/cjcar/Dropbox/MalariaAttribution/Data', layer =  'AfricaADM1')
month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')


########################### R0

for (i in 1:1032) { #1416
  r <- swirl(raster(g[,,i]))
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+2015,sep='.'),'temp',sep='.')
  cont@data[,name] <- unlist(lapply(velox(r)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  
  c <- r*r
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+2015,sep='.'),'temp2',sep='.')
  cont@data[,name] <- unlist(lapply(velox(c)$extract(cont,
                                                     fun = function(x) mean(x, na.rm = TRUE),
                                                     small=TRUE),
                                    mean,na.rm=TRUE))
  print(i)
}

####################

g <- ncvar_get(ncp, 'pr1') # Not sure why this has changed but hm okay!

for (i in 1:1032) { #1416
  r <- swirl(raster(g[,,i]))
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+2015,sep='.'),'ppt',sep='.')
  cont@data[,name] <- unlist(lapply(velox(r)$extract(cont,
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
dffix$year <- as.numeric(dffix$year)

write.csv(dffix, paste("C:/Users/cjcar/Dropbox/MalariaAttribution/ClimateCSVs",paste(outname,'.csv',sep=''), sep='/'))

