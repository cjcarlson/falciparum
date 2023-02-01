
cont <- readOGR(dsn = 'C:/Users/cjcar/Dropbox/MalariaAttribution/Data', layer =  'AfricaADM1')
month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

########################### 

for (i in 1:1368) { #1416
  r <- nct[[i]]
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
  print(i)
}

####################

for (i in 1:1368) { #1416
  r <- ncp[[i]]
  name=paste(paste(month[(i-1)%%12 + 1], ((i-1 - (i-1)%%12)/12)+1901,sep='.'),'ppt',sep='.')
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

write.csv(dffix, paste("~/Github/falciparum/Climate/Historical",paste(outname,'.csv',sep=''), sep='/'))

