#Maps of Attribution analysis results

library(data.table)
library(tidyverse)
#show more significant figures in the display
#options(pillar.sigfig = 7)

setwd("/Users/christophertrisos/Dropbox/MalariaAttribution/IterationFiles/")

#get metadata
metadat<-fread("Historical/RowMetadata.csv")

#get differences in prevalence between Historical NoGHG and Historical with GHG

#get file names
filenames<-list.files(paste0(getwd(),"/Historical"))
#set number of iterations
num.iter<-1000
#make list to store outputs
median.diff.dat<-as.list(rep(NA,num.iter))

#start loop
for (i in 1:num.iter){

#load data
maldat<-fread("Historical/iter1.csv") 
#For MAKING A LOOP 
#maldat<-fread(paste0("Historical/",filenames[i]))

#join malaria and metadata
alldat<-cbind(metadat,maldat)
#remove individual data files
rm(maldat,metadat)

#get only 2014 data
endyrdat<-subset(alldat,year==2014)
#remove all data
rm(alldat)

#get mean across months for 2014
meandat<-endyrdat %>%
  group_by(OBJECTID,GCM,scenario) %>%
  summarise(mean=mean(Pred, na.rm=TRUE)) #!!! change when data fixed to have no NAs

#get differences between Nat and Hist climates for each admin1 by GCM
diffnathist<-meandat %>%
pivot_wider (names_from=scenario, values_from=mean) %>%
mutate(diff = nat - hist)

#store data in the list
median.diff.dat[[i]]<-diffnathist
#Colin end loop over csv file iterations
}

#join all the tibbles together or Colin do something clever to this effect
do.call()
#get median value for 2014 for each admin1xscenario combination


################# Make the maps ##########################

med.val<-




#admin units
cont <- readOGR('./Data/AfricaADM1.shp')