
library(sf)
library(lubridate)
library(magrittr)
library(rgdal)
library(tidyverse)
library(patchwork)
library(data.table)

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

setwd("D:/MalariaAfrica/FutureTempFiles")

setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c("year", "run"))

for (i in 1:1000) { 
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = c("Pred","Pf.temp","Pf.flood","Pf.drought"))
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE), 
                      Pf.temp = mean(Pf.temp, na.rm = TRUE),
                      Pf.flood = mean(Pf.flood, na.rm = TRUE),
                      Pf.drought = mean(Pf.drought, na.rm = TRUE)), by = 'run,year']
  iter$iter <- i
  if(i==1) {iter.df <- iter} else {iter.df <- bind_rows(iter.df, iter)}
  print(i)
} 

iter.df %<>% as_tibble()
vroom::vroom_write(iter.df, "~/Github/falciparum/TempFiles/SuppFutureBig.csv")