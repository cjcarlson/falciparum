
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(sf)
library(lubridate)
library(rgdal)
library(patchwork)
library(multiscales)

setwd("D:/MalariaAfrica/HistoricalTempFiles")
setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c('scenario','GCM','year','OBJECTID'))
rows <- which(meta$year %in% c(1901:1905, 2010:2014))
meta <- meta[rows,]
# Yes I know the below solution is a little janky, god forgive me 
meta$year[meta$year %in% c(1901:1905)] <- 1901
meta$year[meta$year %in% c(2010:2014)] <- 2014 

for (i in 1:1000) {  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- iter[rows,]
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,GCM,year,OBJECTID']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} 

iter.df %<>% as_tibble()
iter.df %<>% pivot_longer(cols = -c(scenario, GCM, year, OBJECTID), names_to = c("Pred"))

vroom::vroom_write(iter.df, "~/Github/falciparum/TempFiles/Fig3Big.csv")
