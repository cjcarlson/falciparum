
library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(sf)
library(lubridate)
library(rgdal)
library(patchwork)
library(multiscales)

setwd("D:/MalariaAfrica/FutureTempFiles")
setDTthreads(1L)
meta <- fread("RowMetadata.csv", select = c('run','year','OBJECTID'))
meta %<>% tidyr::extract(run, 
               into = c('GCM','RCP'),
               regex = "(BCC-CSM2|BCC-CSM2-MR|CanESM5|CESM2|CNRM-CM6|GFDL-ESM4|GISS-E2|HadGEM3|IPSL-CM6A|MIROC6|MRI-ESM2|NorESM2)-(rcp26|rcp45|rcp85)",
               remove = FALSE)
rows <- which((meta$RCP %in% c('rcp26','rcp85')) & (meta$year %in% c(2015:2019, 2096:2100)))
meta <- meta[rows,]
# Yes I know the below solution is a little janky, god forgive me 
meta$year[meta$year %in% c(2015:2019)] <- 2015
meta$year[meta$year %in% c(2096:2100)] <- 2100

for (i in 1:10) {  
  iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
  iter <- iter[rows,]
  iter <- bind_cols(meta, iter)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'RCP,GCM,year,OBJECTID']
  if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
} 

iter.df %<>% as_tibble()
iter.df %<>% pivot_longer(cols = -c(RCP, GCM, year, OBJECTID), names_to = c("Pred"))

vroom::vroom_write(iter.df, "~/Github/falciparum/TempFiles/Fig3Big2100.csv")
