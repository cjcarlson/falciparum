
library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(tidyr)
library(velox)

setwd('G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/ssp126')
                                                                                      
ncp <- brick("pr_Amon_ACCESS-CM2_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")     
nct <- brick("tas_Amon_ACCESS-CM2_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")
outname <- 'ACCESS-CM2-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_ACCESS-ESM1-5_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_ACCESS-ESM1-5_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")   
outname <- 'ACCESS-ESM1-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_BCC-CSM2-MR_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")          
nct <- brick("tas_Amon_BCC-CSM2-MR_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
outname <- 'BCC-CSM2-MR-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_CanESM5_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")
nct <- brick("tas_Amon_CanESM5_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")    
outname <- "CanESM5-rcp26"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_FGOALS-g3_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")       
nct <- brick("tas_Amon_FGOALS-g3_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
outname <- "FGOALS-g3-rcp26"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012_BC_lonlat.nc")    
nct <- brick("tas_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012_BC_lonlat.nc")    
outname <- "GFDL-ESM4-rcp26"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_IPSL-CM6A-LR_ssp126_r1i1p1f1_gr_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_IPSL-CM6A-LR_ssp126_r1i1p1f1_gr_201501-210012_BC_lonlat.nc")  
outname <- 'IPSL-CM6A-LR-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_MIROC6_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_MIROC6_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")   
outname <- 'MIROC6-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_MRI-ESM2-0_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")       
nct <- brick("tas_Amon_MRI-ESM2-0_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")           
outname <- 'MRI-ESM2-0-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")    
nct <- brick("tas_Amon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")        
outname <- 'NorESM2-LM-rcp26'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())


######################

setwd('G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/ssp245')

ncp <- brick("pr_Amon_ACCESS-CM2_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")     
nct <- brick("tas_Amon_ACCESS-CM2_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")
outname <- 'ACCESS-CM2-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_ACCESS-ESM1-5_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_ACCESS-ESM1-5_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")   
outname <- 'ACCESS-ESM1-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")          
nct <- brick("tas_Amon_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
outname <- 'BCC-CSM2-MR-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_CanESM5_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")
nct <- brick("tas_Amon_CanESM5_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")    
outname <- "CanESM5-rcp45"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_FGOALS-g3_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")       
nct <- brick("tas_Amon_FGOALS-g3_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
outname <- "FGOALS-g3-rcp45"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_GFDL-ESM4_ssp245_r1i1p1f1_gr1_201501-210012_BC_lonlat.nc")    
nct <- brick("tas_Amon_GFDL-ESM4_ssp245_r1i1p1f1_gr1_201501-210012_BC_lonlat.nc")    
outname <- "GFDL-ESM4-rcp45"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_IPSL-CM6A-LR_ssp245_r1i1p1f1_gr_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_IPSL-CM6A-LR_ssp245_r1i1p1f1_gr_201501-210012_BC_lonlat.nc")  
outname <- 'IPSL-CM6A-LR-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_MIROC6_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_MIROC6_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")   
outname <- 'MIROC6-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_MRI-ESM2-0_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")       
nct <- brick("tas_Amon_MRI-ESM2-0_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")           
outname <- 'MRI-ESM2-0-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_NorESM2-LM_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")    
nct <- brick("tas_Amon_NorESM2-LM_ssp245_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")        
outname <- 'NorESM2-LM-rcp45'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())


######################

setwd('G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/ssp585')

ncp <- brick("pr_Amon_ACCESS-CM2_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")     
nct <- brick("tas_Amon_ACCESS-CM2_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")
outname <- 'ACCESS-CM2-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_ACCESS-ESM1-5_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_ACCESS-ESM1-5_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")   
outname <- 'ACCESS-ESM1-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")          
nct <- brick("tas_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
outname <- 'BCC-CSM2-MR-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_CanESM5_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")
nct <- brick("tas_Amon_CanESM5_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")    
outname <- "CanESM5-rcp85"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_FGOALS-g3_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")       
nct <- brick("tas_Amon_FGOALS-g3_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
outname <- "FGOALS-g3-rcp85"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012_BC_lonlat.nc")    
nct <- brick("tas_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012_BC_lonlat.nc")    
outname <- "GFDL-ESM4-rcp85"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_IPSL-CM6A-LR_ssp585_r1i1p1f1_gr_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_IPSL-CM6A-LR_ssp585_r1i1p1f1_gr_201501-210012_BC_lonlat.nc")  
outname <- 'IPSL-CM6A-LR-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_MIROC6_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")  
nct <- brick("tas_Amon_MIROC6_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")   
outname <- 'MIROC6-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_MRI-ESM2-0_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")       
nct <- brick("tas_Amon_MRI-ESM2-0_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")           
outname <- 'MRI-ESM2-0-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("pr_Amon_NorESM2-LM_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")    
nct <- brick("tas_Amon_NorESM2-LM_ssp585_r1i1p1f1_gn_201501-210012_BC_lonlat.nc")        
outname <- 'NorESM2-LM-rcp85'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())
