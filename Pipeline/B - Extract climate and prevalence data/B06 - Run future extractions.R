
library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(tidyr)
library(velox)

setwd('G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data')
                                                                                      
ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_BCC-CSM2-MR_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")     
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_BCC-CSM2-MR_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")
outname <- 'BCC-CSM2-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_CanESM5_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_CanESM5_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")   
outname <- 'CanESM5-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_CESM2_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")          
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_CESM2_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")  
outname <- 'CESM2-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_CNRM-CM6-1_ssp126_r1i1p1f2_gr_201501-210012_lonlat.nc")
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_CNRM-CM6-1_ssp126_r1i1p1f2_gr_201501-210012_lonlat.nc")    
outname <- "CNRM-CM6-rcp26"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012_lonlat.nc")  
outname <- "GFDL-ESM4-rcp26"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_GISS-E2-1-G_ssp126_r1i1p1f2_gn_201501-210012_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_GISS-E2-1-G_ssp126_r1i1p1f2_gn_201501-210012_lonlat.nc")    
outname <- "GISS-E2-rcp26"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_HadGEM3-GC31-LL_ssp126_r1i1p1f3_gn_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_HadGEM3-GC31-LL_ssp126_r1i1p1f3_gn_201501-210012_lonlat.nc")  
outname <- 'HadGEM3-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_IPSL-CM6A-LR_ssp126_r1i1p1f1_gr_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_IPSL-CM6A-LR_ssp126_r1i1p1f1_gr_201501-210012_lonlat.nc")   
outname <- 'IPSL-CM6A-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_MIROC6_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_MIROC6_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")           
outname <- 'MIROC6-rcp26'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_MRI-ESM2-0_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_MRI-ESM2-0_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")        
outname <- 'MRI-ESM2-rcp26'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp1-2.6/BC_pr_Amon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc") 
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp126/BC_tas_Amon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012_lonlat.nc")        
outname <- 'NorESM2-rcp26'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())


######################

# Change pr1 and tas1 to 2 

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")
outname <- 'BCC-CSM2-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_CanESM5_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_CanESM5_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")   
outname <- 'CanESM5-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_CESM2_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")          
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_CESM2_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")  
outname <- 'CESM2-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_CNRM-CM6-1_ssp245_r1i1p1f2_gr_201501-210012_lonlat.nc")
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_CNRM-CM6-1_ssp245_r1i1p1f2_gr_201501-210012_lonlat.nc")    
outname <- "CNRM-CM6-rcp45"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_GFDL-ESM4_ssp245_r1i1p1f1_gr1_201501-210012_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_GFDL-ESM4_ssp245_r1i1p1f1_gr1_201501-210012_lonlat.nc")  
outname <- "GFDL-ESM4-rcp45"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_GISS-E2-1-G_ssp245_r1i1p1f2_gn_201501-210012_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_GISS-E2-1-G_ssp245_r1i1p1f2_gn_201501-210012_lonlat.nc")    
outname <- "GISS-E2-rcp45"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_201501-210012_lonlat.nc")  
outname <- 'HadGEM3-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_IPSL-CM6A-LR_ssp245_r1i1p1f1_gr_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_IPSL-CM6A-LR_ssp245_r1i1p1f1_gr_201501-210012_lonlat.nc")   
outname <- 'IPSL-CM6A-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_MIROC6_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_MIROC6_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")           
outname <- 'MIROC6-rcp45'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_MRI-ESM2-0_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_MRI-ESM2-0_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")        
outname <- 'MRI-ESM2-rcp45'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp2-4.5/BC_pr_Amon_NorESM2-LM_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc") 
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp2-4.5/BC_tas_Amon_NorESM2-LM_ssp245_r1i1p1f1_gn_201501-210012_lonlat.nc")        
outname <- 'NorESM2-rcp45'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())



######################


ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")
outname <- 'BCC-CSM2-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_CanESM5_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_CanESM5_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")   
outname <- 'CanESM5-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_CESM2_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")          
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_CESM2_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")  
outname <- 'CESM2-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_CNRM-CM6-1_ssp585_r1i1p1f2_gr_201501-210012_lonlat.nc")
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_CNRM-CM6-1_ssp585_r1i1p1f2_gr_201501-210012_lonlat.nc")    
outname <- "CNRM-CM6-rcp85"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012_lonlat.nc")  
outname <- "GFDL-ESM4-rcp85"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_GISS-E2-1-G_ssp585_r1i1p1f2_gn_201501-210012_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_GISS-E2-1-G_ssp585_r1i1p1f2_gn_201501-210012_lonlat.nc")    
outname <- "GISS-E2-rcp85"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_HadGEM3-GC31-LL_ssp585_r1i1p1f3_gn_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_HadGEM3-GC31-LL_ssp585_r1i1p1f3_gn_201501-210012_lonlat.nc")  
outname <- 'HadGEM3-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_IPSL-CM6A-LR_ssp585_r1i1p1f1_gr_201501-210012_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_IPSL-CM6A-LR_ssp585_r1i1p1f1_gr_201501-210012_lonlat.nc")   
outname <- 'IPSL-CM6A-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_MIROC6_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_MIROC6_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")           
outname <- 'MIROC6-rcp85'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_MRI-ESM2-0_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_MRI-ESM2-0_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")        
outname <- 'MRI-ESM2-rcp85'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/ssp5-8.5/BC_pr_Amon_NorESM2-LM_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc") 
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/ssp5-8.5/BC_tas_Amon_NorESM2-LM_ssp585_r1i1p1f1_gn_201501-210012_lonlat.nc")        
outname <- 'NorESM2-rcp85'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B05 - Extract GCMs - future.R")
rm(list=ls())
