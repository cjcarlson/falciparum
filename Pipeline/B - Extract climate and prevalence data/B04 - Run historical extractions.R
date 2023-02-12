
library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(tidyr)
library(velox)

setwd('G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data')

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_BCC-CSM2-MR_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")     
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_BCC-CSM2-MR_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")
outname <- 'BCC-CSM2-MR'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_CanESM5_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_CanESM5_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")   
outname <- 'CanESM5'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_CESM2_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")          
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_CESM2_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")  
outname <- 'CESM2'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_CNRM-CM6-1_historical_r1i1p1f2_gr_190101-201412_lonlat.nc")
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_CNRM-CM6-1_historical_r1i1p1f2_gr_190101-201412_lonlat.nc")    
outname <- "CNRM-CM6"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_GFDL-ESM4_historical_r1i1p1f1_gr1_190101-201412_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_GFDL-ESM4_historical_r1i1p1f1_gr1_190101-201412_lonlat.nc")  
outname <- "GFDL-ESM4"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_GISS-E2-1-G_historical_r1i1p1f2_gn_190101-201412_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_GISS-E2-1-G_historical_r1i1p1f2_gn_190101-201412_lonlat.nc")    
outname <- "GISS-E2"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_190101-201412_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_190101-201412_lonlat.nc")  
outname <- 'HadGEM3'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_IPSL-CM6A-LR_historical_r1i1p1f1_gr_190101-201412_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_IPSL-CM6A-LR_historical_r1i1p1f1_gr_190101-201412_lonlat.nc")   
outname <- 'IPSL-CM6A'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_MIROC6_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_MIROC6_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")           
outname <- 'MIROC6'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_MRI-ESM2-0_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_MRI-ESM2-0_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")        
outname <- 'MRI-ESM2'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist/BC_pr_Amon_NorESM2-LM_historical_r1i1p1f1_gn_190101-201412_lonlat.nc") 
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist/BC_tas_Amon_NorESM2-LM_historical_r1i1p1f1_gn_190101-201412_lonlat.nc")        
outname <- 'NorESM2'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())


######################


ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_BCC-CSM2-MR_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_BCC-CSM2-MR_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")
outname <- 'BCC-CSM2-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_CanESM5_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_CanESM5_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")   
outname <- 'CanESM5-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_CESM2_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")          
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_CESM2_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")  
outname <- 'CESM2-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_CNRM-CM6-1_hist-nat_r1i1p1f2_gr_190101-201412_lonlat.nc")
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_CNRM-CM6-1_hist-nat_r1i1p1f2_gr_190101-201412_lonlat.nc")    
outname <- "CNRM-CM6-nat"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_GFDL-ESM4_hist-nat_r1i1p1f1_gr1_190101-201412_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_GFDL-ESM4_hist-nat_r1i1p1f1_gr1_190101-201412_lonlat.nc")  
outname <- "GFDL-ESM4-nat"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_GISS-E2-1-G_hist-nat_r1i1p1f2_gn_190101-201412_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_GISS-E2-1-G_hist-nat_r1i1p1f2_gn_190101-201412_lonlat.nc")    
outname <- "GISS-E2-nat"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_HadGEM3-GC31-LL_hist-nat_r1i1p1f3_gn_190101-201412_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_HadGEM3-GC31-LL_hist-nat_r1i1p1f3_gn_190101-201412_lonlat.nc")  
outname <- 'HadGEM3-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_IPSL-CM6A-LR_hist-nat_r1i1p1f1_gr_190101-201412_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_IPSL-CM6A-LR_hist-nat_r1i1p1f1_gr_190101-201412_lonlat.nc")   
outname <- 'IPSL-CM6A-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_MIROC6_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_MIROC6_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")           
outname <- 'MIROC6-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_MRI-ESM2-0_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_MRI-ESM2-0_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")        
outname <- 'MRI-ESM2-nat'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/precipitation/hist-nat/BC_pr_Amon_NorESM2-LM_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc") 
nct <- brick("G:/Shared drives/MALARIA_AFRICA/Bias-Corrected_data/temperature/hist-nat/BC_tas_Amon_NorESM2-LM_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc")        
outname <- 'NorESM2-nat'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())
