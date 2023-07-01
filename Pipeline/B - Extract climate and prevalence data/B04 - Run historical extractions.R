


library(ncdf4)
library(raster)
library(rgdal)
library(sp)
library(tidyr)
library(velox)

setwd('G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/')

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_ACCESS-CM2_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")     
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_ACCESS-CM2_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")
outname <- 'ACCESS-CM2'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_ACCESS-ESM1-5_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_ACCESS-ESM1-5_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")   
outname <- 'ACCESS-ESM1'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_BCC-CSM2-MR_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")          
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_BCC-CSM2-MR_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")  
outname <- 'BCC-CSM2-MR'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_CanESM5_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_CanESM5_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")    
outname <- "CanESM5"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_FGOALS-g3_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_FGOALS-g3_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")  
outname <- "FGOALS-g3"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_GFDL-ESM4_historical_r1i1p1f1_gr1_190101-201412_BC_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_GFDL-ESM4_historical_r1i1p1f1_gr1_190101-201412_BC_lonlat.nc")    
outname <- "GFDL-ESM4"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_IPSL-CM6A-LR_historical_r1i1p1f1_gr_190101-201412_BC_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_IPSL-CM6A-LR_historical_r1i1p1f1_gr_190101-201412_BC_lonlat.nc")  
outname <- 'IPSL-CM6A-LR'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_MIROC6_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_MIROC6_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")   
outname <- 'MIROC6'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_MRI-ESM2-0_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_MRI-ESM2-0_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")           
outname <- 'MRI-ESM2-0'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/pr_Amon_NorESM2-LM_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/historical/tas_Amon_NorESM2-LM_historical_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")        
outname <- 'NorESM2-LM'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B02 - Extract GCMs - historical.R")
rm(list=ls())

######################


setwd('G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/')

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_ACCESS-CM2_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")     
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_ACCESS-CM2_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")
outname <- 'ACCESS-CM2-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_ACCESS-ESM1-5_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_ACCESS-ESM1-5_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")   
outname <- 'ACCESS-ESM1-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_BCC-CSM2-MR_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")          
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_BCC-CSM2-MR_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")  
outname <- 'BCC-CSM2-MR-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_CanESM5_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_CanESM5_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")    
outname <- "CanESM5-nat"

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_FGOALS-g3_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_FGOALS-g3_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")  
outname <- "FGOALS-g3-nat"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_GFDL-ESM4_hist-nat_r1i1p1f1_gr1_190101-201412_BC_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_GFDL-ESM4_hist-nat_r1i1p1f1_gr1_190101-201412_BC_lonlat.nc")    
outname <- "GFDL-ESM4-nat"      

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_IPSL-CM6A-LR_hist-nat_r1i1p1f1_gr_190101-201412_BC_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_IPSL-CM6A-LR_hist-nat_r1i1p1f1_gr_190101-201412_BC_lonlat.nc")  
outname <- 'IPSL-CM6A-LR-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_MIROC6_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")  
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_MIROC6_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")   
outname <- 'MIROC6-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_MRI-ESM2-0_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")       
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_MRI-ESM2-0_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")           
outname <- 'MRI-ESM2-0-nat'

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())

ncp <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/pr_Amon_NorESM2-LM_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")    
nct <- brick("G:/Shared drives/MALARIA_AFRICA/BC-ClimateData20230626/4-Outputs/hist-nat/tas_Amon_NorESM2-LM_hist-nat_r1i1p1f1_gn_190101-201412_BC_lonlat.nc")        
outname <- 'NorESM2-LM-nat'  

source("~/Github/falciparum/Pipeline/B - Extract climate and prevalence data/B03 - Extract GCMs - natural counterfactual.R")
rm(list=ls())
