############################################################
# This script is temporary. It should be integrated into other D03-D04 scripts 
# as it contains additional robustness tests. 
############################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

# packages
library(here)
library(lfe)
library(reshape)
library(stargazer)
library(tidyverse)
library(zoo)
library(lubridate)
library(cowplot)
library(multcomp)
library(sp)
library(gstat)
library(fixest)
library(raster)
library(ggpubr)
library(car)
library(sf)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here("Pipeline", "A - Utility functions", "A01 - Utility code for calculations.R"))
source(here::here("Pipeline", "A - Utility functions", "A02 - Utility code for plotting.R"))

############################################################
# Load data, set plotting toggles ----
############################################################

complete <- readr::read_rds(file.path(datadir,'malaria-replication','prevalence_and_climate.rds'))

Tref = 25 # reference temperature - curve gets recentered to 0 here
Tmin = 10 # min T for x axis
Tmax = 40 # max T for x axis
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2) # temperature vector for plotting response function

############################################################
# Clustering sensitivity ----
############################################################

## ADM1 clustering 
adm1form = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)
adm1mod = felm(data = complete, formula=adm1form)
coefs = summary(adm1mod)$coefficients[1:2] # only need to compute this and the next line once, all specs have same coeffs but different CIs
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
adm1fig =  plotPolynomialResponse(adm1mod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                  yLab = "Prevalence (%)", title = "ADM1 clust.", yLim=c(-30,5), showYTitle = T)


## country clustering 
isoform = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | country"
  )
)
isomod = felm(data = complete, formula=isoform)
isofig =  plotPolynomialResponse(isomod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                  yLab = "Prevalence (%)", title = "country clust.", yLim=c(-30,5), showYTitle = T)

## country x year clustering (no correlation over years) 
complete = complete |> group_by(country,year) |> mutate(cntryyr = cur_group_id()) |> ungroup()
isoyrform = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | cntryyr"
  )
)
isoyrmod = felm(data = complete, formula = isoyrform)
isoyrfig =  plotPolynomialResponse(isoyrmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                     yLab = "Prevalence (%)", title = "country-year clust.", yLim=c(-30,5), showYTitle = T)

## year clustering 
yrform = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | year"
  )
)
yrmod = felm(data = complete, formula = yrform)
yrfig =  plotPolynomialResponse(yrmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                   yLab = "Prevalence (%)", title = "year clust.", yLim=c(-30,5), showYTitle = T)

## country-5-year clustering
yr_bin_size <- 5
complete <- complete |>
  dplyr::mutate(yr_bin = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin) |>
  dplyr::mutate(cntry_yrbin = dplyr::cur_group_id()) |>
  dplyr::ungroup()

iso5form = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin"
  )
)
iso5mod = felm(data = complete, formula = iso5form)
iso5fig =  plotPolynomialResponse(iso5mod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                yLab = "Prevalence (%)", title = "country-5-year clust. (main)", yLim=c(-30,5), showYTitle = T)

## country-decade clustering
yr_bin_size <- 10
complete <- complete |>
  dplyr::mutate(yr_bin = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin) |>
  dplyr::mutate(cntry_yrbin = dplyr::cur_group_id()) |>
  dplyr::ungroup()

iso10form = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin"
  )
)
iso10mod = felm(data = complete, formula = iso10form)
iso10fig =  plotPolynomialResponse(iso10mod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                  yLab = "Prevalence (%)", title = "country-decade clust.", yLim=c(-30,5), showYTitle = T)

## Conley standard errors
centroid_fp <- file.path(datadir, "Data", "ADM1-centroids.csv")
centroids <- readr::read_csv(centroid_fp, show_col_types = FALSE)
spdf <- complete |>
  dplyr::left_join(centroids, by = join_by(OBJECTID))

conleyform = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | OBJECTID "
  )
)

conley_dist_1 <- 200
conley_dist_2 <- 500

conleymod1 = feols(conleyform, data=spdf, conley(conley_dist_1, distance = "spherical"))
conleymod2 = feols(conleyform, data=spdf, conley(conley_dist_2, distance = "spherical"))

coefs = summary(conleymod1)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) 
conleyfig1 =  plotPolynomialResponse(conleymod1, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                     yLab = "Prevalence (%)", title = paste0("Conley (", conley_dist_1, "km)"), yLim=c(-30,5), showYTitle = T)

coefs = summary(conleymod2)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
conleyfig2 =  plotPolynomialResponse(conleymod2, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                     yLab = "Prevalence (%)", title = paste0("Conley (", conley_dist_2, "km)"), yLim=c(-30,5), showYTitle = T)

## merged plot
uncert = plot_grid(
  adm1fig,isofig,
  yrfig, isoyrfig,
  iso5fig, iso10fig,
  conleyfig1, conleyfig2, 
  nrow = 2)

ggsave(
  filename = "temp_response_difft_SEs.pdf",
  path = file.path(resdir, "Figures", "Diagnostics", "Residuals"), 
  plot = uncert, 
  width = 20, 
  height = 10
)

## Table
# feols models do not work with stargazer as it has no method for feols objects (class "fixest")
# so we use stargazer on the felm objects and etable on the feols objects. The two tables are 
# then combined manually

# tabular output
modellist = list(
  adm1mod, isomod,
  yrmod, isoyrmod,
  iso5mod, iso10mod
)
mycollabs = c(
  "Adm1 clust.",
  "Country clust.",
  "Year clust.",
  "Country-year clust.",
  "Country-5-year clust.",
  "Country-decade clust."
)

mynote = "Column specifications: (1) standard errors clustered at ADM1 level; (2) standard errors clustered at country level; (3) standard errors clustered at year level; (4) standard errors clustered at country-year level; (5) standard errors clustered at country-5-year level (main specification); (6) standard errors clustered at country-decade level; (7) standard errors estimated following Conley (2008) using 200km cutoff; (6) standard errors estimated following Conley (2008) using a 500km cutoff."
stargazer(modellist,
          title="Quadratic temperature: standard error sensitivity", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought", "intervention"),
          out = file.path(resdir, "Tables", "Diagnostics","Residuals","uncertainty.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, digits=2,notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

conley_tab <- etable(
  conleymod1, conleymod2,
  keep = c("temp", "flood", "drought", "intervention"),
  tex     = TRUE,   
  fitstat = c("n", "r2", "ar2"), 
  digits  = 3,       
  label   = "tab:conley"  
)
conley_tab

############################################################
# Sensitivity to spatiotemporal controls ----
############################################################

