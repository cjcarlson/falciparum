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
  dplyr::mutate(yr_bin5 = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin5) |>
  dplyr::mutate(cntry_yrbin5 = dplyr::cur_group_id()) |>
  dplyr::ungroup()

iso5form = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin5"
  )
)
iso5mod = felm(data = complete, formula = iso5form)
iso5fig =  plotPolynomialResponse(iso5mod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                yLab = "Prevalence (%)", title = "country-5-year clust. (main)", yLim=c(-30,5), showYTitle = T)

## country-decade clustering
yr_bin_size <- 10
complete <- complete |>
  dplyr::mutate(yr_bin10 = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin10) |>
  dplyr::mutate(cntry_yrbin10 = dplyr::cur_group_id()) |>
  dplyr::ungroup()

iso10form = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin10"
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

## Formulas: all fixed effects (main spec = cXt2intrXm)
common <- paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars)
country_time <- "country:monthyr + country:monthyr2"

# felm doesn't like triple interactions, hard code this one
complete <- complete |>
  dplyr::group_by(as.factor(smllrgn), month) |>
  dplyr::mutate(smllrgnMO = dplyr::cur_group_id()) |>
  dplyr::ungroup() |>
  dplyr::mutate(smllrgnMO = as.factor(smllrgnMO), cntry_yrbin10 = as.factor(cntry_yrbin10)) 
  

ym = as.formula(paste0(common, " | OBJECTID + year + month | 0 | cntry_yrbin5"))
cXt2m = as.formula(paste0(common, " + ", country_time, " | OBJECTID  + month | 0 | cntry_yrbin5"))
cXt2cXm = as.formula(paste0(common, " + ", country_time, " | OBJECTID + country:month | 0 | cntry_yrbin5"))
cXt2intm = as.formula(paste0(common, " + ", country_time, " | OBJECTID  + intervention + month | 0 | cntry_yrbin5"))
cXt2intrXm = as.formula(paste0(common, " + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | cntry_yrbin5")) # main
cXt2intcXm = as.formula(paste0(common, " + I(intervention) + ", country_time, " | OBJECTID  + country:month | 0 | cntry_yrbin5"))
cXt2rXmyXm = as.formula(paste0(common, " + ", country_time, " | OBJECTID  + as.factor(smllrgn):month + year:month | 0 | cntry_yrbin5"))
rXmcXy = as.formula(paste0(common, " | OBJECTID  + as.factor(smllrgn):month + country:year | 0 | cntry_yrbin5"))
rXyrXm = as.formula(paste0(common, " | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | cntry_yrbin5"))
rXycXm = as.formula(paste0(common, " | OBJECTID + country:month + as.factor(smllrgn):year | 0 | cntry_yrbin5"))
aXdrXmd = as.formula(paste0(common, " | as.factor(OBJECTID):cntry_yrbin10  + smllrgnMO:cntry_yrbin10 | 0 | cntry_yrbin5"))
rXyrXmcXt = as.formula(paste0(common, " + country:monthyr | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | cntry_yrbin5"))
myforms = c(
  ym, cXt2m, cXt2cXm, cXt2intm, cXt2intrXm, 
  cXt2intcXm, cXt2rXmyXm, rXmcXy, rXyrXm, rXycXm, aXdrXmd, rXyrXmcXt
) 

mycollabs = c(
  "yr + mo FEs.", # 1
  "cntry trd, mo FEs.", #2  
  "cntry trd, cntry-mo FEs.", #3
  "cntry trd, int + mo FEs.", #4
  "cntry trd, int + rgn-mo FEs.", #5 - Main Spec
  "cntry trd, int + cntry-mo FEs.", #6
  "cntry trd, year-mo + rgn-mo FEs.", #7
  "cntry-yr + rgn-mo FEs.", #8
  "rgn-yr + rgn-mo FEs.",  #9
  "rgn-yr + cntry-mo FEs.", #10
  "adm-decade + rgn-mo-decade FEs.", #11
  "cntry trd, rgn-yr + rgn-mo FEs." #12
)

# Run all models
modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete, formula = m)
}

# Combine into a single stargazer plot 
mynote = "Column specifications: (1) year and month FE; (2) country-specific quad. trends and month FE; (3) country-specific quad. trends and country-by-month FE; (4) country-specific quad. trends, intervention year and month FE; (5) country-specific quad. trends, intervention year FE, GBD region-month FE; (6) country-specific quad. trends with intervention FE and country-month FE; (7) country-specific quad. trends with year-month and GBD region-mont FE; (8) country-year and GBD region-month FE; (9) GBD region-year and regin-month FEs; (10) GBD region-year + country-month FE; (11) ADM1-decade and GBD region-month-decade FE; (12) country-specific quad. trends and GBD region-year and region-month FE."
dir.create(file.path(resdir, "Tables", "sensitivity"), showWarnings = FALSE)
stargazer(modellist,
          title="Quadratic temperature: FE sensitivity", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought", "intervention", "METHOD"),
          out = file.path(resdir, "Tables", "sensitivity","FixedEffects_sensitivity.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, digits=2,notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

########################################################################
## Plot temperature response functions for all fixed effects specifications
########################################################################

plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)

figList = list()
for(m in 1:length(modellist)) {
  # get max of response function
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  figList[[m]] =  plotPolynomialResponse(
    modellist[[m]], 
    "temp", 
    plotXtemp, 
    polyOrder = 2, 
    cluster = T,
    xRef = myrefT, 
    xLab = expression(paste("Mean temperature (",degree,"C)")),
    yLab = "Prevalence (%)", 
    title = mycollabs[m], 
    yLim=c(-30,5), 
    showYTitle = T
  ) +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 8)
    )
}

# point estimate and CIs for main spec
xValsT = genRecenteredXVals_polynomial(plotXtemp,Tref,2)
mainmod = modellist[[5]]
beta = mainmod$coefficients 
vars = rownames(beta)
plotVars = vars[grepl(pattern = "temp", x = vars)] 
b = as.matrix(beta[rownames(beta) %in% plotVars])
vcov = getVcov(mainmod$clustervcv, plotVars)
response = as.matrix(xValsT) %*% b #Prediction
length = 1.96 * sqrt(apply(X = xValsT, FUN = calcVariance, MARGIN = 1, vcov))
lb = response - length
ub = response + length

#Plotting dataframe -- add back in the reference temperature so it's centered at xRef
plotData = data.frame(x = xValsT[,1] + Tref, response = response, lb = lb, ub = ub)
sub = plotData[plotData$x>=10 & plotData$x<=30,]
maxX = max(sub$x[sub$response==max(sub$response)])

mycollabs = c(
  "ym", "cXt2m", "cXt2cXm", "cXt2intm", "cXt2intrXm",  
  "cXt2intcXm", "cXt2rXmyXm", "rXmcXy", "rXyrXm", "rXycXm", "aXdrXmd", "rXyrXmcXt"
)

# loop over all other FE models, add to plotting dataframe
for(mod in 1:length(modellist)){ 
  beta = modellist[[mod]]$coefficients 
  vars = rownames(beta)
  plotVars = vars[grepl(pattern = "temp", x = vars)] 
  b = as.matrix(beta[rownames(beta) %in% plotVars])
  response = as.data.frame(as.matrix(xValsT) %*% b) 
  colnames(response) = paste0(mycollabs[mod])
  plotData = cbind(plotData, response)
}

# reshape
plotmain = plotData %>% dplyr::select(x,response,lb,ub)
plotFE = plotData %>% dplyr::select(x,ym:rXyrXmcXt)
plotFE = plotFE %>% gather(plotFE, response, ym:rXyrXmcXt)  
colnames(plotFE) = c("x", "model","response")  

# plot
g = ggplot()  +
  geom_hline(yintercept = 0, color="darkgrey",alpha=.5) + 
  geom_ribbon(data = plotmain, # CIs main spec
              mapping = aes(x, ymin = lb, ymax = ub), alpha = 0.4, fill = "#C1657C") +
  geom_line(data = plotFE, # point estimate other specs
            aes(x = x, y = response, group = model), color = "seagreen", alpha=0.8) +
  geom_line(data = plotmain, # point estimate main spec
            mapping = aes(x = x, y = response), color = "black", linewidth = 1) +
  geom_vline(xintercept = maxX, colour = "grey39") +
  annotate(
    geom = "text",
    x = maxX + 3.5,
    y = 2.55,
    label = paste0(maxX, " C"),
    color = "grey39",
    size = 3
  ) +
  theme_classic() + 
  labs(x = expression(paste("Mean temperature (",degree,"C)")), y = "Prevalence (%)") + 
  xlim(Tmin,Tmax) + 
  ylim(-30,5)  +
  ggtitle("main: cnty trd, int + rgn−mo FEs.")+
  theme(
    text = element_text(size = 8),
    plot.title = element_text(size = 8)
  ) 

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], g, figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], 
              figList[[10]], figList[[11]], figList[[12]], nrow=4)
p

dir.create(file.path(resdir, "Figures", "Diagnostics","Fixed_effects"), showWarnings = FALSE)
ggsave(
  filename = "panelFE_FE_sensitivity.pdf",
  path = file.path(resdir, "Figures", "Diagnostics", "Fixed_effects"), 
  plot = p, 
  width = 7, 
  height = 9
)

