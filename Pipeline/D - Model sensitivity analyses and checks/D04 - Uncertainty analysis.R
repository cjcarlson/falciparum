############################################################
# This script investigates correlation in the model
# residuals and assesses alternative methods of clustering
# or accounting for spatiotemporal correlations in errors.
############################################################

############################################################
# Set up
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

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here("Pipeline", "A - Utility functions", "A01 - Utility code for calculations.R"))
source(here::here("Pipeline", "A - Utility functions", "A02 - Utility code for plotting.R"))

############################################################
# Plotting toggles
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tref = 25 # reference temperature - curve gets recentered to 0 here
Tmin = 10 # min T for x axis
Tmax = 40 # max T for x axis

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
source(here::here("Pipeline", "A - Utility functions", "A03 - Prep data for estimation.R"))

#### Store output here
resdir = file.path(datadir, "Results")
dir.create(file.path(resdir, "Figures", "Diagnostics"), showWarnings = FALSE)
dir.create(file.path(resdir, "Tables", "Diagnostics"), showWarnings = FALSE)
dir.create(file.path(resdir, "Figures", "Diagnostics", "Residuals"), showWarnings = FALSE)
dir.create(file.path(resdir, "Tables", "Diagnostics", "Residuals"), showWarnings = FALSE)

########################################################################
# Estimate main model, store residuals
########################################################################

# Formula 
cXt2intrXm = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)

# Estimation & residuals
mainmod = felm(data = complete, formula = cXt2intrXm)
complete <- complete |> mutate(res = c(residuals(mainmod)))

########################################################################
# A: Correlation across ADM1s within a country (same year-month)
########################################################################

# Regress residuals on country dummies, control for month and year
resCntry = felm(res ~ I(country) | month + year | 0 | 0 , data=complete)

# Histogram of p-values on each country's coefficient
pvals = summary(resCntry)$coefficients[,"Pr(>|t|)"]
ph = ggplot() + 
  geom_histogram(aes(x=pvals), color= "seagreen", fill = "seagreen") + 
  geom_vline(xintercept=0.05, color="grey") +
  xlab("country p-value (null: no correlation within country)") + ylab("count of countries") +
  theme_classic()
ph
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "pvals_country_correlations.pdf"), plot = ph, width = 7, height = 7)

# Boxplot of residuals by country 
g= ggplot(complete, aes(x=country, y=res)) + 
  geom_boxplot() + 
  theme_classic() + ylab("residuals") + theme( axis.text.x=element_blank(),axis.ticks.x=element_blank())
g
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "residuals_country_boxplot.pdf"), plot = g, width = 9, height = 5)

# Fstatistic
df = data.frame(stat = c("F stat", "p value"), 
                value = c(summary(resCntry)$P.fstat[5], summary(resCntry)$P.fstat[1]))
write.csv(df, file.path(resdir, "Tables", "Diagnostics", "Residuals", "residuals_country_Fstat.csv"))

########################################################################
# B: Correlation across ADM1s within a GBOD region (same year-month)
########################################################################

# Regress residuals on country dummies, control for month and year
resGBOD = felm(res ~ I(smllrgn) | month + year | 0 | 0 , data=complete)

# Histogram of p-values on each region's coefficient
pvals = summary(resGBOD)$coefficients[,"Pr(>|t|)"]
ph = ggplot() + 
  geom_histogram(aes(x=pvals), color= "seagreen", fill = "seagreen") + 
  geom_vline(xintercept=0.05, color="grey") +
  xlab("region p-value (null: no correlation within country)") + ylab("count of regions") +
  theme_classic()
ph
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "pvals_region_correlations.pdf"), plot = ph, width = 7, height = 7)

# Boxplot of residuals by region 
g= ggplot(complete, aes(x=as.factor(smllrgn), y=res)) + 
  geom_boxplot() + 
  theme_classic() + ylab("residuals") + theme( axis.text.x=element_blank(),axis.ticks.x=element_blank())
g
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "residuals_GBOD_boxplot.pdf"), plot = g, width = 9, height = 5)

# Fstatistic
df = data.frame(stat = c("F stat", "p value"), 
                value = c(summary(resGBOD)$P.fstat[5], summary(resGBOD)$P.fstat[1]))
write.csv(df, file.path(resdir, "Tables", "Diagnostics", "Residuals", "residuals_GBOD_Fstat.csv"))

########################################################################
# C: General correlation over space
########################################################################

###### THIS IS NOT DONE. SEE HERE FOR VARIOGRAMST help - needs time not just space https://www.r-bloggers.com/2015/08/spatio-temporal-kriging-in-r/

# bring in lat-lon of ADM1 centroids
centroid_fp <- file.path(datadir, "Data", "ADM1-centroids.csv")

centroids <- readr::read_csv(centroid_fp, show_col_types = FALSE)

spdf <- complete |>
  dplyr::left_join(centroids, by = join_by(OBJECTID))

# Estimate an empirical variogram
library(sp)
library(gstat)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos) 

# data cleaning
spdf$time = as.POSIXct()

# Need to construct a STIDF object
coordinates(spdf) = ~lon+lat
projection(spdf) = CRS("+init=EPSG:4326")
spdf = spTransform(spdf,CRS("+init=EPSG:4326"))

# spatialpoints object
spdfSP = SpatialPoints(spdf@coords,CRS("+init=EPSG:4326"))

# estimate variogram, 0 lags
vv = variogram(res~1, data=spdf, tlags=0)
plot(vv)

# estimate variogram, 12 monthly lags 


########################################################################
# D. Robustness to various clustering approaches, informed by diagnostics above
########################################################################

###### Country
# Formula 
cntryclus = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | country"
  )
)

# Estimation
cntry = felm(data = complete, formula = cntryclus)
summary(cntry)

# Plot
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)

coefs = summary(cntry)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
fig =  plotPolynomialResponse(cntry, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                              yLab = "Prevalence (%)", title = "Main spec: country clustering", yLim=c(-30,5), showYTitle = T)

fig
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "temp_response_country_clustering.pdf"), plot = fig, width = 7, height = 7)

###### Conley

# TBD

########################################################################
# E. Overdispersion?
########################################################################

# Plot model residuals
complete <- complete |> mutate(res = c(residuals(mainmod)))
g <- ggplot(data=complete) + 
  geom_histogram(aes(x=res), color= "seagreen", fill = "seagreen") + 
  xlab("model residuals") + 
  theme_classic()
g

dir.create(file.path(resdir, "Figures", "Diagnostics","Residuals"), showWarnings = FALSE)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "model_residuals.pdf"), plot = g, width = 7, height = 7)

