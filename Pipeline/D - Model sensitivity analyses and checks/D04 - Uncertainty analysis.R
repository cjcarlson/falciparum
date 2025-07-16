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
library(sp)
library(gstat)
library(fixest)
library(raster)

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

# bring in lat-lon of ADM1 centroids
centroid_fp <- file.path(datadir, "Data", "ADM1-centroids.csv")

centroids <- readr::read_csv(centroid_fp, show_col_types = FALSE)

spdf <- complete |>
  dplyr::left_join(centroids, by = join_by(OBJECTID))

# Estimate an empirical variogram
# coordinates - so variogram is in m
coordinates(spdf) = ~lon+lat
projection(spdf) = CRS("+init=EPSG:4326")

# estimate variogram, 0 lags
vv = variogram(res~1, data=spdf, projection(FALSE))
vvP = variogram(PfPR2~1, data = spdf, projection(FALSE))

pdf(file = file.path(resdir, "Figures", "Diagnostics", "Residuals", "variogram_residuals.pdf"), width = 7, height = 7)
plot(vv, xlab="distance (km)", main = "Model residuals")
dev.off()

pdf(file = file.path(resdir, "Figures", "Diagnostics", "Residuals", "variogram_PfPR2.pdf"), width = 7, height = 7)
plot(vvP, xlab="distance (km)", main = "Prevalence (PfPR2)")
dev.off()

########################################################################
# D: General correlation over time
########################################################################

# As detailed in D03 - Additional robustness.R, the panel is sufficiently unbalanced 
# that estimating a distributed lag at monthly scale is likely not feasible. Instead, look across years.

# Average residuals by ADM1-year
anndf = complete |> group_by(OBJECTID,yearnum) |> dplyr::summarize(resmn = mean(res, na.rm=TRUE), year = first(yearnum))

# Expand to be a full panel 
anndf_ex <- anndf_ex %>% 
  group_by(OBJECTID) %>%
  complete(year = 1902:2016) %>%
  ungroup()

# Add lags
anndf_with_lag <- anndf_ex %>%
  arrange(OBJECTID, year) %>%
  mutate(reslag1 = lag(resmn,1), reslag2 = lag(resmn,2),reslag3 = lag(resmn,3),reslag4 = lag(resmn,4),reslag5 = lag(resmn,5)) |> 
  tidyr::drop_na(resmn)

# Estimation 
# TO DO - estimate a set of regressions with 1 to 5 lags, store all in one big table 

reslags = paste(colnames(anndf)[grep("lag", colnames(anndf))], collapse = " + ")

resmod <- lm(resmn ~ reslags,data=anndf)
summary(resmod)

# plot if helpful
coefs <- coef(tmod)[-1]
ses <- summary(tmod)$coefficients[-1,2]
lags <- 1:12

ggplot(data.frame(lag = lags, coef = coefs, se = ses), aes(x=lag, y = coef)) + 
  geom_point() +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax =coef + 1.96*se), width=0.1) +
  labs(title = "Serial correlation in model residuals", x = "Lag (months)", y = "Coefficient") +
  theme_minimal()
  
########################################################################
# E. Robustness to various clustering approaches, informed by diagnostics above
########################################################################

###### Main spec (ADM1 clustering)

# plot
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)
coefs = summary(mainmod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
mainfig =  plotPolynomialResponse(mainmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                              yLab = "Prevalence (%)", title = "Main specification", yLim=c(-30,5), showYTitle = T)

###### ADM1 x year clustering (no correlation over years)

complete = complete |> group_by(OBJECTID,year) |> mutate(adm1yr = cur_group_id()) |> ungroup()

# Formula 
adm1yr = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | adm1yr"
  )
)

# Estimation
adm1yrmod = felm(data = complete, formula = adm1yr)

# Plot
coefs = summary(adm1yrmod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
adm1yrfig =  plotPolynomialResponse(adm1yrmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                   yLab = "Prevalence (%)", title = "Main spec: country clustering", yLim=c(-30,5), showYTitle = T)

###### Country clustering

# Formula 
cntryclus = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | country"
  )
)

# Estimation
cntrymod = felm(data = complete, formula = cntryclus)

# Plot
coefs = summary(cntrymod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
cntryfig =  plotPolynomialResponse(cntrymod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                              yLab = "Prevalence (%)", title = "Main spec: country clustering", yLim=c(-30,5), showYTitle = T)

###### Conley
spdf <- complete |>
  dplyr::left_join(centroids, by = join_by(OBJECTID))

conleyform = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | OBJECTID "
  )
)
conleymod1 = feols( conleyform, data=spdf, conley(100, distance = "spherical"))
conleymod2 = feols( conleyform, data=spdf, conley(500, distance = "spherical"))

# Plot
coefs = summary(conleymod1)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
conleyfig1 =  plotPolynomialResponse(conleymod1, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                   yLab = "Prevalence (%)", title = "Main spec: Conley clustering (100km)", yLim=c(-30,5), showYTitle = T)
coefs = summary(conleymod2)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
conleyfig1 =  plotPolynomialResponse(conleymod2, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                     yLab = "Prevalence (%)", title = "Main spec: Conley clustering (200km)", yLim=c(-30,5), showYTitle = T)

####### Save
# tabular output
modellist = list(mainmod,adm1yrmod,cntrymod,conleymod1,conleymod2)
mycollabs = c(
  "main spec.", 
  "ADM1-year clust.",
  "country clust.", 
  "Conley: 100km",
  "Conley: 500km"
)

# breaking - use modelsummary() instead?
mynote = "Column specifications: (1) main specification (standard errors clustered at ADM1 level); (2) standard errors clustered at country level; (3) standard errors estimated following Conley (2008) using 100km cutoff; (4) standard errors estimated following Conley (2008) using a 200km cutoff."
stargazer(modellist,
          title="Quadratic temperature: standard error sensitivity", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought", "intervention", "METHOD"),
          out = file.path(resdir, "Tables", "Diagnostics","Residuals","uncertainty.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, digits=2,notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

# figure output
uncert = plot_grid(mainfig,cntryfig,conleyfig1,conleyfig2,nrow = 2)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "temp_response_cXt2intrXm.pdf"), plot = uncert, width = 10, height = 4)

########################################################################
# F. Overdispersion?
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

