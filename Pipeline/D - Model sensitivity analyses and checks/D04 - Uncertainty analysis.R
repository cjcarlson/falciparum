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
library(ggpubr)
library(car)

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
  xlab("country p-value (null: no correlation within country)") + ylab("# countries") +
  theme_classic(base_size = 12)
ph

# Boxplot of residuals by country 
g= ggplot(complete, aes(x=country, y=res)) + 
  geom_boxplot() + 
  theme_classic(base_size = 12) + ylab("residuals") + theme( axis.text.x=element_blank(),axis.ticks.x=element_blank())
g

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
pvalsR = summary(resGBOD)$coefficients[,"Pr(>|t|)"]
pr = ggplot() + 
  geom_histogram(aes(x=pvalsR), color= "seagreen", fill = "seagreen") + 
  geom_vline(xintercept=0.05, color="grey") +
  xlab("region p-value (null: no correlation within region)") + ylab("# regions") +
  theme_classic(base_size = 12)
pr

# Boxplot of residuals by region 
gr= ggplot(complete, aes(x=as.factor(smllrgn), y=res)) + 
  geom_boxplot() + 
  theme_classic(base_size = 12) + ylab("residuals") + xlab("region") + theme( axis.text.x=element_blank(),axis.ticks.x=element_blank())
gr

# Fstatistic
df = data.frame(stat = c("F stat", "p value"), 
                value = c(summary(resGBOD)$P.fstat[5], summary(resGBOD)$P.fstat[1]))
write.csv(df, file.path(resdir, "Tables", "Diagnostics", "Residuals", "residuals_GBOD_Fstat.csv"))


########################################################################
# C: Correlation across months (same location)
########################################################################

# Regress residuals on country dummies, control for OBJECTID
resMonth = felm(res ~ I(month) | OBJECTID | 0 | 0 , data=complete)
complete = complete %>% mutate(monthord = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
# Histogram of p-values on each region's coefficient
pvalsM = summary(resMonth)$coefficients[,"Pr(>|t|)"]
pm = ggplot() + 
  geom_histogram(aes(x=pvalsM), color= "seagreen", fill = "seagreen") + 
  geom_vline(xintercept=0.05, color="grey") +
  xlab("monthly p-value (null: no correlation within months)") + ylab("# months") +
  theme_classic(base_size = 12)
pm

# Boxplot of residuals by month 
gm = ggplot(complete, aes(x=as.factor(monthord), y=res)) + 
  geom_boxplot() + 
  theme_classic(base_size = 12) + ylab("residuals") + xlab("month") 
gm

# Fstatistic
df = data.frame(stat = c("F stat", "p value"), 
                value = c(summary(resMonth)$P.fstat[5], summary(resMonth)$P.fstat[1]))
write.csv(df, file.path(resdir, "Tables", "Diagnostics", "Residuals", "residuals_Month_Fstat.csv"))

# combine all boxplots
box = ggarrange(g, gr, gm, ncol = 1, nrow = 3, labels="auto")
box
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "residuals_ALL_boxplot.png"), plot = box, width = 5, height = 5)

# combine all pval hists
hists = ggarrange(ph, pr, pm, ncol = 1, nrow = 3, labels="auto")
hists
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "pvals_ALL_correlations.png"), plot = hists, width = 5, height = 5)

########################################################################
# D: General correlation over space -- distributions of correlations
########################################################################

####### Correlations across space within country?
country_corrs = data.frame(country=NA, pair_N=NA,corr=NA)

for(c in unique(complete$country)){
  sub = complete %>% filter(country==c) %>% dplyr::select(OBJECTID,country,monthyr,res)
  sub_wide <- sub %>%
    pivot_wider(names_from = OBJECTID, values_from = res) %>% 
    arrange(monthyr)
  mycols = colnames(sub_wide %>% dplyr::select(4:last_col()))
  # for each pair of columns with > 10 shared obs, compute correlation. store correlation and # obs
  for (i in 4:(ncol(sub_side) - 1)) {
    for (j in (i + 1):ncol(sub_wide)) {
      var1 <- names(sub_wide)[i]
      var2 <- names(sub_wide)[j]
      cat(sprintf("Pair: %s - %s\n", var1, var2))
      # How many jointly nonmissing obs? 
      non_missing_count <- sub_wide %>%
        filter(!is.na(sub_wide[,i]) & !is.na(sub_wide[,j])) %>%
        nrow()
      if(non_missing_count > 10){
        # Example operation: calculate correlation
        correlation <- cor(sub_wide[, i], sub_wide[, j])
        country_corrs = rbind(country_corrs, c(c,non_missing_count,correlation))
      }
    }
  }
}

######## Correlations across space within one GBOD region? 
# repeat the above, but for GBOD regions instead of countries

######## Correlations across space in different countries?
# repeat, but look for correlations across the entire dataset but only with pairs from different countries

######## Correlations across space >1000km? 
# repeat for all pairs > 1000km apart

######## Correlations across space <1000km? 
# repeat for all pairs < 1000km apart



########################################################################
# C: General correlation over space -- VARIOGRAMS
########################################################################

# create year groupings for variogram
complete = complete %>% mutate(yeargp = (yearnum - min(yearnum)) %/% 5*5 + min(yearnum) )

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
f <- fit.variogram(vv, vgm("Sph"))
fP <- fit.variogram(vvP, vgm("Sph"))

vvplot = plot(vv, model=f, xlab="distance (km)", main = "Model residuals")
vvPplot = plot(vvP, model=fP, xlab="distance (km)", main = "Prevalence (PfPR2)")

vars = ggarrange(vvplot, vvPplot, ncol = 2, nrow = 1, labels="auto")
vars
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "variogram_2panel.png"), plot = vars, width = 9, height = 5, bg = "white")

# By year groupings
range = data.frame(yeargp=NA,n=NA,range=NA)

for(y in unique(spdf$yeargp)){
  test = subset(spdf,yeargp==y)
  if(dim(test)[1]>100){
    vv = variogram(res~1, data=test, projection(FALSE))
    f = fit.variogram(vv, vgm("Sph"))
    range = rbind(range,c(y,dim(test)[1],f$range[2]))
  }
}

range = range %>% arrange(yeargp)
hist(range$range, breaks=30 )
quantile(range$range, probs = c(0.1, 0.5, 0.9, .95, .99), na.rm = TRUE) 

# By country
range = data.frame(country=NA,n=NA,range=NA)

for(c in unique(spdf$country)){
  test = subset(spdf,country==c)
  if(dim(test)[1]>100){
    vv = variogram(res~1, data=test, projection(FALSE))
    vv = subset(vv,dist>0) # many obs of same location
    f = fit.variogram(vv, vgm("Sph"))
    range = rbind(range,c(c,dim(test)[1],f$range[2]))
  }
}

range = range %>% arrange(country) %>% mutate(range=as.numeric(range))
hist(range$range, breaks=30 )
quantile(range$range, probs = c(0.1, 0.5, 0.9, .95, .99), na.rm = TRUE) 

########################################################################
# D: General correlation over time
########################################################################

# As detailed in D03 - Additional robustness.R, the panel is sufficiently unbalanced 
# that estimating a distributed lag at monthly scale is likely not feasible. Instead, look across years.

complete_expanded <- complete %>% 
  mutate(year = as.numeric(as.character(year)),
         month = as.character(month),
         month = match(month, month.abb)) |> 
  group_by(OBJECTID) %>% 
  complete(year = 1902:2016, month = 1:12) %>%
  ungroup()

complete_with_lag <- complete_expanded %>%
  arrange(OBJECTID, year, month) %>%
  group_by(OBJECTID) %>% 
  mutate(
    resmn = res,
    reslag1 = dplyr::lag(resmn,1),
    reslag2 = dplyr::lag(resmn,2),
    reslag3 = dplyr::lag(resmn,3),
    reslag4 = dplyr::lag(resmn,4),
    reslag5 = dplyr::lag(resmn,5)
  ) |> 
  tidyr::drop_na(resmn) 

mn_lag1 <- lm(resmn ~ reslag1, data = complete_with_lag)

mn_lag2 <- lm(resmn ~ reslag1 + reslag2, data = complete_with_lag)

mn_lag3 <- lm(resmn ~ reslag1 + reslag2 + reslag3, data = complete_with_lag)

# Average residuals by ADM1-year
anndf = complete |> group_by(OBJECTID,yearnum) |> dplyr::summarize(resmn = mean(res, na.rm=TRUE), year = first(yearnum))

# Expand to be a full panel 
anndf_ex <- anndf %>% 
  group_by(OBJECTID) %>%
  complete(year = 1902:2016) %>%
  ungroup()

# Add lags
anndf_with_lag <- anndf_ex %>%
  arrange(OBJECTID, year) %>%
  mutate(reslag1 = lag(resmn,1), reslag2 = lag(resmn,2),reslag3 = lag(resmn,3),reslag4 = lag(resmn,4),reslag5 = lag(resmn,5)) |> 
  tidyr::drop_na(resmn)

# Estimation 
lag1 <- lm(resmn ~ reslag1, data = anndf_with_lag)

lag2 <- lm(resmn ~ reslag1 + reslag2, data = anndf_with_lag)

lag3 <- lm(resmn ~ reslag1 + reslag2 + reslag3, data = anndf_with_lag)

lag4 <- lm(resmn ~ reslag1 + reslag2 + reslag3 + reslag4, data = anndf_with_lag)

lag5 <- lm(resmn ~ reslag1 + reslag2 + reslag3 + reslag4 + reslag5, data = anndf_with_lag)

mynote <- "Note"

stargazer(
  mn_lag1, 
  mn_lag2,
  mn_lag3, 
  lag1, lag2, lag3, lag4, lag5,
  title           = "Model diagnostics: Residual lags",
  # align           = TRUE,
  column.labels   = c(
    "1 Mn", "2 Mn","3 Mn",
    "1 Yr", "2 Yr", "3 Yr", "4 Yr", "5 Yr"),
  covariate.labels= c("Res. Lag 1", "Res. Lag 2", "Res. Lag 3",
                      "Res. Lag 4", "Res. Lag 5"),       
  omit.stat       = c("f", "ser"),
  digits          = 2,
  # float           = FALSE,
  type            = "latex",
  notes.append    = TRUE,
  notes.align     = "l",
  notes           = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  out             = file.path(resdir, "Tables", "Diagnostics",
                              "Residuals", "serial_correlation_in_model_residuals.tex")
)

########################################################################
# E. Robustness to various clustering approaches, informed by diagnostics above
########################################################################

###### Main spec (ADM1 clustering)

# plot
plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)
coefs = summary(mainmod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
mainfig =  plotPolynomialResponse(mainmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                              yLab = "Prevalence (%)", title = "ADM1 clust. (main)", yLim=c(-30,5), showYTitle = T)

###### country x year clustering (no correlation over years)

complete = complete |> group_by(country,year) |> mutate(cntryyr = cur_group_id()) |> ungroup()

# Formula 
cntryyr = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | cntryyr"
  )
)

# Estimation
cntryyrmod = felm(data = complete, formula = cntryyr)
# temperature jointly sig?
linearHypothesis(cntryyrmod, "temp + temp2 = 0")['Pr(>Chisq)']

# Plot
coefs = summary(cntryyrmod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
cntryyrfig =  plotPolynomialResponse(cntryyrmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                   yLab = "Prevalence (%)", title = "country-year clust.", yLim=c(-30,5), showYTitle = T)

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
linearHypothesis(cntrymod, "temp + temp2 = 0")['Pr(>Chisq)']

# Plot
coefs = summary(cntrymod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
cntryfig =  plotPolynomialResponse(cntrymod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                              yLab = "Prevalence (%)", title = "country clust.", yLim=c(-30,5), showYTitle = T)
###### GBOD-year clustering

complete = complete |> group_by(smllrgn,year) |> mutate(smllrgnyr = cur_group_id()) |> ungroup()

# Formula 
smllrgnyr = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | smllrgnyr"
  )
)

# Estimation
smllrgnyrmod = felm(data = complete, formula = smllrgnyr)
linearHypothesis(smllrgnyrmod, "temp + temp2 = 0")['Pr(>Chisq)']

# Plot
coefs = summary(smllrgnyrmod)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
smllrgnyrfig =  plotPolynomialResponse(smllrgnyrmod, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                     yLab = "Prevalence (%)", title = "region-year clust.", yLim=c(-30,5), showYTitle = T)


###### Conley
spdf <- complete |>
  dplyr::left_join(centroids, by = join_by(OBJECTID))

# Formula 
conleyform = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 + as.factor(smllrgn):month | OBJECTID "
  )
)
# Estimation
conleymod1 = feols(conleyform, data=spdf, conley(500, distance = "spherical"))
linearHypothesis(conleymod1, "temp + temp2 = 0")['Pr(>Chisq)']
conleymod2 = feols(conleyform, data=spdf, conley(1000, distance = "spherical"))
linearHypothesis(conleymod2, "temp + temp2 = 0")['Pr(>Chisq)']
conleymod3 = feols(conleyform, data=spdf, conley(2000, distance = "spherical"))
linearHypothesis(conleymod3, "temp + temp2 = 0")['Pr(>Chisq)']


# Plot
coefs = summary(conleymod1)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
conleyfig1 =  plotPolynomialResponse(conleymod1, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                   yLab = "Prevalence (%)", title = "Conley (500km)", yLim=c(-30,5), showYTitle = T)

coefs = summary(conleymod2)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
conleyfig2 =  plotPolynomialResponse(conleymod2, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                     yLab = "Prevalence (%)", title = "Conley (1,000km)", yLim=c(-30,5), showYTitle = T)

coefs = summary(conleymod3)$coefficients[1:2]
myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
conleyfig3 =  plotPolynomialResponse(conleymod3, "temp", plotXtemp, polyOrder = 2, cluster = T, xRef = myrefT, xLab = expression(paste("Mean temperature (",degree,"C)")), 
                                     yLab = "Prevalence (%)", title = "Conley (2,000km)", yLim=c(-30,5), showYTitle = T)
####### Save
# feols models do not work with stargazer as it has no method for feols objects (class "fixest")
# so we use stargazer on the felm objects and etable on the feols objects. The two tables are 
# then combined manually

# tabular output
modellist = list(
  mainmod,
  cntryyrmod,
  cntrymod,
  smllrgnyrmod
  # conleymod1,
  # conleymod2,
  # conleymod3
)
mycollabs = c(
  "main spec.", 
  "country-year clust.",
  "country clust.",
  "region-year clust."
  # "Conley: 500km",
  # "Conley: 1,000km",
  # "Conley: 1,500km"
)

# breaking - use modelsummary() instead?
mynote = "Column specifications: (1) main specification (standard errors clustered at ADM1 level); (2) standard errors clustered at country-year level; (3) standard errors clustered at country level; (4) standard errors clustered at GBOD region-year level; (5) standard errors estimated following Conley (2008) using 500km cutoff; (6) standard errors estimated following Conley (2008) using a 2,000km cutoff."
stargazer(modellist,
          title="Quadratic temperature: standard error sensitivity", align=TRUE, column.labels = mycollabs,
          keep = c("temp", "flood", "drought", "intervention", "METHOD"),
          out = file.path(resdir, "Tables", "Diagnostics","Residuals","uncertainty.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F,
          notes.append = TRUE, digits=2,notes.align = "l", notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"))

conley_tab <- etable(
  conleymod2, conleymod3,    
  # vcov = list(
  #   vcov_conley(conleymod1, cutoff = 500, distance = "spherical"),              
  #   vcov_conley(conleymod2, cutoff = 1000, distance = "spherical"),
  #   vcov_conley(conleymod3, cutoff = 1500, distance = "spherical")
  # ),
  keep = c("temp", "flood", "drought", "intervention", "METHOD"),
  tex     = TRUE,    
  digits  = 2,       
  # title   = mynote,
  label   = "tab:conley"   # optional: LaTeX label
)

# figure output
uncert = plot_grid(mainfig,cntryyrfig,cntryfig,smllrgnyrfig,conleyfig1, conleyfig3,nrow = 2)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Residuals", "temp_response_difft_SEs.pdf"), plot = uncert, width = 8, height = 5)

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

