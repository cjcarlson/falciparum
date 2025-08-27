############################################################
# This script conducts additional robustness checks
# This script should be incorporated into D01 when a subset of 
# tests are included in the main text and/or Supplement.
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

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here("Pipeline", "A - Utility functions", "A01 - Utility code for calculations.R"))
source(here::here("Pipeline", "A - Utility functions", "A02 - Utility code for plotting.R"))

 CRUversion = "4.03" # "4.06"
if (CRUversion=="4.03") {
  resdir = file.path(datadir, "Results")
} else if (CRUversion=="4.06") {
  resdir = file.path(datadir, "Results_CRU-TS4-06")
} else {
  print('CRU version not supported! Use 4.03 or 4.06.')
}

############################################################
# Plotting toggles
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tref = 25 # reference temperature - curve gets recentered to 0 here
Tmin = 10 # min T for x axis
Tmax = 40 # max T for x axis

########################################################################
# Data clean up ----
########################################################################

#### Call external script for data cleaning
source(here::here("Pipeline", "A - Utility functions", "A03 - Prep data for estimation.R"))

########################################################################
# Check for missing values if we were to use prevalence lag
########################################################################

# First ensure that for each OBJECTID and year we have months 1 through 12
# and years 1902 through 2016 (1 row for each month of each year).
# This intentionally introduces many NAs, but allows us to calculate the lag.
complete_expanded <- complete %>% 
  mutate(year = as.numeric(as.character(year)),
         month = as.character(month),
         month = match(month, month.abb)) |> 
  group_by(OBJECTID) %>% 
  complete(year = 1902:2016, month = 1:12) %>%
  ungroup()

# Add prevalence lag to complete
complete_with_lag <- complete_expanded %>%
  arrange(OBJECTID, year, month) %>%
  mutate(PfPR2_lag = dplyr::lag(PfPR2)) |> 
  tidyr::drop_na(PfPR2)

# Drop rows with missing lag values
complete_with_complete_lags <- tidyr::drop_na(complete_with_lag)

# Calculate the percentage of observations lost
starting_obs <- length(complete$OBJECTID)
obs_after_lag <- length(complete_with_complete_lags$OBJECTID)
difference <- starting_obs - obs_after_lag
percent_lost <- (difference / starting_obs) * 100

########################################################################
# Does diagnostic method change with T and P shocks? ---- 
########################################################################

# common variables in all regs
common <- paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars)
country_time <- "country:monthyr + country:monthyr2"

# main spec: cXt2intrXm
cXt2intrXm = as.formula(paste0(common, " + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

# new outcome var: Probability method is XX
complete <- complete |> mutate(microscopy = (simplified_METHOD=="MICROSCOPY"))
PrMicro = as.formula(paste0("microscopy ~ temp + temp2 + ", floodvars, " + ", droughtvars, " + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

Micromod = felm(data = complete, formula = PrMicro)
mainmod = felm(data = complete, formula = cXt2intrXm)

# stargazer
dir.create(file.path(resdir, "Tables", "sensitivity"), showWarnings = FALSE)
stargazer(Micromod,
          title="Microscopy method test", align=TRUE,
          keep = c("temp", "flood", "drought"),
          out = file.path(resdir, "Tables", "sensitivity","Microscopy_sample_sensitivity.tex"),  omit.stat=c("f", "ser"), out.header = FALSE, type = "latex", float=F)

########################################################################
# Overdispersion? ----
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

########################################################################
# Control for diagnostic method ----
########################################################################

###### Estimate regressions #######
complete_dm <- filter(complete, dominant_METHOD != "LAMP")

cXt2intrXmDM = as.formula(paste0(common, " + dominant_METHOD + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2intrXmSM = as.formula(paste0(common, " + simplified_METHOD + I(intervention) + ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

myforms = c(cXt2intrXm, cXt2intrXmDM, cXt2intrXmSM) 

mycollabs = c(
  "main specification", # Main Spec
  "+ diag. method (full set)", # Main Spec with dominant method
  "+ diag. method (small set)" # Main Spec with simplified method
)

modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete_dm, formula = m)
}

mynote = "Column specifications: (1) country-specific quad. trends, intervention year FE, GBOD region-by-month FE; (2) same as (1), but with additional controls for diagnostic method: Microscopy, Microcscopy/PCR Confirmed, PCR, RDT, RDT/PCR Confirmed, and RDT/SLIDE Confirmed; (3) same as (2) but using simplified diagnostic method control set: Microscopy, PCR, RDT."
dir.create(file.path(resdir, "Tables", "sensitivity"), showWarnings = FALSE)
stargazer(
  modellist,
  title = "Sensitivity to controlling for diagnostic method", 
  align = TRUE, 
  column.labels = mycollabs,
  keep = c("temp", "flood", "drought", "METHOD"),
  out = file.path(resdir, "Tables", "sensitivity","DiagMethod_sensitivity.tex"),  
  omit.stat = c("f", "ser"), 
  out.header = FALSE,
  type = "latex", 
  float = F,
  notes.append = TRUE, 
  digits = 2,
  notes.align = "l", 
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}")
)

####### Plot temperature responses #######

plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)

figList = list()
for(m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]], "temp", plotXtemp,
    polyOrder = 2, cluster = T, xRef = myrefT,
    xLab = expression(paste("Mean temperature (",degree,"C)")), 
    yLab = "Prevalence (%)", title = mycollabs[m], yLim=c(-30,10), showYTitle = T) +
    theme(plot.title = element_text(size = 10))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], nrow=1)
p

fe_fig_dir <- file.path(resdir, "Figures", "Diagnostics","Fixed_effects")
dir.create(fe_fig_dir, showWarnings = FALSE)
ggsave(
  filename = "diagnostic_method_sensitivity.pdf",
  path = fe_fig_dir, 
  plot = p, 
  width = 9, 
  height = 3
)

########################################################################
# Data imbalance: responses on temporal subsamples ----
########################################################################

complete = complete |> mutate(yearnum = as.numeric(as.character(year)))
g = ggplot(complete) + geom_histogram(aes(x=yearnum), color= "seagreen", fill = "seagreen") + 
  xlab("year") + ylab("count of observations") + theme_classic()
g

# obs by group
complete = complete |> mutate(post1995 = (yearnum>=1995))
complete %>% count(post1995)

# formula (different intervention dummies for each temporal subsample)
cXt2rXm = as.formula(paste0(common, " + I(intervention) +  ", country_time, " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))

pre_data <- subset(complete, post1995==FALSE)
pos_data <- subset(complete, post1995==TRUE)

pre1995 = felm(data = pre_data, formula = cXt2rXm)
post1995 = felm(data = pos_data, formula = cXt2rXm)

# plot temperature responses
modellist = list(pre1995,post1995)
mycollabs = c(
  "Early sample (1901-1994)",
  "Late sample (1995-2016)"
)

percentiles_list = list()
pre_post <- c(F,T)
for(i in 1:length(pre_post)) { # i <- 1
  pre_post_data <- subset(complete, post1995==pre_post[i])$temp
  temp_p01 <- quantile(pre_post_data, 0.01, na.rm = TRUE)
  temp_p99 <- quantile(pre_post_data, 0.99, na.rm = TRUE)
  percentiles_list[[i]] <- list(p01 = temp_p01, p99 = temp_p99, n = length(pre_post_data))
}

plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)
figList = list()
for(m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]], "temp", plotXtemp,
    polyOrder = 2, cluster = T, xRef = myrefT,
    xLab = expression(paste("Mean temperature (",degree,"C)")), 
    yLab = "Prevalence (%)", title = mycollabs[m], yLim=c(-30,10), showYTitle = T) +
    theme(plot.title = element_text(size = 10)) +
    geom_vline(xintercept = percentiles_list[[m]]$p01, colour = "grey39", linetype = "dashed") +
    geom_vline(xintercept = percentiles_list[[m]]$p99, colour = "grey39", linetype = "dashed")
}

h_pre <- ggplot(data = pre_data, aes(x = temp)) +
  geom_histogram(
    fill = "#8B3A4A",
    alpha = 1,
    bins = 30,
    width = 0.7,
    colour = "black"
  ) +
  theme_classic() +
  labs(x = expression(paste("Mean temperature (",degree,"C)")), y = NULL) +
  geom_vline(xintercept = percentiles_list[[1]]$p01, colour = "grey39", linetype = "dashed") +
  geom_vline(xintercept = percentiles_list[[1]]$p99, colour = "grey39", linetype = "dashed") +
  annotate(
    geom = "text", x = 36, y = 0, vjust = -1, 
    label = bquote(italic("N")~"="~.(percentiles_list[[1]]$n)),
    size = 3
  ) +
  xlim(Tmin - .0001, Tmax) +
    geom_vline(xintercept = 22, colour = "grey39") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(-2.5, 1, 0, 1), "cm"),
  )

h_post <- ggplot(data = pos_data, aes(x = temp)) +
  geom_histogram(
    fill = "#8B3A4A",
    alpha = 1,
    bins = 30,
    width = 0.7,
    colour = "black"
  ) +
  theme_classic() +
  labs(x = expression(paste("Mean temperature (",degree,"C)")), y = NULL) +
  xlim(Tmin - .0001, Tmax) +
  geom_vline(xintercept = 25, colour = "grey39") +
  geom_vline(xintercept = percentiles_list[[2]]$p01, colour = "grey39", linetype = "dashed") +
  geom_vline(xintercept = percentiles_list[[2]]$p99, colour = "grey39", linetype = "dashed") +
  annotate(
    geom = "text", x = 36, y = 0, vjust = -1, 
    label = bquote(italic("N")~"="~.(percentiles_list[[2]]$n)),
    size = 3
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(-2.5, 1, 0, 1), "cm"),
  )

p = plot_grid(
  figList[[1]] + 
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ),
  figList[[2]] +
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ), 
  h_pre, 
  h_post,  
  nrow=2,
  align = "v",
  rel_heights = c(15, 1)
)
# p

ggsave(
  filename = "split_sample_1995.pdf",
  # path = file.path(resdir, "Figures", "Diagnostics", "Subsamples"),
  plot = p, 
  width = 10, 
  height = 5
)

########################################################################
# Data imbalance: responses on spatial subsamples ----
########################################################################
#### quadratic
# Regression for each region, no regionXmo FE because we are using region-specific models
regions = unique(complete$smllrgn)
cXt2int = as.formula(paste0(common, " + I(intervention) + ", country_time, " | OBJECTID  + as.factor(month) | 0 | OBJECTID"))

modellist = list()
for (i in 1:length(regions)) {
  mydf = subset(complete, smllrgn==regions[i])
  modellist[[i]] = felm(data = mydf, formula = cXt2int)
}

percentiles_list = list()
for(i in 1:length(regions)) {
  region_data <- subset(complete, smllrgn==regions[i])$temp
  temp_p01 <- quantile(region_data, 0.01, na.rm = TRUE)
  temp_p99 <- quantile(region_data, 0.99, na.rm = TRUE)
  percentiles_list[[i]] <- list(p01 = temp_p01, p99 = temp_p99, n = length(region_data))
  cat(regions[i], ": ", length(region_data), "\n")
}

# Plot them all next to each other
mycollabs = c(
  paste0(regions[1]),paste0(regions[2]),paste0(regions[3]),paste0(regions[4])
)

plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2)
figList = list()
refTemps = numeric(length(modellist))
plot_ref_vec = c(F, T, T, T)
for(m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1*coefs[1]/(2*coefs[2]), digits = 0), 10)
  refTemps[m] = myrefT 
  plot_ref = plot_ref_vec[m]
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]], "temp", plotXtemp,
    polyOrder = 2, cluster = T, xRef = myrefT, plotmax = plot_ref,
    xLab = expression(paste("Mean temperature (",degree,"C)")),
    yLab = "Prevalence (%)", title = mycollabs[m], yLim=c(-30,10), showYTitle = T) +
    theme(plot.title = element_text(size = 10)) +
    geom_vline(xintercept = percentiles_list[[m]]$p01, colour = "grey39", linetype = "dashed") +
    geom_vline(xintercept = percentiles_list[[m]]$p99, colour = "grey39", linetype = "dashed")
}

# p = plot_grid(figList[[1]], figList[[2]], figList[[3]], figList[[4]], nrow=1)
# p
# ggsave(
#   filename = "split_GBOD_2nd_poly.pdf",
#   # path = file.path(resdir, "Figures", "Diagnostics", "Subsamples"), 
#   plot = p, 
#   width = 12,
#   height = 4
# )

histList = list()
for(i in 1:length(regions)) {
  region_data <- subset(complete, smllrgn==regions[i])
  temp_p01 <- percentiles_list[[i]]$p01
  temp_p99 <- percentiles_list[[i]]$p99
  n <- percentiles_list[[i]]$n
  
   t_hist <- ggplot(region_data, aes(x = temp)) +
    geom_histogram(
      fill = "#8B3A4A",
      alpha = 1,
      bins = 30,
      width = 0.7,
      colour = "black"
    ) +
    theme_classic() +
    labs(x = expression(paste("Mean temperature (",degree,"C)")), y = NULL) +
    xlim(Tmin, Tmax) +
    geom_vline(xintercept = temp_p01, colour = "grey39", linetype = "dashed") +
    geom_vline(xintercept = temp_p99, colour = "grey39", linetype = "dashed") +
    annotate(
      geom = "text", x = 36, y = 0, vjust = -1, 
      label = bquote(italic("N")~"="~.(n)),
      size = 3
    ) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = unit(c(-1.5, 0, 0, 0), "cm"),
    )
  plot_ref <- plot_ref_vec[i]
  if (plot_ref == T) {
    t_hist <- t_hist +
    geom_vline(xintercept = refTemps[i], colour = "grey39") 
  }
  histList[[i]] <- t_hist
}

# Combine response plots and histograms
p = plot_grid(
  figList[[1]] + 
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ) ,
  figList[[2]] +
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ),
  figList[[3]] +
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ),
  figList[[4]] +
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ),
  histList[[1]], 
  histList[[2]], 
  histList[[3]], 
  histList[[4]],  
  nrow=2,
  align = "v",
  rel_heights = c(15, 1)
)

# p
ggsave(
  filename = "split_GBOD_temp_hist.pdf",
  # path = file.path(resdir, "Figures", "Diagnostics", "Subsamples"), 
  plot = p, 
  width = 12,
  height = 4
)


# #### cubic
# # Regression for each region, no regionXmo FE because we are using region-specific models
# regions = unique(complete$smllrgn)
# cXt2int = as.formula(
#   paste0("PfPR2 ~ temp + temp2 + temp3 +",
#   floodvars, " + ", droughtvars, "+ I(intervention) + ", country_time, 
#   " | OBJECTID  + as.factor(month) | 0 | OBJECTID"
# ))

# modellist = list()
# for (i in 1:length(regions)) {
#   mydf = subset(complete, smllrgn==regions[i])
#   modellist[[i]] = felm(data = mydf, formula = cXt2int)
# }

# # Plot them all next to each other
# mycollabs = c(
#   paste0(regions[1]),paste0(regions[2]),paste0(regions[3]),paste0(regions[4])
# )

# plotXtemp = cbind(seq(Tmin,Tmax), seq(Tmin,Tmax)^2, seq(Tmin,Tmax)^3)
# figList = list()

# for(m in 1:length(modellist)) {
#   # Extract all 3 coefficients for cubic polynomial
#   coefs = summary(modellist[[m]])$coefficients[1:3]  # temp, temp2, temp3
  
#   # For cubic f(x) = a + bx + cx^2 + dx^3, derivative is f'(x) = b + 2cx + 3dx^2
#   # Setting f'(x) = 0: b + 2cx + 3dx^2 = 0
#   # This is quadratic in x: 3dx^2 + 2cx + b = 0
#   # Using quadratic formula: x = (-2c ± √(4c^2 - 12bd)) / (6d)
  
#   b_coef = coefs[1]  # temp coefficient
#   c_coef = coefs[2]  # temp2 coefficient  
#   d_coef = coefs[3]  # temp3 coefficient
  
#   # Calculate discriminant
#   discriminant = 4 * c_coef^2 - 12 * b_coef * d_coef
  
#   if(discriminant >= 0 && d_coef != 0) {
#     # Two potential critical points
#     x1 = (-2 * c_coef + sqrt(discriminant)) / (6 * d_coef)
#     x2 = (-2 * c_coef - sqrt(discriminant)) / (6 * d_coef)
    
#     # Evaluate function at both points to find maximum within reasonable range
#     temp_range = seq(Tmin, Tmax, length.out = 1000)
#     temp_vals = b_coef * temp_range + c_coef * temp_range^2 + d_coef * temp_range^3
#     myrefT = temp_range[which.max(temp_vals)]
    
#     # Ensure it's within reasonable bounds
#     myrefT = max(min(myrefT, Tmax), Tmin)
#     myrefT = max(round(myrefT, digits = 0), 10)
#   } else {
#     # Fallback: find maximum numerically within the temperature range
#     temp_range = seq(Tmin, Tmax, length.out = 1000)
#     temp_vals = b_coef * temp_range + c_coef * temp_range^2 + d_coef * temp_range^3
#     myrefT = max(round(temp_range[which.max(temp_vals)], digits = 0), 10)
#   }
  
#   print(paste("Region", regions[m], "- Reference temp:", myrefT))
  
#   figList[[m]] = plotPolynomialResponse(
#     modellist[[m]], "temp", plotXtemp,
#     polyOrder = 3,  plotmax = T, cluster = T, xRef = myrefT,
#     xLab = expression(paste("Mean temperature (",degree,"C)")),
#     yLab = "Prevalence (%)", title = mycollabs[m], yLim=c(-30,10), showYTitle = T) +
#     theme(plot.title = element_text(size = 10))
# }

# p = plot_grid(figList[[1]], figList[[2]], figList[[3]], figList[[4]], nrow=1)
# ggsave(
#   filename = "split_GBOD_3rd_poly.pdf",
#   # path = file.path(resdir, "Figures", "Diagnostics", "Subsamples"), 
#   plot = p, 
#   width = 12,
#   height = 4
# )

# TO DO: Add histograms of temp underneath each curve; add 3rd order polynomial and it's SE on top of the existing ones (ala urban/rural)

########################################################################
# Overlay main spec and CIs with results from models with other FE ----
########################################################################

#setup 
Tref = 24
Tmin = 10
Tmax = 40
int = 0.1
plotXtemp = cbind(seq(Tmin,Tmax,by=int), seq(Tmin,Tmax,by=int)^2)
xValsT = genRecenteredXVals_polynomial(plotXtemp,Tref,2)

# point estimate and CIs for main spec
beta = mainmod$coefficients 
vars = rownames(beta)
plotVars = vars[grepl(pattern = "temp", x = vars)] 
b = as.matrix(beta[rownames(beta) %in% plotVars])
vcov = getVcov(mainmod$clustervcv, plotVars)
response = as.matrix(xValsT) %*% b #Prediction
length = 1.96 * sqrt(apply(X = xValsT, FUN = calcVariance, MARGIN = 1, vcov))
lb = response - length
ub = response + length

#Plotgin dataframe -- add back in the reference temperature so it's centered at xRef
plotData = data.frame(x = xValsT[,1] + Tref, response = response, lb = lb, ub = ub)
sub = plotData[plotData$x>=10 & plotData$x<=30,]
maxX = max(sub$x[sub$response==max(sub$response)])

# point estimates for all other FE checks
cym = as.formula(paste0(common, " | OBJECTID + year + month | 0 | OBJECTID"))
cXt2m = as.formula(paste0(common, " + ", country_time, " | OBJECTID  + month | 0 | OBJECTID"))
cXt2cXm = as.formula(paste0(common, " + ", country_time, " | OBJECTID + country:month | 0 | OBJECTID"))
cXt2intm = as.formula(paste0(common, " + ", country_time, " | OBJECTID  + intervention + month | 0 | OBJECTID"))
cXt2intcXm = as.formula(paste0(common, " + I(intervention) + ", country_time, " | OBJECTID  + country:month | 0 | OBJECTID"))
rXyrXm = as.formula(paste0(common, " | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))
rXycXm = as.formula(paste0(common, " | OBJECTID + country:month + as.factor(smllrgn):year | 0 | OBJECTID"))
rXyrXmcXt = as.formula(paste0(common, " + country:monthyr | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))
myforms = c(
  cym, cXt2m, cXt2cXm, cXt2intm, 
  cXt2intcXm, rXyrXm, rXycXm, rXyrXmcXt
) 
mycollabs = c(
  "cym", "cXt2m", "cXt2cXm", "cXt2intm", 
  "cXt2intcXm", "rXyrXm", "rXycXm", "rXyrXmcXt"
)

# Run all robustness models
modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete, formula = m)
}

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
plotFE = plotData %>% dplyr::select(x,cym:rXyrXmcXt)
plotFE = plotFE %>% gather(plotFE, response, cym:rXyrXmcXt)  
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
  theme_classic() + 
  labs(x = expression(paste("Mean temperature (",degree,"C)")), y = "Prevalence (%)") + 
  xlim(Tmin,Tmax) + 
  theme(
    axis.title.x = element_text(vjust = -3),
    axis.title.y = element_text(vjust = 5),
    plot.margin = unit(c(0.3,0.3,1,1), units = "cm"))
g

ggsave(file.path(resdir, "Figures", "Diagnostics", "Fixed_effects", "overlaid_specifications_Tresponse.pdf"), plot = g, width = 4.5, height = 5)

