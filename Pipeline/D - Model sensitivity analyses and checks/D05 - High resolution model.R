############################################################
# This script re-estimates the main empirical specification
# linking PfPR2 to drought, flood, and temperature using
# high-resolution grid-level CRU data as a robustness check.
############################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  here,
  lfe,
  reshape,
  stargazer,
  tidyverse,
  zoo,
  lubridate,
  cowplot,
  multcomp,
  patchwork
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

############################################################
# Plotting toggles ----
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tmin = 10 # min T for x axis
Tmax = 40 # max T for x axis

############################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence
# and CRU temperature and precipitation data aggregated to
# the first level of Administrative division.
############################################################

print("Loading data")
complete <- readr::read_csv(prev_clim_data_grid_fp, show_col_types = FALSE)

# need this for country specific quadratic trends
complete$monthyr2 = complete$monthyr^2

# define key intervention periods
complete$intervention = ifelse(
  complete$yearnum >= 1955 & complete$yearnum <= 1969,
  1,
  0
)
complete$intervention[complete$yearnum >= 2000 & complete$yearnum <= 2015] = 2
complete$intervention = as.factor(complete$intervention)

# classes: important for ensuring felm is treating these correctly
complete$month = as.factor(complete$month)
complete$year = as.factor(complete$year)

complete <- dplyr::rename(complete, country = COUNTRY)

gbod <- sf::read_sf(
  file.path(
    data_dir,
    "Data",
    "OriginalGBD",
    "WorldRegions.shp"
  )
)
# head(gbod@data)

gboddf = as.data.frame(gbod)
gboddf = gboddf %>% dplyr::select("ISO", "NAME_0", "Region", "SmllRgn")
gboddf = gboddf %>%
  group_by(ISO, NAME_0) %>%
  summarize(Region = first(Region), SmllRgn = first(SmllRgn)) # note that the small regions are homogenous within country
colnames(gboddf) = c("ISO", "country", "region", "smllrgn")
gboddf$country = as.character(gboddf$country)

# clean to merge
gboddf$country = ifelse(
  gboddf$country == "Cote D'Ivoire",
  "Côte d'Ivoire",
  gboddf$country
)

complete$country = as.character(complete$country)
complete = left_join(complete, gboddf, by = "country")
complete$country = as.factor(complete$country)

#### Create necessary subfolders
dir.create(file.path(resdir, "Tables"), showWarnings = FALSE)
dir.create(file.path(resdir, "Figures"), showWarnings = FALSE)
dir.create(file.path(resdir, "Models"), showWarnings = FALSE)

########################################################################
# Estimation
########################################################################

# Formula (see other files for robustness/sensitivity checks)
cXt2intrXm = as.formula(
  paste0(
    "`PfPR2-10` ~ temp + temp2 + ",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)

# Estimation & save model results
highresmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(highresmod$coefficients)
vcov = as.data.frame(highresmod$clustervcv)
dir.create(file.path(resdir, "Models", "reproducibility"), showWarnings = FALSE)
bfn = file.path(
  resdir,
  "Models",
  "reproducibility",
  "coefficients_cXt2intrXm_highres.rds"
)
vfn = file.path(
  resdir,
  "Models",
  "reproducibility",
  "vcv_cXt2intrXm-highres.rds"
)
saveRDS(coeffs, file = bfn)
saveRDS(vcov, file = vfn)

# Stargazer output
mynote = "High Resolution Model: Country-specific quad. trends with intervention FE and country by month FE."
dir.create(file.path(resdir, "Tables", "main"), showWarnings = FALSE)
stargazer(
  highresmod,
  title = "PfPR2 response to daily avg. temperature",
  align = TRUE,
  keep = c("temp", "flood", "drought", "intervention"),
  out = file.path(
    resdir,
    "Tables",
    "main",
    "main_specification_cXt2intrXm-highres.tex"
  ),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}")
)

########################################################################
# Plot (Note: analogous to Fig 2A but with analytically derived confidence intervals
# in place of bootstrap runs shown in Fig 2A)
########################################################################

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

coefs = summary(highresmod)$coefficients[1:2]
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) # plot relative to max of quadratic function

beta <- highresmod$coefficients
vars <- rownames(beta)
patternForPlotVars <- "temp"
plotVars <- vars[grepl(patternForPlotVars, vars)]

# t <- plotPolynomialResponse(
#   mod = highresmod,
#   patternForPlotVars = "temp",
#   xVals = plotXtemp,
#   polyOrder = 2,
#   cluster = TRUE,
#   xRef = myrefT,
#   xLab = expression(paste("Mean temperature (", degree, "C)")),
#   yLab = "Prevalence (%)",
#   title = NULL,
#   yLim = c(-30, 10),
#   showYTitle = TRUE
# )

# ylims <- c(-3, 3)

# d <- plotLinearLags(
#   mod = highresmod,
#   patternForPlotVars = "drought",
#   cluster = TRUE,
#   laglength = 3,
#   xLab = "Drought Lag",
#   yLab = "Coefficient",
#   title = NULL,
#   yLim = ylims
# )

# f <- plotLinearLags(
#   mod = highresmod,
#   patternForPlotVars = "flood",
#   cluster = TRUE,
#   laglength = 3,
#   xLab = "Flood Lag",
#   yLab = "Coefficient",
#   title = NULL,
#   yLim = ylims
# )

# combined_plot <- t +
#   d +
#   f +
#   plot_layout(ncol = 3, guides = "collect") &
#   theme(
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 8),
#     # legend.position = "bottom",
#     # legend.margin = margin(0, 0, 0, 0)
#   )

# combined_plot

# dir.create(
#   file.path(resdir, "Figures", "Diagnostics", "Main_model"),
#   showWarnings = FALSE
# )
# ggsave(
#   filename = "temp_response_cXt2intrXm-highres.pdf",
#   path = file.path(resdir, "Figures", "Diagnostics", "Main_model"),
#   plot = combined_plot,
#   width = 7,
#   height = 2.5,
# )

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

############################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence 
# and CRU temperature and precipitation data aggregated to 
# the first level of Administrative division.
############################################################

print("Loading clean data")
complete <- readr::read_rds(replication_fp) 

########################################################################
# Estimation
########################################################################

# Estimation & save model results
mainmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)
dir.create(file.path(resdir, "Models", "reproducibility"), showWarnings = FALSE)
bfn = file.path(
  resdir,
  "Models",
  "reproducibility",
  "coefficients_cXt2intrXm.rds"
)
vfn = file.path(resdir, "Models", "reproducibility", "vcv_cXt2intrXm.rds")
saveRDS(coeffs, file = bfn)
saveRDS(vcov, file = vfn)

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

coefs = summary(mainmod)$coefficients[1:2]
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) # plot relative to max of quadratic function

t1 = plotPolynomialResponse_2_mod(
  mainmod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = NULL,
  yLim = c(-30, 5),
  showYTitle = T,
  mod2 = highresmod,
  model1_name = "Main",
  model2_name = "Grid level",
  fillcolor2 = "grey50"
)
t1

d1 <- plotLinearLags_2_mod(
  mod = mainmod,
  model1_name = "Main",
  patternForPlotVars = "drought",
  cluster = T,
  laglength = 3,
  xLab = "Drought (month lags)",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-4, 4),
  mod2 = highresmod,
  model2_name = "Grid level"
)
d1

f1 <- plotLinearLags_2_mod(
  mod = mainmod,
  model1_name = "Main",
  patternForPlotVars = "flood",
  cluster = T,
  laglength = 3,
  xLab = "Flood (month lags)",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-4, 4),
  mod2 = highresmod,
  model2_name = "Grid level"
)
f1

combined_plot1 <- t1 +
  d1 +
  f1 +
  plot_layout(ncol = 3, guides = "collect") &
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0)
  )

combined_plot1

dir.create(
  file.path(resdir, "Figures", "Diagnostics", "Main_model"),
  showWarnings = FALSE
)
ggsave(
  filename = "temp_drought_flood_cXt2intrXm_w_adm1_and_high_res.pdf",
  path = file.path(resdir, "Figures", "Diagnostics", "Main_model"),
  plot = combined_plot1,
  width = 7,
  height = 2.5,
  dpi = 300
)
