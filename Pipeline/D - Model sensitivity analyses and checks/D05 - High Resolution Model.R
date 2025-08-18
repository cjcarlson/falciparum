############################################################
# This script estimates the main empirical specification linking
# PfPR2 to drought, flood, and temperature.
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
library(patchwork)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(
  "Pipeline",
  "A - Utility functions",
  "A01 - Utility code for calculations.R"
))
source(here::here(
  "Pipeline",
  "A - Utility functions",
  "A02 - Utility code for plotting.R"
))

# CRUversion = "4.03" # "4.06"
if (CRUversion == "4.03") {
  resdir = file.path(datadir, "Results")
} else if (CRUversion == "4.06") {
  resdir = file.path(datadir, "Results_CRU-TS4-06")
} else {
  print('CRU version not supported! Use 4.03 or 4.06.')
}

############################################################
# Plotting toggles
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tref = 24 #reference temperature - curve gets recentered to 0 here
Tmin = 10 #min T for x axis
Tmax = 40 #max T for x axis

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
complete <- readr::read_csv(
  file.path(
    datadir,
    "Data",
    paste0('CRU-', CRUversion, '-Points-Reextraction-May2025.csv')
  ),
  show_col_types = FALSE
)

# cont <- sf::read_sf(here::here(datadir, 'Data', 'AfricaADM1.shp'))

# # Convert to sf object with POINT geometry
# prev_sf <- sf::st_as_sf(
#   prev_df,
#   coords = c("Long", "Lat"),
#   crs = 4326
# )

# # Join the prevalence data to the continent shapefile
# prev_with_cont <- sf::st_join(prev_sf, cont)

# include: contemporaneous temp, then distributed lag in flood and drought
floodvars = paste(
  colnames(complete)[grep("flood", colnames(complete))],
  collapse = " + "
)
droughtvars = paste(
  colnames(complete)[grep("drought", colnames(complete))],
  collapse = " + "
)

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
    datadir,
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

# complete$dominant_METHOD = as.factor(complete$dominant_METHOD)
# complete$simplified_METHOD = as.factor(complete$simplified_METHOD)

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

t <- plotPolynomialResponse(
  mod = highresmod,
  patternForPlotVars = "temp",
  xVals = plotXtemp,
  polyOrder = 2,
  cluster = TRUE,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = NULL,
  yLim = c(-30, 10),
  showYTitle = TRUE
) +
  theme(plot.title = element_text(size = 10))

d <- plotLinearLags(
  mod = highresmod,
  patternForPlotVars = "drought",
  cluster = TRUE,
  laglength = 3,
  xLab = "Drought Lag",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-5, 8)
)

f <- plotLinearLags(
  mod = highresmod,
  patternForPlotVars = "flood",
  cluster = TRUE,
  laglength = 3,
  xLab = "Flood Lag",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-5, 8)
)

combined_plot <- t +
  d +
  f +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0))

combined_plot

dir.create(
  file.path(resdir, "Figures", "Diagnostics", "Main_model"),
  showWarnings = FALSE
)
ggsave(
  file.path(
    resdir,
    "Figures",
    "Diagnostics",
    "Main_model",
    "temp_response_cXt2intrXm-highres.pdf"
  ),
  plot = combined_plot,
  width = 7,
  height = 2.5,
)



########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################




########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
source(here::here("Pipeline", "A - Utility functions", "A03 - Prep data for estimation.R"))

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
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)

# Estimation & save model results
mainmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)
dir.create(file.path(resdir, "Models", "reproducibility"), showWarnings = FALSE)
bfn = file.path(resdir, "Models", "reproducibility", "coefficients_cXt2intrXm.rds")
vfn = file.path(resdir, "Models", "reproducibility", "vcv_cXt2intrXm.rds")
saveRDS(coeffs, file=bfn)
saveRDS(vcov, file=vfn)

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

coefs = summary(mainmod)$coefficients[1:2]
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) # plot relative to max of quadratic function
t1 = plotPolynomialResponse(
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
  showYTitle = T
)

d1 <- plotLinearLags(
  mod = mainmod,
  patternForPlotVars = "drought",
  cluster = TRUE,
  laglength = 3,
  xLab = "Drought Lag",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-5, 8)
)

f1 <- plotLinearLags(
  mod = mainmod,
  patternForPlotVars = "flood",
  cluster = TRUE,
  laglength = 3,
  xLab = "Flood Lag",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-5, 8)
)

combined_plot1 <- t1 +
  d1 +
  f1 +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0))

combined_plot1

dir.create(
  file.path(resdir, "Figures", "Diagnostics", "Main_model"),
  showWarnings = FALSE
)
ggsave(
  file.path(
    resdir,
    "Figures",
    "Diagnostics",
    "Main_model",
    "temp_drought_flood_cXt2intrXm_w_adm1_and_high_res.pdf"
  ),
  plot = combined_plot1,
  width = 7,
  height = 2.5,
)










mod <- mainmod


response_curve_poly_df <- function(mod, pattern = "temp",
                                   xVals, xRef = Tref,
                                   cluster = TRUE, label = "Model") {
  beta <- mod$coefficients
  cn   <- rownames(beta)

  # keep the model's native order (matches VCOV order)
  coef_names <- cn[grepl(pattern, cn)]
  if (!length(coef_names)) stop("No coefficients matching '", pattern, "' found in model.")

  Vfull <- if (cluster) mod$clustervcv else mod$vcv
  # guard against names drifting between coeffs and VCOV
  coef_names <- intersect(coef_names, rownames(Vfull))
  if (!length(coef_names)) stop("No overlap between coefficients and VCOV for pattern '", pattern, "'.")

  # infer polynomial order from the number of matching coefficients
  polyOrder <- length(coef_names)

  # recenters internally; only the first column of xVals is used by your generator
  X <- genRecenteredXVals_polynomial(xVals, xRef = xRef, polyOrder = polyOrder)

  # pull in the same order as in the model object
  b    <- as.matrix(beta[coef_names])
  Vsub <- Vfull[coef_names, coef_names, drop = FALSE]

  fit <- as.numeric(as.matrix(X) %*% b)
  se  <- 1.96 * sqrt(apply(X, 1, calcVariance, Vsub))

  tibble::tibble(
    x = X[, 1] + xRef,
    response = fit,
    lb = fit - se,
    ub = fit + se,
    model = label
  )
}

response_lag_df <- function(mod, pattern, cluster = TRUE, label = "Model") {
  beta <- mod$coefficients
  cn   <- names(beta)

  coef_names <- cn[grepl(pattern, cn)]
  if (!length(coef_names)) stop("No lag coefficients matching '", pattern, "'.")
  Vfull <- if (cluster) mod$clustervcv else mod$vcv
  coef_names <- intersect(coef_names, rownames(Vfull))
  if (!length(coef_names)) stop("No overlap between lag coefficients and VCOV for '", pattern, "'.")

  # keep native order for correct lag sequence
  b    <- as.numeric(beta[coef_names])
  Vsub <- Vfull[coef_names, coef_names, drop = FALSE]
  se   <- 1.96 * sqrt(diag(Vsub))

  tibble::tibble(
    lag = seq.int(0, length(b) - 1),
    response = b,
    lb = b - se,
    ub = b + se,
    model = label
  )
}

# 2) Build tidy data for both models ---------------------------
plotXtemp <- cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

temp_main <- response_curve_poly_df(mainmod, xVals = plotXtemp, label = "Main")
temp_high <- response_curve_poly_df(highresmod, xVals = plotXtemp, label = "High-res")
temp_df   <- dplyr::bind_rows(temp_main, temp_high)

drought_main <- response_lag_df(mainmod,  pattern = "drought", laglength = 3, label = "Main")
drought_high <- response_lag_df(highresmod, pattern = "drought", laglength = 3, label = "High-res")
drought_df   <- dplyr::bind_rows(drought_main, drought_high)

flood_main <- response_lag_df(mainmod,  pattern = "flood", laglength = 3, label = "Main")
flood_high <- response_lag_df(highresmod, pattern = "flood", laglength = 3, label = "High-res")
flood_df   <- dplyr::bind_rows(flood_main, flood_high)

# 3) Plots -----------------------------------------------------
# colors chosen to harmonize with your palette
model_cols  <- c("High-res" = "#C1657C", "Main" = "#2E6FBB")

g_temp <- ggplot(temp_df, aes(x = x, y = response, color = model, fill = model)) +
  geom_hline(yintercept = 0, color = "grey88") +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.25, linewidth = 0, show.legend = FALSE) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = model_cols) +
  scale_fill_manual(values = model_cols) +
  labs(
    x = expression(paste("Mean temperature (", degree, "C)")),
    y = "Prevalence (%)",
    title = NULL,
    color = NULL
  ) +
  coord_cartesian(ylim = c(-30, 10), xlim = c(Tmin, Tmax)) +
  theme_classic(base_size = 9)

# convenient maxima per model (optional vertical ticks)
temp_peaks <- temp_df |>
  dplyr::group_by(model) |>
  dplyr::slice_max(order_by = response, n = 1, with_ties = FALSE)

g_temp <- g_temp +
  geom_vline(data = temp_peaks, aes(xintercept = x, color = model), linetype = "dashed", alpha = 0.6)

g_drought <- ggplot(drought_df, aes(x = lag, y = response, color = model)) +
  geom_hline(yintercept = 0, color = "grey88") +
  geom_point(position = position_dodge(width = 0.35), size = 2) +
  geom_errorbar(aes(ymin = lb, ymax = ub),
                position = position_dodge(width = 0.35), width = 0.15) +
  scale_color_manual(values = model_cols) +
  labs(x = "Drought Lag", y = "Coefficient", title = NULL, color = NULL) +
  coord_cartesian(ylim = c(-5, 8)) +
  theme_classic(base_size = 9)

g_flood <- ggplot(flood_df, aes(x = lag, y = response, color = model)) +
  geom_hline(yintercept = 0, color = "grey88") +
  geom_point(position = position_dodge(width = 0.35), size = 2) +
  geom_errorbar(aes(ymin = lb, ymax = ub),
                position = position_dodge(width = 0.35), width = 0.15) +
  scale_color_manual(values = model_cols) +
  labs(x = "Flood Lag", y = "Coefficient", title = NULL, color = NULL) +
  coord_cartesian(ylim = c(-5, 8)) +
  theme_classic(base_size = 9)

# 4) Assemble + save ------------------------------------------
combined_overlay <- g_temp + g_drought + g_flood +
  patchwork::plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0))

combined_overlay

dir.create(file.path(resdir, "Figures", "Diagnostics", "Main_model"), showWarnings = FALSE)
ggsave(
  file.path(resdir, "Figures", "Diagnostics", "Main_model", "overlay_temp_drought_flood_highres_vs_main.pdf"),
  plot = combined_overlay,
  width = 7,
  height = 2.5
)