############################################################
# This script estimates the main empirical specification linking
# PfPR2 to drought, flood, and temperature.
#
# CLUSTERING: Standard errors are clustered at the
# country × N-year level (set yr_bin_size in config).
############################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
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
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)
source(A_utils_data_fp)

############################################################
# Plotting toggles ----
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tref = 24 #reference temperature - curve gets recentered to 0 here
Tmin = 10 #min T for x axis
Tmax = 40 #max T for x axis

########################################################################
# Data clean up ----
# Build country × N-year clustering variable
########################################################################

complete <- complete |>
  dplyr::mutate(yr_bin = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin) |>
  dplyr::arrange(OBJECTID, monthyr) |> 
  dplyr::mutate(cntry_yrbin = dplyr::cur_group_id()) |>
  dplyr::ungroup()

########################################################################
# Estimation ----
# Formula: same fixed effects as original, but clustering on country × N-year
########################################################################

cXt2intrXm = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + country:monthyr + country:monthyr2",
    " | ",
    "OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin"
  )
)

# Model estimation
mainmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)

#Save results
saveRDS(coeffs, file = main_mod_beta_fn)
saveRDS(vcov, file = main_mod_vcov_fn)

########################################################################
# Table ----
# Formula: same fixed effects as original, but clustering on country × N-year
########################################################################

# Stargazer output
mynote = paste0(
  "Country-specific quad. trends with intervention FE and country by month FE. ",
  "Standard errors clustered at ",
  gsub("_", " ", clust_label),
  " level."
)

stargazer(
  mainmod,
  title = "PfPR2 response to daily avg. temperature",
  align = TRUE,
  keep = c("temp", "flood", "drought", "inter"),
  out = file.path(table_main_dir, "cXt2intrXm.tex"),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  digits = 2,
  star.cutoffs = c(0.05, 0.01, 0.001)
)

########################################################################
# Plot ----
# Note: analogous to Fig 2A but with analytically derived confidence intervals
# in place of bootstrap runs shown in Fig 2A
########################################################################

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

# plot relative to max of quadratic function
coefs = summary(mainmod)$coefficients[1:2]
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)

fig = plotPolynomialResponse(
  mainmod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = paste0("Main spec: ", clust_label),
  yLim = c(-30, 5),
  showYTitle = T
)

ggplot2::ggsave(
  filename = "temp_response_cXt2intrXm.pdf",
  path = figure_main_dir,
  plot = fig,
  width = 7,
  height = 7,
  create.dir = TRUE
)
