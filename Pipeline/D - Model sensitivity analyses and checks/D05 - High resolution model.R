################################################################################
# This script re-estimates the main empirical specification linking PfPR2 to 
# drought, flood, and temperature using high-resolution grid-level CRU data as a 
# robustness check.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  lfe,
  zoo,
  here,
  reshape,
  cowplot,
  multcomp,
  stargazer,
  tidyverse,
  lubridate,
  patchwork
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence and CRU 
# temperature and precipitation data aggregated to the first level of 
# Administrative division.
################################################################################

print("Loading data")

complete <- readr::read_rds(analysis_ready_grid_fp)

################################################################################
# Estimation ----
################################################################################

highresmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(highresmod$coefficients)
vcov = as.data.frame(highresmod$clustervcv)

saveRDS(coeffs, file = grid_mod_beta_fn)
saveRDS(vcov, file = grid_mod_vcov_fn)

#######################################################################
# Table ----
################################################################################

mynote = "High Resolution Model: Country-specific quad. trends with intervention FE and country by month FE."

stargazer(
  highresmod,
  title = "PfPR2 response to daily avg. temperature",
  align = TRUE,
  keep = c("temp", "flood", "drought", "intervention"),
  out = file.path(table_main_dir, "main_specification_adm1_grid.tex"),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  star.cutoffs = table_star_cutoffs
)

################################################################################
# Main specification model (ADM1) ----
# Load the main model for comparison and plotting
################################################################################

mainmod = readRDS(main_mod_obj_fn)

################################################################################
# Temperature plot data ----
################################################################################

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

coefs = summary(highresmod)$coefficients[1:2]

# plot relative to max of quadratic function
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) 

beta <- highresmod$coefficients
vars <- rownames(beta)
patternForPlotVars <- "temp"
plotVars <- vars[grepl(patternForPlotVars, vars)]

################################################################################
# Temperature plot ----
# (Note: analogous to Fig 2A but with analytically derived confidence intervals
# in place of bootstrap runs shown in Fig 2A)
################################################################################

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

################################################################################
# Drought plot ----
################################################################################

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

################################################################################
# Flood plot ----
################################################################################

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

################################################################################
# Combine plots ----
################################################################################

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

################################################################################
# Save plot ----
################################################################################

ggsave(
  filename = "Supp_Figure_temp_rain_adm1_and_grid.jpg",
  # path = figure_grid_dir,
  path = here::here("Results", "Figures"),
  plot = combined_plot1,
  width = 7,
  height = 2.5,
  dpi = 300
)

################################################################################
# End of file ----
################################################################################
