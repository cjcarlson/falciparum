################################################################################
# This script estimates the main empirical specification linking PfPR2 to 
# drought, flood, and temperature.
#
# CLUSTERING: Standard errors are clustered at the
# country × N-year level (set yr_bin_size in config).
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(lfe, here, tidyverse, stargazer)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Set up logging ----
################################################################################

log_file_path <- file.path(logs_dir, "C01_main_spec.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script C01 - MainSpec")

################################################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence
# and CRU temperature and precipitation data aggregated to
# the first level of Administrative division.
################################################################################

log_msg("Loading analysis ready data")

complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

################################################################################
# Estimation ----
# Formula cXt2intrXm is loaded from configuration file
################################################################################

log_msg("Begin modeling")

mainmod = lfe::felm(data = complete, formula = cXt2intrXm)

coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)

log_msg("Save model coefficients and vcov")

# Save results
saveRDS(mainmod, file = main_mod_obj_fn)
saveRDS(coeffs, file = main_mod_beta_fn)
saveRDS(vcov, file = main_mod_vcov_fn)

################################################################################
# Table ----
################################################################################

log_msg("Save table results")

mynote = paste0(
  "Country-specific quad. trends with intervention FE and country by month FE. ",
  "Standard errors clustered at ",
  gsub("_", " ", clust_label),
  " level."
)

stargazer::stargazer(
  mainmod,
  title = "PfPR2 response to daily avg. temperature",
  align = TRUE,
  keep = c("temp", "flood", "drought", "inter"),
  # out = file.path(table_main_dir, "cXt2intrXm.tex"),
  out = here::here("Results", "Tables", "cXt2intrXm.tex"),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  digits = 2,
  star.cutoffs = table_star_cutoffs
)

################################################################################
# Plot ----
# Note: analogous to Fig 2A but with analytically derived confidence
# intervals in place of bootstrap runs.
################################################################################

log_msg("Plot temperature response")

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

# plot relative to max of quadratic function
coefs = summary(mainmod)$coefficients[1:2]

#reference temperature - curve gets recentered to 0 here
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
  showYTitle = T,
  ci_level = 0.95
)

log_msg("Save temperature response plot")

ggplot2::ggsave(
  filename = "temp_response_cXt2intrXm.pdf",
  path = figure_main_dir,
  plot = fig,
  width = 7,
  height = 7
)

log_msg("Script `C01 - MainSpec.R` completed successfully")

################################################################################
# End of file ----
################################################################################
