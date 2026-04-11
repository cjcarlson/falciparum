################################################################################
# This script estimates the main empirical specification linking PfPR2 to
# drought, flood, and temperature via sampling the variance-covariance matrix 
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  lfe,
  here,
  zoo,
  MASS,
  future,
  tidyverse,
  lubridate,
  data.table,
  future.apply
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

# Set number of vcov samples
S = 1000

# Set seed for reproducible output
set.seed(11235)

################################################################################
# Set up logging ----
################################################################################

log_file_path <- file.path(logs_dir, "C03_vcov_sample.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script `C03 - VCOV sampling.R`")

################################################################################
# Load data ----
################################################################################

log_msg("Loading analysis ready data")

complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

column_names <- c(
  "temp",
  "temp2",
  colnames(complete)[grep("flood", colnames(complete))],
  colnames(complete)[grep("drought", colnames(complete))],
  "I(intervention)1",
  "I(intervention)2"
)

################################################################################
# Load model and sample from vcov ----
# Sampling directly from vcov instead of bootstraping
################################################################################

model = readRDS(main_mod_obj_fn)

vcov <- model$clustervcv

beta_hat <- coef(model)[1:12]
V_sub <- vcov[1:12, 1:12]

vcov_draw <- MASS::mvrnorm(n = S, mu = beta_hat, Sigma = V_sub)
colnames(vcov_draw) <- column_names

coeffs_complete <- as.data.frame(rbind(
  setNames(as.data.frame(t(beta_hat)), column_names),
  as.data.frame(vcov_draw)
))
coeffs_complete$model <- c("main", as.character(2:(S + 1)))
coeffs_complete$n <- nrow(complete)

readr::write_csv(coeffs_complete, file = vcov_sample_mod_full_fn)

log_msg(
  "Script `C03 - VCOV sampling.R` completed successfully"
)

################################################################################
# End of file ----
################################################################################
