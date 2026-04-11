################################################################################
# This script estimates the main empirical specification linking PfPR2 to 
# drought, flood, and temperature via block bootstrap.
#
# CLUSTERING / BOOTSTRAP BLOCK: Resampling is done at the country × N-year level 
# (set yr_bin_size in config), matching the clustering used for analytical 
# standard errors.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  here,
  doSNOW,
  lfe,
  tidyverse,
  zoo,
  lubridate,
  data.table,
  parallel,
  MASS
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

# Set number of bootstrap simulations.
S = 1000

# Set seed for reproducible output
set.seed(11235)
# set.seed(42)

################################################################################
# Set up logging ----
################################################################################

log_file_path <- file.path(logs_dir, "C02_bootstrap.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script `C02 - Bootstrap.R`")

################################################################################
# Load data ----
################################################################################

log_msg("Loading analysis ready data")

complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

################################################################################
# Cluster setup ----
################################################################################

log_msg("Preparing the compute cluster")

# n_cores = min(10, future::detectCores())
n_cores <- future::availableCores()

# Make compute cluster
clus <- parallel::makeCluster(n_cores)
doSNOW::registerDoSNOW(clus)

# Make progress bar
pb <- txtProgressBar(max = S, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

################################################################################
# Bootstrap estimation ----
# Sampling by country × N-year cluster
################################################################################

# Block bootstrap by country × N-year clusters:
clusters <- unique(complete$cntry_yrbin)

# Pre-build a lookup: for each cluster ID, store the row indices
cluster_rows <- split(seq_len(nrow(complete)), complete$cntry_yrbin)

# Define important column names to save
column_names <- c(
  "temp",
  "temp2",
  colnames(complete)[grep("flood", colnames(complete))],
  colnames(complete)[grep("drought", colnames(complete))],
  "I(intervention)1",
  "I(intervention)2"
)

log_msg("Begin the bootstrap models")

result <- foreach(
  i = 1:(S + 1),
  .packages = c("lfe"),
  .options.snow = opts
) %dopar%
  {
    if (i == 1) {
      complete.boot <- complete
      model <- "main"
    } else {
      cl <- sample(clusters, size = length(clusters), replace = TRUE)
      boot_idx <- unlist(cluster_rows[cl], use.names = FALSE)
      complete.boot <- complete[boot_idx, ]
      model <- as.character(i)
    }
    mod <- lfe::felm(formula = cXt2intrXm, data = complete.boot)

    out <- t(mod$coefficients[1:12])
    colnames(out) <- column_names

    list(coefs = out, model = model, n = nrow(complete.boot))
  }
close(pb)
stopCluster(clus)

log_msg("Finish the bootstrap models")

################################################################################
# Save coeffs ----
# Pull in all bootstrap runs and full spec to save in one file
################################################################################

log_msg("Consolidating bootstrap coefficients and saving file")

# Unpack into a data.frame
boots <- do.call(
  rbind,
  lapply(
    result,
    function(x) {
      df <- as.data.frame(x$coefs)
      df$model <- x$model
      df$n <- x$n
      df
    }
  )
)

# saveRDS(boots, file = boot_mod_full_fn)

readr::write_csv(boots, file = boot_mod_full_fn)

log_msg("Script `C02 - Bootstrap.R` completed successfully")

################################################################################
# End of file ----
################################################################################
