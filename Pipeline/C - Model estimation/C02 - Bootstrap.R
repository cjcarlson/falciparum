############################################################
# This script estimates the main empirical specification linking
# PfPR2 to drought, flood, and temperature via block bootstrap.
#
# CLUSTERING / BOOTSTRAP BLOCK: Resampling is done at the
# country × N-year level (set yr_bin_size below), matching
# the clustering used for analytical standard errors.
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
  doSNOW,
  lfe,
  tidyverse,
  zoo,
  lubridate,
  data.table,
  parallel,
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

############################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence 
# and CRU temperature and precipitation data aggregated to 
# the first level of Administrative division.
############################################################

print("Loading clean data")
complete <- readr::read_rds(replication_fp) 

########################################################################
# Cluster setup ----
########################################################################

print("Preparing the compute cluster")
flush.console()
# Block bootstrap by country × N-year clusters:
clusters <- unique(complete$cntry_yrbin)

# Pre-build a lookup: for each cluster ID, store the row indices
# (done once up front so each bootstrap draw is a fast index lookup)
cluster_rows <- split(seq_len(nrow(complete)), complete$cntry_yrbin)

# Set number of bootstrap simulations.
S = 1000

# Set seed for reproducible output
set.seed(11235)

# Make compute cluster
n_cores = 10 # detectCores()
clus <- makeCluster(n_cores)
registerDoSNOW(clus)

# Make progress bar
pb <- txtProgressBar(max = S, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Define important column names to save
column_names <- c(
  "temp",
  "temp2",
  colnames(complete)[grep("flood", colnames(complete))],
  colnames(complete)[grep("drought", colnames(complete))],
  "I(intervention)1",
  "I(intervention)2"
)

########################################################################
# Bootstrap estimation ----
# Sampling by country × N-year cluster
########################################################################

print("Begin the bootstrap models")
result <- foreach(
  i = 1:(S + 1),
  .packages = c("lfe"),
  .options.snow = opts
) %dopar%
  {
    if (i == 1) {
      complete.boot <- complete
      fn <- 'full_sample_cXt2intrXm.rds'
      model <- "main"
    } else {
      cl <- sample(clusters, size = length(clusters), replace = TRUE)
      boot_idx <- unlist(cluster_rows[as.character(cl)], use.names = FALSE)
      complete.boot <- complete[boot_idx, ]
      fn <- paste0('block_bootstrap_', i, '_cXt2intrXm.rds')
      model <- as.character(i)
    }
    mod <- lfe::felm(formula = cXt2intrXm, data = complete.boot)

    out <- t(mod$coefficients[1:12])
    colnames(out) <- column_names

    saveRDS(out, file = file.path(model_boot_dir, fn))

    list(coefs = out, model = model)
  }
close(pb)
stopCluster(clus)

########################################################################
# Save  ----
# Pull in all bootstrap runs and full spec to save in one file
########################################################################

# Unpack into a data.frame
boots <- do.call(rbind, lapply(result, function(x) {
  df <- as.data.frame(x$coefs)
  df$model <- x$model
  df
}))

saveRDS(boots, file = boot_mod_full_fn)

