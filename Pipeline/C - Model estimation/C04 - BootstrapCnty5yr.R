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
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)
source(A_utils_data_fp)

########################################################################
# Data clean up ----
########################################################################

#### Build country × N-year clustering variable
complete <- complete |>
  mutate(yr_bin = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  group_by(country, yr_bin) |>
  mutate(cntry_yrbin = cur_group_id()) |>
  ungroup()

########################################################################
# Formula ----
########################################################################

# Main specification: clustering on country × N-year
myform = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin"
  )
)

########################################################################
# Bootstrap setup ----
########################################################################

# max_clust <- length(unique(complete$country)) * length(unique(complete$yr_bin))

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

result <- foreach(
  i = 1:(S + 1),
  .packages = c("lfe"),
  .options.snow = opts
) %dopar%
  {
    if (i == 1) { # i = 2
      # Full-sample run
      complete.boot <- complete
      fn <- 'full_sample_cXt2intrXm.rds'
    } else {
      # Resample cluster IDs with replacement
      cl <- sample(clusters, size = length(clusters), replace = TRUE)
      # Gather all rows belonging to each drawn cluster
      boot_idx <- unlist(cluster_rows[as.character(cl)], use.names = FALSE)
      complete.boot <- complete[boot_idx, ]
      fn = paste0('block_bootstrap_', i, '_cXt2intrXm.rds')
    }
    mod <- felm(myform, data = complete.boot)

    # only keep the climate coefficients, not the intervention dummies
    # out <- t(mod$coefficients[1:10])
    # keep the climate coefficients and the intervention dummies
    out <- t(mod$coefficients[1:12])
    colnames(out) <- column_names

    saveRDS(out, file = file.path(bootdir, fn))
  }
close(pb)
stopCluster(clus)

########################################################################
# Save  ----
# Pull in all bootstrap runs and save in one file
# Store full model (no sampling) in the last row of the output
########################################################################

# all bootstraps, including full sample (alphabetically last)
boot.files = list.files(bootdir)

boots <- data.frame(matrix(nrow = S + 1, ncol = 12))
colnames(boots) <- column_names
boots$model <- NA_character_

for (f in seq_along(boot.files)) {
  file <- boot.files[f]
  if (stringr::str_detect(file, "full_sample")) {
    model <- "main"
  } else {
    model <- stringr::str_extract(file, "(?<=block_bootstrap_)\\d+")
  }
  boots[f, 1:12] <- readRDS(file.path(bootdir, file))
  boots$model[f] <- model
}

saveRDS(boots, file = boot_mod_full_fn)
