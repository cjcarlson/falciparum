################################################################################
# This script
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(here, tidyverse)

# source functions from previous script
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_plot_fp)

################################################################################
# Model coefficients ----
################################################################################

# log_msg("Load model coefficients")

all_mods <- boot_mod_full_fn |>
  readr::read_csv(show_col_types = FALSE)

################################################################################
# Prepare temperature response data ----
################################################################################

# log_msg("Prepare the temperature spaghetti data")

conf_level <- 0.90
Tref = 24
Tmin = 10
Tmax = 40
int = 0.1
plotXtemp = cbind(seq(Tmin, Tmax, by = int), seq(Tmin, Tmax, by = int)^2)
xValsT = genRecenteredXVals_polynomial(plotXtemp, Tref, 2)

# collect bootstrap results as a list, then row-bind once
boot_list <- vector("list", nrow(all_mods))

for (mod in seq_len(nrow(all_mods))) {
  sub <- all_mods[mod, ]
  b <- as.matrix(c(sub$temp, sub$temp2))
  boot_response <- as.numeric(as.matrix(xValsT) %*% b)

  boot_list[[mod]] <- data.frame(
    x = xValsT[, 1] + Tref,
    model = sub$model,
    response = boot_response
  )

  if (mod %% 100 == 0) {
    print(paste0("--------- DONE WITH ITERATION ", mod, " of 1000 --------"))
  }
}

plotData <- data.table::rbindlist(boot_list)

########################################################################
#
########################################################################

temps <- plotData |>
  dplyr::group_by(model) |>
  dplyr::filter(response == max(response))

print(temps)

main <- temps |>
  dplyr::filter(model == "main") |>
  dplyr::pull(x) 

temps <- temps |>
  dplyr::filter(model != "main") |>
  dplyr::pull(x) 




print(fivenum(temps))

print(paste0("mean: ", main))
print(paste0("95% CI: ", quantile(temps, 0.025), " - ", quantile(temps, 0.975)))

################################################################################
# End of file ----
################################################################################
