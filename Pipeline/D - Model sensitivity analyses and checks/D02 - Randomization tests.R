################################################################################
# This script conducts temporal randomization test as a placebo check on the 
# main regression specification.
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
  zoo,
  here,
  tidyverse,
  lubridate,
  data.table,
  doParallel
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Parameters ----
################################################################################

# Which variables do you want to store?
pattern <- c("temp", "flood", "drought")

# Run simulation S times
S <- 1000

# Set number of cores to parallelize over:
# n_cores <- 10
n_cores <- future::availableCores()

set.seed(7812)

################################################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence and CRU 
# temperature and precipitation data aggregated to # the first level of 
# Administrative division.
################################################################################

print("Loading clean data")

complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

################################################################################
# Scramble ----
# Randomly reassign weather within each panel unit
################################################################################

# drop if there is only one observation per ADM1 (these are dropped in regression anyway)
scram <- complete |>
  dplyr::group_by(OBJECTID) |>
  dplyr::mutate(length = length(!is.na(PfPR2))) |>
  dplyr::filter(length > 1)

################################################################################
# Compute cluster ----
################################################################################

cl <- parallel::makeCluster(n_cores)
doSNOW::registerDoSNOW(cl)

# pb <- txtProgressBar(min = 0, max = S, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)

################################################################################
# Process in parallel ----
################################################################################

placebo_out = foreach(
  i = 1:S,
  .combine = rbind,
  .packages = c("dplyr", "lfe")
  # ,
  # .options.snow = opts
) %dopar%
  {
    # i = 1
    # create scrambled climate
    scramS <- scram |>
      dplyr::group_by(OBJECTID) |>
      dplyr::mutate(
        temp = sample(temp),
        drought = sample(drought),
        flood = sample(flood)
      )

    # obtain lags and squared term of scrambled climate
    scramS <- scramS |>
      dplyr::mutate(
        temp2 = temp^2,
        flood.lag = dplyr::lag(flood, order_by = monthyr),
        flood.lag2 = dplyr::lag(flood, order_by = monthyr, n = 2),
        flood.lag3 = dplyr::lag(flood, order_by = monthyr, n = 3),
        drought.lag = dplyr::lag(drought, order_by = monthyr),
        drought.lag2 = dplyr::lag(drought, order_by = monthyr, n = 2),
        drought.lag3 = dplyr::lag(drought, order_by = monthyr, n = 3)
      )

    # run regression
    mod <- lfe::felm(data = scramS, formula = cXt2intrXm)

    # store coefficients, their p values, and the result of an F test on the effect at 35C
    Xvars <- rownames(mod$coefficients)[grepl(
      paste(pattern, collapse = "|"),
      x = rownames(mod$coefficients)
    )]
    Xcoefs <- as.matrix(mod$coefficients[rownames(mod$coefficients) %in% Xvars])
    Xpvals <- as.matrix(summary(mod)$coefficients[
      rownames(mod$coefficients) %in% Xvars,
      4
    ])

    out <- cbind(t(Xcoefs), t(Xpvals))

    return(out)
  }

################################################################################
# Close cluster ----
################################################################################

# close(pb)
parallel::stopCluster(cl)

################################################################################
# Data cleaning ----
################################################################################

placebo_out <- data.frame(placebo_out)

# get column names right
mod <- lfe::felm(data = complete, formula = cXt2intrXm)

Xvars <- rownames(mod$coefficients)[grepl(
  paste(pattern, collapse = "|"),
  x = rownames(mod$coefficients)
)]

pnames <- list()

for (x in Xvars) {
  pnames[[x]] = paste0(x, "_p")
}

colnames(placebo_out) = c(Xvars, unlist(pnames))

################################################################################
# Save placebo data ----
################################################################################

# Save
saveRDS(placebo_out, file = scramble_time_fp)

print(
  "Saved coefficients and p-values from randomization test with scrambled climate data"
)

################################################################################
# Placebo data analysis ----
# Show p-values from randomization versus main model
################################################################################

placebo <- readRDS(scramble_time_fp)

# main model
mainmod <- felm(data = complete, formula = cXt2intrXm)

Xvars <- rownames(mainmod$coefficients)[grepl(
  paste(pattern, collapse = "|"),
  x = rownames(mainmod$coefficients)
)]

Xcoefs <- as.matrix(mainmod$coefficients[
  rownames(mainmod$coefficients) %in% Xvars
])

Xpvals <- as.matrix(summary(mainmod)$coefficients[
  rownames(mainmod$coefficients) %in% Xvars,
  4
])

main <- as.data.frame(cbind(t(Xcoefs), t(Xpvals)))
pnames <- list()

for (x in Xvars) {
  pnames[[x]] <- paste0(x, "_p")
}

colnames(main) <- c(Xvars, unlist(pnames))
main$vline <- 1
vline <- melt(data.table(main), id.vars = "vline")
vline = vline = vline[, 2:3]
colnames(vline) <- c("variable", "value_main")

# matrix of graphs: histogram of coefficients from placebo, compared to coef in true (vertical line in red)
placebo <- placebo |> mutate(sim = row_number())
toplot <- melt(data.table(placebo), id.vars = "sim")
toplot <- toplot |> mutate(pval = (grepl("_p", variable)))

# merge in main mod
toplot <- toplot |> left_join(vline, by = c("variable"))

################################################################################
# Plots ----
################################################################################

p <- ggplot(
  data = toplot[toplot$pval == FALSE & toplot$variable != "temp2", ],
  aes(x = value)
) +
  geom_histogram() +
  geom_vline(aes(xintercept = value_main), colour = "red") +
  facet_wrap(~variable) +
  theme_bw()
p

ggsave(
  filename = "coefficients_time_randomize.jpg",
  path = figure_rand_dir,
  plot = p,
  width = 7,
  height = 9
)

p2 <- ggplot(
  data = toplot[toplot$pval == FALSE & toplot$variable == "temp2", ],
  aes(x = value)
) +
  geom_histogram() +
  geom_vline(aes(xintercept = value_main), colour = "red") +
  theme_bw()
p2

ggsave(
  filename = "coefficients_time_randomiz_TEMP2.jpg",
  path = figure_rand_dir,
  plot = p2,
  width = 7,
  height = 9
)

# matrix of graphs: histogram of pvals from placebo, compared to pval in true (vertical line in red)
p3 <- ggplot(data = toplot[toplot$pval == TRUE, ], aes(x = value)) +
  geom_histogram() +
  geom_vline(aes(xintercept = value_main), colour = "red") +
  facet_wrap(~variable) +
  theme_bw()
p3

ggsave(
  filename = "pVals_time_randomize.jpg",
  path = figure_rand_dir,
  plot = p3,
  width = 7,
  height = 9
)

################################################################################
# End of file ----
################################################################################
