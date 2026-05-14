############################################################
# This script makes all four panels of Figure S1.
############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  lfe,
  zoo,
  here,
  reshape,
  tidyverse,
  lubridate,
  patchwork,
  cowplot
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

#######################################################################
# S1A: Theoretical ----
#######################################################################

print("Loading clean data")

complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

data <- intermediate_CRU_adm1_fp |>
  arrow::read_feather() |> 
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID), year = as.numeric(year)) |> 
  # readr::read_csv() |>
  dplyr::mutate(year = factor(year)) |>
  dplyr::left_join(complete)

# Generate the R0 curves:
data$predR0 <- sapply(data$temp, r0t)

g1 <- ggplot(data = data, aes(x = temp, y = predR0)) +
  geom_vline(
    xintercept = 25.56,
    color = 'dark grey',
    lwd = 1,
    linetype = 'longdash'
  ) +
  geom_line(color = "black", lwd = 0.7) +
  xlim(c(15, 35)) +
  labs(
    x = expression(paste("Temperature (", degree, "C)")),
    y = expression('R'[0] * ' predicted')
  ) +
  theme_bw()


#######################################################################
# S1B: GAMS on raw data ----
#######################################################################

g2 <- ggplot(data = data, aes(x = temp, y = predR0)) +
  geom_vline(
    xintercept = 25.56,
    color = 'dark grey',
    lwd = 0.7,
    linetype = 'longdash'
  ) +
  geom_smooth(aes(y = PfPR2), lwd = 1, color = "#C1657C", fill = 'light grey') +
  xlim(c(15, 35)) +
  labs(
    x = expression(paste("Temperature (", degree, "C)")),
    y = "Prevalence (%, raw data)"
  ) +
  theme_bw()

sm <- ggplot_build(g2)$data[[2]]

optg2 <- sm$x[sm$y == max(sm$y)]

g2 <- ggplot(data = data, aes(x = temp, y = predR0)) +
  geom_vline(
    xintercept = 25.56,
    color = 'dark grey',
    lwd = 0.7,
    linetype = 'longdash'
  ) +
  geom_vline(
    xintercept = optg2,
    color = "#C1657C",
    lwd = 0.7,
    linetype = 'longdash'
  ) +
  geom_smooth(aes(y = PfPR2), lwd = 1, color = "#C1657C", fill = 'light grey') +
  xlim(c(15, 35)) +
  labs(
    x = expression(paste("Temperature (", degree, "C)")),
    y = "Prevalence (%, raw data)"
  ) +
  theme_bw()

#######################################################################
# S1C: Econometric model + uncertainty ----
#######################################################################
# Formula & estimation

mainmod <- felm(data = complete, formula = cXt2intrXm)
beta <- mainmod$coefficients
vars <- rownames(beta)
plotVars <- vars[grepl(pattern = "temp", x = vars)]

# rm(complete, countrydf, data, data_iso, data.reset)

# plot setup
Tref <- 24
int <- 0.1
plotXtemp <- cbind(seq(Tmin, Tmax, by = int), seq(Tmin, Tmax, by = int)^2)
myrefT <- max(round(-1 * beta[1] / (2 * beta[2]), digits = 0), 10) # plot relative to max of quadratic function
optg3 <- -1 * beta[1] / (2 * beta[2])
xValsT <- genRecenteredXVals_polynomial(plotXtemp, myrefT, 2, NA)
vcov <- getVcov(mainmod$clustervcv, plotVars)
b <- as.matrix(beta[rownames(beta) %in% plotVars])

response <- as.matrix(xValsT) %*% b #Prediction
length <- 1.96 * sqrt(apply(X = xValsT, FUN = calcVariance, MARGIN = 1, vcov))
lb <- response - length
ub <- response + length

# add back in the reference temperature so it's centered at xRef
plotData <- data.frame(
  x = xValsT[, 1] + myrefT,
  response = response,
  lb = lb,
  ub = ub
)

g3 <- ggplot(data = plotData) +
  #geom_ribbon(aes(x, ymin = lb, ymax = ub), alpha = 0.4, fill = 'light grey') +
  geom_vline(
    xintercept = 25.56,
    color = 'dark grey',
    lwd = 0.7,
    linetype = 'longdash'
  ) +
  geom_vline(
    xintercept = optg3,
    color = "#C1657C",
    lwd = 0.7,
    linetype = 'longdash'
  ) +
  geom_line(aes(x = x, y = response), color = "#C1657C", lwd = 1) +
  xlim(c(15, 35)) +
  ylim(c(-9, 0.2)) +
  labs(
    x = expression(paste("Temperature (", degree, "C)")),
    y = "Prevalence (%, modeled)"
  ) +
  theme_bw()
g3

#######################################################################
# S1D: Distribution of peak temperatures in econometric model ----
#######################################################################

# upload bootstraps
boots <- coeffs_fn |>
  readr::read_csv() |> 
  dplyr::mutate(peakT = optT(temp, temp2))

main <-  boots|>
  dplyr::filter(model == "main")

# opt = -main$temp / (2 * main$temp2)

boots <- boots |>
  dplyr::filter(model != "main")

meanpeak <- mean(boots$peakT)

quantile(boots$peakT, 0.025)
quantile(boots$peakT, 0.975)

g4 <- ggplot(data = boots) +
  geom_histogram(
    aes(x = peakT, y = ..density..),
    color = "light grey",
    fill = "light grey",
    show.legend = FALSE,
    bins = 50
  ) +
  labs(
    x = expression(paste("Optimum temperature (", degree, "C)")),
    y = "Density"
  ) +
  geom_vline(
    xintercept = meanpeak,
    color = "#C1657C",
    lwd = 0.7,
    linetype = 'longdash'
  ) +
  theme_bw() +
  scale_x_continuous(limits = c(20, 30))
g4


#######################################################################
# Combine and save ----
#######################################################################

p <- plot_grid(
  g1,
  g2,
  g3,
  g4,
  nrow = 2,
  label_size = 12,
  labels = c('A', 'B', 'C', 'D')
)
p

ggsave(
  filename = 'Supp_Figure_thermal_curve_and_data.jpg',
  path = here::here("Results", "Figures"),
  plot = p,
  height = 10,
  width = 10
)

################################################################################
# End of file ----
################################################################################
