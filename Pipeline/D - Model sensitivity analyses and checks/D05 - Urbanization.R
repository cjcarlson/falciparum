################################################################################
# This script assesses whether urbanization confounds the main empirical
# specification by interacting an urban dummy with all climate variables
# (temperature, drought, flood) to test whether climate-PfPR2 relationships
# differ between urban and rural survey locations.
################################################################################
# Setup  ----
################################################################################

# clear workspace
rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  lfe,
  here,
  knitr,
  broom,
  tidyverse,
  patchwork
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Summary of urban data ----
################################################################################

urban_summary <- urban_summary_fp |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::mutate(
    year = factor(year),
    month = as.factor(month)
  )

################################################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence
# and CRU temperature and precipitation data aggregated to
# the first level of Administrative division.
################################################################################

print("Loading clean data")

complete <- analysis_ready_CRU_adm1_fp |>
  readr::read_rds() |>
  dplyr::left_join(urban_summary, by = join_by("OBJECTID", "year", "month")) |>
  tidyr::drop_na(urban_dummy)

ggplot(data = complete, aes(x = urban_dummy)) +
  geom_histogram(bins = 3)

################################################################################
# Model estimation ----
# Interact the urban dummy with all climate variables
################################################################################

model_int <- lfe::felm(
  PfPR2 ~
    urban_dummy *
    (temp +
      temp2 +
      flood +
      flood.lag +
      flood.lag2 +
      flood.lag3 +
      drought +
      drought.lag +
      drought.lag2 +
      drought.lag3) +
    I(intervention) +
    country:monthyr +
    country:monthyr2 |
    OBJECTID + as.factor(smllrgn):month |
    0 |
    cntry_yrbin,
  data = complete
)

################################################################################
# Results table ----
################################################################################

interaction_table <- model_int |>
  broom::tidy() |>
  dplyr::filter(
    stringr::str_detect(term, "urban") |
      stringr::str_detect(term, "temp") |
      stringr::str_detect(term, "flood") |
      stringr::str_detect(term, "drought")
  ) |>
  dplyr::select(term, estimate, std.error, statistic, p.value) |>
  dplyr::mutate(term = str_replace(term, "avg_urban:", "avg_urban × "))

kable(
  interaction_table,
  caption = "Climate × Urbanization interaction effects on PfPR2",
  digits = c(NA, 3, 3, 2, 3),
  align = c("l", "r", "r", "r", "r")
)

################################################################################
# Polynomial & lagged effects plots ----
################################################################################

plotXtemp <- cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)
coefs <- summary(model_int)$coefficients[2:3]
myrefT <- max(round(-1 * coefs[1] / (2 * coefs[2]), 0), 10)

t <- plotPolynomialResponse(
  mod = model_int,
  patternForPlotVars = "temp",
  xVals = plotXtemp,
  polyOrder = 2,
  cluster = TRUE,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "Climate × Urbanization interaction effects on PfPR2",
  yLim = c(-30, 10),
  showYTitle = TRUE,
  plotmax_x = 4,
  plotmax_y = 6,
  max_x_size = 4,
) +
  theme(plot.title = element_text(size = 10))

d <- plotLinearLags_urban(
  mod = model_int,
  patternForPlotVars = "drought",
  cluster = TRUE,
  laglength = 3,
  xLab = "Drought (month lags)",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-5, 8)
)

f <- plotLinearLags_urban(
  mod = model_int,
  patternForPlotVars = "flood",
  cluster = TRUE,
  laglength = 3,
  xLab = "Flood (month lags)",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-5, 8)
)

combined_plot <- t +
  d +
  f +
  plot_layout(ncol = 3, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
  )

################################################################################
# Save final figure ----
################################################################################

ggplot2::ggsave(
  filename = paste0("ED_Figure_urban_sensitivity.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = combined_plot,
  width = 7,
  height = 2.5,
  units = "in"
)

################################################################################
# End of file ----
################################################################################
