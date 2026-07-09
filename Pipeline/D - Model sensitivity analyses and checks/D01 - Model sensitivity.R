################################################################################
# This script conducts a variety of model sensitivity checks on the main
# empirical specification linking PfPR2 to drought, flood, and temperature for
# the extended data and supplementary materials.
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
  lfe,
  stargazer,
  tidyverse,
  zoo,
  cowplot,
  multcomp,
  patchwork
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Plotting toggles ----
################################################################################

Tref <- 25 # reference temperature - curve gets recentered to 0 here

################################################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence and CRU
# temperature and precipitation data aggregated to the first level of
# Administrative division.
################################################################################

print("Loading analysis ready data")

complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

################################################################################
# Temp lags/leads - data prep ----
# Lags and leads of temperature: Dynamic effects
################################################################################

climate_data <- intermediate_CRU_adm1_fp |>
  arrow::read_feather() |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID), year = as.numeric(year)) |>
  tidyr::unite("monthyr", month:year, sep = ' ', remove = FALSE) |>
  dplyr::mutate(
    monthyr = as.Date(zoo::as.yearmon(monthyr)),
    monthyr = as.numeric(ymd(monthyr) - ymd("1900-01-01")),
    yearnum = as.numeric(year),
    year = as.factor(year)
  ) |>
  dplyr::arrange(OBJECTID, monthyr)

templags <- climate_data %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::mutate(
    temp.lag = lag(temp, order_by = monthyr),
    temp.lag2 = lag(temp, order_by = monthyr, n = 2),
    temp.lag3 = lag(temp, order_by = monthyr, n = 3),
    temp2.lag = lag(temp2, order_by = monthyr),
    temp2.lag2 = lag(temp2, order_by = monthyr, n = 2),
    temp2.lag3 = lag(temp2, order_by = monthyr, n = 3),
    temp.lead = lead(temp, order_by = monthyr),
    temp.lead2 = lead(temp, order_by = monthyr, n = 2),
    temp.lead3 = lead(temp, order_by = monthyr, n = 3),
    temp2.lead = lead(temp2, order_by = monthyr),
    temp2.lead2 = lead(temp2, order_by = monthyr, n = 2),
    temp2.lead3 = lead(temp2, order_by = monthyr, n = 3)
  )

# merge back into main dataset
tokeep <- c("OBJECTID", "monthyr", "month", "year")
templags <- templags |>
  dplyr::select(all_of(tokeep), contains("lag"), contains("lead"))

complete <- complete |>
  dplyr::left_join(templags, by = c("OBJECTID", "monthyr", "month", "year"))

complete$month <- as.factor(complete$month)

################################################################################
# Temp lags/leads - formulas ----
################################################################################

# Formulas
myforms <- list(
  cont = make_lag_form(n_lags = 0, n_leads = 0),
  lg1 = make_lag_form(n_lags = 1, n_leads = 0),
  lg2 = make_lag_form(n_lags = 2, n_leads = 0),
  lg3 = make_lag_form(n_lags = 3, n_leads = 0),
  ld1lg1 = make_lag_form(n_lags = 1, n_leads = 1),
  ld2lg2 = make_lag_form(n_lags = 2, n_leads = 2),
  ld3lg3 = make_lag_form(n_lags = 3, n_leads = 3),
  ld1lg3 = make_lag_form(n_lags = 3, n_leads = 1)
)

mycollabs <- c(
  "cont",
  "lg1",
  "lg2",
  "lg3",
  "ld1lg1",
  "ld2lg2",
  "ld3lg3",
  "ld1lg3"
)

################################################################################
# Temp lags/leads - estimation ----
################################################################################

# Run all models
modellist <- list()
i <- 0
for (m in myforms) {
  i <- i + 1
  modellist[[i]] <- lfe::felm(data = complete, formula = m)
}

################################################################################
# Temp lags/leads - plot ----
################################################################################

# Plot main model with SEs
plotXtemp <- cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)
c <- plotPolynomialResponse(
  modellist[[1]],
  "temp",
  plotXtemp,
  polyOrder = 2,
  plotmax = T,
  cluster = T,
  xRef = Tref,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "contemp.",
  yLim = c(-30, 5),
  showYTitle = T,
  max_x_size = 6,
  plotmax_x = 3,
  plotmax_y = 5,
  axis_size = 14,
  axis_title_size = 16,
  title_size = 18
)

# Plot with one lag
p1 <- plotPolynomialResponse(
  modellist[[2]],
  "temp",
  plotXtemp,
  polyOrder = 2,
  lag = 1,
  plotmax = F,
  cluster = T,
  xRef = Tref,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "cumulative (1 mo.)",
  yLim = c(-30, 5),
  showYTitle = F,
  axis_size = 14,
  axis_title_size = 16,
  title_size = 18
) +
  geom_vline(
    mapping = aes(xintercept = 25),
    linetype = "solid",
    colour = "grey39"
  ) +
  annotate(
    geom = "text",
    x = 28,
    y = 5,
    label = paste0("25 C"),
    color = "grey39",
    size = 6
  )

# Plot with two lags
p2 <- plotPolynomialResponse(
  modellist[[3]],
  "temp",
  plotXtemp,
  polyOrder = 2,
  lag = 2,
  plotmax = F,
  cluster = T,
  xRef = Tref,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "cumulative (2 mo.)",
  yLim = c(-30, 5),
  showYTitle = F,
  axis_size = 14,
  axis_title_size = 16,
  title_size = 18
) +
  geom_vline(
    mapping = aes(xintercept = 25),
    linetype = "solid",
    colour = "grey39"
  ) +
  annotate(
    geom = "text",
    x = 28,
    y = 5,
    label = paste0("25 C"),
    color = "grey39",
    size = 6
  )

# Plot with three lags
p3 <- plotPolynomialResponse(
  modellist[[4]],
  "temp",
  plotXtemp,
  polyOrder = 2,
  lag = 3,
  plotmax = F,
  cluster = F,
  xRef = Tref,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "cumulative (3 mo.)",
  yLim = c(-30, 5),
  showYTitle = F,
  axis_size = 14,
  axis_title_size = 16,
  title_size = 18
) +
  geom_vline(
    mapping = aes(xintercept = 25),
    linetype = "solid",
    colour = "grey39"
  ) +
  annotate(
    geom = "text",
    x = 28,
    y = 5,
    label = paste0("25 C"),
    color = "grey39",
    size = 6
  )

p <- (c + p1 + p2 + p3) &
  patchwork::plot_layout(nrow = 1)

ggplot2::ggsave(
  filename = paste0("ED_Figure_templags_cumulative_effects.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = p,
  width = 16,
  height = 4
)

################################################################################
# Drought/flood  ----
# Sensitivity to definitions of drought and flood
################################################################################

# Loop over drought/flood function
dlist <- c(0.01, 0.05, 0.1, 0.15, 0.2)
flist <- c(0.85, 0.9, 0.95)

modellist <- list()
modellabs <- list()
i <- 0
for (dd in dlist) {
  for (ff in flist) {
    i <- i + 1

    # compute drought and flood variables
    dropcols <- grep(
      "flood|drought|ppt_pctile",
      colnames(complete),
      value = TRUE
    )
    newdf <- computePrcpExtremes(
      dfclimate = climate_data,
      dfoutcome = complete[, !(names(complete) %in% dropcols)],
      pctdrought = dd,
      pctflood = ff,
      yearcutoff = NA
    )
    newdf <- newdf %>% dplyr::arrange(OBJECTID, monthyr)
    newdf$month <- as.factor(newdf$month)

    # run regression, store results
    modellist[[i]] <- felm(data = newdf, formula = cXt2intrXm)
    modellabs[[i]] <- paste0("drought:", dd, " flood:", ff)

    print(paste0(
      '----------- Regression run for drought pctile ',
      dd,
      ' and flood pctile ',
      ff,
      ' -----------'
    ))
    rm(newdf)
  }
}

################################################################################
# Drought/flood - temp response  ----
# For each model, plot temperature response
################################################################################

plotXtemp <- cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)
nrowGrid <- 5
ncolGrid <- ceiling(length(modellist) / nrowGrid)
figList <- list()
for (m in 1:length(modellist)) {
  isLeftCol <- ((m - 1) %% ncolGrid) == 0
  isBottomRow <- m > ncolGrid * (nrowGrid - 1)
  coefs <- summary(modellist[[m]])$coefficients[1:2]
  myrefT <- max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)
  figList[[m]] <- plotPolynomialResponse(
    modellist[[m]],
    "temp",
    plotXtemp,
    polyOrder = 2,
    cluster = T,
    xRef = Tref,
    xLab = expression(paste("Mean temperature (", degree, "C)")),
    yLab = "Prevalence (%)",
    title = modellabs[m],
    yLim = c(-30, 5),
    showYTitle = isLeftCol,
    showXTitle = isBottomRow,
    max_x_size = 4,
    plotmax_x = 4,
    plotmax_y = 5,
    axis_size = 10,
    axis_title_size = 12,
    title_size = 18
  ) +
    theme(plot.title = element_text(size = 10))
}

p <- cowplot::plot_grid(
  figList[[1]],
  figList[[2]],
  figList[[3]],
  figList[[4]],
  figList[[5]],
  figList[[6]],
  figList[[7]],
  figList[[8]],
  figList[[9]],
  figList[[10]],
  figList[[11]],
  figList[[12]],
  figList[[13]],
  figList[[14]],
  figList[[15]],
  nrow = 5
)

ggsave(
  filename = paste0(
    "Supp_Figure_temp_responses_drought_flood_sensitivity.",
    fig_file_type
  ),
  path = here::here("Results", "Figures"),
  plot = p,
  width = 7,
  height = 10
)

################################################################################
# Drought/flood - coeffs ----
# For each model, plot drought and flood coeffs
################################################################################

# All drought figures
figList <- list()
for (m in 1:length(modellist)) {
  isLeftCol <- ((m - 1) %% ncolGrid) == 0
  isBottomRow <- m > ncolGrid * (nrowGrid - 1)
  figList[[m]] <- plotLinearLags(
    mod = modellist[[m]],
    patternForPlotVars = "drought",
    cluster = T,
    laglength = 3,
    xLab = "Lag",
    yLab = "Coefficient",
    title = modellabs[[m]],
    yLim = c(-6, 4),
    showYTitle = isLeftCol,
    showXTitle = isBottomRow,
    axis_size = 10,
    axis_title_size = 10,
    title_size = 10
  )
}

p <- cowplot::plot_grid(
  figList[[1]],
  figList[[2]],
  figList[[3]],
  figList[[4]],
  figList[[5]],
  figList[[6]],
  figList[[7]],
  figList[[8]],
  figList[[9]],
  figList[[10]],
  figList[[11]],
  figList[[12]],
  figList[[13]],
  figList[[14]],
  figList[[15]],
  nrow = 5
)

ggsave(
  filename = paste0(
    "Supp_Figure_drought_responses_sensitivity.",
    fig_file_type
  ),
  path = here::here("Results", "Figures"),
  plot = p,
  width = 7,
  height = 10
)

################################################################################
# Flood figures ----
################################################################################

figList <- list()
for (m in 1:length(modellist)) {
  isLeftCol <- ((m - 1) %% ncolGrid) == 0
  isBottomRow <- m > ncolGrid * (nrowGrid - 1)
  figList[[m]] <- plotLinearLags(
    mod = modellist[[m]],
    patternForPlotVars = "flood",
    cluster = T,
    laglength = 3,
    xLab = "Lag",
    yLab = "Coefficient",
    title = modellabs[[m]],
    yLim = c(-4, 4),
    showYTitle = isLeftCol,
    showXTitle = isBottomRow,
    axis_size = 10,
    axis_title_size = 10,
    title_size = 10
  )
}

p <- cowplot::plot_grid(
  figList[[1]],
  figList[[2]],
  figList[[3]],
  figList[[4]],
  figList[[5]],
  figList[[6]],
  figList[[7]],
  figList[[8]],
  figList[[9]],
  figList[[10]],
  figList[[11]],
  figList[[12]],
  figList[[13]],
  figList[[14]],
  figList[[15]],
  nrow = 5
)

ggsave(
  filename = paste0("Supp_Figure_flood_responses_sensitivity.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = p,
  width = 7,
  height = 10
)

################################################################################
# Temperature functional form ----
# estimate polynomial orders up to 5
################################################################################

modellist <- list()
modellist[[1]] <- felm(data = complete, formula = cXt2intrXm)
modellist[[2]] <- felm(
  data = complete,
  formula = as.formula(paste0(
    "PfPR2 ~ temp + temp2 + temp3 +",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | ",
    clustering
  ))
)
modellist[[3]] <- felm(
  data = complete,
  formula = as.formula(paste0(
    "PfPR2 ~ temp + temp2 + temp3 + temp4 +",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | ",
    clustering
  ))
)
modellist[[4]] <- felm(
  data = complete,
  formula = as.formula(paste0(
    "PfPR2 ~ temp + temp2 + temp3 + temp4 + temp5 +",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | ",
    clustering
  ))
)
# plot
plotXtemp <- cbind(
  seq(Tmin, Tmax),
  seq(Tmin, Tmax)^2,
  seq(Tmin, Tmax)^3,
  seq(Tmin, Tmax)^4,
  seq(Tmin, Tmax)^5
)
modellabs <- c("Quadratic", "Cubic", "Quartic", "Quintic")

# get ref T for quadratic model
coefs <- summary(modellist[[1]])$coefficients[1:2]
myrefT <- max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)

nrowGrid <- 2
ncolGrid <- ceiling(length(modellist) / nrowGrid)

figList <- list()
for (m in 1:length(modellist)) {
  isLeftCol <- ((m - 1) %% ncolGrid) == 0
  isBottomRow <- m > ncolGrid * (nrowGrid - 1)
  end <- m + 1
  figList[[m]] <- plotPolynomialResponse(
    mod = modellist[[m]],
    patternForPlotVars = "temp",
    xVals = plotXtemp[, 1:end],
    polyOrder = end,
    plotmax = F,
    cluster = T,
    xRef = myrefT,
    xLab = expression(paste("Mean temperature (", degree, "C)")),
    yLab = "Prevalence (%)",
    title = modellabs[m],
    yLim = c(-30, 5),
    showYTitle = isLeftCol,
    showXTitle = isBottomRow,
    axis_size = 16,
    axis_title_size = 18,
    title_size = 20
  ) +
    geom_vline(
      mapping = aes(xintercept = myrefT),
      linetype = "solid",
      colour = "grey39"
    ) +
    annotate(
      geom = "text",
      x = myrefT + 3,
      y = 5,
      label = paste0(myrefT, " C"),
      color = "grey39",
      size = 6
    )
}

p <- cowplot::plot_grid(
  figList[[1]],
  figList[[2]],
  figList[[3]],
  figList[[4]],
  nrow = 2
)

ggsave(
  filename = paste0("Supp_Figure_temperature_poly_order.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = p,
  width = 10,
  height = 10
)

################################################################################
# Cumulative precipitation ----
################################################################################

# estimate polynomial orders up to 5
modellist <- list()
modellist[[1]] <- felm(
  data = complete,
  formula = as.formula(paste0(
    "PfPR2 ~ temp + temp2 + ppt + ppt2 + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | ",
    clustering
  ))
)
modellist[[2]] <- felm(
  data = complete,
  formula = as.formula(paste0(
    "PfPR2 ~ temp + temp2 + ppt + ppt2 + ppt3 + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | ",
    clustering
  ))
)
modellist[[3]] <- felm(
  data = complete,
  formula = as.formula(paste0(
    "PfPR2 ~ temp + temp2 + ppt + ppt2 + ppt3 + ppt4 + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | ",
    clustering
  ))
)
modellist[[4]] <- felm(
  data = complete,
  formula = as.formula(paste0(
    "PfPR2 ~ temp + temp2 + ppt + ppt2 + ppt3 + ppt4 + ppt5 + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | ",
    clustering
  ))
)
# plot
Tmin <- 0
Tmax <- 600
plotXprcp <- cbind(
  seq(Tmin, Tmax),
  seq(Tmin, Tmax)^2,
  seq(Tmin, Tmax)^3,
  seq(Tmin, Tmax)^4,
  seq(Tmin, Tmax)^5
)
modellabs <- c("Quadratic", "Cubic", "Quartic", "Quintic")

# get ref P
myrefP <- 0

figList <- list()
for (m in 1:length(modellist)) {
  isLeftCol <- ((m - 1) %% ncolGrid) == 0
  isBottomRow <- m > ncolGrid * (nrowGrid - 1)
  end <- m + 1
  figList[[m]] <- plotPolynomialResponse(
    mod = modellist[[m]],
    patternForPlotVars = "ppt",
    xVals = plotXprcp[, 1:end],
    polyOrder = end,
    plotmax = F,
    cluster = T,
    xRef = myrefP,
    fillcolor = "#43A7BA",
    xLab = "Total precipitation (mm)",
    yLab = "Prevalence (%)",
    title = modellabs[m],
    yLim = c(-10, 5),
    showYTitle = isLeftCol,
    showXTitle = isBottomRow,
    axis_size = 16,
    axis_title_size = 18,
    title_size = 20
  )
}

p <- cowplot::plot_grid(
  figList[[1]],
  figList[[2]],
  figList[[3]],
  figList[[4]],
  nrow = 2
)

ggsave(
  filename = paste0("Supp_Figure_precipitation_poly_order.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = p,
  width = 10,
  height = 10
)

################################################################################
# End of file ----
################################################################################
