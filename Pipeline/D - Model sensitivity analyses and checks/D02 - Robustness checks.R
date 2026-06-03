################################################################################
# This script conducts additional robustness checks. This script should be
# incorporated into D01 when a subset of tests are included in the main text
# and/or Supplement.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  here,
  lfe,
  reshape,
  stargazer,
  tidyverse,
  zoo,
  lubridate,
  cowplot,
  multcomp,
  sf
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence and CRU
# temperature and precipitation data aggregated to the first level of
# Administrative division.
################################################################################

print("Loading clean data")
complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

################################################################################
# Diagnostic method ----
################################################################################

complete_dm <- complete |>
  dplyr::mutate(microscopy = simplified_METHOD == "MICROSCOPY") |>
  dplyr::filter(dominant_METHOD != "LAMP")

complete_dm$dominant_METHOD = as.factor(complete_dm$dominant_METHOD)
complete_dm$simplified_METHOD = as.factor(complete_dm$simplified_METHOD)

complete_dm$month = as.factor(complete_dm$month)
complete_dm$year = as.factor(complete_dm$year)

################################################################################
# Control for diagnostic method ----
################################################################################

cXt2intrXmDM = as.formula(
  paste0(
    common,
    " + dominant_METHOD + I(intervention) + ",
    country_time,
    " | OBJECTID  + as.factor(smllrgn):month | 0 | ",
    clustering
  )
)
cXt2intrXmSM = as.formula(
  paste0(
    common,
    " + simplified_METHOD + I(intervention) + ",
    country_time,
    " | OBJECTID  + as.factor(smllrgn):month | 0 | ",
    clustering
  )
)

myforms = c(cXt2intrXm, cXt2intrXmDM, cXt2intrXmSM)

mycollabs = c(
  "main specification", # Main Spec
  "+ diag. method (full set)", # Main Spec with dominant method
  "+ diag. method (small set)" # Main Spec with simplified method
)

modellist = list()
i = 0
for (m in myforms) {
  i = i + 1
  modellist[[i]] = felm(data = complete_dm, formula = m)
}

mynote = "Column specifications: (1) country-specific quad. trends, intervention year FE, GBOD region-by-month FE; (2) same as (1), but with additional controls for diagnostic method: Microscopy, Microcscopy/PCR Confirmed, PCR, RDT, RDT/PCR Confirmed, and RDT/SLIDE Confirmed; (3) same as (2) but using simplified diagnostic method control set: Microscopy, PCR, RDT."

tex <- stargazer(
  modellist,
  title = "Sensitivity to controlling for diagnostic method",
  align = TRUE,
  column.labels = mycollabs,
  covariate.labels = my_covariate_labels,
  dep.var.labels = "$Pf$PR$_{2-10}$",
  keep = c("temp", "flood", "drought", "int", "METHOD"),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  digits = 2,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  star.cutoffs = table_star_cutoffs
)

writeLines(tex, here::here("Results", "Tables", "Diagnostic_method.tex"))

################################################################################
# Data imbalance: responses on temporal subsamples ----
################################################################################

complete = complete |> mutate(yearnum = as.numeric(as.character(year)))
g = ggplot(complete) +
  geom_histogram(aes(x = yearnum), color = "seagreen", fill = "seagreen") +
  xlab("year") +
  ylab("count of observations") +
  theme_classic()
g

# obs by group
complete = complete |> mutate(post1995 = (yearnum >= 1995))
complete %>% count(post1995)

# formula (different intervention dummies for each temporal subsample)
cXt2rXm = as.formula(paste0(
  common,
  " + I(intervention) +  ",
  country_time,
  " | OBJECTID  + as.factor(smllrgn):month | 0 | ",
  clustering
))

pre_data <- subset(complete, post1995 == FALSE)
pos_data <- subset(complete, post1995 == TRUE)

pre1995 = felm(data = pre_data, formula = cXt2rXm)
post1995 = felm(data = pos_data, formula = cXt2rXm)

# plot temperature responses
modellist = list(pre1995, post1995)
mycollabs = c(
  "Early sample (1901-1994)",
  "Late sample (1995-2016)"
)

percentiles_list = list()
pre_post <- c(F, T)
for (i in 1:length(pre_post)) {
  # i <- 1
  pre_post_data <- subset(complete, post1995 == pre_post[i])$temp
  temp_p01 <- quantile(pre_post_data, 0.01, na.rm = TRUE)
  temp_p99 <- quantile(pre_post_data, 0.99, na.rm = TRUE)
  percentiles_list[[i]] <- list(
    p01 = temp_p01,
    p99 = temp_p99,
    n = length(pre_post_data)
  )
}

plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)
figList = list()
for (m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)

  showtitle <- ifelse(m == 1, T, F)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]],
    "temp",
    plotXtemp,
    polyOrder = 2,
    cluster = T,
    xRef = myrefT,
    xLab = expression(paste("Mean temperature (", degree, "C)")),
    yLab = "Prevalence (%)",
    title = mycollabs[m],
    yLim = c(-35, 10),
    showYTitle = showtitle,
    plotmax_x = 3,
    plotmax_y = 5,
    max_x_size = 6
  ) +
    theme(plot.title = element_text(size = 10)) +
    geom_vline(
      xintercept = percentiles_list[[m]]$p01,
      colour = "grey39",
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = percentiles_list[[m]]$p99,
      colour = "grey39",
      linetype = "dashed"
    ) +
    annotate(
      geom = "text",
      x = 37,
      y = 0,
      vjust = -1,
      label = paste0("italic(n) == ", percentiles_list[[m]]$n),
      size = 5,
      parse = TRUE
    ) +
    theme(
      plot.title = element_text(size = 16),
      plot.title.position = "plot",
      axis.title.x = element_text(vjust = -0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
    )
}

# Create histogram grobs for each subsample (F02-style inset approach)
hist_data_list <- list(pre_data, pos_data)
yLim_split <- c(-37, 10)
hist_ymin <- yLim_split[1] # bottom of the response plot y-axis
hist_ymax <- hist_ymin + 5 # height of the histogram band

for (m in 1:length(hist_data_list)) {
  # Build a void histogram matching the x-axis of the response plot
  hist_inset <- ggplot() +
    geom_histogram(
      data = hist_data_list[[m]],
      mapping = aes(x = temp),
      fill = "#8B3A4A",
      alpha = 1,
      bins = 30,
      colour = "black"
    ) +
    theme_void() +
    scale_x_continuous(
      limits = c(Tmin, Tmax),
      expand = expansion(mult = c(0.0, 0.0))
    )

  # Convert to grob
  hist_grob <- ggplotGrob(hist_inset)

  # Add grob inset to each response figure
  figList[[m]] <- figList[[m]] +
    annotation_custom(
      grob = hist_grob,
      xmin = Tmin,
      xmax = Tmax,
      ymin = hist_ymin,
      ymax = hist_ymax
    )
}

p <- plot_grid(figList[[1]], figList[[2]], nrow = 1)

ggsave(
  filename = paste0("Supp_Figure_split_sample_1995.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = p,
  width = 10,
  height = 5
)

################################################################################
# Sensitivity to spatiotemporal controls (tabular output) ----
################################################################################

## country-5-year clustering ----
yr_bin_size <- 5
complete <- complete |>
  dplyr::mutate(yr_bin5 = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin5) |>
  dplyr::mutate(cntry_yrbin5 = dplyr::cur_group_id()) |>
  dplyr::ungroup()

## country-decade clustering ----
yr_bin_size <- 10
complete <- complete |>
  dplyr::mutate(yr_bin10 = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin10) |>
  dplyr::mutate(
    cntry_yrbin10 = dplyr::cur_group_id(),
    cntry_yrbin5 = as.factor(cntry_yrbin5),
    cntry_yrbin10 = as.factor(cntry_yrbin10)
  ) |>
  dplyr::ungroup()

complete <- complete |>
  dplyr::group_by(as.factor(smllrgn), month) |>
  dplyr::mutate(smllrgnMO = dplyr::cur_group_id()) |>
  dplyr::ungroup() |>
  dplyr::mutate(smllrgnMO = as.factor(smllrgnMO), )

# reference temperature - curve gets recentered to 0 here
Tref = 25
# temperature vector for plotting response function
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)


ym = as.formula(paste0(common, " | OBJECTID + year + month | 0 | cntry_yrbin5"))
cXt2m = as.formula(paste0(
  common,
  " + ",
  country_time,
  " | OBJECTID + month | 0 | cntry_yrbin5"
))
cXt2cXm = as.formula(paste0(
  common,
  " + ",
  country_time,
  " | OBJECTID + country:month | 0 | cntry_yrbin5"
))
cXt2intm = as.formula(paste0(
  common,
  " + ",
  country_time,
  " | OBJECTID + intervention + month | 0 | cntry_yrbin5"
))
# cXt2intrXm is main, called from config
cXt2intcXm = as.formula(paste0(
  common,
  " + I(intervention) + ",
  country_time,
  " | OBJECTID + country:month | 0 | cntry_yrbin5"
))
cXt2rXmyXm = as.formula(paste0(
  common,
  " + ",
  country_time,
  " | OBJECTID + as.factor(smllrgn):month + year:month | 0 | cntry_yrbin5"
))
rXmcXy = as.formula(paste0(
  common,
  " | OBJECTID + as.factor(smllrgn):month + country:year | 0 | cntry_yrbin5"
))
rXyrXm = as.formula(paste0(
  common,
  " | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | cntry_yrbin5"
))
rXycXm = as.formula(paste0(
  common,
  " | OBJECTID + country:month + as.factor(smllrgn):year | 0 | cntry_yrbin5"
))
aXdrXmd = as.formula(paste0(
  common,
  " | as.factor(OBJECTID):cntry_yrbin10  + smllrgnMO:cntry_yrbin10 | 0 | cntry_yrbin5"
))
rXyrXmcXt = as.formula(paste0(
  common,
  " + country:monthyr | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | cntry_yrbin5"
))
myforms = c(
  ym,
  cXt2m,
  cXt2cXm,
  cXt2intm,
  cXt2intrXm,
  cXt2intcXm,
  cXt2rXmyXm,
  rXmcXy,
  rXyrXm,
  rXycXm,
  aXdrXmd,
  rXyrXmcXt
)

mycollabs = c(
  "yr + mo FEs.", # 1
  "cntry trd, mo FEs.", #2
  "cntry trd, cntry-mo FEs.", #3
  "cntry trd, int + mo FEs.", #4
  "cntry trd, int + rgn-mo FEs.", #5 - Main Spec
  "cntry trd, int + cntry-mo FEs.", #6
  "cntry trd, year-mo + rgn-mo FEs.", #7
  "cntry-yr + rgn-mo FEs.", #8
  "rgn-yr + rgn-mo FEs.", #9
  "rgn-yr + cntry-mo FEs.", #10
  "adm-decade + rgn-mo-decade FEs.", #11
  "cntry trd, rgn-yr + rgn-mo FEs." #12
)

## Run all models
modellist = list()
i = 0
for (m in myforms) {
  i = i + 1
  modellist[[i]] = felm(data = complete, formula = m)
}

## Combine into a single stargazer plot
mynote = "Column specifications: (1) year and month FE; (2) country-specific quad. trends and month FE; (3) country-specific quad. trends and country-by-month FE; (4) country-specific quad. trends, intervention year and month FE; (5) country-specific quad. trends, intervention year FE, GBD region-month FE; (6) country-specific quad. trends with intervention FE and country-month FE; (7) country-specific quad. trends with year-month and GBD region-mont FE; (8) country-year and GBD region-month FE; (9) GBD region-year and regin-month FEs; (10) GBD region-year + country-month FE; (11) ADM1-decade and GBD region-month-decade FE; (12) country-specific quad. trends and GBD region-year and region-month FE."

tex <- stargazer(
  modellist,
  title = "Quadratic temperature: FE sensitivity",
  align = TRUE,
  column.labels = mycollabs,
  covariate.labels = my_covariate_labels,
  dep.var.labels = "$Pf$PR$_{2-10}$",
  keep = c("temp", "flood", "drought", "intervention", "METHOD"),
  # out = here::here("Results", "Tables", "FixedEffects_sensitivity.tex"),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  digits = 2,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  star.cutoffs = table_star_cutoffs
)

writeLines(tex, here::here("Results", "Tables", "FixedEffects_sensitivity.tex"))

################################################################################
# Sensitivity to spatiotemporal controls (figure output) ----
################################################################################

plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

nrowGrid <- 4
ncolGrid <- ceiling(length(modellist) / nrowGrid)
figList = list()
for (m in 1:length(modellist)) {
  isLeftCol <- ((m - 1) %% ncolGrid) == 0
  isBottomRow <- m > ncolGrid * (nrowGrid - 1)
  # get max of response function
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]],
    "temp",
    plotXtemp,
    polyOrder = 2,
    cluster = T,
    xRef = myrefT,
    xLab = expression(paste("Mean temperature (", degree, "C)")),
    yLab = "Prevalence (%)",
    title = mycollabs[m],
    yLim = c(-30, 5),
    showYTitle = isLeftCol,
    showXTitle = isBottomRow,
    max_x_size = 4,
    plotmax_x = 4,
    plotmax_y = 5,
    axis_size = 9,
    axis_title_size = 9,
    title_size = 9
  )
}

# point estimate and CIs for main spec
xValsT = genRecenteredXVals_polynomial(plotXtemp, Tref, 2)
mainmod = modellist[[5]]
beta = mainmod$coefficients
vars = rownames(beta)
plotVars = vars[grepl(pattern = "temp", x = vars)]
b = as.matrix(beta[rownames(beta) %in% plotVars])
vcov = getVcov(mainmod$clustervcv, plotVars)
response = as.matrix(xValsT) %*% b #Prediction
length = 1.96 * sqrt(apply(X = xValsT, FUN = calcVariance, MARGIN = 1, vcov))
lb = response - length
ub = response + length

#Plotting dataframe -- add back in the reference temperature so it's centered at xRef
plotData = data.frame(
  x = xValsT[, 1] + Tref,
  response = response,
  lb = lb,
  ub = ub
)
sub = plotData[plotData$x >= 10 & plotData$x <= 30, ]
maxX = max(sub$x[sub$response == max(sub$response)])

mycollabs = c(
  "ym",
  "cXt2m",
  "cXt2cXm",
  "cXt2intm",
  "cXt2intrXm",
  "cXt2intcXm",
  "cXt2rXmyXm",
  "rXmcXy",
  "rXyrXm",
  "rXycXm",
  "aXdrXmd",
  "rXyrXmcXt"
)

# loop over all other FE models, add to plotting dataframe
for (mod in 1:length(modellist)) {
  beta = modellist[[mod]]$coefficients
  vars = rownames(beta)
  plotVars = vars[grepl(pattern = "temp", x = vars)]
  b = as.matrix(beta[rownames(beta) %in% plotVars])
  response = as.data.frame(as.matrix(xValsT) %*% b)
  colnames(response) = paste0(mycollabs[mod])
  plotData = cbind(plotData, response)
}

# reshape
plotmain = plotData %>% dplyr::select(x, response, lb, ub)
plotFE = plotData %>% dplyr::select(x, ym:rXyrXmcXt)
plotFE = plotFE %>% gather(plotFE, response, ym:rXyrXmcXt)
colnames(plotFE) = c("x", "model", "response")

## plot
g = ggplot() +
  geom_hline(yintercept = 0, color = "darkgrey", alpha = .5) +
  geom_ribbon(
    data = plotmain, # CIs main spec
    mapping = aes(x, ymin = lb, ymax = ub),
    alpha = 0.4,
    fill = "#C1657C"
  ) +
  geom_line(
    data = plotFE, # point estimate other specs
    aes(x = x, y = response, group = model),
    color = "seagreen",
    alpha = 0.8
  ) +
  geom_line(
    data = plotmain, # point estimate main spec
    mapping = aes(x = x, y = response),
    color = "black",
    linewidth = 1
  ) +
  annotate(
    "curve",
    x = 32,
    xend = 35,
    y = -17,
    yend = -10,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = "black",
    linewidth = 0.4,
    curvature = -0.3
  ) +
  annotate(
    "text",
    x = 28,
    y = -20,
    label = "main\nspecification",
    size = 2.5,
    hjust = 0,
    color = "black"
  ) +
  # Arrow pointing to the other specifications (green lines)
  annotate(
    "curve",
    x = 18,
    xend = 13.5,
    y = -17,
    yend = -12,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = "seagreen",
    linewidth = 0.4,
    curvature = 0.3
  ) +
  annotate(
    "text",
    x = 18,
    y = -20,
    label = "all other\nspecifications",
    size = 2.5,
    hjust = 0.5,
    color = "seagreen"
  ) +
  geom_vline(xintercept = maxX, colour = "grey39") +
  annotate(
    geom = "text",
    x = maxX + 4,
    y = 5,
    label = paste0(maxX, " C"),
    color = "grey39",
    size = 4
  ) +
  theme_classic() +
  labs(
    x = NULL, # expression(paste("Mean temperature (", degree, "C)")),
    y = NULL, #"Prevalence (%)"
  ) +
  xlim(Tmin, Tmax) +
  ylim(-30, 5) +
  ggtitle("main: cnty trd, int + rgn−mo FEs.") +
  theme(
    plot.title = element_text(size = 9),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9)
  )

p = plot_grid(
  figList[[1]],
  figList[[2]],
  figList[[3]],
  figList[[4]],
  g,
  figList[[6]],
  figList[[7]],
  figList[[8]],
  figList[[9]],
  figList[[10]],
  figList[[11]],
  figList[[12]],
  nrow = 4
)
p

ggsave(
  filename = paste0("ED_Figure_panelFE_FE_sensitivity.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = p,
  width = 7,
  height = 9
)

################################################################################
# Influence analysis ----
################################################################################

## main model
betaTmain = mainmod$coefficients[1]
pvalTmain = summary(mainmod)$coefficients[1, 4]
betaT2main = mainmod$coefficients[2]
pvalT2main = summary(mainmod)$coefficients[2, 4]

## Leave one country out
cntrs <- unique(complete$country)
loco = data.frame(
  country = "",
  betaT = NA,
  pvalT = NA,
  betaT2 = NA,
  pvalT2 = NA
)
for (c in 1:length(cntrs)) {
  df <- complete |> dplyr::filter(country != cntrs[c])
  mod <- felm(df, formula = cXt2intrXm)
  mydat = data.frame(
    country = as.character(cntrs[c]),
    betaT = mod$coefficients[1],
    pvalT = summary(mod)$coefficients[1, 4],
    betaT2 = mod$coefficients[2],
    pvalT2 = summary(mod)$coefficients[2, 4]
  )
  loco = rbind(loco, mydat)
  rm(df, mod, mydat)
}

loco <- loco |> dplyr::filter(!is.na(betaT))

## Leave one country out ----
b1loco <- ggplot(data = loco) +
  geom_histogram(aes(x = betaT), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff.") +
  ylab("Count") +
  theme_classic()

p1loco <- ggplot(data = loco) +
  geom_histogram(aes(x = pvalT), bins = 30, color = "wheat2", fill = "wheat2") +
  geom_vline(xintercept = pvalTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff. p-value") +
  ylab("Count") +
  theme_classic()

b2loco <- ggplot(data = loco) +
  geom_histogram(aes(x = betaT2), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff.") +
  ylab("Count") +
  theme_classic()

p2loco <- ggplot(data = loco) +
  geom_histogram(
    aes(x = pvalT2),
    bins = 30,
    color = "wheat2",
    fill = "wheat2"
  ) +
  geom_vline(xintercept = pvalT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff. p-value") +
  ylab("Count") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

## Leave one year out ----
years <- unique(complete$yearnum)
loyo = data.frame(year = NA, betaT = NA, pvalT = NA, betaT2 = NA, pvalT2 = NA)
for (c in 1:length(years)) {
  df <- complete |> dplyr::filter(yearnum != years[c])
  mod <- felm(df, formula = cXt2intrXm)
  mydat = data.frame(
    year = as.character(years[c]),
    betaT = mod$coefficients[1],
    pvalT = summary(mod)$coefficients[1, 4],
    betaT2 = mod$coefficients[2],
    pvalT2 = summary(mod)$coefficients[2, 4]
  )
  loyo = rbind(loyo, mydat)
  rm(df, mod, mydat)
}

loyo <- loyo |> dplyr::filter(!is.na(betaT))

# plot
b1loyo <- ggplot(data = loyo) +
  geom_histogram(aes(x = betaT), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff.") +
  ylab("Count") +
  theme_classic()

p1loyo <- ggplot(data = loyo) +
  geom_histogram(aes(x = pvalT), bins = 30, color = "wheat2", fill = "wheat2") +
  geom_vline(xintercept = pvalTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff. p-value") +
  ylab("Count") +
  theme_classic()

b2loyo <- ggplot(data = loyo) +
  geom_histogram(aes(x = betaT2), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff.") +
  ylab("Count") +
  theme_classic()

p2loyo <- ggplot(data = loyo) +
  geom_histogram(
    aes(x = pvalT2),
    bins = 30,
    color = "wheat2",
    fill = "wheat2"
  ) +
  geom_vline(xintercept = pvalT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff. p-value") +
  ylab("Count") +
  theme_classic()

## Leave one month out ----
months <- unique(complete$month)
lomo = data.frame(month = NA, betaT = NA, pvalT = NA, betaT2 = NA, pvalT2 = NA)
for (c in 1:length(months)) {
  df <- complete |> dplyr::filter(month != months[c])
  mod <- felm(df, formula = cXt2intrXm)
  mydat = data.frame(
    month = months[c],
    betaT = mod$coefficients[1],
    pvalT = summary(mod)$coefficients[1, 4],
    betaT2 = mod$coefficients[2],
    pvalT2 = summary(mod)$coefficients[2, 4]
  )
  lomo = rbind(lomo, mydat)
  rm(df, mod, mydat)
}

lomo <- lomo |> dplyr::filter(!is.na(betaT))

# plot
b1lomo <- ggplot(data = lomo) +
  geom_histogram(aes(x = betaT), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff.") +
  ylab("Count") +
  theme_classic()

p1lomo <- ggplot(data = lomo) +
  geom_histogram(aes(x = pvalT), bins = 30, color = "wheat2", fill = "wheat2") +
  geom_vline(xintercept = pvalTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff. p-value") +
  ylab("Count") +
  theme_classic()
b2lomo <- ggplot(data = lomo) +
  geom_histogram(aes(x = betaT2), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff.") +
  ylab("Count") +
  theme_classic()
p2lomo <- ggplot(data = lomo) +
  geom_histogram(
    aes(x = pvalT2),
    bins = 30,
    color = "wheat2",
    fill = "wheat2"
  ) +
  geom_vline(xintercept = pvalT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff. p-value") +
  ylab("Count") +
  theme_classic()

# Create a bold label for each pair of rows
label1 <- ggdraw() +
  draw_label(
    "Leave one country out",
    fontface = "bold",
    x = 0,
    hjust = 0,
    size = 10
  ) +
  theme(plot.margin = margin(10, 0, 10, 7))

label2 <- ggdraw() +
  draw_label(
    "Leave one year out",
    fontface = "bold",
    x = 0,
    hjust = 0,
    size = 10
  ) +
  theme(plot.margin = margin(0, 0, 10, 7))

label3 <- ggdraw() +
  draw_label(
    "Leave one month out",
    fontface = "bold",
    x = 0,
    hjust = 0,
    size = 10
  ) +
  theme(plot.margin = margin(0, 0, 10, 7))

shrink_text <- function(p, size = 8) {
  p +
    theme(
      axis.text = element_text(size = size),
      axis.title = element_text(size = size)
    )
}

b1loco <- shrink_text(b1loco)
p1loco <- shrink_text(p1loco)
b2loco <- shrink_text(b2loco)
p2loco <- shrink_text(p2loco)

b1loyo <- shrink_text(b1loyo)
p1loyo <- shrink_text(p1loyo)
b2loyo <- shrink_text(b2loyo)
p2loyo <- shrink_text(p2loyo)

b1lomo <- shrink_text(b1lomo)
p1lomo <- shrink_text(p1lomo)
b2lomo <- shrink_text(b2lomo)
p2lomo <- shrink_text(p2lomo)

# Create a 2-column grid for each pair of rows
row_group1 <- plot_grid(b1loco, p1loco, b2loco, p2loco, nrow = 2)
row_group2 <- plot_grid(b1loyo, p1loyo, b2loyo, p2loyo, nrow = 2)
row_group3 <- plot_grid(b1lomo, p1lomo, b2lomo, p2lomo, nrow = 2)

# Stack each label above its corresponding row group
section1 <- plot_grid(label1, row_group1, ncol = 1, rel_heights = c(0.05, 1))
section2 <- plot_grid(label2, row_group2, ncol = 1, rel_heights = c(0.05, 1))
section3 <- plot_grid(label3, row_group3, ncol = 1, rel_heights = c(0.05, 1))

# Stack all three sections vertically
grid <- plot_grid(section1, section2, section3, ncol = 1)
ggsave(
  filename = paste0("Supp_Figure_influence_analysis.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = grid,
  width = 6,
  height = 8
)

################################################################################
# End of file ----
################################################################################
