################################################################################
# This script is temporary. It should be integrated into other D03-D04 scripts
# as it contains additional robustness tests.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  sf,
  sp,
  car,
  lfe,
  zoo,
  here,
  gstat,
  ggpubr,
  fixest,
  reshape,
  cowplot,
  multcomp,
  stargazer,
  tidyverse,
  lubridate,
  conleyreg
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

sf::sf_use_s2(FALSE)

############################################################
# Load data, set plotting toggles ----
############################################################

complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

# reference temperature - curve gets recentered to 0 here
Tref = 25
# temperature vector for plotting response function
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

############################################################
# Clustering sensitivity ----
############################################################

## ADM1 clustering ----
adm1form = as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)
adm1mod = felm(data = complete, formula = adm1form)
# only need to compute this and the next line once, all specs have same coeffs but different CIs
coefs = summary(adm1mod)$coefficients[1:2]
# plot relative to max of Quadratic function
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) 
adm1fig = plotPolynomialResponse(
  adm1mod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "ADM1 clust.",
  yLim = c(-30, 5),
  showYTitle = T
)

## country clustering ----
isoform = as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | country"
  )
)
isomod = felm(data = complete, formula = isoform)
isofig = plotPolynomialResponse(
  isomod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "country clust.",
  yLim = c(-30, 5),
  showYTitle = T
)

## country x year clustering ----
# (no correlation over years)
complete = complete |>
  group_by(country, year) |>
  mutate(cntryyr = cur_group_id()) |>
  ungroup()

isoyrform = as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | cntryyr"
  )
)
isoyrmod = felm(data = complete, formula = isoyrform)
isoyrfig = plotPolynomialResponse(
  isoyrmod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "country-year clust.",
  yLim = c(-30, 5),
  showYTitle = T
)

## year clustering ----
yrform = as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | year"
  )
)
yrmod = felm(data = complete, formula = yrform)
yrfig = plotPolynomialResponse(
  yrmod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "year clust.",
  yLim = c(-30, 5),
  showYTitle = T
)

## country-5-year clustering ----
yr_bin_size <- 5
complete <- complete |>
  dplyr::mutate(yr_bin5 = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin5) |>
  dplyr::mutate(cntry_yrbin5 = dplyr::cur_group_id()) |>
  dplyr::ungroup()

iso5form = as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin5"
  )
)
iso5mod = felm(data = complete, formula = iso5form)
iso5fig = plotPolynomialResponse(
  iso5mod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "country-5-year clust. (main)",
  yLim = c(-30, 5),
  showYTitle = T
)

## country-decade clustering ----
yr_bin_size <- 10
complete <- complete |>
  dplyr::mutate(yr_bin10 = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin10) |>
  dplyr::mutate(cntry_yrbin10 = dplyr::cur_group_id()) |>
  dplyr::ungroup()

iso10form = as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin10"
  )
)
iso10mod = felm(data = complete, formula = iso10form)
iso10fig = plotPolynomialResponse(
  iso10mod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = "country-decade clust.",
  yLim = c(-30, 5),
  showYTitle = T
)

## Conley standard errors ----

centroids <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::mutate(
    lon = sf::st_coordinates(sf::st_centroid(geometry))[, 1],
    lat = sf::st_coordinates(sf::st_centroid(geometry))[, 2],
    OBJECTID = as.numeric(OBJECTID)
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(OBJECTID, lon, lat)

spdf <- complete |>
  dplyr::left_join(centroids, by = join_by(OBJECTID))

conleyform = as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " + as.factor(smllrgn):month | OBJECTID"
  )
)

conley_dist_1 <- 200
conley_dist_2 <- 500

conleymod1 = feols(
  conleyform,
  data = spdf,
  conley(conley_dist_1, distance = "spherical")
)
conleymod2 = feols(
  conleyform,
  data = spdf,
  conley(conley_dist_2, distance = "spherical")
)

coefs = summary(conleymod1)$coefficients[1:2]
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)
conleyfig1 = plotPolynomialResponse(
  conleymod1,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = paste0("Conley (", conley_dist_1, "km)"),
  yLim = c(-30, 5),
  showYTitle = T
)

coefs = summary(conleymod2)$coefficients[1:2]
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) # plot relative to max of Quadratic function
conleyfig2 = plotPolynomialResponse(
  conleymod2,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = paste0("Conley (", conley_dist_2, "km)"),
  yLim = c(-30, 5),
  showYTitle = T
)

## merged plot
uncert = plot_grid(
  adm1fig,
  isofig,
  yrfig,
  isoyrfig,
  iso5fig,
  iso10fig,
  conleyfig1,
  conleyfig2,
  nrow = 2
)

ggsave(
  filename = "Supp_Figure_temp_response_difft_SEs.jpg",
  path = here::here("Results", "Figures"),
  plot = uncert,
  width = 20,
  height = 10
)

## Table
# feols models do not work with stargazer as it has no method for feols objects (class "fixest")
# so we use stargazer on the felm objects and etable on the feols objects. The two tables are
# then combined manually

# tabular output
modellist = list(
  adm1mod,
  isomod,
  yrmod,
  isoyrmod,
  iso5mod,
  iso10mod
)
mycollabs = c(
  "Adm1 clust.",
  "Country clust.",
  "Year clust.",
  "Country-year clust.",
  "Country-5-year clust.",
  "Country-decade clust."
)

mynote = "Column specifications: (1) standard errors clustered at ADM1 level; (2) standard errors clustered at country level; (3) standard errors clustered at year level; (4) standard errors clustered at country-year level; (5) standard errors clustered at country-5-year level (main specification); (6) standard errors clustered at country-decade level; (7) standard errors estimated following Conley (2008) using 200km cutoff; (6) standard errors estimated following Conley (2008) using a 500km cutoff."

tex <- stargazer(
  modellist,
  title = "Quadratic temperature: standard error sensitivity",
  align = TRUE,
  column.labels = mycollabs,
  covariate.labels = my_covariate_labels,
  dep.var.labels = "$Pf$PR$_{2-10}$",
  keep = c("temp", "flood", "drought", "intervention"),
  # out = here::here("Results", "Tables", "uncertainty.tex"),
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

writeLines(tex, here::here("Results", "Tables", "uncertainty.tex"))

conley_tab <- fixest::etable(
  conleymod1,
  conleymod2,
  keep = c("temp", "flood", "drought", "intervention"),
  tex = TRUE,
  fitstat = c("n", "r2", "ar2"),
  digits = 3,
  label = "tab:conley",
  file = here::here("Results", "Tables", "conley.tex"),
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05)
)

conley_tab

############################################################
# Sensitivity to spatiotemporal controls (tabular output) ----
############################################################

# felm doesn't like triple interactions, hard code this one
complete <- complete |>
  dplyr::group_by(as.factor(smllrgn), month) |>
  dplyr::mutate(smllrgnMO = dplyr::cur_group_id()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    smllrgnMO = as.factor(smllrgnMO),
    cntry_yrbin10 = as.factor(cntry_yrbin10)
  )

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

############################################################
# Sensitivity to spatiotemporal controls (figure output) ----
############################################################

plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

figList = list()
for (m in 1:length(modellist)) {
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
    showYTitle = T
  ) +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 8)
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
    x = maxX + 3.5,
    y = 2.55,
    label = paste0(maxX, " C"),
    color = "grey39",
    size = 3
  ) +
  theme_classic() +
  labs(
    x = expression(paste("Mean temperature (", degree, "C)")),
    y = "Prevalence (%)"
  ) +
  xlim(Tmin, Tmax) +
  ylim(-30, 5) +
  ggtitle("main: cnty trd, int + rgn−mo FEs.") +
  theme(
    text = element_text(size = 8),
    plot.title = element_text(size = 8)
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
  filename = "Supp_Figure_panelFE_FE_sensitivity.jpg",
  path = here::here("Results", "Figures"),
  plot = p,
  width = 7,
  height = 9
)

############################################################
# Influence analysis ----
############################################################

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

# plot ----
b1loco = ggplot(data = loco) +
  geom_histogram(aes(x = betaT), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff.") +
  ylab("Count") +
  theme_classic()
p1loco = ggplot(data = loco) +
  geom_histogram(aes(x = pvalT), bins = 30, color = "wheat2", fill = "wheat2") +
  geom_vline(xintercept = pvalTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff. p-value") +
  ylab("Count") +
  theme_classic()
b2loco = ggplot(data = loco) +
  geom_histogram(aes(x = betaT2), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff.") +
  ylab("Count") +
  theme_classic()
p2loco = ggplot(data = loco) +
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
b1loyo = ggplot(data = loyo) +
  geom_histogram(aes(x = betaT), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff.") +
  ylab("Count") +
  theme_classic()
p1loyo = ggplot(data = loyo) +
  geom_histogram(aes(x = pvalT), bins = 30, color = "wheat2", fill = "wheat2") +
  geom_vline(xintercept = pvalTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff. p-value") +
  ylab("Count") +
  theme_classic()
b2loyo = ggplot(data = loyo) +
  geom_histogram(aes(x = betaT2), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff.") +
  ylab("Count") +
  theme_classic()
p2loyo = ggplot(data = loyo) +
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
b1lomo = ggplot(data = lomo) +
  geom_histogram(aes(x = betaT), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff.") +
  ylab("Count") +
  theme_classic()
p1lomo = ggplot(data = lomo) +
  geom_histogram(aes(x = pvalT), bins = 30, color = "wheat2", fill = "wheat2") +
  geom_vline(xintercept = pvalTmain, color = "darkgrey", linetype = "dashed") +
  xlab("Linear temp. coeff. p-value") +
  ylab("Count") +
  theme_classic()
b2lomo = ggplot(data = lomo) +
  geom_histogram(aes(x = betaT2), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = betaT2main, color = "darkgrey", linetype = "dashed") +
  xlab("Quadratic temp. coeff.") +
  ylab("Count") +
  theme_classic()
p2lomo = ggplot(data = lomo) +
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
  filename = "Supp_Figure_influence_analysis.jpg",
  path = here::here("Results", "Figures"),
  plot = grid,
  width = 6,
  height = 8
)

############################################################
# Normally distributed errors ----
############################################################

## histogram of model errors
complete <- complete |> mutate(res = c(residuals(mainmod)))
g <- ggplot(data = complete) +
  geom_histogram(aes(x = res), color = "seagreen", fill = "seagreen") +
  xlab("Model residuals") +
  ylab("Count") +
  theme_classic()
g

## Q-Q plot
p <- ggplot(complete, aes(sample = res)) +
  stat_qq() +
  stat_qq_line(color = "seagreen") +
  xlab("Normal distribution quantiles") +
  ylab("Model residuals quantiles") +
  theme_classic()
p

grid = plot_grid(g, p, nrow = 1)
ggsave(
  filename = "Supp_Figure_model_residuals.jpg",
  path = here::here("Results", "Figures"),
  # path = figure_res_dir,
  plot = grid,
  width = 9,
  height = 4
)


# apptainer exec \
#   --bind /global/scratch/projects/co_carleton:/global/scratch/projects/co_carleton \
#   --bind /global/home/users/cmolitor/falciparum:/global/home/users/cmolitor/falciparum \
#   --pwd /global/home/users/cmolitor/falciparum \
#   /global/scratch/projects/co_carleton/carleton_colab/software/apptainers/rocker-geospatial.sif \
#   Rscript "Pipeline/D - Model sensitivity analyses and checks/D04b - newrob.R"