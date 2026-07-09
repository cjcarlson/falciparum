################################################################################
# This script investigates correlation in the model residuals and assesses
# alternative methods of clustering or accounting for spatiotemporal
# correlations in errors.
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
  sp,
  gstat,
  fixest,
  raster,
  ggpubr,
  car,
  Hmisc,
  sf
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

sf::sf_use_s2(FALSE)

################################################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence and CRU
# temperature and precipitation data aggregated to the first level of
# Administrative division.
################################################################################

print("Loading clean data")
complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

################################################################################
# Estimate main model, store residuals ----
################################################################################

# Estimation & residuals
mainmod <- readRDS(main_mod_obj_fn)

complete <- complete |> mutate(res = c(residuals(mainmod)))

################################################################################
# Normally distributed errors ----
################################################################################

## histogram of model errors
complete <- complete |> mutate(res = c(residuals(mainmod)))
g <- ggplot(data = complete) +
  geom_histogram(aes(x = res), color = "seagreen", fill = "seagreen") +
  xlab("Model residuals") +
  ylab("Count") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
g

## Q-Q plot
p <- ggplot(complete, aes(sample = res)) +
  stat_qq() +
  stat_qq_line(color = "seagreen") +
  xlab("Normal distribution quantiles") +
  ylab("Model residuals quantiles") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
p

grid <- plot_grid(g, p, nrow = 1)
ggsave(
  filename = paste0("Supp_Figure_model_residuals.", fig_file_type),
  path = here::here("Results", "Figures"),
  plot = grid,
  width = 9,
  height = 4
)

################################################################################
# General correlation over space -- distributions of correlations ----
################################################################################

##### Temporal Correlation Matrix ----
residual_wide_yr_mn <- complete |>
  dplyr::select(OBJECTID, monthyr, res) |>
  arrange(monthyr) |>
  pivot_wider(names_from = monthyr, values_from = res) |>
  arrange(OBJECTID)

corr_matrix_yr_mn <- cor(
  residual_wide_yr_mn |> dplyr::select(-OBJECTID),
  use = "pairwise.complete.obs"
)

##### Pairwise N Temporal Matrix ----
count_matrix_yr_mn <- residual_wide_yr_mn |>
  dplyr::select(-OBJECTID) |>
  count_pairwise_obs()

##### Spatial Correlation Matrix ----
residual_wide_location <- complete |>
  dplyr::mutate(
    short_region = case_match(
      smllrgn,
      "Sub-Saharan Africa (Central)" ~ "C",
      "Sub-Saharan Africa (West)" ~ "W",
      "Sub-Saharan Africa (Southern)" ~ "S",
      "Sub-Saharan Africa (East)" ~ "E",
      .default = NA_character_
    ),
    location = paste(short_region, ISO, OBJECTID, sep = ".")
  ) |>
  dplyr::select(location, monthyr, res) |>
  arrange(monthyr) |>
  pivot_wider(names_from = location, values_from = res) |>
  arrange(monthyr)

corr_matrix_location <- cor(
  residual_wide_location |> dplyr::select(-monthyr),
  use = "pairwise.complete.obs"
)

##### Pairwise N Spatial Matrix ----
count_matrix_location <- residual_wide_location |>
  dplyr::select(-monthyr) |>
  count_pairwise_obs()

##### Mean N per ObjectID ----
complete |>
  dplyr::group_by(OBJECTID, smllrgn) |>
  dplyr::summarize(n = n()) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    mean = mean(n),
    median = median(n)
  )

##### Distance Matrix ----
location_simple <- complete |>
  dplyr::distinct(OBJECTID, smllrgn, ISO) |>
  dplyr::mutate(
    short_region = case_match(
      smllrgn,
      "Sub-Saharan Africa (Central)" ~ "C",
      "Sub-Saharan Africa (West)" ~ "W",
      "Sub-Saharan Africa (Southern)" ~ "S",
      "Sub-Saharan Africa (East)" ~ "E",
      .default = NA_character_
    ),
    location = paste(short_region, ISO, OBJECTID, sep = ".")
  )

centroids <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::mutate(
    lon = sf::st_coordinates(sf::st_centroid(geometry))[, 1],
    lat = sf::st_coordinates(sf::st_centroid(geometry))[, 2],
    OBJECTID = as.numeric(OBJECTID)
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(OBJECTID, lon, lat) |>
  dplyr::filter(OBJECTID %in% unique(complete$OBJECTID)) |>
  dplyr::left_join(location_simple, by = join_by(OBJECTID))

centers <- sf::st_as_sf(centroids, coords = c("lon", "lat"), crs = 4326)
distMat <- s2::s2_distance_matrix(centers, centers)
dimnames(distMat) <- list(centers$location, centers$location)

dist_matrix <- corr_matrix_location
dist_matrix[] <- NA_real_
dist_matrix[rownames(distMat), colnames(distMat)] <- distMat

##### Distance between centroids ----
distanceMatrix_km <- dist_matrix / 1000
upper_triangle <- upper.tri(distanceMatrix_km, diag = FALSE)
distances_km <- distanceMatrix_km[upper_triangle]
cat(
  "Mean distance:",
  mean(distances_km),
  "km\n",
  "Median distance:",
  median(distances_km),
  "km\n",
  "Minimum distance:",
  min(distances_km),
  "km\n",
  "Maximum distance:",
  max(distances_km),
  "km\n"
)
ggplot(data.frame(distances = distances_km), aes(x = distances)) +
  geom_histogram(bins = 100) +
  labs(x = "Distance (km)", y = "Frequency")

##### Correlation Data ----
sel <- upper.tri(corr_matrix_yr_mn, diag = FALSE)
corrVecYear <- corr_matrix_yr_mn[sel]
timeDiff <- (col(corr_matrix_yr_mn) - row(corr_matrix_yr_mn))[sel]
obsCountVecYearMon <- count_matrix_yr_mn[sel]

sel <- upper.tri(corr_matrix_location, diag = FALSE)
corrVecGid1 <- corr_matrix_location[sel]
distVecGid1 <- dist_matrix[sel]
obsCountVecGid1 <- count_matrix_location[sel]

colGid1 <- colnames(corr_matrix_location)[col(corr_matrix_location)[sel]]
rowGid1 <- rownames(corr_matrix_location)[row(corr_matrix_location)[sel]]
colGid0 <- str_sub(colGid1, start = 3, end = 5)
rowGid0 <- str_sub(rowGid1, start = 3, end = 5)
colReg <- str_sub(colGid1, end = 1)
rowReg <- str_sub(rowGid1, end = 1)

##### Same month, different year ----
base_date <- as.Date("1900-01-01")
date_indices <- base_date + as.numeric(colnames(corr_matrix_yr_mn))

# Extract month and year for each index
months_vec <- month(date_indices)
years_vec <- year(date_indices)

# Get month and year for each pair in the upper triangle
month_col <- months_vec[col(corr_matrix_yr_mn)[sel]]
month_row <- months_vec[row(corr_matrix_yr_mn)[sel]]
year_col <- years_vec[col(corr_matrix_yr_mn)[sel]]
year_row <- years_vec[row(corr_matrix_yr_mn)[sel]]

# Calculate year difference for each pair
yearDiff <- year_col - year_row

###### Minimum obs for correlation and optional obs weighting ----
T_min <- 10
# weighting <- TRUE
weighting <- FALSE

corrData <- bind_rows(
  ##### temporal correlations ----
  analyze_corr("temporal", corrVecYear, TRUE, "all", obsCountVecYearMon),
  analyze_corr("temporal", corrVecYear, timeDiff == 1, "1", obsCountVecYearMon),
  analyze_corr("temporal", corrVecYear, timeDiff == 2, "2", obsCountVecYearMon),
  analyze_corr("temporal", corrVecYear, timeDiff == 3, "3", obsCountVecYearMon),
  analyze_corr(
    "temporal",
    corrVecYear,
    abs(yearDiff) <= 5,
    "within 5 years",
    obsCountVecYearMon
  ),
  analyze_corr(
    "temporal",
    corrVecYear,
    abs(yearDiff) > 5,
    "> 5 years",
    obsCountVecYearMon
  ),
  ##### Basic spatial patterns ----
  analyze_corr("spatial", corrVecGid1, TRUE, "all", obsCountVecGid1),
  analyze_corr(
    "spatial",
    corrVecGid1,
    colGid0 == rowGid0,
    "same country",
    obsCountVecGid1
  ),
  analyze_corr(
    "spatial",
    corrVecGid1,
    colGid0 != rowGid0,
    "different country",
    obsCountVecGid1
  ),
  analyze_corr(
    "spatial",
    corrVecGid1,
    colGid0 != rowGid0 & colReg == rowReg,
    "same region",
    obsCountVecGid1
  ),
  analyze_corr(
    "spatial",
    corrVecGid1,
    colGid0 != rowGid0 & colReg != rowReg,
    "different region",
    obsCountVecGid1
  ),
  # ##### thresholds (less than) ----
  analyze_corr(
    "spatial",
    corrVecGid1,
    distVecGid1 < 5e5,
    "< 500km",
    obsCountVecGid1
  ),
  analyze_corr(
    "spatial",
    corrVecGid1,
    distVecGid1 < 5e5 & colGid0 != rowGid0,
    "< 500km and different country",
    obsCountVecGid1
  ),
  analyze_corr(
    "spatial",
    corrVecGid1,
    distVecGid1 < 5e5 & colGid0 == rowGid0,
    "< 500km and same country",
    obsCountVecGid1
  ),
  ##### thresholds (greater than) ----
  analyze_corr(
    "spatial",
    corrVecGid1,
    distVecGid1 > 1e6,
    "> 1000km",
    obsCountVecGid1
  ),
  analyze_corr(
    "spatial",
    corrVecGid1,
    distVecGid1 > 5e5 & colGid0 != rowGid0,
    "> 500km and different country",
    obsCountVecGid1
  ),
  analyze_corr(
    "spatial",
    corrVecGid1,
    distVecGid1 > 5e5 & colGid0 == rowGid0,
    "> 500km and same country",
    obsCountVecGid1
  )
) |>
  dplyr::select(kind, group, mean, q25, q75, n)

corrData

tex <- df_to_latex(corrData)

writeLines(
  tex,
  con = here::here("Results", "Tables", "spatial_and_serial_correlations.tex")
)

################################################################################
# Sensitivity to clustering  ----
################################################################################

## ADM1 clustering ----
adm1form <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)
adm1mod <- felm(data = complete, formula = adm1form)
# only need to compute this and the next line once, all specs have same coeffs but different CIs
coefs <- summary(adm1mod)$coefficients[1:2]
# plot relative to max of Quadratic function
myrefT <- max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)
plotXtemp <- cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)
adm1fig <- plotPolynomialResponse(
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
  plotmax_x = 3,
  plotmax_y = 5,
  max_x_size = 8,
  showYTitle = T,
  showXTitle = F,
  axis_size = 20,
  axis_title_size = 22,
  title_size = 22
)

## country clustering ----
isoform <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | country"
  )
)
isomod <- felm(data = complete, formula = isoform)
isofig <- plotPolynomialResponse(
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
  plotmax_x = 3,
  plotmax_y = 5,
  max_x_size = 8,
  showYTitle = F,
  showXTitle = F,
  axis_size = 20,
  axis_title_size = 22,
  title_size = 22
)

## country x year clustering ----
# (no correlation over years)
complete <- complete |>
  group_by(country, year) |>
  mutate(cntryyr = cur_group_id()) |>
  ungroup()

isoyrform <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | cntryyr"
  )
)
isoyrmod <- felm(data = complete, formula = isoyrform)
isoyrfig <- plotPolynomialResponse(
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
  plotmax_x = 3,
  plotmax_y = 5,
  max_x_size = 8,
  showYTitle = F,
  showXTitle = F,
  axis_size = 20,
  axis_title_size = 22,
  title_size = 22
)

## year clustering ----
yrform <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | year"
  )
)
yrmod <- felm(data = complete, formula = yrform)
yrfig <- plotPolynomialResponse(
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
  plotmax_x = 3,
  plotmax_y = 5,
  max_x_size = 8,
  showYTitle = F,
  showXTitle = F,
  axis_size = 20,
  axis_title_size = 22,
  title_size = 22
)

## country-5-year clustering ----
yr_bin_size <- 5
complete <- complete |>
  dplyr::mutate(yr_bin5 = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin5) |>
  dplyr::mutate(cntry_yrbin5 = dplyr::cur_group_id()) |>
  dplyr::ungroup()

iso5form <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin5"
  )
)
iso5mod <- felm(data = complete, formula = iso5form)
iso5fig <- plotPolynomialResponse(
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
  plotmax_x = 3,
  plotmax_y = 5,
  max_x_size = 8,
  showYTitle = T,
  showXTitle = T,
  axis_size = 20,
  axis_title_size = 22,
  title_size = 22
)

## country-decade clustering ----
yr_bin_size <- 10
complete <- complete |>
  dplyr::mutate(yr_bin10 = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin10) |>
  dplyr::mutate(cntry_yrbin10 = dplyr::cur_group_id()) |>
  dplyr::ungroup()

iso10form <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin10"
  )
)
iso10mod <- felm(data = complete, formula = iso10form)
iso10fig <- plotPolynomialResponse(
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
  plotmax_x = 3,
  plotmax_y = 5,
  max_x_size = 8,
  showYTitle = F,
  showXTitle = T,
  axis_size = 20,
  axis_title_size = 22,
  title_size = 22
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

conleyform <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    " + as.factor(smllrgn):month | OBJECTID"
  )
)

conley_dist_1 <- 200
conley_dist_2 <- 500

conleymod1 <- feols(
  conleyform,
  data = spdf,
  conley(conley_dist_1, distance = "spherical")
)
conleymod2 <- feols(
  conleyform,
  data = spdf,
  conley(conley_dist_2, distance = "spherical")
)

coefs <- summary(conleymod1)$coefficients[1:2]
myrefT <- max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)
conleyfig1 <- plotPolynomialResponse(
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
  plotmax_x = 3,
  plotmax_y = 5,
  max_x_size = 8,
  showYTitle = F,
  showXTitle = T,
  axis_size = 20,
  axis_title_size = 22,
  title_size = 22
)

coefs <- summary(conleymod2)$coefficients[1:2]
myrefT <- max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) # plot relative to max of Quadratic function
conleyfig2 <- plotPolynomialResponse(
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
  plotmax_x = 3,
  plotmax_y = 5,
  max_x_size = 8,
  showYTitle = F,
  showXTitle = T,
  axis_size = 20,
  axis_title_size = 22,
  title_size = 22
)

## merged plot
uncert <- plot_grid(
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
  filename = paste0("Supp_Figure_temp_response_difft_SEs.", fig_file_type),
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
modellist <- list(
  adm1mod,
  isomod,
  yrmod,
  isoyrmod,
  iso5mod,
  iso10mod
)
mycollabs <- c(
  "Adm1 clust.",
  "Country clust.",
  "Year clust.",
  "Country-year clust.",
  "Country-5-year clust.",
  "Country-decade clust."
)

mynote <- "Column specifications: (1) standard errors clustered at ADM1 level; (2) standard errors clustered at country level; (3) standard errors clustered at year level; (4) standard errors clustered at country-year level; (5) standard errors clustered at country-5-year level (main specification); (6) standard errors clustered at country-decade level; (7) standard errors estimated following Conley (2008) using 200km cutoff; (6) standard errors estimated following Conley (2008) using a 500km cutoff."

tex <- stargazer(
  modellist,
  title = "Quadratic temperature: standard error sensitivity",
  align = TRUE,
  column.labels = mycollabs,
  covariate.labels = my_covariate_labels,
  dep.var.labels = "$Pf$PR$_{2-10}$",
  keep = c("temp", "flood", "drought", "intervention"),
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

################################################################################
# End of file ----
################################################################################
