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
# Read in the analysis ready data file with malaria prevalence
# and CRU temperature and precipitation data aggregated to
# the first level of Administrative division.
################################################################################

print("Loading clean data")
complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

################################################################################
# Estimate main model, store residuals ----
################################################################################

# Estimation & residuals
mainmod = lfe::felm(data = complete, formula = cXt2intrXm)
complete <- complete |> mutate(res = c(residuals(mainmod)))

################################################################################
# A: Correlation across ADM1s within a country (same year-month) ----
################################################################################

# Regress residuals on country dummies, control for month and year
resCntry = felm(res ~ I(country) | month + year | 0 | 0, data = complete)

# Histogram of p-values on each country's coefficient
pvals = summary(resCntry)$coefficients[, "Pr(>|t|)"]
ph = ggplot() +
  geom_histogram(aes(x = pvals), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = 0.05, color = "grey") +
  xlab("country p-value (null: no correlation within country)") +
  ylab("# countries") +
  theme_classic(base_size = 12)
ph

# Boxplot of residuals by country
g = ggplot(complete, aes(x = country, y = res)) +
  geom_boxplot() +
  theme_classic(base_size = 12) +
  ylab("residuals") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
g

# Fstatistic
df = data.frame(
  stat = c("F stat", "p value"),
  value = c(summary(resCntry)$P.fstat[5], summary(resCntry)$P.fstat[1])
)
write.csv(df, file.path(table_diag_res_dir, "residuals_country_Fstat.csv"))

################################################################################
# B: Correlation across ADM1s within a GBOD region (same year-month) ----
################################################################################

# Regress residuals on country dummies, control for month and year
resGBOD = felm(res ~ I(smllrgn) | month + year | 0 | 0, data = complete)

# Histogram of p-values on each region's coefficient
pvalsR = summary(resGBOD)$coefficients[, "Pr(>|t|)"]
pr = ggplot() +
  geom_histogram(aes(x = pvalsR), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = 0.05, color = "grey") +
  xlab("region p-value (null: no correlation within region)") +
  ylab("# regions") +
  theme_classic(base_size = 12)
pr

# Boxplot of residuals by region
gr = ggplot(complete, aes(x = as.factor(smllrgn), y = res)) +
  geom_boxplot() +
  theme_classic(base_size = 12) +
  ylab("residuals") +
  xlab("region") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
gr

# Fstatistic
df = data.frame(
  stat = c("F stat", "p value"),
  value = c(summary(resGBOD)$P.fstat[5], summary(resGBOD)$P.fstat[1])
)
write.csv(df, file.path(table_diag_res_dir, "residuals_GBOD_Fstat.csv"))

################################################################################
# C: Correlation across months (same location) ----
################################################################################

# Regress residuals on country dummies, control for OBJECTID
resMonth = felm(res ~ I(month) | OBJECTID | 0 | 0, data = complete)

complete = complete %>%
  mutate(monthord = factor(month, levels = month.abb))

# Histogram of p-values on each region's coefficient
pvalsM = summary(resMonth)$coefficients[, "Pr(>|t|)"]
pm = ggplot() +
  geom_histogram(aes(x = pvalsM), color = "seagreen", fill = "seagreen") +
  geom_vline(xintercept = 0.05, color = "grey") +
  xlab("monthly p-value (null: no correlation within months)") +
  ylab("# months") +
  theme_classic(base_size = 12)
pm

# Boxplot of residuals by month
gm = ggplot(complete, aes(x = as.factor(monthord), y = res)) +
  geom_boxplot() +
  theme_classic(base_size = 12) +
  ylab("residuals") +
  xlab("month")
gm

# Fstatistic
df = data.frame(
  stat = c("F stat", "p value"),
  value = c(summary(resMonth)$P.fstat[5], summary(resMonth)$P.fstat[1])
)
write.csv(df, file.path(table_diag_res_dir, "residuals_Month_Fstat.csv"))

# combine all boxplots
box = ggarrange(g, gr, gm, ncol = 1, nrow = 3, labels = "auto")
box
ggsave(
  filename = "residuals_ALL_boxplot.jpg",
  path = figure_res_dir,
  plot = box,
  width = 5,
  height = 5
)

# combine all pval hists
hists = ggarrange(ph, pr, pm, ncol = 1, nrow = 3, labels = "auto")
hists
ggsave(
  filename = "pvals_ALL_correlations.jpg",
  path = table_diag_res_dir,
  plot = hists,
  width = 5,
  height = 5
)

################################################################################
# D: General correlation over space -- distributions of correlations ----
################################################################################

##### Temporal Correlation Matrix ----
residual_wide_yr_mn <-
  complete |>
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
residual_wide_location <-
  complete |>
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
  dplyr::summarize(
    n = n()
  ) |>
  dplyr::ungroup() |>
  # dplyr::group_by(smllrgn) |>
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
  dplyr::select(OBJECTID, lon, lat)

location_simple <- location_simple |>
  dplyr::left_join(centroids, by = join_by(OBJECTID))

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

##### Minimum obs for correlation and optional obs weighting ----
T_min <- 10
# weighting <- TRUE
weighting <- FALSE

##### Temporal specs ----
temporal_specs <- list(
  list(TRUE, "all"),
  list(timeDiff == 1, "1"),
  list(timeDiff == 2, "2"),
  list(timeDiff == 3, "3"),
  list(month_col == month_row & year_col != year_row, "same month, diff year")
)

##### Spatial specs ----
# Distance thresholds used throughout
dists <- c(1e5, 2e5, 5e5, 1e6, 2e6)
dist_labels <- c("100km", "200km", "500km", "1000km", "2000km")

spatial_specs <- c(
  # Basic groupings
  list(
    list(TRUE, "all"),
    list(colGid0 == rowGid0, "same country"),
    list(colGid0 != rowGid0, "different country"),
    list(colGid0 != rowGid0 & colReg == rowReg, "same region"),
    list(colGid0 != rowGid0 & colReg != rowReg, "different region")
  ),
  # Distance thresholds: <, >, and crossed with same/different country
  unlist(
    recursive = FALSE,
    lapply(
      seq_along(dists),
      function(i) {
        d <- dists[i]
        lab <- dist_labels[i]
        list(
          list(distVecGid1 < d, paste("<", lab)),
          list(
            distVecGid1 < d & colGid0 != rowGid0,
            paste("<", lab, "and different country")
          ),
          list(
            distVecGid1 < d & colGid0 == rowGid0,
            paste("<", lab, "and same country")
          ),
          list(distVecGid1 > d, paste(">", lab)),
          list(
            distVecGid1 > d & colGid0 != rowGid0,
            paste(">", lab, "and different country")
          ),
          list(
            distVecGid1 > d & colGid0 == rowGid0,
            paste(">", lab, "and same country")
          )
        )
      }
    )
  )
)

#### CorrData ----
corrData <- bind_rows(
  run_corr_specs(
    "temporal",
    corrVecYear,
    temporal_specs,
    obsCountVecYearMon,
    T_min,
    weighting
  ),
  run_corr_specs(
    "spatial",
    corrVecGid1,
    spatial_specs,
    obsCountVecGid1,
    T_min,
    weighting
  )
)

df_to_latex(dplyr::select(corrData, kind, group, mean, q25, q75, n))

################################################################################
# E: General correlation over space -- VARIOGRAMS ----
################################################################################

# create year groupings for variogram
complete = complete %>%
  mutate(yeargp = (yearnum - min(yearnum)) %/% 5 * 5 + min(yearnum))

spdf <- complete |>
  dplyr::left_join(centroids, by = join_by(OBJECTID))

# Estimate an empirical variogram
# coordinates - so variogram is in m
coordinates(spdf) = ~ lon + lat
projection(spdf) = CRS("+init=EPSG:4326")

# estimate variogram, 0 lags
vv = variogram(res ~ 1, data = spdf, projection(FALSE))
vvP = variogram(PfPR2 ~ 1, data = spdf, projection(FALSE))
f <- fit.variogram(vv, vgm("Sph"))
fP <- fit.variogram(vvP, vgm("Sph"))

vvplot = plot(vv, model = f, xlab = "distance (km)", main = "Model residuals")
vvPplot = plot(
  vvP,
  model = fP,
  xlab = "distance (km)",
  main = "Prevalence (PfPR2)"
)

# vars = ggarrange(vvplot, vvPplot, ncol = 2, nrow = 1)
vars = ggarrange(vvPplot, vvplot, ncol = 2, nrow = 1)
vars
ggsave(
  filename = "variogram_2panel.jpg",
  path = table_diag_res_dir,
  plot = vars,
  width = 9,
  height = 5,
  bg = "white"
)

# By year groupings
range = data.frame(yeargp = NA, n = NA, range = NA)

for (y in unique(spdf$yeargp)) {
  test = subset(spdf, yeargp == y)
  if (dim(test)[1] > 100) {
    vv = variogram(res ~ 1, data = test, projection(FALSE))
    f = fit.variogram(vv, vgm("Sph"))
    range = rbind(range, c(y, dim(test)[1], f$range[2]))
  }
}

range = range %>% arrange(yeargp)
hist(range$range, breaks = 30)
quantile(range$range, probs = c(0.1, 0.5, 0.9, .95, .99), na.rm = TRUE)

# By country
range = data.frame(country = NA, n = NA, range = NA)

for (c in unique(spdf$country)) {
  # c <- "Sierra Leone"
  test = subset(spdf, country == c)
  if (dim(test)[1] > 115) {
    vv = variogram(res ~ 1, data = test, projection(FALSE))
    vv = subset(vv, dist > 0) # many obs of same location
    f = fit.variogram(vv, vgm("Sph"))
    range = rbind(range, c(c, dim(test)[1], f$range[2]))
  }
}

range = range %>% arrange(country) %>% mutate(range = as.numeric(range))
hist(range$range, breaks = 30)
quantile(range$range, probs = c(0.1, 0.5, 0.9, .95, .99), na.rm = TRUE)

################################################################################
# F: General correlation over time ----
################################################################################

# As detailed in D03 - Additional robustness.R, the panel is sufficiently unbalanced
# that estimating a distributed lag at monthly scale is likely not feasible. Instead, look across years.

complete_expanded <- complete %>%
  mutate(
    year = as.numeric(as.character(year)),
    month = as.character(month),
    month = match(month, month.abb)
  ) |>
  group_by(OBJECTID) %>%
  complete(year = 1902:2016, month = 1:12) %>%
  ungroup()

complete_with_lag <- complete_expanded %>%
  arrange(OBJECTID, year, month) %>%
  group_by(OBJECTID) %>%
  mutate(
    resmn = res,
    reslag1 = dplyr::lag(resmn, 1),
    reslag2 = dplyr::lag(resmn, 2),
    reslag3 = dplyr::lag(resmn, 3),
    reslag4 = dplyr::lag(resmn, 4),
    reslag5 = dplyr::lag(resmn, 5)
  ) |>
  tidyr::drop_na(resmn)

mn_lag1 <- lm(resmn ~ reslag1, data = complete_with_lag)

mn_lag2 <- lm(resmn ~ reslag1 + reslag2, data = complete_with_lag)

mn_lag3 <- lm(resmn ~ reslag1 + reslag2 + reslag3, data = complete_with_lag)

# Average residuals by ADM1-year
anndf = complete |>
  group_by(OBJECTID, yearnum) |>
  dplyr::summarize(resmn = mean(res, na.rm = TRUE), year = first(yearnum))

# Expand to be a full panel
anndf_ex <- anndf %>%
  group_by(OBJECTID) %>%
  complete(year = 1902:2016) %>%
  ungroup()

# Add lags
anndf_with_lag <- anndf_ex %>%
  arrange(OBJECTID, year) %>%
  mutate(
    reslag1 = lag(resmn, 1),
    reslag2 = lag(resmn, 2),
    reslag3 = lag(resmn, 3),
    reslag4 = lag(resmn, 4),
    reslag5 = lag(resmn, 5)
  ) |>
  tidyr::drop_na(resmn)

# Estimation
lag1 <- lm(resmn ~ reslag1, data = anndf_with_lag)

lag2 <- lm(resmn ~ reslag1 + reslag2, data = anndf_with_lag)

lag3 <- lm(resmn ~ reslag1 + reslag2 + reslag3, data = anndf_with_lag)

lag4 <- lm(resmn ~ reslag1 + reslag2 + reslag3 + reslag4, data = anndf_with_lag)

lag5 <- lm(
  resmn ~ reslag1 + reslag2 + reslag3 + reslag4 + reslag5,
  data = anndf_with_lag
)

mynote <- "Note"

stargazer(
  mn_lag1,
  mn_lag2,
  mn_lag3,
  lag1,
  lag2,
  lag3,
  lag4,
  lag5,
  title = "Model diagnostics: Residual lags",
  # align = TRUE,
  column.labels = c(
    "1 Mn",
    "2 Mn",
    "3 Mn",
    "1 Yr",
    "2 Yr",
    "3 Yr",
    "4 Yr",
    "5 Yr"
  ),
  covariate.labels = c(
    "Res. Lag 1",
    "Res. Lag 2",
    "Res. Lag 3",
    "Res. Lag 4",
    "Res. Lag 5"
  ),
  omit.stat = c("f", "ser"),
  digits = 2,
  # float = FALSE,
  type = "latex",
  notes.append = TRUE,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  out = file.path(
    table_diag_res_dir,
    "serial_correlation_in_model_residuals.tex"
  ),
  star.cutoffs = table_star_cutoffs
)


