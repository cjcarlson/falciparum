#############################################################################-
# Title       : Climate × Urbanization Interaction Analysis
# Description : Load, process, and analyze PfPR2-10 prevalence data with urbanization
#               interactions; fit fixed-effects models and generate sensitivity plots.
# Author      : Cullen Molitor
# Email       : cullen_molitor@ucsb.edu
# Date        : 2025-05-01
#############################################################################-

#-------------------------------------------------------------------------------
# 0. Setup environment
#-------------------------------------------------------------------------------

# clear workspace
rm(list = ls())

# use pacman for package management
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  sf,
  zoo,
  lfe,
  here,
  terra,
  knitr,
  broom,
  reshape,
  cowplot,
  multcomp,
  lubridate,
  tidyverse,
  stargazer,
  patchwork,
  exactextractr
)

# ensure s2 geometry behavior as desired
sf::sf_use_s2(FALSE)


#-------------------------------------------------------------------------------
# 1. Configuration & utility functions
#-------------------------------------------------------------------------------

# Load config and helper functions
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(pipeline_A_dir, "A01 - Utility code for calculations.R"))
source(here::here(pipeline_A_dir, "A02 - Utility code for plotting.R"))


#-------------------------------------------------------------------------------
# 2. Data loading
#-------------------------------------------------------------------------------

# Move this code to B01 - Extract CRU data and temperature for model.R
# after other branch is merged

## 2.1 Read continent shapefile
cont <- sf::read_sf(here::here(datadir, 'Data', 'AfricaADM1.shp'))

## 2.2 Read prevalence CSV
prev_df <- file.path(
  datadir,
  "Data",
  'dataverse_files',
  '00 Africa 1900-2015 SSA PR database (260617).csv'
) %>%
  readr::read_csv(
    col_types = cols(
      Long = col_double(),
      Lat = col_double(),
      MM = col_integer(),
      YY = col_integer(),
      Pf = col_double(),
      `PfPR2-10` = col_double()
    )
  ) %>%
  mutate(METHOD = str_to_upper(METHOD))

## 2.3 Convert to sf and join urban areas
prev_sf <- st_as_sf(prev_df, coords = c("Long", "Lat"), crs = 4326)

urban_areas <- here::here(
  datadir,
  "Data",
  "GHS_UCDB_REGION_SUB_SAHARAN_AFRICA_R2024A.gpkg"
) %>%
  read_sf() %>%
  filter(GC_UCB_YOB_2025 <= 2015) %>%
  st_transform(4326)

prev_with_yob <- st_join(
  prev_sf,
  urban_areas %>% select(GC_UCB_YOB_2025),
  join = st_within,
  left = TRUE
)

prev_classified <- prev_with_yob %>%
  mutate(
    urban = case_when(
      !is.na(GC_UCB_YOB_2025) & YY >= GC_UCB_YOB_2025 ~ 1,
      is.na(GC_UCB_YOB_2025) & YY >= 1975 ~ 0,
      TRUE ~ NA_integer_
    )
  )

## 2.4 Join to continent shapefile
prev_with_cont <- st_join(prev_classified, cont)


#-------------------------------------------------------------------------------
# 3. Aggregation & summaries
#-------------------------------------------------------------------------------

## 3.1 Compute mean prevalence and urban share
mean_data <- prev_with_cont %>%
  as_tibble() %>%
  select(
    OBJECTID,
    MM,
    YY,
    Pf,
    `PfPR2-10`,
    METHOD,
    GC_UCB_YOB_2025,
    urban
  ) %>%
  mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) %>%
  group_by(OBJECTID, year, month) %>%
  summarise(
    n_methods = n_distinct(METHOD),
    n_urban = sum(urban, na.rm = TRUE),
    n_surveys = n(),
    avg_urban = n_urban / n_surveys,
    Pf_mean = mean(Pf, na.rm = TRUE),
    PfPR2_mean = mean(`PfPR2-10`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    n_urban = ifelse(year < 1975, NA_integer_, n_urban),
    avg_urban = ifelse(year < 1975, NA_real_, avg_urban)
  )

urban_summary <- mean_data %>%
  select(OBJECTID, year, month, n_urban, n_surveys, avg_urban, n_methods)

## 3.2 Determine dominant diagnostic method
dominant_method <- prev_with_cont %>%
  as_tibble() %>%
  select(OBJECTID, MM, YY, `PfPR2-10`, METHOD) %>%
  mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) %>%
  group_by(OBJECTID, year, month, METHOD) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(OBJECTID, year, month) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(OBJECTID, year, month, dominant_METHOD = METHOD) %>%
  mutate(
    simplified_METHOD = case_when(
      dominant_METHOD %in%
        c("RDT", "RDT/SLIDE CONFIRMED", "RDT/PCR CONFIRMED") ~
        "RDT",
      dominant_METHOD %in% c("MICROSCOPY", "MICROSCOPY/PCR CONFIRMED") ~
        "MICROSCOPY",
      TRUE ~ dominant_METHOD
    )
  ) %>%
  left_join(urban_summary, by = c("OBJECTID", "year", "month"))

readr::write_csv(
  dominant_method,
  file.path(datadir, "Data", 'dominant_diagnostic_method_summary.csv')
)

# End Move Here

aggregated_data <- mean_data %>%
  left_join(
    dominant_method,
    by = join_by(
      OBJECTID,
      year,
      month,
      n_methods,
      n_urban,
      n_surveys,
      avg_urban
    )
  ) %>%
  mutate(year = as.character(year), month = as.character(month))


#-------------------------------------------------------------------------------
# 4. Data cleaning & prep for estimation
#-------------------------------------------------------------------------------

source(here::here(pipeline_A_dir, "A03 - Prep data for estimation.R"))

complete <- complete %>%
  drop_na(n_urban) |>
  mutate(urban_dummy = ifelse(n_urban > 0, 1, 0))


#-------------------------------------------------------------------------------
# 5. Exploratory plot
#-------------------------------------------------------------------------------

ggplot(data = complete, aes(x = urban_dummy)) +
  geom_histogram(bins = 50)


#-------------------------------------------------------------------------------
# 6. Model estimation
#-------------------------------------------------------------------------------

## 6.1 Main specification formula
cXt2intrXm <- as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + flood + flood.lag + flood.lag2 + flood.lag3",
    " + drought + drought.lag + drought.lag2 + drought.lag3",
    " + I(intervention) + country:monthyr + country:monthyr2",
    " | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)

model <- felm(data = complete, formula = cXt2intrXm)

## 6.2 Interaction model
model_int <- felm(
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
      OBJECTID,
  data = complete
)

interaction_table <- model_int %>%
  tidy() %>%
  filter(
    str_detect(term, "urban") |
      str_detect(term, "temp") |
      str_detect(term, "flood") |
      str_detect(term, "drought")
  ) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate(term = str_replace(term, "avg_urban:", "avg_urban × "))


#-------------------------------------------------------------------------------
# 7. Results table
#-------------------------------------------------------------------------------

kable(
  interaction_table,
  caption = "Climate × Urbanization interaction effects on PfPR2",
  digits = c(NA, 3, 3, 2, 3),
  align = c("l", "r", "r", "r", "r")
)


#-------------------------------------------------------------------------------
# 8. Polynomial & lagged effects plots
#-------------------------------------------------------------------------------

Tref <- 25 # reference temperature for recentering
Tmin <- 10
Tmax <- 40

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
  showYTitle = TRUE
) +
  theme(plot.title = element_text(size = 10))

d <- plotLinearLags_urban(
  mod = model_int,
  patternForPlotVars = "drought",
  cluster = TRUE,
  laglength = 3,
  xLab = "Drought Lag",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-5, 8)
)

f <- plotLinearLags_urban(
  mod = model_int,
  patternForPlotVars = "flood",
  cluster = TRUE,
  laglength = 3,
  xLab = "Flood Lag",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-5, 8)
)

combined_plot <- t +
  d +
  f +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0))


#-------------------------------------------------------------------------------
# 9. Save final figure
#-------------------------------------------------------------------------------

ggsave(
  file.path(
    datadir,
    "Results",
    "Figures",
    "Diagnostics",
    "Fixed_effects",
    "urban_sensitivity.jpg"
  ),
  plot = combined_plot,
  width = 7,
  height = 2.5,
  units = "in",
  dpi = 300
)
