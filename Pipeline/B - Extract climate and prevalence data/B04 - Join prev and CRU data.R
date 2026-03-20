############################################################
# This script prepares the climate and prevalence data for
# estimation. It calculates the drought and flood variables
# and makes the categorical variables into factors where
# necessary.
############################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  here,
  sf,
  tidyverse,
  lubridate,
  zoo
  # ,
  # reshape,
  # stargazer,
  # cowplot,
  # multcomp,
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

########################################################################
# Spatial data ----
# gbod = Define Global Burden of Disease regions
# cont = Administrative units level 1
########################################################################

gbod <- world_regions_fp |>
  sf::read_sf() |>
  as.data.frame() |>
  dplyr::select("ISO", "NAME_0", "Region", "SmllRgn") |>
  dplyr::group_by(ISO, NAME_0) |>
  # note that the small regions are homogenous within country
  dplyr::summarize(Region = first(Region), SmllRgn = first(SmllRgn)) |>
  dplyr::ungroup() |>
  dplyr::rename(
    "country" = "NAME_0",
    "region" = "Region",
    "smllrgn" = "SmllRgn"
  ) |>
  dplyr::mutate(
    country = as.character(country),
    country = str_replace(country, "Cote D'Ivoire", "Côte d'Ivoire")
  )

cont <- ADM1_fp |>
  sf::st_read() |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

########################################################################
# Prevalence data ----
########################################################################

data <- readr::read_csv(prev_clim_data_adm1_fp, show_col_types = FALSE) 

#### Spatial data
spatial <- read.csv(file.path(
  data_dir,
  'Dataframe backups',
  'shapefile-backup.csv'
))
countrydf <- unique(spatial[, c('OBJECTID', 'NAME_0')])
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x) {
  which(countrydf$OBJECTID == x)
})]
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x) {
  which(countrydf$OBJECTID == x)
})]
# iso = data %>% group_by(country, month, year) %>% summarize_all(mean, na.rm=T)
# data_iso <- iso[complete.cases(iso),]

#### Dates & times
data$yearnum <- data$year
data$year <- factor(data$year)

data.reset <- data %>%
  unite("monthyr", month:year, sep = ' ', remove = FALSE) %>%
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>%
  mutate(monthyr = as.numeric(ymd(monthyr) - ymd("1900-01-01"))) 

cols_to_check <- setdiff(names(data.reset), c("avg_urban", "n_urban"))
## Store complete records only
complete <- data.reset[complete.cases(data.reset[, cols_to_check]), ]

complete$country = as.character(complete$country)
complete = left_join(complete, gbod, by = "country")
complete$country = as.factor(complete$country)

# data <- prev_clim_data_adm1_fp |>
#   readr::read_csv(show_col_types = FALSE) |>
#   dplyr::left_join(
#     cont |> sf::st_drop_geometry() |> dplyr::distinct(OBJECTID, NAME_0),
#     by = "OBJECTID"
#   ) |>
#   dplyr::rename(country = NAME_0) |>
#   tidyr::unite("monthyr", month:year, sep = ' ', remove = FALSE) |>
#   dplyr::mutate(
#     yearnum = year,
#     year = factor(year),
#     monthyr = as.Date(zoo::as.yearmon(monthyr)),
#     monthyr = as.numeric(lubridate::ymd(monthyr) - lubridate::ymd("1900-01-01"))
#   )
# 
# complete <- data |>
#   dplyr::left_join(gbod, by = "country") |>
#   dplyr::mutate(country = as.factor(country))

########################################################################
# Define covariates ----
# a) Drought/flood events
# b) Malaria intervention periods
########################################################################

##### Define flood/drought variables - need to pass the climate data separately from the merged dataset with the outcome
##### variable because we want to define climate over the whole period
complete <- computePrcpExtremes(
  dfclimate = data.reset,
  dfoutcome = complete,
  pctdrought = 0.10,
  pctflood = 0.90,
  yearcutoff = NA
)
complete <- complete |> arrange(OBJECTID, monthyr)

complete |>
  dplyr::select(OBJECTID, ppt_pctile0.1, ppt_pctile0.9) |>
  distinct() |>
  write_csv(file = precip_fp)

# need this for country specific quadratic trends
complete$monthyr2 = complete$monthyr^2

# define key intervention periods
complete$intervention = ifelse(
  complete$yearnum >= 1955 & complete$yearnum <= 1969,
  1,
  0
)
complete$intervention[complete$yearnum >= 2000 & complete$yearnum <= 2015] = 2
complete$intervention = as.factor(complete$intervention)

# classes: important for ensuring felm is treating these correctly
complete$month = as.factor(complete$month)
complete$year = as.factor(complete$year)

# Build the country × N-year clustering variable
complete <- complete |>
  dplyr::mutate(yr_bin = floor(yearnum / yr_bin_size) * yr_bin_size) |>
  dplyr::group_by(country, yr_bin) |>
  dplyr::arrange(OBJECTID, monthyr) |> 
  dplyr::mutate(cntry_yrbin = dplyr::cur_group_id()) |>
  dplyr::ungroup()

########################################################################
# Replication file save ----
########################################################################

location_cols <- c("region", "smllrgn", "country", "ISO", "OBJECTID")
time_cols <- c("monthyr", "monthyr2", "month", "year", "yearnum")
prev_cols <- c("PfPR2", "Pf")
temp_cols <- c("temp", "temp2", "temp3", "temp4", "temp5")
prec_cols <- c("ppt", "ppt2", "ppt3", "ppt4", "ppt5")
flood_cols <- c("flood", "flood.lag", "flood.lag2", "flood.lag3")
drought_cols <- c("drought", "drought.lag", "drought.lag2", "drought.lag3")

replication <- complete |>
  dplyr::select(
    all_of(location_cols),
    all_of(time_cols),
    all_of(prev_cols),
    all_of(temp_cols),
    all_of(prec_cols),
    all_of(flood_cols),
    all_of(drought_cols),
    intervention,
    everything()
  )

readr::write_rds(replication, file = replication_fp)

############################################################
# Urban summary ----
############################################################

## Read prevalence CSV
prev_df <- prev_DB_fp %>%
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

## Convert to sf and join urban areas
prev_sf <- st_as_sf(prev_df, coords = c("Long", "Lat"), crs = 4326)

urban_areas <- here::here(
  data_dir,
  "Data",
  "GHS_UCDB_REGION_SUB_SAHARAN_AFRICA_R2024A.gpkg"
) %>%
  read_sf() %>%
  filter(GC_UCB_YOB_2025 <= 2015) %>%
  st_transform(4326)

prev_with_yob <- st_join(
  prev_sf,
  urban_areas %>% dplyr::select(GC_UCB_YOB_2025),
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

## Join to continent shapefile

prev_with_cont <- st_join(prev_classified, cont)

## Compute mean prevalence and urban share
urban_summary <- prev_with_cont %>%
  as_tibble() %>%
  dplyr::select(
    OBJECTID,
    MM,
    YY,
    Pf,
    `PfPR2-10`,
    METHOD,
    GC_UCB_YOB_2025,
    urban
  ) %>%
  dplyr::mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) %>%
  dplyr::group_by(OBJECTID, year, month) %>%
  dplyr::summarise(
    n_methods = n_distinct(METHOD),
    n_urban = sum(urban, na.rm = TRUE),
    n_surveys = n(),
    avg_urban = n_urban / n_surveys,
    Pf_mean = mean(Pf, na.rm = TRUE),
    PfPR2_mean = mean(`PfPR2-10`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(
    n_urban = ifelse(year < 1975, NA_integer_, n_urban),
    avg_urban = ifelse(year < 1975, NA_real_, avg_urban)
  ) %>%
  dplyr::select(OBJECTID, year, month, n_urban, n_surveys, avg_urban, n_methods)

########################################################################
# Diagnostic methods ----
########################################################################

## Determine dominant diagnostic method
dominant_method <- prev_with_cont %>%
  as_tibble() %>%
  dplyr::select(OBJECTID, MM, YY, `PfPR2-10`, METHOD) %>%
  dplyr::mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) %>%
  dplyr::group_by(OBJECTID, year, month, METHOD) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  dplyr::group_by(OBJECTID, year, month) %>%
  dplyr::slice_max(order_by = count, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(OBJECTID, year, month, dominant_METHOD = METHOD) %>%
  dplyr::mutate(
    simplified_METHOD = case_when(
      dominant_METHOD %in%
        c("RDT", "RDT/SLIDE CONFIRMED", "RDT/PCR CONFIRMED") ~
        "RDT",
      dominant_METHOD %in% c("MICROSCOPY", "MICROSCOPY/PCR CONFIRMED") ~
        "MICROSCOPY",
      TRUE ~ dominant_METHOD
    )
  ) %>%
  dplyr::left_join(urban_summary, by = c("OBJECTID", "year", "month"))

readr::write_csv(
  dominant_method,
  file.path(data_dir, "Data", 'dominant_diagnostic_method_summary.csv')
)

# complete$dominant_METHOD = as.factor(complete$dominant_METHOD)
# complete$simplified_METHOD = as.factor(complete$simplified_METHOD)

# unique(complete$dominant_METHOD)
# unique(complete$simplified_METHOD)
