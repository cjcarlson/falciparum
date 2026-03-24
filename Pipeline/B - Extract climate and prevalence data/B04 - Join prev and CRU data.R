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
pacman::p_load(here, sf, tidyverse, lubridate, zoo)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

sf::sf_use_s2(FALSE)

########################################################################
# Spatial data ----
########################################################################

# Define Global Burden of Disease regions
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
# Administrative units level 1
cont <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::mutate(
    OBJECTID = as.numeric(OBJECTID)
  ) |>
  dplyr::select(OBJECTID, country = NAME_0)

########################################################################
# Intermediate climate data ----
########################################################################

climate_data <- readr::read_csv(intermediate_CRU_fp)

########################################################################
# Prevalence data ----
########################################################################

# Load the raw prevalence data
prev_df <- prev_DB_fp |>
  readr::read_csv(
    col_types = readr::cols(
      Long = col_double(),
      Lat = col_double(),
      MM = col_integer(),
      YY = col_integer(),
      Pf = col_double(),
      `PfPR2-10` = col_double()
    )
  ) |>
  dplyr::mutate(METHOD = str_to_upper(METHOD)) |>
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = FALSE) |>
  sf::st_join(cont) |>
  sf::st_drop_geometry() |>
  dplyr::as_tibble()

# Find the dominant diagnostic method for each year, month, and ADM1
diagnostic_method_summary <- prev_df |>
  dplyr::select(OBJECTID, MM, YY, `PfPR2-10`, METHOD) |>
  dplyr::mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) |>
  dplyr::group_by(OBJECTID, year, month, METHOD) |>
  dplyr::summarise(count = n(), .groups = 'drop') |>
  dplyr::group_by(OBJECTID, year, month) |>
  dplyr::slice_max(order_by = count, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(OBJECTID, year, month, dominant_METHOD = METHOD) |>
  dplyr::mutate(
    simplified_METHOD = case_when(
      dominant_METHOD %in%
        c("RDT", "RDT/SLIDE CONFIRMED", "RDT/PCR CONFIRMED") ~
        "RDT",
      dominant_METHOD %in% c("MICROSCOPY", "MICROSCOPY/PCR CONFIRMED") ~
        "MICROSCOPY",
      TRUE ~ dominant_METHOD
    )
  )

# Summarise the prevalence data to the ADM 1 level
prev_mean_df <- prev_df |>
  dplyr::select(OBJECTID, MM, YY, Pf, `PfPR2-10`, METHOD) |>
  dplyr::mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) |>
  dplyr::group_by(OBJECTID, year, month) |>
  dplyr::summarise(
    Pf = mean(Pf, na.rm = TRUE),
    PfPR2 = mean(`PfPR2-10`, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID)) |>
  dplyr::left_join(
    diagnostic_method_summary,
    by = join_by(OBJECTID, year, month)
  )

########################################################################
# Join clim and prev ----
########################################################################

clim_prev_full_df <- climate_data |>
  dplyr::left_join(prev_mean_df, by = c("OBJECTID", "year", "month")) |>
  dplyr::left_join(sf::st_drop_geometry(cont), by = dplyr::join_by(OBJECTID)) |>
  dplyr::mutate(yearnum = year, year = factor(year))

########################################################################
# Subset to complete records ----
########################################################################

data.reset <- clim_prev_full_df |>
  tidyr::unite("monthyr", month:year, sep = ' ', remove = FALSE) |>
  dplyr::mutate(monthyr = as.Date(as.yearmon(monthyr))) |>
  dplyr::mutate(monthyr = as.numeric(ymd(monthyr) - ymd("1900-01-01")))

complete <- data.reset[complete.cases(data.reset), ]

########################################################################
# Drought and flood ----
########################################################################

##### Define flood/drought variables - need to pass the climate data 
##### separately from the merged dataset with the outcome
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

########################################################################
# Clean variables ----
########################################################################

complete <- complete |>
  dplyr::left_join(gbod, by = "country") |>
  dplyr::mutate(
    monthyr2 = monthyr^2,
    intervention = dplyr::case_when(
      dplyr::between(yearnum, 1955, 1969) ~ 1,
      dplyr::between(yearnum, 2000, 2015) ~ 2,
      TRUE ~ 0
    ),
    intervention = as.factor(intervention),
    month = as.factor(month),
    year = as.factor(year),
    country = as.factor(country),
    simplified_METHOD = as.factor(simplified_METHOD),
    dominant_METHOD = as.factor(dominant_METHOD),
    yr_bin = floor(yearnum / yr_bin_size) * yr_bin_size
  ) |>
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

urban_areas <- urban_centers_fp |>
  sf::read_sf() |>
  dplyr::filter(GC_UCB_YOB_2025 <= 2015) |>
  sf::st_transform(4326) |>
  dplyr::select(GC_UCB_YOB_2025)

## Compute urban dummy
urban_summary <- prev_df |>
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326) |>
  sf::st_join(urban_areas, join = st_within, left = TRUE) |>
  dplyr::mutate(
    urban = dplyr::case_when(
      !is.na(GC_UCB_YOB_2025) & YY >= GC_UCB_YOB_2025 ~ 1,
      is.na(GC_UCB_YOB_2025) & YY >= 1975 ~ 0,
      TRUE ~ NA_integer_
    ),
    month = factor(MM, levels = 1:12, labels = month.abb)
  ) |>
  dplyr::as_tibble() |>
  dplyr::group_by(OBJECTID, month, year = YY) |>
  dplyr::summarise(n_urban = sum(urban, na.rm = TRUE), .groups = 'drop') |>
  dplyr::mutate(
    n_urban = ifelse(year < 1975, NA_integer_, n_urban),
    urban_dummy = ifelse(n_urban > 0, 1, 0)
  ) |>
  dplyr::select(OBJECTID, year, month, urban_dummy)

readr::write_csv(urban_summary, urban_summary_fp)

# complete <- readr::read_rds(replication_fp)

# cols_to_check <- c("PfPR2", "temp", "temp2", "ppt")

# for (col in cols_to_check) {
#   result <- all.equal(complete[[col]], replication[[col]], tolerance = 1e-8)
#   cat(col, ":", if (isTRUE(result)) "MATCH" else result, "\n")
# }