################################################################################
# This script prepares the climate and prevalence data for estimation. It 
# calculates the drought and flood variables and makes the categorical variables 
# into factors where necessary.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(here, sf, tidyverse, lubridate, zoo, data.table)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

sf::sf_use_s2(FALSE)

################################################################################
# Set up logging ----
################################################################################

log_file_path <- file.path(logs_dir, "B06_join_prev_era5.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script `B06 - Join prev and ERA5 data.R`")

################################################################################
# Spatial data ----
################################################################################

log_msg("Loading spatial data")

# Global Burden of Disease regions
gbod <- gbd_fp |>
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
log_msg(sprintf(
  "  Loaded GBD regions: %d unique countries",
  length(unique(gbod$country))
))

# Administrative units level 1
cont <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::mutate(
    OBJECTID = as.numeric(OBJECTID)
  ) |>
  dplyr::select(OBJECTID, country = NAME_0)

log_msg(sprintf("  Loaded ADM1 units: %d regions", nrow(cont)))

################################################################################
# ADM1 level data ----
################################################################################

log_msg("Processing ADM1 level data")

################################################################################
# Intermediate climate ADM1 ----
################################################################################

log_msg("  Loading intermediate climate data (ADM1)")

climate_data <- intermediate_ERA_adm1_fp |>
  readr::read_csv(show_col_types = FALSE)

log_msg(sprintf("  Climate data loaded: %d rows", nrow(climate_data)))

################################################################################
# Prevalence data ----
################################################################################

log_msg("  Loading and processing prevalence data")

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
    ),
    show_col_types = FALSE
  ) |>
  dplyr::mutate(METHOD = str_to_upper(METHOD)) |>
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = FALSE) |>
  sf::st_join(cont) |>
  sf::st_drop_geometry() |>
  dplyr::as_tibble()

log_msg(sprintf("  Prevalence data loaded: %d observations", nrow(prev_df)))

log_msg("  Computing dominant diagnostic methods by ADM1/year/month")

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

log_msg("  Summarizing prevalence data to ADM1 level")

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

log_msg(sprintf(
  "  Prevalence summarized: %d ADM1/month/year combinations",
  nrow(prev_mean_df)
))

################################################################################
# Join clim and prev ----
################################################################################

log_msg("  Joining climate and prevalence data")

clim_prev_full_df <- climate_data |>
  dplyr::left_join(prev_mean_df, by = c("OBJECTID", "year", "month")) |>
  dplyr::left_join(sf::st_drop_geometry(cont), by = dplyr::join_by(OBJECTID)) |>
  dplyr::mutate(yearnum = year, year = factor(year))

log_msg(sprintf(
  "  Climate and prevalence joined: %d rows",
  nrow(clim_prev_full_df)
))

################################################################################
# Subset to complete records ----
################################################################################

log_msg("  Subsetting to complete records")

data.reset <- clim_prev_full_df |>
  tidyr::unite("monthyr", month:year, sep = ' ', remove = FALSE) |>
  dplyr::mutate(monthyr = as.Date(as.yearmon(monthyr))) |>
  dplyr::mutate(monthyr = as.numeric(ymd(monthyr) - ymd("1900-01-01")))

complete <- data.reset[complete.cases(data.reset), ]

log_msg(sprintf(
  "  Complete cases: %d rows (%.1f%% of total)",
  nrow(complete),
  100 * nrow(complete) / nrow(data.reset)
))

################################################################################
# Drought and flood ----
################################################################################

log_msg("  Computing drought and flood variables")

##### Define flood/drought variables - need to pass the climate data
##### separately from the merged dataset with the outcome
##### variable because we want to define climate over the whole period
complete <- computePrcpExtremes(
  dfclimate = data.reset,
  dfoutcome = complete,
  pctdrought = pct_drought,
  pctflood = pct_flood,
  yearcutoff = year_cutoff
)
complete <- complete |> arrange(OBJECTID, monthyr)

log_msg("  Saving precipitation percentiles to file")
complete |>
  dplyr::select(OBJECTID, ppt_pctile0.1, ppt_pctile0.9) |>
  distinct() |>
  write_csv(file = precip_ERA5_adm1_fp)

################################################################################
# Clean variables ----
################################################################################

log_msg("  Cleaning and creating factor variables")

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

log_msg(sprintf(
  "  Created %d country-year bin groups",
  max(complete$cntry_yrbin)
))

################################################################################
# Replication file save ----
################################################################################

log_msg("  Preparing and saving replication dataset (ADM1)")

location_cols <- c("region", "smllrgn", "country", "ISO", "OBJECTID")
time_cols <- c("monthyr", "monthyr2", "month", "year", "yearnum")
prev_cols <- c("PfPR2", "Pf")
temp_cols <- c("temp", "temp2" ) # , "temp3", "temp4", "temp5"
prec_cols <- c("ppt") # , "ppt2", "ppt3", "ppt4", "ppt5"
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

readr::write_rds(replication, file = analysis_ready_ERA5_adm1_fp)

log_msg(sprintf(
  "  Saved replication dataset: %d rows, %d columns",
  nrow(replication),
  ncol(replication)
))

log_msg("Script `B06 - Join prev and ERA5 data.R` completed successfully")
