################################################################################
# This script prepares the climate and prevalence data for estimation. It
# calculates the drought and flood variables and makes the categorical variables
# into factors where necessary.
# - Note: The analysis ready data produced by this script can be downloaded from
#   https://zenodo.org/records/20399793
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

log_msg <- create_logger()

log_msg("Starting script `B04 - Join prev and CRU data.R`")

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
# Intermediate climate ADM1 ----
################################################################################

log_msg("  Loading intermediate climate data (ADM1)")

climate_data <- intermediate_CRU_adm1_fp |>
  arrow::read_feather() |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID), year = as.numeric(year))

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
  dplyr::distinct() |>
  readr::write_csv(file = precip_CRU_adm1_fp)

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

readr::write_rds(replication, file = analysis_ready_CRU_adm1_fp)
log_msg(sprintf(
  "  Saved replication dataset: %d rows, %d columns",
  nrow(replication),
  ncol(replication)
))

################################################################################
# Urban summary ----
################################################################################

log_msg("Computing urban summary")

urban_areas <- urban_fp |>
  sf::read_sf() |>
  dplyr::filter(GC_UCB_YOB_2025 <= 2015) |>
  sf::st_transform(4326) |>
  dplyr::select(GC_UCB_YOB_2025)

log_msg(sprintf("  Loaded %d urban centers", nrow(urban_areas)))

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
log_msg(sprintf("  Urban summary saved: %d rows", nrow(urban_summary)))

################################################################################
# Grid level data ----
################################################################################

log_msg("Processing grid level data")

################################################################################
# Prevalence data ----
################################################################################

log_msg("  Loading grid-level prevalence data")

prev_df <- data.table::fread(
  prev_DB_fp,
  select = list(
    double = c("Long", "Lat", "Pf", "PfPR2-10"),
    integer = c("MM", "YY")
  )
)

prev_df[, `:=`(
  month = factor(MM, levels = 1:12, labels = month.abb),
  year = as.character(YY)
)]

prev_df <- prev_df[,
  .(Pf = mean(Pf, na.rm = TRUE), PfPR2 = mean(`PfPR2-10`, na.rm = TRUE)),
  by = .(Lat, Long, month, year)
]

log_msg(sprintf(
  "  Grid prevalence data: %d unique location/time points",
  nrow(prev_df)
))

################################################################################
# Intermediate climate grid ----
################################################################################

log_msg("  Loading grid-level climate data")

climate_grid_data <- arrow::read_feather(intermediate_CRU_grid_fp) |>
  data.table::as.data.table()

log_msg(sprintf("  Grid climate data: %d rows", nrow(climate_grid_data)))

################################################################################
# Percentiles by point ----
################################################################################

log_msg("  Computing precipitation percentiles by grid point")

pctiles_dt <- climate_grid_data[
  if (!is.na(year_cutoff)) as.integer(year) <= year_cutoff else TRUE,
  .(
    ppt_pctile0.9 = quantile(ppt, pct_flood, na.rm = TRUE),
    ppt_pctile0.1 = quantile(ppt, pct_drought, na.rm = TRUE)
  ),
  by = point_id
]

data.table::fwrite(pctiles_dt, precip_CRU_grid_fp)
log_msg(sprintf("  Percentiles computed for %d grid points", nrow(pctiles_dt)))

################################################################################
# Flood and drought lag ----
################################################################################

log_msg("  Computing flood/drought indicators and lags for grid data")

# Update-join percentiles onto the full climate data
climate_grid_data[
  pctiles_dt,
  on = "point_id",
  `:=`(ppt_pctile0.9 = i.ppt_pctile0.9, ppt_pctile0.1 = i.ppt_pctile0.1)
]

# Flood/drought indicators and time ordering
climate_grid_data[, `:=`(
  flood = as.integer(ppt >= ppt_pctile0.9),
  drought = as.integer(ppt <= ppt_pctile0.1),
  yearnum = as.integer(year),
  monthnum = match(month, month.abb),
  monthyr = as.integer(year) * 12L + match(month, month.abb)
)]

data.table::setorder(climate_grid_data, point_id, monthyr)

# Lags on the full continuous series
climate_grid_data[,
  `:=`(
    flood.lag = shift(flood, 1L, type = "lag"),
    flood.lag2 = shift(flood, 2L, type = "lag"),
    flood.lag3 = shift(flood, 3L, type = "lag"),
    drought.lag = shift(drought, 1L, type = "lag"),
    drought.lag2 = shift(drought, 2L, type = "lag"),
    drought.lag3 = shift(drought, 3L, type = "lag")
  ),
  by = point_id
]

log_msg("  Flood and drought lags computed")

################################################################################
# join to prevalence ----
################################################################################

log_msg("  Joining grid climate to grid prevalence data")

prev_df[, year := as.character(year)]
climate_grid_data[, year := as.character(year)]

result <- climate_grid_data[
  prev_df,
  on = .(Lat, Long, month, year),
  nomatch = NULL
]

log_msg(sprintf("  Grid join complete: %d matched records", nrow(result)))
log_msg("  Adding spatial joins and creating factors")

result <- result |>
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = FALSE) |>
  sf::st_join(cont) |>
  dplyr::left_join(gbod, by = "country") |>
  sf::st_drop_geometry() |>
  dplyr::as_tibble() |>
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
    yr_bin = floor(yearnum / yr_bin_size) * yr_bin_size
  ) |>
  dplyr::group_by(country, yr_bin) |>
  dplyr::arrange(OBJECTID, monthyr) |>
  dplyr::mutate(cntry_yrbin = dplyr::cur_group_id()) |>
  dplyr::ungroup()

log_msg(sprintf(
  "  Created %d country-year bin groups for grid data",
  max(result$cntry_yrbin)
))

################################################################################
# Replication file save ----
################################################################################

log_msg("  Preparing and saving replication dataset (grid)")

location_cols <- c("region", "smllrgn", "country", "ISO", "OBJECTID")
time_cols <- c("monthyr", "monthyr2", "month", "year", "yearnum")
prev_cols <- c("PfPR2", "Pf")
temp_cols <- c("temp", "temp2", "temp3", "temp4", "temp5")
prec_cols <- c("ppt", "ppt2", "ppt3", "ppt4", "ppt5")
flood_cols <- c("flood", "flood.lag", "flood.lag2", "flood.lag3")
drought_cols <- c("drought", "drought.lag", "drought.lag2", "drought.lag3")

replication_grid <- result |>
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

readr::write_rds(replication_grid, file = analysis_ready_grid_fp)
log_msg(sprintf(
  "  Saved grid replication dataset: %d rows, %d columns",
  nrow(replication_grid),
  ncol(replication_grid)
))

log_msg("Script `B04 - Join prev and CRU data.R` completed successfully")

################################################################################
# End of file ----
################################################################################
