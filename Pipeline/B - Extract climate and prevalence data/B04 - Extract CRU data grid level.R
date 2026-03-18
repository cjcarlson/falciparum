#############################################################################-
#### Extract temperature and precipitation data from CRU-TS4.XX at point locations
#### This modified version extracts climate data at specific survey lat/lon points
#### rather than averaging across administrative units
#############################################################################-

rm(list = ls())

library(sf)
library(here)
library(terra)
library(tidyverse)

sf::sf_use_s2(FALSE)

# Load configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(pipeline_A_dir, "A01 - Utility code for calculations.R"))

# Function to process each power for point data
process_clim_powers_points <- function(
  power,
  clim_data,
  points_sf,
  rast_times,
  var_name
) {
  # power = 1
  # clim_data = tmp
  # points_sf = prev_sf
  # rast_times = time_names
  # var_name = "temp"
  cat("Processing power: ", power, "\n", sep = "")
  # Apply power transformation
  clim_data_k <- clim_data^power

  cat("\tExtracting values\n")
  # Extract values at point locations
  clim_extract_k <- terra::extract(
    x = clim_data_k,
    y = points_sf,
  )

  cat("\tAdding point IDs\n")
  # Add the point ID back
  clim_extract_k$point_id <- 1:nrow(points_sf)
  clim_extract_k$OBJECTID <- points_sf$OBJECTID

  cat("\tRenaming columns\n")
  # Rename columns
  colnames(clim_extract_k)[2:(ncol(clim_extract_k) - 2)] <- rast_times

  cat("\tReshaping data\n")
  # Reshape from wide to long format
  clim_extract_k <- clim_extract_k |>
    tidyr::pivot_longer(
      cols = -c(ID, point_id, OBJECTID),
      names_to = c("year", "month", "day"),
      names_sep = "-",
      values_to = paste0(var_name, ifelse(power == 1, "", power))
    ) |>
    dplyr::select(-day, -ID) |>
    dplyr::mutate(month = month.abb[as.numeric(month)])
  cat("\tDone\n\n")
  return(clim_extract_k)
}

# Read the prevalence CSV file first to get point locations
prev_df <- file.path(
  data_dir,
  "Data",
  'dataverse_files',
  '00 Africa 1900-2015 SSA PR database (260617).csv'
) |>
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
  dplyr::mutate(
    METHOD = str_to_upper(METHOD),
    point_id = row_number() # Add unique point identifier
  )

# Read continent shapefile for cropping rasters
cont <- sf::read_sf(here::here(data_dir, 'Data', 'AfricaADM1.shp')) |>
  dplyr::select(OBJECTID, geometry)

# Convert to sf object with POINT geometry
prev_sf <- sf::st_as_sf(
  prev_df,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE # Keep Long and Lat columns
) |>
  sf::st_join(cont)

# Read and process temperature raster data
tmp <- file.path(
  data_dir,
  "Data",
  "CRU_TS403_data",
  "tmp",
  "cru_ts4.03.1901.2018.tmp.dat.nc",
  "cru_ts4.03.1901.2018.tmp.dat.nc"
) |>
  terra::rast() |>
  terra::crop(cont) %>%
  terra::subset(grep("tmp_", names(.)))

time_names <- as.character(time(tmp))

# Define the powers to be applied
powers <- 1:5

# Apply the function to all powers for temperature
temp_extract_list <- lapply(
  powers,
  process_clim_powers_points,
  clim_data = tmp,
  points_sf = prev_sf,
  rast_times = time_names,
  var_name = "temp"
)

# Merge all temperature data frames into one
temp_df <- purrr::reduce(
  temp_extract_list,
  left_join,
  by = c("OBJECTID", "point_id", 'year', 'month')
)

# Read and process precipitation raster data
pre <- file.path(
  data_dir,
  "Data",
  "CRU_TS403_data",
  "pr",
  "cru_ts4.03.1901.2018.pre.dat.nc",
  "cru_ts4.03.1901.2018.pre.dat.nc"
) |>
  terra::rast() |>
  terra::crop(cont) %>%
  terra::subset(grep("pre_", names(.)))

time_names <- as.character(time(pre))

# Apply the function to all powers for precipitation
pre_extract_list <- lapply(
  powers,
  process_clim_powers_points,
  clim_data = pre,
  points_sf = prev_sf,
  rast_times = time_names,
  var_name = "ppt"
)

# Merge all precipitation data frames into one
pre_df <- purrr::reduce(
  pre_extract_list,
  left_join,
  by = c("OBJECTID", "point_id", 'year', 'month')
)

# Prepare prevalence data for joining
prev_join <- prev_df |>
  dplyr::select(
    point_id,
    Long,
    Lat,
    MM,
    YY,
    Pf,
    `PfPR2-10`,
    METHOD,
    everything()
  ) |>
  dplyr::mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = as.character(YY),
  )

# Join the temperature, precipitation, and prevalence data
complete_df <- dplyr::left_join(
  temp_df,
  pre_df,
  by = c("OBJECTID", "point_id", "year", "month")
) |>
  dplyr::left_join(
    prev_join,
    by = c("point_id", "year", "month")
  ) |>
  # Reorder columns for clarity
  dplyr::select(
    point_id,
    Long,
    Lat,
    year,
    month,
    MM,
    YY,
    starts_with("temp"),
    starts_with("ppt"),
    Pf,
    `PfPR2-10`,
    METHOD,
    everything()
  )
## ------------------------------------------------------------------------- ##
## Flag floods (>= 90 th pct.), droughts (<= 10 th pct.)
## and generate 1-, 2- and 3-month lags for every survey point
## ------------------------------------------------------------------------- ##

# > You can modify these if you want a different baseline or thresholds
pct_flood <- 0.90 # 90 th percentile ⇒ “flood”
pct_drought <- 0.10 # 10 th percentile ⇒ “drought”
year_cutoff <- NA # e.g. 2000 if you want climatology up to year 2000 only

pctiles_by_point <- complete_df |>
  mutate(yearnum = as.integer(year)) %>%
  {
    if (!is.na(year_cutoff)) {
      dplyr::filter(., yearnum <= year_cutoff)
    } else {
      .
    }
  } |>
  group_by(point_id) |>
  summarise(
    ppt_pctile0.9 = quantile(ppt, pct_flood, na.rm = TRUE),
    ppt_pctile0.1 = quantile(ppt, pct_drought, na.rm = TRUE),
    .groups = "drop"
  )

complete_df <- complete_df |>
  left_join(pctiles_by_point, by = "point_id") |>
  mutate(
    flood = as.integer(ppt >= ppt_pctile0.9),
    drought = as.integer(ppt <= ppt_pctile0.1),
    yearnum = as.integer(year),
    monthnum = match(month, month.abb),
    monthyr = yearnum * 12 + monthnum
  ) |>
  arrange(point_id, monthyr) |>
  group_by(point_id) |>
  mutate(
    flood_lag = lag(flood, 1),
    flood_lag2 = lag(flood, 2),
    flood_lag3 = lag(flood, 3),
    drought_lag = lag(drought, 1),
    drought_lag2 = lag(drought, 2),
    drought_lag3 = lag(drought, 3)
  ) |>
  ungroup()

complete_df <- tidyr::drop_na(complete_df, `PfPR2-10`)

readr::write_csv(
  complete_df,
  file.path(
    data_dir,
    "Data",
    paste0('CRU-', CRUversion, '-Points-Reextraction-May2025.csv')
  )
)
