#############################################################################-
#### Extract temperature and precipitation data from CRU-TS4.XX at point locations
#### This modified version extracts climate data at specific survey lat/lon points
#### rather than averaging across administrative units
#############################################################################-

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(sf, here, terra, tidyverse)

# Source configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)

sf::sf_use_s2(FALSE)

############################################################
# Prevalence data ----
############################################################

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

############################################################
# Extract temperature data ----
############################################################

# Read and process temperature raster data
tmp <- cru_tmp_fp |>
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

############################################################
# Extract precipitation data ----
############################################################

# Read and process precipitation raster data
pre <- cru_prc_fp |>
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

############################################################
# Combine tmp and prc data ----
############################################################

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

############################################################
# Flood and lag ----
############################################################

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

############################################################
# Save data ----
############################################################

complete_df <- tidyr::drop_na(complete_df, `PfPR2-10`)

readr::write_csv(complete_df, prev_clim_data_grid_fp)
