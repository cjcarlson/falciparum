#############################################################################-
#### Use the following code to extract temperature and precipitation data from
#### the CRU-TS4.XX data set. The code uses the exactextractr package to extract
#### the data for each administrative unit. The code applies a power transformation
#### to the data and calculates the mean for each administrative unit. It then 
#### joins the data to the prevalence data and saves the final version in CSV format.
#### -----------------------------------------------------------------------. 
#### Written by: Cullen Molitor
#### Date: 2024-10-08
#### Email: cullen_molitor@ucsb.edu
#############################################################################-

rm(list = ls())

library(sf)
library(here)
library(terra)
library(tidyverse)
library(exactextractr)

sf::sf_use_s2(FALSE)

# Load configuration and utility functions
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(pipeline_A_dir, "A01 - Utility code for calculations.R"))

### ADD THIS FUNCTION TO A01 - Utility code for calculations.R
# Function to process each power
process_clim_powers <- function(
    power, 
    clim_data, 
    adm_data, 
    rast_times,
    var_name
) {
  
  clim_data_k <- clim_data ^ power
  
  clim_extract_k <- exactextractr::exact_extract(
    x = clim_data_k, y = adm_data, fun = 'mean', 
    progress = TRUE, append_cols = c("OBJECTID")
  )
  
  colnames(clim_extract_k) <- c("OBJECTID", rast_times)
  
  clim_extract_k <- clim_extract_k |>
    tidyr::pivot_longer(
      cols = -OBJECTID, 
      names_to = "date", 
      values_to = paste0(var_name, ifelse(power == 1, "", power))
    ) |> 
    tidyr::separate(date, into=c('year','month','day'), sep='-') |> 
    dplyr::select(-day) |>
    dplyr::mutate(month = month.abb[as.numeric(month)])
  
  return(clim_extract_k)
}

# Read shapefile
cont <- sf::read_sf(here::here(datadir, 'Data', 'AfricaADM1.shp'))

# Read and process raster data
tmp <- file.path(
  datadir, "Data", "CRU_TS403_data", "tmp", 
  "cru_ts4.03.1901.2018.tmp.dat.nc", 
  "cru_ts4.03.1901.2018.tmp.dat.nc") |> 
  terra::rast() |>
  terra::crop(cont) %>%
  terra::subset(grep("tmp_", names(.)))

time_names <- as.character(time(tmp))

# Define the powers to be applied
powers <- 1:5

# Apply the function to all powers and store in a list
temp_extract_list <- lapply(
  powers, 
  process_clim_powers, 
  clim_data = tmp, 
  adm_data = cont, 
  rast_times = time_names,
  var_name = "temp"
)

# Merge all data frames into one
temp_df <- purrr::reduce(
  temp_extract_list, 
  left_join, 
  by = c("OBJECTID", 'year','month')
)



# Read and process raster data
pre <- file.path(
  datadir, "Data", "CRU_TS403_data", "pr", 
  "cru_ts4.03.1901.2018.pre.dat.nc", 
  "cru_ts4.03.1901.2018.pre.dat.nc") |> 
  terra::rast() |>
  terra::crop(cont) %>% # need magrittr for dot notation in names()
  terra::subset(grep("pre_", names(.)))

time_names <- as.character(time(pre))

# Apply the function to all powers and store in a list
pre_extract_list <- lapply(
  powers, 
  process_clim_powers, 
  clim_data = pre, 
  adm_data = cont, 
  rast_times = time_names,
  var_name = "ppt"
)

# Merge all data frames into one
pre_df <- purrr::reduce(
  pre_extract_list, 
  left_join, 
  by = c("OBJECTID", 'year','month')
)

# Read the prevalence CSV file
prev_df <- file.path(
  datadir, 
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
  )

# Convert to sf object with POINT geometry
prev_sf <- sf::st_as_sf(
  prev_df, coords = c("Long", "Lat"), crs = 4326
) 

# Join the prevalence data to the continent shapefile
prev_with_cont <- sf::st_join(prev_sf, cont)

# Summarise the prevalence data to the ADM 1 level
aggregated_data <- prev_with_cont %>%
  tibble::as_tibble() %>%
  dplyr::select(OBJECTID, MM, YY, Pf, `PfPR2-10`, METHOD) %>%
  dplyr::mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) %>%
  dplyr::group_by(OBJECTID, year, month, METHOD) %>%
  dplyr::summarise(
    Pf = mean(Pf, na.rm = TRUE),
    PfPR2 = mean(`PfPR2-10`, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  dplyr::mutate(
    year = as.character(year),
    month = as.character(month))


# aggregated_data <- prev_with_cont %>%
#   as_tibble() %>%
#   # select(OBJECTID, MM, YY, Pf, `PfPR2-10`, METHOD) %>%
#   mutate(
#     month = factor(MM, levels = 1:12, labels = month.abb),
#     year = YY
#   ) %>%
#   group_by(OBJECTID, year, month) %>%
#   summarise( n_methods = n_distinct(METHOD)) |> 
#   dplyr::filter(n_methods > 1)


# Calculate Mean Pf and PfPR2-10 per ADM1, Year, Month
mean_data <- prev_with_cont %>%
  as_tibble() %>%
  dplyr::select(OBJECTID, MM, YY, Pf, `PfPR2-10`, METHOD) %>%
  dplyr::mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) %>%
  dplyr::group_by(OBJECTID, year, month) %>%
  dplyr::summarise(
    Pf_mean = mean(Pf, na.rm = TRUE),
    PfPR2_mean = mean(`PfPR2-10`, na.rm = TRUE),
    .groups = 'drop'
  )

# Determine Dominant Method per ADM1, Year, Month
dominant_method <- prev_with_cont %>%
  as_tibble() %>%
  dplyr::select(OBJECTID, MM, YY, `PfPR2-10`, METHOD) %>%
  dplyr::mutate(
    month = factor(MM, levels = 1:12, labels = month.abb),
    year = YY
  ) %>%
  dplyr::group_by(OBJECTID, year, month, METHOD) %>%
  dplyr::summarise(
    PfPR2_sum = sum(`PfPR2-10`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  dplyr::group_by(OBJECTID, year, month) %>%
  dplyr::slice_max(order_by = PfPR2_sum, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(OBJECTID, year, month, dominant_METHOD = METHOD)

readr::write_csv(
  dominant_method, 
  file.path(
    datadir, 
    "Data", 
    paste0('dominant_diagnostic_method_summary.csv')
  )
)

# Merge Mean Data with Dominant Method
aggregated_data <- mean_data %>%
  left_join(dominant_method, by = c("OBJECTID", "year", "month")) %>%
  mutate(
    year = as.character(year),
    month = as.character(month)
  )

# Join the temperature, precipitation, and prevalence data
complete_df <- dplyr::left_join(
  temp_df, 
  pre_df, 
  by = c("OBJECTID", "year", "month")
) |> 
  dplyr::left_join(
    aggregated_data, 
    by = c("OBJECTID", "year", "month")
  )

# Save the complete data frame
readr::write_csv(
  complete_df, 
  file.path(
    datadir, 
    "Data", 
    paste0('CRU-', CRUversion, '-Reextraction-Oct2024.csv')
  )
)
