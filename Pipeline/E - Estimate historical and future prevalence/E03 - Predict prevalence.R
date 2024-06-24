#### Setup ----
library(zoo)
library(here)
library(terra)
library(tictoc)
library(tidyverse)
library(lubridate)
# library(vroom)
library(foreach)
library(doParallel)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(pipeline_A_dir, "A01 - Utility code for calculations.R"))

# mode <- "historical"
mode <- "future"
overwrite <- TRUE

#### Spatial data ----
countrydf <- here::here(datadir, 'Data', 'AfricaADM1.shp') |> 
  sf::read_sf() |> 
  tibble::as_tibble() |>
  dplyr::select(OBJECTID, NAME_0) |> 
  dplyr::distinct() |> 
  dplyr::rename(country = NAME_0) |> 
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

gboddf <- file.path(datadir, 'Data', "OriginalGBD", "WorldRegions.shp") |> 
  sf::read_sf() |> 
  tibble::as_tibble() |>
  dplyr::select("ISO", "NAME_0", "SmllRgn") |> 
  dplyr::group_by(ISO, NAME_0) |> 
  dplyr::summarize(SmllRgn = first(SmllRgn)) |>
  dplyr::rename(country = "NAME_0", region = "SmllRgn") |>
  dplyr::mutate(country = gsub('Cote D\'Ivoire','C?te d\'Ivoire', country))

#### Precipitation thresholds ----
precip.key <- here::here("Climate", "PrecipKey.csv") |> 
  readr::read_csv(show_col_types=FALSE)

if (mode == "historical") {
  idx <- c(1, 2)
  start_date <- lubridate::ymd("1900-01-01")
  results_dir <- file.path(datadir, "IterationFiles", "HistoricalTempFiles")
} else {
  idx <- c(3, 4, 5)
  start_date <- lubridate::ymd("2015-01-01")
  results_dir <- file.path(datadir, "IterationFiles", "FutureTempFiles")
}

if (!dir.exists(results_dir)) {dir.create(results_dir, recursive = TRUE)}

files <- vector("character")

for (dir in scenarios[idx]) {
  files <- c(
    files, list.files(file.path(datadir, "Climate", dir), full.names = TRUE)
  )
}

#### Climate Scenario data ----
data <- readr::read_csv(files, show_col_types=FALSE, id="run") |> 
  tidyr::drop_na() |> 
  dplyr::mutate(
    scenario = basename(dirname(run)),
    model = tools::file_path_sans_ext(basename(run))
  ) |> 
  dplyr::select(-run) |> 
  dplyr::left_join(countrydf, by=c("OBJECTID")) |> 
  dplyr::left_join(gboddf, by=c("country")) |> 
  dplyr::filter(
    !region %in% c('Asia (Southeast)', 'North Africa & Middle East'),
    OBJECTID %in% unique(precip.key$OBJECTID)
  ) |> 
  tidyr::unite("monthyr", month:year, sep=' ', remove=FALSE) |>
  dplyr::mutate(
    monthyr = as.Date(zoo::as.yearmon(monthyr)),
    monthyr = as.numeric(lubridate::ymd(monthyr)-start_date)
  ) |>
  dplyr::select(
    scenario, model, region, ISO, country,
    OBJECTID, month, year, monthyr, temp, temp2, ppt
  )

#### Bootstrap coefficients ----
bootstrap <- file.path(
  datadir, "Results", "Models", "block_bootstrap_cXt2intrXm.rds"
) |> 
  readRDS() |> 
  tibble::as_tibble() 

numCores <- 50
cl <- makeCluster(numCores, outfile="")
registerDoParallel(cl)

#### Apply model coefficients ----
tictoc::tic()
# for (i in c(1:1000)) { # i <- 1
foreach(
  i = 1:1000, .export = c("results_dir", "data", "precip.key", "bootstrap")
) %dopar% {
  tictoc::tic()
  
  # file_name <- paste0("iter_", i, ".csv")
  file_name <- paste0("iter_", i, ".feather")
  file_path <- file.path(results_dir, file_name)
  meta_path <- file.path(results_dir, "RowMetadata.feather")
  
  if (!file.exists(file_path) | overwrite) {
    coef <- bootstrap[i,]
    
    iter.df <- data |> 
      dplyr::left_join(precip.key, by = "OBJECTID") |> 
      dplyr::rename(ppt.90 = ppt_pctile0.9, ppt.10 = ppt_pctile0.1) |> 
      dplyr::group_by(OBJECTID, scenario, model) |> 
      dplyr::mutate(
        Pf.temp = coef$temp*temp + coef$temp2*temp2,
        flood = as.numeric(ppt >= ppt.90),
        drought = as.numeric(ppt <= ppt.10),
        flood.lag = dplyr::lag(flood, order_by = monthyr),
        flood.lag2 = dplyr::lag(flood, order_by = monthyr, n=2),
        flood.lag3 = dplyr::lag(flood, order_by = monthyr, n=3),
        drought.lag = dplyr::lag(drought, order_by = monthyr),
        drought.lag2 = dplyr::lag(drought, order_by = monthyr, n=2),
        drought.lag3 = dplyr::lag(drought, order_by = monthyr, n=3),
        Pf.flood = (
          coef[['flood']]*flood + 
            coef[['flood.lag']]*flood.lag + 
            coef[['flood.lag2']]*flood.lag2 + 
            coef[['flood.lag3']]*flood.lag3),
        Pf.drought = (
          coef[['drought']]*drought + 
            coef[['drought.lag']]*drought.lag + 
            coef[['drought.lag2']]*drought.lag2 + 
            coef[['drought.lag3']]*drought.lag3),
        Pf.prec = (Pf.flood + Pf.drought),
        Pred = (Pf.temp + Pf.prec)
      ) |> 
      dplyr::ungroup()
    
    if(i == 1) {
      iter.df |> 
        dplyr::select(
          OBJECTID, monthyr, month, year, 
          scenario, model, country, ISO, region) |>
        arrow:: write_feather(meta_path)
      # vroom_write(meta_path)
    }
    
    iter.df |> 
      dplyr::select(Pred, Pf.temp, Pf.flood, Pf.drought) |>
      arrow:: write_feather(file_path)
    # vroom_write(file_path)
    
    tictoc::toc()
  } else {
    tictoc::toc(); return(NULL)
  }
  
}
tictoc::toc()
stopCluster(cl)

