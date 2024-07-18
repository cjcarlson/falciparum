library(here)
library(terra)
library(tidyr)
library(readr)
library(tictoc)
library(foreach)
library(doParallel)
library(exactextractr)

tictoc::tic()

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(pipeline_A_dir, "A01 - Utility code for calculations.R"))

numCores <- 100

overwrite <- FALSE

cl <- makeCluster(numCores, outfile="")

registerDoParallel(cl)

for (scenario in scenarios) {
  
  tictoc::tic()
  print(paste0("Starting: ", scenario))
  
  date_range <- dplyr::case_when(
    scenario %in% c("historical", "hist-nat") ~ "_190101-201412_",
    scenario %in% c("ssp126", "ssp245", "ssp585") ~ "_201501-210012_"
  )
  
  year_mon <- ifelse(scenario %in% c("historical", "hist-nat"), 1368, 1032)
  year_start <- ifelse(scenario %in% c("historical", "hist-nat"), 1901, 2015)
  
  for (model in models) {
    
    tictoc::tic()
    print(paste0("Starting: ", model))
    
    grid <- dplyr::case_when(
      model == "GFDL-ESM4" ~ "gr1",
      model == "IPSL-CM6A-LR" ~ "gr",
      TRUE ~ "gn"
    )
    
    prc_fn <- paste0(
      "pr_Amon_", model, "_", scenario,
      "_r1i1p1f1_", grid, date_range, "BC_lonlat.nc"
    )
    tmp_fn <- paste0(
      "tas_Amon_", model, "_", scenario, 
      "_r1i1p1f1_", grid, date_range, "BC_lonlat.nc"
    )
    
    foreach(
      i = 1:year_mon,
      .export = c(
        "datadir",
        "bc_cruts_output_dir",
        "scenario", 
        "prc_fn", 
        "tmp_fn"
      )
    ) %dopar% {
      
      month_year <- paste(
        month.abb[(i-1)%%12 + 1], 
        ((i-1 - (i-1)%%12)/12) + year_start, 
        sep='.'
      )
      
      output_path <- file.path(
        datadir, "int", scenario, paste0(model, "_", i, ".csv")
      )
      output_dir <- dirname(output_path)
      
      if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}    
      
      if (!file.exists(output_path) | overwrite) {
        
        cont <- sf::read_sf(here::here(datadir, 'Data', 'AfricaADM1.shp')) 
        
        nct <- terra::rast(file.path(bc_cruts_output_dir, scenario, tmp_fn))
        ncp <- terra::rast(file.path(bc_cruts_output_dir, scenario, prc_fn))
        
        temp <- nct[[i]] 
        temp_ex <- exactextractr::exact_extract(
          x = temp, y = cont, fun = 'mean', progress = FALSE
        )
        
        temp2 <- temp * temp
        temp2_ex <- exactextractr::exact_extract(
          x = temp2, y = cont, fun = 'mean', progress = FALSE
        )
        
        ppt <- ncp[[i]]
        ppt_ex <- exactextractr::exact_extract(
          x = ppt, y = cont, fun = 'mean', progress = FALSE
        )
        
        dummy_df <- tibble::tibble(
          OBJECTID = cont$OBJECTID, 
          !!paste0(month_year, '.temp') := temp_ex,
          !!paste0(month_year, '.temp2') := temp2_ex, 
          !!paste0(month_year, '.ppt') := ppt_ex
        ) |> 
          tidyr::pivot_longer(
            -OBJECTID, 
            names_to = c('month', 'year', 'var'), 
            names_sep = '\\.'
          ) |>
          tidyr::pivot_wider(names_from = 'var', values_from = 'value') |> 
          dplyr::mutate(year = as.numeric(year)) |> 
          readr::write_csv(output_path)
      }
    }
    print(paste0("Finished: ", model, "\nConsilidating intermediate files"))
    
    files <- list.files(
      file.path(datadir, "int", scenario),
      pattern = model, full.names = TRUE
    )
    file_name <- file.path(datadir, "Climate", scenario, paste0(model, ".csv"))
    file_dir <- dirname(file_name)
    
    if (!dir.exists(file_dir)) {dir.create(file_dir, recursive = TRUE)}
    
    if (!file.exists(file_name) | overwrite) {
      results <- readr::read_csv(files, show_col_types = FALSE) |> 
        dplyr::mutate(month = factor(month, levels = month.abb)) |> 
        dplyr::arrange(year, month) |>
        readr::write_csv(file_name)
      tictoc::toc()
    }
  }
  print(paste0("Finished: ", scenario))
  tictoc::toc()
}
tictoc::toc()
stopCluster(cl)

