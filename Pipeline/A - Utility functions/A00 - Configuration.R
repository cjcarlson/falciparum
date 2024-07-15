
print("Loading A00 - Configuration.R")

##### Pick one of the following users
# user = "Colin" 
# user = "Tamma"
user = "Cullen" 

##### location for data and output
datadir <- dplyr::case_when(
  user == "Colin" ~ 'C:/Users/cjcar/Dropbox/MalariaAttribution/Data/',
  user == "Tamma" ~ '/Users/tammacarleton/Dropbox/MalariaAttribution/Data/',
  user == "Cullen" ~ '/home/emlab/data/malaria-attribution',
  TRUE ~ NA_character_
)

print(paste0("data directory set to: ", datadir))

##### location for cloned repo 
repo <- dplyr::case_when(
  user == "Colin" ~ 'C:/Users/cjcar/Documents/Github/falciparum',
  user == "Tamma" ~ '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum',
  user == "Cullen" ~ '/home/cullen_molitor/falciparum',
  TRUE ~ NA_character_
)

print(paste0("repository directory set to: ", repo))


pipeline_A_dir <- here::here("Pipeline", "A - Utility functions")
pipeline_B_dir <- here::here("Pipeline", "B - Extract climate and prevalence data")


bc_cruts_output_dir <- file.path(
  datadir, 
  "BC-ClimateData20230626", 
  "3-Outputs",
  "bc_cruts4.03"
)

bc_cruts_old_output_dir <- file.path(
  datadir, 
  "BC-ClimateData20230626", 
  "3-Outputs",
  "bc_crts4.06"
)

#### Constants
models <- c(
  "ACCESS-CM2",
  "ACCESS-ESM1-5",
  "BCC-CSM2-MR", 
  "CanESM5",
  "FGOALS-g3", 
  "GFDL-ESM4",
  "IPSL-CM6A-LR", 
  "MIROC6", 
  "MRI-ESM2-0", 
  "NorESM2-LM"
)

scenarios <- c(
  "historical",
  "hist-nat", 
  "ssp126",
  "ssp245",
  "ssp585"
)


region_names <- c(
  "Sub-Saharan Africa (continent-wide)" = "Sub-Saharan Africa\n(continent-wide)",
  "Sub-Saharan Africa (Southern)" = "Southern Africa",
  "Sub-Saharan Africa (West)" = "West Africa",
  "Sub-Saharan Africa (East)" = "East Africa",
  "Sub-Saharan Africa (Central)" = "Central Africa"
)

historical_scenario_names <- c(
  "historical" = "Historical",
  "hist-nat" = "Historical natural"
)

future_scenario_names <- c(
  "ssp126" = "SSP1-RCP2.6",
  "ssp245" = "SSP2-RCP4.5",
  "ssp585" = "SSP5-RCP8.5"
)

region_formulas <- purrr::map2(
  names(region_names), 
  unname(region_names),
  rlang::new_formula
)

future_scenario_formulas <- purrr::map2(
  names(future_scenario_names),
  unname(future_scenario_names),
  rlang::new_formula
)

historical_scenario_formulas <- purrr::map2(
  names(historical_scenario_names), 
  unname(historical_scenario_names), 
  rlang::new_formula
)
