
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


historic_scenarios <- scenarios[1:2]

future_scenarios <- scenarios[3:5]
