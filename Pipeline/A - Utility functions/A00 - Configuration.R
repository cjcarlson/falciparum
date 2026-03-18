############################################################
# General ----
############################################################

print("Loading A00 - Configuration.R")

user = Sys.info()['user']

print(paste0("User set to: ", user))

##### location for data and output
data_dir <- dplyr::case_when(
  user == "Colin" ~ 'C:/Users/cjcar/Dropbox/MalariaAttribution/Data/',
  user == "Tamma" ~ '/Users/tammacarleton/Dropbox/MalariaAttribution',
  user == "cullen_molitor" ~ '/home/emlab/data/malaria-attribution',
  user == "cmolitor" ~ '/global/scratch/projects/co_carleton/carleton_colab/projects/malaria-attribution',
  TRUE ~ NA_character_
)

print(paste0("data directory set to: ", data_dir))

##### location for cloned repo
repo_dir <- dplyr::case_when(
  user == "Colin" ~ 'C:/Users/cjcar/Documents/Github/falciparum',
  user == "Tamma" ~
    '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum',
  user == "cullen_molitor" ~ '/home/cullen_molitor/falciparum',
  user == "cmolitor" ~ '/global/home/users/cmolitor/falciparum',
  TRUE ~ NA_character_
)

print(paste0("repository directory set to: ", repo_dir))

############################################################
# Clustering toggle ----
# Set clustering year range (country-yr groups)
# Build a label used for folder names, file names, and plot titles
############################################################

yr_bin_size <- 5
# yr_bin_size <- 10

clust_label <- paste0("country_x_", yr_bin_size, "yr")

############################################################
# Directories ----
############################################################

clust_dir <- file.path(data_dir, clust_label)

iter_hist_dir <- file.path(clust_dir, "IterationFiles", "Historical")
iter_futu_dir <- file.path(clust_dir, "IterationFiles", "Future")
summ_hist_dir <- file.path(clust_dir, "SummaryFiles", "Historical")
summ_futu_dir <- file.path(clust_dir, "SummaryFiles", "Future")

dir.create(iter_hist_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(iter_futu_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(summ_hist_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(summ_futu_dir, showWarnings = FALSE, recursive = TRUE)

climate_dir <- file.path(repo_dir, "Climate") 
precip_fp <- file.path(climate_dir, "PrecipKey.csv") 

pipeline_A_dir <- file.path(repo_dir, "Pipeline", "A - Utility functions")
A_utils_calc_fp <- file.path(pipeline_A_dir, "A01 - Utility code for calculations.R")
A_utils_plot_fp <- file.path(pipeline_A_dir, "A02 - Utility code for plotting.R")
A_utils_data_fp <- file.path(pipeline_A_dir, "A03 - Prep data for estimation.R")


bc_cru_ts_dir <- file.path(data_dir, "BC-ClimateData20230626", "3-Outputs")
bc_cruts_output_dir <- file.path(bc_cru_ts_dir, "bc_cruts4.03")
bc_cruts_old_output_dir <- file.path(bc_cru_ts_dir, "bc_crts4.06")
data_data_dir <- file.path(data_dir, "Data")

ADM1_fp <- file.path(data_data_dir, 'AfricaADM1.shp') 
world_regions_fp <- file.path(data_data_dir, "OriginalGBD", "WorldRegions.shp")

diag_meth_fn <- file.path(
  data_data_dir,
  'dominant_diagnostic_method_summary.csv'
)

resdir = file.path(clust_dir, "Results")
data_fp <- file.path(data_dir, 'Data', 'CRU-Reextraction-Aug2022.csv')

dir.create(resdir, showWarnings = FALSE, recursive = TRUE)

############################################################
# Figure directories ----
# Create necessary subfolders
############################################################

figure_dir <- file.path(resdir, "Figures")
figure_main_dir <- file.path(figure_dir, "main")
figure_diag_dir <- file.path(resdir, "Figures", "Diagnostics")
figure_diag_sub_dir <- file.path(figure_diag_dir, "Subsamples")
figure_diag_res_dir <- file.path(figure_diag_dir, "Residuals")
figure_diag_fe_dir <- file.path(figure_diag_dir, "Fixed_effects")

dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_main_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_sub_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_res_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_res_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# Model directories ----
############################################################

model_dir <- file.path(resdir, "Models")
model_boot_dir <- file.path(model_dir, "bootstrap")
model_repro_dir <- file.path(model_dir, "reproducibility")

dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_boot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_repro_dir, showWarnings = FALSE, recursive = TRUE)

main_mod_beta_fn <- file.path(model_repro_dir, "coefficients_cXt2intrXm.rds")
main_mod_vcov_fn <- file.path(model_repro_dir, "vcv_cXt2intrXm.rds")
boot_mod_full_fn <- file.path(model_dir, "block_bootstrap_cXt2intrXm.rds")

############################################################
# Table directories ----
############################################################

table_dir <- file.path(resdir, "Tables")
table_main_dir <- file.path(table_dir, "main")
table_diag_dir <- file.path(resdir, "Tables", "Diagnostics")
table_diag_res_dir <- file.path(table_diag_dir, "Residuals")
table_sens_dir <- file.path(resdir, "Tables", "sensitivity")

dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_main_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_diag_res_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_sens_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# Constants ----
############################################################

models <- c(
  "ACCESS-CM2",
  "ACCESS-ESM1-5",
  "BCC-CSM2-MR",
  "BCC-CSM2-MR",
  "CanESM5",
  "FGOALS-g3",
  "FGOALS-g3",
  "GFDL-ESM4",
  "IPSL-CM6A-LR",
  "MIROC6",
  "MRI-ESM2-0",
  "IPSL-CM6A-LR",
  "MIROC6",
  "MRI-ESM2-0",
  "NorESM2-LM"
)

part1 <- paste0(replicate(151, "\nAAAAAAAAABBCC"), collapse = "")
part2 <- "\nAAAAAAAAA####\n"
part3 <- paste(replicate(80, "DDDDDDDDDDDDD\n"), collapse = "")
fig_3_4_layout <- paste(part1, part2, part3, sep = "")

scenario_labels <- c(
  'Historical counterfactual',
  'Historical counterfactual',
  'Historical climate',
  'Future climate (SSP1-RCP2.6)',
  'Future climate (SSP2-RCP4.5)',
  'Future climate (SSP5-RCP8.5)'
)

scenarios <- c(
  "historical",
  "hist-nat",
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
  names(historical_scenario_names),
  unname(historical_scenario_names),
  rlang::new_formula
)

yr_1901 <- 1901:1905
yr_2014 <- 2010:2014
yr_2015 <- 2015:2019
yr_2050 <- 2048:2052
yr_2100 <- 2096:2100

# Year bins — defined once, reused everywhere
yr_bins <- list(
  "1901" = 1901:1905,
  "2014" = 2010:2014,
  "2015" = 2015:2019,
  "2050" = 2048:2052,
  "2100" = 2096:2100
)

# Build a named lookup vector: original_year -> bin_year
yr_lookup <- unlist(lapply(names(yr_bins), function(nm) {
  setNames(rep(as.integer(nm), length(yr_bins[[nm]])), yr_bins[[nm]])
}))

yr_1901 <- 1901:1905
yr_2014 <- 2010:2014
yr_2015 <- 2015:2019
yr_2050 <- 2048:2052
yr_2100 <- 2096:2100

# Year bins — defined once, reused everywhere
yr_bins <- list(
  "1901" = 1901:1905,
  "2014" = 2010:2014,
  "2015" = 2015:2019,
  "2050" = 2048:2052,
  "2100" = 2096:2100
)

# Build a named lookup vector: original_year -> bin_year
yr_lookup <- unlist(lapply(names(yr_bins), function(nm) {
  setNames(rep(as.integer(nm), length(yr_bins[[nm]])), yr_bins[[nm]])
}))

floodvars <- "flood + flood.lag + flood.lag2 + flood.lag3"
droughtvars <- "drought + drought.lag + drought.lag2 + drought.lag3"

# common variables in all regs
common <- paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars)
country_time <- "country:monthyr + country:monthyr2"

# Main Specification Formula (see other files for robustness/sensitivity checks)
cXt2intrXm = as.formula(
  paste0(
    common,
    " + I(intervention) + ", country_time, 
    "| OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)