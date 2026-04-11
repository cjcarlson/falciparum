################################################################################
# Configuration file for the malaria attribution pipeline. This script sets up
# all file paths, directories, model parameters, and constants used throughout
# the analysis. It should be sourced at the beginning of each pipeline script.
################################################################################
# Setup ----
################################################################################

print("Begin loading A01 - Configuration.R")

user = Sys.info()['user']

print(paste0("User set to: ", user))

savio_dir <- '/global/scratch/projects/co_carleton/carleton_colab/projects/'

##### location for data and output
data_dir <- dplyr::case_when(
  user == "Colin" ~ 'C:/Users/cjcar/Dropbox/MalariaAttribution/Data/',
  user == "Tamma" ~ '/Users/tammacarleton/Dropbox/MalariaAttribution',
  user == "cullen_molitor" ~ '/home/emlab/data/malaria-attribution',
  user == "cmolitor" ~ paste0(savio_dir, 'malaria-replication/data'),
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

################################################################################
# Model formula ----
# Main specification formula. See other files for robustness/sensitivity checks.
# Set clustering year range (country-yr groups)
################################################################################

# yr_bin_size <- 10
yr_bin_size <- 5

clust_label <- paste0("country_x_", yr_bin_size, "yr")

floodvars <- "flood + flood.lag + flood.lag2 + flood.lag3"
droughtvars <- "drought + drought.lag + drought.lag2 + drought.lag3"

# common variables in all regs
common <- paste0("PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars)
country_time <- "country:monthyr + country:monthyr2"

# clustering <- "OBJECTID"
clustering <- "cntry_yrbin"

cXt2intrXm <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    "| OBJECTID + as.factor(smllrgn):month | 0 | ",
    clustering
  )
)

################################################################################
# Plotting toggles ----
# Choose the minimum and maximum for range of temperature for x axis
################################################################################

Tmin = 10
Tmax = 40

################################################################################
# Utility files ----
################################################################################

pipeline_A_dir <- file.path(repo_dir, "Pipeline", "A - Utility functions")
A_utils_calc_fp <- file.path(
  pipeline_A_dir,
  "A02 - Utility code for calculations.R"
)
A_utils_plot_fp <- file.path(
  pipeline_A_dir,
  "A03 - Utility code for plotting.R"
)

logs_dir <- file.path(repo_dir, "code_logs")

################################################################################
# Input, intermediate, output data ----
################################################################################

input_dir <- file.path(data_dir, "input")
inter_dir <- file.path(data_dir, "intermediate")
output_dir <- file.path(data_dir, "output")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

################################################################################
# Spatial data (input) ----
################################################################################

geo_data_dir <- file.path(input_dir, "geographic")
ADM1_fp <- file.path(geo_data_dir, "ADM1_shapefile", 'AfricaADM1.shp')
gbd_fp <- file.path(geo_data_dir, "GBD_shapefile", "WorldRegions.shp")
urban_fp <- file.path(
  geo_data_dir,
  "urban_shapefile",
  "GHS_UCDB_REGION_SUB_SAHARAN_AFRICA_R2024A.gpkg"
)

elevation_fp <- file.path(
  geo_data_dir,
  "elevation",
  "elevation_extracted_all_ADM1.csv"
)
################################################################################
# Climate data (input) ----
################################################################################

climate_dir <- file.path(input_dir, "climate")
climate_cru_dir <- file.path(climate_dir, "CRU_TS403")
climate_bc_cmip6_dir <- file.path(climate_dir, "bc_CMIP6")
climate_era5_dir <- file.path(climate_dir, "ERA5_monthly_single_levels_1940-2026")
climate_gwl_dir <- file.path(climate_dir, "GWL")

################################################################################
# CRU data (input) ----
################################################################################

cru_tmp_fp <- file.path(
  climate_cru_dir,
  "tmp",
  "cru_ts4.03.1901.2018.tmp.dat.nc"
)
cru_pre_fp <- file.path(
  climate_cru_dir,
  "pre",
  "cru_ts4.03.1901.2018.pre.dat.nc"
)

################################################################################
# ERA5 data (input) ----
################################################################################

temp_fp <- file.path(climate_era5_dir, "2m_temperature.grib")
prec_fp <- file.path(climate_era5_dir, "Total_precipitation.grib")

# era5_tmp_dir <- file.path(climate_era5_dir, "t2m_tiff")
# era5_pre_dir <- file.path(climate_era5_dir, "prec_nc")

################################################################################
# Prevalence data (input) ----
################################################################################

prev_dir <- file.path(input_dir, "prevalence")

prev_DB_fp <- file.path(
  prev_dir,
  '00 Africa 1900-2015 SSA PR database (260617).csv'
)

################################################################################
# Intermediate data ----
################################################################################

climate_prc_key_dir <- file.path(inter_dir, "precip_keys")
inter_cru_ext_dir <- file.path(inter_dir, "CRU_extract")
inter_cmip6_ext_dir <- file.path(inter_dir, "CMIP6_extract")
inter_cmip6_pred_dir <- file.path(inter_dir, "CMIP6_predict")
inter_cmip6_sum_dir <- file.path(inter_dir, "CMIP6_summary")
inter_era5_ex_dir <- file.path(inter_dir, "ERA5_extract")
hist_pred_dir <- file.path(inter_cmip6_pred_dir, "historical")
fut_pred_dir <- file.path(inter_cmip6_pred_dir, "future")
hist_sum_dir <- file.path(inter_cmip6_sum_dir, "historical")
fut_sum_dir <- file.path(inter_cmip6_sum_dir, "future")
inter_urban_dir <- file.path(inter_dir, "urban")

dir.create(climate_prc_key_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(inter_cru_ext_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(inter_cmip6_ext_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(inter_cmip6_pred_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(inter_cmip6_sum_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(inter_era5_ex_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(hist_pred_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(hist_sum_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fut_pred_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fut_sum_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(inter_urban_dir, showWarnings = FALSE, recursive = TRUE)

intermediate_CRU_adm1_fp <- file.path(
  inter_cru_ext_dir,
  "CRU-climate-intermediate-adm1.csv"
)

intermediate_CRU_grid_fp <- file.path(
  inter_cru_ext_dir,
  'CRU-climate-intermediate-grid.feather'
)

intermediate_ERA_adm1_fp <- file.path(
  inter_era5_ex_dir,
  'ERA5-climate-intermediate-adm1.csv'
)

precip_CRU_adm1_fp <- file.path(climate_prc_key_dir, "PrecipKey_CRU_adm1.csv")
precip_CRU_grid_fp <- file.path(climate_prc_key_dir, "PrecipKey_CRU_grid.csv")
precip_ERA5_adm1_fp <- file.path(climate_prc_key_dir, "PrecipKey_ERA5_adm1.csv")

urban_summary_fp <- file.path(inter_urban_dir, 'urban_summary.csv')

################################################################################
# Analysis ready files (output) ----
################################################################################

analysis_ready_CRU_adm1_fp <- file.path(
  output_dir,
  "prevalence_and_climate_adm1.rds"
)

analysis_ready_grid_fp <- file.path(
  output_dir,
  "prevalence_and_climate_grid.rds"
)

analysis_ready_ERA5_adm1_fp <- file.path(
  output_dir,
  "prevalence_and_climate_ERA5_adm1.rds"
)

################################################################################
# Results directory ----
################################################################################

results_dir <- file.path(dirname(data_dir), "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

################################################################################
# Figure directories ----
################################################################################

figure_dir <- file.path(results_dir, "figures")
figure_main_dir <- file.path(figure_dir, "main_specification")
figure_supp_dir <- file.path(figure_dir, "supplemental")
# figure_diag_dir <- file.path(figure_dir, "diagnostics")
figure_sub_dir <- file.path(figure_dir, "subsamples")
figure_res_dir <- file.path(figure_dir, "residuals")
figure_fe_dir <- file.path(figure_dir, "fixed_effects")
figure_temp_dir <- file.path(figure_dir, "temp_lags")
figure_df_dir <- file.path(figure_dir, "drought_flood_defn")
figure_tff_dir <- file.path(figure_dir, "temp_functional_form")
figure_rand_dir <- file.path(figure_dir, "randomization_tests")
figure_grid_dir <- file.path(figure_dir, "grid_level")
figure_urban_dir <- file.path(figure_dir, "urbanization")
figure_era5_dir <- file.path(figure_dir, "era5")
figure_vcov_dir <- file.path(figure_dir, "vcov")

dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_main_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_supp_dir, showWarnings = FALSE, recursive = TRUE)
# dir.create(figure_diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_sub_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_res_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_fe_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_temp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_df_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_tff_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_rand_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_grid_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_urban_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_era5_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_vcov_dir, showWarnings = FALSE, recursive = TRUE)

################################################################################
# Model directories ----
################################################################################

model_dir <- file.path(results_dir, "models")
model_main_dir <- file.path(model_dir, "main")
model_boot_dir <- file.path(model_dir, "bootstrap")
model_rand_dir <- file.path(model_dir, "randomization_tests")
model_grid_dir <- file.path(model_dir, "grid")

dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_boot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_main_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_rand_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_grid_dir, showWarnings = FALSE, recursive = TRUE)

main_mod_obj_fn <- file.path(model_main_dir, "model_object_cXt2intrXm..rds")
main_mod_beta_fn <- file.path(model_main_dir, "coefficients_cXt2intrXm.rds")
main_mod_vcov_fn <- file.path(model_main_dir, "vcv_cXt2intrXm.rds")

grid_mod_beta_fn <- file.path(
  model_main_dir,
  "coefficients_cXt2intrXm_grid.rds"
)
grid_mod_vcov_fn <- file.path(model_main_dir, "vcv_cXt2intrXm_grid.rds")

ERA5_mod_beta_fn <- file.path(
  model_main_dir,
  "ERA5_coefficients_cXt2intrXm.rds"
)
ERA5_mod_vcov_fn <- file.path(model_main_dir, "ERA5_vcv_cXt2intrXm.rds")

boot_mod_full_fn <- file.path(model_boot_dir, "block_bootstrap_cXt2intrXm.csv")
boot_mod_full_ERA5_fn <- file.path(
  model_boot_dir,
  "block_bootstrap_ERA5_cXt2intrXm.csv"
)
vcov_sample_mod_full_fn <- file.path(
  model_boot_dir,
  "vcov_sample_cXt2intrXm.csv"
)

scramble_time_fp <- file.path(model_rand_dir, "scramble_time_placebo.rds")

################################################################################
# Table directories ----
################################################################################

table_dir <- file.path(results_dir, "tables")
table_main_dir <- file.path(table_dir, "main")
table_diag_dir <- file.path(table_dir, "diagnostics")
table_sens_dir <- file.path(table_dir, "sensitivity")
table_diag_res_dir <- file.path(table_diag_dir, "residuals")

dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_main_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_sens_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_diag_res_dir, showWarnings = FALSE, recursive = TRUE)

table_star_cutoffs <- c(0.05, 0.01, 0.001)

################################################################################
# Constants ----
################################################################################

# > You can modify these if you want a different baseline or thresholds
pct_flood <- 0.90 # 90 th percentile ⇒ “flood”
pct_drought <- 0.10 # 10 th percentile ⇒ “drought”
year_cutoff <- NA # e.g. 2000 if you want climatology up to year 2000 only

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

part1 <- paste0(replicate(151, "\nAAAAAAAAABBCC"), collapse = "")
part2 <- "\nAAAAAAAAA####\n"
part3 <- paste(replicate(80, "DDDDDDDDDDDDD\n"), collapse = "")
fig_3_4_layout <- paste(part1, part2, part3, sep = "")

scenario_colors <- c(
  "hist-nat" = "grey50",
  "historical" = "#287DAB",
  "ssp126" = "#4d5f8e",
  "ssp245" = "#C582B2",
  "ssp585" = "#325756"
)

scenario_labels <- c(
  'Historical counterfactual',
  'Historical climate',
  'Future climate (SSP1-RCP2.6)',
  'Future climate (SSP2-RCP4.5)',
  'Future climate (SSP5-RCP8.5)'
)

scenarios <- c(
  "hist-nat",
  "historical",
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
  "hist-nat" = "Historical natural",
  "historical" = "Historical"
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

yr_1901 <- 1901:1905
yr_2014 <- 2010:2014
yr_2015 <- 2015:2019
yr_2050 <- 2048:2052
yr_2100 <- 2096:2100

print("Finished loading A01 - Configuration.R")

################################################################################
# End of file ----
################################################################################
