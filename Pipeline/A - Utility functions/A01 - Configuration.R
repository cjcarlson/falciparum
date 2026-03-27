############################################################
# General ----
############################################################

print("Begin loading A01 - Configuration.R")

user = Sys.info()['user']

print(paste0("User set to: ", user))

##### location for data and output
data_dir <- dplyr::case_when(
  user == "Colin" ~ 'C:/Users/cjcar/Dropbox/MalariaAttribution/Data/',
  user == "Tamma" ~ '/Users/tammacarleton/Dropbox/MalariaAttribution',
  user == "cullen_molitor" ~ '/home/emlab/data/malaria-attribution',
  user ==
    "cmolitor" ~ '/global/scratch/projects/co_carleton/carleton_colab/projects/malaria-replication/data',
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
############################################################

yr_bin_size <- 5 # 10

clust_label <- paste0("country_x_", yr_bin_size, "yr")

############################################################
# Analysis ready files ----
############################################################

analysis_ready_dir <- file.path(data_dir, "analysis_ready_data")

analysis_ready_adm1_fp <- file.path(
  analysis_ready_dir,
  "prevalence_and_climate_adm1.rds"
)

analysis_ready_grid_fp <- file.path(
  analysis_ready_dir,
  "prevalence_and_climate_grid.rds"
)

dir.create(analysis_ready_dir, showWarnings = FALSE, recursive = TRUE)


# clust_dir <- file.path(data_dir, clust_label)

# iter_hist_dir <- file.path(clust_dir, "IterationFiles", "Historical")
# iter_futu_dir <- file.path(clust_dir, "IterationFiles", "Future")
# summ_hist_dir <- file.path(clust_dir, "SummaryFiles", "Historical")
# summ_futu_dir <- file.path(clust_dir, "SummaryFiles", "Future")

# dir.create(iter_hist_dir, showWarnings = FALSE, recursive = TRUE)
# dir.create(iter_futu_dir, showWarnings = FALSE, recursive = TRUE)
# dir.create(summ_hist_dir, showWarnings = FALSE, recursive = TRUE)
# dir.create(summ_futu_dir, showWarnings = FALSE, recursive = TRUE)

# data_data_dir <- file.path(data_dir, "Data")
# dir.create(data_data_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# Spatial data ----
############################################################

geo_data_dir <- file.path(data_dir, "geographic_data")

ADM1_fp <- file.path(geo_data_dir, "ADM1_shapefile", 'AfricaADM1.shp')
gbd_fp <- file.path(geo_data_dir, "GBD_shapefile", "WorldRegions.shp")
urban_fp <- file.path(
  geo_data_dir,
  "urban_shapefile",
  "GHS_UCDB_REGION_SUB_SAHARAN_AFRICA_R2024A.gpkg"
)

############################################################
# Utility files ----
############################################################

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

############################################################
# Climate directories ----
############################################################
climate_dir <- file.path(data_dir, "climate_data")
climate_cru_dir <- file.path(climate_dir, "CRU_TS403")
climate_prc_key_dir <- file.path(climate_dir, "cru_prc_keys")
climate_bc_cmip6_dir <- file.path(climate_dir, "bc_CMIP6")

############################################################
# Intermediate directories ----
############################################################

inter_dir <- file.path(data_dir, "intermediate_data")
inter_cru_ext_dir <- file.path(inter_dir, "CRU_extract")
inter_cmip6_ext_dir <- file.path(inter_dir, "CMIP6_extract")
inter_cmip6_pre_dir <- file.path(inter_dir, "CMIP6_predict")
inter_cmip6_sum_dir <- file.path(inter_dir, "CMIP6_summary")

dir.create(inter_cru_ext_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(inter_cmip6_ext_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# CRU Files ----
############################################################

precip_adm1_fp <- file.path(climate_prc_key_dir, "PrecipKey_adm1.csv")
precip_grid_fp <- file.path(climate_prc_key_dir, "PrecipKey_grid.csv")

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

intermediate_CRU_adm1_fp <- file.path(
  inter_cru_ext_dir,
  "CRU-climate-intermediate-adm1.csv"
)

intermediate_CRU_grid_fp <- file.path(
  inter_cru_ext_dir,
  'CRU-climate-intermediate-grid.csv'
)

############################################################
# Prevalence files ----
############################################################

prev_dir <- file.path(data_dir, "prevalence_data")

prev_DB_fp <- file.path(
  prev_dir,
  '00 Africa 1900-2015 SSA PR database (260617).csv'
)

urban_summary_fp <- file.path(prev_dir, 'urban_summary.csv')

############################################################
# Results directory ----
############################################################

results_dir <- file.path(dirname(data_dir), "Results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# Figure directories ----
############################################################

figure_dir <- file.path(results_dir, "Figures")
figure_main_dir <- file.path(figure_dir, "Main_model")
figure_diag_dir <- file.path(figure_dir, "Diagnostics")
figure_diag_sub_dir <- file.path(figure_diag_dir, "Subsamples")
figure_diag_res_dir <- file.path(figure_diag_dir, "Residuals")
figure_diag_fe_dir <- file.path(figure_diag_dir, "Fixed_effects")
figure_diag_temp_dir <- file.path(figure_diag_dir, "Temp_lags")
figure_diag_df_dir <- file.path(figure_diag_dir, "Drought_flood_defn")
figure_diag_tff_dir <- file.path(figure_diag_dir, "Temp_functionalForm")

dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_main_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_sub_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_res_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_fe_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_temp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_diag_df_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# Model directories ----
############################################################

model_dir <- file.path(results_dir, "Models")
model_boot_dir <- file.path(model_dir, "bootstrap")
model_repro_dir <- file.path(model_dir, "reproducibility")

dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_boot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_repro_dir, showWarnings = FALSE, recursive = TRUE)

main_mod_beta_fn <- file.path(model_repro_dir, "coefficients_cXt2intrXm.rds")
main_mod_vcov_fn <- file.path(model_repro_dir, "vcv_cXt2intrXm.rds")
# boot_mod_full_fn <- file.path(model_dir, "block_bootstrap_cXt2intrXm.rds")
boot_mod_full_fn <- file.path(model_boot_dir, "block_bootstrap_cXt2intrXm.csv")

############################################################
# Table directories ----
############################################################

table_dir <- file.path(results_dir, "Tables")
table_main_dir <- file.path(table_dir, "main")
table_diag_dir <- file.path(table_dir, "Diagnostics")
table_sens_dir <- file.path(table_dir, "sensitivity")
table_diag_res_dir <- file.path(table_diag_dir, "Residuals")

dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_main_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_sens_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_diag_res_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# Constants ----
############################################################

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
cXt2intrXm <- as.formula(
  paste0(
    common,
    " + I(intervention) + ",
    country_time,
    "| OBJECTID + as.factor(smllrgn):month | 0 | cntry_yrbin"
  )
)

# cXt2intrXm <- as.formula(
#   paste0(
#     common,
#     " + I(intervention) + ",
#     country_time,
#     "| OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
#   )
# )

print("Finished loading A01 - Configuration.R")
