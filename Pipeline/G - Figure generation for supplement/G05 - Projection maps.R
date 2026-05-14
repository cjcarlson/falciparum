############################################################
# This script makes all four panels of
############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  sf,
  here,
  patchwork,
  tidyverse,
  colorspace
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Load and process future scenarios ----
################################################################################

future_scen_mod_yr_adm1_pred <- file.path(
  fut_sum_dir,
  "future_vcov_pred_sum_scen_mod_yr_obj.feather"
) |>
  arrow::read_feather() |>
  dplyr::group_by(scenario, model, year, OBJECTID) |> 
  dplyr::summarise(Pred = mean(Pred, na.rm = TRUE))

cont <- ADM1_fp |>
  sf::read_sf() |>
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

scenarios <- names(future_scenario_names)
years <- c("2050", "2100")

for (scenario in scenarios) {
  for (year in years) {
    tmp <- create_future_slice_map_data(
      df = future_scen_mod_yr_adm1_pred,
      scenario = scenario,
      year = year
    )
    cont <- dplyr::left_join(cont, tmp, by = join_by(OBJECTID))
  }
}

################################################################################
# Plot each future scenario ----
################################################################################

all_columns <- paste0("mean.diff.", rep(years, length(scenarios)), 
                      rep(substr(scenarios, 4, 6), each = length(years)))
global_limits <- range(sapply(all_columns, function(col) cont[[col]]), na.rm = TRUE)

limits <- c(-5, 2.5)

plots <- list()

for (scenario in scenarios) {
  for (year in years) {
    column_name <- paste0("mean.diff.", year, substr(scenario, 4, 6))
    plots[[length(plots) + 1]] <- create_future_slice_map(column_name, limits)
  }
}

plots[[1]] <- plots[[1]] +
  labs(tag = "SSP1-RCP2.6", title = "2048-2052") +
  theme(
    plot.tag = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    plot.tag.position = "left",
    plot.title = element_text(hjust = 0.5)
  )

plots[[2]] <- plots[[2]] +
  ggtitle("2096-2100") +
  theme(plot.title = element_text(hjust = 0.5))

plots[[3]] <- plots[[3]] +
  labs(tag = "SSP2-RCP4.5") +
  theme(
    plot.tag = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    plot.tag.position = "left"
  )

plots[[5]] <- plots[[5]] +
  labs(tag = "SSP5-RCP8.5") +
  theme(
    plot.tag = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    plot.tag.position = "left"
  )

################################################################################
# Combine plots and save ----
################################################################################

map_grid <- ((plots[[1]] + plots[[2]]) /
  (plots[[3]] + plots[[4]]) /
  (plots[[5]] + plots[[6]])) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "right")

ggplot2::ggsave(
  filename = "Supp_Figure_projection_maps.jpg",
  plot = map_grid,
  path = here::here("Results", "Figures"),
  width = 9.53,
  height = 10.07,
  units = "in"
)

################################################################################
# End of file ----
################################################################################
