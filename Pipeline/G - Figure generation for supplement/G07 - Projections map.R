library(sf)
library(magrittr)
library(patchwork)
library(tidyverse)
library(data.table)
library(colorspace)
library(multiscales)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

cont <- sf::read_sf(file.path(datadir, 'Data', 'AfricaADM1.shp')) |> 
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

iter.df <- here::here("TempFiles", "Fig4Big.feather") |> 
  arrow::read_feather() 

create_slice_map <- function(scenario, year) {
  slice_map <- iter.df |> 
    dplyr::filter(scenario == !!scenario) |>
    tidyr::pivot_wider(names_from = year, values_from = Pred) |> 
    dplyr::mutate(diff = (!!sym(year) - `2015`)) |>
    dplyr::group_by(OBJECTID) |>
    dplyr::summarize(mean_diff = mean(diff)) |>
    dplyr::select(OBJECTID, mean_diff)
  
  colnames(slice_map)[2] <- paste0("mean.diff.", year, substr(scenario, 4, 6))
  slice_map
}

scenarios <- c("ssp126", "ssp245", "ssp585")
years <- c("2050", "2100")

for (scenario in scenarios) {
  for (year in years) {
    cont <- dplyr::left_join(cont, create_slice_map(scenario, year))
  }
}

create_ggplot <- function(data_column, limits) {
  ggplot(cont) + 
    geom_sf(aes(fill = !!sym(data_column)), color = "gray30", size = 0.05) +
    coord_sf(datum = NA, xlim = c(-17.5, 52), ylim = c(-35.5, 37.5)) + 
    theme_void() + 
    scale_fill_continuous_divergingx(
      palette = "Geyser", na.value = "white",
      limits = limits) +
    labs(fill = "Prevalence (%)")
}

plots <- list()
for (scenario in scenarios) {
  column_2100 <- paste0("mean.diff.2100", substr(scenario, 4, 6))
  limits <- range(cont[[column_2100]], na.rm = TRUE)
  
  for (year in years) {
    column_name <- paste0("mean.diff.", year, substr(scenario, 4, 6))
    plots[[length(plots) + 1]] <- create_ggplot(column_name, limits)
  }
}

many_maps <- (
  (plots[[1]] + plots[[2]] + plot_layout(guides = 'collect')) / 
    (plots[[3]] + plots[[4]] + plot_layout(guides = 'collect')) / 
    (plots[[5]] + plots[[6]] + plot_layout(guides = 'collect'))
) + plot_annotation(tag_levels = 'A')

ggplot2::ggsave(
  filename = "FigureS3_new.pdf",
  plot = many_maps,
  device = cairo_pdf,
  path = here::here("Figures"),
  width = 9.53,
  height = 10.07,
  units = "in",
  dpi = 1200
)