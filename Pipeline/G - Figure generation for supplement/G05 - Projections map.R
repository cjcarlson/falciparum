library(sf)
library(magrittr)
library(patchwork)
library(tidyverse)
library(data.table)
library(colorspace)
library(multiscales)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

cont <- sf::read_sf(file.path(data_dir, 'Data', 'AfricaADM1.shp')) |> 
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
  pal <- colorspace::divergingx_hcl(n = 11, palette = "Geyser")
  
  midpoint <- 0
  mid_rescaled <- (midpoint - limits[1]) / (limits[2] - limits[1])
  
  values <- c(
    seq(0, mid_rescaled, length.out = 6),
    seq(mid_rescaled, 1, length.out = 6)[-1]
  )
  
  ggplot(cont) + 
    geom_sf(aes(fill = !!sym(data_column)), color = "gray30", size = 0.05) +
    coord_sf(datum = NA, xlim = c(-17.5, 52), ylim = c(-35.5, 37.5)) + 
    theme_void() + 
    scale_fill_gradientn(
      colors = pal,
      values = values,
      na.value = "white",
      limits = limits,
      oob = scales::squish
    ) +
    labs(fill = "Prevalence (%)")
}

all_columns <- paste0("mean.diff.", rep(years, length(scenarios)), 
                      rep(substr(scenarios, 4, 6), each = length(years)))
global_limits <- range(sapply(all_columns, function(col) cont[[col]]), na.rm = TRUE)


limits <- c(-5, 2.5)

plots <- list()
for (scenario in scenarios) {
  for (year in years) {
    column_name <- paste0("mean.diff.", year, substr(scenario, 4, 6))
    plots[[length(plots) + 1]] <- create_ggplot(column_name, limits)
  }
}

plots[[1]] <- plots[[1]] + 
  labs(tag = "SSP1-RCP2.6", title = "2048-2052") + 
  theme(plot.tag = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        plot.tag.position = "left",
        plot.title = element_text(hjust = 0.5))

plots[[2]] <- plots[[2]] + 
  ggtitle("2096-2100") + 
  theme(plot.title = element_text(hjust = 0.5))

plots[[3]] <- plots[[3]] + 
  labs(tag = "SSP2-RCP4.5") + 
  theme(plot.tag = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        plot.tag.position = "left")

plots[[5]] <- plots[[5]] + 
  labs(tag = "SSP5-RCP8.5") + 
  theme(plot.tag = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        plot.tag.position = "left")

map_grid <- (
  (plots[[1]] + plots[[2]]) / 
    (plots[[3]] + plots[[4]]) / 
    (plots[[5]] + plots[[6]])
) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "right")

ggplot2::ggsave(
  filename = "FigureS3.jpg",
  plot = map_grid,
  path = here::here("Figures"),
  width = 9.53,
  height = 10.07,
  units = "in"
)

ggplot2::ggsave(
  filename = "FigureS3.pdf",
  plot = map_grid,
  path = here::here("Figures"),
  width = 9.53,
  height = 10.07,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)
