library(here)
library(tidyverse)
library(data.table)
library(patchwork)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

calculate_baseline_mean <- function(df, variable) {
  df |>
    dplyr::filter(year %in% 2015:2020) |>
    dplyr::group_by(model, scenario, iter) |>
    dplyr::summarize(BetaMean = mean({{variable}}, na.rm = TRUE))
}

adjust_data <- function(df, bm, variable) {
  df |>
    dplyr::left_join(bm) |>
    dplyr::mutate({{variable}} := ({{variable}} - BetaMean)) |>
    dplyr::select(-BetaMean)
}

summarize_data <- function(df, variable) {
  df |>
    dplyr::group_by(scenario, year) |>
    dplyr::summarize(
      median = median({{variable}}, na.rm = TRUE),
      upper = quantile({{variable}}, 0.95, na.rm = TRUE),
      lower = quantile({{variable}}, 0.05, na.rm = TRUE)) |>
    dplyr::mutate(scenario = factor(scenario, levels = c('ssp126', 'ssp245', 'ssp585'))) |>
    dplyr::filter(year > 2016)
}

create_plot <- function(data, y_label, show_legend = FALSE) {
  colors <- c("#4d5f8e", "#C582B2", "#325756")
  labels <- c(
    'Future climate (SSP1-RCP2.6)',
    'Future climate (SSP2-RCP4.5)', 
    'Future climate (SSP5-RCP8.5)'
  )
  p <- ggplot(data, aes(x = year, y = median, group = scenario, color = scenario)) +
    theme_bw() +
    geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) +
    scale_color_manual(
      values = c(colors),
      labels = c(labels),
      name = '') +
    scale_fill_manual(
      values = c(colors),
      labels = c(labels),
      name = '') +
    geom_line(aes(x = year, y = median), lwd = 1.3) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), color = NA, alpha = 0.1) +
    xlab(NULL) + ylab(y_label) +
    theme(axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 6),
          plot.margin = unit(c(0.2,0.5,0.2,1), "cm"),
          legend.title = element_blank())
  
  if (show_legend) {
    p <- p + theme(legend.position = c(0.2, 0.27))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

iter.df <- here::here("TempFiles", "SuppFutureBig.feather") |>
  arrow::read_feather()

variables <- list(
  list(name = "Pred", label = "Prevalence (%)"),
  list(name = "Pf.temp", label = "Partial effect of temperature"),
  list(name = "Pf.flood", label = "Partial effect of floods"),
  list(name = "Pf.drought", label = "Partial effect of droughts")
)

plots <- list()

for (i in seq_along(variables)) {
  var <- variables[[i]]
  bm <- calculate_baseline_mean(iter.df, !!sym(var$name))
  df <- adjust_data(iter.df, bm, !!sym(var$name))
  data <- summarize_data(df, !!sym(var$name))
  plots[[i]] <- create_plot(data, var$label, i == 1)
}

combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
print(combined_plot)

ggplot2::ggsave(
  filename = "FigureS5_new.pdf",
  plot = combined_plot,
  device = cairo_pdf,
  path = here::here("Figures"),
  width = 7.42,
  height = 10.07,
  units = "in",
  dpi = 1200
)