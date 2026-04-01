############################################################
# This script estimates the main empirical specification linking
# PfPR2 to drought, flood, and temperature using ERA5 data
# instead of CRU. 
############################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(lfe, here, tidyverse, stargazer, fixest, parallel, doSNOW)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

############################################################
# Plotting toggles ----
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tmin = 10 # min T for x axis
Tmax = 40 # max T for x axis

############################################################
# Set up logging ----
############################################################

log_file_path <- file.path(logs_dir, "D07_ERA5_analysis.log")

log_msg <- create_logger(log_file_path)

log_msg("Starting script `D07 - ERA5 analyhsis.R`")

############################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence
# and CRU temperature and precipitation data aggregated to
# the first level of Administrative division.
############################################################

log_msg("Loading analysis ready data")

complete <- readr::read_rds(analysis_ready_ERA5_adm1_fp)

########################################################################
# Estimation ----
# Formula cXt2intrXm is loaded from configuration file
########################################################################

log_msg("Begin modeling")

mainmod = lfe::felm(data = complete, formula = cXt2intrXm)

coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)

log_msg("Save model coefficients and vcov")

# Save results
saveRDS(coeffs, file = ERA5_mod_beta_fn)
saveRDS(vcov, file = ERA5_mod_vcov_fn)

########################################################################
# Table ----
########################################################################

log_msg("Save table results")

# Stargazer output
mynote = paste0(
  "Country-specific quad. trends with intervention FE and country by month FE. ",
  "Standard errors clustered at ",
  gsub("_", " ", clust_label),
  " level."
)

stargazer(
  mainmod,
  title = "PfPR2 response to daily avg. temperature",
  align = TRUE,
  keep = c("temp", "flood", "drought", "inter"),
  out = file.path(table_diag_dir, "ERA5_cXt2intrXm.tex"),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  digits = 2,
  star.cutoffs = c(0.05, 0.01, 0.001)
)

########################################################################
# Plot ----
# Note: analogous to Fig 2A but with analytically derived confidence
# intervals in place of bootstrap runs.
########################################################################

log_msg("Plot temperature response")

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

# plot relative to max of quadratic function
coefs = summary(mainmod)$coefficients[1:2]

#reference temperature - curve gets recentered to 0 here
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)

fig = plotPolynomialResponse(
  mainmod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = paste0("Main spec: ", clust_label),
  yLim = c(-30, 5),
  showYTitle = T,
  ci_level = 0.95
)

log_msg("Save temperature response plot")

ggplot2::ggsave(
  filename = "temp_response_ERA5_cXt2intrXm.pdf",
  path = figure_diag_dir,
  plot = fig,
  width = 7,
  height = 7,
  create.dir = TRUE
)

log_msg("Script `D07 - ERA5 analyhsis.R` completed successfully")

############################################################
# End of file ----
############################################################

########################################################################
# Cluster setup ----
########################################################################

# Set number of bootstrap simulations.
S = 1000

log_msg("Preparing the compute cluster")

n_cores = min(15, parallel::detectCores())

# Set seed for reproducible output
set.seed(11235)

# Make compute cluster
clus <- parallel::makeCluster(n_cores)
doSNOW::registerDoSNOW(clus)

# Make progress bar
pb <- txtProgressBar(max = S, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

########################################################################
# Bootstrap estimation ----
# Sampling by country × N-year cluster
########################################################################

# Block bootstrap by country × N-year clusters:
clusters <- unique(complete$cntry_yrbin)

# Pre-build a lookup: for each cluster ID, store the row indices
cluster_rows <- split(seq_len(nrow(complete)), complete$cntry_yrbin)

# Define important column names to save
column_names <- c(
  "temp",
  "temp2",
  colnames(complete)[grep("flood", colnames(complete))],
  colnames(complete)[grep("drought", colnames(complete))],
  "I(intervention)1",
  "I(intervention)2"
)

log_msg("Begin the bootstrap models")

result <- foreach(
  i = 1:(S + 1),
  .packages = c("lfe"),
  .options.snow = opts
) %dopar%
  {
    if (i == 1) {
      complete.boot <- complete
      model <- "main"
    } else {
      cl <- sample(clusters, size = length(clusters), replace = TRUE)
      boot_idx <- unlist(cluster_rows[cl], use.names = FALSE)
      complete.boot <- complete[boot_idx, ]
      model <- as.character(i)
    }
    mod <- lfe::felm(formula = cXt2intrXm, data = complete.boot)

    out <- t(mod$coefficients[1:12])
    colnames(out) <- column_names

    list(coefs = out, model = model, n = nrow(complete.boot))
  }
close(pb)
stopCluster(clus)

log_msg("Finish the bootstrap models")

########################################################################
# Save  ----
# Pull in all bootstrap runs and full spec to save in one file
########################################################################

log_msg("Consolidating bootstrap coefficients and saving file")

# Unpack into a data.frame
boots <- do.call(
  rbind,
  lapply(result, function(x) {
    df <- as.data.frame(x$coefs)
    df$model <- x$model
    df$n <- x$n
    df
  })
)

# saveRDS(boots, file = boot_mod_full_fn)

readr::write_csv(boots, file = boot_mod_full_ERA5_fn)

# log_msg("Script `C02 - Bootstrap.R` completed successfully")




########################################################################
# This script plots the main prevalence-temperature dose-response function
# as well as its uncertainty over 1,000 bootstrap samples
########################################################################

############################################################
# Set up ----
############################################################

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

# packages
pacman::p_load(
  lfe,
  here,
  reshape,
  tidyverse,
  lubridate,
  cowplot,
  zoo,
  patchwork,
  stringr
)

source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

############################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence 
# and CRU temperature and precipitation data aggregated to 
# the first level of Administrative division.
# Builds the country × N-year clustering variable
############################################################

print("Loading clean data")

complete <- analysis_ready_ERA5_adm1_fp |> 
  readr::read_rds() 

########################################################################
# Model coefficients ----
########################################################################

all_mods <- boot_mod_full_ERA5_fn |>
  readr::read_csv(show_col_types = FALSE)

main <- all_mods |>
  dplyr::filter(model == "main")

bootstraps <- all_mods |>
  dplyr::filter(model != "main")

########################################################################
# Spaghetti plot of estimated T response functions ----
########################################################################

conf_level <- 0.90

Tref = 24
Tmin = 10
Tmax = 40
int = 0.1
plotXtemp = cbind(seq(Tmin, Tmax, by = int), seq(Tmin, Tmax, by = int)^2)
xValsT = genRecenteredXVals_polynomial(plotXtemp, Tref, 2)

# point estimate
b <- as.matrix(c(main$temp, main$temp2))
response <- as.matrix(xValsT) %*% b

plotData <- data.frame(
  x = xValsT[, 1] + Tref,
  model = "main",
  response = as.numeric(response),
  n = length(complete$OBJECTID)
)

# collect bootstrap results as a list, then row-bind once
boot_list <- vector("list", nrow(bootstraps))

for (mod in seq_len(nrow(bootstraps))) {
  sub <- bootstraps[mod, ]
  b <- as.matrix(c(sub$temp, sub$temp2))
  boot_response <- as.numeric(as.matrix(xValsT) %*% b)

  boot_list[[mod]] <- data.frame(
    x = xValsT[, 1] + Tref,
    model = sub$model,
    response = boot_response,
    n = sub$n
  )

  if (mod %% 100 == 0) {
    print(paste0("--------- DONE WITH ITERATION ", mod, " of 1000 --------"))
  }
}

plotData <- rbind(plotData, do.call(rbind, boot_list))

percentile_data <- plotData |>
  dplyr::filter(model != "main") |>
  dplyr::group_by(x) |>
  dplyr::summarize(
    lower_bound = quantile(response, 0 + ((1 - conf_level) / 2)),
    upper_bound = quantile(response, 1 - ((1 - conf_level) / 2))
  )

########################################################################
# Temperature response ----
########################################################################

median_temps <- complete |>
  dplyr::group_by(smllrgn) |>
  dplyr::summarise(median_temp = median(temp, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    smllrgn = str_remove_all(smllrgn, "Sub-Saharan Africa \\("),
    smllrgn = str_remove_all(smllrgn, "\\)")
  )

g <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgrey", alpha = .5) +
  geom_segment(
    data = median_temps,
    aes(
      x = median_temp, # start & end x at the median
      xend = median_temp,
      y = 0, # start just above the curves
      yend = min(subset(plotData, model != "main")$response, na.rm = T)
    ),
    linewidth = .5,
    linetype = "solid",
    colour = "black"
  ) +
  geom_line(
    data = subset(plotData, model != "main"),
    aes(x = x, y = response, group = model),
    color = "#C1657C",
    alpha = .1
  ) +
  geom_line(
    data = subset(plotData, model == "main"),
    mapping = aes(x = x, y = response),
    color = "black",
    linewidth = .5
  ) +
  geom_line(
    data = percentile_data,
    aes(x = x, y = lower_bound),
    color = "black",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_line(
    data = percentile_data,
    aes(x = x, y = upper_bound),
    color = "black",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_text(
    data = median_temps,
    aes(x = median_temp, y = -30, label = smllrgn),
    angle = 90, # letters run alongside the line
    vjust = -0.3, # a little above the line tip
    hjust = 0, # left‑aligned
    size = 2
  ) +
  labs(
    x = NULL,
    y = "Prevalence (%)"
  ) +
  scale_x_continuous(
    limits = c(Tmin, Tmax),
    breaks = seq(Tmin, Tmax, by = 10),
    labels = as.character(seq(Tmin, Tmax, by = 10)),
    expand = expansion(mult = c(0.0, 0.0))
  ) +
  scale_y_continuous(
    breaks = seq(0, -40, -10),
    labels = as.character(seq(0, -40, -10)),
    expand = expansion(mult = c(0.0, 0.01))
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0, 1), units = "cm")
  )

########################################################################
# Temperature histogram inset ----
########################################################################

# Create the histogram as a separate plot
h_inset <- ggplot() +
  geom_histogram(
    data = complete,
    aes(x = temp),
    fill = "#8B3A4A",
    alpha = 1,
    bins = 30,
    colour = "black"
  ) +
  theme_void() +
  scale_x_continuous(
    limits = c(Tmin, Tmax),
    breaks = seq(Tmin, Tmax, by = 10),
    labels = as.character(seq(Tmin, Tmax, by = 10)),
    expand = expansion(mult = c(0.0, 0.0))
  ) +
  scale_y_continuous(
    breaks = seq(0, -40, -10),
    labels = as.character(seq(0, -40, -10)),
    expand = expansion(mult = c(0.0, 0.01))
  )

# Convert histogram to grob
h_grob <- ggplotGrob(h_inset)

# Add histogram to main plot
g_with_hist <- g +
  annotation_custom(
    h_grob,
    xmin = Tmin,
    xmax = Tmax, # Match x-axis range
    ymin = min(subset(plotData, model != "main")$response, na.rm = T), # Position at bottom
    ymax = -35 # Height of histogram
  ) +
  labs(x = "Mean temperature (\u00B0C)") +
  theme(
    axis.title.x = element_text(vjust = -0.5),
    plot.title.position = "plot",
    axis.text.x = element_text(),
    plot.margin = unit(c(0.0, 0.0, 1, 0), units = "cm"),
  )

g_with_hist

########################################################################
# Lagged drought and flood responses ----
########################################################################

# reformat: want a dataset of lag x var x model for flood and drought
# subset to flood and drought
mycols = c(
  colnames(bootstraps)[grep("flood", colnames(bootstraps))],
  colnames(bootstraps)[grep("drought", colnames(bootstraps))]
)

rain <- bootstraps |>
  dplyr::select(dplyr::all_of(mycols), model) |>
  dplyr::mutate(
    # calculate cumulative effect
    flood.cumu = rowSums(pick(flood:flood.lag3), na.rm = TRUE),
    drought.cumu = rowSums(pick(drought:drought.lag3), na.rm = TRUE)
  ) |>
  dplyr::relocate(model) |>
  tidyr::pivot_longer(
    cols = flood:drought.cumu,
    names_to = "var",
    values_to = "response"
  ) |>
  dplyr::mutate(
    lagst = str_extract(var, "(?<=\\.)\\w+") |> replace_na("cont"),
    lag = case_match(
      lagst,
      "lag3" ~ 3L,
      "lag2" ~ 2L,
      "lag" ~ 1L,
      "cumu" ~ -1L,
      .default = 0L
    ),
    var = case_when(
      str_detect(var, "flood") ~ "flood",
      str_detect(var, "drought") ~ "drought"
    )
  )

min_max <- c(
  min(
    subset(rain, model != "main" & var == "flood")$response,
    subset(rain, model != "main" & var == "drought")$response
  ),
  max(
    subset(rain, model != "main" & var == "flood")$response,
    subset(rain, model != "main" & var == "drought")$response
  )
)

custom_stats <- rain |>
  filter(model != "main") |>
  group_by(lag = factor(lag), var) |>
  summarise(
    ymin = quantile(response, 0.05, na.rm = TRUE),
    lower = quantile(response, 0.25, na.rm = TRUE),
    middle = quantile(response, 0.50, na.rm = TRUE),
    upper = quantile(response, 0.75, na.rm = TRUE),
    ymax = quantile(response, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

########################################################################
# Flood plot ----
########################################################################

f = ggplot() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "darkgrey",
    alpha = 0.5
  ) +
  geom_boxplot(
    data = filter(custom_stats, var == "flood"),
    aes(
      x = lag,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    color = "#43A7BA",
    fill = "#43A7BA",
    alpha = 0.35,
    size = 0.5,
    width = 0.3
  ) +
  geom_point(
    data = subset(rain, model == "main" & var == "flood"),
    aes(x = factor(lag), y = response),
    color = "black",
    alpha = 1,
    size = 0.5
  ) +
  # geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Flood (month lags)", y = NULL) +
  scale_x_discrete(
    breaks = c("-1", "0", "1", "2", "3"),
    labels = c("cumulative\neffect", "0", "1", "2", "3")
  ) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 5),
    plot.margin = unit(c(0.3, 0.3, 1, 0), units = "cm")
  ) +
  ylim(min_max)
f

########################################################################
# Drought plot ----
########################################################################

d = ggplot() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "darkgrey",
    alpha = .5
  ) +
  geom_boxplot(
    data = filter(custom_stats, var == "drought"),
    aes(
      x = lag,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    color = "#C99776",
    fill = "#C99776",
    alpha = 0.35,
    size = 0.5,
    width = 0.3
  ) +
  geom_point(
    data = subset(rain, model == "main" & var == "drought"),
    aes(x = factor(lag), y = response),
    color = "black",
    alpha = 1,
    size = .5
  ) +
  # geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Drought (month lags)", y = NULL) +
  scale_x_discrete(
    breaks = c("-1", "0", "1", "2", "3"),
    labels = c("cumulative\neffect", "0", "1", "2", "3")
  ) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 0),
    plot.margin = unit(c(0.0, 0.0, 1, 0.2), units = "cm"),
    # plot.margin = unit(c(0, 0, 0, 0), units = "cm")
  ) +
  # ylim(-5, 5) +
  scale_y_continuous(
    limits = c(-7, 5),
    expand = expansion(c(0, 0)),
    breaks = seq(-6, 4, by = 2)
  )
d

########################################################################
# Intervention plot ----
########################################################################

mycols <- c(colnames(bootstraps)[grep("intervention", colnames(bootstraps))])

inter <- bootstraps |>
  dplyr::select(dplyr::all_of(mycols), model) |>
  tidyr::pivot_longer(
    cols = -model,
    names_to = "var",
    values_to = "response"
  ) |>
  dplyr::mutate(
    var = stringr::str_replace_all(var, "I\\(intervention", "Int. "),
    var = stringr::str_replace_all(var, "\\)", "")
  )

inter_stats <- inter |>
  dplyr::filter(model != "main") |>
  dplyr::group_by(var) |>
  dplyr::summarise(
    ymin = quantile(response, 0.05, na.rm = TRUE),
    lower = quantile(response, 0.25, na.rm = TRUE),
    middle = quantile(response, 0.50, na.rm = TRUE),
    upper = quantile(response, 0.75, na.rm = TRUE),
    ymax = quantile(response, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

intervention_fig <- ggplot() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "darkgrey",
    alpha = .5
  ) +
  geom_boxplot(
    data = inter_stats,
    aes(
      x = var,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    color = "pink",
    fill = "pink",
    alpha = 0.35,
    size = 0.5,
    width = 0.3
  ) +
  geom_point(
    data = subset(inter, model == "main"),
    aes(x = factor(var), y = response),
    color = "black",
    alpha = 1,
    size = .5
  ) +
  # geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Interventions", y = NULL) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 0),
    plot.margin = unit(c(0.0, 0.0, 1, 0.2), units = "cm"),
    # plot.margin = unit(c(0, 0, 0, 0), units = "cm")
  ) +
  scale_y_continuous(
    limits = c(-7, 5),
    expand = expansion(c(0, 0)),
    breaks = seq(-6, 4, by = 2)
  )

intervention_fig

########################################################################
# Global time series ----
########################################################################

hist.to.graph <- here::here("TempFiles", "Fig2Hist.csv") |>
  vroom::vroom(show_col_types = FALSE)

future.to.graph <- here::here("TempFiles", "Fig2Future.csv") |>
  vroom::vroom(show_col_types = FALSE)

hist.to.graph |>
  filter(scenario == 'historical', year %in% c(2010:2014)) |>
  pull(median) |>
  mean() -> base

future.to.graph <- future.to.graph |>
  mutate(median = median + base, upper = upper + base, lower = lower + base)

graph.data <- bind_rows(hist.to.graph, future.to.graph) #|> dplyr::rename(scenario = RCP))

graph.data <- graph.data |>
  mutate(
    scenario = factor(
      scenario,
      levels = c('hist-nat', 'historical', 'ssp126', 'ssp245', 'ssp585')
    )
  ) |>

  ### Start plotting in 1902 and 2016 because it's the first full year with lags incorporated right.
  filter(!(year %in% c(1901, 2015))) |>
  ############ radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT LOOKING CLOSELY
  ############ this is a way of hard coding the CI's to still plot thanks to how ggplot does CI's
  ############ this is for plotting purposes ONLY and text stats give full CI's
  mutate(lower = pmax(lower, -1.75)) 

 s <- graph.data |>
  ggplot(aes(x = year, y = median, group = scenario, color = scenario)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'grey30', lwd = 0.2) +
  scale_color_manual(
    values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"),
    labels = scenario_labels,
    name = ''
  ) +
  scale_fill_manual(
    values = c("grey50", "#287DAB", "#4d5f8e", "#C582B2", "#325756"),
    labels = scenario_labels,
    name = ''
  ) +
  geom_vline(xintercept = 2014.5, linetype = 'dashed') +
  geom_line(aes(x = year, y = median), lwd = 1.3) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, colour = scenario),
    fill = NA,
    linewidth = 0.1,
    show.legend = FALSE,
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = scenario),
    color = NA,
    alpha = 0.1
  ) +
  labs(x = 'Year', y = 'Prevalence (%)') +
  scale_x_continuous(
    breaks = seq(1900, 2100, by = 50),
    labels = as.character(seq(1900, 2100, by = 50)),
    expand = expansion(mult = c(0.02, 0.01))
  ) +
  theme(
    axis.title.x = element_text(vjust = -3),
    # axis.title.y = element_text(vjust = 6),
    plot.margin = unit(c(0.0, 0.5, 0.5, 0), "cm"),
    legend.position = "inside",
    legend.position.inside = c(0.13, 0.29),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_blank(),  
    # plot.margin = unit(c(0, 0, 0, 0), units = "cm")
  )
s

########################################################################
# Plot assembly ----
########################################################################

top_row <- (g_with_hist + f + d + intervention_fig) +
  plot_layout(ncol = 4, widths = c(5, 5, 5, 2))

f2 <- top_row / s +  plot_annotation(tag_levels = 'A')

ggsave(
  filename = "Figure2_ERA5.pdf",
  plot = f2,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure2_ERA5.jpg",
  plot = f2,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in"
)

