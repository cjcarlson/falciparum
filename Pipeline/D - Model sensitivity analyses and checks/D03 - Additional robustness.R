################################################################################
# This script conducts additional robustness checks. This script should be
# incorporated into D01 when a subset of tests are included in the main text
# and/or Supplement.
################################################################################
# Set up ----
################################################################################

rm(list = ls())

# packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  here,
  lfe,
  reshape,
  stargazer,
  tidyverse,
  zoo,
  lubridate,
  cowplot,
  multcomp,
  sf
)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A01 - Configuration.R"))
source(A_utils_calc_fp)
source(A_utils_plot_fp)

################################################################################
# Load data ----
# Read in the analysis ready data file with malaria prevalence
# and CRU temperature and precipitation data aggregated to
# the first level of Administrative division.
################################################################################

print("Loading clean data")
complete <- readr::read_rds(analysis_ready_CRU_adm1_fp)

################################################################################
# Prevalence lag ----
# Check for missing values if we were to use prevalence lag
################################################################################

# First ensure that for each OBJECTID and year we have months 1 through 12
# and years 1902 through 2016 (1 row for each month of each year).
# This intentionally introduces many NAs, but allows us to calculate the lag.
complete_expanded <- complete %>%
  mutate(
    year = as.numeric(as.character(year)),
    month = as.character(month),
    month = match(month, month.abb)
  ) |>
  group_by(OBJECTID) %>%
  complete(year = 1902:2016, month = 1:12) %>%
  ungroup()

# Add prevalence lag to complete
complete_with_lag <- complete_expanded %>%
  arrange(OBJECTID, year, month) %>%
  mutate(PfPR2_lag = dplyr::lag(PfPR2)) |>
  tidyr::drop_na(PfPR2)

# Drop rows with missing lag values
complete_with_complete_lags <- tidyr::drop_na(complete_with_lag)

# Calculate the percentage of observations lost
starting_obs <- length(complete$OBJECTID)
obs_after_lag <- length(complete_with_complete_lags$OBJECTID)
difference <- starting_obs - obs_after_lag
percent_lost <- (difference / starting_obs) * 100

################################################################################
# Main model ----
# main spec: cXt2intrXm
################################################################################

mainmod = readRDS(main_mod_obj_fn)

################################################################################
# Overdispersion? ----
################################################################################

# Plot model residuals
complete <- complete |>
  mutate(res = c(residuals(mainmod)))

g <- ggplot(data = complete) +
  geom_histogram(aes(x = res), color = "seagreen", fill = "seagreen") +
  xlab("model residuals") +
  theme_classic()
g

ggsave(
  filename = "model_residuals.jpg",
  path = figure_res_dir,
  plot = g,
  width = 7,
  height = 7
)

################################################################################
# Does diagnostic method change with T and P shocks? ----
################################################################################

complete_dm <- complete |>
  dplyr::mutate(
    microscopy = simplified_METHOD == "MICROSCOPY"
  ) |>
  filter(dominant_METHOD != "LAMP")

complete_dm$dominant_METHOD = as.factor(complete_dm$dominant_METHOD)
complete_dm$simplified_METHOD = as.factor(complete_dm$simplified_METHOD)

complete_dm$month = as.factor(complete_dm$month)
complete_dm$year = as.factor(complete_dm$year)

# new outcome var: Probability method is XX
PrMicro = as.formula(
  paste0(
    "microscopy ~ temp + temp2 + ",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + ",
    country_time,
    " | OBJECTID  + as.factor(smllrgn):month | 0 | ",
    clustering
  )
)

Micromod = felm(data = complete_dm, formula = PrMicro)

stargazer(
  Micromod,
  title = "Microscopy method test",
  align = TRUE,
  keep = c("temp", "flood", "drought"),
  out = file.path(table_sens_dir, "Microscopy_sample_sensitivity.tex"),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  star.cutoffs = table_star_cutoffs
)

################################################################################
# Control for diagnostic method ----
################################################################################

cXt2intrXmDM = as.formula(
  paste0(
    common,
    " + dominant_METHOD + I(intervention) + ",
    country_time,
    " | OBJECTID  + as.factor(smllrgn):month | 0 | ",
    clustering
  )
)
cXt2intrXmSM = as.formula(
  paste0(
    common,
    " + simplified_METHOD + I(intervention) + ",
    country_time,
    " | OBJECTID  + as.factor(smllrgn):month | 0 | ",
    clustering
  )
)

myforms = c(cXt2intrXm, cXt2intrXmDM, cXt2intrXmSM)

mycollabs = c(
  "main specification", # Main Spec
  "+ diag. method (full set)", # Main Spec with dominant method
  "+ diag. method (small set)" # Main Spec with simplified method
)

modellist = list()
i = 0
for (m in myforms) {
  i = i + 1
  modellist[[i]] = felm(data = complete_dm, formula = m)
}

mynote = "Column specifications: (1) country-specific quad. trends, intervention year FE, GBOD region-by-month FE; (2) same as (1), but with additional controls for diagnostic method: Microscopy, Microcscopy/PCR Confirmed, PCR, RDT, RDT/PCR Confirmed, and RDT/SLIDE Confirmed; (3) same as (2) but using simplified diagnostic method control set: Microscopy, PCR, RDT."

stargazer(
  modellist,
  title = "Sensitivity to controlling for diagnostic method",
  align = TRUE,
  column.labels = mycollabs,
  keep = c("temp", "flood", "drought", "METHOD"),
  # out = file.path(table_sens_dir, "Diagnostic_method.tex"),
  out = here::here("Results", "Tables", "Diagnostic_method.tex"),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  digits = 2,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}"),
  star.cutoffs = table_star_cutoffs
)

####### Plot temperature responses #######

plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

figList = list()
for (m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]],
    "temp",
    plotXtemp,
    polyOrder = 2,
    cluster = T,
    xRef = myrefT,
    xLab = expression(paste("Mean temperature (", degree, "C)")),
    yLab = "Prevalence (%)",
    title = mycollabs[m],
    yLim = c(-30, 10),
    showYTitle = T
  ) +
    theme(plot.title = element_text(size = 10))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], nrow = 1)
p

ggsave(
  filename = "diagnostic_method_sensitivity.jpg",
  path = figure_fe_dir,
  plot = p,
  width = 9,
  height = 3
)

################################################################################
# Data imbalance: responses on temporal subsamples ----
################################################################################
 
complete = complete |> mutate(yearnum = as.numeric(as.character(year)))
g = ggplot(complete) +
  geom_histogram(aes(x = yearnum), color = "seagreen", fill = "seagreen") +
  xlab("year") +
  ylab("count of observations") +
  theme_classic()
g
 
# obs by group
complete = complete |> mutate(post1995 = (yearnum >= 1995))
complete %>% count(post1995)
 
# formula (different intervention dummies for each temporal subsample)
cXt2rXm = as.formula(paste0(
  common,
  " + I(intervention) +  ",
  country_time,
  " | OBJECTID  + as.factor(smllrgn):month | 0 | ",
  clustering
))
 
pre_data <- subset(complete, post1995 == FALSE)
pos_data <- subset(complete, post1995 == TRUE)
 
pre1995 = felm(data = pre_data, formula = cXt2rXm)
post1995 = felm(data = pos_data, formula = cXt2rXm)
 
# plot temperature responses
modellist = list(pre1995, post1995)
mycollabs = c(
  "Early sample (1901-1994)",
  "Late sample (1995-2016)"
)
 
percentiles_list = list()
pre_post <- c(F, T)
for (i in 1:length(pre_post)) {
  # i <- 1
  pre_post_data <- subset(complete, post1995 == pre_post[i])$temp
  temp_p01 <- quantile(pre_post_data, 0.01, na.rm = TRUE)
  temp_p99 <- quantile(pre_post_data, 0.99, na.rm = TRUE)
  percentiles_list[[i]] <- list(
    p01 = temp_p01,
    p99 = temp_p99,
    n = length(pre_post_data)
  )
}
 
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)
figList = list()
for (m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]],
    "temp",
    plotXtemp,
    polyOrder = 2,
    cluster = T,
    xRef = myrefT,
    xLab = expression(paste("Mean temperature (", degree, "C)")),
    yLab = "Prevalence (%)",
    title = mycollabs[m],
    yLim = c(-35, 10),
    showYTitle = T,
    plotmax_x = 2,
    plotmax_y = 5
  ) +
    theme(plot.title = element_text(size = 10)) +
    geom_vline(
      xintercept = percentiles_list[[m]]$p01,
      colour = "grey39",
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = percentiles_list[[m]]$p99,
      colour = "grey39",
      linetype = "dashed"
    ) +
    annotate(
      geom = "text",
      x = 36,
      y = 10,
      vjust = -1,
      label = paste0("italic(N) == ", percentiles_list[[m]]$n),
      size = 3,
      parse = TRUE
    )
}
 
# Create histogram grobs for each subsample (F02-style inset approach)
hist_data_list <- list(pre_data, pos_data)
yLim_split <- c(-37, 10)
hist_ymin <- yLim_split[1]       # bottom of the response plot y-axis
hist_ymax <- hist_ymin + 5       # height of the histogram band
 
for (m in 1:length(hist_data_list)) {
  # Build a void histogram matching the x-axis of the response plot
  hist_inset <- ggplot() +
    geom_histogram(
      data = hist_data_list[[m]],
      mapping = aes(x = temp),
      fill = "#8B3A4A",
      alpha = 1,
      bins = 30,
      colour = "black"
    ) +
    theme_void() +
    scale_x_continuous(
      limits = c(Tmin, Tmax),
      expand = expansion(mult = c(0.0, 0.0))
    )
 
  # Convert to grob
  hist_grob <- ggplotGrob(hist_inset)
 
  # Add grob inset to each response figure
  figList[[m]] <- figList[[m]] +
    annotation_custom(
      grob = hist_grob,
      xmin = Tmin,
      xmax = Tmax,
      ymin = hist_ymin,
      ymax = hist_ymax
    ) +
    labs(x = expression(paste("Mean temperature (", degree, "C)"))) +
    theme(
      axis.title.x = element_text(vjust = -0.5),
      plot.title.position = "plot"
    )
}
 
p <- plot_grid(figList[[1]], figList[[2]], nrow = 1)
p
 
ggsave(
  filename = "Supp_Figure_split_sample_1995.jpg",
  path = here::here("Results", "Figures"),
  plot = p,
  width = 10,
  height = 5
)

################################################################################
# Data imbalance: responses on spatial subsamples ----
################################################################################
#### quadratic
# Regression for each region, no regionXmo FE because we are using region-specific models
regions = unique(complete$smllrgn)
cXt2int = as.formula(paste0(
  common,
  " + I(intervention) + ",
  country_time,
  " | OBJECTID  + as.factor(month) | 0 | ",
  clustering
))

modellist = list()
for (i in 1:length(regions)) {
  mydf = subset(complete, smllrgn == regions[i])
  modellist[[i]] = felm(data = mydf, formula = cXt2int)
}

percentiles_list = list()
for (i in 1:length(regions)) {
  region_data <- subset(complete, smllrgn == regions[i])$temp
  temp_p01 <- quantile(region_data, 0.01, na.rm = TRUE)
  temp_p99 <- quantile(region_data, 0.99, na.rm = TRUE)
  percentiles_list[[i]] <- list(
    p01 = temp_p01,
    p99 = temp_p99,
    n = length(region_data)
  )
  cat(regions[i], ": ", length(region_data), "\n")
}

# Plot them all next to each other
mycollabs = c(
  paste0(regions[1]),
  paste0(regions[2]),
  paste0(regions[3]),
  paste0(regions[4])
)

plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)
figList = list()
refTemps = numeric(length(modellist))
plot_ref_vec = c(F, T, T, T)
for (m in 1:length(modellist)) {
  coefs = summary(modellist[[m]])$coefficients[1:2]
  myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10)
  refTemps[m] = myrefT
  plot_ref = plot_ref_vec[m]
  figList[[m]] = plotPolynomialResponse(
    modellist[[m]],
    "temp",
    plotXtemp,
    polyOrder = 2,
    cluster = T,
    xRef = myrefT,
    plotmax = plot_ref,
    xLab = expression(paste("Mean temperature (", degree, "C)")),
    yLab = "Prevalence (%)",
    title = mycollabs[m],
    yLim = c(-30, 10),
    showYTitle = T
  ) +
    theme(plot.title = element_text(size = 10)) +
    geom_vline(
      xintercept = percentiles_list[[m]]$p01,
      colour = "grey39",
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = percentiles_list[[m]]$p99,
      colour = "grey39",
      linetype = "dashed"
    )
}

# p = plot_grid(figList[[1]], figList[[2]], figList[[3]], figList[[4]], nrow=1)
# p
# ggsave(
#   filename = "split_GBOD_2nd_poly.jpg",
#   # path = figure_sub_dir,
#   plot = p,
#   width = 12,
#   height = 4
# )

histList = list()
for (i in 1:length(regions)) {
  region_data <- subset(complete, smllrgn == regions[i])
  temp_p01 <- percentiles_list[[i]]$p01
  temp_p99 <- percentiles_list[[i]]$p99
  n <- percentiles_list[[i]]$n

  t_hist <- ggplot(region_data, aes(x = temp)) +
    geom_histogram(
      fill = "#8B3A4A",
      alpha = 1,
      bins = 30,
      width = 0.7,
      colour = "black"
    ) +
    theme_classic() +
    labs(x = expression(paste("Mean temperature (", degree, "C)")), y = NULL) +
    xlim(Tmin, Tmax) +
    geom_vline(xintercept = temp_p01, colour = "grey39", linetype = "dashed") +
    geom_vline(xintercept = temp_p99, colour = "grey39", linetype = "dashed") +
    annotate(
      geom = "text",
      x = 36,
      y = 0,
      vjust = -1,
      label = paste0("italic(N) == ", n),
      size = 3,
      parse = TRUE
    ) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = unit(c(-1.5, 0, 0, 0), "cm"),
    )
  plot_ref <- plot_ref_vec[i]
  if (plot_ref == T) {
    t_hist <- t_hist +
      geom_vline(xintercept = refTemps[i], colour = "grey39")
  }
  histList[[i]] <- t_hist
}

# Combine response plots and histograms
p = plot_grid(
  figList[[1]] +
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ),
  figList[[2]] +
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ),
  figList[[3]] +
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ),
  figList[[4]] +
    theme(
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ),
  histList[[1]],
  histList[[2]],
  histList[[3]],
  histList[[4]],
  nrow = 2,
  align = "v",
  rel_heights = c(15, 1)
)

# p
ggsave(
  filename = "split_GBOD_temp_hist.jpg",
  path = figure_sub_dir,
  plot = p,
  width = 12,
  height = 4
)

################################################################################
# End of file ----
################################################################################
