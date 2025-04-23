########################################################################
# This script plots the main prevalence-temperature dose-response function
# as well as its uncertainty over 1,000 bootstrap samples
########################################################################

# packages
# library(ggplot2)
library(lfe)
library(reshape)
library(tidyverse)
library(cowplot)
library(tidyr)
library(zoo)
library(lubridate)
library(patchwork)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(pipeline_A_dir, "A01 - Utility code for calculations.R"))
source(here::here(pipeline_A_dir, "A02 - Utility code for plotting.R"))
source(here::here(pipeline_A_dir, "A03 - Prep data for estimation.R"))

########################################################################
# A. Read in saved regression results
########################################################################

# load main model (full sample)

#### Bootstrap coefficients ----
main <- file.path(
  datadir,
  "Results",
  "Models",
  "coefficients_cXt2intrXm.rds"
) |>
  readRDS()

# main <- readRDS("./Results/Models/coefficients_cXt2intrXm.rds")
coefs <- main[, 1]
names(coefs) <- rownames(main)
main <- as.data.frame(t(as.matrix(coefs)))

# function to compute optimal temp for each run
optT <- function(beta1, beta2) {
  opt = -beta1 / (2 * beta2)
  return(opt)
}

# load bootstraps into one dataframe
df = as.data.frame(main)
df$model = "main"

files = list.files(file.path(datadir, "Results", "Models", "bootstrap"))
stop = length(files) - 1
files = files[1:stop]

for (f in 1:length(files)) {
  tmp = readRDS(file.path(datadir, "Results", "Models", "bootstrap", files[f]))
  tmp = as.data.frame(tmp)
  tmp$model = paste0("boot", f)
  df = df %>% add_row(as.data.frame(tmp))
}

########################################################################
# C. Spaghetti plot of estimated T response functions
########################################################################

#setup
Tref = 24
Tmin = 10
Tmax = 40
int = 0.1
plotXtemp = cbind(seq(Tmin, Tmax, by = int), seq(Tmin, Tmax, by = int)^2)
xValsT = genRecenteredXVals_polynomial(plotXtemp, Tref, 2)

# point estimate
main = subset(df, model == "main")
b = as.matrix(c(main$temp, main$temp2))
response = as.matrix(xValsT) %*% b #Prediction

plotData = data.frame(x = xValsT[, 1] + Tref, response = response)

#plotData$model = "main"

# data <- file.path(datadir, "Data", "CRU-Reextraction-Aug2022.csv") |>
#   read.csv() |>
#   drop_na(PfPr2)

# loop over all bootstraps, add to dataframe
for (mod in 1:dim(df)[1]) {
  #dim(df)[1]
  sub = df[mod, ]
  b = as.matrix(c(sub$temp, sub$temp2))
  boot = as.data.frame(as.matrix(xValsT) %*% b) #Prediction
  colnames(boot) = paste0("boot", mod)
  plotData = cbind(plotData, boot)

  # progress
  if (mod / 100 == round(mod / 100)) {
    print(paste0('--------- DONE WITH ITERATION ', mod, ' of 1000 --------'))
  }
}

# reshape
colnames(plotData)[3] = "boot1"
plotData = plotData %>% gather(plotData, response, boot1:boot1001)
colnames(plotData) = c("x", "model", "response")

percentile_data <- plotData %>%
  dplyr::filter(model != "boot1") %>%
  group_by(x) %>%
  summarize(
    p05 = quantile(response, 0.05),
    p95 = quantile(response, 0.95)
  )

# data <- file.path(datadir, "Data", "CRU-Reextraction-Aug2022.csv") |>
#   read.csv() |>
#   drop_na(PfPR2)

median_temps <- complete %>%
  group_by(smllrgn) %>%
  summarise(median_temp = median(temp, na.rm = TRUE)) %>%
  ungroup() |>
  mutate(
    smllrgn = str_remove_all(smllrgn, "Sub-Saharan Africa \\("),
    smllrgn = str_remove_all(smllrgn, "\\)")
  )

g <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgrey", alpha = .5) +
  geom_line(
    data = subset(plotData, model != "boot1"),
    aes(x = x, y = response, group = model),
    color = "#C1657C",
    alpha = .1
  ) +
  geom_line(
    data = subset(plotData, model == "boot1"),
    mapping = aes(x = x, y = response),
    color = "black",
    linewidth = 1
  ) +
  geom_line(
    data = percentile_data,
    aes(x = x, y = p05),
    color = "black",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_line(
    data = percentile_data,
    aes(x = x, y = p95),
    color = "black",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  # geom_vline(
  #   data = median_temps,
  #   aes(xintercept = median_temp),
  #   linewidth = 0.5,
  #   linetype = "solid",
  #   colour = "black"
  # ) +
  geom_segment(
    data = median_temps,
    aes(
      x = median_temp, # start & end x at the median
      xend = median_temp,
      y = 0, # start just above the curves
      yend = min(subset(plotData, model != "boot1")$response, na.rm = T)
    ),
    linewidth = .5,
    linetype = "solid",
    colour = "black"
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
  xlim(Tmin, Tmax) +
  ylim(
    min(subset(plotData, model != "boot1")$response, na.rm = T),
    max(subset(plotData, model != "boot1")$response, na.rm = T)
  ) +
  # scale_y_continuous(
  #   # expand = expansion(mult = c(0.1, 0.1)),
  #   breaks = seq(0, -40, -10)
  # ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0, 1), units = "cm")
  )

h <- ggplot(complete, aes(x = temp)) +
  geom_histogram(
    fill = "#8B3A4A",
    alpha = 1,
    bins = 30,
    width = 0.7,
    colour = "black"
  ) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  xlim(Tmin - .0001, Tmax) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(-2, 0.3, 0.3, 1), "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

combined_plot = plot_grid(
  g +
    theme(
      axis.text.x = element_blank(), # Remove x-axis text
      axis.line.x = element_blank(), # Remove x-axis line
      axis.ticks.x = element_blank(), # Remove x-axis ticks
      axis.title.x = element_blank() # Remove x-axis title
    ),
  h + labs(x = expression(paste("Mean temperature (", degree, "C)"))), # Add the PDF plot
  align = "v", # Align vertically
  nrow = 2, # Use two rows
  rel_heights = c(10, 1) # Set relative heights of the plots
)

combined_plot


# # Combine the plots
# combined_plot <- g / h +
#   plot_layout(heights = c(15, 1)) +  # Main plot taller, histogram narrower vertically
#   plot_annotation(
#     caption = expression(paste("Mean temperature (", degree, "C)"))
#   ) &
#   theme(
#     plot.caption = element_text(hjust = 0.5, size = 12)
#   )

# combined_plot

########################################################################
# D. Lagged drought and flood responses
########################################################################

# reformat: want a dataset of lag x var x model for flood and drought
# subset to flood and drought
mycols = c(
  colnames(df)[grep("flood", colnames(df))],
  colnames(df)[grep("drought", colnames(df))]
)
rain = df %>% dplyr::select(dplyr::all_of(mycols), model)

# calculate cumulative effect
rain %>%
  dplyr::select(flood:flood.lag3) %>%
  rowSums(na.rm = TRUE) -> rain$flood.cumu
rain %>%
  dplyr::select(drought:drought.lag3) %>%
  rowSums(na.rm = TRUE) -> rain$drought.cumu
rain = rain %>% relocate(model)

# reshape long & format lags
rain = rain %>% gather(rain, response, flood:drought.cumu)
colnames(rain) = c("model", "var", "response")
rain$lagst = sapply(strsplit(as.character(rain$var), "\\."), `[`, 2)
rain$lag = ifelse(rain$lagst == "lag3", 3, NA)
rain$lag = ifelse(rain$lagst == "lag2", 2, rain$lag)
rain$lag = ifelse(rain$lagst == "lag", 1, rain$lag)
rain$lag = ifelse(rain$lagst == "cumu", -1, rain$lag)
rain$lag = ifelse(is.na(rain$lag), 0, rain$lag)

rain$lagst = ifelse(is.na(rain$lagst), "cont", rain$lagst)
rain$var = ifelse(grepl("flood", rain$var), "flood", rain$var)
rain$var = ifelse(grepl("drought", rain$var), "drought", rain$var)

custom_stats <- rain %>%
  filter(model != "main") %>%
  group_by(lag = factor(lag), var) %>%
  summarise(
    ymin = quantile(response, 0.05, na.rm = TRUE),
    lower = quantile(response, 0.25, na.rm = TRUE),
    middle = quantile(response, 0.50, na.rm = TRUE),
    upper = quantile(response, 0.75, na.rm = TRUE),
    ymax = quantile(response, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# plot - flood
f = ggplot() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "darkgrey",
    alpha = .5
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
    alpha = 0.25,
    size = 1,
    width = 0.3
  ) +
  # geom_boxplot(
  #   data = subset(rain, model != "main" & var == "flood"),
  #   aes(x = factor(lag), y = response),
  #   color = "#43A7BA",
  #   fill = "#43A7BA",
  #   alpha = .25,
  #   size = 1,
  #   width = 0.3
  # ) +
  # geom_point(
  #   data = subset(rain, model != "main" & var == "flood"),
  #   aes(x = factor(lag), y = response),
  #   color = "#43A7BA",
  #   alpha = .1,
  #   size = 1.75
  # ) +
  geom_point(
    data = subset(rain, model == "main" & var == "flood"),
    aes(x = factor(lag), y = response),
    color = "black",
    alpha = 1,
    size = 1
  ) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Flood (month lags)", y = NULL) +
  # ylab(NULL) +
  # xlab("Flood (month lags)") +
  scale_x_discrete(
    breaks = c("-1", "0", "1", "2", "3"),
    labels = c("cumulative\neffect", "0", "1", "2", "3")
  ) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 5),
    plot.margin = unit(c(0.3, 0.3, 1, 0), units = "cm")
  ) +
  ylim(-5, 5)

f

# plot- drought
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
    alpha = 0.25,
    size = 1,
    width = 0.3
  ) +
  # geom_boxplot(
  #   data = subset(rain, model != "main" & var == "drought"),
  #   aes(x = factor(lag), y = response),
  #   color = "#C99776",
  #   fill = "#C99776",
  #   alpha = .25,
  #   size = 1,
  #   width = 0.3
  # ) +
  # geom_point(
  #   data = subset(rain, model != "main" & var == "drought"),
  #   aes(x = factor(lag), y = response),
  #   color = "#C99776",
  #   alpha = .05,
  #   size = 1.75
  # ) +
  geom_point(
    data = subset(rain, model == "main" & var == "drought"),
    aes(x = factor(lag), y = response),
    color = "black",
    alpha = 1,
    size = 1
  ) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Drought (month lags)", y = NULL) +
  # ylab(NULL) +
  # xlab("Drought (month lags)") +
  scale_x_discrete(
    breaks = c("-1", "0", "1", "2", "3"),
    labels = c("cumulative\neffect", "0", "1", "2", "3")
  ) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 0),
    plot.margin = unit(c(0.3, 0.3, 1, 0), units = "cm")
  ) +
  ylim(-5, 5)
d

########################################################################
# Middle row multipanel
########################################################################

# g + f + d

combined_plot + f + d
