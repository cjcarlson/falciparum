############################################################
# This script estimates the main empirical specification linking
# PfPR2 to drought, flood, and temperature.
############################################################

############################################################
# Set up
############################################################

rm(list = ls())

# packages
library(here)
library(lfe)
library(reshape)
library(stargazer)
library(tidyverse)
library(zoo)
library(lubridate)
library(cowplot)
library(multcomp)
library(patchwork)

# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(
  "Pipeline",
  "A - Utility functions",
  "A01 - Utility code for calculations.R"
))
source(here::here(
  "Pipeline",
  "A - Utility functions",
  "A02 - Utility code for plotting.R"
))

# CRUversion = "4.03" # "4.06"
if (CRUversion == "4.03") {
  resdir = file.path(datadir, "Results")
} else if (CRUversion == "4.06") {
  resdir = file.path(datadir, "Results_CRU-TS4-06")
} else {
  print('CRU version not supported! Use 4.03 or 4.06.')
}

############################################################
# Plotting toggles
# Choose reference temperature for response function, as well
# as minimum and maximum for range of temperature
############################################################

Tref = 24 #reference temperature - curve gets recentered to 0 here
Tmin = 10 #min T for x axis
Tmax = 40 #max T for x axis

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
complete <- readr::read_csv(
  file.path(
    datadir,
    "Data",
    paste0('CRU-', CRUversion, '-Points-Reextraction-May2025.csv')
  ),
  show_col_types = FALSE
)

# cont <- sf::read_sf(here::here(datadir, 'Data', 'AfricaADM1.shp'))

# # Convert to sf object with POINT geometry
# prev_sf <- sf::st_as_sf(
#   prev_df,
#   coords = c("Long", "Lat"),
#   crs = 4326
# )

# # Join the prevalence data to the continent shapefile
# prev_with_cont <- sf::st_join(prev_sf, cont)

# include: contemporaneous temp, then distributed lag in flood and drought
floodvars = paste(
  colnames(complete)[grep("flood", colnames(complete))],
  collapse = " + "
)
droughtvars = paste(
  colnames(complete)[grep("drought", colnames(complete))],
  collapse = " + "
)

# need this for country specific quadratic trends
complete$monthyr2 = complete$monthyr^2

# define key intervention periods
complete$intervention = ifelse(
  complete$yearnum >= 1955 & complete$yearnum <= 1969,
  1,
  0
)
complete$intervention[complete$yearnum >= 2000 & complete$yearnum <= 2015] = 2
complete$intervention = as.factor(complete$intervention)

# classes: important for ensuring felm is treating these correctly
complete$month = as.factor(complete$month)
complete$year = as.factor(complete$year)

complete <- dplyr::rename(complete, country = COUNTRY)


gbod <- sf::read_sf(
  file.path(
    datadir,
    "Data",
    "OriginalGBD",
    "WorldRegions.shp"
  )
)
# head(gbod@data)

gboddf = as.data.frame(gbod)
gboddf = gboddf %>% dplyr::select("ISO", "NAME_0", "Region", "SmllRgn")
gboddf = gboddf %>%
  group_by(ISO, NAME_0) %>%
  summarize(Region = first(Region), SmllRgn = first(SmllRgn)) # note that the small regions are homogenous within country
colnames(gboddf) = c("ISO", "country", "region", "smllrgn")
gboddf$country = as.character(gboddf$country)

# clean to merge
gboddf$country = ifelse(
  gboddf$country == "Cote D'Ivoire",
  "Côte d'Ivoire",
  gboddf$country
)

complete$country = as.character(complete$country)
complete = left_join(complete, gboddf, by = "country")
complete$country = as.factor(complete$country)

# complete$dominant_METHOD = as.factor(complete$dominant_METHOD)
# complete$simplified_METHOD = as.factor(complete$simplified_METHOD)

#### Create necessary subfolders
dir.create(file.path(resdir, "Tables"), showWarnings = FALSE)
dir.create(file.path(resdir, "Figures"), showWarnings = FALSE)
dir.create(file.path(resdir, "Models"), showWarnings = FALSE)

########################################################################
# Estimation
########################################################################

# Formula (see other files for robustness/sensitivity checks)
cXt2intrXm = as.formula(
  paste0(
    "`PfPR2-10` ~ temp + temp2 + ",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)

# Estimation & save model results
highresmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(highresmod$coefficients)
vcov = as.data.frame(highresmod$clustervcv)
dir.create(file.path(resdir, "Models", "reproducibility"), showWarnings = FALSE)
bfn = file.path(
  resdir,
  "Models",
  "reproducibility",
  "coefficients_cXt2intrXm_highres.rds"
)
vfn = file.path(
  resdir,
  "Models",
  "reproducibility",
  "vcv_cXt2intrXm-highres.rds"
)
saveRDS(coeffs, file = bfn)
saveRDS(vcov, file = vfn)

# Stargazer output
mynote = "High Resolution Model: Country-specific quad. trends with intervention FE and country by month FE."
dir.create(file.path(resdir, "Tables", "main"), showWarnings = FALSE)
stargazer(
  highresmod,
  title = "PfPR2 response to daily avg. temperature",
  align = TRUE,
  keep = c("temp", "flood", "drought", "intervention"),
  out = file.path(
    resdir,
    "Tables",
    "main",
    "main_specification_cXt2intrXm-highres.tex"
  ),
  omit.stat = c("f", "ser"),
  out.header = FALSE,
  type = "latex",
  float = F,
  notes.append = TRUE,
  notes.align = "l",
  notes = paste0("\\parbox[t]{\\textwidth}{", mynote, "}")
)

########################################################################
# Plot (Note: analogous to Fig 2A but with analytically derived confidence intervals
# in place of bootstrap runs shown in Fig 2A)
########################################################################

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

coefs = summary(highresmod)$coefficients[1:2]
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) # plot relative to max of quadratic function

beta <- highresmod$coefficients
vars <- rownames(beta)
patternForPlotVars <- "temp"
plotVars <- vars[grepl(patternForPlotVars, vars)]

# t <- plotPolynomialResponse(
#   mod = highresmod,
#   patternForPlotVars = "temp",
#   xVals = plotXtemp,
#   polyOrder = 2,
#   cluster = TRUE,
#   xRef = myrefT,
#   xLab = expression(paste("Mean temperature (", degree, "C)")),
#   yLab = "Prevalence (%)",
#   title = NULL,
#   yLim = c(-30, 10),
#   showYTitle = TRUE
# ) 

# ylims <- c(-3, 3)

# d <- plotLinearLags(
#   mod = highresmod,
#   patternForPlotVars = "drought",
#   cluster = TRUE,
#   laglength = 3,
#   xLab = "Drought Lag",
#   yLab = "Coefficient",
#   title = NULL,
#   yLim = ylims
# )

# f <- plotLinearLags(
#   mod = highresmod,
#   patternForPlotVars = "flood",
#   cluster = TRUE,
#   laglength = 3,
#   xLab = "Flood Lag",
#   yLab = "Coefficient",
#   title = NULL,
#   yLim = ylims
# )

# combined_plot <- t +
#   d +
#   f +
#   plot_layout(ncol = 3, guides = "collect") &
#   theme(
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 8),
#     # legend.position = "bottom", 
#     # legend.margin = margin(0, 0, 0, 0)
#   )

# combined_plot

# dir.create(
#   file.path(resdir, "Figures", "Diagnostics", "Main_model"),
#   showWarnings = FALSE
# )
# ggsave(
#   filename = "temp_response_cXt2intrXm-highres.pdf",
#   path = file.path(resdir, "Figures", "Diagnostics", "Main_model"),
#   plot = combined_plot,
#   width = 7,
#   height = 2.5,
# )


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
source(here::here(
  "Pipeline",
  "A - Utility functions",
  "A03 - Prep data for estimation.R"
))

#### Create necessary subfolders
dir.create(file.path(resdir, "Tables"), showWarnings = FALSE)
dir.create(file.path(resdir, "Figures"), showWarnings = FALSE)
dir.create(file.path(resdir, "Models"), showWarnings = FALSE)

########################################################################
# Estimation
########################################################################

# Formula (see other files for robustness/sensitivity checks)
cXt2intrXm = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ",
    floodvars,
    " + ",
    droughtvars,
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)

# Estimation & save model results
mainmod = felm(data = complete, formula = cXt2intrXm)
coeffs = as.data.frame(mainmod$coefficients)
vcov = as.data.frame(mainmod$clustervcv)
dir.create(file.path(resdir, "Models", "reproducibility"), showWarnings = FALSE)
bfn = file.path(
  resdir,
  "Models",
  "reproducibility",
  "coefficients_cXt2intrXm.rds"
)
vfn = file.path(resdir, "Models", "reproducibility", "vcv_cXt2intrXm.rds")
saveRDS(coeffs, file = bfn)
saveRDS(vcov, file = vfn)

# Temperature support
plotXtemp = cbind(seq(Tmin, Tmax), seq(Tmin, Tmax)^2)

coefs = summary(mainmod)$coefficients[1:2]
myrefT = max(round(-1 * coefs[1] / (2 * coefs[2]), digits = 0), 10) # plot relative to max of quadratic function



plotPolynomialResponse_2_mod = function(
  mod, 
  patternForPlotVars, 
  xVals, 
  polyOrder, 
  lag = NA, 
  plotmax = T, 
  cluster = T, 
  xRef = 0, 
  fillcolor = "#C1657C", 
  xLab, 
  yLab, 
  title = "title", 
  yLim = c(-1, 1), 
  showYTitle = T,
  mod2 = NULL,
  model1_name = "Model 1",
  model2_name = "Model 2",
  fillcolor2 = "#43A7BA"
) {
  
  # Function to extract polynomial response data for a single model
  extractPolynomialData = function(model, pattern, xValues, polyOrd, lagVal, clusterFlag, xReference) {
    # Handle different model types
    if (inherits(model, "felm")) {
      beta = model$coefficients 
      vars = rownames(beta)
    } else if (inherits(model, "fixest")) {
      beta = model$coefficients
      vars = names(beta)
      beta = as.matrix(beta)
      rownames(beta) = vars
    } else {
      beta = model$coefficients
      vars = names(beta)
      beta = as.matrix(beta)
      rownames(beta) = vars
    }
    
    # Get the variables that we're plotting
    plotVars = vars[grepl(pattern = pattern, x = vars)] 
    
    # Recenter Xs so predictions are relative to the reference T
    xValsT = genRecenteredXVals_polynomial(xValues, xReference, polyOrd, lagVal)
    
    # Get the estimated variance covariance matrix
    if (clusterFlag == T) {
      if (inherits(model, "fixest")) {
        vcov_full = vcov(model)
        vcov = getVcov(vcov_full, plotVars)
      } else {
        vcov = getVcov(model$clustervcv, plotVars)
      }
    } else {
      if (inherits(model, "fixest")) {
        vcov_full = vcov(model, se = "iid")
        vcov = getVcov(vcov_full, plotVars)
      } else {
        vcov = getVcov(model$vcv, plotVars)
      }
    }
    
    b = as.matrix(beta[rownames(beta) %in% plotVars])
    
    response = as.matrix(xValsT) %*% b
    length = 1.96 * sqrt(apply(X = xValsT, FUN = calcVariance, MARGIN = 1, vcov))
    
    lb = response - length
    ub = response + length
    
    # Return data with x recentered to xRef
    return(data.frame(
      x = xValsT[,1] + xReference, 
      response = response, 
      lb = lb, 
      ub = ub
    ))
  }
  
  # Extract data for first model
  plotData1 = extractPolynomialData(mod, patternForPlotVars, xVals, polyOrder, lag, cluster, xRef)
  plotData1$model = model1_name
  
  # If second model provided, extract its data too
  if (!is.null(mod2)) {
    plotData2 = extractPolynomialData(mod2, patternForPlotVars, xVals, polyOrder, lag, cluster, xRef)
    plotData2$model = model2_name
    plotData = rbind(plotData1, plotData2)
  } else {
    plotData = plotData1
  }
  
  # Calculate maximum points for vertical lines if needed
  if (plotmax == T) {
    sub1 = plotData1[plotData1$x >= 10 & plotData1$x <= 30, ]
    maxX1 = max(sub1$x[sub1$response == max(sub1$response)])
    
    if (!is.null(mod2)) {
      sub2 = plotData2[plotData2$x >= 10 & plotData2$x <= 30, ]
      maxX2 = max(sub2$x[sub2$response == max(sub2$response)])
    }
  }
  
  # Create the plot
  if (is.null(mod2)) {
    # Single model plot (original behavior)
    if (sum(is.na(yLim)) > 0) {
      g = ggplot(data = plotData) + 
        geom_hline(yintercept = 0, color = "grey88") +
        geom_ribbon(aes(x, ymin = lb, ymax = ub), alpha = 0.4, fill = fillcolor) +
        geom_line(mapping = aes(x = x, y = response), color = "black", linewidth = 1) + 
        theme_classic() +
        labs(x = xLab, y = yLab) +
        ggtitle(title)
    } else {
      g = ggplot(data = plotData) + 
        geom_hline(yintercept = 0, color = "grey88") +
        geom_ribbon(aes(x, ymin = lb, ymax = ub), alpha = 0.4, fill = fillcolor) +
        geom_line(mapping = aes(x = x, y = response), color = "black", linewidth = 1) +
        theme_classic() +
        labs(x = xLab, y = yLab) +
        coord_cartesian(ylim = yLim) + 
        ggtitle(title)
    }
    
    if (plotmax == T) {
      g = g + geom_vline(mapping = aes(xintercept = maxX1), linetype = "solid", colour = "grey39") +
        annotate(geom = "text", x = maxX1 + 3, y = 5, label = paste0(maxX1, " C"), color = "grey39")
    }
    
  } else {
    # Two model plot
    if (sum(is.na(yLim)) > 0) {
      g = ggplot(data = plotData) + 
        geom_hline(yintercept = 0, color = "grey88") +
        geom_ribbon(aes(x, ymin = lb, ymax = ub, fill = factor(model, levels = c("Main", "Grid level"))), alpha = 0.4) +
        geom_line(aes(x = x, y = response, color = factor(model, levels = c("Main", "Grid level"))), linewidth = 1) + 
        theme_classic() +
        labs(x = xLab, y = yLab) +
        ggtitle(title) +
        scale_fill_manual(values = c(fillcolor, fillcolor2)) +
        scale_color_manual(values = c("black", "black")) +
        theme(legend.position = "bottom", legend.title = element_blank())
    } else {
      g = ggplot(data = plotData) + 
        geom_hline(yintercept = 0, color = "grey88") +
        geom_ribbon(aes(x, ymin = lb, ymax = ub, fill = factor(model, levels = c("Main", "Grid level"))), alpha = 0.4) +
        geom_line(
          aes(x = x, y = response, color = factor(model, levels = c("Main", "Grid level")),
           linetype=factor(model, levels = c("Main", "Grid level"))), linewidth = 0.5) +
        theme_classic() +
        labs(x = xLab, y = yLab) +
        coord_cartesian(ylim = yLim) + 
        ggtitle(title) +
        scale_fill_manual(values = c(fillcolor, fillcolor2)) +
        scale_color_manual(values = c("black", "black")) +
        scale_linetype_manual(values = c("solid", "dashed")) +
        theme(legend.position = "bottom", legend.title = element_blank())
    }
    
    if (plotmax == T) {
      g = g + 
        geom_vline(xintercept = maxX1, linetype = "solid", colour = "black") +
        geom_vline(xintercept = maxX2, linetype = "dashed", colour = "black") +
        annotate(geom = "text", x = maxX1 - 4, y = 4, 
                label = paste0(maxX1, " C"), color = "grey39", size = 3) +
        annotate(geom = "text", x = maxX2 + 4, y = 4,
                label = paste0(maxX2, " C "), color = "grey39", size = 3)
    }
  }
  
  if (!showYTitle) {
    g = g + theme(axis.title.y = element_blank())
  }
  
  return(g)
}

plotLinearLags_2_mod = function(
  mod,
  patternForPlotVars,
  cluster = T,
  laglength = 3,
  xLab,
  yLab,
  title = "title",
  yLim = c(-1, 1),
  mod2 = NULL,
  model1_name = "Model 1",
  model2_name = "Model 2"
) {
  
  # Function to extract data for a single model
  extractModelData = function(model, pattern, cluster_flag, lag_length) {
    beta = model$coefficients
    vars = rownames(beta)
    plotVars = vars[grepl(pattern = pattern, x = vars)]
    b = as.matrix(beta[rownames(beta) %in% plotVars])
    
    if (cluster_flag == T) {
      vcov = getVcov(model$clustervcv, plotVars)
    } else {
      vcov = getVcov(model$vcv, plotVars)
    }
    
    response = 1 * b
    length = 1.96 * sqrt(diag(vcov))
    lb = response - length
    ub = response + length
    
    return(data.frame(
      lag = 0:lag_length,
      response = response,
      lb = lb,
      ub = ub
    ))
  }
  
  # Extract data for first model
  plotData1 = extractModelData(mod, patternForPlotVars, cluster, laglength)
  plotData1$model = model1_name
  
  # If second model provided, extract its data too
  if (!is.null(mod2)) {
    plotData2 = extractModelData(mod2, patternForPlotVars, cluster, laglength)
    plotData2$model = model2_name
    plotData = rbind(plotData1, plotData2)
  } else {
    plotData = plotData1
  }
  
  # Determine colors
  if (is.null(mod2)) {
    # Single model - use original color logic
    beta = mod$coefficients
    vars = rownames(beta)
    plotVars = vars[grepl(pattern = patternForPlotVars, x = vars)]
    
    if (plotVars[1] == "drought") {
      mycolor = "#C99776"
    } else if (plotVars[1] == "flood") {
      mycolor = "#43A7BA"
    } else {
      mycolor = "black"
    }
    
    g = ggplot(data = plotData, aes(x = lag)) +
      geom_hline(yintercept = 0, linewidth = .5, color = "grey") +
      geom_point(aes(y = response), color = mycolor, size = 2) +
      geom_errorbar(aes(ymin = lb, ymax = ub), color = mycolor, width = .1) +
      theme_classic() +
      labs(x = xLab, y = yLab) +
      coord_cartesian(ylim = yLim) +
      ggtitle(title) +
      theme(plot.title = element_text(size = 8), text = element_text(size = 8))
  } else {
    # Two models - use different colors/shapes for each
    g = ggplot(data = plotData, aes(x = lag, color = factor(model, levels = c("Main", "Grid level")))) +
      geom_hline(yintercept = 0, linewidth = .5, color = "grey") +
      geom_point(aes(y = response), size = 2, position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = lb, ymax = ub), width = .1, position = position_dodge(width = 0.5)) +
      theme_classic() +
      labs(x = xLab, y = yLab) +
      coord_cartesian(ylim = yLim) +
      ggtitle(title) +
      theme(plot.title = element_text(size = 8), text = element_text(size = 8),
            legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = c("Main" = "#C1657C", "Grid level" = "grey50")) 
  }
  
  return(g)
}



t1 = plotPolynomialResponse_2_mod(
  mainmod,
  "temp",
  plotXtemp,
  polyOrder = 2,
  cluster = T,
  xRef = myrefT,
  xLab = expression(paste("Mean temperature (", degree, "C)")),
  yLab = "Prevalence (%)",
  title = NULL,
  yLim = c(-30, 5),
  showYTitle = T,
  mod2 = highresmod,
  model1_name = "Main",
  model2_name = "Grid level",
  fillcolor2 = "grey50"
)
t1


d1 <- plotLinearLags_2_mod(
  mod = mainmod,
  model1_name = "Main",
  patternForPlotVars = "drought",
  cluster = T,
  laglength = 3,
  xLab = "Drought (month lags)",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-4, 4),
  mod2 = highresmod,
  model2_name = "Grid level"
) 
d1

f1 <- plotLinearLags_2_mod(
  mod = mainmod,
  model1_name = "Main",
  patternForPlotVars = "flood",
  cluster = T,
  laglength = 3,
  xLab = "Flood (month lags)",
  yLab = "Coefficient",
  title = NULL,
  yLim = c(-4, 4),
  mod2 = highresmod,
  model2_name = "Grid level"
) 
f1


combined_plot1 <- t1 +
  d1 +
  f1 +
  plot_layout(ncol = 3, guides = "collect") &
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "bottom", 
    legend.margin = margin(0, 0, 0, 0)
  )

combined_plot1

dir.create(
  file.path(resdir, "Figures", "Diagnostics", "Main_model"),
  showWarnings = FALSE
)
ggsave(
  filename = "temp_drought_flood_cXt2intrXm_w_adm1_and_high_res.pdf",
  path = file.path(resdir, "Figures", "Diagnostics", "Main_model"),
  plot = combined_plot1,
  width = 7,
  height = 2.5,
  dpi = 300
)



