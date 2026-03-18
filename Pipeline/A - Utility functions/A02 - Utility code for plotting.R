############## plot a polynomial response curve ##############
##### CalcVariance and getVcov are helper functions to plotPolynomialResponse
calcVariance = function(cvec, vcovMat) {
  ##Calculate the variance:
  #cvec is a vector of climate variables (k by 1), coming from the GCM
  #vcov is a variance covariance matrix with the same column order as cvec. This comes from our empirical estimation.
  return(t(cvec) %*% vcovMat %*% cvec)
}


getVcov = function(vcv, vars) {
  #from the entire variance covariance matrix, select the portion corresponding to the variable you're interested in.
  return(vcv[vars, vars])
}


genRecenteredXVals_polynomial = function(xVals, xRef, polyOrder, lag = NA) {
  ### This function generates X values that are recentered around value xRef. The output of this function can be passed to plotPolynomialResponse to generate recentered polynomial response functions
  ### xVals are the x values you want to pass to your plotting function
  ### xRef is the reference value you want to recenter around
  ### polyOrder is the order of the polynomial
  ### lag is either NA or a scalar of the number of lags in regression

  if (is.na(lag)) {
    newX <- data.frame(matrix(NA, nrow = dim(xVals)[1], ncol = polyOrder))

    for (p in 1:polyOrder) {
      newX[, p] <- xVals[, 1]^p - xRef^p
    }
    return(newX)
  } else {
    lagcols = lag + 1
    newX <- data.frame(matrix(
      NA,
      nrow = dim(xVals)[1],
      ncol = polyOrder * (lagcols)
    ))

    i = 0
    for (l in 1:lagcols) {
      for (p in 1:polyOrder) {
        i = i + 1
        newX[, i] <- xVals[, 1]^p - xRef^p
      }
    }
    return(newX)
  }
}


plotPolynomialResponse <- function(
  mod,
  patternForPlotVars,
  xVals,
  polyOrder,
  lag = NA,
  plotmax = TRUE,
  cluster = TRUE,
  xRef = 0,
  fillcolor = "#C1657C",
  xLab,
  yLab,
  title = "title",
  yLim = c(-1, 1),
  showYTitle = TRUE
) {
  ### mod is a model regression model object (e.g. mod = lm(y~x) or mode = felm(y~x)).
  ### patternForPlotVars is a string that is in the variables from the model that you want to plot but not in the ones you don't want to plot.
  ### xVals is a matrix of dataframe that corresponds to the variables you're plotting. e.g. if b1 was for T and b2 for T2 then xVals[,1] would be T and xVals[,2] would be T2. NOTE: Do not pass the function recentered xVals, the function recenters for you!
  ### polyOrder is the order of polynomial contained within xVals.
  ### lag is either NA or a scalar of the number of lags in regression. Make sure to order variables in the regression as all polynomial orders of a given lag before the next lag.
  ### plotmax = T will plot a vertical line at the maximum of the estimated response function
  ### cluster = T if clustering (need clustervcv), cluster = F if not clustering SEs
  ### xRef is the reference value for the x-axis. If you are passing the function recentered xVals, this is the value where you want y to be equal to zero.
  ### xLab is x-axis label
  ### yLab is y-axis label
  ### title is graph title
  ### yLim limits y-axis values
  ### showYTitle turns on and off the y-axis label
  
  # Handle different model types
  if (inherits(mod, "felm")) {
    beta = mod$coefficients 
    vars = rownames(beta)
  } else if (inherits(mod, "fixest")) {  # feols returns objects of class "fixest"
    beta = mod$coefficients
    vars = names(beta)
    # Convert to matrix format to maintain compatibility with rest of code
    beta = as.matrix(beta)
    rownames(beta) = vars
  } else {
    # Handle regular lm or other models
    beta = mod$coefficients
    vars = names(beta)
    beta = as.matrix(beta)
    rownames(beta) = vars
  }
  
  #Get the variables that we're plotting
  plotVars = vars[grepl(pattern = patternForPlotVars, x = vars)] 
  
  # Recenter Xs so predictions are relative to the reference T
  xValsT = genRecenteredXVals_polynomial(xVals,xRef,polyOrder,lag)
  
  #Get the estimated variance covariance matrix
  if (cluster==T) {
    if (inherits(mod, "fixest")) {
      # For feols/fixest models, use vcov() function
      vcov_full = vcov(mod)
      vcov = getVcov(vcov_full, plotVars)
    } else {
      # For felm models
      vcov = getVcov(mod$clustervcv, plotVars)
    }
  } else {
    if (inherits(mod, "fixest")) {
      vcov_full = vcov(mod, se = "iid")  # Get non-clustered vcov
      vcov = getVcov(vcov_full, plotVars)
    } else {
      vcov = getVcov(mod$vcv, plotVars)
    }
  }
  # b <- as.matrix(beta[plotVars])
  b = as.matrix(beta[rownames(beta) %in% plotVars])

  n = nrow(xValsT)
  
  # 4) see if there are any interactions:
  if (any(grepl(":", plotVars))) {
    # build two design matrices, one for urban=0 (“rural”) and one for urban=1
    X_rural <- cbind(xValsT, matrix(0, nrow = n, ncol = ncol(xValsT)))
    X_urban <- cbind(xValsT, xValsT)
    colnames(X_rural) <- plotVars
    colnames(X_urban) <- plotVars

    # 5) predictions
    resp_rur <- as.matrix(X_rural) %*% b
    resp_urb <- as.matrix(X_urban) %*% b

    # 6) standard errors
    se_rur <- 1.96 * sqrt(apply(X_rural, 1, calcVariance, vcov))
    se_urb <- 1.96 * sqrt(apply(X_urban, 1, calcVariance, vcov))

    # 7) bounds
    lb_rur <- resp_rur - se_rur
    ub_rur <- resp_rur + se_rur
    lb_urb <- resp_urb - se_urb
    ub_urb <- resp_urb + se_urb

    # 8) stack into one data.frame
    plotData <- data.frame(
      x = rep(xValsT[, 1] + xRef, 2),
      response = c(resp_rur, resp_urb),
      lb = c(lb_rur, lb_urb),
      ub = c(ub_rur, ub_urb),
      group = rep(c("Rural", "Urban"), each = n)
    )
    g <- ggplot() +
      geom_hline(yintercept = 0, colour = "grey88", linewidth = .4) +
      geom_ribbon(
        data = filter(plotData, group == "Urban"),
        aes(x, ymin = lb, ymax = ub),
        fill = "grey50",
        alpha = .4
      ) +
      geom_ribbon(
        data = filter(plotData, group == "Rural"),
        aes(x, ymin = lb, ymax = ub),
        fill = "#C1657C",
        alpha = .6
      ) +
      geom_line(
        data = filter(plotData, group == "Urban"),
        aes(x = x, y = response),
        colour = "black",
        alpha = .6,
        linewidth = .5
      ) +
      geom_line(
        data = filter(plotData, group == "Rural"),
        aes(x = x, y = response),
        colour = "black",
        alpha = 1,
        linewidth = .5
      ) +
      labs(
        x = expression(paste("Mean temperature (", degree, "C)")),
        y = "Prevalence (%)",
        title = NULL,
        colour = "",
        linetype = "",
        fill = ""
      ) +
      coord_cartesian(ylim = c(-30, 10)) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        text = element_text(size = 8)
      )
  } else {
    response <- as.matrix(xValsT) %*% b
    length <- 1.96 * sqrt(apply(xValsT, 1, calcVariance, vcov))
    lb <- response - length
    ub <- response + length

    plotData <- data.frame(
      x = xValsT[, 1] + xRef,
      response = response,
      lb = lb,
      ub = ub
    )

    g = ggplot(data = plotData) +
      geom_hline(yintercept = 0, color = "grey88") +
      geom_ribbon(
        aes(x, ymin = lb, ymax = ub),
        alpha = 0.4,
        fill = fillcolor
      ) +
      geom_line(
        mapping = aes(x = x, y = response),
        color = "black",
        linewidth = 1
      ) +
      theme_classic() +
      labs(x = xLab, y = yLab) +
      ggtitle(title)
  }

  # 10) optionally fix y-limits
  if (!any(is.na(yLim))) {
    g <- g + coord_cartesian(ylim = yLim)
  }

  # 11) add the “max” vertical line if requested
  if (plotmax) {
    if (any(grepl(":", plotVars))) {
      # find the peak on the main effect curve (e.g. rural)
      sub = plotData[plotData$x >= 10 & plotData$x <= 30, ]

      rural_sub <- filter(sub, group == "Rural")
      urban_sub <- filter(sub, group == "Urban")

      rural_maxX <- max(rural_sub$x[
        rural_sub$response == max(rural_sub$response)
      ])
      urban_maxX <- max(urban_sub$x[
        urban_sub$response == max(urban_sub$response)
      ])

      cat(
        "Rural max at",
        rural_maxX,
        "C\nUrban max at",
        urban_maxX,
        "C\n"
      )

      maxX = rural_maxX
    } else {
      maxX = max(plotData$x[plotData$response == max(plotData$response)])
    }

    g <- g +
      geom_vline(xintercept = maxX, colour = "grey39") +
      annotate(
        geom = "text",
        x = maxX + 3.5,
        y = 2.55,
        label = paste0(maxX, " C"),
        color = "grey39",
        size = 3
      )
  }

  return(g)
}


plotPolynomialResponseSimple = function(
  coefs,
  xVals,
  polyOrder,
  plotmax = F,
  xRef = 0,
  xLab,
  yLab,
  title = "title",
  yLim = c(-1, 1),
  showYTitle = T
) {
  ### same as plotPolynomialResponse(), but input is not a model but the actual set of coefficients, and no SE are plotted
  ### temporary function until I can figure out how to get the right SEs on the cumulative effects

  beta = coefs ##See if this works

  # Recenter Xs so predictions are relative to the reference T
  xValsT = genRecenteredXVals_polynomial(xVals, xRef, polyOrder)

  b = as.matrix(beta)

  response = as.matrix(xValsT) %*% b #Prediction

  #Plot -- add back in the reference temperature so it's centered at xRef
  plotData = data.frame(x = xValsT[, 1] + xRef, response = response)

  # plot maximum if desired
  maxX = max(plotData$x[plotData$response == max(plotData$response)])

  if (sum(is.na(yLim)) > 0) {
    g = ggplot(data = plotData) +
      geom_hline(yintercept = 0, color = "grey88") +
      geom_line(mapping = aes(x = x, y = response), color = "black") +
      theme_classic() +
      labs(x = xLab, y = yLab) +
      ggtitle(title)
  } else {
    g = ggplot(data = plotData) +
      geom_hline(yintercept = 0, color = "grey88") +
      geom_line(mapping = aes(x = x, y = response), color = "black") +
      theme_classic() +
      labs(x = xLab, y = yLab) +
      coord_cartesian(ylim = yLim) +
      ggtitle(title)
  }
  if (!showYTitle) {
    g = g + theme(axis.title.y = element_blank())
  }
  if (plotmax == T) {
    g = g +
      geom_vline(
        mapping = aes(xintercept = maxX),
        linetype = "solid",
        colour = "grey39"
      ) +
      annotate(
        geom = "text",
        x = maxX + 2.5,
        y = 15,
        label = paste0(maxX, " C"),
        color = "grey39"
      )
  }
  return(g)
}


# Plot linear responses that are lagged
plotLinearLags = function(
  mod,
  patternForPlotVars,
  cluster = T,
  laglength = 3,
  xLab,
  yLab,
  title = "title",
  yLim = c(-1, 1)
) {
  beta = mod$coefficients
  vars = rownames(beta)
  plotVars = vars[grepl(pattern = patternForPlotVars, x = vars)]
  b = as.matrix(beta[rownames(beta) %in% plotVars])

  if (cluster == T) {
    vcov = getVcov(mod$clustervcv, plotVars)
  } else {
    vcov = getVcov(mod$vcv, plotVars)
  }

  response = 1 * b #Prediction -- here, all responses are linear!
  length = 1.96 * sqrt(diag(vcov))
  lb = response - length
  ub = response + length

  # Plot
  plotData = data.frame(
    lag = 0:laglength,
    response = response,
    lb = lb,
    ub = ub
  )
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
    labs(x = "Lag", y = "Coefficient") +
    coord_cartesian(ylim = yLim) +
    ggtitle(title) +
    theme(plot.title = element_text(size = 8), text = element_text(size = 8))

  return(g)
}


plotLinearLags_urban = function(
  mod,
  patternForPlotVars,
  cluster = T,
  laglength = 3,
  xLab,
  yLab,
  title = "title",
  yLim = c(-1, 1)
) {
  beta = mod$coefficients
  vars = rownames(beta)
  plotVars = vars[grepl(pattern = patternForPlotVars, x = vars)]
  b = as.matrix(beta[rownames(beta) %in% plotVars])

  if (cluster == T) {
    vcov = getVcov(mod$clustervcv, plotVars)
  } else {
    vcov = getVcov(mod$vcv, plotVars)
  }

  response = 1 * b #Prediction -- here, all responses are linear!
  length = 1.96 * sqrt(diag(vcov))
  lb = response - length
  ub = response + length

  group <- ifelse(startsWith(plotVars, "urban_dummy"), "Urban", "Rural")

  # Plot
  plotData = data.frame(
    lag = 0:laglength,
    response = response,
    lb = lb,
    ub = ub,
    group = factor(group, levels = c("Rural", "Urban"))
  )
  if (plotVars[1] == "drought") {
    mycolor = "#C99776"
  } else if (plotVars[1] == "flood") {
    mycolor = "#43A7BA"
  } else {
    mycolor = "black"
  }

  g = ggplot(data = plotData, aes(x = lag, color = group)) +
    geom_hline(yintercept = 0, linewidth = .5, color = "grey") +
    geom_point(
      aes(y = response),
      size = 2,
      position = position_dodge(width = .5)
    ) +
    geom_errorbar(
      aes(ymin = lb, ymax = ub),
      width = .2,
      position = position_dodge(width = .5)
    ) +
    scale_color_manual(values = c("#C1657C", "grey50")) +
    theme_classic() +
    labs(x = xLab, y = yLab, color = NULL) +
    coord_cartesian(ylim = yLim) +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 8),
      text = element_text(size = 8),
      legend.position = "bottom"
    )

  return(g)
}

# For D05
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

