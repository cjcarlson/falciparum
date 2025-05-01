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
  # 1) pull out coefficients & names
  beta <- mod$coefficients
  vars <- rownames(beta)
  plotVars <- vars[grepl(patternForPlotVars, vars)]

  # 2) recenter your X's
  xValsT <- genRecenteredXVals_polynomial(xVals, xRef, polyOrder, lag)
  n <- nrow(xValsT)

  # 3) grab the right VCOV
  if (cluster) {
    vcov <- getVcov(mod$clustervcv, plotVars)
  } else {
    vcov <- getVcov(mod$vcv, plotVars)
  }
  # b <- as.matrix(beta[plotVars])
  b = as.matrix(beta[rownames(beta) %in% plotVars])

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

    g <- g +
      geom_vline(xintercept = maxX, colour = "grey39") +
      annotate(
        geom = "text",
        x = maxX + 3.5,
        y = 7.55,
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
