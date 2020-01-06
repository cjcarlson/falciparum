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
  return(vcv[vars,vars])
}


genRecenteredXVals_polynomial = function(xVals,xRef,polyOrder) {
  ### This function generates X values that are recentered around value xRef. The output of this function can be passed to plotPolynomialResponse to generate recentered polynomial response functions
  ### xVals are the x values you want to pass to your plotting function
  ### xRef is the reference value you want to recenter around
  ### polyOrder is the order of the polynomial
  
  newX <- data.frame(matrix(NA, nrow=dim(xVals)[1], ncol = polyOrder))
  
  for (p in 1:polyOrder) {
    newX[,p] <- xVals[,1]^p - xRef^p
  }
  return(newX)
}


plotPolynomialResponse = function(mod, patternForPlotVars, xVals, cluster = T, xRef = 0, xLab, yLab, yLim=NA, title = "title", showYTitle = F) {
  ### mod is a model regression model object (e.g. mod = lm(y~x) or mode = felm(y~x)). 
  ### patternForPlotVars is a string that is in the variables from the model that you want to plot but not in the ones you don't want to plot. 
  ### xVals is a matrix of dataframe that corresponds to the variables you're plotting. e.g. if b1 was for T and b2 for T2 then xVals[,1] would be T and xVals[,2] would be T2. NOTE: If you are passing the function recentered xVals, you need to put the reference x value into the xRef option! 
  ### cluster = T if clustering (need clustervcv), cluster = F if not clustering SEs
  ### xRef is the reference value for the x-axis. If you are passing the function recentered xVals, this is the value where you want y to be equal to zero. 
  ### xLab is x-axis label
  ### yLab is y-axis label
  ### title is graph title
  ### yLim limits y-axis values
  ### showYTitle turns on and off the y-axis label
  
  beta = mod$coefficients ##See if this works
  vars = rownames(beta)
  
  #Get the variables that we're plotting
  plotVars = vars[grepl(pattern = patternForPlotVars, x = vars)] 
  
  #Get the estimated variance covariance matrix
  if (cluster==T) {
    vcov = getVcov(mod$clustervcv, plotVars) ##This needs to change if we don't cluster
  } else {
    vcov = getVcov(mod$vcv, plotVars)
  }
  
  b = as.matrix(beta[rownames(beta) %in% plotVars])
  
  response = as.matrix(xVals) %*% b #Prediction
  length = 1.96 * sqrt(apply(X = xVals, FUN = calcVariance, MARGIN = 1, vcov))
  
  lb = response - length
  ub = response + length
  
  #Plot -- add back in the reference temperature so it's centered at xRef
  plotData = data.frame(x = xVals[,1] + xRef, response = response, lb = lb, ub = ub)
  
  if(is.na(yLim)) {
    g = ggplot(data = plotData) + geom_line(mapping = aes(x = x, y = response, color = "known_true")) +
      geom_line(data = plotData, mapping = aes(x = x, y = ub, color = "known_true"), linetype = 3) +
      geom_line(data = plotData, mapping = aes(x = x, y = lb, color = "known_true"), linetype = 3) +
      theme_classic() +
      geom_hline(yintercept = 0) + labs(x = xLab , y = yLab) +
      ggtitle(title)
  } else {
    g = ggplot(data = plotData) + geom_line(mapping = aes(x = x, y = response, color = "known_true")) +
      geom_line(data = plotData, mapping = aes(x = x, y = ub, color = "known_true"), linetype = 3) +
      geom_line(data = plotData, mapping = aes(x = x, y = lb, color = "known_true"), linetype = 3) +
      theme_classic() +
      geom_hline(yintercept = 0) + labs(x = xLab , y = yLab) +
      coord_cartesian(ylim=yLim) + ggtitle(title)
  }
  
  
  if (!showYTitle) {
    g = g + theme(axis.title.y=element_blank())
  }
  return(g)
}
