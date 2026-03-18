
print("Loading A01 - Utility code for calculations.R")

## Functions needed throughout falciparum/ repo

swirl <- function(input.raster) {
  input.raster <- flip(t(input.raster),direction='y')
  extent(input.raster) <- c(-180,180,-90,90)
  input.raster = crop(input.raster, c(-30,60,-37,40))
  return(input.raster)
}


r0t <- function(T, na.rm=TRUE) {
  suppressWarnings(if(is.na(T)) return(NA))
  if(T<=14) {return(0)}
  a = 0.000203*T*(T-11.7)*((42.3-T)^0.5)
  bc = -0.54*T*T + 25.2*T - 206
  p = -0.000828*T*T + 0.0367*T + 0.522 # e^-mu
  mu = -1*log(p)
  PDR = 0.000111*T*(T-14.7)*((34.4-T)^0.5) # 1/EIP
  pEA = -0.00924*T*T + 0.453*T - 4.77
  MDR = 0.000111*T*(T-14.7)*((34-T)^0.5) # 1/tauEA
  EFD = -0.153*T*T + 8.61*T - 97.7
  
  R0 = (((a^2)*bc*(p^(1/PDR))*EFD*pEA*MDR)/(mu^3))^(1/2)
  if(is.nan(R0)){return(0)}
  return(R0/87.13333) # that's the max
}


computePrcpExtremes = function(dfclimate, dfoutcome, pctdrought, pctflood, yearcutoff=NA) {
  # NOTE: for percentiles: enter 0.90 for 90th, 0.10 for 10th.
  # NOTE: dfclimate is the dataframe where your climate data live (may be more comprehensive in space and/or time than your outcome variable)
  # dfoutcome is the dataframe where your outcome merged to climate data live
  
  # initialize
  data = dfclimate
  data = data %>% dplyr::arrange(OBJECTID, monthyr)
  
  # year cutoff, if not null
  if(is.na(yearcutoff)) { yearcutoff = max(data$yearnum) }
  
  # flood definition
  data %>% 
    dplyr::filter(yearnum <= yearcutoff ) %>% 
    dplyr::group_by(OBJECTID) %>% 
    dplyr::summarize(ppt.90 = quantile(na.omit(ppt), pctflood)) -> ppt.90
  data <- dplyr::left_join(data, ppt.90)
  data$flood <- as.numeric(data$ppt >= data$ppt.90)
  colnames(data)[dim(data)[2]-1] = paste0("ppt_pctile", pctflood)
  
  # flood lags
  data %>% 
    dplyr::group_by(OBJECTID) %>% 
    dplyr::mutate(flood.lag = lag(flood, order_by = monthyr),
           flood.lag2 = lag(flood, order_by = monthyr, n=2),
           flood.lag3 = lag(flood, order_by = monthyr, n=3)) -> data
  
  # drought definition
  data %>% 
    dplyr::filter(yearnum <= yearcutoff ) %>% 
    dplyr::group_by(OBJECTID) %>% 
    dplyr::summarize(ppt.10 = quantile(na.omit(ppt), pctdrought)) -> ppt.10
  
  data <- left_join(data, ppt.10)
  data$drought <- as.numeric(data$ppt <= data$ppt.10)
  colnames(data)[dim(data)[2]-1] = paste0("ppt_pctile", pctdrought)
  
  # drought lags
  data %>% 
    dplyr::group_by(OBJECTID) %>% 
    dplyr::mutate(drought.lag = lag(drought, order_by = monthyr),
           drought.lag2 = lag(drought, order_by = monthyr, n=2),
           drought.lag3 = lag(drought, order_by = monthyr, n=3)) -> data
  
  # merge into outcome dataframe 
  tokeep = c("OBJECTID", "monthyr", "month", "year")
  data = data %>% dplyr::select(tokeep, dplyr::contains("flood"), dplyr::contains("ppt_pctile"), dplyr::contains("drought"))
  dfoutcome <- dplyr::left_join(dfoutcome, data, by=c("OBJECTID", "monthyr", "month", "year"))
  
  # return
  return(dfoutcome)
  
}


##### Correlation Helper Functions ----
analyze_corr <- function(
  kind,
  corrVec,
  selection,
  name,
  obsCountVec = NULL,
  T_min = 10,
  weighting = FALSE
) {
  # Taken and modified from:
  # github.com/chroetz/ClusSpatCorr/blob/2299012ea07b8817bc11b9bbfb61d3e7a7150459/03_2_Correlation_Analyze.R#L8-L25

  # Apply selection filter
  selection[is.na(selection)] <- FALSE

  if (!is.null(obsCountVec)) {
    # Filter out low observation counts
    sample_size_filter <- obsCountVec >= T_min & !is.na(obsCountVec)
    selection <- selection & sample_size_filter
  }

  # Get filtered data
  x <- corrVec[selection & !is.na(corrVec)]

  # Calculate statistics (weighted or unweighted based on weighting parameter)
  probs <- seq(0, 1, 0.05)

  if (weighting && !is.null(obsCountVec)) {
    # Weighted statistics using obsCountVec
    weights <- obsCountVec[selection & !is.na(corrVec)]

    # Weighted mean
    mean <- weighted.mean(x, weights)

    # Weighted quantiles (using Hmisc package)
    quantiles <- Hmisc::wtd.quantile(
      x,
      weights = weights,
      probs = probs,
      normwt = TRUE
    )

    # Weighted standard deviation
    var <- sum(weights * (x - mean)^2) / sum(weights)
    sd <- sqrt(var)

    # Weighted median (50th percentile)
    median <- Hmisc::wtd.quantile(
      x,
      weights = weights,
      probs = 0.5,
      normwt = TRUE
    )

    total_weight <- sum(weights)
  } else {
    # Unweighted statistics
    mean <- mean(x)
    quantiles <- quantile(x, probs = probs)
    sd <- sd(x)
    median <- median(x)
    total_weight <- length(x) # For unweighted, total weight equals sample size
  }

  bind_cols(
    tibble(
      kind = kind,
      group = name,
      mean = mean,
      median = as.numeric(median),
      sd = sd,
      n = length(x),
      total_weight = total_weight
    ),
    setNames(
      as.list(quantiles),
      sprintf("q%02d", as.integer(probs * 100))
    ) |>
      as_tibble()
  )
}

# Function to convert dataframe to LaTeX table
df_to_latex <- function(df, digits = 2) {
  # Handle NaN/NA values
  df$mean[is.nan(df$mean)] <- NA
  df$q25[is.nan(df$q25)] <- NA
  df$q75[is.nan(df$q75)] <- NA

  # Round numeric columns
  df$mean <- round(df$mean, digits)
  df$q25 <- round(df$q25, digits)
  df$q75 <- round(df$q75, digits)

  # Replace NA with empty string for display
  df$mean[is.na(df$mean)] <- ""
  df$q25[is.na(df$q25)] <- ""
  df$q75[is.na(df$q75)] <- ""

  # Start LaTeX table
  cat("\\begin{tabular}{llcccc}\n")
  cat("    \\toprule\n")
  cat("    Kind & Group & $\\overline{\\rho}$ & Q25 & Q75 & $N$ \\\\\n")
  cat("    \\hline\n")

  # Print each row
  for (i in 1:nrow(df)) {
    cat(
      "    ",
      df$kind[i],
      " & ",
      df$group[i],
      " & ",
      df$mean[i],
      " & ",
      df$q25[i],
      " & ",
      df$q75[i],
      " & ",
      df$n[i],
      " \\\\\n",
      sep = ""
    )
  }

  cat("    \\bottomrule\n")
  cat("\\end{tabular}\n")
}

count_pairwise_obs <- function(data) {
  data_matrix <- as.matrix(data)
  n_vars <- ncol(data_matrix)
  obs_count_matrix <- matrix(0, nrow = n_vars, ncol = n_vars)

  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      complete_pairs <- sum(!is.na(data_matrix[, i]) & !is.na(data_matrix[, j]))
      obs_count_matrix[i, j] <- complete_pairs
    }
  }

  rownames(obs_count_matrix) <- colnames(data_matrix)
  colnames(obs_count_matrix) <- colnames(data_matrix)
  return(obs_count_matrix)
}


# Helper: reduce boilerplate for repeated analyze_corr calls.
# Each spec is list(selection_expr, name). Shared args are bound once.
run_corr_specs <- function(
  kind,
  corrVec,
  specs,
  obsCountVec,
  T_min,
  weighting
) {
  bind_rows(lapply(specs, function(s) {
    analyze_corr(
      kind,
      corrVec,
      s[[1]],
      s[[2]],
      obsCountVec,
      T_min,
      weighting = weighting
    )
  }))
}
