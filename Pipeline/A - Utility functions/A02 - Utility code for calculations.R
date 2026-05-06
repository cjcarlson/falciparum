################################################################################
# Utility functions for calculations including climate data extraction,
# precipitation extremes, correlation analysis, and R0 computations. Core helper
# functions used throughout the analysis pipeline. Should be sourced after A01.
################################################################################
# Start ----
################################################################################

print("Begin loading A01 - Utility code for calculations.R")

################################################################################
# swirl ----
################################################################################

# This function takes in a raster that is in x=lat, y=lon format (native CRU is like this) and
# rotates it 90 degrees to be in x=lon, y=lat format. It also crops to the extent of Africa.
swirl <- function(input.raster) {
  input.raster <- flip(t(input.raster), direction = 'y')
  extent(input.raster) <- c(-180, 180, -90, 90)
  input.raster = crop(input.raster, c(-30, 60, -37, 40))
  return(input.raster)
}

################################################################################
# r0t ----
################################################################################

r0t <- function(T, na.rm = TRUE) {
  suppressWarnings(if (is.na(T)) return(NA))
  if (T <= 14) {
    return(0)
  }
  a = 0.000203 * T * (T - 11.7) * ((42.3 - T)^0.5)
  bc = -0.54 * T * T + 25.2 * T - 206
  p = -0.000828 * T * T + 0.0367 * T + 0.522 # e^-mu
  mu = -1 * log(p)
  PDR = 0.000111 * T * (T - 14.7) * ((34.4 - T)^0.5) # 1/EIP
  pEA = -0.00924 * T * T + 0.453 * T - 4.77
  MDR = 0.000111 * T * (T - 14.7) * ((34 - T)^0.5) # 1/tauEA
  EFD = -0.153 * T * T + 8.61 * T - 97.7

  R0 = (((a^2) * bc * (p^(1 / PDR)) * EFD * pEA * MDR) / (mu^3))^(1 / 2)
  if (is.nan(R0)) {
    return(0)
  }
  return(R0 / 87.13333) # that's the max
}

################################################################################
# optT ----
################################################################################

# function to compute optimal temp for each run
optT <- function(beta1, beta2) {
  opt = -beta1 / (2 * beta2)
  return(opt)
}

################################################################################
# computePrcpExtremes ----
################################################################################

computePrcpExtremes = function(
  dfclimate,
  dfoutcome,
  pctdrought,
  pctflood,
  yearcutoff = NA
) {
  # NOTE: for percentiles: enter 0.90 for 90th, 0.10 for 10th.
  # NOTE: dfclimate is the dataframe where your climate data live (may be more comprehensive in space and/or time than your outcome variable)
  # dfoutcome is the dataframe where your outcome merged to climate data live

  # initialize
  data = dfclimate
  data = data %>% dplyr::arrange(OBJECTID, monthyr)

  # year cutoff, if not null
  if (is.na(yearcutoff)) {
    yearcutoff = max(data$yearnum)
  }

  # flood definition
  ppt.90 <- data %>%
    dplyr::filter(yearnum <= yearcutoff) %>%
    dplyr::group_by(OBJECTID) %>%
    dplyr::summarize(ppt.90 = quantile(na.omit(ppt), pctflood))

  data <- dplyr::left_join(data, ppt.90)
  data$flood <- as.numeric(data$ppt >= data$ppt.90)
  colnames(data)[dim(data)[2] - 1] = paste0("ppt_pctile", pctflood)

  # flood lags
  data <- data %>%
    dplyr::group_by(OBJECTID) %>%
    dplyr::mutate(
      flood.lag = lag(flood, order_by = monthyr),
      flood.lag2 = lag(flood, order_by = monthyr, n = 2),
      flood.lag3 = lag(flood, order_by = monthyr, n = 3)
    )

  # drought definition
  ppt.10 <- data %>%
    dplyr::filter(yearnum <= yearcutoff) %>%
    dplyr::group_by(OBJECTID) %>%
    dplyr::summarize(ppt.10 = quantile(na.omit(ppt), pctdrought))

  data <- left_join(data, ppt.10)
  data$drought <- as.numeric(data$ppt <= data$ppt.10)
  colnames(data)[dim(data)[2] - 1] = paste0("ppt_pctile", pctdrought)

  # drought lags
  data <- data %>%
    dplyr::group_by(OBJECTID) %>%
    dplyr::mutate(
      drought.lag = lag(drought, order_by = monthyr),
      drought.lag2 = lag(drought, order_by = monthyr, n = 2),
      drought.lag3 = lag(drought, order_by = monthyr, n = 3)
    )

  # merge into outcome dataframe
  tokeep = c("OBJECTID", "monthyr", "month", "year")
  data = data %>%
    dplyr::select(
      all_of(tokeep),
      dplyr::contains("flood"),
      dplyr::contains("ppt_pctile"),
      dplyr::contains("drought")
    )
  dfoutcome <- dplyr::left_join(
    dfoutcome,
    data,
    by = c("OBJECTID", "monthyr", "month", "year")
  )

  # return
  return(dfoutcome)
}

################################################################################
# analyze_corr ----
################################################################################

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

################################################################################
# count_pairwise_obs ----
################################################################################

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

################################################################################
# df_to_latex ----
################################################################################

# Function to convert dataframe to LaTeX table
df_to_latex <- function(df, digits = 2, print = TRUE) {
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

  # Build lines
  lines <- character()
  lines <- c(lines, "\\begin{tabular}{llcccc}")
  lines <- c(lines, "    \\toprule")
  lines <- c(
    lines,
    "    Kind & Group & $\\overline{\\rho}$ & Q25 & Q75 & $N$ \\\\"
  )
  lines <- c(lines, "    \\hline")

  for (i in 1:nrow(df)) {
    lines <- c(
      lines,
      paste0(
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
        " \\\\"
      )
    )
  }

  lines <- c(lines, "    \\bottomrule")
  lines <- c(lines, "\\end{tabular}")

  result <- paste(lines, collapse = "\n")

  if (print) {
    cat(result, "\n")
  }

  invisible(result)
}

################################################################################
# build_colnames ----
################################################################################

build_colnames <- function(
  timestep_idx,
  var_prefix,
  max_power,
  start_year = 1901L
) {
  month_str <- month.abb[(timestep_idx - 1L) %% 12L + 1L]
  year_int <- ((timestep_idx - 1L) - (timestep_idx - 1L) %% 12L) /
    12L +
    start_year

  powers <- seq_len(max_power)
  suffixes <- ifelse(powers == 1L, var_prefix, paste0(var_prefix, powers))
  paste(month_str, year_int, suffixes, sep = ".")
}

################################################################################
# make_filename ----
################################################################################

make_filename <- function(clim, model, scenario, grid, date_range) {
  paste0(
    clim,
    "_Amon_",
    model,
    "_",
    scenario,
    "_r1i1p1f1_",
    grid,
    date_range,
    "BC_lonlat.nc"
  )
}

################################################################################
# extract_clim_data_polygons ----
################################################################################

# Helper: extract, pivot to long, return data.table with one value column
extract_clim_data_polygons <- function(rast, polygons, rast_times, value_name, power = 1) {
  if (!is.null(power) && power != 1) {
    rast <- rast^power
  }
  ex <- exactextractr::exact_extract(
    x = rast,
    y = polygons,
    fun = 'weighted_mean',
    progress = FALSE,
    append_cols = "OBJECTID",
    weights = "area"
  )
  colnames(ex) <- c("OBJECTID", rast_times)

  dt <- data.table::as.data.table(ex)
  dt <- data.table::melt(
    dt,
    id.vars = "OBJECTID",
    variable.name = "date",
    value.name = value_name
  )
  dt[, c("year", "month") := data.table::tstrsplit(date, "-", keep = 1:2)]
  dt[, month := month.abb[as.integer(month)]]
  dt[, date := NULL]
  dt
}

################################################################################
# extract_clim_data_points ----
################################################################################

# Function to process each power for point data
extract_clim_data_points <- function(
  power, # power = 1
  clim_data, # clim_data = tmp
  points_sf, # points_sf = prev_df
  rast_times, # rast_times = time_names
  var_name # var_name = "temp"
) {
  cat("Processing power: ", power, "\n", sep = "")
  # Apply power transformation
  clim_data_k <- clim_data^power

  cat("\tExtracting values\n")
  # Extract values at point locations
  clim_extract_k <- terra::extract(
    x = clim_data_k,
    y = points_sf,
    ID = FALSE
  )

  cat("\tAdding point IDs\n")
  # Add the point ID back
  clim_extract_k$point_id <- points_sf$point_id
  clim_extract_k$Lat <- points_sf$Lat
  clim_extract_k$Long <- points_sf$Long

  cat("\tRenaming columns\n")
  # Rename columns
  colnames(clim_extract_k) <- c(rast_times, "point_id", "Lat", "Long")

  cat("\tReshaping data\n")
  # Reshape from wide to long format
  clim_extract_k <- clim_extract_k |>
    tidyr::pivot_longer(
      cols = -c(point_id, Lat, Long),
      names_to = c("year", "month", "day"),
      names_sep = "-",
      values_to = paste0(var_name, ifelse(power == 1, "", power))
    ) |>
    dplyr::select(-day) |>
    dplyr::mutate(month = month.abb[as.numeric(month)])
  cat("\tDone\n\n")
  return(clim_extract_k)
}

################################################################################
# create_logger ----
################################################################################

create_logger <- function(log_file = NULL) {
  # Create directory up front if needed
  if (!is.null(log_file)) {
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    }
  }

  function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    formatted_msg <- sprintf("[%s] %s", timestamp, msg)
    cat(formatted_msg, "\n")
    if (!is.null(log_file)) {
      cat(formatted_msg, "\n", file = log_file, append = TRUE)
    }
    flush.console()
  }
}

################################################################################
# make_temp_terms ----
################################################################################

make_temp_terms <- function(n_lags = 0, n_leads = 0) {
  # Always include contemporaneous
  terms <- c("temp", "temp2")
  for (i in seq_len(n_lags)) {
    suffix <- ifelse(i == 1, "", i)
    terms <- c(terms, paste0("temp.lag", suffix), paste0("temp2.lag", suffix))
  }
  for (i in seq_len(n_leads)) {
    suffix <- ifelse(i == 1, "", i)
    terms <- c(terms, paste0("temp.lead", suffix), paste0("temp2.lead", suffix))
  }
  paste(terms, collapse = " + ")
}

################################################################################
# make_lag_form ----
################################################################################

make_lag_form <- function(n_lags = 0, n_leads = 0) {
  temp_terms <- make_temp_terms(n_lags, n_leads)
  as.formula(
    paste0(
      "PfPR2 ~ ",
      temp_terms,
      " + ",
      floodvars,
      " + ",
      droughtvars,
      " + I(intervention) + ",
      country_time,
      " | OBJECTID + as.factor(smllrgn):month | 0 | ",
      clustering
    )
  )
}

################################################################################
# baseline_adjust_summarize ----
################################################################################

baseline_adjust_summarize <- function(
  df,
  variable,
  baseline_group,
  adjusted_group,
  baseline_years,
  confidence_level = 0.90
) {
  lower_prob <- (1 - confidence_level) / 2
  upper_prob <- 1 - lower_prob

  var_sym <- rlang::sym(variable)
  baseline_syms <- rlang::syms(baseline_group)
  adjusted_syms <- rlang::syms(adjusted_group)

  baseline_means <- df |>
    dplyr::filter(year %in% baseline_years) |>
    dplyr::group_by(!!!baseline_syms) |>
    dplyr::summarize(
      baseline_mean = mean(!!var_sym, na.rm = TRUE),
      .groups = "drop"
    )

  adjusted <- df |>
    dplyr::left_join(baseline_means, by = baseline_group) |>
    dplyr::mutate(!!var_sym := (!!var_sym) - baseline_mean) |>
    dplyr::select(-baseline_mean)

  adjusted |>
    dplyr::group_by(!!!adjusted_syms) |>
    dplyr::summarize(
      mean = mean(!!var_sym, na.rm = TRUE),
      median = median(!!var_sym, na.rm = TRUE),
      upper = quantile(!!var_sym, upper_prob, na.rm = TRUE),
      lower = quantile(!!var_sym, lower_prob, na.rm = TRUE),
      .groups = "drop"
    )
}

################################################################################
# calc_hist_diff ----
################################################################################

calc_hist_regional_diff <- function(data, region_name = NULL) {
  if (!is.null(region_name)) {
    data <- data %>% dplyr::filter(region == region_name)
  }

  bm <- data |>
    dplyr::filter(year %in% 1900:1930) |>
    dplyr::group_by(model, scenario, run) |>
    dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE))

  boot_diffs <- data |>
    dplyr::left_join(bm) |>
    dplyr::mutate(Pred = Pred - BetaMean) |>
    dplyr::select(-BetaMean) |>
    dplyr::filter(year %in% 2010:2014, run != "main") |>
    dplyr::select(model, run, year, Pred, scenario) |>
    tidyr::pivot_wider(names_from = scenario, values_from = Pred) |>
    dplyr::group_by(model, run) |>
    dplyr::summarize(
      `hist-nat` = mean(`hist-nat`),
      historical = mean(historical)
    ) |>
    dplyr::mutate(diff = historical - `hist-nat`)  |>
    dplyr::pull(diff)

  results <- tibble::tibble(
    MeanDifference = mean(boot_diffs),
    ScaledMeanDifference = 100000 * mean(boot_diffs) / 100,
    Quantile_025 = quantile(boot_diffs, 0.025),
    Quantile_975 = quantile(boot_diffs, 0.975),
    ProportionPositive = prop.table(table(boot_diffs > 0))["TRUE"]
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 4)))

  if (!is.null(region_name)) {
    results <- results %>% dplyr::mutate(Region = region_name)
  } else {
    results <- results %>%
      dplyr::mutate(Region = "Sub-Saharan Africa (continent-wide)")
  }
}

################################################################################
# calc_future_regional_diff ----
################################################################################

calc_future_regional_diff <- function(data, region_name = NULL) {
  if (!is.null(region_name)) {
    data <- data |> dplyr::filter(region == region_name)
  }

  bm <- data |>
    dplyr::filter(year %in% 2015:2020) |>
    dplyr::group_by(model, scenario, run) |>
    dplyr::summarize(BetaMean = mean(Pred, na.rm = TRUE), .groups = "drop")

  df <- data |>
    dplyr::left_join(bm, by = c("model", "scenario", "run")) |>
    dplyr::mutate(Pred = Pred - BetaMean) |>
    dplyr::select(-BetaMean) |>
    dplyr::filter(run != "main")

  pred_by_run <- dplyr::bind_rows(
    df |>
      dplyr::filter(year %in% 2048:2052) |>
      dplyr::mutate(period = "2048-2052"),
    df |>
      dplyr::filter(year %in% 2096:2100) |>
      dplyr::mutate(period = "2096-2100")
  ) |>
    dplyr::group_by(run, model, scenario, period) |>
    dplyr::summarize(Pred = mean(Pred), .groups = "drop")

  results <- pred_by_run |>
    dplyr::group_by(scenario, period) |>
    dplyr::summarize(
      mean = mean(Pred),
      lower = quantile(Pred, 0.025),
      upper = quantile(Pred, 0.975),
      prop_positive = mean(Pred > 0),
      n_positive = sum(Pred > 0),
      n_total = n(),
      .groups = "drop"
    )

  if (!is.null(region_name)) {
    results <- dplyr::mutate(results, region = region_name)
  } else {
    results <- results |>
      dplyr::mutate(region = "Sub-Saharan Africa (continent-wide)")
  }

  return(results)
}

################################################################################
# generate_future_latex ----
################################################################################

generate_future_latex <- function(df) {
  header <- r"(\begin{tabular}{|l|l|lll|lll|}
    \hline
    \multirow{2}{*}{\textbf{Region}} & \multirow{2}{*}{\textbf{Scenario}} & \multicolumn{3}{c|}{\textbf{2048-2052}} & \multicolumn{3}{c|}{\textbf{2096-2100}} \\
    \cline{3-8}
    & & \textbf{Estimate} & \textbf{95\% CI} & \textbf{P$_{+}$} & \textbf{Estimate} & \textbf{95\% CI} & \textbf{P$_{+}$} \\
    \hline)"

  # Footer
  footer <- r"(\end{tabular})"

  # Get unique regions in order
  regions <- unique(df$Region)

  body_lines <- character(0)

  for (reg in regions) {
    sub <- df |> dplyr::filter(Region == reg)
    n <- nrow(sub)

    # Format the region label for LaTeX
    if (grepl("\n", reg)) {
      parts <- strsplit(reg, "\n")[[1]]
      region_label <- sprintf(
        r"(\begin{tabular}[c]{@{}l@{}}%s\\%s\end{tabular})",
        parts[1],
        parts[2]
      )
    } else {
      region_label <- reg
    }

    for (i in seq_len(n)) {
      row <- sub[i, ]

      # Format numeric values with padding for alignment
      est1 <- sprintf("%.3f", row$Estimate_2048_2052)
      est2 <- sprintf("%.3f", row$Estimate_2096_2100)
      pp1 <- sprintf("%.3f", row$PropPositive_2048_2052)
      pp2 <- sprintf("%.3f", row$PropPositive_2096_2100)

      if (i == 1) {
        region_cell <- sprintf("\\multirow{%d}{*}{%s}", n, region_label)
      } else {
        region_cell <- ""
      }

      line <- sprintf(
        "    %s & %s & %s & %s & %s & %s & %s & %s \\\\",
        region_cell,
        row$Scenario,
        est1,
        row$CI_2048_2052,
        pp1,
        est2,
        row$CI_2096_2100,
        pp2
      )
      body_lines <- c(body_lines, line)
    }
    body_lines <- c(body_lines, "    \\hline")
  }

  paste(c(header, body_lines, footer), collapse = "\n")
}

print("Finished loading A01 - Utility code for calculations.R")

################################################################################
# End of file ----
################################################################################
