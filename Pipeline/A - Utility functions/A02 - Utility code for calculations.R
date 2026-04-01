############################################################
# Utility functions for calculations including climate data
# extraction, precipitation extremes, correlation analysis,
# and R0 computations. Core helper functions used throughout
# the analysis pipeline. Should be sourced after A01.
############################################################

############################################################
# Start ----
############################################################

print("Begin loading A01 - Utility code for calculations.R")

############################################################
# swirl ----
############################################################

# This function takes in a raster that is in x=lat, y=lon format (native CRU is like this) and
# rotates it 90 degrees to be in x=lon, y=lat format. It also crops to the extent of Africa.
swirl <- function(input.raster) {
  input.raster <- flip(t(input.raster), direction = 'y')
  extent(input.raster) <- c(-180, 180, -90, 90)
  input.raster = crop(input.raster, c(-30, 60, -37, 40))
  return(input.raster)
}

############################################################
# r0t ----
############################################################

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

############################################################
# optT ----
############################################################

# function to compute optimal temp for each run
optT <- function(beta1, beta2) {
  opt = -beta1 / (2 * beta2)
  return(opt)
}

############################################################
# computePrcpExtremes ----
############################################################

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

############################################################
# analyze_corr ----
############################################################

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

############################################################
# df_to_latex ----
############################################################

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

############################################################
# count_pairwise_obs ----
############################################################

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

############################################################
# run_corr_specs ----
############################################################

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

############################################################
# extract_spatial_means ----
############################################################

extract_spatial_means <- function(velox_raster, spatial_polygons) {
  unlist(
    lapply(
      velox_raster$extract(
        spatial_polygons,
        fun = function(x) mean(x, na.rm = TRUE),
        small = TRUE
      ),
      mean,
      na.rm = TRUE
    )
  )
}

############################################################
# build_colnames ----
############################################################

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

############################################################
# .process_timestep ----
############################################################

.process_timestep <- function(timestep_idx) {
  base_raster <- swirl(raster::raster(.climate_array[,, timestep_idx]))

  n_regions <- nrow(.admin_regions@data)
  result <- numeric(n_regions * .max_power)

  for (power_k in seq_len(.max_power)) {
    powered_raster <- if (power_k == 1L) base_raster else base_raster^power_k
    velox_obj <- velox::velox(powered_raster)
    offset <- (power_k - 1L) * n_regions
    result[offset + seq_len(n_regions)] <- extract_spatial_means(
      velox_obj,
      .admin_regions
    )
  }

  result
}

############################################################
# extract_cru_variable ----
############################################################

extract_cru_variable <- function(
  nc_filepath,
  nc_varname,
  admin_sp,
  var_prefix,
  max_power = 5L,
  start_year = 1901L
) {
  nc_conn <- ncdf4::nc_open(nc_filepath)
  climate_array <- ncdf4::ncvar_get(nc_conn, nc_varname)
  longitudes <- ncdf4::ncvar_get(nc_conn, "lon")
  ncdf4::nc_close(nc_conn)

  if (dim(climate_array)[1] == length(longitudes)) {
    message(sprintf(
      "[%s] Input format as expected for CRU (lat,lon); will rotate to (lon,lat)",
      nc_varname
    ))
  } else {
    stop(sprintf(
      "[%s] Input format not as expected for CRU; stopping.",
      nc_varname
    ))
  }

  n_timesteps <- dim(climate_array)[3]
  n_regions <- nrow(admin_sp@data)

  .GlobalEnv$.climate_array <- climate_array
  .GlobalEnv$.admin_regions <- admin_sp
  .GlobalEnv$.max_power <- max_power

  on.exit(
    {
      rm(.climate_array, .admin_regions, .max_power, envir = .GlobalEnv)
    },
    add = TRUE
  )

  message(sprintf(
    "Starting parallel extraction [%s]: %d timesteps x %d powers across %d cores",
    var_prefix,
    n_timesteps,
    max_power,
    N_CORES
  ))

  t0 <- proc.time()

  results_list <- future.apply::future_lapply(
    seq_len(n_timesteps),
    .process_timestep,
    future.seed = FALSE,
    future.scheduling = 2.0
  )

  elapsed <- (proc.time() - t0)["elapsed"]
  message(sprintf(
    "[%s] Extraction complete in %.1f seconds (%.1f min)",
    var_prefix,
    elapsed,
    elapsed / 60
  ))

  results_mat <- do.call(rbind, results_list)

  all_colnames <- unlist(lapply(
    seq_len(n_timesteps),
    build_colnames,
    var_prefix = var_prefix,
    max_power = max_power,
    start_year = start_year
  ))

  for (timestep_idx in seq_len(n_timesteps)) {
    col_offset <- (timestep_idx - 1L) * max_power
    for (power_k in seq_len(max_power)) {
      col_name <- all_colnames[col_offset + power_k]
      region_offset <- (power_k - 1L) * n_regions
      admin_sp@data[[col_name]] <- results_mat[
        timestep_idx,
        region_offset + seq_len(n_regions)
      ]
    }
  }

  admin_sp
}

############################################################
# make_filename ----
############################################################

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

############################################################
# extract_long ----
############################################################

# Helper: extract, pivot to long, return data.table with one value column
extract_long <- function(rast, polygons, rast_times, value_name) {
  ex <- exactextractr::exact_extract(
    x = rast,
    y = polygons,
    fun = 'mean',
    progress = TRUE,
    append_cols = "OBJECTID"
  )
  colnames(ex) <- c("OBJECTID", rast_times)

  dt <- as.data.table(ex)
  dt <- melt(
    dt,
    id.vars = "OBJECTID",
    variable.name = "date",
    value.name = value_name
  )
  dt[, c("year", "month") := tstrsplit(date, "-", keep = 1:2)]
  dt[, month := month.abb[as.integer(month)]]
  dt[, date := NULL]
  dt
}

############################################################
# process_clim_powers_points ----
############################################################

# Function to process each power for point data
process_clim_powers_points <- function(
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

print("Finished loading A01 - Utility code for calculations.R")

############################################################
# create_logger ----
############################################################

create_logger <- function(log_file = NULL) {
  # Create directory up front if needed
  if (!is.null(log_file)) {
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }
  }

  function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    formatted_msg <- sprintf("[%s] %s", timestamp, msg)
    cat(formatted_msg, "\n")
    if (!is.null(log_file)) {
      cat(formatted_msg, "\n", file = log_file, append = TRUE)
    }
  }
}

############################################################
# make_temp_terms ----
############################################################

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

############################################################
# make_lag_form ----
############################################################

make_lag_form <- function(n_lags = 0, n_leads = 0) {
  temp_terms <- make_temp_terms(n_lags, n_leads)
  as.formula(paste0(
    "PfPR2 ~ ", temp_terms, " + ",
    floodvars, " + ", droughtvars,
    " + I(intervention) + ", country_time,
    " | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  ))
}


############################################################
# End of file ----
############################################################
