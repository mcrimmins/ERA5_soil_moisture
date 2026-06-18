# R/05_run_operational_percentile_workflow.R

# ------------------------------------------------------------
# Operational ERA5-Land soil moisture percentile workflow.
#
# This script produces full-calendar-month percentile products.
# For beginning-of-month reporting, run on the 6th or later, and
# preferably the 7th or 8th if CDS availability is delayed.
#
# Default behavior is resume/skip mode:
#   - existing monthly means are reused
#   - existing percentile products are reused
#   - existing snow/freeze diagnostics are reused
#   - existing snow/freeze mask is reused
#
# Set overwrite options to TRUE only when you intentionally want
# to rebuild products.
# ------------------------------------------------------------

tictoc::tic("Total workflow runtime")

# ------------------------------------------------------------
# User options: target month
# ------------------------------------------------------------

target_year <- NULL
target_month <- NULL

# Manual override example:
# target_year <- 2026
# target_month <- 5

layers_to_run <- c("swvl1", "swvl2")

# ------------------------------------------------------------
# User options: rebuild / overwrite behavior
# ------------------------------------------------------------

# FALSE = skip/reuse existing monthly mean soil moisture files.
# TRUE  = redownload hourly swvl data and rebuild monthly means.
overwrite_current_monthly <- FALSE

# FALSE = skip/reuse existing percentile and class rasters.
# TRUE  = recompute percentile and class rasters.
overwrite_percentiles <- FALSE

# FALSE = skip/reuse existing snow/frozen-ground mask.
# TRUE  = rebuild final categorical mask from diagnostics.
overwrite_mask <- FALSE

# FALSE = skip/reuse existing snow/stl1 monthly diagnostics.
# TRUE  = redownload snow/stl1 hourly data and rebuild diagnostics.
overwrite_mask_diagnostics <- FALSE

# ------------------------------------------------------------
# User options: optional products
# ------------------------------------------------------------

build_snow_freeze_mask <- TRUE
make_mask_qa_plot <- FALSE
make_percentile_qa_plot <- TRUE

# ------------------------------------------------------------
# User options: cleanup
# ------------------------------------------------------------

clean_temp_files <- TRUE
delete_download_zip <- TRUE
delete_hourly_debug_files <- TRUE

# Usually keep these. They make reruns much faster.
delete_current_monthly_mean <- FALSE
delete_percentile_outputs <- FALSE

# ------------------------------------------------------------
# User options: operational timing warning
# ------------------------------------------------------------

minimum_recommended_run_day <- 6
warn_if_before_recommended_day <- TRUE

# ------------------------------------------------------------
# Load dependencies and project scripts
# ------------------------------------------------------------

source("R/00_config.R")
source("R/03_download_current_month_era5land_hourly.R")
source("R/04_compute_monthly_percentile.R")

library(fs)
library(lubridate)
library(terra)

# ------------------------------------------------------------
# Resolve target month
# ------------------------------------------------------------

manual_target <- !(is.null(target_year) || is.null(target_month))

if (!manual_target) {
  
  target <- get_previous_month()
  target_year <- target$year
  target_month <- target$month
  
  if (
    warn_if_before_recommended_day &&
    lubridate::day(Sys.Date()) < minimum_recommended_run_day
  ) {
    warning(
      "It is only day ",
      lubridate::day(Sys.Date()),
      " of the month. Previous-month ERA5-Land hourly data may not be complete. ",
      "The workflow will check actual layer counts and stop if data are incomplete."
    )
  }
}

target_month_chr <- sprintf("%02d", target_month)

# ------------------------------------------------------------
# Validate user options
# ------------------------------------------------------------

bad_layers <- setdiff(layers_to_run, soil_layers$shortname)

if (length(bad_layers) > 0) {
  stop(
    "Invalid layers_to_run: ",
    paste(bad_layers, collapse = ", "),
    ". Valid options are: ",
    paste(soil_layers$shortname, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Startup report
# ------------------------------------------------------------

message("============================================================")
message("Operational ERA5-Land soil moisture percentile workflow")
message("Target month: ", target_year, "-", target_month_chr)
message("Layers: ", paste(layers_to_run, collapse = ", "))
message("Climatology baseline: ", get_clim_label())
message("Full-month product only; no rolling window is used.")
message("------------------------------------------------------------")
message("Overwrite current monthly means: ", overwrite_current_monthly)
message("Overwrite percentiles: ", overwrite_percentiles)
message("Build snow/freeze mask: ", build_snow_freeze_mask)
message("Overwrite snow/freeze mask: ", overwrite_mask)
message("Overwrite snow/freeze diagnostics: ", overwrite_mask_diagnostics)
message("============================================================")

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

file_status <- function(path) {
  if (file.exists(path)) {
    paste0("exists: ", path)
  } else {
    paste0("missing: ", path)
  }
}

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " was not created or does not exist:\n", path)
  }
  invisible(TRUE)
}

get_current_monthly_file <- function(year, month, layer_shortname) {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_current_monthly,
    paste0(
      layer_shortname,
      "_monthly_mean_",
      year,
      "_",
      month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
}

get_percentile_file <- function(year, month, layer_shortname) {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_percentiles,
    paste0(
      layer_shortname,
      "_percentile_",
      year,
      "_",
      month_chr,
      "_vs_",
      get_clim_label(),
      ".tif"
    )
  )
}

get_percentile_class_file <- function(year, month, layer_shortname) {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_percentiles,
    paste0(
      layer_shortname,
      "_percentile_class_",
      year,
      "_",
      month_chr,
      "_vs_",
      get_clim_label(),
      ".tif"
    )
  )
}

summarize_percentile_raster <- function(percentile_file, layer_shortname) {
  
  pct <- terra::rast(percentile_file)
  pct_summary <- terra::global(
    pct,
    c("min", "mean", "max"),
    na.rm = TRUE
  )
  
  print(pct_summary)
  
  pct_min <- pct_summary[1, "min"]
  pct_max <- pct_summary[1, "max"]
  
  if (pct_min < 0 || pct_max > 100) {
    warning(
      "Percentile values outside expected 0-100 range for ",
      layer_shortname,
      ". Check current raster and climatology stack."
    )
  } else {
    message("Percentile values are within expected 0-100 range for ", layer_shortname)
  }
  
  pct_summary
}

cleanup_layer_debug_files <- function(layer_shortname, year, month) {
  
  if (!delete_hourly_debug_files) {
    return(invisible(character(0)))
  }
  
  if (!exists("dir_current_raw") || !dir.exists(dir_current_raw)) {
    return(invisible(character(0)))
  }
  
  month_chr <- sprintf("%02d", month)
  
  debug_pattern <- paste0(
    layer_shortname,
    "_(hourly|daily_mean)_",
    year,
    "_",
    month_chr,
    "_ERA5Land\\.tif$"
  )
  
  debug_files <- list.files(
    dir_current_raw,
    pattern = debug_pattern,
    full.names = TRUE
  )
  
  if (length(debug_files) > 0) {
    message("Deleting hourly/daily debug file(s):")
    print(debug_files)
    unlink(debug_files, force = TRUE)
  }
  
  invisible(debug_files)
}

# ------------------------------------------------------------
# Output list
# ------------------------------------------------------------

workflow_outputs <- list()

# ------------------------------------------------------------
# Main soil moisture loop
# ------------------------------------------------------------

for (layer in layers_to_run) {
  
  message("============================================================")
  message("Processing layer: ", layer)
  message("Depth: ", get_soil_layer_depth(layer))
  message("============================================================")
  
  clim_file <- get_climatology_stack_file(
    month = target_month,
    layer_shortname = layer
  )
  
  if (!file.exists(clim_file)) {
    stop(
      "Missing climatology stack:\n",
      clim_file,
      "\nRun R/02_build_climatology.R for layer ",
      layer,
      " before running this workflow."
    )
  }
  
  message("Found climatology stack: ", clim_file)
  
  expected_current_file <- get_current_monthly_file(
    year = target_year,
    month = target_month,
    layer_shortname = layer
  )
  
  expected_percentile_file <- get_percentile_file(
    year = target_year,
    month = target_month,
    layer_shortname = layer
  )
  
  expected_class_file <- get_percentile_class_file(
    year = target_year,
    month = target_month,
    layer_shortname = layer
  )
  
  # ----------------------------------------------------------
  # Step 1: Monthly mean soil moisture
  # ----------------------------------------------------------
  
  message("------------------------------------------------------------")
  message("Step 1: Monthly mean soil moisture")
  message("Layer: ", layer)
  message("------------------------------------------------------------")
  
  if (file.exists(expected_current_file) && !overwrite_current_monthly) {
    
    current_file <- expected_current_file
    message("Reusing existing current monthly mean:")
    message(current_file)
    
  } else {
    
    message("Downloading hourly ERA5-Land and aggregating to monthly mean.")
    
    current_file <- download_current_month_swvl(
      year = target_year,
      month = target_month,
      layer_shortname = layer,
      overwrite = overwrite_current_monthly,
      keep_hourly_tif = FALSE,
      delete_download_zip = delete_download_zip
    )
  }
  
  stop_if_missing(current_file, "Current monthly mean file")
  
  # ----------------------------------------------------------
  # Step 2: Percentile products
  # ----------------------------------------------------------
  
  message("------------------------------------------------------------")
  message("Step 2: Monthly percentile products")
  message("Layer: ", layer)
  message("------------------------------------------------------------")
  
  if (
    file.exists(expected_percentile_file) &&
    file.exists(expected_class_file) &&
    !overwrite_percentiles
  ) {
    
    pct_files <- list(
      percentile = expected_percentile_file,
      class = expected_class_file
    )
    
    message("Reusing existing percentile raster:")
    message(pct_files$percentile)
    message("Reusing existing percentile class raster:")
    message(pct_files$class)
    
  } else {
    
    message("Computing percentile and class rasters.")
    
    pct_files <- compute_monthly_percentile(
      year = target_year,
      month = target_month,
      layer_shortname = layer,
      current_file = current_file,
      overwrite = overwrite_percentiles,
      make_qa_plot = make_percentile_qa_plot
    )
  }
  
  stop_if_missing(pct_files$percentile, "Percentile raster")
  stop_if_missing(pct_files$class, "Percentile class raster")
  
  # ----------------------------------------------------------
  # Step 3: QA summary
  # ----------------------------------------------------------
  
  message("------------------------------------------------------------")
  message("Step 3: QA summary")
  message("Layer: ", layer)
  message("------------------------------------------------------------")
  
  pct_summary <- summarize_percentile_raster(
    percentile_file = pct_files$percentile,
    layer_shortname = layer
  )
  
  # ----------------------------------------------------------
  # Record outputs
  # ----------------------------------------------------------
  
  workflow_outputs[[layer]] <- list(
    layer = layer,
    depth = get_soil_layer_depth(layer),
    climatology_stack = clim_file,
    current_monthly = current_file,
    percentile = pct_files$percentile,
    class = pct_files$class,
    summary = pct_summary
  )
  
  # ----------------------------------------------------------
  # Optional cleanup
  # ----------------------------------------------------------
  
  cleanup_layer_debug_files(
    layer_shortname = layer,
    year = target_year,
    month = target_month
  )
  
  if (delete_current_monthly_mean && file.exists(current_file)) {
    message("Deleting current monthly mean file: ", current_file)
    unlink(current_file, force = TRUE)
  }
  
  if (delete_percentile_outputs) {
    
    message("Deleting final percentile outputs as requested for ", layer)
    
    if (file.exists(pct_files$percentile)) {
      unlink(pct_files$percentile, force = TRUE)
    }
    
    if (file.exists(pct_files$class)) {
      unlink(pct_files$class, force = TRUE)
    }
  }
}

# ------------------------------------------------------------
# Optional snow/frozen-ground mask and soil-temperature outputs
# ------------------------------------------------------------

mask_file <- NULL
temperature_c_file <- NULL
temperature_k_file <- NULL
freeze_fraction_file <- NULL

if (build_snow_freeze_mask) {
  
  message("============================================================")
  message("Snow/frozen-ground mask and soil-temperature outputs")
  message("============================================================")
  
  source("R/06_download_snow_freeze_monthly.R")
  
  expected_mask_file <- get_snow_freeze_mask_file(
    year = target_year,
    month = target_month
  )
  
  expected_temperature_c_file <- file.path(
    dir_temperature,
    paste0(
      "stl1_monthly_mean_c_",
      target_year,
      "_",
      target_month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
  
  expected_temperature_k_file <- file.path(
    dir_temperature,
    paste0(
      "stl1_monthly_mean_k_",
      target_year,
      "_",
      target_month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
  
  expected_freeze_fraction_file <- file.path(
    dir_temperature,
    paste0(
      "stl1_freeze_fraction_",
      target_year,
      "_",
      target_month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
  
  mask_outputs_complete <- file.exists(expected_mask_file) &&
    file.exists(expected_temperature_c_file) &&
    file.exists(expected_temperature_k_file) &&
    file.exists(expected_freeze_fraction_file)
  
  if (mask_outputs_complete && !overwrite_mask && !overwrite_mask_diagnostics) {
    
    message("Reusing existing snow/freeze mask and soil-temperature outputs.")
    
    mask_file <- expected_mask_file
    temperature_c_file <- expected_temperature_c_file
    temperature_k_file <- expected_temperature_k_file
    freeze_fraction_file <- expected_freeze_fraction_file
    
  } else {
    
    message("Creating or updating snow/freeze mask and diagnostics.")
    
    mask_file <- create_snow_freeze_mask(
      year = target_year,
      month = target_month,
      overwrite = overwrite_mask,
      overwrite_diagnostics = overwrite_mask_diagnostics,
      delete_download_zip = delete_download_zip
    )
    
    temperature_c_file <- expected_temperature_c_file
    temperature_k_file <- expected_temperature_k_file
    freeze_fraction_file <- expected_freeze_fraction_file
  }
  
  stop_if_missing(mask_file, "Snow/frozen-ground mask")
  stop_if_missing(temperature_c_file, "Soil temperature Celsius raster")
  stop_if_missing(temperature_k_file, "Soil temperature Kelvin raster")
  stop_if_missing(freeze_fraction_file, "Freeze-fraction raster")
  
  workflow_outputs$snow_freeze_mask <- mask_file
  
  workflow_outputs$soil_temperature <- list(
    monthly_mean_c = temperature_c_file,
    monthly_mean_k = temperature_k_file,
    freeze_fraction = freeze_fraction_file
  )
  
  if (make_mask_qa_plot) {
    plot_snow_freeze_mask(
      year = target_year,
      month = target_month
    )
  }
}

# ------------------------------------------------------------
# General cleanup
# ------------------------------------------------------------

message("------------------------------------------------------------")
message("General cleanup")
message("------------------------------------------------------------")

if (clean_temp_files && exists("dir_temp") && dir.exists(dir_temp)) {
  message("Cleaning temp directory: ", dir_temp)
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
}

# ------------------------------------------------------------
# Final report
# ------------------------------------------------------------

message("============================================================")
message("Workflow complete")
message("Target month: ", target_year, "-", target_month_chr)
message("============================================================")

for (layer in layers_to_run) {
  
  x <- workflow_outputs[[layer]]
  
  message("Layer: ", x$layer)
  message("Depth: ", x$depth)
  message("Climatology stack: ", x$climatology_stack)
  message("Current monthly mean: ", x$current_monthly)
  message("Percentile raster: ", x$percentile)
  message("Percentile class raster: ", x$class)
  
  if (make_percentile_qa_plot) {
    qa_file <- file.path(
      dir_qa,
      paste0(
        "qa_",
        layer,
        "_percentile_",
        target_year,
        "_",
        target_month_chr,
        ".png"
      )
    )
    message("QA plot: ", qa_file)
  }
  
  message("------------------------------------------------------------")
}

if (!is.null(mask_file)) {
  message("Snow/frozen-ground mask: ", mask_file)
  message("------------------------------------------------------------")
}

if (!is.null(temperature_c_file)) {
  message("Soil temperature monthly mean C: ", temperature_c_file)
  message("Soil temperature monthly mean K: ", temperature_k_file)
  message("Soil temperature freeze fraction: ", freeze_fraction_file)
  message("------------------------------------------------------------")
}

message("Recommended map script:")
message("source(\"R/07_plot_percentile_maps.R\")")

message("============================================================")
message("Returned object: workflow_outputs")
message("============================================================")

workflow_outputs

tictoc::toc()