# R/05_run_operational_percentile_workflow.R

# ------------------------------------------------------------
# Operational ERA5-Land soil moisture percentile workflow
#
# Produces operational percentile maps for target_layers.
# Default behavior: previous complete month.
# ------------------------------------------------------------

# -----------------------------
# User options
# -----------------------------

target_year <- NULL
target_month <- NULL

# Manual override example:
# target_year <- 2025
# target_month <- 5

layers_to_run <- c("swvl1", "swvl2")

overwrite_download <- TRUE
overwrite_percentile <- TRUE

make_qa_plot <- TRUE

clean_temp_files <- TRUE
delete_download_zip <- TRUE
delete_hourly_debug_files <- TRUE
delete_current_monthly_mean <- FALSE
delete_percentile_outputs <- FALSE


pause_between_layers <- TRUE
pause_seconds <- 180

# -----------------------------
# Load scripts
# -----------------------------

source("R/00_config.R")
source("R/03_download_current_month_era5land_hourly.R")
source("R/04_compute_monthly_percentile.R")

library(fs)
library(lubridate)
library(terra)

# -----------------------------
# Resolve target month
# -----------------------------

if (is.null(target_year) || is.null(target_month)) {
  target <- get_previous_month()
  target_year <- target$year
  target_month <- target$month
}

target_month_chr <- sprintf("%02d", target_month)

message("============================================================")
message("Operational ERA5-Land soil moisture percentile workflow")
message("Target month: ", target_year, "-", target_month_chr)
message("Layers: ", paste(layers_to_run, collapse = ", "))
message("============================================================")

# -----------------------------
# Check layers
# -----------------------------

bad_layers <- setdiff(layers_to_run, soil_layers$shortname)

if (length(bad_layers) > 0) {
  stop(
    "Invalid layers_to_run: ",
    paste(bad_layers, collapse = ", "),
    ". Valid options are: ",
    paste(soil_layers$shortname, collapse = ", ")
  )
}

# -----------------------------
# Output list
# -----------------------------

workflow_outputs <- list()

# -----------------------------
# Main layer loop
# -----------------------------

for (layer in layers_to_run) {
  
  message("============================================================")
  message("Processing layer: ", layer)
  message("Depth: ", get_soil_layer_depth(layer))
  message("============================================================")
  
  # Check climatology exists
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
  
  # ----------------------------------------------------------
  # Step 1: Download hourly ERA5-Land and aggregate to monthly mean
  # ----------------------------------------------------------
  
  message("------------------------------------------------------------")
  message("Step 1: Downloading hourly ERA5-Land and aggregating to monthly mean")
  message("Layer: ", layer)
  message("------------------------------------------------------------")
  
  current_file <- download_current_month_swvl(
    year = target_year,
    month = target_month,
    layer_shortname = layer,
    overwrite = overwrite_download,
    keep_hourly_tif = FALSE,
    delete_download_zip = delete_download_zip
  )
  
  if (!file.exists(current_file)) {
    stop("Current monthly mean file was not created: ", current_file)
  }
  
  message("Current monthly mean file: ", current_file)
  
  # ----------------------------------------------------------
  # Step 2: Compute percentile raster
  # ----------------------------------------------------------
  
  message("------------------------------------------------------------")
  message("Step 2: Computing monthly percentile raster")
  message("Layer: ", layer)
  message("------------------------------------------------------------")
  
  pct_files <- compute_monthly_percentile(
    year = target_year,
    month = target_month,
    layer_shortname = layer,
    current_file = current_file,
    overwrite = overwrite_percentile,
    make_qa_plot = make_qa_plot
  )
  
  if (!file.exists(pct_files$percentile)) {
    stop("Percentile raster was not created: ", pct_files$percentile)
  }
  
  if (!file.exists(pct_files$class)) {
    stop("Percentile class raster was not created: ", pct_files$class)
  }
  
  # ----------------------------------------------------------
  # Step 3: QA summary
  # ----------------------------------------------------------
  
  message("------------------------------------------------------------")
  message("Step 3: QA summary")
  message("Layer: ", layer)
  message("------------------------------------------------------------")
  
  pct <- rast(pct_files$percentile)
  
  pct_summary <- global(
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
      layer,
      ". Check current raster and climatology stack."
    )
  } else {
    message("Percentile values are within expected 0-100 range for ", layer)
  }
  
  workflow_outputs[[layer]] <- list(
    layer = layer,
    depth = get_soil_layer_depth(layer),
    current_monthly = current_file,
    percentile = pct_files$percentile,
    class = pct_files$class,
    summary = pct_summary
  )
  
  # ----------------------------------------------------------
  # Layer-specific cleanup
  # ----------------------------------------------------------
  
  if (delete_hourly_debug_files && exists("dir_current_raw") && dir.exists(dir_current_raw)) {
    
    debug_pattern <- paste0(
      layer,
      "_(hourly|daily_mean)_",
      target_year,
      "_",
      target_month_chr,
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
  }
  
  if (delete_current_monthly_mean) {
    
    if (file.exists(current_file)) {
      message("Deleting current monthly mean file: ", current_file)
      unlink(current_file, force = TRUE)
    }
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
  
  if (pause_between_layers && layer != tail(layers_to_run, 1)) {
    message("Pausing ", pause_seconds, " seconds before next CDS request.")
    Sys.sleep(pause_seconds)
  }
  
}

# -----------------------------
# General cleanup
# -----------------------------

message("------------------------------------------------------------")
message("General cleanup")
message("------------------------------------------------------------")

if (clean_temp_files && exists("dir_temp") && dir.exists(dir_temp)) {
  message("Cleaning temp directory: ", dir_temp)
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
}

# -----------------------------
# Final report
# -----------------------------

message("============================================================")
message("Workflow complete")
message("Target month: ", target_year, "-", target_month_chr)
message("============================================================")

for (layer in names(workflow_outputs)) {
  
  x <- workflow_outputs[[layer]]
  
  message("Layer: ", x$layer)
  message("Depth: ", x$depth)
  message("Current monthly mean: ", x$current_monthly)
  message("Percentile raster: ", x$percentile)
  message("Percentile class raster: ", x$class)
  
  if (make_qa_plot) {
    
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

workflow_outputs