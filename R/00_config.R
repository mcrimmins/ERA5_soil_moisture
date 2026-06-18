# R/00_config.R
# 6/18/26 MAC/ChatGPT


library(ecmwfr)
library(terra)
library(sf)
library(dplyr)
library(lubridate)
library(fs)

# ------------------------------------------------------------
# Project paths
# ------------------------------------------------------------

project_dir <- normalizePath(".")

dir_raw <- file.path(project_dir, "data", "raw", "era5land_monthly")
dir_temp <- file.path(project_dir, "data", "temp")

dir_clim <- file.path(project_dir, "data", "processed", "climatology")
dir_current_raw <- file.path(project_dir, "data", "raw", "era5land_hourly_current")
dir_current_monthly <- file.path(project_dir, "data", "processed", "current_monthly")
dir_percentiles <- file.path(project_dir, "data", "processed", "percentiles")

dir_mask_raw <- file.path(project_dir, "data", "raw", "era5land_mask_hourly")
dir_mask_monthly <- file.path(project_dir, "data", "processed", "snow_freeze_mask")

dir_logs <- file.path(project_dir, "logs")
dir_output <- file.path(project_dir, "output")
dir_qa <- file.path(dir_output, "qa_maps")
dir_maps <- file.path(dir_output, "maps")

dir_create(dir_raw, recurse = TRUE)
dir_create(dir_temp, recurse = TRUE)
dir_create(dir_clim, recurse = TRUE)
dir_create(dir_current_raw, recurse = TRUE)
dir_create(dir_current_monthly, recurse = TRUE)
dir_create(dir_percentiles, recurse = TRUE)
dir_create(dir_mask_raw, recurse = TRUE)
dir_create(dir_mask_monthly, recurse = TRUE)
dir_create(dir_logs, recurse = TRUE)
dir_create(dir_qa, recurse = TRUE)
dir_create(dir_maps, recurse = TRUE)

# ------------------------------------------------------------
# CDS / ECMWF key
# ------------------------------------------------------------

cds_key <- Sys.getenv("CDS_KEY")

if (cds_key == "") {
  stop(
    "CDS_KEY is not set. Add CDS_KEY=your_key_here to your project .Renviron, ",
    "then restart R."
  )
}

wf_set_key(key = cds_key)

# ------------------------------------------------------------
# ERA5-Land settings
# ------------------------------------------------------------

era5land_monthly_dataset <- "reanalysis-era5-land-monthly-means"
era5land_hourly_dataset <- "reanalysis-era5-land"

soil_layers <- data.frame(
  shortname = c("swvl1", "swvl2"),
  variable = c(
    "volumetric_soil_water_layer_1",
    "volumetric_soil_water_layer_2"
  ),
  depth = c("0-7 cm", "7-28 cm"),
  stringsAsFactors = FALSE
)

get_soil_layer_variable <- function(layer_shortname) {
  if (!layer_shortname %in% soil_layers$shortname) {
    stop(
      "Unknown soil layer: ", layer_shortname,
      ". Valid options are: ",
      paste(soil_layers$shortname, collapse = ", ")
    )
  }
  soil_layers$variable[soil_layers$shortname == layer_shortname]
}

get_soil_layer_depth <- function(layer_shortname) {
  if (!layer_shortname %in% soil_layers$shortname) {
    stop(
      "Unknown soil layer: ", layer_shortname,
      ". Valid options are: ",
      paste(soil_layers$shortname, collapse = ", ")
    )
  }
  soil_layers$depth[soil_layers$shortname == layer_shortname]
}

# Soil layers to process operationally.
target_layers <- c("swvl1", "swvl2")

# CDS area order: North, West, South, East.
aoi_cds <- c(49.8, -126.9, 23.7, -65.6)

# ------------------------------------------------------------
# Climatology settings
# ------------------------------------------------------------

# Current default baseline. Keep README examples consistent with this.
clim_years <- 1981:2020
clim_months <- 1:12

get_clim_label <- function() {
  paste0(min(clim_years), "_", max(clim_years))
}

# ------------------------------------------------------------
# Snow / frozen-ground mask settings
# ------------------------------------------------------------

mask_variables <- data.frame(
  shortname = c("snow_depth", "stl1"),
  variable = c(
    "snow_depth",
    "soil_temperature_level_1"
  ),
  description = c(
    "Snow depth",
    "Soil temperature level 1"
  ),
  stringsAsFactors = FALSE
)

get_mask_variable <- function(mask_shortname) {
  
  if (!mask_shortname %in% mask_variables$shortname) {
    stop(
      "Unknown mask variable: ",
      mask_shortname,
      ". Valid options are: ",
      paste(mask_variables$shortname, collapse = ", ")
    )
  }
  
  mask_variables$variable[mask_variables$shortname == mask_shortname]
}

dir_mask_raw <- file.path(project_dir, "data", "raw", "era5land_mask_hourly")
dir_mask_monthly <- file.path(project_dir, "data", "processed", "snow_freeze_mask")
dir_temperature <- file.path(project_dir, "data", "processed", "temperature")

dir_create(dir_mask_raw, recurse = TRUE)
dir_create(dir_mask_monthly, recurse = TRUE)
dir_create(dir_temperature, recurse = TRUE)

# Hard-mask thresholds
snow_depth_threshold_m <- 0.01
snow_fraction_threshold <- 0.25

soil_freeze_threshold_k <- 273.15
freeze_fraction_threshold <- 0.5

soil_temp_kelvin_offset <- 273.15

# IMPORTANT:
# If snow_depth_threshold_m or soil_freeze_threshold_k changes, rebuild
# diagnostics with create_snow_freeze_mask(..., overwrite_diagnostics = TRUE).

# ------------------------------------------------------------
# Shared date helpers
# ------------------------------------------------------------

get_previous_month <- function(today = Sys.Date()) {
  first_day_this_month <- floor_date(today, unit = "month")
  target_date <- first_day_this_month %m-% months(1)
  list(
    year = year(target_date),
    month = month(target_date)
  )
}

get_days_in_month <- function(year, month) {
  start_date <- as.Date(sprintf("%04d-%02d-01", year, month))
  end_date <- ceiling_date(start_date, unit = "month") - days(1)
  seq(start_date, end_date, by = "day")
}

# ------------------------------------------------------------
# CDS request helper
# ------------------------------------------------------------

safe_wf_request <- function(request,
                            path,
                            max_attempts = 5,
                            base_wait = 180,
                            jitter = 60) {
  last_error <- NULL
  
  for (attempt in seq_len(max_attempts)) {
    message("CDS request attempt ", attempt, " of ", max_attempts)
    
    result <- tryCatch(
      {
        wf_request(
          request = request,
          transfer = TRUE,
          path = path
        )
      },
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    
    if (!is.null(result) && file.exists(result)) {
      message("CDS request succeeded: ", result)
      return(result)
    }
    
    if (attempt < max_attempts) {
      wait_seconds <- base_wait * attempt + sample(0:jitter, 1)
      message("CDS request failed. Waiting ", wait_seconds, " seconds before retrying.")
      if (!is.null(last_error)) {
        message("Last error: ", conditionMessage(last_error))
      }
      Sys.sleep(wait_seconds)
    }
  }
  
  stop(
    "CDS request failed after ",
    max_attempts,
    " attempts. Last error: ",
    if (!is.null(last_error)) conditionMessage(last_error) else "unknown error"
  )
}

# ------------------------------------------------------------
# Hourly completeness helper
# ------------------------------------------------------------

check_complete_hourly_layers <- function(r_hourly,
                                         year,
                                         month,
                                         month_dates,
                                         time_vec,
                                         label = "ERA5-Land hourly data") {
  expected_layers <- length(month_dates) * length(time_vec)
  actual_layers <- terra::nlyr(r_hourly)
  
  message(label, " expected hourly layers: ", expected_layers)
  message(label, " actual hourly layers: ", actual_layers)
  
  if (actual_layers != expected_layers) {
    missing_layers <- expected_layers - actual_layers
    missing_days <- missing_layers / 24
    
    stop(
      "Incomplete hourly data for ",
      year,
      "-",
      sprintf("%02d", month),
      " in ",
      label,
      ". Expected ",
      expected_layers,
      " hourly layers but found ",
      actual_layers,
      ". Missing approximately ",
      round(missing_days, 2),
      " day(s). Do not create a monthly product from partial data. ",
      "Rerun when CDS data for the full previous month are complete."
    )
  }
  
  invisible(TRUE)
}

# ------------------------------------------------------------
# Raster alignment helper
# ------------------------------------------------------------

align_to_reference <- function(x,
                               reference,
                               method = "near",
                               label = "raster") {
  if (!terra::compareGeom(reference, x, stopOnError = FALSE)) {
    message(label, " does not align with reference raster. Resampling.")
    x <- terra::resample(x, reference, method = method)
  }
  
  if (!terra::compareGeom(reference, x, stopOnError = FALSE)) {
    stop(label, " still does not align after resampling.")
  }
  
  x
}

####
# long fixer
####

fix_longitude_0_360 <- function(r, label = "raster") {
  
  r_ext <- terra::ext(r)
  
  if (terra::xmin(r_ext) >= 0 && terra::xmax(r_ext) > 180) {
    
    message(label, " appears to use 0-360 longitude. Shifting to -180/180.")
    
    terra::ext(r) <- terra::ext(
      terra::xmin(r_ext) - 360,
      terra::xmax(r_ext) - 360,
      terra::ymin(r_ext),
      terra::ymax(r_ext)
    )
  }
  
  r
}

