# R/06_download_snow_freeze_monthly.R

source("R/00_config.R")

library(ecmwfr)
library(terra)
library(archive)
library(lubridate)
library(fs)
library(tictoc)

# ------------------------------------------------------------
# Purpose:
#   Download ERA5-Land hourly snow depth and top-layer soil
#   temperature, aggregate to monthly diagnostics, preserve
#   soil-temperature reporting layers, and create a hard
#   snow/frozen-ground caution mask.
#
# Mask classes:
#   0 = not masked
#   1 = snow
#   2 = frozen ground
#   3 = snow + frozen ground
# ------------------------------------------------------------

# ------------------------------------------------------------
# Safety defaults, used only if not already defined in config
# ------------------------------------------------------------

if (!exists("dir_mask_raw")) {
  dir_mask_raw <- file.path(project_dir, "data", "raw", "era5land_mask_hourly")
}

if (!exists("dir_mask_monthly")) {
  dir_mask_monthly <- file.path(project_dir, "data", "processed", "snow_freeze_mask")
}

if (!exists("dir_temperature")) {
  dir_temperature <- file.path(project_dir, "data", "processed", "temperature")
}

fs::dir_create(dir_mask_raw, recurse = TRUE)
fs::dir_create(dir_mask_monthly, recurse = TRUE)
fs::dir_create(dir_temperature, recurse = TRUE)

if (!exists("snow_depth_threshold_m")) {
  snow_depth_threshold_m <- 0.01
}

if (!exists("snow_fraction_threshold")) {
  snow_fraction_threshold <- 0.25
}

if (!exists("soil_freeze_threshold_k")) {
  soil_freeze_threshold_k <- 273.15
}

if (!exists("freeze_fraction_threshold")) {
  freeze_fraction_threshold <- 0.25
}

if (!exists("soil_temp_kelvin_offset")) {
  soil_temp_kelvin_offset <- 273.15
}

if (!exists("mask_variables")) {
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
}

if (!exists("get_mask_variable")) {
  
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
}

# ------------------------------------------------------------
# Date helpers
# ------------------------------------------------------------

if (!exists("get_previous_month")) {
  
  get_previous_month <- function(today = Sys.Date()) {
    
    first_day_this_month <- floor_date(today, unit = "month")
    target_date <- first_day_this_month %m-% months(1)
    
    list(
      year = year(target_date),
      month = month(target_date)
    )
  }
}

if (!exists("get_days_in_month")) {
  
  get_days_in_month <- function(year, month) {
    
    start_date <- as.Date(sprintf("%04d-%02d-01", year, month))
    end_date <- ceiling_date(start_date, unit = "month") - days(1)
    
    seq(start_date, end_date, by = "day")
  }
}

# ------------------------------------------------------------
# Raster alignment helper
# ------------------------------------------------------------

if (!exists("align_to_reference")) {
  
  align_to_reference <- function(x,
                                 reference,
                                 method = "near",
                                 label = "raster") {
    
    if (!terra::compareGeom(reference, x, stopOnError = FALSE)) {
      
      message(label, " does not align with reference raster. Resampling.")
      
      message("Reference raster:")
      print(reference)
      
      message(label, " before alignment:")
      print(x)
      
      x <- terra::resample(
        x,
        reference,
        method = method
      )
    }
    
    if (!terra::compareGeom(reference, x, stopOnError = FALSE)) {
      stop(label, " still does not align after resampling.")
    }
    
    x
  }
}

# ------------------------------------------------------------
# Hourly completeness helper
# ------------------------------------------------------------

if (!exists("check_complete_hourly_layers")) {
  
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
        " day(s). ",
        "Do not create a monthly product from partial data. ",
        "Rerun on the 6th-8th of the month or after CDS data are complete."
      )
    }
    
    invisible(TRUE)
  }
}

# ------------------------------------------------------------
# Output path helpers
# ------------------------------------------------------------

get_mask_diagnostic_file <- function(year,
                                     month,
                                     mask_shortname,
                                     statistic) {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_mask_monthly,
    paste0(
      mask_shortname,
      "_",
      statistic,
      "_",
      year,
      "_",
      month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
}

get_snow_freeze_mask_file <- function(year, month) {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_mask_monthly,
    paste0(
      "snow_freeze_mask_",
      year,
      "_",
      month_chr,
      ".tif"
    )
  )
}

get_temperature_file <- function(year,
                                 month,
                                 statistic,
                                 units = c("c", "k")) {
  
  units <- match.arg(units)
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_temperature,
    paste0(
      "stl1_",
      statistic,
      "_",
      units,
      "_",
      year,
      "_",
      month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
}

get_freeze_fraction_file <- function(year, month) {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_temperature,
    paste0(
      "stl1_freeze_fraction_",
      year,
      "_",
      month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
}

# ------------------------------------------------------------
# Download one hourly variable and create monthly diagnostics
# ------------------------------------------------------------

download_monthly_mask_variable <- function(year = NULL,
                                           month = NULL,
                                           mask_shortname,
                                           overwrite = FALSE,
                                           delete_download_zip = TRUE) {
  
  if (is.null(year) || is.null(month)) {
    target <- get_previous_month()
    year <- target$year
    month <- target$month
  }
  
  if (!mask_shortname %in% mask_variables$shortname) {
    stop(
      "Unknown mask_shortname: ",
      mask_shortname,
      ". Valid options are: ",
      paste(mask_variables$shortname, collapse = ", ")
    )
  }
  
  mask_variable <- get_mask_variable(mask_shortname)
  month_chr <- sprintf("%02d", month)
  
  mean_file <- get_mask_diagnostic_file(
    year = year,
    month = month,
    mask_shortname = mask_shortname,
    statistic = "monthly_mean"
  )
  
  fraction_file <- get_mask_diagnostic_file(
    year = year,
    month = month,
    mask_shortname = mask_shortname,
    statistic = "monthly_fraction"
  )
  
  # Temperature reporting outputs
  stl1_mean_k_file <- get_temperature_file(
    year = year,
    month = month,
    statistic = "monthly_mean",
    units = "k"
  )
  
  stl1_mean_c_file <- get_temperature_file(
    year = year,
    month = month,
    statistic = "monthly_mean",
    units = "c"
  )
  
  stl1_freeze_fraction_file <- get_freeze_fraction_file(
    year = year,
    month = month
  )
  
  if (mask_shortname == "stl1") {
    
    if (
      file.exists(mean_file) &&
      file.exists(fraction_file) &&
      file.exists(stl1_mean_k_file) &&
      file.exists(stl1_mean_c_file) &&
      file.exists(stl1_freeze_fraction_file) &&
      !overwrite
    ) {
      message("Already exists: ", mean_file)
      message("Already exists: ", fraction_file)
      message("Already exists: ", stl1_mean_k_file)
      message("Already exists: ", stl1_mean_c_file)
      message("Already exists: ", stl1_freeze_fraction_file)
      
      return(
        list(
          mean = mean_file,
          fraction = fraction_file,
          temperature_k = stl1_mean_k_file,
          temperature_c = stl1_mean_c_file,
          freeze_fraction = stl1_freeze_fraction_file
        )
      )
    }
    
  } else {
    
    if (file.exists(mean_file) && file.exists(fraction_file) && !overwrite) {
      message("Already exists: ", mean_file)
      message("Already exists: ", fraction_file)
      
      return(
        list(
          mean = mean_file,
          fraction = fraction_file
        )
      )
    }
  }
  
  month_dates <- get_days_in_month(year, month)
  day_vec <- sprintf("%02d", day(month_dates))
  time_vec <- sprintf("%02d:00", 0:23)
  
  zip_target <- paste0(
    "era5land_",
    mask_shortname,
    "_hourly_",
    year,
    "_",
    month_chr,
    ".zip"
  )
  
  request <- list(
    dataset_short_name = era5land_hourly_dataset,
    variable = list(mask_variable),
    year = list(as.character(year)),
    month = list(month_chr),
    day = as.list(day_vec),
    time = as.list(time_vec),
    data_format = "netcdf",
    download_format = "zip",
    area = aoi_cds,
    target = zip_target
  )
  
  fs::dir_create(dir_mask_raw, recurse = TRUE)
  fs::dir_create(dir_temp, recurse = TRUE)
  
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
  
  message("Downloading ERA5-Land hourly ", mask_shortname, " for ", year, "-", month_chr)
  
  tictoc::tic()
  
  if (exists("safe_wf_request")) {
    
    zip_file <- safe_wf_request(
      request = request,
      path = dir_mask_raw,
      max_attempts = 5,
      base_wait = 180,
      jitter = 60
    )
    
  } else {
    
    zip_file <- wf_request(
      request = request,
      transfer = TRUE,
      path = dir_mask_raw
    )
  }
  
  tictoc::toc()
  
  if (!file.exists(zip_file)) {
    stop("Download failed or file not found: ", zip_file)
  }
  
  message("Extracting: ", zip_file)
  
  archive::archive_extract(zip_file, dir = dir_temp)
  
  nc_files <- list.files(
    dir_temp,
    pattern = "\\.nc$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(nc_files) == 0) {
    stop("No NetCDF file found after extracting: ", zip_file)
  }
  
  if (length(nc_files) > 1) {
    message("Multiple NetCDF files found. Using first:")
    print(nc_files)
  }
  
  nc_file <- nc_files[1]
  
  message("Reading NetCDF: ", nc_file)
  
  r_hourly <- terra::rast(nc_file)
  
  check_complete_hourly_layers(
    r_hourly = r_hourly,
    year = year,
    month = month,
    month_dates = month_dates,
    time_vec = time_vec,
    label = paste0("Snow/freeze diagnostic ", mask_shortname)
  )
  
  # ----------------------------------------------------------
  # Monthly mean diagnostic
  # ----------------------------------------------------------
  
  message("Computing monthly mean for ", mask_shortname)
  
  r_mean <- terra::app(
    r_hourly,
    mean,
    na.rm = TRUE
  )
  
  names(r_mean) <- paste0(
    mask_shortname,
    "_monthly_mean_",
    year,
    "_",
    month_chr
  )
  
  terra::writeRaster(
    r_mean,
    filename = mean_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  message("Wrote monthly mean: ", mean_file)
  
  # ----------------------------------------------------------
  # Preserve soil temperature reporting layers
  # ----------------------------------------------------------
  
  if (mask_shortname == "stl1") {
    
    r_mean_k <- r_mean
    r_mean_c <- r_mean - soil_temp_kelvin_offset
    
    names(r_mean_k) <- paste0(
      "stl1_monthly_mean_k_",
      year,
      "_",
      month_chr
    )
    
    names(r_mean_c) <- paste0(
      "stl1_monthly_mean_c_",
      year,
      "_",
      month_chr
    )
    
    terra::writeRaster(
      r_mean_k,
      filename = stl1_mean_k_file,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW")
    )
    
    terra::writeRaster(
      r_mean_c,
      filename = stl1_mean_c_file,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW")
    )
    
    message("Wrote soil temperature monthly mean K: ", stl1_mean_k_file)
    message("Wrote soil temperature monthly mean C: ", stl1_mean_c_file)
  }
  
  # ----------------------------------------------------------
  # Monthly persistence/fraction diagnostic
  # ----------------------------------------------------------
  
  if (mask_shortname == "snow_depth") {
    
    message(
      "Computing snow fraction using threshold: ",
      snow_depth_threshold_m,
      " m"
    )
    
    r_fraction <- terra::app(
      r_hourly > snow_depth_threshold_m,
      mean,
      na.rm = TRUE
    )
    
    names(r_fraction) <- paste0(
      "snow_fraction_",
      year,
      "_",
      month_chr
    )
    
  } else if (mask_shortname == "stl1") {
    
    message(
      "Computing frozen-ground fraction using threshold: ",
      soil_freeze_threshold_k,
      " K"
    )
    
    r_fraction <- terra::app(
      r_hourly <= soil_freeze_threshold_k,
      mean,
      na.rm = TRUE
    )
    
    names(r_fraction) <- paste0(
      "freeze_fraction_",
      year,
      "_",
      month_chr
    )
    
  } else {
    
    stop("No fraction rule defined for mask_shortname: ", mask_shortname)
  }
  
  terra::writeRaster(
    r_fraction,
    filename = fraction_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  message("Wrote monthly fraction: ", fraction_file)
  
  # ----------------------------------------------------------
  # Preserve freeze fraction reporting layer
  # ----------------------------------------------------------
  
  if (mask_shortname == "stl1") {
    
    names(r_fraction) <- paste0(
      "stl1_freeze_fraction_",
      year,
      "_",
      month_chr
    )
    
    terra::writeRaster(
      r_fraction,
      filename = stl1_freeze_fraction_file,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW")
    )
    
    message("Wrote soil temperature freeze fraction: ", stl1_freeze_fraction_file)
  }
  
  # ----------------------------------------------------------
  # Cleanup
  # ----------------------------------------------------------
  
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
  
  if (delete_download_zip && file.exists(zip_file)) {
    message("Deleting downloaded zip: ", zip_file)
    unlink(zip_file, force = TRUE)
  }
  
  if (mask_shortname == "stl1") {
    
    return(
      list(
        mean = mean_file,
        fraction = fraction_file,
        temperature_k = stl1_mean_k_file,
        temperature_c = stl1_mean_c_file,
        freeze_fraction = stl1_freeze_fraction_file
      )
    )
    
  } else {
    
    return(
      list(
        mean = mean_file,
        fraction = fraction_file
      )
    )
  }
}

# ------------------------------------------------------------
# Create categorical snow/freeze mask
# ------------------------------------------------------------

create_snow_freeze_mask <- function(year = NULL,
                                    month = NULL,
                                    overwrite = FALSE,
                                    overwrite_diagnostics = FALSE,
                                    delete_download_zip = TRUE) {
  
  if (is.null(year) || is.null(month)) {
    target <- get_previous_month()
    year <- target$year
    month <- target$month
  }
  
  month_chr <- sprintf("%02d", month)
  
  out_mask_file <- get_snow_freeze_mask_file(year, month)
  
  if (file.exists(out_mask_file) && !overwrite) {
    message("Already exists: ", out_mask_file)
    return(out_mask_file)
  }
  
  message("============================================================")
  message("Creating snow/frozen-ground mask")
  message("Target month: ", year, "-", month_chr)
  message("Snow depth threshold: ", snow_depth_threshold_m, " m")
  message("Snow fraction threshold: ", snow_fraction_threshold)
  message("Soil freeze threshold: ", soil_freeze_threshold_k, " K")
  message("Freeze fraction threshold: ", freeze_fraction_threshold)
  message("============================================================")
  
  # ----------------------------------------------------------
  # Get diagnostics
  # ----------------------------------------------------------
  
  snow_files <- download_monthly_mask_variable(
    year = year,
    month = month,
    mask_shortname = "snow_depth",
    overwrite = overwrite_diagnostics,
    delete_download_zip = delete_download_zip
  )
  
  stl1_files <- download_monthly_mask_variable(
    year = year,
    month = month,
    mask_shortname = "stl1",
    overwrite = overwrite_diagnostics,
    delete_download_zip = delete_download_zip
  )
  
  snow_mean <- terra::rast(snow_files$mean)
  snow_fraction <- terra::rast(snow_files$fraction)
  
  stl1_mean <- terra::rast(stl1_files$mean)
  freeze_fraction <- terra::rast(stl1_files$fraction)
  
  # ----------------------------------------------------------
  # Align everything to snow_mean
  # ----------------------------------------------------------
  
  snow_fraction <- align_to_reference(
    x = snow_fraction,
    reference = snow_mean,
    method = "near",
    label = "Snow-fraction raster"
  )
  
  stl1_mean <- align_to_reference(
    x = stl1_mean,
    reference = snow_mean,
    method = "near",
    label = "Soil-temperature mean raster"
  )
  
  freeze_fraction <- align_to_reference(
    x = freeze_fraction,
    reference = snow_mean,
    method = "near",
    label = "Freeze-fraction raster"
  )
  
  # ----------------------------------------------------------
  # Build mask flags
  # ----------------------------------------------------------
  
  snow_flag <- (snow_mean > snow_depth_threshold_m) |
    (snow_fraction >= snow_fraction_threshold)
  
  freeze_flag <- (stl1_mean <= soil_freeze_threshold_k) |
    (freeze_fraction >= freeze_fraction_threshold)
  
  # 0 = no mask
  # 1 = snow
  # 2 = frozen ground
  # 3 = snow + frozen ground
  mask <- snow_flag + (2 * freeze_flag)
  
  names(mask) <- "snow_freeze_mask"
  
  terra::writeRaster(
    mask,
    filename = out_mask_file,
    overwrite = TRUE,
    datatype = "INT1U",
    gdal = c("COMPRESS=LZW")
  )
  
  message("Wrote snow/frozen-ground mask: ", out_mask_file)
  
  message("Soil temperature Celsius output: ", stl1_files$temperature_c)
  message("Soil temperature Kelvin output: ", stl1_files$temperature_k)
  message("Freeze fraction output: ", stl1_files$freeze_fraction)
  
  # ----------------------------------------------------------
  # QA summary
  # ----------------------------------------------------------
  
  mask_freq <- terra::freq(mask)
  
  message("Mask class frequency:")
  print(mask_freq)
  
  message("Mask class definitions:")
  message("0 = not masked")
  message("1 = snow")
  message("2 = frozen ground")
  message("3 = snow + frozen ground")
  
  out_mask_file
}

# ------------------------------------------------------------
# Quick QA plot
# ------------------------------------------------------------

plot_snow_freeze_mask <- function(year = NULL,
                                  month = NULL) {
  
  if (is.null(year) || is.null(month)) {
    target <- get_previous_month()
    year <- target$year
    month <- target$month
  }
  
  mask_file <- get_snow_freeze_mask_file(year, month)
  
  if (!file.exists(mask_file)) {
    stop("Mask file does not exist: ", mask_file)
  }
  
  mask <- terra::rast(mask_file)
  
  plot(
    mask,
    type = "classes",
    col = c("white", "lightblue", "gray70", "mediumpurple"),
    main = paste0("Snow/frozen-ground mask: ", year, "-", sprintf("%02d", month))
  )
  
  invisible(mask_file)
}