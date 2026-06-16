# R/03_download_current_month_era5land_hourly.R

source("R/00_config.R")

library(ecmwfr)
library(terra)
library(archive)
library(lubridate)
library(fs)
library(tictoc)

# ------------------------------------------------------------
# Download hourly ERA5-Land soil moisture for target month
# and aggregate to monthly mean.
#
# Output examples:
#   data/processed/current_monthly/swvl1_monthly_mean_2025_05_ERA5Land_hourly_derived.tif
#   data/processed/current_monthly/swvl2_monthly_mean_2025_05_ERA5Land_hourly_derived.tif
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

download_current_month_swvl <- function(year = NULL,
                                        month = NULL,
                                        layer_shortname = "swvl1",
                                        overwrite = FALSE,
                                        keep_hourly_tif = FALSE,
                                        delete_download_zip = FALSE) {
  
  if (is.null(year) || is.null(month)) {
    target <- get_previous_month()
    year <- target$year
    month <- target$month
  }
  
  if (!layer_shortname %in% soil_layers$shortname) {
    stop(
      "Unknown layer_shortname: ",
      layer_shortname,
      ". Valid options are: ",
      paste(soil_layers$shortname, collapse = ", ")
    )
  }
  
  layer_variable <- get_soil_layer_variable(layer_shortname)
  month_chr <- sprintf("%02d", month)
  
  message("Target month: ", year, "-", month_chr)
  message("Layer: ", layer_shortname, " / ", get_soil_layer_depth(layer_shortname))
  
  out_monthly_file <- file.path(
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
  
  if (file.exists(out_monthly_file) && !overwrite) {
    message("Already exists: ", out_monthly_file)
    return(out_monthly_file)
  }
  
  month_dates <- get_days_in_month(year, month)
  day_vec <- sprintf("%02d", day(month_dates))
  time_vec <- sprintf("%02d:00", 0:23)
  
  zip_target <- paste0(
    "era5land_",
    layer_shortname,
    "_hourly_",
    year,
    "_",
    month_chr,
    ".zip"
  )
  
  request <- list(
    dataset_short_name = era5land_hourly_dataset,
    variable = list(layer_variable),
    year = list(as.character(year)),
    month = list(month_chr),
    day = as.list(day_vec),
    time = as.list(time_vec),
    data_format = "netcdf",
    download_format = "zip",
    area = aoi_cds,
    target = zip_target
  )
  
  fs::dir_create(dir_current_raw)
  fs::dir_create(dir_temp)
  
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
  
  message("Downloading hourly ERA5-Land ", layer_shortname)
  
  tictoc::tic()
  zip_file <- safe_wf_request(
    request = request,
    path = dir_current_raw,
    max_attempts = 5,
    base_wait = 180,
    jitter = 60
  )
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
  
  print(r_hourly)
  print(names(r_hourly))
  
  expected_layers <- length(month_dates) * length(time_vec)
  
  if (terra::nlyr(r_hourly) != expected_layers) {
    warning(
      "Expected ",
      expected_layers,
      " hourly layers but found ",
      terra::nlyr(r_hourly),
      ". Inspect layer order before using operationally."
    )
  }
  
  layer_times <- as.POSIXct(
    paste(
      rep(month_dates, each = 24),
      rep(time_vec, times = length(month_dates))
    ),
    tz = "UTC"
  )
  
  if (length(layer_times) == terra::nlyr(r_hourly)) {
    names(r_hourly) <- paste0(
      layer_shortname,
      "_",
      format(layer_times, "%Y%m%d_%H")
    )
  }
  
  # Aggregate hourly to daily means first so each day receives equal weight.
  message("Aggregating hourly data to daily means")
  
  if (length(layer_times) == terra::nlyr(r_hourly)) {
    
    daily_index <- as.Date(layer_times)
    
    r_daily <- terra::tapp(
      r_hourly,
      index = daily_index,
      fun = mean,
      na.rm = TRUE
    )
    
    names(r_daily) <- paste0(
      layer_shortname,
      "_daily_mean_",
      format(unique(daily_index), "%Y%m%d")
    )
    
  } else {
    
    message("Layer count mismatch. Aggregating all layers directly to monthly mean.")
    r_daily <- NULL
  }
  
  message("Aggregating to monthly mean")
  
  if (!is.null(r_daily)) {
    r_monthly <- terra::app(r_daily, mean, na.rm = TRUE)
  } else {
    r_monthly <- terra::app(r_hourly, mean, na.rm = TRUE)
  }
  
  names(r_monthly) <- paste0(
    layer_shortname,
    "_monthly_mean_",
    year,
    "_",
    month_chr
  )
  
  message("Writing monthly mean raster: ", out_monthly_file)
  
  terra::writeRaster(
    r_monthly,
    filename = out_monthly_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  if (keep_hourly_tif) {
    
    hourly_tif <- file.path(
      dir_current_raw,
      paste0(
        layer_shortname,
        "_hourly_",
        year,
        "_",
        month_chr,
        "_ERA5Land.tif"
      )
    )
    
    terra::writeRaster(
      r_hourly,
      filename = hourly_tif,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW")
    )
    
    if (!is.null(r_daily)) {
      
      daily_tif <- file.path(
        dir_current_raw,
        paste0(
          layer_shortname,
          "_daily_mean_",
          year,
          "_",
          month_chr,
          "_ERA5Land.tif"
        )
      )
      
      terra::writeRaster(
        r_daily,
        filename = daily_tif,
        overwrite = TRUE,
        gdal = c("COMPRESS=LZW")
      )
    }
  }
  
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
  
  if (delete_download_zip && file.exists(zip_file)) {
    message("Deleting downloaded zip: ", zip_file)
    unlink(zip_file, force = TRUE)
  }
  
  message("Finished current-month download and aggregation for ", layer_shortname)
  
  out_monthly_file
}