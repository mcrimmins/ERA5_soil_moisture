# R/01_download_era5land_monthly_climatology.R

source("R/00_config.R")

library(ecmwfr)
library(terra)
library(archive)
library(tictoc)
library(fs)

# ------------------------------------------------------------
# Download ERA5-Land monthly means for climatology.
#
# Output examples:
#   data/raw/era5land_monthly/1981_swvl1_ERA5Land_monthly.tif
#   data/raw/era5land_monthly/1981_swvl2_ERA5Land_monthly.tif
# ------------------------------------------------------------

download_era5land_monthly_year <- function(year,
                                           layer_shortname = "swvl1",
                                           overwrite = FALSE) {
  layer_variable <- get_soil_layer_variable(layer_shortname)
  
  out_file <- file.path(
    dir_raw,
    paste0(year, "_", layer_shortname, "_ERA5Land_monthly.tif")
  )
  
  if (file.exists(out_file) && !overwrite) {
    message("Already exists: ", out_file)
    return(out_file)
  }
  
  zip_target <- paste0(
    "era5land_",
    layer_shortname,
    "_monthly_",
    year,
    ".zip"
  )
  
  request <- list(
    dataset_short_name = era5land_monthly_dataset,
    product_type = list("monthly_averaged_reanalysis"),
    variable = list(layer_variable),
    year = list(as.character(year)),
    month = as.list(sprintf("%02d", 1:12)),
    time = list("00:00"),
    data_format = "netcdf",
    download_format = "zip",
    area = aoi_cds,
    target = zip_target
  )
  
  fs::dir_create(dir_temp, recurse = TRUE)
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
  
  message("Downloading ERA5-Land ", layer_shortname, " for ", year)
  
  tictoc::tic()
  zip_file <- safe_wf_request(
    request = request,
    path = dir_temp,
    max_attempts = 5,
    base_wait = 180,
    jitter = 60
  )
  tictoc::toc()
  
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
  
  r <- terra::rast(nc_files[1])
  
  if (terra::nlyr(r) > 12) {
    message("More than 12 layers found. Keeping first 12.")
    r <- r[[1:12]]
  }
  
  if (terra::nlyr(r) != 12) {
    stop(
      "Expected 12 monthly layers but found ",
      terra::nlyr(r),
      " for ",
      year,
      " ",
      layer_shortname
    )
  }
  
  names(r) <- paste0(
    layer_shortname,
    "_",
    year,
    "_",
    sprintf("%02d", 1:12)
  )
  
  message("Writing: ", out_file)
  terra::writeRaster(
    r,
    filename = out_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
  
  out_file
}

# ------------------------------------------------------------
# Run downloads
# ------------------------------------------------------------

for (layer in target_layers) {
  for (yr in clim_years) {
    download_era5land_monthly_year(
      year = yr,
      layer_shortname = layer,
      overwrite = FALSE
    )
  }
}
