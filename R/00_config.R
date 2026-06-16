# R/00_config.R

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

dir_logs <- file.path(project_dir, "logs")
dir_output <- file.path(project_dir, "output")
dir_qa <- file.path(dir_output, "qa_maps")

dir_create(dir_raw, recurse = TRUE)
dir_create(dir_temp, recurse = TRUE)
dir_create(dir_clim, recurse = TRUE)
dir_create(dir_current_raw, recurse = TRUE)
dir_create(dir_current_monthly, recurse = TRUE)
dir_create(dir_percentiles, recurse = TRUE)
dir_create(dir_logs, recurse = TRUE)
dir_create(dir_qa, recurse = TRUE)

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

# Layers to process operationally
target_layers <- c("swvl1", "swvl2")

# CDS area order: North, West, South, East
aoi_cds <- c(49.8, -126.9, 23.7, -65.6)

# ------------------------------------------------------------
# Climatology settings
# ------------------------------------------------------------

clim_years <- 1981:2020
clim_months <- 1:12


# -------------------------
# safe download wrapper

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


