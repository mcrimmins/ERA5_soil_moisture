# R/04_compute_monthly_percentile.R

source("R/00_config.R")

library(terra)
library(fs)
library(lubridate)

# ------------------------------------------------------------
# Compute monthly percentile from current monthly mean raster
# and same-calendar-month climatology stack.
# ------------------------------------------------------------

dir_clim_stack <- file.path(dir_clim, "monthly_stacks")

dir_create(dir_percentiles, recurse = TRUE)
dir_create(dir_qa, recurse = TRUE)

get_previous_month <- function(today = Sys.Date()) {
  
  first_day_this_month <- floor_date(today, unit = "month")
  target_date <- first_day_this_month %m-% months(1)
  
  list(
    year = year(target_date),
    month = month(target_date)
  )
}

get_current_monthly_file <- function(year,
                                     month,
                                     layer_shortname = "swvl1") {
  
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

get_climatology_stack_file <- function(month,
                                       layer_shortname = "swvl1") {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_clim_stack,
    paste0(
      layer_shortname,
      "_clim_stack_",
      month_chr,
      "_",
      min(clim_years),
      "_",
      max(clim_years),
      ".tif"
    )
  )
}

calc_percentile_from_stack <- function(current_raster, clim_stack) {
  
  x <- c(current_raster, clim_stack)
  
  pct <- app(x, function(v) {
    
    current_val <- v[1]
    hist_vals <- v[-1]
    
    if (is.na(current_val) || all(is.na(hist_vals))) {
      return(NA_real_)
    }
    
    100 * mean(hist_vals <= current_val, na.rm = TRUE)
  })
  
  names(pct) <- "soil_moisture_percentile"
  
  pct
}

classify_percentile <- function(pct_raster) {
  
  rcl <- matrix(
    c(
      -Inf,  2, 1,
      2,   5, 2,
      5,  10, 3,
      10,  20, 4,
      20,  40, 5,
      40,  60, 6,
      60, Inf, 7
    ),
    ncol = 3,
    byrow = TRUE
  )
  
  pct_class <- classify(
    pct_raster,
    rcl = rcl,
    include.lowest = TRUE
  )
  
  names(pct_class) <- "soil_moisture_percentile_class"
  
  pct_class
}

compute_monthly_percentile <- function(year = NULL,
                                       month = NULL,
                                       layer_shortname = "swvl1",
                                       current_file = NULL,
                                       overwrite = FALSE,
                                       make_qa_plot = TRUE) {
  
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
  
  month_chr <- sprintf("%02d", month)
  
  if (is.null(current_file)) {
    current_file <- get_current_monthly_file(
      year = year,
      month = month,
      layer_shortname = layer_shortname
    )
  }
  
  clim_file <- get_climatology_stack_file(
    month = month,
    layer_shortname = layer_shortname
  )
  
  if (!file.exists(current_file)) {
    stop(
      "Missing current monthly file: ",
      current_file,
      "\nRun R/03_download_current_month_era5land_hourly.R first."
    )
  }
  
  if (!file.exists(clim_file)) {
    stop(
      "Missing climatology stack: ",
      clim_file,
      "\nRun R/02_build_climatology.R first."
    )
  }
  
  out_pct_file <- file.path(
    dir_percentiles,
    paste0(
      layer_shortname,
      "_percentile_",
      year,
      "_",
      month_chr,
      "_vs_",
      min(clim_years),
      "_",
      max(clim_years),
      ".tif"
    )
  )
  
  out_class_file <- file.path(
    dir_percentiles,
    paste0(
      layer_shortname,
      "_percentile_class_",
      year,
      "_",
      month_chr,
      "_vs_",
      min(clim_years),
      "_",
      max(clim_years),
      ".tif"
    )
  )
  
  if (
    file.exists(out_pct_file) &&
    file.exists(out_class_file) &&
    !overwrite
  ) {
    message("Already exists: ", out_pct_file)
    message("Already exists: ", out_class_file)
    return(
      list(
        percentile = out_pct_file,
        class = out_class_file
      )
    )
  }
  
  message("Reading current monthly mean: ", current_file)
  current <- rast(current_file)
  
  if (nlyr(current) != 1) {
    stop(
      "Expected current_file to have one monthly mean layer, but found ",
      nlyr(current)
    )
  }
  
  names(current) <- paste0(
    layer_shortname,
    "_monthly_mean_",
    year,
    "_",
    month_chr
  )
  
  message("Reading climatology stack: ", clim_file)
  clim_stack <- rast(clim_file)
  
  if (!compareGeom(current, clim_stack, stopOnError = FALSE)) {
    stop(
      "Current raster and climatology stack do not have matching geometry. ",
      "Check AOI, resolution, CRS, and download settings."
    )
  }
  
  message("Computing percentile raster for ", layer_shortname, " ", year, "-", month_chr)
  
  pct <- calc_percentile_from_stack(current, clim_stack)
  
  pct_summary <- global(pct, c("min", "mean", "max"), na.rm = TRUE)
  print(pct_summary)
  
  message("Writing percentile raster: ", out_pct_file)
  
  writeRaster(
    pct,
    filename = out_pct_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  message("Computing percentile class raster")
  pct_class <- classify_percentile(pct)
  
  message("Writing percentile class raster: ", out_class_file)
  
  writeRaster(
    pct_class,
    filename = out_class_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  if (make_qa_plot) {
    
    png_file <- file.path(
      dir_qa,
      paste0(
        "qa_",
        layer_shortname,
        "_percentile_",
        year,
        "_",
        month_chr,
        ".png"
      )
    )
    
    png(png_file, width = 1400, height = 800)
    
    par(mfrow = c(1, 2))
    
    plot(
      pct,
      range = c(0, 100),
      main = paste0(
        "ERA5-Land ",
        layer_shortname,
        " percentile: ",
        year,
        "-",
        month_chr,
        "\nDepth: ",
        get_soil_layer_depth(layer_shortname)
      )
    )
    
    plot(
      pct_class,
      main = paste0(
        layer_shortname,
        " percentile class: ",
        year,
        "-",
        month_chr
      )
    )
    
    dev.off()
    
    message("Wrote QA plot: ", png_file)
  }
  
  list(
    percentile = out_pct_file,
    class = out_class_file
  )
}

compute_previous_month_percentile <- function(layer_shortname = "swvl1",
                                              overwrite = FALSE,
                                              make_qa_plot = TRUE) {
  
  target <- get_previous_month()
  
  compute_monthly_percentile(
    year = target$year,
    month = target$month,
    layer_shortname = layer_shortname,
    overwrite = overwrite,
    make_qa_plot = make_qa_plot
  )
}