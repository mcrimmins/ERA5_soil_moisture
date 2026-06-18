# R/02_build_climatology.R

source("R/00_config.R")

library(terra)
library(fs)

# ------------------------------------------------------------
# Build ERA5-Land monthly climatology stacks and quantile rasters.
# ------------------------------------------------------------

dir_clim_stack <- file.path(dir_clim, "monthly_stacks")
dir_clim_quant <- file.path(dir_clim, "monthly_quantiles")

dir_create(dir_clim_stack, recurse = TRUE)
dir_create(dir_clim_quant, recurse = TRUE)

get_raw_monthly_file <- function(year, layer_shortname) {
  file.path(
    dir_raw,
    paste0(year, "_", layer_shortname, "_ERA5Land_monthly.tif")
  )
}

read_month_from_year <- function(year, month, layer_shortname) {
  file <- get_raw_monthly_file(year, layer_shortname)
  
  if (!file.exists(file)) {
    stop("Missing raw monthly file: ", file)
  }
  
  r <- terra::rast(file)
  
  if (terra::nlyr(r) < month) {
    stop("File has fewer layers than expected: ", file)
  }
  
  x <- r[[month]]
  names(x) <- paste0(layer_shortname, "_", year, "_", sprintf("%02d", month))
  x
}

build_month_stack <- function(month,
                              layer_shortname = "swvl1",
                              overwrite = FALSE) {
  month_chr <- sprintf("%02d", month)
  clim_label <- get_clim_label()
  
  out_file <- file.path(
    dir_clim_stack,
    paste0(layer_shortname, "_clim_stack_", month_chr, "_", clim_label, ".tif")
  )
  
  if (file.exists(out_file) && !overwrite) {
    message("Already exists: ", out_file)
    return(out_file)
  }
  
  message("Building climatology stack: ", layer_shortname, " month ", month_chr)
  
  r_list <- lapply(
    clim_years,
    read_month_from_year,
    month = month,
    layer_shortname = layer_shortname
  )
  
  r_stack <- terra::rast(r_list)
  names(r_stack) <- paste0(layer_shortname, "_", clim_years, "_", month_chr)
  
  terra::writeRaster(
    r_stack,
    filename = out_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  out_file
}

build_month_quantiles <- function(month,
                                  layer_shortname = "swvl1",
                                  overwrite = FALSE) {
  month_chr <- sprintf("%02d", month)
  clim_label <- get_clim_label()
  
  stack_file <- file.path(
    dir_clim_stack,
    paste0(layer_shortname, "_clim_stack_", month_chr, "_", clim_label, ".tif")
  )
  
  if (!file.exists(stack_file)) {
    stack_file <- build_month_stack(
      month = month,
      layer_shortname = layer_shortname,
      overwrite = FALSE
    )
  }
  
  out_file <- file.path(
    dir_clim_quant,
    paste0(layer_shortname, "_clim_quantiles_", month_chr, "_", clim_label, ".tif")
  )
  
  if (file.exists(out_file) && !overwrite) {
    message("Already exists: ", out_file)
    return(out_file)
  }
  
  message("Building climatology quantiles: ", layer_shortname, " month ", month_chr)
  
  r_stack <- terra::rast(stack_file)
  
  q_raster <- terra::app(r_stack, function(x) {
    if (all(is.na(x))) {
      return(rep(NA_real_, 7))
    }
    as.numeric(
      quantile(
        x,
        probs = c(0.02, 0.05, 0.10, 0.20, 0.50, 0.80, 0.90),
        na.rm = TRUE,
        type = 8
      )
    )
  })
  
  names(q_raster) <- c("q02", "q05", "q10", "q20", "q50", "q80", "q90")
  
  terra::writeRaster(
    q_raster,
    filename = out_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  out_file
}

# ------------------------------------------------------------
# Build all layers and months
# ------------------------------------------------------------

for (layer in target_layers) {
  message("============================================================")
  message("Building climatology for layer: ", layer)
  message("Depth: ", get_soil_layer_depth(layer))
  message("============================================================")
  
  for (mo in clim_months) {
    build_month_stack(
      month = mo,
      layer_shortname = layer,
      overwrite = FALSE
    )
    
    build_month_quantiles(
      month = mo,
      layer_shortname = layer,
      overwrite = FALSE
    )
  }
}

message("Finished building climatology stacks and quantile rasters.")
