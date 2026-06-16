# R/06_plot_percentile_maps.R

source("R/00_config.R")

library(terra)
library(sf)
library(fs)

# ------------------------------------------------------------
# Purpose:
#   Plot operational soil moisture percentile maps for swvl1 and swvl2
#   using a diverging dry-to-wet color ramp centered on 50.
#
# Input examples:
#   data/processed/percentiles/swvl1_percentile_2025_05_vs_1991_2020.tif
#   data/processed/percentiles/swvl2_percentile_2025_05_vs_1991_2020.tif
#
# Output examples:
#   output/maps/soil_moisture_percentiles_2025_05_swvl1_swvl2.png
#   output/maps/soil_moisture_percentiles_2025_05_swvl1_swvl2.pdf
# ------------------------------------------------------------

dir_maps <- file.path(project_dir, "output", "maps")
dir_create(dir_maps, recurse = TRUE)

# ------------------------------------------------------------
# User options
# ------------------------------------------------------------

target_year <- 2025
target_month <- 5

layers_to_plot <- c("swvl1", "swvl2")

# Optional boundary overlays.
# Set to NULL if you do not want boundaries.
boundary_file <- NULL
# Example:
# boundary_file <- file.path(project_dir, "data", "vector", "small_areas.gpkg")

make_png <- TRUE
make_pdf <- TRUE

# ------------------------------------------------------------
# Helper: percentile raster path
# ------------------------------------------------------------

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
      min(clim_years),
      "_",
      max(clim_years),
      ".tif"
    )
  )
}

# ------------------------------------------------------------
# Helper: map labels
# ------------------------------------------------------------

get_layer_plot_label <- function(layer_shortname) {
  
  paste0(
    layer_shortname,
    " percentile\n",
    get_soil_layer_depth(layer_shortname)
  )
}

# ------------------------------------------------------------
# Color ramp
# ------------------------------------------------------------

# Breaks are intentionally symmetric around 50.
# These bins emphasize dry tails while still showing wet anomalies.
pct_breaks <- c(0, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98, 100)

# Dry-to-wet ramp:
# dark brown/orange = driest
# pale neutral = near median
# blue/teal = wettest
pct_cols <- colorRampPalette(
  c(
    "#7f3b08",  # very dry brown
    "#b35806",
    "#e08214",
    "#fdb863",
    "#fee0b6",
    "#f7f7f7",  # near normal
    "#d8daeb",
    "#b2abd2",
    "#80cdc1",
    "#35978f",
    "#01665e"   # very wet teal
  )
)(length(pct_breaks) - 1)

# ------------------------------------------------------------
# Plot function
# ------------------------------------------------------------

plot_soil_moisture_percentile_maps <- function(year,
                                               month,
                                               layers = c("swvl1", "swvl2"),
                                               boundary_file = NULL,
                                               make_png = TRUE,
                                               make_pdf = TRUE) {
  
  month_chr <- sprintf("%02d", month)
  
  pct_files <- vapply(
    layers,
    function(layer) get_percentile_file(year, month, layer),
    character(1)
  )
  
  missing_files <- pct_files[!file.exists(pct_files)]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing percentile raster(s):\n",
      paste(missing_files, collapse = "\n"),
      "\nRun R/05_run_operational_percentile_workflow.R first."
    )
  }
  
  pct_rasters <- lapply(pct_files, rast)
  
  names(pct_rasters) <- layers
  
  # Optional boundaries
  boundaries <- NULL
  
  if (!is.null(boundary_file)) {
    
    if (!file.exists(boundary_file)) {
      stop("Boundary file does not exist: ", boundary_file)
    }
    
    boundaries <- st_read(boundary_file, quiet = TRUE)
    boundaries <- st_transform(boundaries, crs(pct_rasters[[1]]))
    boundaries <- vect(boundaries)
  }
  
  title_main <- paste0(
    "ERA5-Land soil moisture percentiles, ",
    year,
    "-",
    month_chr,
    "\nPercentile rank relative to ",
    min(clim_years),
    "-",
    max(clim_years),
    " same-month climatology"
  )
  
  png_file <- file.path(
    dir_maps,
    paste0(
      "soil_moisture_percentiles_",
      year,
      "_",
      month_chr,
      "_",
      paste(layers, collapse = "_"),
      ".png"
    )
  )
  
  pdf_file <- file.path(
    dir_maps,
    paste0(
      "soil_moisture_percentiles_",
      year,
      "_",
      month_chr,
      "_",
      paste(layers, collapse = "_"),
      ".pdf"
    )
  )
  
  draw_maps <- function() {
    
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    par(
      mfrow = c(1, length(layers)),
      mar = c(3, 3, 4, 5),
      oma = c(3, 1, 5, 1)
    )
    
    for (layer in layers) {
      
      r <- pct_rasters[[layer]]
      
      plot(
        r,
        breaks = pct_breaks,
        col = pct_cols,
        range = c(0, 100),
        main = get_layer_plot_label(layer),
        plg = list(
          title = "Percentile"
        ),
        axes = TRUE,
        box = TRUE
      )
      
      if (!is.null(boundaries)) {
        lines(boundaries, col = "black", lwd = 0.5)
      }
    }
    
    mtext(
      title_main,
      outer = TRUE,
      side = 3,
      line = 1.5,
      cex = 1.1,
      font = 2
    )
    
    mtext(
      "Lower percentiles indicate drier-than-normal soil moisture; higher percentiles indicate wetter-than-normal soil moisture.",
      outer = TRUE,
      side = 1,
      line = 1,
      cex = 0.85
    )
  }
  
  if (make_png) {
    png(png_file, width = 1800, height = 850, res = 150)
    draw_maps()
    dev.off()
    message("Wrote PNG map: ", png_file)
  }
  
  if (make_pdf) {
    pdf(pdf_file, width = 12, height = 6.5)
    draw_maps()
    dev.off()
    message("Wrote PDF map: ", pdf_file)
  }
  
  invisible(
    list(
      png = if (make_png) png_file else NULL,
      pdf = if (make_pdf) pdf_file else NULL,
      rasters = pct_files
    )
  )
}

# ------------------------------------------------------------
# Run
# ------------------------------------------------------------

# for previous month 
#target <- get_previous_month()
target_year <- target$year
target_month <- target$month


map_files <- plot_soil_moisture_percentile_maps(
  year = target_year,
  month = target_month,
  layers = layers_to_plot,
  boundary_file = boundary_file,
  make_png = make_png,
  make_pdf = make_pdf
)

print(map_files)