# R/07_plot_percentile_maps.R

source("R/00_config.R")

library(terra)
library(sf)
library(fs)

dir_maps <- file.path(project_dir, "output", "maps")
fs::dir_create(dir_maps, recurse = TRUE)

# ------------------------------------------------------------
# Purpose:
#   Plot swvl1 and swvl2 soil moisture percentile maps.
#   Optionally apply a hard snow/frozen-ground display mask.
#
# Percentile convention:
#   low percentile  = dry
#   near 50         = near normal
#   high percentile = wet
#
# Mask convention:
#   0 = no mask
#   1 = snow
#   2 = frozen ground
#   3 = snow + frozen ground
# ------------------------------------------------------------

# ------------------------------------------------------------
# User options
# ------------------------------------------------------------

target_year <- NULL
target_month <- NULL

# Manual test example:
# target_year <- 2025
# target_month <- 5

layers_to_plot <- c("swvl1", "swvl2")

use_snow_freeze_mask <- TRUE
mask_classes_to_apply <- c(1, 2, 3)
mask_color <- "gray80"
show_mask_as_gray <- TRUE

boundary_file <- NULL
# Example:
# boundary_file <- file.path(project_dir, "data", "vector", "small_areas.gpkg")

make_png <- TRUE
make_pdf <- TRUE

make_temperature_maps <- TRUE

# ------------------------------------------------------------
# Resolve target month
# ------------------------------------------------------------

if (is.null(target_year) || is.null(target_month)) {
  target <- get_previous_month()
  target_year <- target$year
  target_month <- target$month
}

# ------------------------------------------------------------
# Helper functions
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
      get_clim_label(),
      ".tif"
    )
  )
}

get_snow_freeze_mask_file <- function(year, month) {
  month_chr <- sprintf("%02d", month)
  file.path(dir_mask_monthly, paste0("snow_freeze_mask_", year, "_", month_chr, ".tif"))
}

get_layer_plot_label <- function(layer_shortname) {
  paste0(layer_shortname, " percentile\n", get_soil_layer_depth(layer_shortname))
}

pct_breaks <- c(0, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98, 100)

pct_cols <- colorRampPalette(
  c(
    "#7f3b08",
    "#b35806",
    "#e08214",
    "#fdb863",
    "#fee0b6",
    "#f7f7f7",
    "#d8daeb",
    "#b2abd2",
    "#80cdc1",
    "#35978f",
    "#01665e"
  )
)(length(pct_breaks) - 1)

prepare_display_mask <- function(mask_file,
                                 reference_raster,
                                 mask_classes = c(1, 2, 3)) {
  if (!file.exists(mask_file)) {
    stop(
      "Snow/frozen-ground mask file does not exist:\n",
      mask_file,
      "\nRun R/06_download_snow_freeze_monthly.R first."
    )
  }
  
  mask <- terra::rast(mask_file)
  
  if (!terra::compareGeom(mask, reference_raster, stopOnError = FALSE)) {
    message("Mask geometry does not match percentile raster. Resampling mask to match.")
    mask <- terra::resample(mask, reference_raster, method = "near")
  }
  
  hard_mask <- mask %in% mask_classes
  names(hard_mask) <- "hard_snow_freeze_mask"
  hard_mask
}

apply_hard_mask <- function(pct_raster, hard_mask) {
  pct_masked <- pct_raster
  pct_masked[hard_mask == 1] <- NA
  pct_masked
}

add_percentile_legend <- function() {
  legend_labels <- paste0(head(pct_breaks, -1), "-", tail(pct_breaks, -1))
  
  legend(
    "right",
    legend = legend_labels,
    fill = pct_cols,
    title = "Percentile",
    cex = 0.65,
    bty = "n",
    inset = -0.18,
    xpd = TRUE
  )
}

plot_soil_moisture_percentile_maps <- function(year,
                                               month,
                                               layers = c("swvl1", "swvl2"),
                                               use_snow_freeze_mask = TRUE,
                                               mask_classes_to_apply = c(1, 2, 3),
                                               mask_color = "gray80",
                                               show_mask_as_gray = TRUE,
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
  
  pct_rasters <- lapply(pct_files, terra::rast)
  names(pct_rasters) <- layers
  
  hard_mask <- NULL
  mask_file <- NULL
  
  if (use_snow_freeze_mask) {
    mask_file <- get_snow_freeze_mask_file(year, month)
    hard_mask <- prepare_display_mask(
      mask_file = mask_file,
      reference_raster = pct_rasters[[1]],
      mask_classes = mask_classes_to_apply
    )
  }
  
  boundaries <- NULL
  
  if (!is.null(boundary_file)) {
    if (!file.exists(boundary_file)) {
      stop("Boundary file does not exist: ", boundary_file)
    }
    boundaries <- sf::st_read(boundary_file, quiet = TRUE)
    boundaries <- sf::st_transform(boundaries, terra::crs(pct_rasters[[1]]))
    boundaries <- terra::vect(boundaries)
  }
  
  mask_label <- ifelse(use_snow_freeze_mask, "_snow_freeze_masked", "")
  
  png_file <- file.path(
    dir_maps,
    paste0(
      "soil_moisture_percentiles_",
      year,
      "_",
      month_chr,
      "_",
      paste(layers, collapse = "_"),
      mask_label,
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
      mask_label,
      ".pdf"
    )
  )
  
  title_main <- paste0(
    "ERA5-Land soil moisture percentiles, ",
    year,
    "-",
    month_chr,
    "\nPercentile rank relative to ",
    get_clim_label(),
    " same-month climatology"
  )
  
  if (use_snow_freeze_mask) {
    title_main <- paste0(title_main, "\nGray areas masked for snow and/or frozen surface soil")
  }
  
  draw_maps <- function() {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    par(
      mfrow = c(1, length(layers)),
      mar = c(3, 3, 4, 7),
      oma = c(4, 1, 6, 1)
    )
    
    for (layer in layers) {
      pct <- pct_rasters[[layer]]
      
      if (use_snow_freeze_mask) {
        pct_to_plot <- apply_hard_mask(pct, hard_mask)
      } else {
        pct_to_plot <- pct
      }
      
      plot(
        pct_to_plot,
        breaks = pct_breaks,
        col = pct_cols,
        range = c(0, 100),
        main = get_layer_plot_label(layer),
        axes = TRUE,
        box = TRUE,
        legend = FALSE
      )
      
      if (use_snow_freeze_mask && show_mask_as_gray) {
        mask_overlay <- terra::ifel(hard_mask, 1, NA)
        plot(
          mask_overlay,
          add = TRUE,
          col = mask_color,
          legend = FALSE
        )
      }
      
      if (!is.null(boundaries)) {
        lines(boundaries, col = "black", lwd = 0.5)
      }
      
      add_percentile_legend()
      
      if (use_snow_freeze_mask && show_mask_as_gray) {
        legend(
          "bottomright",
          legend = "Snow/frozen mask",
          fill = mask_color,
          bty = "n",
          cex = 0.75
        )
      }
    }
    
    mtext(
      title_main,
      outer = TRUE,
      side = 3,
      line = 1.5,
      cex = 1.05,
      font = 2
    )
    
    mtext(
      "Lower percentiles indicate drier-than-normal soil moisture; higher percentiles indicate wetter-than-normal soil moisture.",
      outer = TRUE,
      side = 1,
      line = 1.5,
      cex = 0.85
    )
  }
  
  if (make_png) {
    png(png_file, width = 1900, height = 900, res = 150)
    draw_maps()
    dev.off()
    message("Wrote PNG map: ", png_file)
  }
  
  if (make_pdf) {
    pdf(pdf_file, width = 13, height = 7)
    draw_maps()
    dev.off()
    message("Wrote PDF map: ", pdf_file)
  }
  
  invisible(
    list(
      png = if (make_png) png_file else NULL,
      pdf = if (make_pdf) pdf_file else NULL,
      percentile_rasters = pct_files,
      mask_file = mask_file,
      mask_classes_applied = if (use_snow_freeze_mask) mask_classes_to_apply else NULL
    )
  )
}

# ------------------------------------------------------------
# Soil temperature plotting helpers
# ------------------------------------------------------------

get_soil_temperature_c_file <- function(year, month) {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_temperature,
    paste0(
      "stl1_monthly_mean_c_",
      year,
      "_",
      month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
}

get_soil_temperature_k_file <- function(year, month) {
  
  month_chr <- sprintf("%02d", month)
  
  file.path(
    dir_temperature,
    paste0(
      "stl1_monthly_mean_k_",
      year,
      "_",
      month_chr,
      "_ERA5Land_hourly_derived.tif"
    )
  )
}

get_soil_temperature_freeze_fraction_file <- function(year, month) {
  
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

plot_soil_temperature_maps <- function(year,
                                       month,
                                       make_png = TRUE,
                                       make_pdf = TRUE,
                                       boundary_file = NULL) {
  
  month_chr <- sprintf("%02d", month)
  
  temp_file <- get_soil_temperature_c_file(year, month)
  freeze_file <- get_soil_temperature_freeze_fraction_file(year, month)
  
  if (!file.exists(temp_file)) {
    stop("Missing soil temperature Celsius file: ", temp_file)
  }
  
  if (!file.exists(freeze_file)) {
    stop("Missing freeze fraction file: ", freeze_file)
  }
  
  temp_c <- terra::rast(temp_file)
  freeze_fraction <- terra::rast(freeze_file)
  
  boundaries <- NULL
  
  if (!is.null(boundary_file)) {
    
    if (!file.exists(boundary_file)) {
      stop("Boundary file does not exist: ", boundary_file)
    }
    
    boundaries <- sf::st_read(boundary_file, quiet = TRUE)
    boundaries <- sf::st_transform(boundaries, terra::crs(temp_c))
    boundaries <- terra::vect(boundaries)
  }
  
  temp_breaks <- c(-30, -20, -15, -10, -5, 0, 2, 5, 10, 15, 20, 25, 30, 35, 40, 45)
  
  temp_cols <- colorRampPalette(
    c(
      "#313695",
      "#4575b4",
      "#74add1",
      "#abd9e9",
      "#e0f3f8",
      "#ffffbf",
      "#fee090",
      "#fdae61",
      "#f46d43",
      "#d73027",
      "#a50026"
    )
  )(length(temp_breaks) - 1)
  
  freeze_breaks <- c(0, 0.01, 0.10, 0.25, 0.50, 0.75, 1.00)
  
  freeze_cols <- colorRampPalette(
    c(
      "#f7fbff",
      "#deebf7",
      "#c6dbef",
      "#9ecae1",
      "#6baed6",
      "#2171b5",
      "#08306b"
    )
  )(length(freeze_breaks) - 1)
  
  png_file <- file.path(
    dir_maps,
    paste0(
      "soil_temperature_",
      year,
      "_",
      month_chr,
      ".png"
    )
  )
  
  pdf_file <- file.path(
    dir_maps,
    paste0(
      "soil_temperature_",
      year,
      "_",
      month_chr,
      ".pdf"
    )
  )
  
  draw_temp_maps <- function() {
    
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    par(
      mfrow = c(1, 2),
      mar = c(3, 3, 4, 6),
      oma = c(3, 1, 5, 1)
    )
    
    plot(
      temp_c,
      breaks = temp_breaks,
      col = temp_cols,
      main = "Top-layer soil temperature\nmonthly mean, °C",
      axes = TRUE,
      box = TRUE
    )
    
    if (!is.null(boundaries)) {
      lines(boundaries, col = "black", lwd = 0.5)
    }
    
    plot(
      freeze_fraction,
      breaks = freeze_breaks,
      col = freeze_cols,
      range = c(0, 1),
      main = "Frozen surface soil frequency\nfraction of hourly timesteps",
      axes = TRUE,
      box = TRUE
    )
    
    if (!is.null(boundaries)) {
      lines(boundaries, col = "black", lwd = 0.5)
    }
    
    mtext(
      paste0(
        "ERA5-Land top-layer soil temperature diagnostics, ",
        year,
        "-",
        month_chr
      ),
      outer = TRUE,
      side = 3,
      line = 1.5,
      cex = 1.05,
      font = 2
    )
  }
  
  if (make_png) {
    png(png_file, width = 1800, height = 850, res = 150)
    draw_temp_maps()
    dev.off()
    message("Wrote PNG soil temperature map: ", png_file)
  }
  
  if (make_pdf) {
    pdf(pdf_file, width = 12, height = 6.5)
    draw_temp_maps()
    dev.off()
    message("Wrote PDF soil temperature map: ", pdf_file)
  }
  
  invisible(
    list(
      png = if (make_png) png_file else NULL,
      pdf = if (make_pdf) pdf_file else NULL,
      temperature_c = temp_file,
      freeze_fraction = freeze_file
    )
  )
}

# ------------------------------------------------------------
# Run soil moisture percentile maps
# ------------------------------------------------------------

map_files <- plot_soil_moisture_percentile_maps(
  year = target_year,
  month = target_month,
  layers = layers_to_plot,
  use_snow_freeze_mask = use_snow_freeze_mask,
  mask_classes_to_apply = mask_classes_to_apply,
  mask_color = mask_color,
  show_mask_as_gray = show_mask_as_gray,
  boundary_file = boundary_file,
  make_png = make_png,
  make_pdf = make_pdf
)

print(map_files)

# ------------------------------------------------------------
# Optional soil temperature maps
# ------------------------------------------------------------

if (make_temperature_maps) {
  
  temp_map_files <- plot_soil_temperature_maps(
    year = target_year,
    month = target_month,
    make_png = make_png,
    make_pdf = make_pdf,
    boundary_file = boundary_file
  )
  
  print(temp_map_files)
}