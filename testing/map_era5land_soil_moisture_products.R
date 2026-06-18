# R/map_era5land_soil_moisture_products.R

# ------------------------------------------------------------
# Map ERA5-Land soil moisture percentile, soil temperature,
# and snow/frozen-ground mask products for a report AOI.
#
# Supports either:
#   source_type = "local"  -> product_base is a local directory
#   source_type = "url"    -> product_base is a URL where files are hosted
#
# Expected product filenames:
#   swvl1_percentile_YYYY_MM_vs_1981_2020.tif
#   swvl2_percentile_YYYY_MM_vs_1981_2020.tif
#   stl1_monthly_mean_c_YYYY_MM_ERA5Land_hourly_derived.tif
#   snow_freeze_mask_YYYY_MM.tif
#
# Optional:
#   apply_mask_to_soil_moisture = TRUE
# ------------------------------------------------------------

mapERA5LandProducts <- function(mo,
                                yr,
                                boundPoly,
                                projDir,
                                mapFeatures = NULL,
                                Mapbox = NULL,
                                product_base,
                                source_type = c("local", "url"),
                                clim_label = "1981_2020",
                                apply_mask_to_soil_moisture = TRUE,
                                show_mask_overlay = TRUE,
                                mask_classes_to_apply = c(1, 2, 3),
                                cache_dir = file.path(projDir, "era5land_cache"),
                                delete_cached_downloads = FALSE) {
  
  source_type <- match.arg(source_type)
  
  # ----------------------------------------------------------
  # Packages
  # ----------------------------------------------------------
  
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("fs", quietly = TRUE)
  
  fs::dir_create(projDir, recurse = TRUE)
  fs::dir_create(cache_dir, recurse = TRUE)
  
  # ----------------------------------------------------------
  # Date and filenames
  # ----------------------------------------------------------
  
  month_chr <- sprintf("%02d", mo)
  ym_label <- paste0(yr, "_", month_chr)
  map_date_label <- paste0(yr, "-", month_chr)
  
  product_files <- list(
    swvl1 = file.path(
      "percentiles",
      paste0("swvl1_percentile_", ym_label, "_vs_", clim_label, ".tif")
    ),
    swvl2 = file.path(
      "percentiles",
      paste0("swvl2_percentile_", ym_label, "_vs_", clim_label, ".tif")
    ),
    soil_temperature_c = file.path(
      "temperature",
      paste0("stl1_monthly_mean_c_", ym_label, "_ERA5Land_hourly_derived.tif")
    ),
    snow_freeze_mask = file.path(
      "snow_freeze_mask",
      paste0("snow_freeze_mask_", ym_label, ".tif")
    )
  )
  
  # ----------------------------------------------------------
  # Helper: resolve local or URL file
  # ----------------------------------------------------------
  
  get_product_file <- function(filename) {
    
    local_target <- file.path(cache_dir, filename)
    
    if (source_type == "local") {
      
      local_file <- file.path(product_base, filename)
      
      if (!file.exists(local_file)) {
        stop("Missing local ERA5-Land product file: ", local_file)
      }
      
      return(local_file)
    }
    
    if (source_type == "url") {
      
      file_url <- paste0(
        sub("/+$", "", product_base),
        "/",
        filename
      )
      
      if (!file.exists(local_target)) {
        
        message("Downloading ERA5-Land product: ", file_url)
        
        ok <- tryCatch(
          {
            utils::download.file(
              url = file_url,
              destfile = local_target,
              mode = "wb",
              quiet = FALSE
            )
            TRUE
          },
          error = function(e) {
            message("Download failed: ", conditionMessage(e))
            FALSE
          }
        )
        
        if (!ok || !file.exists(local_target)) {
          stop("Could not download ERA5-Land product from: ", file_url)
        }
      } else {
        message("Using cached ERA5-Land product: ", local_target)
      }
      
      return(local_target)
    }
  }
  
  product_paths <- lapply(product_files, get_product_file)
  
  # ----------------------------------------------------------
  # Helper: get plotting extent
  # ----------------------------------------------------------
  
  get_plot_bbox <- function(boundPoly, Mapbox = NULL) {
    
    # If this is a ggmap-style object, it often has a "bb" attribute.
    if (!is.null(Mapbox)) {
      
      bb_attr <- attr(Mapbox, "bb")
      
      if (!is.null(bb_attr)) {
        return(
          list(
            xmin = as.numeric(bb_attr$ll.lon),
            xmax = as.numeric(bb_attr$ur.lon),
            ymin = as.numeric(bb_attr$ll.lat),
            ymax = as.numeric(bb_attr$ur.lat)
          )
        )
      }
    }
    
    bound_sf <- sf::st_as_sf(boundPoly)
    bound_sf <- sf::st_transform(bound_sf, 4326)
    bb <- sf::st_bbox(bound_sf)
    
    list(
      xmin = as.numeric(bb["xmin"]),
      xmax = as.numeric(bb["xmax"]),
      ymin = as.numeric(bb["ymin"]),
      ymax = as.numeric(bb["ymax"])
    )
  }
  
  bb <- get_plot_bbox(boundPoly = boundPoly, Mapbox = Mapbox)
  
  # ----------------------------------------------------------
  # Helper: lat/lon labels
  # ----------------------------------------------------------
  
  label_lon_ew <- function(digits = 1) {
    function(x) {
      paste0(
        sprintf(paste0("%.", digits, "f"), abs(x)),
        "\u00B0",
        ifelse(x < 0, "W", "E")
      )
    }
  }
  
  label_lat_ns <- function(digits = 1) {
    function(x) {
      paste0(
        sprintf(paste0("%.", digits, "f"), abs(x)),
        "\u00B0",
        ifelse(x < 0, "S", "N")
      )
    }
  }
  
  x_rng <- bb$xmax - bb$xmin
  y_rng <- bb$ymax - bb$ymin
  
  n_x <- max(2, min(6, round(5 * x_rng / max(y_rng, 1e-9))))
  n_y <- max(2, min(6, round(5 * y_rng / max(x_rng, 1e-9))))
  
  lon_breaks <- pretty(c(bb$xmin, bb$xmax), n = n_x)
  lat_breaks <- pretty(c(bb$ymin, bb$ymax), n = n_y)
  
  lon_breaks <- lon_breaks[lon_breaks >= bb$xmin & lon_breaks <= bb$xmax]
  lat_breaks <- lat_breaks[lat_breaks >= bb$ymin & lat_breaks <= bb$ymax]
  
  # fix longitude
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
  
  
  # ----------------------------------------------------------
  # Read AOI and rasters
  # ----------------------------------------------------------
  
  bound_sf <- sf::st_as_sf(boundPoly)
  bound_sf <- sf::st_transform(bound_sf, 4326)
  
  crop_ext <- terra::ext(bb$xmin, bb$xmax, bb$ymin, bb$ymax)
  
  swvl1 <- terra::rast(product_paths$swvl1)
  swvl2 <- terra::rast(product_paths$swvl2)
  soil_temp_c <- terra::rast(product_paths$soil_temperature_c)
  mask <- terra::rast(product_paths$snow_freeze_mask)
  
  swvl1 <- fix_longitude_0_360(swvl1, "swvl1 percentile")
  swvl2 <- fix_longitude_0_360(swvl2, "swvl2 percentile")
  soil_temp_c <- fix_longitude_0_360(soil_temp_c, "soil temperature")
  mask <- fix_longitude_0_360(mask, "snow/freeze mask")

  
  # Ensure all are in lon/lat for plotting
  # If products are already EPSG:4326, this is skipped.
  if (!grepl("4326|longlat", terra::crs(swvl1), ignore.case = TRUE)) {
    swvl1 <- terra::project(swvl1, "EPSG:4326")
  }
  
  if (!grepl("4326|longlat", terra::crs(swvl2), ignore.case = TRUE)) {
    swvl2 <- terra::project(swvl2, "EPSG:4326")
  }
  
  if (!grepl("4326|longlat", terra::crs(soil_temp_c), ignore.case = TRUE)) {
    soil_temp_c <- terra::project(soil_temp_c, "EPSG:4326")
  }
  
  if (!grepl("4326|longlat", terra::crs(mask), ignore.case = TRUE)) {
    mask <- terra::project(mask, "EPSG:4326", method = "near")
  }
  
  swvl1 <- terra::crop(swvl1, crop_ext)
  swvl2 <- terra::crop(swvl2, crop_ext)
  soil_temp_c <- terra::crop(soil_temp_c, crop_ext)
  mask <- terra::crop(mask, crop_ext)
  
  # Align mask to soil moisture rasters for masking/overlay
  if (!terra::compareGeom(mask, swvl1, stopOnError = FALSE)) {
    mask_swvl1 <- terra::resample(mask, swvl1, method = "near")
  } else {
    mask_swvl1 <- mask
  }
  
  if (!terra::compareGeom(mask, swvl2, stopOnError = FALSE)) {
    mask_swvl2 <- terra::resample(mask, swvl2, method = "near")
  } else {
    mask_swvl2 <- mask
  }
  
  # ----------------------------------------------------------
  # Apply display mask to soil moisture if requested
  # ----------------------------------------------------------
  
  # ----------------------------------------------------------
  # Apply display mask to soil moisture if requested
  # ----------------------------------------------------------
  
  make_hard_mask <- function(mask_raster,
                             mask_classes = c(1, 2, 3)) {
    
    hard_mask <- mask_raster == mask_classes[1]
    
    if (length(mask_classes) > 1) {
      for (i in 2:length(mask_classes)) {
        hard_mask <- hard_mask | (mask_raster == mask_classes[i])
      }
    }
    
    hard_mask <- terra::ifel(hard_mask, 1, NA)
    names(hard_mask) <- "hard_mask"
    
    hard_mask
  }
  
  hard_mask_swvl1 <- make_hard_mask(
    mask_raster = mask_swvl1,
    mask_classes = mask_classes_to_apply
  )
  
  hard_mask_swvl2 <- make_hard_mask(
    mask_raster = mask_swvl2,
    mask_classes = mask_classes_to_apply
  )
  
  swvl1_plot <- swvl1
  swvl2_plot <- swvl2
  
  if (apply_mask_to_soil_moisture) {
    swvl1_plot <- terra::mask(swvl1_plot, hard_mask_swvl1, maskvalues = 1)
    swvl2_plot <- terra::mask(swvl2_plot, hard_mask_swvl2, maskvalues = 1)
  }
  
  # ----------------------------------------------------------
  # Raster-to-data-frame helper
  # ----------------------------------------------------------
  
  raster_to_df <- function(r, value_name) {
    
    out <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
    
    if (ncol(out) < 3) {
      stop("Raster conversion failed for ", value_name)
    }
    
    names(out)[3] <- value_name
    out
  }
  
  swvl1_df <- raster_to_df(swvl1_plot, "value")
  swvl2_df <- raster_to_df(swvl2_plot, "value")
  temp_df <- raster_to_df(soil_temp_c, "value")
  mask_df <- raster_to_df(mask, "value")
  
  overlay_mask_df_swvl1 <- raster_to_df(
    hard_mask_swvl1,
    "mask"
  )
  
  overlay_mask_df_swvl2 <- raster_to_df(
    hard_mask_swvl2,
    "mask"
  )
  
  # ----------------------------------------------------------
  # Map feature alpha adjustment
  # ----------------------------------------------------------
  
  mapFeatures2 <- mapFeatures
  
  if (!is.null(mapFeatures2) && !is.null(mapFeatures2$layers)) {
    
    if (length(mapFeatures2$layers) >= 1) {
      mapFeatures2$layers[[1]]$aes_params$alpha <- 0.15
    }
    
    if (length(mapFeatures2$layers) >= 2) {
      mapFeatures2$layers[[2]]$aes_params$alpha <- 0.25
    }
    
    if (length(mapFeatures2$layers) >= 3) {
      mapFeatures2$layers[[3]]$aes_params$alpha <- 0.10
    }
    
    if (length(mapFeatures2$layers) >= 4) {
      mapFeatures2$layers[[4]]$aes_params$alpha <- 0.70
    }
  }
  
  add_map_features_before <- function(p) {
    
    if (!is.null(mapFeatures2) && !is.null(mapFeatures2$layers)) {
      
      # County/state lines, if present
      if (length(mapFeatures2$layers) >= 3) {
        p <- p + mapFeatures2$layers[[3]]
      }
    }
    
    p
  }
  
  add_map_features_after <- function(p) {
    
    if (!is.null(mapFeatures2) && !is.null(mapFeatures2$layers)) {
      
      # Roads
      if (length(mapFeatures2$layers) >= 1) {
        p <- p + mapFeatures2$layers[[1]]
      }
      
      # Water
      if (length(mapFeatures2$layers) >= 2) {
        p <- p + mapFeatures2$layers[[2]]
      }
      
      # Cities
      if (length(mapFeatures2$layers) >= 4) {
        p <- p + mapFeatures2$layers[[4]]
      }
    }
    
    p
  }
  
  # ----------------------------------------------------------
  # Color ramps
  # ----------------------------------------------------------
  
  pct_cols <- c(
    "orangered4",
    "orangered2",
    "darkorange3",
    "tan1",
    "khaki1",
    "gray90",
    "azure1",
    "lightskyblue",
    "skyblue3",
    "dodgerblue2",
    "dodgerblue4"
  )
  
  pct_breaks <- c(0, 2, 5, 10, 20, 30, 70, 80, 90, 95, 98, 100)
  pct_labs <- c(
    "0-2",
    "2-5",
    "5-10",
    "10-20",
    "20-30",
    "30-70",
    "70-80",
    "80-90",
    "90-95",
    "95-98",
    "98-100"
  )
  
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
  
  mask_cols <- c(
    "0" = "white",
    "1" = "lightblue",
    "2" = "gray70",
    "3" = "mediumpurple"
  )
  
  mask_labs <- c(
    "0" = "No mask",
    "1" = "Snow",
    "2" = "Frozen soil",
    "3" = "Snow + frozen soil"
  )
  
  # ----------------------------------------------------------
  # Shared plot theme
  # ----------------------------------------------------------
  
  base_map_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 10, colour = "grey20"),
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, colour = "grey30", hjust = 0.5),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
  
  add_common_map_layout <- function(p) {
    
    p +
      ggplot2::geom_sf(
        data = bound_sf,
        fill = NA,
        color = "black",
        linewidth = 0.5,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_x_continuous(
        breaks = lon_breaks,
        labels = label_lon_ew(1)
      ) +
      ggplot2::scale_y_continuous(
        breaks = lat_breaks,
        labels = label_lat_ns(1)
      ) +
      ggplot2::coord_sf(
        crs = 4326,
        xlim = c(bb$xmin, bb$xmax),
        ylim = c(bb$ymin, bb$ymax),
        expand = FALSE
      ) +
      ggplot2::labs(x = NULL, y = NULL) +
      base_map_theme
  }
  
  # ----------------------------------------------------------
  # Soil moisture map helper
  # ----------------------------------------------------------
  
  make_soil_moisture_map <- function(rast_df,
                                     overlay_df,
                                     layer_label,
                                     depth_label) {
    
    rast_df$range <- cut(
      rast_df$value,
      breaks = pct_breaks,
      right = FALSE,
      include.lowest = TRUE
    )
    
    p <- ggplot2::ggplot()
    p <- add_map_features_before(p)
    
    p <- p +
      ggplot2::geom_tile(
        data = rast_df,
        ggplot2::aes(x = x, y = y, fill = range),
        alpha = 0.85
      )
    
    if (show_mask_overlay && !all(is.na(overlay_df$mask))) {
      p <- p +
        ggplot2::geom_tile(
          data = overlay_df,
          ggplot2::aes(x = x, y = y),
          fill = "gray80",
          alpha = 0.85,
          na.rm = TRUE
        )
    }
    
    p <- p +
      ggplot2::scale_fill_manual(
        values = pct_cols,
        na.value = "white",
        name = "Percentile",
        labels = pct_labs,
        drop = FALSE
      )
    
    p <- add_common_map_layout(p)
    p <- add_map_features_after(p)
    
    p +
      ggplot2::labs(
        title = paste0("ERA5-Land soil moisture percentile: ", layer_label),
        subtitle = paste0(
          depth_label,
          " | ",
          map_date_label,
          " | Relative to ",
          clim_label,
          " same-month climatology",
          ifelse(
            apply_mask_to_soil_moisture,
            " | Gray = snow/frozen-soil caution mask",
            ""
          )
        )
      )
  }
  
  # ----------------------------------------------------------
  # Soil temperature map
  # ----------------------------------------------------------
  
  make_temperature_map <- function(temp_df) {
    
    temp_df$range <- cut(
      temp_df$value,
      breaks = temp_breaks,
      right = FALSE,
      include.lowest = TRUE
    )
    
    temp_labs <- paste0(
      head(temp_breaks, -1),
      " to ",
      tail(temp_breaks, -1)
    )
    
    p <- ggplot2::ggplot()
    p <- add_map_features_before(p)
    
    p <- p +
      ggplot2::geom_tile(
        data = temp_df,
        ggplot2::aes(x = x, y = y, fill = range),
        alpha = 0.85
      ) +
      ggplot2::scale_fill_manual(
        values = temp_cols,
        na.value = "white",
        name = "\u00B0C",
        labels = temp_labs,
        drop = FALSE
      )
    
    p <- add_common_map_layout(p)
    p <- add_map_features_after(p)
    
    p +
      ggplot2::labs(
        title = "ERA5-Land top-layer soil temperature",
        subtitle = paste0("Monthly mean, \u00B0C | ", map_date_label)
      )
  }
  
  # ----------------------------------------------------------
  # Snow/freeze mask map
  # ----------------------------------------------------------
  
  make_mask_map <- function(mask_df) {
    
    mask_df$class <- factor(
      as.integer(round(mask_df$value)),
      levels = c(0, 1, 2, 3),
      labels = mask_labs
    )
    
    p <- ggplot2::ggplot()
    p <- add_map_features_before(p)
    
    p <- p +
      ggplot2::geom_tile(
        data = mask_df,
        ggplot2::aes(x = x, y = y, fill = class),
        alpha = 0.85
      ) +
      ggplot2::scale_fill_manual(
        values = unname(mask_cols),
        na.value = "white",
        name = "Mask class",
        drop = FALSE
      )
    
    p <- add_common_map_layout(p)
    p <- add_map_features_after(p)
    
    p +
      ggplot2::labs(
        title = "ERA5-Land snow/frozen-ground caution mask",
        subtitle = paste0(
          map_date_label,
          " | 0 = no mask, 1 = snow, 2 = frozen soil, 3 = snow + frozen soil"
        )
      )
  }
  
  # ----------------------------------------------------------
  # Build maps
  # ----------------------------------------------------------
  
  map_swvl1 <- make_soil_moisture_map(
    rast_df = swvl1_df,
    overlay_df = overlay_mask_df_swvl1,
    layer_label = "swvl1",
    depth_label = "0-7 cm"
  )
  
  map_swvl2 <- make_soil_moisture_map(
    rast_df = swvl2_df,
    overlay_df = overlay_mask_df_swvl2,
    layer_label = "swvl2",
    depth_label = "7-28 cm"
  )
  
  map_temperature <- make_temperature_map(temp_df)
  map_mask <- make_mask_map(mask_df)
  
  # ----------------------------------------------------------
  # Save outputs
  # ----------------------------------------------------------
  
  out_files <- list(
    swvl1 = file.path(projDir, paste0("era5land_swvl1_percentile_", ym_label, ".png")),
    swvl2 = file.path(projDir, paste0("era5land_swvl2_percentile_", ym_label, ".png")),
    soil_temperature = file.path(projDir, paste0("era5land_soil_temperature_", ym_label, ".png")),
    snow_freeze_mask = file.path(projDir, paste0("era5land_snow_freeze_mask_", ym_label, ".png"))
  )
  
  ggplot2::ggsave(out_files$swvl1, map_swvl1, width = 8, height = 6, dpi = 150)
  ggplot2::ggsave(out_files$swvl2, map_swvl2, width = 8, height = 6, dpi = 150)
  ggplot2::ggsave(out_files$soil_temperature, map_temperature, width = 8, height = 6, dpi = 150)
  ggplot2::ggsave(out_files$snow_freeze_mask, map_mask, width = 8, height = 6, dpi = 150)
  
  # ----------------------------------------------------------
  # Cleanup downloaded URL cache if requested
  # ----------------------------------------------------------
  
  if (delete_cached_downloads && source_type == "url") {
    unlink(unlist(product_paths), force = TRUE)
  }
  
  # ----------------------------------------------------------
  # Return ggplot objects, file paths, and source product paths
  # ----------------------------------------------------------
  
  list(
    maps = list(
      swvl1 = map_swvl1,
      swvl2 = map_swvl2,
      soil_temperature = map_temperature,
      snow_freeze_mask = map_mask
    ),
    output_files = out_files,
    product_paths = product_paths,
    settings = list(
      year = yr,
      month = mo,
      source_type = source_type,
      product_base = product_base,
      clim_label = clim_label,
      apply_mask_to_soil_moisture = apply_mask_to_soil_moisture,
      show_mask_overlay = show_mask_overlay,
      mask_classes_to_apply = mask_classes_to_apply
    )
  )
}


###################
# examples
#####

load("C:/Users/Crimmins/OneDrive - University of Arizona/RProjects/ERA5_LAND_Soil_Moisture/testing/KNF_testing_data.RData")

# local directory example
era5_maps <- mapERA5LandProducts(
  mo = 5,
  yr = 2026,
  boundPoly = Level1Data,
  projDir = "./testing/figs/",
  mapFeatures = NULL,
  Mapbox = NULL,
  product_base = "./data/processed",
  source_type = "local",
  clim_label = "1981_2020",
  apply_mask_to_soil_moisture = TRUE
)

#### external URL
# Also update the URL version
# 
# The same structure works for a hosted URL if you mirror the folder structure online:
#   
#   https://your-server.org/era5land-soil-moisture/processed/
#   percentiles/
#   temperature/
#   snow_freeze_mask/
#   
#   Then use:
#   
#   product_base = "https://your-server.org/era5land-soil-moisture/processed"
# 
# The function will construct URLs like:
#   
#   https://your-server.org/era5land-soil-moisture/processed/percentiles/swvl1_percentile_2026_05_vs_1981_2020.tif
# https://your-server.org/era5land-soil-moisture/processed/temperature/stl1_monthly_mean_c_2026_05

era5_maps <- mapERA5LandProducts(
  mo = 5,
  yr = 2026,
  boundPoly = boundPoly,
  projDir = "./Pima County Monthly Report/Figures/",
  mapFeatures = mapFeatures,
  Mapbox = Mapbox,
  product_base = "https://your-server.org/era5land-soil-moisture/2026_05",
  source_type = "url",
  clim_label = "1981_2020",
  apply_mask_to_soil_moisture = TRUE
)

##### no mask applied
era5_maps <- mapERA5LandProducts(
  mo = 5,
  yr = 2026,
  boundPoly = boundPoly,
  projDir = "./Pima County Monthly Report/Figures/",
  mapFeatures = mapFeatures,
  Mapbox = Mapbox,
  product_base = "C:/path/to/products/2026_05",
  source_type = "local",
  apply_mask_to_soil_moisture = FALSE,
  show_mask_overlay = FALSE
)
