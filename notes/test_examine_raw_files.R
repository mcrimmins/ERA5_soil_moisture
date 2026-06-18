
# R/02_inspect_raw_tifs.R

source("R/00_config.R")

library(terra)
library(dplyr)
library(purrr)
library(tibble)
library(fs)

# ------------------------------------------------------------
# Locate downloaded raw annual GeoTIFFs
# ------------------------------------------------------------

raw_files <- dir_ls(
  dir_raw,
  regexp = paste0("[0-9]{4}_", era5land_shortname, "_ERA5Land_monthly\\.tif$")
)

raw_files <- sort(raw_files)

if (length(raw_files) == 0) {
  stop("No raw GeoTIFF files found in: ", dir_raw)
}

message("Found ", length(raw_files), " raw files.")
print(raw_files)

# Pick one file
f <- raw_files[1]

r <- rast(f)

print(f)
print(r)
print(names(r))

# Basic metadata
cat("\nNumber of layers:", nlyr(r), "\n")
cat("CRS:\n")
print(crs(r))
cat("\nExtent:\n")
print(ext(r))
cat("\nResolution:\n")
print(res(r))

# Quick plot of all 12 months
plot(r, main = names(r))

# Plot a single month
plot(r[[7]], main = paste("July", basename(f)))


