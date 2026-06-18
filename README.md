# ERA5-Land Soil Moisture Percentile Workflow

This repository contains an R workflow for producing monthly ERA5-Land soil moisture percentile maps for the western United States. The workflow downloads ERA5-Land soil moisture, builds same-calendar-month climatologies, generates operational percentile rasters for recent full calendar months, and creates optional display masks for areas affected by snow or persistent frozen surface soil.

The primary use case is a beginning-of-month monitoring report where the previous month's soil moisture conditions need to be summarized during the first week of the following month. The workflow is designed as a **full-calendar-month product**, not a rolling-window product.

## Overview

The workflow produces percentile ranks for ERA5-Land volumetric soil water content at two soil depths.

| Layer   | ERA5-Land variable              |   Depth | Interpretation                                                                     |
| ------- | ------------------------------- | ------: | ---------------------------------------------------------------------------------- |
| `swvl1` | `volumetric_soil_water_layer_1` |  0-7 cm | Near-surface soil moisture; responsive to short-term wetting and drying            |
| `swvl2` | `volumetric_soil_water_layer_2` | 7-28 cm | Shallow subsurface/root-zone soil moisture; smoother antecedent moisture indicator |

Percentiles are computed relative to a same-calendar-month climatology. For example, May 2026 soil moisture is compared only with historical May values from the selected baseline period.

The percentile convention is:

| Percentile | Meaning                                                     |
| ---------: | ----------------------------------------------------------- |
|       0-10 | Very dry relative to the historical same-month distribution |
|      10-20 | Dry                                                         |
|      40-60 | Near normal                                                 |
|     80-100 | Wet                                                         |

Lower percentile values indicate drier-than-normal soil moisture. Higher percentile values indicate wetter-than-normal soil moisture.

## Current default settings

The main settings are defined in `R/00_config.R`.

Current defaults:

```r
target_layers <- c("swvl1", "swvl2")
aoi_cds <- c(49.8, -126.9, 23.7, -65.6)
clim_years <- 1981:2020
clim_months <- 1:12
```

The CDS area convention is:

```text
North, West, South, East
```

The default climatology baseline is currently `1981:2020`.

## Workflow summary

The workflow has four major phases.

### 1. Climatology build phase

This phase is run occasionally, not every month. It downloads monthly ERA5-Land soil moisture for the baseline period and builds same-month climatology stacks.

Example outputs:

```text
data/processed/climatology/monthly_stacks/
  swvl1_clim_stack_01_1981_2020.tif
  swvl1_clim_stack_02_1981_2020.tif
  ...
  swvl2_clim_stack_12_1981_2020.tif
```

Each climatology stack contains one layer per baseline year for a given calendar month.

### 2. Operational monthly phase

This phase is run each month. It downloads hourly ERA5-Land data for the previous complete calendar month, verifies that all expected hourly layers are present, aggregates hourly data to daily means, aggregates daily means to a monthly mean, computes percentile ranks against the appropriate climatology stack, and writes map-ready GeoTIFF outputs.

Example outputs for May 2026:

```text
data/processed/current_monthly/
  swvl1_monthly_mean_2026_05_ERA5Land_hourly_derived.tif
  swvl2_monthly_mean_2026_05_ERA5Land_hourly_derived.tif

data/processed/percentiles/
  swvl1_percentile_2026_05_vs_1981_2020.tif
  swvl2_percentile_2026_05_vs_1981_2020.tif
  swvl1_percentile_class_2026_05_vs_1981_2020.tif
  swvl2_percentile_class_2026_05_vs_1981_2020.tif
```

### 3. Snow/frozen-ground mask and soil-temperature diagnostics

The workflow can optionally create a snow/frozen-ground caution mask using ERA5-Land snow depth and top-layer soil temperature. Masked cells are not removed from the raw percentile rasters; they are grayed out in display products to indicate that soil-moisture percentile interpretation may be affected by snowpack, frozen soil, or thaw processes.

The workflow also preserves top-layer soil temperature diagnostics as separate reporting layers.

Example outputs:

```text
data/processed/snow_freeze_mask/
  snow_depth_monthly_mean_2026_05_ERA5Land_hourly_derived.tif
  snow_depth_monthly_fraction_2026_05_ERA5Land_hourly_derived.tif
  stl1_monthly_mean_2026_05_ERA5Land_hourly_derived.tif
  stl1_monthly_fraction_2026_05_ERA5Land_hourly_derived.tif
  snow_freeze_mask_2026_05.tif

data/processed/temperature/
  stl1_monthly_mean_c_2026_05_ERA5Land_hourly_derived.tif
  stl1_monthly_mean_k_2026_05_ERA5Land_hourly_derived.tif
  stl1_freeze_fraction_2026_05_ERA5Land_hourly_derived.tif
```

Mask classes are:

| Class | Meaning                                 |
| ----: | --------------------------------------- |
|     0 | Not masked                              |
|     1 | Snow                                    |
|     2 | Persistent frozen surface soil          |
|     3 | Snow and persistent frozen surface soil |

### 4. Plotting phase

The plotting script creates presentation-ready PNG and PDF maps for the soil moisture percentiles and, optionally, soil temperature diagnostics.

Example outputs:

```text
output/maps/
  soil_moisture_percentiles_2026_05_swvl1_swvl2_snow_freeze_masked.png
  soil_moisture_percentiles_2026_05_swvl1_swvl2_snow_freeze_masked.pdf
  soil_temperature_2026_05.png
  soil_temperature_2026_05.pdf
```

## Repository structure

```text
.
├── R/
│   ├── 00_config.R
│   ├── 01_download_era5land_monthly_climatology.R
│   ├── 02_build_climatology.R
│   ├── 03_download_current_month_era5land_hourly.R
│   ├── 04_compute_monthly_percentile.R
│   ├── 05_run_operational_percentile_workflow.R
│   ├── 06_download_snow_freeze_monthly.R
│   └── 07_plot_percentile_maps.R
├── data/
│   ├── raw/
│   │   ├── era5land_monthly/
│   │   ├── era5land_hourly_current/
│   │   └── era5land_mask_hourly/
│   ├── processed/
│   │   ├── climatology/
│   │   │   ├── monthly_stacks/
│   │   │   └── monthly_quantiles/
│   │   ├── current_monthly/
│   │   ├── percentiles/
│   │   ├── snow_freeze_mask/
│   │   └── temperature/
│   └── temp/
├── output/
│   ├── maps/
│   └── qa_maps/
├── logs/
├── .Renviron
├── .gitignore
└── README.md
```

The `data/`, `output/`, and `logs/` directories are generated by the workflow and can be excluded from Git. If you want empty directories to appear in GitHub, add `.gitkeep` files and adjust `.gitignore` accordingly.

## Requirements

This workflow is written in R and uses the Copernicus Climate Data Store through the `ecmwfr` package.

Recommended R packages:

```r
install.packages(c(
  "ecmwfr",
  "terra",
  "sf",
  "dplyr",
  "lubridate",
  "fs",
  "archive",
  "tictoc"
))
```

Optional packages for development and project setup:

```r
install.packages(c("usethis", "here"))
```

## CDS API setup

You need a Copernicus Climate Data Store account and API key.

Store your CDS key in a project-level `.Renviron` file rather than hard-coding it in scripts.

Create or edit `.Renviron`:

```r
usethis::edit_r_environ(scope = "project")
```

Add:

```text
CDS_KEY=your-cds-key-here
```

Then restart R.

Confirm R can see the key:

```r
Sys.getenv("CDS_KEY")
```

Make sure `.Renviron` is listed in `.gitignore` so credentials are not committed:

```text
.Renviron
```

## Running the workflow

### Step 1: Download climatology-period monthly ERA5-Land data

Run this once when building or updating the climatology archive:

```r
source("R/01_download_era5land_monthly_climatology.R")
```

This downloads annual 12-layer monthly files for each configured soil layer.

Example output:

```text
data/raw/era5land_monthly/1981_swvl1_ERA5Land_monthly.tif
data/raw/era5land_monthly/1981_swvl2_ERA5Land_monthly.tif
```

### Step 2: Build climatology stacks

```r
source("R/02_build_climatology.R")
```

This creates same-calendar-month stacks and quantile rasters for QA.

Example output:

```text
data/processed/climatology/monthly_stacks/swvl1_clim_stack_05_1981_2020.tif
data/processed/climatology/monthly_quantiles/swvl1_clim_quantiles_05_1981_2020.tif
```

### Step 3: Run the monthly operational workflow

For the previous complete month:

```r
source("R/05_run_operational_percentile_workflow.R")
```

To run a specific test month, edit the options at the top of `R/05_run_operational_percentile_workflow.R`:

```r
target_year <- 2026
target_month <- 5
layers_to_run <- c("swvl1", "swvl2")
```

The workflow will:

1. download hourly ERA5-Land soil moisture for the full target calendar month if monthly mean files do not already exist,
2. verify that all expected hourly layers are present,
3. aggregate hourly data to daily means,
4. aggregate daily means to a monthly mean,
5. compute percentile ranks against the matching climatology stack,
6. write percentile rasters and QA plots,
7. optionally create a snow/frozen-ground mask,
8. optionally preserve soil-temperature diagnostics,
9. optionally clean temporary files.

The operational script is designed to support reruns. By default, it reuses existing monthly means, percentile rasters, snow/freeze diagnostics, and mask outputs unless overwrite options are set to `TRUE`.

### Step 4: Plot percentile and soil-temperature maps

```r
source("R/07_plot_percentile_maps.R")
```

The plotting script produces side-by-side percentile maps for `swvl1` and `swvl2` using a dry-to-wet color ramp. If the snow/frozen-ground mask exists and `use_snow_freeze_mask <- TRUE`, masked cells are shown in gray.

If `make_temperature_maps <- TRUE`, the plotting script also produces soil-temperature diagnostic maps showing monthly mean top-layer soil temperature in degrees Celsius and frozen surface soil frequency.

## Operational timing

This workflow is a full-calendar-month product. It should normally be run on the **6th of the month or later** for the previous month. Running on the 7th or 8th provides additional buffer if CDS availability is delayed.

The scripts check the actual number of hourly layers returned by CDS. If a full month is incomplete, the workflow stops rather than producing a partial-month map.

Expected hourly layers:

| Month length | Expected layers |
| -----------: | --------------: |
|      28 days |             672 |
|      29 days |             696 |
|      30 days |             720 |
|      31 days |             744 |

## Snow and frozen-ground mask settings

Snow/frozen-ground mask settings live in `R/00_config.R`.

Current recommended settings:

```r
snow_depth_threshold_m <- 0.01
snow_fraction_threshold <- 0.25

soil_freeze_threshold_k <- 273.15
freeze_fraction_threshold <- 0.50
```

ERA5-Land top-layer soil temperature is downloaded in Kelvin. The workflow writes Celsius reporting products by subtracting 273.15:

```r
stl1_c <- stl1_k - 273.15
```

The current snow/frozen-ground mask logic is:

```r
snow_flag <- (snow_mean > snow_depth_threshold_m) |
             (snow_fraction >= snow_fraction_threshold)

freeze_flag <- (stl1_mean <= soil_freeze_threshold_k) &
               (freeze_fraction >= freeze_fraction_threshold)

mask <- snow_flag + 2 * freeze_flag
```

This logic treats snow relatively sensitively, while frozen-ground masking is more conservative. A grid cell is flagged for persistent frozen surface soil only when the monthly mean top-layer soil temperature is at or below freezing and the fraction of hourly timesteps at or below freezing exceeds the configured threshold.

If `snow_depth_threshold_m` or `soil_freeze_threshold_k` changes, rebuild diagnostics because the fraction rasters depend on those thresholds:

```r
source("R/06_download_snow_freeze_monthly.R")

create_snow_freeze_mask(
  year = 2026,
  month = 5,
  overwrite = TRUE,
  overwrite_diagnostics = TRUE,
  delete_download_zip = TRUE
)
```

If only `snow_fraction_threshold` or `freeze_fraction_threshold` changes, you can rebuild the final categorical mask without redownloading hourly diagnostics:

```r
source("R/06_download_snow_freeze_monthly.R")

create_snow_freeze_mask(
  year = 2026,
  month = 5,
  overwrite = TRUE,
  overwrite_diagnostics = FALSE,
  delete_download_zip = TRUE
)
```

## Operational overwrite and cleanup options

The operational script is organized around resume/skip behavior. These options live near the top of `R/05_run_operational_percentile_workflow.R`.

Recommended production defaults:

```r
overwrite_current_monthly <- FALSE
overwrite_percentiles <- FALSE
overwrite_mask <- FALSE
overwrite_mask_diagnostics <- FALSE

build_snow_freeze_mask <- TRUE
make_mask_qa_plot <- FALSE
make_percentile_qa_plot <- TRUE

clean_temp_files <- TRUE
delete_download_zip <- TRUE
delete_hourly_debug_files <- TRUE

delete_current_monthly_mean <- FALSE
delete_percentile_outputs <- FALSE
```

These settings reuse completed products and make reruns faster.

For mask-threshold testing:

```r
overwrite_current_monthly <- FALSE
overwrite_percentiles <- FALSE
overwrite_mask <- TRUE
overwrite_mask_diagnostics <- FALSE
```

For a complete rebuild of the operational month:

```r
overwrite_current_monthly <- TRUE
overwrite_percentiles <- TRUE
overwrite_mask <- TRUE
overwrite_mask_diagnostics <- TRUE
```

Use complete rebuilds sparingly because they require additional CDS requests and can take substantially longer.

## Final monthly deliverables

For GIS users, the recommended raw GeoTIFF deliverables are:

```text
data/processed/percentiles/
  swvl1_percentile_YYYY_MM_vs_1981_2020.tif
  swvl2_percentile_YYYY_MM_vs_1981_2020.tif
  swvl1_percentile_class_YYYY_MM_vs_1981_2020.tif
  swvl2_percentile_class_YYYY_MM_vs_1981_2020.tif

data/processed/snow_freeze_mask/
  snow_freeze_mask_YYYY_MM.tif

data/processed/temperature/
  stl1_monthly_mean_c_YYYY_MM_ERA5Land_hourly_derived.tif
  stl1_freeze_fraction_YYYY_MM_ERA5Land_hourly_derived.tif
```

The Kelvin soil-temperature file is also preserved internally:

```text
data/processed/temperature/
  stl1_monthly_mean_k_YYYY_MM_ERA5Land_hourly_derived.tif
```

For report or communication products, share the map outputs:

```text
output/maps/
  soil_moisture_percentiles_YYYY_MM_swvl1_swvl2_snow_freeze_masked.png
  soil_moisture_percentiles_YYYY_MM_swvl1_swvl2_snow_freeze_masked.pdf
  soil_temperature_YYYY_MM.png
  soil_temperature_YYYY_MM.pdf
```

## Storage and cleanup guidance

The largest folders over time are likely to be:

```text
data/raw/era5land_monthly/
data/raw/era5land_hourly_current/
data/raw/era5land_mask_hourly/
data/processed/current_monthly/
data/processed/percentiles/
data/processed/snow_freeze_mask/
data/processed/temperature/
output/maps/
output/qa_maps/
```

Suggested long-term storage policy:

Keep:

```text
data/processed/climatology/
data/processed/percentiles/
data/processed/snow_freeze_mask/snow_freeze_mask_*.tif
data/processed/temperature/stl1_monthly_mean_c_*.tif
data/processed/temperature/stl1_freeze_fraction_*.tif
output/maps/
```

Clean periodically:

```text
data/temp/
data/raw/era5land_hourly_current/
data/raw/era5land_mask_hourly/
output/qa_maps/
logs/
```

Archive externally if storage is limited:

```text
data/raw/era5land_monthly/
data/processed/current_monthly/
data/processed/snow_freeze_mask/*monthly_mean*.tif
data/processed/snow_freeze_mask/*monthly_fraction*.tif
```

The monthly climatology files are expensive to rebuild because they require many CDS downloads. Avoid deleting `data/processed/climatology/` unless you intend to rebuild the baseline.

## Notes on ERA5-Land and near-real-time reporting

For first-week monthly reporting, the final ERA5-Land monthly product may not be available quickly enough. This workflow therefore downloads hourly ERA5-Land data for the target month and aggregates it to a monthly mean.

Recent ERA5-Land data may be preliminary near-real-time data and can be superseded by finalized data later. For archival or publication-quality analysis, consider rerunning reports after finalized ERA5-Land data become available.

## Baseline period

The default climatology baseline is currently `1981:2020`. This provides 40 same-month samples per grid cell and avoids relying on only the 30 samples available from a 1991-2020 climate-normal-style baseline.

Alternative fixed baselines can be tested by changing `clim_years` in `R/00_config.R`:

```r
clim_years <- 1991:2020
clim_years <- 1951:2020
```

Avoid using a continuously updating full-period-to-present baseline for operational reports unless reproducibility concerns are addressed.

## Citation and data acknowledgement

This workflow uses ERA5-Land data produced by the European Centre for Medium-Range Weather Forecasts and distributed through the Copernicus Climate Data Store.

Users should cite and acknowledge ERA5-Land and the Copernicus Climate Data Store according to the current CDS dataset citation guidance.

## Disclaimer

The outputs are modeled soil moisture percentiles from ERA5-Land. They are useful for monitoring relative land-surface wetness and dryness, but they are not direct fuel-moisture measurements and should not be used as standalone fire-danger ratings.

Gray snow/frozen-ground mask areas are caution flags for interpretation. They do not mean the raw soil moisture percentile rasters are missing or invalid.
