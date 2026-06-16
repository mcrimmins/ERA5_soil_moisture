# ERA5-Land Soil Moisture Percentile Workflow

This repository contains an R workflow for producing monthly ERA5-Land soil moisture percentile maps for the western United States. The workflow downloads ERA5-Land soil moisture, builds same-calendar-month climatologies, and generates operational percentile maps for recent months using hourly ERA5-Land data aggregated to monthly means.

The primary use case is a beginning-of-month monitoring report where the previous month's soil moisture conditions need to be summarized during the first week of the following month.

## Overview

The workflow produces percentile ranks for ERA5-Land volumetric soil water content at two soil depths:

| Layer | ERA5-Land variable | Depth | Interpretation |
|---|---|---:|---|
| `swvl1` | `volumetric_soil_water_layer_1` | 0-7 cm | Near-surface soil moisture; responsive to short-term wetting and drying |
| `swvl2` | `volumetric_soil_water_layer_2` | 7-28 cm | Shallow subsurface/root-zone soil moisture; smoother antecedent moisture indicator |

Percentiles are computed relative to a same-calendar-month climatology. For example, May 2025 soil moisture is compared only with historical May values from the selected baseline period.

The percentile convention is:

| Percentile | Meaning |
|---:|---|
| 0-10 | Very dry relative to the historical same-month distribution |
| 10-20 | Dry |
| 40-60 | Near normal |
| 80-100 | Wet |

Lower percentile values indicate drier-than-normal soil moisture. Higher percentile values indicate wetter-than-normal soil moisture.

## Workflow summary

The workflow has two major phases.

### 1. Climatology build phase

This phase is run occasionally, not every month. It downloads monthly ERA5-Land soil moisture for the baseline period and builds same-month climatology stacks.

Example outputs:

```text
data/processed/climatology/monthly_stacks/
  swvl1_clim_stack_01_1991_2020.tif
  swvl1_clim_stack_02_1991_2020.tif
  ...
  swvl2_clim_stack_12_1991_2020.tif
```

Each climatology stack contains one layer per baseline year for a given calendar month.

### 2. Operational monthly phase

This phase is run each month. It downloads hourly ERA5-Land data for the previous complete month, aggregates it to a monthly mean, computes percentile ranks against the appropriate climatology stack, and writes map-ready outputs.

Example outputs for May 2025:

```text
data/processed/current_monthly/
  swvl1_monthly_mean_2025_05_ERA5Land_hourly_derived.tif
  swvl2_monthly_mean_2025_05_ERA5Land_hourly_derived.tif

data/processed/percentiles/
  swvl1_percentile_2025_05_vs_1991_2020.tif
  swvl2_percentile_2025_05_vs_1991_2020.tif
  swvl1_percentile_class_2025_05_vs_1991_2020.tif
  swvl2_percentile_class_2025_05_vs_1991_2020.tif

output/maps/
  soil_moisture_percentiles_2025_05_swvl1_swvl2.png
  soil_moisture_percentiles_2025_05_swvl1_swvl2.pdf
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
│   └── 06_plot_percentile_maps.R
├── data/
│   ├── raw/
│   │   ├── era5land_monthly/
│   │   └── era5land_hourly_current/
│   ├── processed/
│   │   ├── climatology/
│   │   ├── current_monthly/
│   │   └── percentiles/
│   └── temp/
├── output/
│   ├── maps/
│   └── qa_maps/
├── logs/
├── .Renviron
├── .gitignore
└── README.md
```

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

## Configuration

The main settings are defined in `R/00_config.R`, including:

- project directories
- CDS key setup
- ERA5-Land dataset names
- soil moisture layers to process
- geographic bounding box
- climatology years

Example settings:

```r
target_layers <- c("swvl1", "swvl2")
aoi_cds <- c(49.8, -126.9, 23.7, -65.6)
clim_years <- 1991:2020
clim_months <- 1:12
```

The CDS area convention is:

```text
North, West, South, East
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
data/raw/era5land_monthly/1991_swvl1_ERA5Land_monthly.tif
data/raw/era5land_monthly/1991_swvl2_ERA5Land_monthly.tif
```

### Step 2: Build climatology stacks

```r
source("R/02_build_climatology.R")
```

This creates same-calendar-month stacks and quantile rasters for QA.

Example output:

```text
data/processed/climatology/monthly_stacks/swvl1_clim_stack_05_1991_2020.tif
data/processed/climatology/monthly_quantiles/swvl1_clim_quantiles_05_1991_2020.tif
```

### Step 3: Run the monthly operational workflow

For the previous complete month:

```r
source("R/05_run_operational_percentile_workflow.R")
```

To run a specific test month, edit the options at the top of `R/05_run_operational_percentile_workflow.R`:

```r
target_year <- 2025
target_month <- 5
layers_to_run <- c("swvl1", "swvl2")
```

The workflow will:

1. download hourly ERA5-Land data for the target month,
2. aggregate hourly data to daily means,
3. aggregate daily means to a monthly mean,
4. compute percentile ranks against the matching climatology stack,
5. write percentile rasters and QA plots,
6. optionally clean temporary and intermediate files.

### Step 4: Plot percentile maps

```r
source("R/06_plot_percentile_maps.R")
```

The plotting script produces side-by-side percentile maps for `swvl1` and `swvl2` using a diverging dry-to-wet color ramp centered on the 50th percentile.

## Cleanup options

The operational workflow includes options for removing temporary and intermediate files.

Recommended development settings:

```r
overwrite_download <- TRUE
overwrite_percentile <- TRUE
clean_temp_files <- TRUE
delete_download_zip <- TRUE
delete_hourly_debug_files <- TRUE
delete_current_monthly_mean <- FALSE
delete_percentile_outputs <- FALSE
```

Recommended production settings may differ depending on storage needs. In general, keep the final percentile rasters and delete large zip/temp files.

## Notes on ERA5-Land and near-real-time reporting

For first-week monthly reporting, the final ERA5-Land monthly product may not be available quickly enough. This workflow therefore downloads hourly ERA5-Land data for the target month and aggregates it to a monthly mean.

Recent ERA5-Land data may be preliminary near-real-time data and can be superseded by finalized data later. For archival or publication-quality analysis, consider rerunning reports after finalized ERA5-Land data become available.

## Snow and frozen soil interpretation

ERA5-Land `swvl1` and `swvl2` are model soil water states. Snow and frozen soil are not explicitly masked in the percentile calculation. They are included implicitly in the ERA5-Land land-surface model and in the same-month historical distribution.

This is appropriate for same-calendar-month percentile monitoring, but winter and high-elevation values should not be interpreted directly as fire-danger or fuel-moisture indicators without additional snow/freeze context.

Recommended future additions:

- snow depth or snow water equivalent flag
- 2 m temperature freeze flag
- polygon-level reporting summaries
- comparison with RAWS/NFDRS/ERC or fuel-moisture observations

## Baseline period

The default climatology baseline is currently 1991-2020. This is easy to communicate as a climate-normal-style reference period, but it provides only 30 same-month samples per grid cell.

For more stable tail percentiles, consider testing longer fixed baselines such as:

```r
clim_years <- 1981:2020
clim_years <- 1951:2020
```

Avoid using a continuously updating full-period-to-present baseline for operational reports unless reproducibility concerns are addressed.

## Citation and data acknowledgement

This workflow uses ERA5-Land data produced by the European Centre for Medium-Range Weather Forecasts and distributed through the Copernicus Climate Data Store.

Users should cite and acknowledge ERA5-Land and Copernicus Climate Data Store according to the current CDS dataset citation guidance.

## Disclaimer

The outputs are modeled soil moisture percentiles from ERA5-Land. They are useful for monitoring relative land-surface wetness and dryness, but they are not direct fuel-moisture measurements and should not be used as standalone fire-danger ratings.
