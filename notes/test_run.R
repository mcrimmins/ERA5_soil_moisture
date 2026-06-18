# Recommended run order
# 
# For a new build with both layers:
#   
#   source("R/01_download_era5land_monthly_climatology.R")
# source("R/02_build_climatology.R")
# 
# Then for an operational month:
#   
#   source("R/05_run_operational_percentile_workflow.R")
# 
# For a test run on May 2025, edit the top of R/05_run_operational_percentile_workflow.R:
#   
#   target_year <- 2025
# target_month <- 5
# layers_to_run <- c("swvl1", "swvl2")
# 
# Expected operational outputs:
#   
#   data/processed/current_monthly/
#   swvl1_monthly_mean_2025_05_ERA5Land_hourly_derived.tif
# swvl2_monthly_mean_2025_05_ERA5Land_hourly_derived.tif
# 
# data/processed/percentiles/
#   swvl1_percentile_2025_05_vs_1991_2020.tif
# swvl2_percentile_2025_05_vs_1991_2020.tif
# swvl1_percentile_class_2025_05_vs_1991_2020.tif
# swvl2_percentile_class_2025_05_vs_1991_2020.tif
# 
# output/qa_maps/
#   qa_swvl1_percentile_2025_05.png
# qa_swvl2_percentile_2025_05.png