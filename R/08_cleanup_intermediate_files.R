# R/08_cleanup_intermediate_files.R

source("R/00_config.R")

library(fs)

# ------------------------------------------------------------
# User options
# ------------------------------------------------------------

clean_temp <- TRUE
clean_raw_hourly_zips <- TRUE
clean_mask_hourly_zips <- TRUE
clean_hourly_debug_tifs <- TRUE

clean_old_current_monthly <- FALSE
clean_old_mask_diagnostics <- FALSE
clean_old_qa_maps <- FALSE

# Keep this many most recent files when cleaning old outputs
keep_n_recent_current_monthly <- 6
keep_n_recent_mask_diagnostics <- 6
keep_n_recent_qa_maps <- 12

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

delete_files_matching <- function(path, pattern) {
  
  if (!dir.exists(path)) {
    message("Directory does not exist: ", path)
    return(invisible(character(0)))
  }
  
  files <- list.files(
    path,
    pattern = pattern,
    full.names = TRUE,
    recursive = FALSE
  )
  
  if (length(files) == 0) {
    message("No files matched in ", path, ": ", pattern)
    return(invisible(character(0)))
  }
  
  message("Deleting ", length(files), " file(s) from ", path)
  print(files)
  
  unlink(files, force = TRUE)
  
  invisible(files)
}

delete_old_files_keep_recent <- function(path, pattern, keep_n_recent = 6) {
  
  if (!dir.exists(path)) {
    message("Directory does not exist: ", path)
    return(invisible(character(0)))
  }
  
  files <- list.files(
    path,
    pattern = pattern,
    full.names = TRUE,
    recursive = FALSE
  )
  
  if (length(files) <= keep_n_recent) {
    message("Nothing to delete in ", path, ". File count <= keep_n_recent.")
    return(invisible(character(0)))
  }
  
  info <- file.info(files)
  files_ordered <- rownames(info)[order(info$mtime, decreasing = TRUE)]
  
  files_to_keep <- head(files_ordered, keep_n_recent)
  files_to_delete <- setdiff(files_ordered, files_to_keep)
  
  message("Keeping ", length(files_to_keep), " recent file(s).")
  message("Deleting ", length(files_to_delete), " older file(s) from ", path)
  print(files_to_delete)
  
  unlink(files_to_delete, force = TRUE)
  
  invisible(files_to_delete)
}

# ------------------------------------------------------------
# Cleanup actions
# ------------------------------------------------------------

if (clean_temp && dir.exists(dir_temp)) {
  message("Cleaning temp directory: ", dir_temp)
  unlink(file.path(dir_temp, "*"), recursive = TRUE, force = TRUE)
}

if (clean_raw_hourly_zips) {
  delete_files_matching(
    path = dir_current_raw,
    pattern = "\\.zip$"
  )
}

if (clean_mask_hourly_zips) {
  delete_files_matching(
    path = dir_mask_raw,
    pattern = "\\.zip$"
  )
}

if (clean_hourly_debug_tifs) {
  
  delete_files_matching(
    path = dir_current_raw,
    pattern = "_(hourly|daily_mean)_.*_ERA5Land\\.tif$"
  )
  
  delete_files_matching(
    path = dir_mask_raw,
    pattern = "_(hourly|daily_mean)_.*_ERA5Land\\.tif$"
  )
}

if (clean_old_current_monthly) {
  
  delete_old_files_keep_recent(
    path = dir_current_monthly,
    pattern = "swvl[12]_monthly_mean_.*_ERA5Land_hourly_derived\\.tif$",
    keep_n_recent = keep_n_recent_current_monthly
  )
}

if (clean_old_mask_diagnostics) {
  
  delete_old_files_keep_recent(
    path = dir_mask_monthly,
    pattern = "(snow_depth|stl1)_monthly_(mean|fraction)_.*_ERA5Land_hourly_derived\\.tif$",
    keep_n_recent = keep_n_recent_mask_diagnostics
  )
}

if (clean_old_qa_maps) {
  
  delete_old_files_keep_recent(
    path = dir_qa,
    pattern = "\\.(png|pdf)$",
    keep_n_recent = keep_n_recent_qa_maps
  )
}

message("Cleanup complete.")