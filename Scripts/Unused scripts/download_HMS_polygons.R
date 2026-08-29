# =============================================================================
# Download HMS smoke shapefiles for the 2025 YEWA date range
# Run this BEFORE the corroboration section of 2a_YEWA smoke data.
# Writes into the same Smoke_polygons folder that script reads from.
# =============================================================================

library(lubridate)
library(tidyverse)

###########################
######## WTSP #############
###########################

# --- Settings ----------------------------------------------------------------
# homeDir MUST match the analysis script: homeDir <- "Smoke_polygons"
# (that script uses a relative path, so set your working directory to the YEWA
#  project root first, or make this an absolute path that points to the same folder)
homeDir <- "Smoke_polygons"

# Set to your actual YEWA 2025 recording span. Check with:
#   range(as.Date(YEWA$date))
start_date <- as.Date("2023-05-01")
end_date   <- as.Date("2023-06-20")

all_dates <- seq(start_date, end_date, by = "day")

# --- Download one zip per date -----------------------------------------------
# Existing valid zips are skipped, so re-running is cheap.
for (d in all_dates) {
  d         <- as.Date(d, origin = "1970-01-01")
  inFolder  <- file.path(year(d), sprintf("%02d", month(d)))
  fullDir   <- file.path(homeDir, inFolder)
  dir.create(fullDir, recursive = TRUE, showWarnings = FALSE)
  
  date_id   <- format(d, "%Y%m%d")
  file_name <- paste0("hms_smoke", date_id, ".zip")
  dest      <- file.path(fullDir, file_name)
  
  if (file.exists(dest) && file.info(dest)$size > 1000) next   # already have it
  
  url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
                inFolder, "/", file_name)
  ok <- tryCatch(
    { download.file(url, destfile = dest, mode = "wb", method = "libcurl", quiet = TRUE); TRUE },
    error = function(e) { message("  failed: ", file_name, " - ", conditionMessage(e)); FALSE }
  )
  if (ok && file.exists(dest) && file.info(dest)$size < 1000) file.remove(dest)
}

# --- Unzip everything --------------------------------------------------------
walk(list.files(homeDir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE),
     ~ unzip(.x, exdir = dirname(.x)))

shp_check <- list.files(homeDir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
message(sprintf("Extracted %d shapefile(s) for %s to %s.",
                length(shp_check), start_date, end_date))
if (length(shp_check) == 0) {
  stop("No HMS shapefiles on disk - downloads likely failed (check TLS/proxy) or dates outside HMS coverage.")
}




###########################
######## YEWA #############
###########################

# --- Settings ----------------------------------------------------------------
# homeDir MUST match the analysis script: homeDir <- "Smoke_polygons"
# (that script uses a relative path, so set your working directory to the YEWA
#  project root first, or make this an absolute path that points to the same folder)
homeDir <- "Smoke_polygons"

# Set to your actual YEWA 2025 recording span. Check with:
#   range(as.Date(YEWA$date))
start_date <- as.Date("2025-06-01")
end_date   <- as.Date("2025-06-20")

all_dates <- seq(start_date, end_date, by = "day")

# --- Download one zip per date -----------------------------------------------
# Existing valid zips are skipped, so re-running is cheap.
for (d in all_dates) {
  d         <- as.Date(d, origin = "1970-01-01")
  inFolder  <- file.path(year(d), sprintf("%02d", month(d)))
  fullDir   <- file.path(homeDir, inFolder)
  dir.create(fullDir, recursive = TRUE, showWarnings = FALSE)
  
  date_id   <- format(d, "%Y%m%d")
  file_name <- paste0("hms_smoke", date_id, ".zip")
  dest      <- file.path(fullDir, file_name)
  
  if (file.exists(dest) && file.info(dest)$size > 1000) next   # already have it
  
  url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
                inFolder, "/", file_name)
  ok <- tryCatch(
    { download.file(url, destfile = dest, mode = "wb", method = "libcurl", quiet = TRUE); TRUE },
    error = function(e) { message("  failed: ", file_name, " - ", conditionMessage(e)); FALSE }
  )
  if (ok && file.exists(dest) && file.info(dest)$size < 1000) file.remove(dest)
}

# --- Unzip everything --------------------------------------------------------
walk(list.files(homeDir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE),
     ~ unzip(.x, exdir = dirname(.x)))

shp_check <- list.files(homeDir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
message(sprintf("Extracted %d shapefile(s) for %s to %s.",
                length(shp_check), start_date, end_date))
if (length(shp_check) == 0) {
  stop("No HMS shapefiles on disk - downloads likely failed (check TLS/proxy) or dates outside HMS coverage.")
}
