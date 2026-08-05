# =============================================================================
# Smoke & birds: join ARU recordings to HMS smoke density + PM2.5
# Flat script — set `proj` and `spp` below, run top to bottom, repeat per project.
# =============================================================================

library(AirMonitor)   # PM2.5 data
library(lubridate)    # date-times
library(sf)           # spatial data
library(tidyverse)    # data wrangling
library(wildrtrax)    # WildTrax


# --- Settings ----------------------------------------------------------------
# Set these two, run the whole script, then change and re-run for the next project.
# proj 3778 -> "YEWA"   |   proj 2203 -> "WTSP"
proj    <- 2203
spp     <- "WTSP"
projDir <- "C:/Users/leona/OneDrive/Desktop/WTSP smoke"
homeDir <- file.path(projDir, "Smoke_polygons")


# --- Authenticate with WildTrax ----------------------------------------------
config <- "Scripts/login.R"
source(config)
wt_auth()


# --- Download report & filter to target species ------------------------------
message("Downloading WildTrax data...")
selected_project <- wt_download_report(proj, "ARU", "main")

# Keep target species + NONE (no-detection / blank tags)
selected_project <- selected_project |>
  filter(species_code %in% c(spp, "NONE"))

# FHP sites are missing their coords. Join them in manually (fill only the gaps).
FHP <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP smoke/Input/BU_locations/BU_Feral_Horse_Project_2023_location_report.csv") %>%
  select(location, location_id, latitude, longitude)

selected_project <- selected_project |>
  left_join(FHP, by = "location", suffix = c("", "_fhp")) |>
  mutate(
    latitude  = coalesce(latitude,  latitude_fhp),
    longitude = coalesce(longitude, longitude_fhp)
  ) |>
  select(-latitude_fhp, -longitude_fhp, -location_id_fhp)

# Save file (will need later for analysis code)
selected_project_2 <- selected_project %>%
  distinct(location, latitude, longitude)
write.csv(selected_project_2, "Output/Tabular Data/latlong_to_join.csv")

# Study period comes from the report's own date range
start_date <- as.Date(min(selected_project$recording_date_time))
end_date   <- as.Date(max(selected_project$recording_date_time))


# --- Build the list of dates to download -------------------------------------
inDatesYMDseq <- seq(start_date, end_date, by = "day")

inDates <- tibble(date = inDatesYMDseq) |>
  mutate(
    Year  = as.integer(format(date, "%Y")),
    Month = as.integer(format(date, "%m")),
    Day   = as.integer(format(date, "%d"))
  )


# --- Download HMS smoke shapefiles, one zip per date -------------------------
for (i in seq_len(nrow(inDates))) {
  
  inFolder  <- file.path(inDates$Year[i], sprintf("%02d", inDates$Month[i]))
  fullDir   <- file.path(homeDir, inFolder)
  dir.create(fullDir, recursive = TRUE, showWarnings = FALSE)
  
  date_id   <- sprintf("%04d%02d%02d", inDates$Year[i], inDates$Month[i], inDates$Day[i])
  file_name <- paste0("hms_smoke", date_id, ".zip")
  url       <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
                      inFolder, "/", file_name)
  dest      <- file.path(fullDir, file_name)
  
  # Download with libcurl; report (don't silence) failures
  ok <- tryCatch(
    { download.file(url, destfile = dest, mode = "wb", method = "libcurl", quiet = TRUE); TRUE },
    error = function(e) { message("  failed: ", file_name, " — ", conditionMessage(e)); FALSE }
  )
  
  # Drop empty / HTML-error files so unzip doesn't choke later
  if (ok && file.exists(dest) && file.info(dest)$size < 1000) {
    message("  suspicious (", file.info(dest)$size, " bytes), removing: ", file_name)
    file.remove(dest)
  }
}


# --- Unzip everything --------------------------------------------------------
zip_files <- list.files(homeDir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
walk(zip_files, ~ unzip(.x, exdir = dirname(.x)))

# Sanity check: did any shapefiles actually land?
shp_check <- list.files(homeDir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
message(sprintf("Downloaded %d zip(s), extracted %d shapefile(s).", length(zip_files), length(shp_check)))
if (length(shp_check) == 0) {
  stop("No HMS shapefiles on disk for ", start_date, " to ", end_date,
       " — downloads likely failed (check TLS/proxy) or dates are outside HMS coverage.")
}


# --- Pick densest overlapping polygon, flag temporal window ------------------
all_joined_data <- joined_raw |>
  mutate(Density = factor(Density, levels = c("Light", "Medium", "Heavy"), ordered = TRUE)) |>
  group_by(location, location_id, latitude, longitude,
           recording_date_time, call_count, duration_min, var, date) |>
  slice_max(Density, n = 1, with_ties = FALSE) |>
  ungroup()

all_joined_data <- all_joined_data |>
  mutate(
    smoke_start         = as.POSIXct(Start, format = "%Y%j %H%M", tz = "UTC"),
    smoke_end           = as.POSIXct(End,   format = "%Y%j %H%M", tz = "UTC"),
    recording_utc       = with_tz(recording_date_time, "UTC"),
    within_smoke_window = recording_utc >= smoke_start & recording_utc <= smoke_end
  ) |>
  dplyr::select(location, location_id, latitude, longitude,
         recording_date_time, call_count, duration_min, var,
         Density, Start, End, smoke_start, smoke_end,
         within_smoke_window, date) |>
  distinct()


# --- Load PM2.5 monitors for the study year & period -------------------------
cf <- monitor_loadAnnual(unique(year(all_joined_data$recording_date_time))) |>
  monitor_filterDate(
    startdate = as.numeric(format(start_date, "%Y%m%d")),
    enddate   = as.numeric(format(end_date,   "%Y%m%d")),
    timezone  = "MST"
  ) |>
  monitor_dropEmpty()


# --- Area-average PM2.5 per location (60 km radius around each site) ----------
loc_coords <- all_joined_data |>
  dplyr::select(location, latitude, longitude) |>
  distinct()

get_location_pm <- function(location, latitude, longitude) {
  
  area <- tryCatch(
    monitor_filterByDistance(cf, longitude = longitude, latitude = latitude, radius = 60000),
    error = function(e) NULL
  )
  
  # No monitor within 60 km -> empty series (location gets NA after join)
  if (is.null(area) || nrow(area$meta) == 0) {
    return(tibble(location = location, datetime = as.POSIXct(NA), pm25 = NA_real_)[0, ])
  }
  
  area |>
    monitor_collapse(deviceID = "area") |>
    monitor_getData() |>
    rename(pm25 = 2) |>
    arrange(datetime) |>
    mutate(pm25 = zoo::na.approx(pm25, x = datetime, na.rm = FALSE, maxgap = 3)) |>
    mutate(location = location)
}

pm_by_location <- loc_coords |>
  pmap(get_location_pm) |>
  list_rbind()


# --- Join PM2.5 to recordings by location + hour -----------------------------
joined_to_pm <- all_joined_data |>
  mutate(recording_hour = floor_date(recording_date_time, "hour")) |>
  left_join(pm_by_location, by = c("location", "recording_hour" = "datetime"))


# --- Result ------------------------------------------------------------------
# `joined_to_pm` = one row per recording: VAR, smoke density, temporal flag, PM2.5.
# To run the other project: set proj <- 2203 / spp <- "WTSP" up top and re-run.






### Check if HMS smoke polygons agree with PM2.5 data

# --- Define expected density per date ---------------------------------------
expected <- tribble(
  ~date,          ~expected,
  "2023-05-15",   "Light",
  "2023-05-24",   "Light",
  "2023-06-09",   "Light",
  "2023-06-13",   "Light",
  "2023-06-11",   "Medium_or_Heavy",
  "2023-05-21",   "Medium_or_Heavy"
) |>
  mutate(date = as.Date(date))

# --- Tag each recording as match / mismatch ---------------------------------
density_check <- joined_to_pm |>
  mutate(date = as.Date(recording_date_time)) |>
  inner_join(expected, by = "date") |>
  mutate(
    density_chr = as.character(Density),
    matches = case_when(
      expected == "Light"           & density_chr == "Light"                     ~ TRUE,
      expected == "Medium_or_Heavy" & density_chr %in% c("Medium", "Heavy")      ~ TRUE,
      TRUE                                                                        ~ FALSE
    )
  )

# --- Summary: how many match vs mismatch per date ---------------------------
density_check |>
  group_by(date, expected) |>
  summarise(
    n            = n(),
    n_match      = sum(matches),
    n_mismatch   = sum(!matches),
    densities_seen = paste(sort(unique(density_chr)), collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(date) |>
  print(n = Inf)

# --- Flag which expected dates are missing from the data entirely ------------
missing_dates <- setdiff(as.character(expected$date),
                         as.character(as.Date(joined_to_pm$recording_date_time)))
if (length(missing_dates) > 0) {
  message("Expected dates with NO recordings in the data: ",
          paste(missing_dates, collapse = ", "))
}

# --- List the actual mismatched rows so you can inspect them -----------------
density_check |>
  filter(!matches) |>
  dplyr::select(location, recording_date_time, date, expected, Density) |>
  arrange(date, location) |>
  print(n = Inf)



















# =============================================================================
# Hourly HMS smoke panel: one row per site per hour, May 1 - Jun 30 2023
# Two projects, kept separate:
#   YEWA (3778) -> main report uploaded manually, read from disk
#   WTSP (2203) -> pulled from WildTrax
# =============================================================================




# --- Settings ----------------------------------------------------------------
projDir <- getwd()                                   # a folder you can write to
homeDir <- file.path(projDir, "Smoke_Polygons")      # HMS shapefiles download here

start_date <- as.Date("2023-05-01")
end_date   <- as.Date("2023-06-30")

# Path to the manually-uploaded YEWA (3778) main report.
# Assumes columns: location, location_id, latitude, longitude (see guards below).
yewa_report_path <- "C:/Users/leona/OneDrive/Desktop/WTSP smoke/Input/COE_Yellow_Warbler_vocal_activity_across_wildfire_smoke_exposure_gradients_in_Edmonton_Alberta_main_report.csv"           # <- set to your file's path


# --- Authenticate with WildTrax (needed for WTSP only) -----------------------
config <- "Scripts/login.R"
source(config)
wt_auth()


# --- Download HMS shapefiles for the full date range -------------------------
# Existing valid zips are skipped, so re-running is cheap.
all_dates <- seq(start_date, end_date, by = "day")

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
  stop("No HMS shapefiles on disk - downloads likely failed (check TLS/proxy).")
}


# --- Site coordinates: one source each ---------------------------------------
# Reused cleaning for both sources.
clean_sites <- function(df) {
  df |>
    dplyr::select(location, location_id, latitude, longitude) |>
    mutate(across(c(latitude, longitude), as.numeric)) |>   # guard: coords may read as chr
    filter(!is.na(latitude), !is.na(longitude)) |>
    distinct()
}

get_sites_from_file <- function(path) {
  read_csv(path, show_col_types = FALSE) |> clean_sites()
}

get_sites_from_wt <- function(proj) {
  wt_download_report(proj, "ARU", "main") |> clean_sites()
}


# --- Build the hourly smoke panel for one project ----------------------------
Density_levels <- c("Light", "Medium", "Heavy")

build_smoke_panel <- function(sites) {
  
  points_sf <- st_as_sf(sites, coords = c("longitude", "latitude"),
                        crs = 4326, remove = FALSE)
  
  # For each day: spatially join sites to that day's polygons, keep the windows
  day_join <- function(current_date) {
    smokes <- list.files(homeDir, pattern = format(current_date, "%Y%m%d"),
                         recursive = TRUE, full.names = TRUE)
    shp <- smokes[grepl("\\.shp$", smokes)]
    if (length(shp) == 0) return(NULL)
    
    shape_data <- st_read(shp[1], quiet = TRUE) |> st_make_valid()
    
    st_join(points_sf, shape_data, join = st_within) |>
      st_drop_geometry() |>
      filter(!is.na(Density)) |>                    # keep only sites under a polygon
      transmute(
        location, location_id, latitude, longitude,
        Density,
        smoke_start = as.POSIXct(Start, format = "%Y%j %H%M", tz = "UTC"),
        smoke_end   = as.POSIXct(End,   format = "%Y%j %H%M", tz = "UTC")
      )
  }
  
  polygons_by_site <- all_dates |>
    map(~ day_join(as.Date(.x, origin = "1970-01-01"))) |>
    list_rbind()
  
  # Full hourly grid: every site x every hour in the range (UTC)
  hour_grid <- expand_grid(
    sites,
    hour_utc = seq(as.POSIXct(paste0(start_date, " 00:00"), tz = "UTC"),
                   as.POSIXct(paste0(end_date,   " 23:00"), tz = "UTC"),
                   by = "hour")
  )
  
  # Each site-hour gets the densest polygon whose window contains that hour;
  # uncovered site-hours become "No Smoke".
  hour_grid |>
    left_join(polygons_by_site,
              by = c("location", "location_id", "latitude", "longitude"),
              relationship = "many-to-many") |>
    filter(is.na(Density) | (hour_utc >= smoke_start & hour_utc <= smoke_end)) |>
    mutate(Density = factor(Density, levels = Density_levels, ordered = TRUE)) |>
    group_by(location, location_id, latitude, longitude, hour_utc) |>
    slice_max(Density, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      Density    = as.character(Density),
      Density    = replace_na(Density, "No Smoke"),
      hour_local = with_tz(hour_utc, "America/Edmonton")   # Mountain time convenience
    ) |>
    dplyr::select(location, location_id, latitude, longitude,
           hour_utc, hour_local, Density) |>
    arrange(location, hour_utc)
}


# --- Run per project, keep separate ------------------------------------------
sites_3778 <- get_sites_from_file(yewa_report_path)   # YEWA, uploaded manually
sites_2203 <- get_sites_from_wt(2203)                 # WTSP, from WildTrax

smoke_panel_3778 <- build_smoke_panel(sites_3778)     # hourly panel, YEWA sites
smoke_panel_2203 <- build_smoke_panel(sites_2203)     # hourly panel, WTSP sites


# --- Result ------------------------------------------------------------------
# smoke_panel_3778 / smoke_panel_2203: one row per site per hour, with the HMS
# smoke Density ("Light"/"Medium"/"Heavy"/"No Smoke") for that site-hour.





# =============================================================================
# Hourly HMS smoke panel: one row per site per hour, May 1 - Jun 30 2023
# Single project, sites supplied directly as the FHP data frame.
# =============================================================================

library(lubridate)   # date-times
library(sf)          # spatial data
library(tidyverse)   # data wrangling


# --- Settings ----------------------------------------------------------------
projDir <- getwd()                                   # a folder you can write to
homeDir <- file.path(projDir, "Smoke_Polygons")      # HMS shapefiles download here

start_date <- as.Date("2023-06-06")
end_date   <- as.Date("2023-06-20")


# --- Download HMS shapefiles for the full date range -------------------------
# Existing valid zips are skipped, so re-running is cheap.
all_dates <- seq(start_date, end_date, by = "day")

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
  stop("No HMS shapefiles on disk - downloads likely failed (check TLS/proxy).")
}


# --- Site coordinates --------------------------------------------------------
clean_sites <- function(df) {
  df |>
    dplyr::select(location, location_id, latitude, longitude) |>
    mutate(across(c(latitude, longitude), as.numeric)) |>   # guard: coords may read as chr
    filter(!is.na(latitude), !is.na(longitude)) |>
    distinct()
}


# --- Build the hourly smoke panel --------------------------------------------
Density_levels <- c("Light", "Medium", "Heavy")

build_smoke_panel <- function(sites) {
  
  points_sf <- st_as_sf(sites, coords = c("longitude", "latitude"),
                        crs = 4326, remove = FALSE)
  
  # For each day: spatially join sites to that day's polygons, keep the windows
  day_join <- function(current_date) {
    smokes <- list.files(homeDir, pattern = format(current_date, "%Y%m%d"),
                         recursive = TRUE, full.names = TRUE)
    shp <- smokes[grepl("\\.shp$", smokes)]
    if (length(shp) == 0) return(NULL)
    
    shape_data <- st_read(shp[1], quiet = TRUE) |> st_make_valid()
    
    st_join(points_sf, shape_data, join = st_within) |>
      st_drop_geometry() |>
      filter(!is.na(Density)) |>                    # keep only sites under a polygon
      transmute(
        location, location_id, latitude, longitude,
        Density,
        smoke_start = as.POSIXct(Start, format = "%Y%j %H%M", tz = "UTC"),
        smoke_end   = as.POSIXct(End,   format = "%Y%j %H%M", tz = "UTC")
      )
  }
  
  polygons_by_site <- all_dates |>
    map(~ day_join(as.Date(.x, origin = "1970-01-01"))) |>
    list_rbind()
  
  # Full hourly grid: every site x every hour in the range (UTC)
  hour_grid <- expand_grid(
    sites,
    hour_utc = seq(as.POSIXct(paste0(start_date, " 00:00"), tz = "UTC"),
                   as.POSIXct(paste0(end_date,   " 23:00"), tz = "UTC"),
                   by = "hour")
  )
  
  # Each site-hour gets the densest polygon whose window contains that hour;
  # uncovered site-hours become "No Smoke".
  hour_grid |>
    left_join(polygons_by_site,
              by = c("location", "location_id", "latitude", "longitude"),
              relationship = "many-to-many") |>
    filter(is.na(Density) | (hour_utc >= smoke_start & hour_utc <= smoke_end)) |>
    mutate(Density = factor(Density, levels = Density_levels, ordered = TRUE)) |>
    group_by(location, location_id, latitude, longitude, hour_utc) |>
    slice_max(Density, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      Density    = as.character(Density),
      Density    = replace_na(Density, "No Smoke"),
      hour_local = with_tz(hour_utc, "America/Edmonton")   # Mountain time convenience
    ) |>
    dplyr::select(location, location_id, latitude, longitude,
                  hour_utc, hour_local, Density) |>
    arrange(location, hour_utc)
}


# --- Run ---------------------------------------------------------------------
sites_FHP       <- clean_sites(FHP)
smoke_panel_FHP <- build_smoke_panel(sites_FHP)
