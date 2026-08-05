# ---
# title: "2a_YEWA smoke data"
# author: "Leonard Patterson"
# created: "2026-08-01"
# description: This code takes hourly PM2.5 concentration from Alberta Air Warehouse assigns concentrations to each recording
#              as a means of identifying smoky/non-smoky days. This script then assigns each ARU recording the densest HMS smoke 
#              polygon overlapping its location during the recording hour and then assesses agreement between the two products at 
#              using Cohen's Kappa. This script does the above steps only for YEWA recordings from 2025.
# ---
# Clear environment
rm(list=ls())

library(tidyverse)    # wrangling
library(sf)           # spatial analysis

## Read in ARU locations
YEWA <- read.csv("Input/Tabular Data/YEWA_data_for_analysis_2026-07-28.csv") 

# Tab-delimited despite the .csv extension, with a variable-length preamble
# above the metadata block, so split manually and locate rows by content.
# Comma-delimited with quoted fields, a variable-length preamble above the
# metadata block, and four columns per station. Split manually, strip quotes,
# and locate rows by content rather than position.
file_path <- "Input/May_June_July_2025_smoke_data.csv"

lines <- read_lines(file_path)
n_col <- max(str_count(lines, ",")) + 1

PM25 <- str_split_fixed(lines, ",", n_col) |>
  as_tibble(.name_repair = ~ paste0("V", seq_along(.x))) |>
  mutate(across(everything(), ~ str_trim(str_remove_all(.x, '"'))))

first_col   <- PM25[[1]]
station_row <- which(str_detect(first_col, fixed("Station Name")))[1]
id_row      <- which(str_detect(first_col, fixed("Station Id")))[1]
lat_row     <- which(str_detect(first_col, fixed("Station Latitude")))[1]
lon_row     <- which(str_detect(first_col, fixed("Station Longitude")))[1]
header_row  <- which(first_col == "Interval Start")[1]

c(ncol(PM25), station_row, id_row, lat_row, lon_row, header_row)   # expect 372, 16, 17, 20, 21, 29
stopifnot(!is.na(header_row), ncol(PM25) > 100)

station_line <- as.character(PM25[station_row, ])
header_line  <- as.character(PM25[header_row, ])
start_cols   <- which(header_line == "Interval Start")
stations     <- station_line[start_cols + 1]

length(start_cols); head(stations)                 # expect 93 blocks

dat <- PM25[(header_row + 1):nrow(PM25), ]

# Raw timestamp strings, so the format is visible if parsing fails
head(setdiff(as.character(dat[[start_cols[1]]]), ""), 3)

# Handles "2025-05-20 0:00", "2025-05-20 00:00:00", and Excel serial numbers
parse_dt <- function(x) {
  x   <- str_trim(as.character(x))
  out <- suppressWarnings(ymd_hm(x, quiet = TRUE))
  
  i <- is.na(out) & x != ""
  out[i] <- suppressWarnings(ymd_hms(x[i], quiet = TRUE))
  
  i <- is.na(out) & x != "" & !is.na(suppressWarnings(as.numeric(x)))
  out[i] <- as.POSIXct(round(as.numeric(x[i]) * 86400), origin = "1899-12-30", tz = "UTC")
  
  round_date(out, "hour")
}

out <- map2_dfr(start_cols, stations, function(sc, st) {
  tibble(
    interval_start = as.character(dat[[sc]]),
    station        = st,
    value          = suppressWarnings(as.numeric(dat[[sc + 2]]))
  )
}) %>%
  filter(!is.na(interval_start), interval_start != "") %>%
  mutate(interval_start = parse_dt(interval_start)) %>%
  filter(!is.na(interval_start)) %>%
  group_by(interval_start, station) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(value = ifelse(is.nan(value), NA_real_, value)) %>%
  pivot_wider(names_from = station, values_from = value) %>%
  arrange(interval_start) %>%
  rename(`Interval Start` = interval_start)

range(out$`Interval Start`)      # must cover your 2025 recording dates

stations_meta <- tibble(
  station    = stations,
  station_id = as.character(PM25[id_row,  ])[start_cols + 1],
  latitude   = as.numeric(as.character(PM25[lat_row, ])[start_cols + 1]),
  longitude  = as.numeric(as.character(PM25[lon_row, ])[start_cols + 1])
) |>
  distinct(station, .keep_all = TRUE)

nrow(stations_meta)
stations_meta |> filter(is.na(latitude) | is.na(longitude))   # should be empty




### Find nearest PM2.5 station to each sites and calculate distance in m to each site

# Project site and monitoring stations dfs
sites_sf <- YEWA |>
  distinct(site, latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3400)

mon_sf <- stations_meta |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3400)

# Determine closest station to each site
nn <- st_nearest_feature(sites_sf, mon_sf)

site_station <- sites_sf |>
  st_drop_geometry() |>
  mutate(
    pm_station    = mon_sf$station[nn],
    pm_station_id = mon_sf$station_id[nn],
    dist_m        = as.numeric(st_distance(sites_sf, mon_sf[nn, ], by_element = TRUE)),
    dist_km       = dist_m / 1000
  )

# Join closest site and distance back to YEWA
YEWA <- YEWA |> left_join(site_station, by = "site")

# Summary of distances to PM2.5 stations
summary(site_station$dist_km)

# Summary of which stations are closest to site
count(site_station, pm_station, sort = TRUE)

# Filter PM2.5 stations to only include sites relevant to analysis; rename Interval Start column
out2 <- out %>%
  select(`Interval Start`, `Edmonton Lendrum`, `Edmonton McCauley`, `Edmonton East`, `Edmonton-Woodcroft`, `Enoch`, `St. Albert`) %>%
  rename(date_time = 'Interval Start') 

# Convert PM2.5 to long format
pm_long <- out2 |>
  pivot_longer(-date_time, names_to = "pm_station", values_to = "pm25") |>
  mutate(hkey = format(date_time, "%Y-%m-%d %H"))

# Station names must match across the two frames
setdiff(unique(YEWA$pm_station), unique(pm_long$pm_station))   # should be empty

# --- Join on site's station + recording hour ---------------------------------
YEWA <- YEWA |>
  mutate(hkey = paste(as.character(date), substr(time, 1, 2))) |>
  left_join(dplyr::select(pm_long, pm_station, hkey, pm25),
            by = c("pm_station", "hkey"))

# Confirm the keys look right before trusting the join
head(YEWA$hkey); head(pm_long$hkey)

# --- Checks ------------------------------------------------------------------
sum(is.na(YEWA$pm25))
YEWA |> filter(is.na(pm25)) |> count(pm_station, date)




### Three recordings have no PM2.5 values because the station was offline for a period. For just 
### these two site-days, find the next closest station and extract the PM2.5 from there.

# Calculate all site-to-station distances, reusing the projected objects from above
dmat <- st_distance(sites_sf, mon_sf)
site_station_all <- expand_grid(si = seq_len(nrow(sites_sf)),
                                mi = seq_len(nrow(mon_sf))) |>
  transmute(site       = sites_sf$site[si],
            pm_station = mon_sf$station[mi],
            fill_km    = as.numeric(dmat[cbind(si, mi)]) / 1000)

fill <- YEWA |>
  filter(is.na(pm25) | is.nan(pm25)) |>
  distinct(site, hkey) |>
  left_join(site_station_all, by = "site", relationship = "many-to-many") |>
  left_join(dplyr::select(pm_long, pm_station, hkey, pm25),
            by = c("pm_station", "hkey")) |>
  filter(!is.na(pm25), !is.nan(pm25)) |>
  group_by(site, hkey) |>
  slice_min(fill_km, n = 1, with_ties = FALSE) |>
  ungroup() |>
  dplyr::select(site, hkey, fill_pm25 = pm25, fill_station = pm_station, fill_km)

YEWA <- YEWA |>
  left_join(fill, by = c("site", "hkey")) |>
  mutate(
    pm25            = ifelse(is.nan(pm25), NA_real_, pm25),
    pm_fallback     = is.na(pm25) & !is.na(fill_pm25),
    pm_used_station = ifelse(pm_fallback, fill_station, pm_station),
    pm_used_km      = ifelse(pm_fallback, fill_km,      dist_km),
    pm25            = coalesce(pm25, fill_pm25)
  ) |>
  dplyr::select(-fill_pm25, -fill_station, -fill_km)

# Check
sum(is.na(YEWA$pm25))
YEWA |> filter(pm_fallback) |>
  dplyr::select(recording_name, pm_station, dist_km, pm_used_station, pm_used_km, pm25)

# Site-day means
YEWA_sd <- YEWA |>
  group_by(site) |>
  summarise(pm25       = mean(pm25, na.rm = TRUE),
            pm_used_km = mean(pm_used_km, na.rm = TRUE),
            n_rec      = n(), .groups = "drop")

count(YEWA_sd, n_rec)   # expect 48 and 48, all n_rec = 3

# Summary of distances, one value per site
YEWA |>
  group_by(site) |>
  summarise(km = mean(pm_used_km), .groups = "drop") |>
  pull(km) |> summary()

#Min.    1st Qu. Median  Mean    3rd Qu.  Max. 
#1.087   2.665   4.117   4.577   5.452     14.035


### Determine how many smoky day days there are and how many transcribed recordings
### at suitable times exist on each of those days

# Plot PM2.5 as a function of day so see where the smoky days are
base <- ggplot(YEWA, aes(x = date, y = pm25)) + 
  geom_point()

# See where the high-density smoke days are
YEWA_high_smoke <- YEWA %>%
  filter(pm25 > 70) 

# All smoky days are on the 10/11th of June. Filter to those dates
# and determine the number of recordings at each site on those days
YEWA_all_smoky_day_recordings <- YEWA %>%
  filter(date %in% c("2025-06-10", "2025-06-11"))
YEWA_all_smoky_day_recordings_2 <- YEWA_all_smoky_day_recordings %>%
  count(site, date) %>%
  pivot_wider(names_from = date, values_from = n, values_fill = 0) %>%
  mutate(total = rowSums(across(-site))) %>%
  arrange(desc(total))

# Confirm how many recordings on the 10/11th have been transcribed
YEWA_raw <- read.csv("Input/Tabular Data/YEWA_main_report.csv")
YEWA_n_recordings <- YEWA_raw %>%
  mutate(rec_dt   = as_datetime(recording_date_time),
         rec_date = as_date(rec_dt),
         rec_hour = hour(rec_dt)) %>%
  filter(rec_date %in% as_date(c("2025-06-10", "2025-06-11")),
         rec_hour >= 5 & rec_hour < 13) %>%
  distinct(location, recording_id, rec_dt, task_is_complete) %>%
  group_by(location) %>%
  summarise(
    n_recordings_comp    = n_distinct(recording_id[task_is_complete == "t"]),
    recording_times      = paste(sort(format(rec_dt[task_is_complete == "t"], "%Y-%m-%d %H:%M:%S")), collapse = ", "),
    .groups = "drop"
  )
write.csv(YEWA_n_recordings, "Output/Tabular Data/YEWA_smoky_day_recordings.csv") # For Alex







### =========================================================================
### HMS smoke polygon corroboration
### =========================================================================

homeDir     <- "Smoke_polygons"     # folder holding the unzipped HMS shapefiles
DENS_LEVELS <- c("Light", "Medium", "Heavy")
HMS_MAX_GAP <- 6                    # hours; flag fallbacks beyond this
TZ_LOCAL    <- "America/Edmonton"

max_dens <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  as.character(x[which.max(as.integer(x))])
}

YEWA <- YEWA |>
  mutate(date          = as.Date(date),
         recording_dt  = ymd_hms(paste(date, time), tz = TZ_LOCAL),
         recording_utc = with_tz(recording_dt, "UTC"),
         hour_start    = floor_date(recording_utc, "hour"),
         hour_end      = hour_start + hours(1))

shp_check <- list.files(homeDir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
stopifnot(length(shp_check) > 0)
print(names(st_drop_geometry(st_read(shp_check[1], quiet = TRUE))))   # need Density, Start, End


### --- 1. Assign density per recording -------------------------------------
# gap_hours = 0 when a polygon's imagery window overlaps the recording hour,
# otherwise the hours between them. Take the densest polygon at gap 0; if none,
# take the temporally closest polygon that date, densest breaking ties.

hms_assign <- function(current_date) {
  
  recs <- filter(YEWA, date == current_date)
  if (nrow(recs) == 0) return(NULL)
  
  base_out <- recs |> dplyr::select(recording_name)
  
  shp <- list.files(homeDir, pattern = format(current_date, "%Y%m%d"),
                    recursive = TRUE, full.names = TRUE)
  shp <- shp[grepl("\\.shp$", shp)]
  if (length(shp) == 0) {
    return(base_out |> mutate(density_hourly = NA_character_,
                              density_nearest = NA_character_, gap_hours = NA_real_))
  }
  
  poly <- st_read(shp[1], quiet = TRUE) |> st_make_valid()
  pts  <- st_as_sf(recs, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  j <- st_join(pts, poly, join = st_within) |>
    st_drop_geometry() |>
    mutate(
      dens        = factor(Density, levels = DENS_LEVELS, ordered = TRUE),
      smoke_start = as.POSIXct(Start, format = "%Y%j %H%M", tz = "UTC"),
      smoke_end   = as.POSIXct(End,   format = "%Y%j %H%M", tz = "UTC"),
      gap_hours = case_when(
        is.na(dens)                                     ~ NA_real_,
        smoke_start < hour_end & smoke_end > hour_start  ~ 0,
        smoke_start >= hour_end ~ as.numeric(difftime(smoke_start, hour_end,  units = "hours")),
        TRUE                    ~ as.numeric(difftime(hour_start,  smoke_end, units = "hours"))
      )
    ) |>
    filter(!is.na(dens))
  
  if (nrow(j) == 0) {
    return(base_out |> mutate(density_hourly = NA_character_,
                              density_nearest = NA_character_, gap_hours = NA_real_))
  }
  
  hourly <- j |> filter(gap_hours == 0) |>
    group_by(recording_name) |>
    summarise(density_hourly = max_dens(dens), .groups = "drop")
  
  nearest <- j |>
    group_by(recording_name) |>
    arrange(gap_hours, desc(as.integer(dens)), .by_group = TRUE) |>
    slice(1) |> ungroup() |>
    transmute(recording_name, density_nearest = as.character(dens), gap_hours)
  
  base_out |>
    left_join(hourly,  by = "recording_name") |>
    left_join(nearest, by = "recording_name")
}

hms_all <- sort(unique(YEWA$date)) |> map(hms_assign) |> list_rbind()

YEWA <- YEWA |>
  left_join(hms_all, by = "recording_name") |>
  mutate(
    hms_density  = factor(coalesce(density_hourly, density_nearest),
                          levels = DENS_LEVELS, ordered = TRUE),
    hms_gap      = if_else(!is.na(density_hourly), 0, gap_hours),
    hms_source   = case_when(!is.na(density_hourly)  ~ "direct overlap",
                             !is.na(density_nearest) ~ "nearest in time",
                             TRUE                    ~ "no polygon"),
    hms_far_flag = hms_source == "nearest in time" & hms_gap > HMS_MAX_GAP
  ) |>
  dplyr::select(-density_hourly, -density_nearest, -gap_hours)


### --- 2. How much came from the temporal fallback? ------------------------

count(YEWA, hms_source, hms_density)

YEWA |>
  summarise(n_total    = n(),
            n_direct   = sum(hms_source == "direct overlap"),
            n_fallback = sum(hms_source == "nearest in time"),
            pct_fallback = 100 * mean(hms_source == "nearest in time"),
            n_no_poly  = sum(hms_source == "no polygon"))

YEWA |> filter(hms_source == "nearest in time") |>
  summarise(n = n(), median_gap = median(hms_gap), mean_gap = mean(hms_gap),
            max_gap = max(hms_gap), n_over_6h = sum(hms_far_flag))

YEWA |> filter(hms_source == "nearest in time") |>
  mutate(gap_bin = cut(hms_gap, c(0, 1, 2, 3, 6, Inf),
                       labels = c("<=1h", "1-2h", "2-3h", "3-6h", ">6h"),
                       include.lowest = TRUE)) |>
  count(smoke_status, gap_bin) |>
  pivot_wider(names_from = gap_bin, values_from = n, values_fill = 0)


### --- 3. Aggregate to site x condition -------------------------------------

corrob <- YEWA |>
  group_by(site, smoke_status) |>
  summarise(hms_density = factor(max_dens(hms_density), levels = DENS_LEVELS, ordered = TRUE),
            pm25        = mean(pm25, na.rm = TRUE),
            n_fallback  = sum(hms_source == "nearest in time"),
            .groups = "drop") |>
  mutate(design = if_else(smoke_status == "smoky", "Smoky", "Non-smoky"))

nrow(corrob)                                  # expect 96
count(corrob, smoke_status, hms_density)

# PM2.5 by density class - reported for Light and Heavy
corrob |>
  group_by(hms_density) |>
  summarise(n = n(), median_pm = median(pm25),
            q25 = quantile(pm25, .25), q75 = quantile(pm25, .75),
            min = min(pm25), max = max(pm25), .groups = "drop")

ggplot(corrob, aes(hms_density, pm25)) +
  geom_boxplot(outlier.alpha = 0.4) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 1.4) +
  scale_y_log10() +
  labs(x = "HMS smoke density", y = expression(PM[2.5]~(mu*g/m^3))) +
  theme_classic(base_size = 13)

### --- 4. Agreement ---------------------------------------------------------

# Unambiguous classes only: does Light/Heavy sort the two conditions?
unamb <- corrob |> filter(hms_density %in% c("Light", "Heavy"))
tab_unamb <- table(Design = unamb$design,
                   HMS    = if_else(unamb$hms_density == "Heavy", "Smoky", "Non-smoky"))
tab_unamb
cat(sprintf("Unambiguous classes: %d of %d agree (%.1f%%)\n",
            sum(diag(tab_unamb)), sum(tab_unamb),
            100 * sum(diag(tab_unamb)) / sum(tab_unamb)))

# Full sample, Medium assigned both ways - conclusion shouldn't depend on it
agreement <- map_dfr(c("Smoky", "Non-smoky"), function(m) {
  cb  <- corrob |>
    mutate(hms_binary = case_when(hms_density == "Heavy"  ~ "Smoky",
                                  hms_density == "Medium" ~ m,
                                  TRUE                    ~ "Non-smoky"))
  tab <- table(Design = cb$design, HMS = cb$hms_binary)
  tibble(medium_as = m,
         n         = sum(tab),
         n_agree   = sum(diag(tab)),
         pct_agree = 100 * sum(diag(tab)) / sum(tab),
         kappa     = irr::kappa2(cbind(cb$design, cb$hms_binary))$value)
})
agreement

#medium_as     n   n_agree pct_agree kappa
#1 Smoky      96      91      94.8   0.896
#2 Non-smoky  96      89      92.7   0.854

# Disagreements under each rule
walk(c("Smoky", "Non-smoky"), function(m) {
  cat("\n--- Medium as", m, "---\n")
  corrob |>
    mutate(hms_binary = case_when(hms_density == "Heavy"  ~ "Smoky",
                                  hms_density == "Medium" ~ m,
                                  TRUE                    ~ "Non-smoky")) |>
    filter(design != hms_binary) |>
    dplyr::select(site, design, hms_density, pm25) |>
    arrange(site) |> print(n = Inf)
})

