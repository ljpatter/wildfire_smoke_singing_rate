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

# Load packages
library(tidyverse)    # wrangling
library(sf)           # spatial analysis

## Read in ARU locations
YEWA <- read.csv("Input/Tabular Data/YEWA_data_for_analysis_2026-09-29.csv") 

# Tab-delimited despite the .csv extension, with a variable-length preamble
# above the metadata block, so split manually and locate rows by content.
# Comma-delimited with quoted fields, a variable-length preamble above the
# metadata block, and four columns per station. Split manually, strip quotes,
# and locate rows by content rather than position.
file_path <- "Input/Tabular Data/May_June_July_2025_smoke_data.csv"

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

# Summary of values by treatment, one value per site-day
YEWA_PM25 <- YEWA %>%
  group_by(smoke_status) %>%
  summarise(n_sites = n(),
            median_pm = median(pm25),
            q25 = quantile(pm25, 0.25), q75 = quantile(pm25, 0.75),
            min = min(pm25), max = max(pm25),
            .groups = "drop")

# A tibble: 2 × 7
#smoke_status n_sites median_pm   q25   q75   min   max
#1 non-smoky         81      10.7  6.18  16.2  1.11  28.4
#2 smoky             81      90.7 67.0  122.  39.2  193. 

# Summary of distances, one value per site
YEWA |>
  group_by(site) |>
  summarise(km = mean(pm_used_km), .groups = "drop") |>
  pull(km) |> summary()

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.004   2.992   4.312   4.642   5.442  13.959


### Determine how many smoky day days there are and how many transcribed recordings
### at suitable times exist on each of those days

# Plot PM2.5 as a function of day so see where the smoky days are
base <- ggplot(YEWA, aes(x = as.Date(date), y = pm25)) + 
  geom_point() +
  scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
  labs(x = "Date", y = "PM2.5") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# See where the high-density smoke days are
YEWA_high_smoke <- YEWA %>%
  filter(pm25 > 70) 

# Save
write.csv(YEWA, "Output/Tabular Data/YEWA_with_smoke.csv")
