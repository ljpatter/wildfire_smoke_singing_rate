library(dplyr)
library(readr)
library(lubridate)

tasks <- read_csv("C:/Users/leona/Downloads/leonard_tasks.csv",
                  show_col_types = FALSE,
                  col_types = cols(recording_date_time = col_character()))

coe <- read_csv("C:/Users/leona/Downloads/COE_Recordings_2026-08-20/City_of_Edmonton_recordings_20260821_021023UTC.csv",
                show_col_types = FALSE,
                col_types = cols(recording_date_time = col_character()))

# Parse WITHOUT a timezone: avoids DST-gap NAs entirely; wall-clock is all we need
parse_dt <- function(x) {
  out <- ymd_hms(x, quiet = TRUE)   # "2025-08-12 15:00:00"
  need <- is.na(out)
  out[need] <- ymd_hm(x[need], quiet = TRUE)  # "2026-08-03 9:00" fallback
  out
}

tasks <- tasks %>%
  mutate(dt = parse_dt(recording_date_time),
         key = paste(location, format(dt, "%Y-%m-%d %H:%M")))

coe <- coe %>%
  mutate(dt = parse_dt(recording_date_time),
         key = paste(location, format(dt, "%Y-%m-%d %H:%M")))

cat("Unparsed task datetimes:", sum(is.na(tasks$dt)), "/n")
cat("Unparsed COE datetimes: ", sum(is.na(coe$dt)), "/n/n")

# ═══════════════════════════════════════════════════════════════
# PART 1: Which recordings in leonard_tasks are NOT available in COE?
# ═══════════════════════════════════════════════════════════════
existing_keys <- coe$key

tasks_checked <- tasks %>% mutate(available = key %in% existing_keys)
unavailable   <- tasks_checked %>% filter(!available)

cat("Total tasks:", nrow(tasks_checked),
    "| Available:", sum(tasks_checked$available),
    "| NOT available:", nrow(unavailable), "/n/n")

cat("── Recordings in leonard_tasks NOT available in COE ──/n")
print(unavailable %>% select(location, recording_date_time), n = Inf)

write_csv(unavailable %>% select(location, recording_date_time),
          "Output/Tabular Data/leonard_tasks_unavailable.csv")

# ═══════════════════════════════════════════════════════════════
# PART 2: For every site in leonard_tasks, the 2025 recording date range in COE
# ═══════════════════════════════════════════════════════════════
task_sites <- unique(tasks$location)

site_ranges_2025 <- coe %>%
  filter(location %in% task_sites, year(dt) == 2025) %>%
  group_by(location) %>%
  summarise(
    first_recording = min(dt, na.rm = TRUE),
    last_recording  = max(dt, na.rm = TRUE),
    n_recordings    = n(),
    .groups = "drop"
  )

# Keep task sites with no 2025 recordings visible rather than dropping them
missing_sites <- setdiff(task_sites, site_ranges_2025$location)
if (length(missing_sites) > 0) {
  site_ranges_2025 <- bind_rows(
    site_ranges_2025,
    tibble(location = missing_sites,
           first_recording = as.POSIXct(NA, tz = "America/Edmonton"),
           last_recording  = as.POSIXct(NA, tz = "America/Edmonton"),
           n_recordings = 0L)
  ) %>% arrange(location)
}

cat("/n── 2025 recording date range per task site (COE file) ──/n")
print(site_ranges_2025, n = Inf)

write_csv(site_ranges_2025,
          "Output/Tabular Data/COE_YEWA_recording_ranges.csv")



### Remove unavailable tasks from leonard_tasks and save the remaining tasks to a new CSV file
available_tasks <- tasks %>%
  filter(key %in% coe$key) %>%
  select(location, recording_date_time, task_method, recording_sample_frequency,
         task_duration, task_is_complete, observer, task_comments, task_id)

cat("Original tasks:", nrow(tasks),
    "| Kept (available):", nrow(available_tasks),
    "| Removed:", nrow(tasks) - nrow(available_tasks), "/n")

write_csv(available_tasks,
          "Output/Tabular Data/leonard_tasks_available.csv",
          na = "")














# Load data
rm(list =ls())

dat1 <- read.csv("C:/Users/leona/OneDrive/Desktop/YEWA_1st take.csv") %>%
  filter(observer == "Leonard Patterson")
dat2 <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP smoke/Output/Tabular Data/COE_tasks_to_add.csv")

head(dat1)
head(dat2)

# Normalize datetimes to a common format
dat1$dt <- format(as.POSIXct(dat1$recording_date_time, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M")
dat2$dt <- format(as.POSIXct(dat2$recording_date_time, format = "%Y-%m-%d %H:%M"),    "%Y-%m-%d %H:%M")

# Rows in dat2 whose location + datetime don't appear in dat1
missing <- dat2 %>%
  anti_join(dat1, by = c("location", "dt"))

missing$dt <- NULL   # drop helper column
write.csv(missing, "Output/Tabular Data/YEWA_round_2.csv"
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
# ---
# title: "YEWA PM2.5 for target smoke dates"
# author: "Leonard Patterson"
# description: Reads YEWA recording tasks + location coordinates, assigns each
#              recording the PM2.5 from its nearest Alberta Air Warehouse station
#              for the recording hour, restricted to June 5/6/10/11/14/15/16 2025,
#              then plots PM2.5 by day.
# ---
rm(list = ls())

library(tidyverse)    # wrangling
library(sf)           # spatial analysis

## ---- Read recording tasks and location coordinates -------------------------
tasks_path <- "C:/Users/leona/Downloads/Yellow_Warbler_vocal_activity_across_wildfire_smoke_exposure_gradients_in_Edmonton_Alberta_Tasks_2026-08-25 (1).csv"
locs_path  <- "C:/Users/leona/Downloads/Yellow_Warbler_vocal_activity_across_wildfire_smoke_exposure_gradients_in_Edmonton_Alberta_Locations_2026-08-25.csv"

target_dates <- as.Date(c("2025-06-05", "2025-06-06", "2025-06-10",
                          "2025-06-11", "2025-06-14", "2025-06-15", "2025-06-16"))

tasks <- read.csv(tasks_path, stringsAsFactors = FALSE) |>
  mutate(
    location          = str_trim(location),
    recording_date_time = ymd_hms(str_trim(recording_date_time)),
    date              = as.Date(recording_date_time),
    time              = format(recording_date_time, "%H:%M:%S")
  ) |>
  rename(site = location) |>
  filter(date %in% target_dates)

locs <- read.csv(locs_path, stringsAsFactors = FALSE) |>
  mutate(site = str_trim(location)) |>
  distinct(site, latitude, longitude)

# Attach coordinates to each recording
YEWA <- tasks |> left_join(locs, by = "site")

# Confirm every recording has coordinates
stopifnot(sum(is.na(YEWA$latitude) | is.na(YEWA$longitude)) == 0)
cat(nrow(YEWA), "recordings across", length(unique(YEWA$site)), "sites/n")

## ---- Read and reshape PM2.5 station data -----------------------------------
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

stopifnot(!is.na(header_row), ncol(PM25) > 100)

station_line <- as.character(PM25[station_row, ])
header_line  <- as.character(PM25[header_row, ])
start_cols   <- which(header_line == "Interval Start")
stations     <- station_line[start_cols + 1]

dat <- PM25[(header_row + 1):nrow(PM25), ]

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

range(out$`Interval Start`)      # must cover the target recording dates

stations_meta <- tibble(
  station    = stations,
  station_id = as.character(PM25[id_row,  ])[start_cols + 1],
  latitude   = as.numeric(as.character(PM25[lat_row, ])[start_cols + 1]),
  longitude  = as.numeric(as.character(PM25[lon_row, ])[start_cols + 1])
) |>
  distinct(station, .keep_all = TRUE)

stations_meta |> filter(is.na(latitude) | is.na(longitude))   # should be empty

## ---- Nearest PM2.5 station to each site ------------------------------------
sites_sf <- YEWA |>
  distinct(site, latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3400)

mon_sf <- stations_meta |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3400)

nn <- st_nearest_feature(sites_sf, mon_sf)

site_station <- sites_sf |>
  st_drop_geometry() |>
  mutate(
    pm_station    = mon_sf$station[nn],
    pm_station_id = mon_sf$station_id[nn],
    dist_m        = as.numeric(st_distance(sites_sf, mon_sf[nn, ], by_element = TRUE)),
    dist_km       = dist_m / 1000
  )

YEWA <- YEWA |> left_join(site_station, by = "site")

summary(site_station$dist_km)
count(site_station, pm_station, sort = TRUE)

## ---- Join PM2.5 on nearest station + recording hour ------------------------
pm_long <- out |>
  rename(date_time = `Interval Start`) |>
  pivot_longer(-date_time, names_to = "pm_station", values_to = "pm25") |>
  mutate(hkey = format(date_time, "%Y-%m-%d %H"))

# Any nearest station that isn't present in the PM2.5 frame (should be empty)
setdiff(unique(YEWA$pm_station), unique(pm_long$pm_station))

YEWA <- YEWA |>
  mutate(hkey = paste(as.character(date), substr(time, 1, 2))) |>
  left_join(dplyr::select(pm_long, pm_station, hkey, pm25),
            by = c("pm_station", "hkey"))

# Checks
sum(is.na(YEWA$pm25))
YEWA |> filter(is.na(pm25)) |> count(pm_station, date)

## ---- Fallback to next-nearest station where PM2.5 is missing ---------------
# (station offline for part of the hour). Reuses the projected objects above.
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

sum(is.na(YEWA$pm25))   # remaining unmatched recordings, if any

## ---- Plot PM2.5 by day -----------------------------------------------------
ggplot(YEWA, aes(x = as.Date(date), y = pm25)) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.4, size = 1.6) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5,
               colour = "firebrick", linewidth = 0.4) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  labs(x = "Date", y = expression(PM[2.5]~(mu*g/m^3)),
       title = "YEWA recording PM2.5 by day (target smoke dates, 2025)") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






library(dplyr)
library(lubridate)

df <- read.csv(
  "C:/Users/leona/Downloads/Yellow_Warbler_vocal_activity_across_wildfire_smoke_exposure_gradients_in_Edmonton_Alberta_Tasks_2026-08-25 (2).csv",
  stringsAsFactors = FALSE
)

df$location <- trimws(df$location)
df$recording_date <- as_date(ymd_hms(trimws(df$recording_date_time)))

result <- df %>%
  group_by(location) %>%
  summarise(latest = max(recording_date), .groups = "drop") %>%
  filter(latest == as_date("2025-06-10"))

cat(nrow(result), "location(s) with latest recording day of 2025-06-10:/n")
print(result$location)







library(dplyr)
##### Check which site still need more recordings ###
dat1 <- read.csv("C:/Users/leona/Downloads/Yellow_Warbler_vocal_activity_across_wildfire_smoke_exposure_gradients_in_Edmonton_Alberta_Tasks_2026-08-27 (5).csv")

dat2 <- dat1 %>%
  mutate(
    recording_dt   = as.POSIXct(recording_date_time, format = "%Y-%m-%d %H:%M:%S"),
    recording_date = as.Date(recording_dt),
    hour           = as.numeric(format(recording_dt, "%H"))
  ) %>%
  filter(
    recording_date %in% as.Date(c(
      "2025-06-05", "2025-06-06", "2025-06-07",
      "2025-06-10", "2025-06-11",
      "2025-06-14", "2025-06-15", "2025-06-16"
    )),
    hour >= 4 & hour <= 13    # 4 am up to 1 pm (13:00 excluded)
  ) %>%
  mutate(
    smoke_period = case_when(
      recording_date %in% as.Date(c("2025-06-05", "2025-06-06", "2025-06-07")) ~ "before non-smoky",
      recording_date %in% as.Date(c("2025-06-10", "2025-06-11"))               ~ "smoky",
      recording_date %in% as.Date(c("2025-06-14", "2025-06-15", "2025-06-16")) ~ "after non-smoky"
    ),
    smoke_period = factor(smoke_period,
                          levels = c("before non-smoky", "smoky", "after non-smoky"))
  )

# Count recordings per period
period_counts <- dat2 %>%
  count(smoke_period, name = "n_recordings")

period_counts


site_period_counts <- dat2 %>%
  count(location, smoke_period, name = "n_recordings") %>%
  tidyr::pivot_wider(names_from = smoke_period, values_from = n_recordings, values_fill = 0)

# Remove sites that have no YEWA
site_period_counts_2 <- site_period_counts %>%
  filter(!(location %in% c("EDALA02KER2", "EDALB01CAP1", "EDALC02ZOO1", "EDALC03JAM1")))
