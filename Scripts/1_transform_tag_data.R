# ---
# title: "Transform tag data"
# author: "Leonard Patterson"
# created: "2026-08-01"
# description: This code downloads point count data from WildTrax and manipulate tag data prior to analysis.
# ---

# Clear environment
rm(list=ls())

# Load packages
library(dplyr)
library(hms)
library(tidyr)


##################################
############# WTSP ###############
##################################

# Load main report 
WTSP <- read.csv("Input/Tabular Data/WTSP_main_report_2026-09-29.csv")

# Select relevant columns only
WTSP1 <- WTSP %>%
  select(location,latitude,longitude,recording_date_time,species_code,task_is_complete,observer,max_noise_volume,max_noise_type,individual_order,abundance,vocalization)

# Combine columns 1 and 2 to create recording name file
WTSP1$recording_name <- paste(WTSP1$location, WTSP1$recording_date_time, sep = " ")

# Remove all tags that are calls
WTSP2 <- WTSP1 %>%
  filter(vocalization != "Call")

# Call "NONE" tags have a mix of "Song" and "Non-vocal" as the vocalization type.
# Change all "Non-vocal" to "Song" in vocalization columns
WTSP3 <- WTSP2 %>%
  mutate(vocalization = ifelse(species_code == "NONE" & vocalization == "Non-vocal",
                               "Song", vocalization))

# Replace values in "individual_order" and "abundance" with zero where "species_code" is "NONE"
WTSP4 <- WTSP3 %>%
  mutate(
    individual_order = ifelse(species_code == "NONE", 0, individual_order),
    abundance = ifelse(species_code == "NONE", 0, abundance)
  )

# Change all "NONE" species codes to spp. names
WTSP5 <- WTSP4 %>%
  mutate(species_code = ifelse(species_code == "NONE", "WTSP", species_code))

# Remove rows where noise is "High"
WTSP6 <- WTSP5 %>%
  filter(max_noise_volume != "High" | is.na(max_noise_volume))

# Ensure abundance is numeric
WTSP6$abundance <- as.numeric(WTSP6$abundance)

### Summarize number of individuals and abundnace of songs in each recordings ###

# Summarize abundance column by recording_name
WTSP7 <- WTSP6 %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))

# Split recording_date_time into 3 columns
WTSP8 <- WTSP7 %>%
  separate_wider_delim(
    recording_name,
    delim      = " ",
    names      = c("site", "date", "time"),
    cols_remove = FALSE          # drop this arg if you don't need the original
  )

# Create smoky / non-smoky column
smoky_dates <- as.Date(c("2023-05-19", "2023-05-21", "2023-06-11"))

WTSP9 <- WTSP8 %>%
  mutate(
    date         = as.Date(date),   # skip if already Date class
    smoke_status = if_else(date %in% smoky_dates, "smoky", "non-smoky")
  )

# Count the number of site/date combinations
WTSP10 <- WTSP9 %>%
  count(site,smoke_status)

# Great - now that we have confirmed there are 3 recordings per smoky/non-smoky day,
# at each site, we can now continue preparing our data from analysis

# Remove sites where WTSP is not present in any recordings
WTSP11 <- WTSP9 %>%
  group_by(site) %>%
  filter(sum(total_abundance, na.rm = TRUE) > 0) %>%
  ungroup()

# Check which sites were dropped
WTSP12 <- WTSP9 %>%
  group_by(site) %>%
  summarise(total = sum(total_abundance), n_rec = n(), .groups = "drop") %>%
  filter(total == 0) ## Looks good!

## Rejoin lat/lon values to WTSP tag data

# Read in latlon df
WTSP_latlon <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP smoke/Input/Tabular Data/latlong_to_join.csv") %>%
  select(-X) %>%
  rename(site = location)

# Join lat/lon to WTSP11 based on 'site'
WTSP12 <- left_join(WTSP11, WTSP_latlon, by = "site", relationship = "many-to-one")

# Save csv
write.csv(WTSP12, paste0("Input/Tabular Data/WTSP_data_for_analysis_", Sys.Date(), ".csv"), row.names = FALSE)





##################################
############# YEWA ###############
##################################

## In the YEWA project, there are many more recordings than are needed/many recordings
## outside the temporal window of this study. Load file with randomly selected smoky/non-smoky
## recordings and use that to filter tag data

# Load recordings_to_select df
recordings_to_select <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP smoke/Input/Tabular Data/YEWA_tasks_to_use.csv")

# Load main report 
YEWA <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP smoke/Input/Tabular Data/YEWA_main_report_2026-09-29.csv")

# Normalize join keys: trim whitespace and parse datetime to a common format
YEWA <- YEWA %>%
  mutate(
    location            = trimws(as.character(location)),
    recording_date_time = format(
      as.POSIXct(trimws(as.character(recording_date_time)),
                 format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "%Y-%m-%d %H:%M:%S"
    )
  )

recordings_to_select <- recordings_to_select %>%
  mutate(
    location            = trimws(as.character(location)),
    recording_date_time = format(
      as.POSIXct(trimws(as.character(recording_date_time)),
                 format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "%Y-%m-%d %H:%M:%S"
    )
  )

YEWA1 <- YEWA %>%
  semi_join(
    recordings_to_select %>% select(location, recording_date_time),
    by = c("location", "recording_date_time")
  )

YEWA2 <- YEWA1 %>%
  select(location,latitude,longitude,recording_date_time,species_code,task_is_complete,observer,max_noise_volume,max_noise_type,individual_order,abundance,vocalization)

# Combine columns 1 and 2 to create recording_name file
YEWA2$recording_name <- paste(YEWA2$location, YEWA2$recording_date_time, sep = " ")

# Retain only completed tasks
YEWA3 <- YEWA2 %>%
  filter(task_is_complete == "TRUE")

# Ensure abundance is numeric
YEWA3$abundance <- as.numeric(YEWA3$abundance)

### Summarize number of individuals and abundance of songs in each recordings ###

# Summarize abundance column by recording_name
YEWA4 <- YEWA3 %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))

# Split recording_date_time into 3 columns
YEWA5 <- YEWA4 %>%
  separate_wider_delim(
    recording_name,
    delim      = " ",
    names      = c("site", "date", "time"),
    cols_remove = FALSE          # drop this arg if you don't need the original
  )

## Rejoin lat/lon values to YEWA tag data

# Create df with site, lat, lon
YEWA_latlon <- YEWA %>%
  distinct(location, latitude, longitude) %>%
  rename(site = location)

# Filter latlon df to only include sites present in YEWA5
YEWA_latlon_2 <- YEWA_latlon %>%
  filter(site %in% YEWA5$site)

# Join lat/lon to YEWA5 based on 'site'
YEWA6 <- left_join(YEWA5, YEWA_latlon_2, by = "site", relationship = "many-to-one")

## Re-add sites present in recordings_to_select but absent from YEWA6 ##
## (sites with no YEWA detections never appear in the tag data)

# Build the full set of expected recordings from recordings_to_select,
# split into the same site/date/time keys as YEWA5
expected <- recordings_to_select %>%
  transmute(
    recording_name = paste(location, recording_date_time, sep = " "),
    site = location,
    date = sub(" .*$", "", recording_date_time),   # everything before the space
    time = sub("^\\S+ ", "", recording_date_time)   # everything after the space
  )

# Left-join YEWA6 onto the full expected set: rows with no detections get NA abundance
YEWA7 <- expected %>%
  left_join(
    YEWA6 %>% select(recording_name, total_abundance, latitude, longitude),
    by = "recording_name"
  ) %>%
  # zero-fill abundance for recordings that had no YEWA tags
  mutate(total_abundance = tidyr::replace_na(total_abundance, 0))

# Fill lat/lon for the re-added sites from the full site list
YEWA_latlon_all <- YEWA %>%
  distinct(location, latitude, longitude) %>%
  rename(site = location)

YEWA8 <- YEWA7 %>%
  rows_patch(YEWA_latlon_all, by = "site", unmatched = "ignore")

# Assign smoky / non-smoky status based on day
smoky_dates <- as.Date(c("2025-06-10", "2025-06-11"))

YEWA9 <- YEWA8 %>%
  mutate(
    date         = as.Date(date),   # skip if already Date class
    smoke_status = if_else(date %in% smoky_dates, "smoky", "non-smoky")
  )

# Lastly, remove sites where there is no YEWA present in any recordings
YEWA10 <- YEWA9 %>%
  filter(!(site %in% c("EDALA02KER2", "EDALB01CAP1", "EDALC02ZOO1", "EDALC03JAM1")))

## --- Count recordings on smoky vs non-smoky days per site ---
# Non-smoky: June 4-6 and 14-16 ; Smoky: June 10-11
nonsmoky_dates <- as.Date(c("2025-06-04","2025-06-05","2025-06-06",
                            "2025-06-14","2025-06-15","2025-06-16"))
smoky_dates    <- as.Date(c("2025-06-10","2025-06-11"))

recording_check <- YEWA10 %>%
  mutate(
    day = as.Date(date),
    smoke_status = case_when(
      day %in% smoky_dates    ~ "smoky",
      day %in% nonsmoky_dates ~ "nonsmoky",
      TRUE                    ~ "other"
    )
  ) %>%
  group_by(site) %>%
  summarize(
    n_smoky    = sum(smoke_status == "smoky"),
    n_nonsmoky = sum(smoke_status == "nonsmoky"),
    n_other    = sum(smoke_status == "other"),   # recordings outside both windows
    n_total    = n(),
    .groups = "drop"
  )

# Sites that don't have the expected 3 smoky and 3 non-smoky recordings
recording_check %>% filter(n_smoky != 3 | n_nonsmoky != 3)

# Save csv
write.csv(YEWA10, paste0("Input/Tabular Data/YEWA_data_for_analysis_", Sys.Date(), ".csv"), row.names = FALSE)
