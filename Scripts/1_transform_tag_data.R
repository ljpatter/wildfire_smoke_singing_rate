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


##################################
############# WTSP ###############
##################################

# Load main report 
WTSP <- read.csv("Input/Tabular Data/WTSP_main_report_2026-07-27.csv")

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

# Create df with site, lat, lon
WTSP_latlon <- WTSP %>%
  distinct(location, latitude, longitude) %>%
  rename(site = location)

# Filter latlon df to only include sites present in YEWA8
WTSP_latlon_2 <- WTSP_latlon %>%
  filter(site %in% WTSP11$site)

# Join lat/lon to WTSP11 based on 'site'
WTSP12 <- left_join(WTSP11, WTSP_latlon_2, by = "site", relationship = "many-to-one")

# Save csv
write.csv(WTSP12, paste0("Input/Tabular Data/WTSP_data_for_analysis_", Sys.Date(), ".csv"), row.names = FALSE)





##################################
############# YEWA ###############
##################################

# Load main report 
YEWA <- read.csv("Input/Tabular Data/YEWA_main_report.csv")

# Select relevant columns only
YEWA1 <- YEWA %>%
  select(location,latitude,longitude,recording_date_time,species_code,task_is_complete,observer,max_noise_volume,max_noise_type,individual_order,abundance,vocalization)

# Combine columns 1 and 2 to create recording name file
YEWA1$recording_name <- paste(YEWA1$location, YEWA1$recording_date_time, sep = " ")

# Remove all tags that are calls
YEWA2 <- YEWA1 %>%
  filter(vocalization != "Call")

# Retain only completed tasks
YEWA3 <- YEWA2 %>%
  filter(task_is_complete == "t")

# Replace values in "individual_order" and "abundance" with zero where "species_code" is "NONE"
YEWA4 <- YEWA3 %>%
  mutate(
    individual_order = ifelse(species_code == "NONE", 0, individual_order),
    abundance = ifelse(species_code == "NONE", 0, abundance)
  )

# Change all "NONE" species codes to spp. names
YEWA5 <- YEWA4 %>%
  mutate(species_code = ifelse(species_code == "NONE", "YEWA", species_code))

# Ensure abundance is numeric
YEWA5$abundance <- as.numeric(YEWA5$abundance)

### Summarize number of individuals and abundance of songs in each recordings ###

# Summarize abundance column by recording_name
YEWA6 <- YEWA5 %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))

# Split recording_date_time into 3 columns
YEWA7 <- YEWA6 %>%
  separate_wider_delim(
    recording_name,
    delim      = " ",
    names      = c("site", "date", "time"),
    cols_remove = FALSE          # drop this arg if you don't need the original
  )

# Remove recordings outside of the window of interest
#YEWA8 <- YEWA7 %>%
#  filter(as_hms(time) >= as_hms("03:59:00"),
#         as_hms(time) <= as_hms("15:01:00"))

## Rejoin lat/lon values to YEWA tag data

# Create df with site, lat, lon
YEWA_latlon <- YEWA %>%
  distinct(location, latitude, longitude) %>%
  rename(site = location)

# Filter latlon df to only include sites present in YEWA8
YEWA_latlon_2 <- YEWA_latlon %>%
  filter(site %in% YEWA7$site)

# Join lat/lon to YEWA8 based on 'site'
YEWA8 <- left_join(YEWA7, YEWA_latlon_2, by = "site", relationship = "many-to-one")
  
# Save csv
write.csv(YEWA8, paste0("Input/Tabular Data/YEWA_data_for_analysis_", Sys.Date(), ".csv"), row.names = FALSE)
