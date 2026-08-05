# Load packages

library(tidyverse)
library(dplyr)
library(lme4)

# Load tag data

tags <- read.csv("Input/ABMI_Influence_of_wildfire_smoke_on_avian_singing_rates_tag_report.csv")

### Data transformation for paired t-test

# Select relevant columns only

tags <-tags %>%
  select(location,recording_date_time,species_code,aru_task_status,individual_order,abundance,vocalization)

# Combine columns 1 and 2 to create recording name file

tags$recording_name <- paste(tags$location, tags$recording_date_time, sep = " ")

# Remove location and date columns

tags <-tags %>%
  select(recording_name,species_code,aru_task_status,individual_order,abundance,vocalization)

# Filter out all call tags

tags <- tags %>%
  mutate(
    individual_order = ifelse(vocalization == "Call", 0, individual_order),
    abundance = ifelse(vocalization == "Call", 0, abundance)
  )

# Replace values in "individual_order" and "abundance" with zero where "species_code" is "NONE"

tags <- tags %>%
  mutate(
    individual_order = ifelse(species_code == "NONE", 0, individual_order),
    abundance = ifelse(species_code == "NONE", 0, abundance)
  )

# Change all "NONE" species codes to "WTSP"

tags <- tags %>%
  mutate(species_code = ifelse(species_code == "NONE", "WTSP", species_code))

### Summarize number of individuals and abundnace of songs in each recordings

# Summarize abundance column by recording_name
summary_abundance <- tags %>%
  group_by(recording_name) %>%
  summarize(total_song_count = sum(abundance))

# Count unique individual_order values by recording_name

summary_individual_order <- tags %>%
  group_by(recording_name) %>%
  summarize(unique_order_count = n_distinct(individual_order[individual_order != 0]))

# Merge the two summaries based on recording_name

tags <- merge(summary_abundance, summary_individual_order, by = "recording_name")

# Replace "your_column" with the actual column name you want to split

tags <- tags %>%
  separate(recording_name, into = c("location", "date", "time"), sep = " ")

# Combine location and date columns in a column called "recording name"

tags <- tags %>%
  mutate(recording_name = paste(location, date, sep = " "))

# Creating "mean singing rate column"

tags <- tags %>%
  mutate(mean_singing_rate = total_song_count / unique_order_count)

# Change all NaN values to 0

tags <- tags %>%
  mutate(mean_singing_rate = ifelse(is.nan(mean_singing_rate), 0, mean_singing_rate))

# Reduce the number of decimal places in "mean_singing_rate" to 2

tags <- tags %>%
  mutate(mean_singing_rate = round(mean_singing_rate, 2))

# Change "Location" to "ARU"

tags <- tags %>% rename(site = location)

# Clean up column names

tags <- tags %>% rename(number_of_individuals = unique_order_count)

# Select only relevant columns

tags <- tags %>%
  select(recording_name,site,date,time,total_song_count,number_of_individuals,mean_singing_rate)

# Create new smoky / non-smoky column

tags <- tags %>%
  mutate(treatment = case_when(
    grepl("21$|11$", recording_name) ~ "smoky",
    grepl("15$|24$|9$|13$", recording_name) ~ "non-smoky",
    TRUE ~ NA_character_  # Handle other cases if needed
  ))

# Reorder columns

tags <- tags %>%
  select(recording_name,site,date,time,treatment,total_song_count,number_of_individuals,mean_singing_rate)

# Change site, treatment, and recording name from characters to factor variables

tags$treatment <- as.factor(tags$treatment)
tags$site <- as.factor(tags$site)

# Remove ARU location (1266-SE) that was too hard to transcribed accurately

tags <- tags %>% filter(site != "1266-SE")

# Remove all sites that have count values of 0 in both recordings

tags_filtered <- tags %>%
  group_by(site) %>%
  filter(!(all(total_song_count == 0) & n() > 1))

write.csv(tags_filtered, "Output/data_for_erin.csv")



















