---
  title: "HawkEars_conversion_script"
author: "Kevin Kelly"
date: "2024-10-16"
output: html_document
---
  
  # This script will combine all HawkEars outpur files and convert them into a WT tag csv
  
  ## 1. Load libraries and enter files location
  
  ```{r}
library(stringr)
library(tidyr)
library(dplyr)
library(fs)
#library(tibble)
#library(data.table)
#library(purrr)
#library(readr)
#library(hms)

# Main working directory
root <- "C:/folder/path" # change to match your folder path

#Location of HawkEars outputs within root directory
Hawkears_output <- "/Data/HawkEars/output/folder/" # change to match your output folder
```

# Identify and separate 0 kb files

```{r, include=FALSE}
# Step not complete
```

#Read in Hawkears output files within the output folder into a df; append file names in new column; split species and score into separate columns

```{r}

files = list.files(path=str_c(root, Hawkears_output), full.names = TRUE) 
files_combined = lapply(files, read.table, header=FALSE, sep="")
for (i in 1:length(files_combined)){files_combined[[i]]<-cbind(files_combined[[i]],files[i])}
files_df <- do.call("rbind", lapply(files_combined, as.data.frame)) 
colnames(files_df)[c(1,2,3,4)]<-c("start_time", "end_time", "tag","file_path")

# Remove files and i objects
rm(files)
rm(i)

# Drop file_path and split tag into species and score
detections_df <- files_df %>%
  mutate(file_name = path_file(file_path), .keep = "unused") %>%
  separate_wider_delim(tag, delim = ";", names = c("species", "score"), cols_remove = FALSE)

```

# Define target species and filter to only include records for the target species

```{r, include=FALSE}
target_spp_list <- c("COYE", "WTSP") # add all species of interest to list in this object

detections_target <- detections_df %>%
  filter(species %in% target_spp_list)
```

## Optional

# Determine which detections amongst overlapping detections have the higher score and keep those, while dropping those with lower scores 

```{r, include=FALSE}
detections_keep <- detections_target %>% 
  mutate(keep = case_when(
    start_time - lag(start_time) < 3 & file_name == lag(file_name) & as.numeric(score) - lag(as.numeric(score)) < 0 ~ "Drop",
    lead(start_time) - start_time < 3 & file_name == lead(file_name) & as.numeric(score) - lead(as.numeric(score)) <= 0 ~ "Drop",
    TRUE ~ "Keep")) %>%
  filter(keep == "Keep") %>%
  select(!(keep))

```

## Optional

# Write csv of target species detections. Either including all detections, or including only the highest ranked of overlapping detections

```{r}
# All records of target species
write.csv(detections_target, file = str_c(root, "/Data/folder/path/file_detections_all.csv", sep = ""), row.names = FALSE) # save to chosen folder in root directory

# Only highest ranking overlapping records
write.csv(detections_keep, file = str_c(root, "/Data/folder/path/file_detections_keep.csv", sep = ""), row.names = FALSE) # save to chosen folder in root directory
```

# Choose and Define thresholds for each species of interest (using AOU codes for species)

```{r}
threshold_df <- data.frame(
  species = c("COYE", "WTSP"), # change "SPPx" to 4-letter AOU code of species of interest
  threshold = c(0.75, 0.9) # change values to the values that correspond to the species, in the same order as the species appear in the df
)
```

# Enter length of recordings to define task length later

```{r}
recordingLength <- 180 # time in seconds of recording length or desired task length in WT

```
# Pare down detections to those above threshold and clean up dataframe

```{r}
detections_threshold <- detections_keep %>%
  left_join(threshold_df, by = "species") %>%
  filter(score >= threshold)%>%
  mutate(recording_id = str_remove_all(file_name, "_HawkEars.txt")) %>%
  mutate(recording_id = as.factor(recording_id)) %>%
  select(!c(file_name, tag, threshold))
```

# Create df for WT tags export

```{r}
tags_WT <- detections_threshold %>%
  separate(recording_id, into = c("location", "Date", "Time"), sep = "_") %>%
  mutate(recordingDate = as.POSIXct(
    paste0(substr(Date, 1, 4), ":",   # Year
           substr(Date, 5, 6), ":",   # Month
           substr(Date, 7, 8), " ",   # Day
           substr(Time, 1, 2), ":",   # Hour
           substr(Time, 3, 4), ":",   # Minute
           substr(Time, 5, 6)         # Second
    ), format = "%Y:%m:%d %H:%M:%S")) %>%
  mutate(method = "None") %>%
  mutate(taskLength = recordingLength) %>%
  group_by(location, recordingDate, species) %>%
  mutate(speciesIndividualNumber = row_number()) %>% # generates iterative speciesIndividualNumber values 
  ungroup() %>%
  rename(startTime = start_time) %>%
  mutate(tagLength = end_time - startTime, .after = startTime) %>%
  relocate(startTime, .after = speciesIndividualNumber) %>%
  relocate(tagLength, .after = startTime) %>%
  relocate(species, .after = method) %>%
  select(!c(Date, Time, score, end_time))

```

# Export csv file to upload to Project in WT to create tags

```{r}
#Export consolidated csv
write.csv(tags_WT, str_c(root,"WT_Project_tags.csv"), row.names = FALSE) # change folderpath and filename as you wish

```

