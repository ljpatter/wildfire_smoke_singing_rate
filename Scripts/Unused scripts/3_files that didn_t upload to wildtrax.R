### Recording list check -----------------------------------------

### Create list of recording that were manually extracted from Cirrus

# Load packages

library(zoo)
library(openxlsx)
library(readr)
library(dplyr)
library(tidyr)

# Set working direction to "Recordings" folder where audio files are stored

setwd("/Users/leonardpatterson/Desktop/Recordings")

# List all files in the directory
recording_files <- list.files()

# Create a data frame with the file names
recording_data <- data.frame(RecordingName = recording_files)

# Set working directory back to R Project

setwd("/Users/leonardpatterson/Desktop/R Files/Biol 399 Project")

# Replace 'recordings.csv' with the desired name for your CSV file
write_csv(recording_data, "Output/recordings_folder.csv")






### Compare wildtrax_recordings & recording_data to ensure files were extracted accurately --------


# Read the first CSV file

data1 <- read.csv("Output/recordings_folder.csv")

# Read the second CSV file

data2 <- read.csv("Input/wildtrax_recordings.csv")





#### Transform wildtrax_recordings data to be able to compare with recordings_folder

# Replace space in recording date with an underscore

data2[["recordingDate"]] <- gsub("^\\s+", "", data2[["recordingDate"]])  
data2[["recordingDate"]] <- gsub(" ", "_", data2[["recordingDate"]], fixed = TRUE) 

# Remove all hyphens and colons from the recordingDate column

data2[["recordingDate"]] <- gsub("[-:]", "", data2[["recordingDate"]])

# Combine "location" and "recordingDate" columns

data2$RecordingName <- paste(data2[["location"]], data2[["recordingDate"]], sep = "_")

# Remove the original columns (column1 and column2)

data2 <- data2[, !(names(data2) %in% c("location", "recordingDate"))]

# Add .wav to all RecordingName cells

data2[["RecordingName"]] <- paste(data2[["RecordingName"]], ".wav", sep = "")






### Compare wildtrax recordings with recording folder


# Determine which values differ between two columns
differing_data <- anti_join(data1, data2, by = "RecordingName")

# Write the differing cell values to a new CSV file
write_csv(differing_data, "Output/missingrecordings.csv")









### Convert differing_data into a format to be reuploaded to WildTrax


# One million data transformations

differing_data <- read.csv("Output/missingrecordings.csv")
differing_data$RecordingName <- gsub("_", " ", differing_data$RecordingName)
differing_data <- separate(differing_data, RecordingName, into = c("location", "recordingDate"), sep = " ", extra = "merge")
differing_data <- separate(differing_data, recordingDate, into = c("recordingDate", "recordingTime"), sep = " ")
differing_data$recordingTime <- sub("\\.wav$", "", differing_data$recordingTime)
differing_data$recordingDate <- as.Date(as.character(differing_data$recordingDate), format = "%Y%m%d")
differing_data$recordingDate <- format(differing_data$recordingDate, "%Y-%m-%d")
differing_data$recordingTime <- sprintf(
  "%02d:%02d:%02d",
  as.integer(substr(differing_data$recordingTime, 1, 2)),
  as.integer(substr(differing_data$recordingTime, 3, 4)),
  as.integer(substr(differing_data$recordingTime, 5, 6))
)
differing_data$recordingDate <- paste(differing_data$recordingDate, differing_data$recordingTime, sep = " ")
differing_data<-differing_data%>%
  select(location,recordingDate)

# Merge missing sites with task.csv from WildTrax

incomplete_wildtrax_tasks <- read.csv("Input/Influence of wildfire smoke on avian singing rates_Tasks_202321.csv")
complete_wildtrax_tasks <- bind_rows(differing_data, incomplete_wildtrax_tasks)

# The missing files that were added to the master file as missing values in other columns
# This code applies values generated in the wildtrax tasks csv to all cells in that column

complete_wildtrax_tasks$method <- "None"
complete_wildtrax_tasks$taskLength <- 1
complete_wildtrax_tasks$status <- "New"
complete_wildtrax_tasks$transcriber <- "Not Assigned"
complete_wildtrax_tasks<-complete_wildtrax_tasks%>%
  select(location,recordingDate,method,taskLength)
complete_wildtrax_tasks$organization <- "ABMI"


# Export to csv

write.csv(complete_wildtrax_tasks,"Output/complete_tasks_wildtrax.csv")
