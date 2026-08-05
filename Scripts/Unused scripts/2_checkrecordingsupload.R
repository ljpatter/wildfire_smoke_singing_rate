

### Recording list check -----------------------------------------

### Create list of recording that were manually extracted from Cirrus

# Load packages

library(openxlsx)
library(readr)

# Set working direction to "Recordings" folder where audio files are stored

setwd("/Users/leonardpatterson/Desktop/Recordings/new")

# List all files in the directory
recording_files <- list.files()

# Create a data frame with the file names
recording_data <- data.frame(RecordingName = recording_files)

# Set working directory back to R Project

setwd("/Users/leonardpatterson/Desktop/R Files/Biol 399 Project")

# Replace 'recordings.csv' with the desired name for your CSV file
write_csv(recording_data, "Output/recordings_folder.csv")







### Compare master_file & recording_data to ensure files were extracted accurately --------


# Read the first CSV file

data1 <- read.csv("Output/recordings_folder.csv")

# Read the second CSV file

data2 <- read.csv("Output/BUmaster_recording_list.csv")

# Remove row number column from data2

data2 <- data2[, -1]

# Extract the "file_name" columns from both data frames

file_names1 <- data1$RecordingName
file_names2 <- data2$file_name

# Find values that are in file1.csv but not in file2.csv

values_unique_to_file1 <- setdiff(file_names1, file_names2)

# Find values that are in file2.csv but not in file1.csv

values_unique_to_file2 <- setdiff(file_names2, file_names1)

# Display the values that are unique to each file

cat("Values unique to file1.csv: ", values_unique_to_file1, "\n")
cat("Values unique to file2.csv: ", values_unique_to_file2, "\n")

# Export data2 as datasheet for recording data

#write.xlsx(data2, "Output/datasheet.xlsx")
