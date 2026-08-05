# Load packages

library(tidyr)
library(dplyr) 

# Load data

dat1 <- read.csv("Input/classifier_recordings.csv", stringsAsFactors = FALSE)

# Create new columns for recording name info

dat1 <- dat1 %>%
  separate(col = file_path, 
           into = paste0("Part", 1:8),  # Adjust based on your expected parts
           sep = "\\\\", 
           extra = "merge",  # Ensures extra pieces stay together
           fill = "right",   # Fills missing pieces with NA
           remove = FALSE)   # Keep the original column

# Split the last part into Site, Date, and Time

dat1 <- dat1 %>%
  separate(col = Part8, 
           into = c("Site", "Date", "Time"), 
           sep = "_", 
           fill = "right",  # Fills missing pieces with NA
           extra = "merge") # Merges extra pieces into the last column

# Convert Date to proper format, ensuring to handle any NAs

dat1$Date <- as.Date(dat1$Date, format = "%Y%m%d")

# Check for missing values in the Date column

missing_dates <- sum(is.na(dat1$Date))

# Print the number of missing values

if (missing_dates > 0) {
  cat("There are", missing_dates, "missing values in the Date column.\n")
} else {
  cat("There are no missing values in the Date column.\n")
}

# Filter recordings based on date

dat1 <- dat1 %>%
  filter(!is.na(Date) & Date >= as.Date("2023-05-15") & Date <= as.Date("2023-05-25"))

# Filter to only include sites of interest

dat1 <- dat1 %>%
  separate(col = Site, 
           into = c("Site", "quadrant"), 
           sep = "-", 
           extra = "merge",  # Merges extra pieces into the last column if any
           fill = "right")   # Fills missing pieces with NA

sites_to_include <- c(1014, 1015, 1048, 1049, 1154, 
                      1237, 1239, 1249, 1266, 
                      1267, 1268, 1294, 1295, 
                      1296, 1324)

dat1 <- dat1 %>%
  filter(Site %in% sites_to_include)

# Filter to only file_path column for extraction from Cirrus

dat2 <- dat1 %>%
  select(1)

# Filter to only include recording name

# Create new columns for recording name info

dat2 <- dat2 %>%
  separate(col = file_path, 
           into = paste0("Part", 1:8),  # Adjust based on your expected parts
           sep = "\\\\", 
           extra = "merge",  # Ensures extra pieces stay together
           fill = "right",   # Fills missing pieces with NA
           remove = TRUE)   # Keep the original column

dat2 <- dat2 %>%
  select(8)

# Save 

write.csv(dat2, "Output/classifier_recordings_filtered.csv")
write.table(dat2, file = "Output/classifier_recordings_filtered.txt", sep = "\t", row.names = FALSE, quote = FALSE)




### Manipulations to create format suitable for uploading tasks to WildTrax

dat3 <- dat1 %>%
  select(8,11,12)

dat3$Time <- gsub("\\.wav$", "", dat3$Time)
dat3$Time <- sub("^(\\d{2})(\\d{2})(\\d{2})$", "\\1:\\2:\\3", dat3$Time)
dat3$recordingDate <- paste(dat3$Date, dat3$Time)
dat3$recordingDate <- as.POSIXct(dat3$recordingDate, format = "%Y-%m-%d %H:%M:%S")
dat3$recordingDate <- format(dat3$recordingDate, "%Y-%m-%d %H:%M:%S")
colnames(dat3)[1] <- "location"
dat3 <- dat3[, !colnames(dat3) %in% c("Date", "Time")]
dat3$method <- "None"
dat3$taskLength <- 180
dat3$status <- ""  # Initialize with blank
dat3$transcriber <- ""  # Initialize with blank
dat3$rain <- ""  # Initialize with blank
dat3$wind <- ""  # Initialize with blank
dat3$industryNoise <- ""  # Initialize with blank
dat3$otherNoise <- ""  # Initialize with blank
dat3$taskComments <- ""  # Initialize with blank
dat3$internal_task_id <- ""  # Initialize with blank
dat3$audioQuality <- "" 

# Export to CSV with the specified file name
write.csv(dat3, "Output/classifier_upload_to_WildTrax.csv", row.names = FALSE)








