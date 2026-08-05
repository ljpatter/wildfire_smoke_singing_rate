# Load packages

library(dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)
library(sf)





##### LOCATION DATA TRANSFORMATION FOR ABMI EH SITES (separate code for BU sites to come later in code)


# Load data

dat1 <- read.csv("Input/ABMI_Site_Public_Locations.csv")
dat2 <- read.csv("Input/ABMI-EH-currentyearrecordingperiods2023.csv")

# Change column names of dat1 so that it can be merged with dat2

new_column_names <- c("location", "lat", "long")  
colnames(dat1) <- new_column_names

# Remove unnecessary columns from dat1 and dat2

dat1<-dat1%>%
  select(location,lat,long)

dat2<-dat2%>%
  select(location,earliestCalendar,latestCalendar)

# Split dat2 'location' column into two

split_values <- strsplit(dat2$location, "-")
dat2$location <- sapply(split_values, function(x) x[1])
dat2$quadrant <- sapply(split_values, function(x) x[2])

# Remove 'quadrant' column from dat2

dat2 <- dat2[, -c(4)]

# Remove duplicate sites

dat2 <- dat2[!duplicated(dat2$location), ]

# Change dat1 'location' from an integer to a character

dat1$location <- as.character(dat1$location)

# Merge dat1 and dat2 based on location

merged_data <- inner_join(dat1, dat2, by = "location")

# Export merged dat1/dat2 files to csv

write.csv(merged_data, "Output/ABMI_locations_merged.csv", row.names = FALSE)








##### TRANSFORM RECORDING LIST DATA FOR ABMI EH SITES ######

# Load data 

allrecordings <-read.csv("Input/allcurrentyearWAVrecordings.csv")


# Split 'recording name' column into 'date' and 'time' columns

allrecordings$recording_date <- sapply(strsplit(allrecordings$recording_date_time, " "), function(x) x[1])
allrecordings$recording_time <- sapply(strsplit(allrecordings$recording_date_time, " "), function(x) x[2])

allrecordings$recording_hour <- sapply(strsplit(allrecordings$recording_time, ":"), function(x) x[1])
allrecordings$recording_minute <- sapply(strsplit(allrecordings$recording_time, ":"), function(x) x[2])

# Split 'site' column into 'site' and 'quadrant' columns

allrecordings$site <- sapply(strsplit(allrecordings$location, "-"), function(x) x[1])
allrecordings$quadrant <- sapply(strsplit(allrecordings$location, "-"), function(x) x[2])

# Remove unneeded columns

allrecordings <- allrecordings[, -c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11)]

# Rearrange order of columns

allrecordings <- allrecordings[, c("file_name", "site", "quadrant", "recording_date", "recording_time", "recording_hour", "recording_minute", "time_index")]

# Save file

write.csv(allrecordings, "Output/transformed_data.csv")








library(gstat)
library(sp)
library(dplyr)
library(sf)
library(tidyr)




### Transform PM2.5 values


# Load PM2.5 data

dat1 <- read.csv("Input/PM25_values.csv")

### Remove first 6 rows

dat1 <- dat1[-c(1:6), ]

### Create new data frame with one column of station names

stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

### Create new data frame with lat lon of air monitoring stations

coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns

# Create new data frame with smoke values

smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

### Combine station names, coordinates, and PM2.5 values

dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")






























### TRANSFORM SMOKE DATA 

# Load smoke data

smokedata <- read.csv("Input/AQHI_communities_filtered.csv")

# Remove column headers and replace with row 1

colnames(smokedata) <- as.character(smokedata[1, ])
smokedata <- smokedata[-1, ]

# Convert wide to long

#smokedata <- pivot_longer(smokedata, cols = c("Caroline", "Drayton Valley", "Genesee", "St. Albert", "Steeper"), names_to = "AQHI communities", values_to = "AQHI values")
smokedata <- pivot_longer(smokedata, cols = c("Caroline", "Drayton Valley", "Genesee", "St. Albert", "Steeper","Elk Island", "Lamont", "Grimshaw", "Red Deer", "Cadotte Lake"), names_to = "AQHI communities", values_to = "AQHI values")

# Split 'date' column into a column for 'date' and another for 'time'

split_data <- strsplit(smokedata$Date, " ")
smokedata$date <- sapply(split_data, function(x) paste(x[1], x[2], x[3], collapse = " "))
smokedata$time <- sapply(split_data, function(x) paste(x[4], x[5], x[6], collapse = " "))

# Remove old 'date' column

smokedata<-smokedata%>%
  select(date, time,`AQHI communities`,`AQHI values`)

# convert 'date' values so that they match up, and can be merged with, merged_data2_subset

smokedata <- smokedata %>%
  mutate(date = as.Date(date, format = "%B %d %Y"))

# Remove time zone from the time column so that it can be merged with merged_data2_subset

split_data <- strsplit(smokedata$time, " ")
smokedata$time <- sapply(split_data, `[`, 1)
smokedata$deleteme <- sapply(split_data, `[`, 2)

# Remove old 'time' column

smokedata<-smokedata%>%
  select(date, time,`AQHI communities`,`AQHI values`)

# Change 'date' in smoke data to a character so it can merge w/ merged_data2_subset

smokedata$date <- as.character(smokedata$date)

# Write to file

write.csv(smokedata, "Output/transformed_smokedata.csv")








### CALCULATE DISTANCE BETWEEN 2023 ABMI EH SITES AND WEATHER STATIONS ###


# Create shapefile

ssyr.sf <- st_as_sf(x = merged_data,                         
                    coords = c("long", "lat"),
                    crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 
                    +datum=WGS84 +no_defs +towgs84=0,0,0")

print("Bounding box of new shapefile in lat/long:")

st_bbox(ssyr.sf)

ssyr.sf<-st_transform(ssyr.sf, "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
str(ssyr.sf)
st_write(ssyr.sf, "Output/ABMI_2023locations.shp", append=FALSE)
#we can use this shape-file as an asset to extract data in GEE



#now add weather stations
weatherStations<-read.csv("Input/AQHI_communities_lat_long.csv")
weatherStations <- weatherStations[-13, ]
weatherStations <- weatherStations[-33, ]
weather.sf <- st_as_sf(x = weatherStations,                         
                       coords = c("long", "lat"),
                       crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 
                       +datum=WGS84 +no_defs +towgs84=0,0,0")

weather.sf<-st_transform(weather.sf, "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
str(weather.sf)
st_write(weather.sf, "Output/weatherstation_2023locations.shp", append=FALSE)

print("Bounding box of new shapefile in UTM coordinates:")

weather.sf$town<-as.factor(weather.sf$town)


estimDist<-function(pointfile, polypointfile, maxdist){
  ss.sf.summary<-list()
  #for (i in 1:100){#first 100 point counts in this shapefile
  for (i in 1:nrow(pointfile)){#all of the point counts in this shapefile
    ss.sf.i<-pointfile[i,]
    SS.i<-ss.sf.i$location
    b.maxdist<-st_buffer(ss.sf.i,maxdist)
    p.maxdist<-st_intersection(polypointfile, b.maxdist)#replaced polypointfile.y with polypointfile
    nearest<-ifelse((nrow(p.maxdist)==0), maxdist+1, 
                    min(st_distance(ss.sf.i,p.maxdist)))
    p.maxdist$townDistance<-as.vector(st_distance(ss.sf.i, p.maxdist))
    
    nearestTown<-ifelse(nrow(p.maxdist)==0, "Too far away", p.maxdist %>% filter(townDistance==nearest)%>% st_drop_geometry() %>%group_by(town) %>% as_tibble %>%
                          select(town)%>%
                          filter(row_number()==1))
    
    ss.sf.summary[[i]]<-data.frame(location=SS.i,
                                   Town=nearestTown,
                                   NEAR.DIST=nearest)
    print(paste0("point ",i," done"))  #you can turn this on if you want to watch the progress in your analysis
    colnames(ss.sf.summary[[i]])<-c("location","town","NEAR.DIST")
  }
  summaries<-do.call(rbind, ss.sf.summary)
  return(summaries)
}


maxdist<-200000 #200 km buffer
pointfile<-ssyr.sf
pointfilebuffer<-st_buffer(pointfile, maxdist)
featuretype<-weather.sf

a<-Sys.time()
polypointfile<-st_intersection(featuretype, pointfilebuffer)

summaries<-estimDist(pointfile,polypointfile,maxdist)
summaries<-data.frame(summaries)
#n=100; the estimDist function by default just estimates distance to footprint for the first 100 points, but can be easily modifed to estimate distance to footprint for all points
str(summaries)
summaries$distWeatherStation<-summaries$NEAR.DIST
summaries$NEAR.DIST<-NULL
write.csv(summaries, file=paste0("Output/potentialPoints_weatherStationDistancesNew.csv"))
b<-Sys.time()
print(b-a)










#### JOIN WEATHER STATIONS AND DISTANCES w/ RECORDINGS ####

# Read in distances file

distances <-read.csv("Output/potentialPoints_weatherStationDistancesNew.csv")

# Remove unnecessary columns 

distances<-distances%>%
  select(location,town,distWeatherStation)

# Change column name "location" to "site" 

names(distances)[names(distances) == "location"] <- "site"

# Change distances$site from an integer to a character

distances$site <- as.character(distances$site)

# Join "distances" and "all recordings" using location

merged_data2 <- inner_join(distances, allrecordings, by = "site")

# Rearrange column order

merged_data2 <- merged_data2[, c("file_name", "site", "quadrant", "recording_date", "recording_time", "recording_hour", "recording_minute", "time_index", "town", "distWeatherStation")]

# write to file 

write.csv(merged_data2, "Output/merged_data2.csv")









### FILTER MERGED_DATA2 TO ONLY INCLUDE DATES AND TIMES OF INTEREST ---------


# Subset merged_data2 to only include recordings on May 15, 21, and 24

dates_to_include <- c("2023-05-15", "2023-05-21", "2023-05-24")
merged_data2_subset <- merged_data2 %>% filter(recording_date %in% dates_to_include)

# Subset merged_data2_subset to only include recordings during the dawn chorus (i.e., during time index == 3)

merged_data2_subset <- merged_data2_subset[merged_data2_subset$time_index == 3, ]

# Remove stations that are greater than 200 km from a AQHI community

merged_data2_subset <- merged_data2_subset[merged_data2_subset$distWeatherStation < 200000, ]

# Change before-smoky dates in Grimshaw from 15th to the 17th

merged_data2_subset <- merged_data2_subset %>%
  mutate(recording_date = ifelse(site == "457" & recording_date == "2023-05-15", "2023-05-17", recording_date))

# Change before-smoky file_name in Grimshaw from 15th to the 17th

merged_data2_subset$file_name[merged_data2_subset$file_name == "457-NE_20230515_060200"] <- "457-NE_20230517_060200"
merged_data2_subset$recording_time[merged_data2_subset$file_name == "457-NE_20230517_060200"] <- "06:02:00"

merged_data2_subset$file_name[merged_data2_subset$file_name == "457-NW_20230515_060500"] <- "457-NW_20230517_060200"
merged_data2_subset$recording_time[merged_data2_subset$file_name == "457-NW_20230517_060200"] <- "06:02:00"

merged_data2_subset$file_name[merged_data2_subset$file_name == "457-SW_20230515_060500"] <- "457-SW_20230517_060200"
merged_data2_subset$recording_time[merged_data2_subset$file_name == "457-SW_20230517_060200"] <- "06:02:00"

# Write to file

write.csv(merged_data2_subset, "Output/merged_data2_subset")









### FINALLY, MERGE SMOKE DATA W/ MERGED_DATA2_SUBSET ------------------------

# Change column name from 'town' to 'AQHI communities'

names(merged_data2_subset)[names(merged_data2_subset) == "town"] <- "AQHI communities"

# Find rows where the time is in "hh" format and update it to "hh:mm:ss"

recording_hour <- "recording_hour"
merged_data2_subset[[recording_hour]] <- gsub("^(\\d{2})$", "\\1:00", merged_data2_subset[[recording_hour]])

# Change date and time column name in merged_data2_subset to match those in smokedata

names(merged_data2_subset)[names(merged_data2_subset) == "recording_hour"] <- "time"
names(merged_data2_subset)[names(merged_data2_subset) == "recording_date"] <- "date"

# merge "merged_data2_subset" with "smokedata"

common_columns <- c("AQHI communities", "date", "time")
master_file <- inner_join(merged_data2_subset, smokedata, by = common_columns)

# Clean up master_file

names(master_file)[names(master_file) == "time"] <- "recording_hour"
master_file <- master_file[, !names(master_file) %in% "recording_minute"]

# Add .wav to the end of all file names

master_file <- master_file %>%
  mutate(file_name = paste0(file_name, ".wav"))

# Site 1237-NE only had recordings for the 15th - had to discard this ARU
# This code removes the one 1237-NE file that transferrd (1237-NE_20230515_060500.wav)

value_to_remove <- "1237-NE_20230515_060500.wav"
master_file <- master_file[master_file$file_name != value_to_remove, ]

# Remove all sites that don't have AQHI data for the dates in question

master_file <- master_file %>%
  filter(`AQHI values` != "N/A")

# write "master recording file" to csv for mover script

write.csv(master_file, "Output/master_recording_list.csv")










##### BELOW IS CODE FOR 2023 BU SITES ######


# Load packages

library(dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)
library(sf)

# Load files

dat1 <- read.csv("Input/potential_locations.csv")


### GET DISTANCE BETWEEN 2023 BU SITES AND WEATHER STATIONS ### 


# Create shapefile

ssyr.sf <- st_as_sf(x = dat1,                         
                    coords = c("longitude", "latitude"),
                    crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 
                    +datum=WGS84 +no_defs +towgs84=0,0,0")

print("Bounding box of new shapefile in lat/long:")

st_bbox(ssyr.sf)

ssyr.sf<-st_transform(ssyr.sf, "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
str(ssyr.sf)
st_write(ssyr.sf, "Output/BU_2023locations.shp", append=FALSE)
#we can use this shape-file as an asset to extract data in GEE


#now add weather stations
weatherStations<-read.csv("Input/AQHI_communities_lat_long.csv")
weatherStations <- weatherStations[-13, ]
weatherStations <- weatherStations[-33, ]
weather.sf <- st_as_sf(x = weatherStations,                         
                       coords = c("long", "lat"),
                       crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 
                       +datum=WGS84 +no_defs +towgs84=0,0,0")

weather.sf<-st_transform(weather.sf, "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
str(weather.sf)
st_write(weather.sf, "Output/weatherstation_2023locations.shp", append=FALSE)

print("Bounding box of new shapefile in UTM coordinates:")

weather.sf$town<-as.factor(weather.sf$town)


estimDist<-function(pointfile, polypointfile, maxdist){
  ss.sf.summary<-list()
  #for (i in 1:100){#first 100 point counts in this shapefile
  for (i in 1:nrow(pointfile)){#all of the point counts in this shapefile
    ss.sf.i<-pointfile[i,]
    SS.i<-ss.sf.i$location
    b.maxdist<-st_buffer(ss.sf.i,maxdist)
    p.maxdist<-st_intersection(polypointfile, b.maxdist) #replaced polypointfile.y with polypointfile
    nearest<-ifelse((nrow(p.maxdist)==0), maxdist+1, 
                    min(st_distance(ss.sf.i,p.maxdist)))
    p.maxdist$townDistance<-as.vector(st_distance(ss.sf.i, p.maxdist))
    
    nearestTown<-ifelse(nrow(p.maxdist)==0, "Too far away", p.maxdist %>% filter(townDistance==nearest)%>% st_drop_geometry() %>%group_by(town) %>% as_tibble %>%
                          select(town)%>%
                          filter(row_number()==1))
    
    ss.sf.summary[[i]]<-data.frame(location=SS.i,
                                   Town=nearestTown,
                                   NEAR.DIST=nearest)
    print(paste0("point ",i," done"))  #you can turn this on if you want to watch the progress in your analysis
    colnames(ss.sf.summary[[i]])<-c("location","town","NEAR.DIST")
  }
  summaries<-do.call(rbind, ss.sf.summary)
  return(summaries)
}


maxdist<-200000 #200 km buffer
pointfile<-ssyr.sf
pointfilebuffer<-st_buffer(pointfile, maxdist)
featuretype<-weather.sf

a<-Sys.time()
polypointfile<-st_intersection(featuretype, pointfilebuffer)

summaries<-estimDist(pointfile,polypointfile,maxdist)
summaries<-data.frame(summaries)
#n=100; the estimDist function by default just estimates distance to footprint for the first 100 points, but can be easily modifed to estimate distance to footprint for all points
str(summaries)
summaries$distWeatherStation<-summaries$NEAR.DIST
summaries$NEAR.DIST<-NULL
write.csv(summaries, file=paste0("Output/BUpotentialPoints_weatherStationDistancesNew.csv"))
b<-Sys.time()
print(b-a)








### JOIN WEATHER STATIONS AND DISTANCES w/ RECORDINGS -----------------------

# Load all 2023 recordings

allrecordings <- read.csv("Input/allcurrentyearWAVrecordings.csv")

# Read in distances file

distances <-read.csv("Output/BUpotentialPoints_weatherStationDistancesNew.csv")

# Remove unnecessary columns 

distances<-distances%>%
  select(location,town,distWeatherStation)

# Select only sites that are nearest to Caroline

distances <- distances[distances$town != "Airdrie", ]

# Change distances$site from an integer to a character

distances$location <- as.character(distances$location)

# Change "location" to all uppercase so that it can merge with allrecordings.csv

distances$location <- toupper(distances$location)

# Join "distances" and "all recordings" using location

dat1 <- inner_join(distances, allrecordings, by = "location")

# Split recording_date_time into recording_date and recording_time

split_date_time <- strsplit(dat1$recording_date_time, " ")

# Create new columns in the 'distances' data frame

dat1$recording_date <- sapply(split_date_time, "[", 1)
dat1$recording_time <- sapply(split_date_time, "[", 2)

# Rearrange column order

dat1<-dat1%>%
  select(location,town,distWeatherStation,recording_date,recording_time,time_index)

# Filter rows where time_index is equal to 7

dat2 <- dat1[dat1$time_index == 7, ]

# Remove all rows where time_index 7 is not equal to ~6:00 AM

dat3 <- dat2[!(dat2$recording_time %in% c("01:45:00", "11:40:00")), ]

# Combine 'recording_date' and 'recording_time' into a new column

dat3$combined_datetime <- as.POSIXct(sprintf("%s %s", dat3$recording_date, dat3$recording_time), format="%Y-%m-%d %H:%M:%S")

# Round the 'combined_datetime' to the nearest hour

dat3$recording_time_rounded <- round_date(dat3$combined_datetime, unit = "hour")

# Remove the intermediate 'combined_datetime' column if not needed

dat3 <- dat3[, !(names(dat3) %in% "combined_datetime")]

# Split 'recording_time_rounded' based on the space

split_time <- str_split(dat3$recording_time_rounded, " ", simplify = TRUE)

# Create a new column 'round_recording_time'

dat3$round_recording_time <- split_time[, 2]

# Remove the intermediate split date column

dat3 <- dat3[, !(names(dat3) %in% "recording_time_rounded")]

# Convert time values from hh:mm:ss to hh:mm

dat3$round_recording_time <- sprintf("%02d:%02d", hour(hms(dat3$round_recording_time)), minute(hms(dat3$round_recording_time)))

# Change town column to AQHI communities

colnames(dat3)[colnames(dat3) == "town"] <- "AQHI communities"











### TRANSFORM SMOKE DATA ###

# Load smoke data

smokedata <- read.csv("Input/BU_Communities_AQHI.csv")

# Remove column headers and replace with row 1

colnames(smokedata) <- as.character(smokedata[1, ])
smokedata <- smokedata[-1, ]

# Convert wide to long

#smokedata <- pivot_longer(smokedata, cols = c("Caroline", "Drayton Valley", "Genesee", "St. Albert", "Steeper"), names_to = "AQHI communities", values_to = "AQHI values")
smokedata <- pivot_longer(smokedata, cols = c("Caroline"), names_to = "AQHI communities", values_to = "AQHI values")

# Split 'date' column into a column for date and another for time

split_data <- strsplit(smokedata$Date, " ")
smokedata$date <- sapply(split_data, function(x) paste(x[1], x[2], x[3], collapse = " "))
smokedata$time <- sapply(split_data, function(x) paste(x[4], x[5], x[6], collapse = " "))

# Remove old date column

smokedata<-smokedata%>%
  select(date, time,`AQHI communities`,`AQHI values`)

# convert date values so that they match up, and can be merged with dat3

smokedata <- smokedata %>%
  mutate(date = as.Date(date, format = "%B %d %Y"))

# Remove time zone from the time column so that it can be merged with dat3

split_data <- strsplit(smokedata$time, " ")
smokedata$time <- sapply(split_data, `[`, 1)
smokedata$deleteme <- sapply(split_data, `[`, 2)

# Remove old time column

smokedata<-smokedata%>%
  select(date, time,`AQHI communities`,`AQHI values`)

# Change 'date' in smoke data to a character so it can merge w/ merged_data2_subset

smokedata$date <- as.character(smokedata$date)

# Write to file

write.csv(smokedata, "Output/transformed_smokedata.csv")









### FINALLY, MERGE SMOKE DATA W/ dat3 ###


# Change date and time column name in dat2 to match those in smokedata

names(dat3)[names(dat3) == "round_recording_time"] <- "time"
names(dat3)[names(dat3) == "recording_date"] <- "date"

# merge "merged_data2_subset" with "smokedata"

common_columns <- c("AQHI communities", "date", "time")
master_file <- inner_join(dat3, smokedata, by = common_columns)

# Extract only relevant dates

selected_dates <- c("2023-06-09", "2023-06-11", "2023-06-13")
master_file <- master_file[master_file$date %in% selected_dates, ]

# Remove all sites that don't have a complete set of before / smoky / after

location_counts <- table(master_file$location)
valid_locations <- names(location_counts[location_counts >= 3])
master_file <- master_file[master_file$location %in% valid_locations, ]

# Remove dashes from the "date" column

master_file$date <- gsub("-", "", master_file$date)

# Remove colons from the "recording_time" column

master_file$recording_time <- gsub(":", "", master_file$recording_time)

# Combine "location", "date", and "recording_time" columns with underscores

master_file$recording_name <- paste(master_file$location, master_file$date, master_file$recording_time, sep = "_")

# Add .wav to the end of all file names

master_file <- master_file %>%
  mutate(recording_name = paste0(recording_name, ".wav"))

# Remove all sites that start with the prefix "H23" as they were not in BUpublic
rows_to_remove <- grepl("^H", master_file$location)
master_file <- master_file[!rows_to_remove, ]

# write "master recording file" to csv for mover script

write.csv(master_file, "Output/BUmaster_recording_list.csv")





