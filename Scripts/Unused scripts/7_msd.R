# Load required libraries
library(dplyr)
library(tidyr)



### MAY CLUSTER 1 MSD ###

# Step 1: Load the data
# Replace "your_data_file.csv" with the actual filename of your data.
data <- read.csv("Input/msd.csv")

# Step 2: Function to calculate Mean Squared Difference (MSD)
calculate_msd <- function(vec1, vec2) {
  mean((vec1 - vec2)^2, na.rm = TRUE)
}

# Step 3: Calculate MSD for each day

# Create a data frame to store MSD values for each day
msd_results <- data.frame(Date = character(), Average_MSD = numeric(), 
                          stringsAsFactors = FALSE)

# Get unique dates
dates <- unique(data$Date)

# Loop through each date and calculate MSD between stations
for (date in dates) {
  # Filter data for the current date
  daily_data <- data %>% filter(Date == date)
  
  # Ensure there's data for the current date
  if (nrow(daily_data) > 0) {
    # Extract PM2.5 values for different stations
    pm25_caroline <- daily_data$Caroline_PM25
    pm25_drayton <- daily_data$Drayton_Valley_PM25
    pm25_steeper <- daily_data$Steeper_PM25
    
    # Check if we have values from all stations
    if (length(pm25_caroline) > 0 && length(pm25_drayton) > 0 && length(pm25_steeper) > 0) {
      # Calculate MSD between pairs of stations
      msd_cd <- calculate_msd(pm25_caroline, pm25_drayton)  # Caroline vs Drayton
      msd_cs <- calculate_msd(pm25_caroline, pm25_steeper)  # Caroline vs Steeper
      msd_ds <- calculate_msd(pm25_drayton, pm25_steeper)   # Drayton vs Steeper
      
      # Calculate the average MSD for the current date
      average_msd <- mean(c(msd_cd, msd_cs, msd_ds), na.rm = TRUE)
      
      # Append the results to the data frame
      msd_results <- rbind(msd_results, data.frame(Date = date, Average_MSD = average_msd))
    }
  }
}

# Step 4: Print average MSD results for each date
print(msd_results)












### JUNE DATES MSD ###

# Step 1: Load the data
data2 <- read.csv("Input/msd2.csv")

# Step 2: Function to calculate Mean Squared Difference (MSD)
calculate_msd <- function(vec1, vec2) {
  mean((vec1 - vec2)^2, na.rm = TRUE)
}

# Step 3: Calculate MSD for each day

# Create a data frame to store MSD values for each day
msd_results <- data.frame(Date = character(), Average_MSD = numeric(), Smoke = integer(),
                          stringsAsFactors = FALSE)

# Get unique dates
dates <- unique(data2$Date)

# Loop through each date and calculate MSD between stations
for (date in dates) {
  # Filter data for the current date
  daily_data <- data2 %>% filter(Date == date)
  
  # Ensure there's data for the current date
  if (nrow(daily_data) > 0) {
    # Extract PM2.5 values for Caroline and Airdrie stations
    pm25_caroline <- daily_data$Caroline_PM25
    pm25_airdrie <- daily_data$Airdrie_PM25
    
    # Check if we have values from both stations
    if (length(pm25_caroline) > 0 && length(pm25_airdrie) > 0) {
      # Calculate MSD between Caroline and Airdrie stations
      msd_ca <- calculate_msd(pm25_caroline, pm25_airdrie)
      
      # Append the results to the data frame, including the Smoke condition
      msd_results <- rbind(msd_results, data.frame(Date = date, Average_MSD = msd_ca, Smoke = daily_data$Smoke[1]))
    }
  }
}

# Step 4: Print average MSD results for each date
print(msd_results)








### MAY CLUSTER 2 ###


### MAY DATES MSD ###

# Step 1: Load the data
data3 <- read.csv("Input/msd3.csv")

# Step 2: Function to calculate Mean Squared Difference (MSD)
calculate_msd <- function(vec1, vec2) {
  mean((vec1 - vec2)^2, na.rm = TRUE)
}

# Step 3: Calculate averaged MSD for each day

# Create a data frame to store MSD values for each day
msd_results <- data.frame(Date = character(), Average_MSD = numeric(), Smoke = integer(),
                          stringsAsFactors = FALSE)

# Get unique dates
dates <- unique(data3$Date)

# Loop through each date and calculate MSD between stations
for (date in dates) {
  # Filter data for the current date
  daily_data <- data3 %>% filter(Date == date)
  
  # Ensure there's data for the current date
  if (nrow(daily_data) > 0) {
    # Extract PM2.5 values for Redwater, St. Albert, and Power stations
    pm25_redwater <- daily_data$Redwater_PM25
    pm25_st_albert <- daily_data$St_Albert_PM25
    pm25_power <- daily_data$Power_PM25
    
    # Check if we have values from all stations
    if (length(pm25_redwater) > 0 && length(pm25_st_albert) > 0 && length(pm25_power) > 0) {
      # Calculate MSD between station pairs
      msd_rs <- calculate_msd(pm25_redwater, pm25_st_albert)  # Redwater vs St. Albert
      msd_rp <- calculate_msd(pm25_redwater, pm25_power)      # Redwater vs Power
      msd_sp <- calculate_msd(pm25_st_albert, pm25_power)     # St. Albert vs Power
      
      # Calculate the average MSD for the current date
      average_msd <- mean(c(msd_rs, msd_rp, msd_sp), na.rm = TRUE)
      
      # Append the results to the data frame with one average MSD per date
      msd_results <- rbind(msd_results, data.frame(Date = date, 
                                                   Average_MSD = average_msd, 
                                                   Smoke = daily_data$Smoke[1]))
    }
  }
}

# Step 4: Print average MSD results for each date
print(msd_results)

