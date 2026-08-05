library(tidyverse)
library(dplyr)


### TTEST, CALCULATING BY SPLITTING DATA INTO 2 GROUPS:
### 1) PAIRS OF RECORDINGS IN WHICH NON-SMOKY OCCURS BEFORE SMOKY and 2) PAIRS OF RECORDINGS IN WHICH NON-SMOKY OCCURS AFTER SMOKY 


tags1 <- read.csv("Output/tags.csv")

# Remove 1266-SE  (started transcribing but decided too remove due to too much uncertainty in trancription)

tags1 <- tags1 %>% filter(location != "1266-SE")

# Split recording_name to create a new "date" column

tags1 <- transform(tags1, date = sub("^[^ ]+ ", "", recording_name))

# Assign 'smoky' and 'non-smoky' to respective dates

tags1$treatment <- ifelse(tags1$date %in% c("2023-05-21", "2023-06-11"), "smoky", 
                          ifelse(tags1$date %in% c("2023-05-24", "2023-05-15", "2023-06-09", "2023-06-13"), 
                                 "non-smoky", NA))

# Filter out rows where both of the recordings in a pair are equal to zero (no singing )

tags2 <- tags1 %>%
  group_by(location) %>%
  filter(!(all(mean_singing_rate == 0)))

# Split data two 'before_smoky' and 'after_smoky' groupings

split_pairs <- function(df) {
  df_pairs <- split(df, f = rep(1:(nrow(df) / 2), each = 2))
  return(df_pairs)
}
pairs <- split_pairs(tags2)
before_nonsmoky <- bind_rows(lapply(pairs, function(x) {
  x <- arrange(x, location)
  if (x$treatment[1] == "non-smoky") {
    return(x)
  }
}))
after_nonsmoky <- bind_rows(lapply(pairs, function(x) {
  x <- arrange(x, location)
  if (x$treatment[1] == "smoky") {
    return(x)
  }
}))





### BEFORE SMOKY TRANSFORMATIONS ####

# Filter out 'smoky' day recordings from 'before smoky pairs'

smoky1 <- before_nonsmoky %>%
  filter(grepl("21$|11$", date))

# Filter out 'non-smoky' day recordings from 'before smoky pairs'

non_smoky1 <- before_nonsmoky %>%
  filter(grepl("15$|9$", date))

# Clean smoky and non-smoky data frames

smoky1 <- smoky1 %>%
  dplyr::select(recording_name,mean_singing_rate)

non_smoky1 <- non_smoky1 %>%
  dplyr::select(recording_name,mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

smoky1 <- smoky1 %>%
  rename(mean_SR_smoky = mean_singing_rate)

# Change name of "mean_singing_rate" for non-smoky day recordings

non_smoky1 <- non_smoky1 %>%
  rename(mean_SR_non_smoky = mean_singing_rate)

# Split the first column of both files so that they can be merged based on location

smoky1 <- smoky1 %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

non_smoky1 <- non_smoky1 %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

# Remove time column

smoky1 <- smoky1 %>%
  dplyr::select(Location, mean_SR_smoky)

non_smoky1 <- non_smoky1 %>%
  dplyr::select(Location, mean_SR_non_smoky)

# Merge smoky and non-smoky data frames based on location

before_nonsmoky <- merge(smoky1, non_smoky1, by = "Location")

# Calculate the differences between the smoky and non-smoky singing rates

before_nonsmoky$difference <- before_nonsmoky$mean_SR_smoky - before_nonsmoky$mean_SR_non_smoky

# BEFORE SMOKY Paired t-test

before_pairedttest <- t.test(before_nonsmoky$mean_SR_smoky, before_nonsmoky$mean_SR_non_smoky, paired = TRUE)

# View paired t-test result

before_pairedttest












### Create side-by-side histograms

par(mfrow=c(1,2))  # Set the plotting layout to 1 row, 2 columns

# Smoky histogram
hist_vals_smoky <- hist(before_nonsmoky$mean_SR_smoky, 
                        breaks = seq(floor(min(before_nonsmoky$mean_SR_smoky)), 
                                     ceiling(max(before_nonsmoky$mean_SR_smoky)), 0.5), 
                        plot = FALSE)
plot(hist_vals_smoky, col = "light blue", border = "black", main = "Smoky",
     xlab = "Singing Rate", ylab = "Number of Sites", ylim = c(0, max(hist_vals_smoky$counts, na.rm = TRUE)),
     cex.axis = 1.2, cex.lab = 1.4)  # Adjusting axis and label sizes

# Non-smoky histogram
hist_vals_non_smoky <- hist(before_nonsmoky$mean_SR_non_smoky, 
                            breaks = seq(floor(min(before_nonsmoky$mean_SR_non_smoky)), 
                                         ceiling(max(before_nonsmoky$mean_SR_non_smoky)), 0.5), 
                            plot = FALSE)
plot(hist_vals_non_smoky, col = "light blue", border = "black", main = "Non-smoky",
     xlab = "Singing Rate", ylab = "Number of Sites", ylim = c(0, max(hist_vals_non_smoky$counts, na.rm = TRUE)),
     cex.axis = 1.2, cex.lab = 1.4)  # Adjusting axis and label sizes

# Define file path
file_path <- "Graphics/before non-smoky.png"

# Export the plot as a PNG file
png(file_path, width = 800, height = 400)  # Adjust width and height as needed

# Redraw the plot
par(mfrow=c(1,2))  # Set the plotting layout to 1 row, 2 columns

# Smoky histogram
hist_vals_smoky <- hist(before_nonsmoky$mean_SR_smoky, 
                        breaks = seq(floor(min(before_nonsmoky$mean_SR_smoky)), 
                                     ceiling(max(before_nonsmoky$mean_SR_smoky)), 0.5), 
                        plot = FALSE)
plot(hist_vals_smoky, col = "light blue", border = "black", main = "Smoky",
     xlab = "Singing Rate", ylab = "Number of Sites", ylim = c(0, max(hist_vals_smoky$counts, na.rm = TRUE)),
     cex.axis = 1.2, cex.lab = 1.4)  # Adjusting axis and label sizes

# Non-smoky histogram
hist_vals_non_smoky <- hist(before_nonsmoky$mean_SR_non_smoky, 
                            breaks = seq(floor(min(before_nonsmoky$mean_SR_non_smoky)), 
                                         ceiling(max(before_nonsmoky$mean_SR_non_smoky)), 0.5), 
                            plot = FALSE)
plot(hist_vals_non_smoky, col = "light blue", border = "black", main = "Non-smoky",
     xlab = "Singing Rate", ylab = "Number of Sites", ylim = c(0, max(hist_vals_non_smoky$counts, na.rm = TRUE)),
     cex.axis = 1.2, cex.lab = 1.4)  # Adjusting axis and label sizes

dev.off()  # Close the PNG device













### AFTER SMOKY TRANSFORMATIONS ####

# Filter out "smoky" day recordings

smoky2 <- after_nonsmoky %>%
  filter(grepl("21$|11$", date))

# Filter out "non-smoky" day recordings

non_smoky2 <- after_nonsmoky %>%
  filter(grepl("24$|13$", date))

# Clean smoky and non-smoky data frames

smoky2 <- smoky2 %>%
  dplyr::select(recording_name,mean_singing_rate)

non_smoky2 <- non_smoky2 %>%
  dplyr::select(recording_name,mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

smoky2 <- smoky2 %>%
  rename(mean_SR_smoky = mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

non_smoky2 <- non_smoky2 %>%
  rename(mean_SR_non_smoky = mean_singing_rate)

# Split the first column of both files so that they can be merged based on location

smoky2 <- smoky2 %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

non_smoky2 <- non_smoky2 %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

# Remove time column

smoky2 <- smoky2 %>%
  dplyr::select(Location, mean_SR_smoky)

non_smoky2 <- non_smoky2 %>%
  dplyr::select(Location, mean_SR_non_smoky)

# Merge smoky and non-smoky data frames based on location

after_nonsmoky <- merge(smoky2, non_smoky2, by = "Location")

# Calculate the differences between the smoky and non-smoky singing rates

after_nonsmoky$difference <- after_nonsmoky$mean_SR_smoky - after_nonsmoky$mean_SR_non_smoky

# AFTER SMOKY Paired t-test

after_pairedttest <- t.test(after_nonsmoky$mean_SR_smoky, after_nonsmoky$mean_SR_non_smoky, paired = TRUE)

# View paired t-test result

after_pairedttest






### Create side-by-side histograms

par(mfrow=c(1,2))  # Set the plotting layout to 1 row, 2 columns

# Smoky histogram
hist_vals_smoky <- hist(after_nonsmoky$mean_SR_smoky, 
                        breaks = seq(floor(min(after_nonsmoky$mean_SR_smoky)), 
                                     ceiling(max(after_nonsmoky$mean_SR_smoky)), 0.5), 
                        plot = FALSE)
plot(hist_vals_smoky, col = "light blue", border = "black", main = "Smoky",
     xlab = "Singing Rate", ylab = "Number of Sites", ylim = c(0, max(hist_vals_smoky$counts, na.rm = TRUE)),
     cex.axis = 1.2, cex.lab = 1.4)  # Adjusting axis and label sizes

# Non-smoky histogram
hist_vals_non_smoky <- hist(after_nonsmoky$mean_SR_non_smoky, 
                            breaks = seq(floor(min(after_nonsmoky$mean_SR_non_smoky)), 
                                         ceiling(max(after_nonsmoky$mean_SR_non_smoky)), 0.5), 
                            plot = FALSE)
plot(hist_vals_non_smoky, col = "light blue", border = "black", main = "Non-smoky",
     xlab = "Singing Rate", ylab = "Number of Sites", ylim = c(0, max(hist_vals_non_smoky$counts, na.rm = TRUE)),
     cex.axis = 1.2, cex.lab = 1.4)  # Adjusting axis and label sizes

# Define file path
file_path <- "Graphics/after non-smoky.png"

# Export the plot as a PNG file
png(file_path, width = 800, height = 400)  # Adjust width and height as needed

# Redraw the plot
par(mfrow=c(1,2))  # Set the plotting layout to 1 row, 2 columns

# Smoky histogram
hist_vals_smoky <- hist(after_nonsmoky$mean_SR_smoky, 
                        breaks = seq(floor(min(after_nonsmoky$mean_SR_smoky)), 
                                     ceiling(max(after_nonsmoky$mean_SR_smoky)), 0.5), 
                        plot = FALSE)
plot(hist_vals_smoky, col = "light blue", border = "black", main = "Smoky",
     xlab = "Singing Rate", ylab = "Number of Sites", ylim = c(0, max(hist_vals_smoky$counts, na.rm = TRUE)),
     cex.axis = 1.2, cex.lab = 1.4)  # Adjusting axis and label sizes

# Non-smoky histogram
hist_vals_non_smoky <- hist(after_nonsmoky$mean_SR_non_smoky, 
                            breaks = seq(floor(min(after_nonsmoky$mean_SR_non_smoky)), 
                                         ceiling(max(after_nonsmoky$mean_SR_non_smoky)), 0.5), 
                            plot = FALSE)
plot(hist_vals_non_smoky, col = "light blue", border = "black", main = "Non-smoky",
     xlab = "Singing Rate", ylab = "Number of Sites", ylim = c(0, max(hist_vals_non_smoky$counts, na.rm = TRUE)),
     cex.axis = 1.2, cex.lab = 1.4)  # Adjusting axis and label sizes

dev.off()  # Close the PNG device



