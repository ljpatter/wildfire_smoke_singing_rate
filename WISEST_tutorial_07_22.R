# Hi Hannah, here is the couple lines of code we talked about yesterday: 

# Load packages
library(dplyr)

# Load main report 
WTSP <- read.csv("Input/Tabular Data/WTSP_main_report.csv") # You will need to change this to the file path on your computer

# Use select() function to remove columns that are not relevant
WTSP1 <- WTSP %>%
  select(location,recording_date_time,species_code,task_is_complete,observer,max_noise_volume,max_noise_type,individual_order,abundance,vocalization)

# Combine "columns"location" and "recording_date_time" values to create a new column (will use this later)
WTSP1$recording_name <- paste(WTSP1$location, WTSP1$recording_date_time, sep = " ")

# Use filter() function to select only "Song" tags (i.e., remove all WTSP call tags)
WTSP2 <- WTSP1 %>%
  filter(vocalization == "Song")

# Use mutate() function to take all instances of "NONE" (i.e., recording where no WTSP were present) and 
# replace values in "individual_order" and "abundance" with zero - this is important for later calculations
# mutate() is a very common function - good idea for read more about what it can do here: https://dplyr.tidyverse.org/reference/mutate.html
WTSP3 <- WTSP2 %>%
  mutate(
    individual_order = ifelse(species_code == "NONE", 0, individual_order),
    abundance = ifelse(species_code == "NONE", 0, abundance)
  )

# Use mutate() again for change all "NONE" tags to "WTSP" - when combined with the previous code, this will convert
# all "NONE" tags with an abundance of 1 and individual of 1 to WTSP, abundance 0, and indivdual order of 0 
WTSP4 <- WTSP3 %>%
  mutate(species_code = ifelse(species_code == "NONE", "WTSP", species_code))

## Right now, our data is still in long form (a row for every single tags). For analysis, we want a single row for each recording (i.e., three
## recordings per site, per day). This code uses the summarize() function (also from dplyr) to first group of data by the recording_name
## variable we created earlier (group_by() function) and then summarizes by the "Abundance" column, creating a new column called total_abundance.
## This code also counts the max number of individuals in a recording and stores this in a new column called n_individuals:
WTSP5 <- WTSP4 %>%
  group_by(recording_name) %>%
  summarise(
    n_individuals   = max(individual_order),
    total_abundance = sum(abundance),
    .groups = "drop"
  )


## A couple of errors I noticed in our data that we want to correct before running the summarize code above: 1) there are a couple of recordings 
## where the "abundance" value was entered as 2 - can you track down these recordings and confirm they were tagged correctly? You can find these 
## recordings using the filter() function (hint - we want to find any rows where abundance == 2). After you make these changes, you will need to 
## re-download the csv file and rerun the code; 2) we need to remove all tags from our data where there were "High" levels of wind/rain (this is 
## stored in the "max_noise_volume" (we may have removed this when we ran our select() code earlier so will need to add that back); 3) lastly, we 
## need to create a column that indicates if a recording was collected during a smoky vs. non-smoky day. All of our smoky recordings are from May 21 for ABMI sites and June 11 for FHP sites and non-smoky are either May 15 or 24
## 24 for ABMI sites and June 9 or 13 for FHP sites. This last step is quite tricky so I recommend asking Google Gemini to help you with a line of
## code to do this.

WTSP6 <- WTSP5 %>%
  mutate(
    rec_date = as.Date(substr(recording_name, nchar(recording_name) - 18, nchar(recording_name) - 9)),
    smoke_status = case_when(
      rec_date %in% as.Date(c("2023-05-21", "2023-06-11")) ~ "smoky",
      rec_date %in% as.Date(c("2023-05-15", "2023-05-24", "2023-06-09", "2023-06-13")) ~ "non-smoky",
      TRUE ~ NA_character_
    )
  )
