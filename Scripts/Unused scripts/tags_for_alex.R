
tags <- read.csv("Input/ABMI_Influence_of_wildfire_smoke_on_avian_singing_rates_tag_report.csv")

tags <- tags %>%
  dplyr::select(location,recording_date_time,species_code,aru_task_status,individual_order,abundance,vocalization)

# Remove rows with duplicate combinations of 'Location' and 'Date and Time'
tags <- tags[!duplicated(tags[, c("location", "recording_date_time")]), ]


# Create a new dataframe with the desired structure
tags <- data.frame(
  location = tags$location,
  recordingDate = tags$recording_date_time,
  method = "None",           # Set to "None" for all rows
  taskLength = 180,          # Set to 180 for all rows
  status = "",               # Set to blank for all rows
  transcriber = "",          # Placeholder as blank
  rain = "",                 # Placeholder as blank
  wind = "",                 # Placeholder as blank
  industryNoise = "",        # Placeholder as blank
  otherNoise = "",           # Placeholder as blank
  taskComments = "",         # Placeholder as blank
  internal_task_id = "",     # Placeholder as blank
  audioQuality = ""          # Placeholder as blank
)


# Remove the rows after 98
tags <- tags[1:98, ]

write.csv(tags, "Output/tags_for_alex.csv")














