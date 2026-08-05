library(dplyr)
library(readr)

dat1 <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP tasks and tags/Single Species_ WhiteThroated Sparrow (WTSP) - Sound Rates - Wildfire smoke - 2023 - Patterson_Tasks_202427.csv")

# Filter
dat2 <- dat1 %>%
  filter(status == "Transcribed")

write.csv(dat2, "Output/tasks_for_wildtrax.csv")


dat1 <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP tasks and tags/tags_for_wildtrax.csv")

dat3 <- dat1 %>%
  mutate(
    min_tag_freq = parse_number(min_tag_freq) * 1000,
    max_tag_freq = parse_number(max_tag_freq) * 1000
  )
write.csv(dat3, "C:/Users/leona/OneDrive/Desktop/WTSP tasks and tags/tags_for_wildtrax_2.csv")






######## subset for wildtrax upload
dat1 <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP tasks and tags/LP_WTSP_wildtrax_tags.csv")

# first instance of each individual_number within each
# location-recording_date_time combination
dat1_first <- dat1 %>%
  distinct(location, recording_date_time, individual_number, .keep_all = TRUE)

# all remaining rows
dat1_remaining <- dat1 %>%
  anti_join(
    dat1_first %>%
      mutate(.row_id = row_number()),
    by = colnames(dat1)
  )

write.csv(dat1_first, "C:/Users/leona/OneDrive/Desktop/WTSP tasks and tags/WTSP_tags_subset_1.csv")
write.csv(dat1_remaining, "C:/Users/leona/OneDrive/Desktop/WTSP tasks and tags/WTSP_tags_subset_2.csv")


