library(dplyr)

# Load csv file (I got this file path my right clicking on the file in Windows Explorer 
# and selecting "Copy as Path")
dat1 <- read.csv("C:/Users/leona/OneDrive/Desktop/Single_Species_-_WhiteThroated_Sparrow_(WTSP)_-_Sound_Rates_-_Wildfire_smoke_-_2023_-_Patterson_Tags_2026-07-07.csv")

# Filter columns
dat2 <- dat1 %>%
  select(location, recording_date_time, observer, species_code, individual_number,
         vocalization, abundance)

# Filter to LP
dat3 <- dat2 %>%
  filter(observer == "Leonard Patterson")

# Get unique recordings
dat4 <- dat3 %>%
  distinct(location, recording_date_time)

# Select 20 recordings randomly
set.seed(123)  # Optional

dat4_sample <- dat4 %>%
  slice_sample(n = 20)

# Save
write.csv(dat4_sample, "Output/20_random_recordings.csv", row.names = FALSE)




######## Compare 1 vs. 3 min

library(dplyr)
library(tidyr)
library(ggplot2)

# Read data
dat1 <- read.csv("Input/3_min_recordings.csv")

# Filter columns
dat2 <- dat1 %>%
  select(location, recording_date_time, observer, species_code, individual_number,
         tag_start_time, abundance, species_individual_comments)

# Ensure tag_start_time is numeric (TMTT rows have NA here)
dat2 <- dat2 %>%
  mutate(tag_start_time = suppressWarnings(as.numeric(tag_start_time)))

# Flag recordings with no WTSP (TMTT/NONE)
none_flag <- dat2 %>%
  group_by(location, recording_date_time) %>%
  summarise(no_wtsp = any(abundance == "TMTT" &
                            species_individual_comments == "NONE"),
            .groups = "drop")

# Count songs per recording within each time window
rates <- dat2 %>%
  filter(!(abundance == "TMTT" & species_individual_comments == "NONE")) %>%
  group_by(location, recording_date_time) %>%
  summarise(
    n_1min = sum(tag_start_time < 60,  na.rm = TRUE),
    n_3min = sum(tag_start_time < 180, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rate_1min = n_1min / 1,
    rate_3min = n_3min / 3
  )

# Merge in NONE recordings and set their rates to zero
wide <- none_flag %>%
  left_join(rates, by = c("location", "recording_date_time")) %>%
  mutate(
    rate_1min = ifelse(no_wtsp, 0, rate_1min),
    rate_3min = ifelse(no_wtsp, 0, rate_3min)
  ) %>%
  select(location, recording_date_time, rate_1min, rate_3min)

# Linear regression: 1-min rate ~ 3-min rate
fit <- lm(rate_1min ~ rate_3min, data = wide)
summary(fit)
r2 <- summary(fit)$r.squared

# Plot
ggplot(wide, aes(x = rate_3min, y = rate_1min)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = paste0("R^2 == ", round(r2, 3)), parse = TRUE) +
  labs(
    x = "WTSP songs per minute (3-min recording)",
    y = "WTSP songs per minute (1-min recording)",
    title = "WTSP song rate: 1-min vs 3-min recordings"
  ) +
  theme_minimal()
