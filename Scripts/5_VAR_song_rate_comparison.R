# ---
# title: "5_VAR_song_rate_comparison"
# author: "Leonard Patterson"
# created: "2026-08-01"
# description: 
# ---

# Clear environment
rm(list=ls())

# To install wildrtrax
library(remotes)
library(wildrtrax)
library(tidyverse)

# Authenticate with WildTrax using environment variables for credentials
config <- "Scripts/login.R"
source(config)
wt_auth()

### Download 10-min VAR project (project id = )
ten_min_VAR <- wt_get_projects("ARU") |>
  filter(project_id == "4733") |>
  pull(project_id) |>
  wt_download_report(sensor_id = "ARU", reports = "main")

# Save----
write.csv(ten_min_VAR, paste0("Input/Tabular Data/ten_min_VAR_main_report_", Sys.Date(), ".csv"), row.names = FALSE)




########################################################################################################################

# Clear env
rm(list = ls())

# Load tag data
dat1 <- read.csv("C:/Users/leona/OneDrive/Desktop/WTSP smoke/Input/Tabular Data/ten_min_VAR_main_report_2026-10-06.csv")

# Remove unnessary columns
dat2 <- dat1 %>%
  select(location,latitude,longitude,recording_date_time,species_code,vocalization,individual_order,abundance,detection_time)

# Create unique site-date-time id for summing tags
# Combine columns 1 and 2 to create recording_name file
dat2$recording_name <- paste(dat2$location, dat2$recording_date_time, sep = " ")

# Ensure abundance is numeric
dat2$abundance <- as.numeric(dat2$abundance)

# Subset into 1-, 3-, and 10-minute dataset
one_min <- dat2 %>%
  filter(detection_time < 60)
three_min <- dat2 %>%
  filter(detection_time < 180)
ten_min <- dat2

# Subset further to WTSP and YEWA
one_min_WTSP <- one_min %>%
  filter(species_code == "WTSP")
one_min_YEWA <- one_min %>%
  filter(species_code == "YEWA")

three_min_WTSP <- three_min %>%
  filter(species_code == "WTSP")
three_min_YEWA <- three_min %>%
  filter(species_code == "YEWA")

ten_min_WTSP <- ten_min %>%
  filter(species_code == "WTSP")
ten_min_YEWA <- ten_min %>%
  filter(species_code == "YEWA")

# Summarize abundance column by recording_name for each recording length
one_min_WTSP_1 <- one_min_WTSP %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))
one_min_YEWA_1 <- one_min_YEWA %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))

three_min_WTSP_1 <- three_min_WTSP %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))
three_min_YEWA_1 <- three_min_YEWA %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))

ten_min_WTSP_1 <- ten_min_WTSP %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))
ten_min_YEWA_1 <- ten_min_YEWA %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))

# Create VAR column for each df
one_min_WTSP_2 <- one_min_WTSP_1 %>%
  mutate(VAR_1m = total_abundance)
one_min_YEWA_2 <- one_min_YEWA_1 %>%
  mutate(VAR_1m = total_abundance)

three_min_WTSP_2 <- three_min_WTSP_1 %>%
  mutate(VAR_3m = total_abundance/3)
three_min_YEWA_2 <- three_min_YEWA_1 %>%
  mutate(VAR_3m = total_abundance/3)

ten_min_WTSP_2 <- ten_min_WTSP_1 %>%
  mutate(VAR_10m = total_abundance/10)
ten_min_YEWA_2 <- ten_min_YEWA_1 %>%
  mutate(VAR_10m = total_abundance/10)

# Drop total_abundance before merging VAR into per species-recording length dfs
one_min_WTSP_3 <- one_min_WTSP_2 %>%
  select(-total_abundance)
one_min_YEWA_3 <- one_min_YEWA_2 %>%
  select(-total_abundance)

three_min_WTSP_3 <- three_min_WTSP_2 %>%
  select(-total_abundance)
three_min_YEWA_3 <- three_min_YEWA_2 %>%
  select(-total_abundance)

ten_min_WTSP_3 <- ten_min_WTSP_2 %>%
  select(-total_abundance)
ten_min_YEWA_3 <- ten_min_YEWA_2 %>%
  select(-total_abundance)

# Join 1-, 3-, and 10-min VAR into dfs for WTSP and YEWA
WTSP_3_10 <- left_join(ten_min_WTSP_3, three_min_WTSP_3)
WTSP_all <- left_join(WTSP_3_10, one_min_WTSP_3)

YEWA_3_10 <- left_join(ten_min_YEWA_3, three_min_YEWA_3)
YEWA_all <- left_join(YEWA_3_10, one_min_YEWA_3)

# Replace sites NA sites with 0s
WTSP_all[is.na(WTSP_all)] <- 0
YEWA_all[is.na(YEWA_all)] <- 0

# Regress 1, 3, 10-minute VAR
model_WTSP_1_3 <- lm(VAR_3m ~ VAR_1m, data = WTSP_all)
model_WTSP_1_10 <- lm(VAR_10m ~ VAR_1m, data = WTSP_all)

model_YEWA_1_3 <- lm(VAR_3m ~ VAR_1m, data = YEWA_all)
model_YEWA_1_10 <- lm(VAR_10m ~ VAR_1m, data = YEWA_all)

# Model summary
summary(model_WTSP_1_3)
summary(model_WTSP_1_10)
summary(model_YEWA_1_3)
summary(model_YEWA_1_10)

# Pull out R-squared values (rounded to 3 decimals)
r2_WTSP_1_3 <- round(summary(model_WTSP_1_3)$r.squared, 3)
r2_WTSP_1_10 <- round(summary(model_WTSP_1_10)$r.squared, 3)
r2_YEWA_1_3 <- round(summary(model_YEWA_1_3)$r.squared, 3)
r2_YEWA_1_10 <- round(summary(model_YEWA_1_10)$r.squared, 3)

# Pearson's correlation coefficient (r) for each pair (rounded to 3 decimals)
r_WTSP_1_3 <- round(cor(WTSP_all$VAR_1m, WTSP_all$VAR_3m, method = "pearson"), 3)
r_WTSP_1_10 <- round(cor(WTSP_all$VAR_1m, WTSP_all$VAR_10m, method = "pearson"), 3)
r_YEWA_1_3 <- round(cor(YEWA_all$VAR_1m, YEWA_all$VAR_3m, method = "pearson"), 3)
r_YEWA_1_10 <- round(cor(YEWA_all$VAR_1m, YEWA_all$VAR_10m, method = "pearson"), 3)

# Plots

# Open a PNG file to draw into 
png("Figures/VAR_regressions.png", width = 10, height = 8, units = "in", res = 300)

# Split the window into 2 rows x 2 columns
par(mfrow = c(2, 2))

# WTSP: 3-min vs 1-min
plot(WTSP_all$VAR_3m ~ WTSP_all$VAR_1m,
     xlab = "VAR 1-min", ylab = "VAR 3-min", main = "WTSP: 3-min vs 1-min")
abline(model_WTSP_1_3, col = "red", lwd = 2)
legend("topleft",
       legend = as.expression(c(bquote(r == .(r_WTSP_1_3)),
                                bquote(R^2 == .(r2_WTSP_1_3)), 
                                bquote(n == .(nrow(WTSP_all))))),
       bty = "n")

# WTSP: 10-min vs 1-min
plot(WTSP_all$VAR_10m ~ WTSP_all$VAR_1m,
     xlab = "VAR 1-min", ylab = "VAR 10-min", main = "WTSP: 10-min vs 1-min")
abline(model_WTSP_1_10, col = "red", lwd = 2)
legend("topleft",
       legend = as.expression(c(bquote(r == .(r_WTSP_1_10)),
                                bquote(R^2 == .(r2_WTSP_1_10)),  bquote(n == .(nrow(WTSP_all))))),
       bty = "n")

# YEWA: 3-min vs 1-min
plot(YEWA_all$VAR_3m ~ YEWA_all$VAR_1m,
     xlab = "VAR 1-min", ylab = "VAR 3-min", main = "YEWA: 3-min vs 1-min")
abline(model_YEWA_1_3, col = "red", lwd = 2)
legend("topleft",
       legend = as.expression(c(bquote(r == .(r_YEWA_1_3)),
                                bquote(R^2 == .(r2_YEWA_1_3)),
                                bquote(n == .(nrow(YEWA_all))))),
       bty = "n")

# YEWA: 10-min vs 1-min
plot(YEWA_all$VAR_10m ~ YEWA_all$VAR_1m,
     xlab = "VAR 1-min", ylab = "VAR 10-min", main = "YEWA: 10-min vs 1-min")
abline(model_YEWA_1_10, col = "red", lwd = 2)
legend("topleft",
       legend = as.expression(c(bquote(r == .(r_YEWA_1_10)),
                                bquote(R^2 == .(r2_YEWA_1_10)),
                                bquote(n == .(nrow(YEWA_all))))),
       bty = "n")

# Close the file (this is what actually writes it to disk)
dev.off()

# Reset the layout back to one plot per window
par(mfrow = c(1, 1))