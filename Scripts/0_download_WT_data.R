# ---
# title: "0_Get WT reports"
# author: "Leonard Patterson"
# created: "2026-08-01"
# description: This code downloads point count data from WildTrax
# ---

# Clear environment
rm(list=ls())

# To install wildrtrax
library(remotes)
library(wildrtrax)
library(dplyr)

# Authenticate with WildTrax using environment variables for credentials
config <- "Scripts/login.R"
source(config)
wt_auth()


### Download ARU projects (WTSP project_id is 2203 and YEWA is 3778)

# WTSP
WTSP <- wt_get_projects("ARU") |>
  filter(project_id == "2203") |>
  pull(project_id) |>
  wt_download_report(sensor_id = "ARU", reports = "main")

# YEWA
YEWA <- wt_get_projects("ARU") |>
  filter(project_id == "3778") |>
  pull(project_id) |>
  wt_download_report(sensor_id = "ARU", reports = "main")


# Save----
write.csv(WTSP, paste0("Input/Tabular Data/WTSP_main_report_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv(YEWA, paste0("Input/Tabular Data/YEWA_main_report_", Sys.Date(), ".csv"), row.names = FALSE)
