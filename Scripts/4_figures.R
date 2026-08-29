# ---
# title: "3_Figures"
# author: "Leonard Patterson"
# created: "2026-08-01"
# description: 
# ---

# Clear environment
rm(list=ls())

# Load packages
library(dplyr)
library(ggplot2)
library(grid)  # for unit() in the color bar
library(patchwork)  # for combining plots
library(sf)

###############################################
################# WTSP ########################
###############################################

# Load data
WTSP <- read.csv("Output/Tabular Data/WTSP_with_smoke.csv")

# Convert date column to julian day
WTSP2 <- WTSP %>%
  mutate(julian_day = as.numeric(format(as.Date(date), "%j")))

# Make plot
WTSP2$smoke <- ifelse(WTSP2$julian_day %in% c(141, 162), "Smoke", "No Smoke")

p1 <- ggplot(WTSP2, aes(julian_day, pm25, color = smoke)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("Smoke" = "red", "No Smoke" = "black"),
                     labels = c("Smoke" = "Smoky", "No Smoke" = "Non-smoky")) +
  scale_x_continuous(breaks = seq(0, 366, by = 5)) +
  scale_y_continuous(breaks = seq(0, 400, by = 50),
                     expand = expansion(mult = c(0, 0.02)),
                     limits = c(0, max(WTSP2$pm25, na.rm = TRUE) * 1.05)) +
  labs(x = "Julian Day",
       y = expression(PM[2.5]~"("*mu*g/m^3*")")) +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_blank())

p1






###############################################
################# YEWA ########################
###############################################

# Load data
YEWA <- read.csv("Output/Tabular Data/YEWA_with_smoke.csv")

# Convert date column to julian day
YEWA2 <- YEWA %>%
  mutate(julian_day = as.numeric(format(as.Date(date), "%j")))

# Make plot
YEWA2$smoke <- ifelse(YEWA2$julian_day %in% c(161, 162), "Smoke", "No Smoke")

p2 <- ggplot(YEWA2, aes(julian_day, pm25, color = smoke)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("Smoke" = "red", "No Smoke" = "black"),
                     labels = c("Smoke" = "Smoky", "No Smoke" = "Non-smoky")) +
  scale_x_continuous(breaks = seq(0, 366, by = 5)) +
  scale_y_continuous(breaks = seq(0, 400, by = 50),
                     expand = expansion(mult = c(0, 0.02)),
                     limits = c(0, max(YEWA2$pm25, na.rm = TRUE) * 1.05)) +
  labs(x = "Julian Day",
       y = expression(PM[2.5]~"("*mu*g/m^3*")")) +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_blank())

p2






### Combined plots ###

# Clear environment
rm(list=ls())


###############################################
################# WTSP ########################
###############################################
# Load data
WTSP <- read.csv("Output/Tabular Data/WTSP_with_smoke.csv")
# Convert date column to julian day
WTSP2 <- WTSP %>%
  mutate(julian_day = as.numeric(format(as.Date(date), "%j")))
# Make plot
WTSP2$smoke <- ifelse(WTSP2$julian_day %in% c(141, 162), "Smoke", "No Smoke")
p1 <- ggplot(WTSP2, aes(julian_day, pm25, color = smoke)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("Smoke" = "red", "No Smoke" = "black"),
                     labels = c("Smoke" = "Smoky", "No Smoke" = "Non-smoky")) +
  scale_x_continuous(breaks = seq(0, 366, by = 5)) +
  scale_y_continuous(breaks = seq(0, 400, by = 50),
                     expand = expansion(mult = c(0, 0.02)),
                     limits = c(0, max(WTSP2$pm25, na.rm = TRUE) * 1.05)) +
  labs(x = "Julian Day",
       y = expression(PM[2.5]~"("*mu*g/m^3*")")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text  = element_text(size = 14),
        axis.title = element_text(size = 14),
        panel.grid = element_blank())

###############################################
################# YEWA ########################
###############################################
# Load data
YEWA <- read.csv("Output/Tabular Data/YEWA_with_smoke.csv")
# Convert date column to julian day
YEWA2 <- YEWA %>%
  mutate(julian_day = as.numeric(format(as.Date(date), "%j")))
# Make plot
YEWA2$smoke <- ifelse(YEWA2$julian_day %in% c(161, 162), "Smoke", "No Smoke")
p2 <- ggplot(YEWA2, aes(julian_day, pm25, color = smoke)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("Smoke" = "red", "No Smoke" = "black"),
                     labels = c("Smoke" = "Smoky", "No Smoke" = "Non-smoky")) +
  scale_x_continuous(breaks = seq(0, 366, by = 5)) +
  scale_y_continuous(breaks = seq(0, 400, by = 50),
                     expand = expansion(mult = c(0, 0.02)),
                     limits = c(0, max(YEWA2$pm25, na.rm = TRUE) * 1.05)) +
  labs(x = "Julian Day",
       y = expression(PM[2.5]~"("*mu*g/m^3*")")) +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.text  = element_text(size = 14),
        legend.background = element_rect(fill = "white", color = "black"),
        axis.text  = element_text(size = 14),
        axis.title = element_text(size = 14),
        panel.grid = element_blank())

###############################################
############### COMBINE #######################
###############################################
p3 <- p1 + p2
p3

# Save figure
ggsave(
  "Figures/smoke_PM25_by_date.png",
  plot   = p3,
  device = agg_png,
  width  = 12,
  height = 7,
  units  = "in",
  dpi    = 600
)

