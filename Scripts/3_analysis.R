# ---
# title: "3_Analysis"
# author: "Leonard Patterson"
# created: "2026-08-01"
# description: This code runs analysis for WTSP and YEWA, starting with a paired t-test, followed up
#              by a GLMM with a NB1 distribution, following by code for assessing different functional
#              forms for TOD.
# ---

# Clear environment
rm(list=ls())

# Load packages
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(broom.mixed)
library(tidyverse)
library(patchwork)
library(suncalc)
library(ragg)

##################################
############# WTSP ###############
##################################

# Load data
WTSP <- read.csv("Input/Tabular Data/WTSP_data_for_analysis_2026-08-05.csv")

#### PAIRED T-TEST ####

# Format for paired t-test
WTSP_paired <- WTSP %>%
  group_by(site, smoke_status) %>%
  summarise(var = mean(total_abundance), n_rec = n(), .groups = "drop") %>%
  mutate(smoke_status = ifelse(smoke_status == "non-smoky", "nonsmoky", "smoky")) %>%
  pivot_wider(names_from = smoke_status, values_from = c(var, n_rec)) %>%
  mutate(diff = var_smoky - var_nonsmoky)

# Confirm every site is a complete 3-and-3 pair before testing
nrow(WTSP_paired)
table(WTSP_paired$n_rec_smoky, WTSP_paired$n_rec_nonsmoky, useNA = "ifany")
WTSP_paired %>% filter(is.na(var_smoky) | is.na(var_nonsmoky))

# Paired t-test
WTSP_paired_ttest <- t.test(WTSP_paired$var_smoky, WTSP_paired$var_nonsmoky, paired = TRUE)
print(WTSP_paired_ttest)

# Create hist of differences
ggplot(WTSP_paired, aes(x = diff)) +
  geom_histogram(bins = 15, colour = "black", fill = "grey70") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Change in VAR (songs/min)", y = "Number of sites") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    legend.position  = "none",
    strip.text       = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y      = element_text(face = "italic"),
    plot.background  = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Assess residuals using Shaprio-Wilk
shapiro.test(WTSP_paired$diff)

# Q-Q plot to assess normality
ggplot(WTSP_paired, aes(sample = diff)) +
  stat_qq() + stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") + theme_bw()
# Differences not normally distributed

### Plot paired t-test results ###

# ---- shared setup ----
WTSP_paired$id   <- seq_len(nrow(WTSP_paired))
WTSP_paired$diff <- WTSP_paired$var_smoky - WTSP_paired$var_nonsmoky

long <- pivot_longer(WTSP_paired,
                     cols = c(var_smoky, var_nonsmoky),
                     names_to = "condition", values_to = "value")
long$condition <- factor(long$condition,
                         levels = c("var_nonsmoky", "var_smoky"),
                         labels = c("Non-smoky", "Smoky"))

diff_df <- data.frame(
  label    = "Smoky − Non-smoky",
  estimate = -1.156028,
  lower    = -1.8716994,
  upper    = -0.4403574
)


# ---- 1. Connected paired plot (raw data + pairing + group means) ----
p1 <- ggplot(long, aes(condition, value, group = id)) +
  geom_line(alpha = 0.25) +
  geom_point(alpha = 0.5) +
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               color = "red", linewidth = 1) +
  stat_summary(aes(group = 1), fun = mean, geom = "point",
               color = "red", size = 3) +
  labs(x = NULL, y = "Songs per minute") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(size = 14, color = "black")
  )

print(p1)


# ---- 2. Histogram of differences ----
p2 <- ggplot(WTSP_paired, aes(diff)) +
  geom_histogram(binwidth = 1, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean(WTSP_paired$diff), color = "red") +
  labs(x = "Difference (smoky − non-smoky), songs/min",
       y = "Number of sites",
       subtitle = "Paired differences; red = mean, dashed = 0") +
  theme_minimal()


# ---- 3. Forest / interval plot: mean difference + CI vs 0 ----
p3 <- ggplot(diff_df, aes(estimate, label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1) +
  geom_point(size = 3) +
  labs(x = "Mean difference (95% CI), songs/min", y = NULL) +
  theme_minimal()


# ---- print individually ----
p1; p2; p3








######## GLMM #########

# Ensure site, date, and smoke_status are factors
dat <- WTSP %>% mutate(
  site         = factor(site),
  date         = factor(date),
  smoke_status = factor(smoke_status, levels = c("non-smoky", "smoky"))
)

# Capitalize smoky and non-smoky for figures later on
dat1 <- dat %>%
  mutate(smoke_status = factor(smoke_status,
                               levels = c("non-smoky", "smoky"),
                               labels = c("Non-smoky", "Smoky")))


## Lat/lon are missing for FHP sites. Join lat/long values back to calculate time since 
## sunrise (tssr) covariates

# Read in WTSP df with lat/lon
latlon <- read.csv("Output/Tabular Data/latlong_to_join.csv")

# Filter latlon df to only contain rows that are present in the dat1 df
latlon2 <- latlon %>%
  filter(location %in% dat$site) %>%
  mutate(site = location) %>%
  select(-X, -location)

# Remove lat/lon from dat 1
dat2 <- dat1 %>%
  select(-latitude, -longitude)

# Left join latlon2 to dat1
dat3 <- left_join(dat2, latlon2, by = "site", relationship = "many-to-one")

# Get sunrise times (renaming columns to 'lat' and 'lon')
sun <- getSunlightTimes(
  data = dat3 %>% 
    transmute(
      date = as.Date(as.character(date)), 
      lat = latitude, 
      lon = longitude
    ),
  keep = "sunrise", 
  tz = "America/Edmonton"
)

# Create a datetime column and calculate TSSR
dat4 <- dat3 %>%
  mutate(
    # Combine date and time into a single datetime object
    recording_datetime = as.POSIXct(
      paste(as.character(date), time), 
      tz = "America/Edmonton"
    ),
    # Attach calculated sunrise
    sunrise = sun$sunrise,
    # Calculate Time Since Sunrise in hours (change to units = "mins" if needed)
    tssr = as.numeric(difftime(recording_datetime, sunrise, units = "hours"))
  )

### Diagnostics to select error family via AIC

# Specify model
form <- total_abundance ~ smoke_status + tssr + (1 | site)

safe_fit <- function(...) tryCatch(glmmTMB(...), error = function(e) NULL)

fits <- list(
  poisson = safe_fit(form, family = poisson,  data = dat4),
  nbinom1 = safe_fit(form, family = nbinom1,  data = dat4),
  nbinom2 = safe_fit(form, family = nbinom2,  data = dat4),
  genpois = safe_fit(form, family = genpois,  data = dat4),
  compois = safe_fit(form, family = compois,  data = dat4),
  zip     = safe_fit(form, family = poisson,  ziformula = ~1, data = dat4),
  zinb2   = safe_fit(form, family = nbinom2,  ziformula = ~1, data = dat4)
)

# Drop model/distributions that failed/converged poorly 
converged <- function(m) {
  !is.null(m) && m$fit$convergence == 0 && isTRUE(m$sdr$pdHess) &&
    all(is.finite(sqrt(diag(vcov(m)$cond))))
}
fits <- keep(fits, converged)

aic_tab <- tibble(
  model = names(fits),
  npar  = map_dbl(fits, ~ attr(logLik(.x), "df")),
  AIC   = map_dbl(fits, AIC),
  BIC   = map_dbl(fits, BIC)
) %>% mutate(dAIC = AIC - min(AIC)) %>% arrange(AIC)
aic_tab

# Goodness of Fit test
gof <- map_dfr(fits, function(m) {
  r <- simulateResiduals(m, n = 1000, plot = FALSE)
  obs_zero <- sum(dat$total_abundance == 0)
  sim_zero <- mean(apply(r$simulatedResponse, 2, function(x) sum(x == 0)))
  tibble(
    unif_p     = testUniformity(r, plot = FALSE)$p.value,
    disp_p     = testDispersion(r, plot = FALSE)$p.value,
    zi_p       = testZeroInflation(r, plot = FALSE)$p.value,
    outlier_p  = testOutliers(r, plot = FALSE)$p.value,
    obs_zeros  = obs_zero,
    exp_zeros  = round(sim_zero, 1)
  )
}, .id = "model")

left_join(aic_tab, gof, by = "model")

# Compare models/distributions/assess residuals
best <- fits[[aic_tab$model[1]]]
plot(simulateResiduals(best, n = 1000))
res <- simulateResiduals(best, n = 1000)
plotResiduals(res, form = dat$smoke_status)
plotResiduals(res, form = dat$site)
# NB1 fits best



### MODEL ###
m_final <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site),
                   family = nbinom1, data = dat4)
summary(m_final)

# Rate ratio with CI and p
pairs(emmeans(m_final, ~ smoke_status), type = "response", reverse = TRUE)

# Estimated marginal means (median site)
emm  <- emmeans(m_final, ~ smoke_status, type = "response")
pred <- as.data.frame(emm)
pred

# Assess var explained with random effects
VarCorr(m_final)
performance::icc(m_final, by_group = TRUE)

### PLOT MODEL ###
site_means <- dat4 %>%
  group_by(site, smoke_status) %>%
  summarise(var = mean(total_abundance), .groups = "drop") %>%
  group_by(site) %>%
  mutate(dir = {
    d <- diff(var[order(smoke_status)])
    if (d < 0) "down" else if (d > 0) "up" else "flat"
  }) %>%
  ungroup()

p <- ggplot(site_means, aes(smoke_status, var)) +
  geom_line(aes(group = site, colour = dir), alpha = 0.4, linewidth = 0.4) +
  geom_point(alpha = 0.35, size = 1.4, colour = "grey40") +
  geom_line(data = pred, aes(smoke_status, response, group = 1),
            colour = "black", linewidth = 1) +
  geom_point(aes(colour = dir), alpha = 1, size = 1.4) +
  scale_colour_manual(values = c(down = "#D55E00", up = "#0072B2", flat = "grey70"),
                      guide = "none") +
  geom_errorbar(data = pred, aes(smoke_status, ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.06, colour = "black", linewidth = 0.8,
                inherit.aes = FALSE) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Vocal activity rate (songs/min)") +
  theme_classic(base_size = 14)

# Save figure
ggsave(
  "Figures/WTSP_VAR_plot.png",
  plot   = p,
  device = agg_png,
  width  = 10,
  height = 7,
  units  = "in",
  dpi    = 600
)




### Assess various functional forms for tssr ###

# Linear
mod_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site),
                 family = nbinom1, data = dat4)

# Quadratic 
mod_2 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 2) + (1 | site),
                   family = nbinom1, data = dat4)

# Cubic
mod_3 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 3) + (1 | site),
                 family = nbinom1, data = dat4)

# Interaction between smoke_status and hr
mod_4 <- glmmTMB(total_abundance ~ smoke_status*tssr + (1 | site),
                   family = nbinom1, data = dat4)

# AIC
AIC(mod_1)
AIC(mod_2)
AIC(mod_3)
AIC(mod_4)
# Linear is best















WTSP_sites <- dat4 %>%
  distinct(site, latitude, longitude)


# ---------------- Settings ----------------

# 600 m = side of the square -> each corner is +/- 300 m from centre in x and y.
# If you meant 600 m = centre-to-corner, set OFFSET_M <- 600 / sqrt(2)  (~424.26).
OFFSET_M <- 300

TARGET_CRS <- 3400     # NAD83 / Alberta 10-TM (Forest), metres
INPUT_CRS  <- 4326     # incoming lat/lon are WGS84 decimal degrees

# ---------------- 1. Start from the raw table (pre-sf) ----------------
# `WTSP_sites` here is the data.frame BEFORE it was made an sf object — the one
# with `site`, `latitude`, `longitude`. If your only copy is already sf, run:
#   raw <- WTSP_sites %>% st_drop_geometry()   # (coords must still be columns)
# but the cleanest is to feed the original data.frame.
raw <- WTSP_sites

# ---------------- 2. Split site / corner, drop FHP ----------------
# Site id = everything before the FINAL "-"; corner = the trailing token.
# FHP names (e.g. FHP-G-299O22) have no NE/NW/SE/SW corner and are excluded.
stations <- raw %>%
  mutate(
    is_fhp = str_starts(site, "FHP"),
    corner = str_extract(site, "(NE|NW|SE|SW)$"),
    site_id = str_remove(site, "-(NE|NW|SE|SW)$")
  ) %>%
  filter(!is_fhp)

# Sanity: anything non-FHP that didn't parse a corner is malformed — surface it.
bad <- stations %>% filter(is.na(corner))
if (nrow(bad) > 0) {
  warning("Rows with no NE/NW/SE/SW corner (check names):\n",
          paste(bad$site, collapse = ", "))
}

# ---------------- 3. One CENTRE per site ----------------
# The lat/lon repeat the centre across stations, so collapse to a single centre
# per site_id. Guard: if a site's rows disagree on centre by more than a rounding
# wobble, flag it rather than silently averaging a real discrepancy.
centres <- stations %>%
  group_by(site_id) %>%
  summarise(
    lat = mean(latitude,  na.rm = TRUE),
    lon = mean(longitude, na.rm = TRUE),
    lat_spread = max(latitude)  - min(latitude),
    lon_spread = max(longitude) - min(longitude),
    .groups = "drop"
  )

wobble <- centres %>% filter(lat_spread > 1e-3 | lon_spread > 1e-3)
if (nrow(wobble) > 0) {
  message("Note: these sites have stations with genuinely differing centre coords ",
          "(>~100 m); using their mean:\n  ",
          paste(wobble$site_id, collapse = ", "))
}

# ---------------- 4. Project centres to metres ----------------
centre_sf <- st_as_sf(centres, coords = c("lon", "lat"), crs = INPUT_CRS) %>%
  st_transform(TARGET_CRS)

centre_xy <- st_coordinates(centre_sf)
centres_m <- centres %>%
  mutate(x_centre = centre_xy[, 1],
         y_centre = centre_xy[, 2])

# ---------------- 5. Offset table: corner -> (dx, dy) in metres ----------------
# Easting increases east, northing increases north.
offsets <- tibble(
  corner = c("NE", "NW", "SE", "SW"),
  dx = c(+OFFSET_M, -OFFSET_M, +OFFSET_M, -OFFSET_M),
  dy = c(+OFFSET_M, +OFFSET_M, -OFFSET_M, -OFFSET_M)
)

# ---------------- 6. Generate corners, then keep ONLY those present in input ----------------
# Corners are derived from each site's centre (not the listed rows), but the
# FINAL object is restricted to the station names that actually appear in your
# input — so a site that only had NE keeps only NE, not a fabricated full set.
corner_pts <- centres_m %>%
  select(site_id, x_centre, y_centre) %>%
  tidyr::crossing(offsets) %>%
  mutate(
    x = x_centre + dx,
    y = y_centre + dy,
    station = paste0(site_id, "-", corner)
  )

# Station names present in the input (non-FHP only — `stations` already dropped FHP).
input_stations <- stations %>% distinct(station = site) %>% pull(station)

corner_pts <- corner_pts %>% filter(station %in% input_stations)

corner_sf <- st_as_sf(corner_pts, coords = c("x", "y"), crs = TARGET_CRS) %>%
  select(station, site_id, corner)

# Guard: every input station should have produced exactly one point.
missing_out <- setdiff(input_stations, corner_sf$station)
if (length(missing_out) > 0) {
  warning("Input stations that produced no point (check corner parse):\n  ",
          paste(missing_out, collapse = ", "))
}

cat(sprintf("Generated %d corner stations across %d sites (offset = %.1f m).\n",
            nrow(corner_sf), dplyr::n_distinct(corner_sf$site_id), OFFSET_M))

# ---------------- 7. Save ----------------
st_write(corner_sf,
         "Output/Spatial Data/ABMI_corner_stations_3400.gpkg",
         delete_dsn = TRUE)