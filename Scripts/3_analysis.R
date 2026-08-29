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
library(ape)
library(sf)
# Convert to df where geometry moves to lat/long
site_dat <- st_read("Output/Spatial Data/to delete/WTSP_all_stations_3400.gpkg") %>%
  st_transform(crs = 4326) %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude  = st_coordinates(.)[, 2]
  ) %>%
  st_set_geometry(NULL)

write.csv(site_dat, "Output/Tabular Data/ABMI_site_latlong_to_join.csv", row.names = FALSE)


##################################
############# WTSP ###############
##################################

# Load data
WTSP <- read.csv("Input/Tabular Data/WTSP_data_for_analysis_2026-09-29.csv")

### Pre-model manipulations and selecting error family

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
latlon <- read.csv("Output/Tabular Data/ABMI_site_latlong_to_join.csv")

# Filter latlon df to only contain rows that are present in the dat1 df
latlon2 <- latlon %>%
  filter(site %in% dat$site)

# Remove site# Remove lat/lon from dat 1
dat2 <- dat1 %>%
  select(-latitude, -longitude)

# Left join latlon2 to dat2
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


### FIT MODEL - fit with i) date nested in site and ii) site as random effect, then compare AIC site ###
WTSP_model_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site/date), family = nbinom1, data = dat4)
WTSP_model_2 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat4)

# Model summary
summary(WTSP_model_1) # date variance is essentially 0 and SEs are identical between models, so site-only random effect is sufficient
summary(WTSP_model_2)

## Fit model but aggregate to site-level to see if results are consistent
dat5 <- dat4 %>% 
  group_by(site, smoke_status) %>% 
  summarise(total_abundance = sum(total_abundance), tssr = mean(tssr), .groups = "drop")
WTSP_model_3 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat5)
summary(WTSP_model_3) # results are consistent with point count-level model

### Assess various functional forms for tssr ###
WTSP_tssr_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat4) # Linear
WTSP_tssr_2 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 2) + (1 | site), family = nbinom1, data = dat4) # Quadratic
WTSP_tssr_3 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 3) + (1 | site), family = nbinom1, data = dat4) # Cubic

# Compare tssr functional responses using AIC
AIC(WTSP_tssr_1)
AIC(WTSP_tssr_2)
AIC(WTSP_tssr_3)
# Linear is best

### Calculate estimate marginal means

# Rate ratio with CI and p
pairs(emmeans(WTSP_model_2, ~ smoke_status), type = "response", reverse = TRUE)

# Estimated marginal means (median site)
emm  <- emmeans(WTSP_model_2, ~ smoke_status, type = "response")
pred <- as.data.frame(emm)
pred

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

# View plot 
plot(p)

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


### WTSP model validation ###

# Simulate scaled residuals from the fitted model
sim_res <- simulateResiduals(fittedModel = WTSP_tssr_1, n = 1000)

# Main diagnostic plot: QQ plot (with KS, dispersion, outlier tests) + residual vs. predicted
plot(sim_res)

# Residuals against each predictor
plotResiduals(sim_res, form = dat4$smoke_status)
plotResiduals(sim_res, form = dat4$tssr)

# Formal tests
testUniformity(sim_res)    # KS test for correct distribution
testDispersion(sim_res)    # over/underdispersion
testZeroInflation(sim_res) # excess zeros
testOutliers(sim_res)      # outlier frequency

## Check for spatial pattern in residuals
site_dat <- dat4 %>%
  mutate(res = residuals(WTSP_tssr_1, type = "pearson")) %>%
  group_by(site) %>%
  summarise(
    res      = mean(res),
    longitude  = first(longitude),
    latitude = first(latitude),
    .groups  = "drop"
  )

# Distance matrix; guard against duplicate coordinates
d <- as.matrix(dist(cbind(site_dat$longitude, site_dat$latitude)))
if (any(d[upper.tri(d)] == 0)) warning("Some sites share identical coordinates.")

w <- 1 / d
diag(w) <- 0
w[is.infinite(w)] <- 0        # kill Inf from any zero distances
w <- w / rowSums(w)           # row-standardize

# also catch any all-zero rows (a site with no finite weights) or NAs
stopifnot(all(is.finite(w)))

# Moran's I
Moran.I(site_dat$res, w) # 'sal good, man.





##################################
############# YEWA ###############
##################################

# Clear env
rm(list = ls())

# Load data
YEWA <- read.csv("Input/Tabular Data/YEWA_data_for_analysis_2026-09-29.csv")

### Pre-model manipulations and selecting error family

# Ensure site, date, and smoke_status are factors
dat <- YEWA %>% mutate(
  site         = factor(site),
  date         = factor(date),
  smoke_status = factor(smoke_status, levels = c("non-smoky", "smoky"))
)

# Capitalize smoky and non-smoky for figures later on
dat1 <- dat %>%
  mutate(smoke_status = factor(smoke_status,
                               levels = c("non-smoky", "smoky"),
                               labels = c("Non-smoky", "Smoky")))


# Get sunrise times (renaming columns to 'lat' and 'lon')
sun <- getSunlightTimes(
  data = dat1 %>% 
    transmute(
      date = as.Date(as.character(date)), 
      lat = latitude, 
      lon = longitude
    ),
  keep = "sunrise", 
  tz = "America/Edmonton"
)

# Create a datetime column and calculate TSSR
dat2 <- dat1 %>%
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
  poisson = safe_fit(form, family = poisson,  data = dat2),
  nbinom1 = safe_fit(form, family = nbinom1,  data = dat2),
  nbinom2 = safe_fit(form, family = nbinom2,  data = dat2),
  genpois = safe_fit(form, family = genpois,  data = dat2),
  compois = safe_fit(form, family = compois,  data = dat2),
  zip     = safe_fit(form, family = poisson,  ziformula = ~1, data = dat2),
  zinb2   = safe_fit(form, family = nbinom2,  ziformula = ~1, data = dat2)
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


### FIT MODEL - fit with i) date nested in site and ii) site as random effect, then compare AIC site ###
YEWA_model_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site/date), family = nbinom1, data = dat2)
YEWA_model_2 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat2)

# Model summary
summary(YEWA_model_1) # date variance is essentially 0 and SEs are identical between models, so site-only random effect is sufficient
summary(YEWA_model_2)

## Fit model but aggregate to site-level to see if results are consistent
dat3 <- dat2 %>% 
  group_by(site, smoke_status) %>% 
  summarise(total_abundance = sum(total_abundance), tssr = mean(tssr), .groups = "drop")
YEWA_model_3 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat3)
summary(YEWA_model_3) # results are consistent with point count-level model

### Assess various functional forms for tssr ###
YEWA_tssr_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat2) # Linear
YEWA_tssr_2 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 2) + (1 | site), family = nbinom1, data = dat2) # Quadratic
YEWA_tssr_3 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 3) + (1 | site), family = nbinom1, data = dat2) # Cubic

# Compare tssr functional responses using AIC
AIC(YEWA_tssr_1)
AIC(YEWA_tssr_2)
AIC(YEWA_tssr_3)
# Linear is best

### Calculate estimate marginal means

# Rate ratio with CI and p
pairs(emmeans(YEWA_tssr_1, ~ smoke_status), type = "response", reverse = TRUE)

# Estimated marginal means (median site)
emm  <- emmeans(YEWA_tssr_1, ~ smoke_status, type = "response")
pred <- as.data.frame(emm)
pred

### PLOT MODEL ###
site_means <- dat2 %>%
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

# View plot 
plot(p)

# Save figure
ggsave(
  "Figures/YEWA_VAR_plot.png",
  plot   = p,
  device = agg_png,
  width  = 10,
  height = 7,
  units  = "in",
  dpi    = 600
)


### YEWA model validation ###

# Simulate scaled residuals from the fitted model
sim_res <- simulateResiduals(fittedModel = YEWA_tssr_1, n = 1000)

# Main diagnostic plot: QQ plot (with KS, dispersion, outlier tests) + residual vs. predicted
plot(sim_res)

# Residuals against each predictor
plotResiduals(sim_res, form = dat2$smoke_status)
plotResiduals(sim_res, form = dat2$tssr)

# Formal tests
testUniformity(sim_res)    # KS test for correct distribution
testDispersion(sim_res)    # over/underdispersion
testZeroInflation(sim_res) # excess zeros
testOutliers(sim_res)      # outlier frequency

## Check for spatial pattern in residuals
site_dat <- dat2 %>%
  mutate(res = residuals(YEWA_tssr_1, type = "pearson")) %>%
  group_by(site) %>%
  summarise(
    res      = mean(res),
    longitude  = first(longitude),
    latitude = first(latitude),
    .groups  = "drop"
  )

# Distance matrix; guard against duplicate coordinates
d <- as.matrix(dist(cbind(site_dat$longitude, site_dat$latitude)))
if (any(d[upper.tri(d)] == 0)) warning("Some sites share identical coordinates.")

w <- 1 / d
diag(w) <- 0
w[is.infinite(w)] <- 0        # kill Inf from any zero distances
w <- w / rowSums(w)           # row-standardize

# also catch any all-zero rows (a site with no finite weights) or NAs
stopifnot(all(is.finite(w)))

# Moran's I
Moran.I(site_dat$res, w) # Deadly


dat2
