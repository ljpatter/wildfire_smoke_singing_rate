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
library(splines)

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

# Remove lat/lon from dat 1
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
  zinb1   = safe_fit(form, family = nbinom1,  ziformula = ~1, data = dat4),
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
WTSP_model_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site/smoke_status), family = nbinom1, data = dat4)
WTSP_model_2 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat4)

# Model summary
summary(WTSP_model_1) # smoke_status is est at ~0 and including it has no effect on SE; therefore, drop it
summary(WTSP_model_2)

## Fit model but aggregate to site-level to see if results are consistent
dat5 <- dat4 %>% 
  group_by(site, smoke_status) %>% 
  summarise(total_abundance = sum(total_abundance), tssr = mean(tssr), .groups = "drop")
WTSP_model_3 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat5)
summary(WTSP_model_3) # results are consistent with point count-level model

### Assess various functional forms for tssr ###
WTSP_tssr_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site/smoke_status), family = nbinom1, data = dat4) # Linear
WTSP_tssr_2 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 2) + (1 | site/smoke_status), family = nbinom1, data = dat4) # Quadratic
WTSP_tssr_3 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 3) + (1 | site/smoke_status), family = nbinom1, data = dat4) # Cubic

# Compare tssr functional responses using AIC
AIC(WTSP_tssr_1)
AIC(WTSP_tssr_2)
AIC(WTSP_tssr_3)
# Linear is best

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


### Spatial autocorrelation diagnostics ###

# Model to check
final_model <- WTSP_tssr_1

# Summarize to one row per site with coordinates
site_coords <- dat4 %>%
  group_by(site) %>%
  summarise(longitude = first(longitude),
            latitude  = first(latitude),
            .groups   = "drop") %>%
  arrange(site) %>%
  mutate(site = as.character(site))

# Project coords to UTM zone 12N so distances are in meters
site_sf <- st_as_sf(site_coords, coords = c("longitude", "latitude"), crs = 4326)
site_sf <- st_transform(site_sf, 32612)
xy      <- st_coordinates(site_sf)
site_coords$x_km <- xy[, 1] / 1000
site_coords$y_km <- xy[, 2] / 1000

# DHARMa spatial test on residuals summed to the site level
sim_res      <- simulateResiduals(final_model, n = 1000)
sim_res_site <- recalculateResiduals(sim_res, group = dat4$site)
testSpatialAutocorrelation(sim_res_site,
                           x = site_coords$x_km,
                           y = site_coords$y_km)

# Extract each site's random intercept (i.e., estimated deviation from the average site)
site_re <- ranef(final_model)$cond$site
site_re <- data.frame(site = rownames(site_re),
                      re   = site_re[, "(Intercept)"])
site_coords <- left_join(site_coords, site_re, by = "site")

# Moran's I on site random intercepts, inverse-distance weights
d_km <- as.matrix(dist(cbind(site_coords$x_km, site_coords$y_km)))
if (any(d_km[upper.tri(d_km)] == 0)) warning("Some sites share identical coordinates.")

w <- 1 / d_km
diag(w) <- 0
w[is.infinite(w)] <- 0
w <- w / rowSums(w)
stopifnot(all(is.finite(w)))

Moran.I(site_coords$re, w)

# Semivariogram-style plot
pair_dist    <- d_km[upper.tri(d_km)]
pair_semivar <- (outer(site_coords$re, site_coords$re, "-")^2 / 2)[upper.tri(d_km)]

semivar_df <- data.frame(dist_km = pair_dist, semivar = pair_semivar)

p_semivar <- ggplot(semivar_df, aes(dist_km, semivar)) +
  geom_point(alpha = 0.3, colour = "grey40") +
  geom_smooth(method = "loess", se = TRUE, colour = "black") +
  labs(x = "Distance between sites (km)",
       y = "Semivariance of site random intercepts") +
  theme_classic(base_size = 14)

plot(p_semivar)

# Finally, try adding a random effect for site cluster to see if it improves AIC
dat5 <- dat4 %>%
  separate_wider_delim(
    site,
    delim = "-",
    names = c("site_group", "other"),
    too_many = "merge", #include if date_time contains more spaces than output fields
    cols_remove = FALSE # Removes OG column if TRUE
  )

# Now run model with site_group as another random effect and compare AIC
WTSP_no_cluster_grouping <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, data = dat5)
WTSP_with_cluster_grouping <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site_group/site), family = nbinom1, data = dat5)

# Model summary
AIC(WTSP_no_cluster_grouping)
AIC(WTSP_with_cluster_grouping) # w/ cluster RE is > 2 AIC - keep. Now re-run residual diagnostics

### WTSP model validation, 2nd round ###
final_model <- WTSP_with_cluster_grouping

# Simulate scaled residuals from the fitted model
sim_res <- simulateResiduals(fittedModel = final_model, n = 1000)

# Main diagnostic plot: QQ plot (with KS, dispersion, outlier tests) + residual vs. predicted
plot(sim_res)

# Residuals against each predictor
plotResiduals(sim_res, form = dat4$smoke_status)
plotResiduals(sim_res, form = dat4$tssr) # Still a slight trend in residuals: decrease in higher values with tssr

# Formal tests
testUniformity(sim_res)    # KS test for correct distribution - looks good
testDispersion(sim_res)    # over/underdispersion - looks good
testZeroInflation(sim_res) # excess zeros - looks good
testOutliers(sim_res)      # outlier frequency - looks good


### Dig deeper into issue fitting tssr

### Check for systematic bias in tssr among treatments
dat5 %>%
  group_by(smoke_status) %>%
  summarise(n = n(),
            mean_tssr   = mean(tssr),
            median_tssr = median(tssr),
            min_tssr    = min(tssr),
            max_tssr    = max(tssr)) # Nope, looks good.


### Compare current top model with one where disperson of tssr is allowd to vary
WTSP_nb1_lin <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site_group/site),
                    family = nbinom1, data = dat5)

WTSP_nb1_disp_lin <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site_group/site),
                         dispformula = ~ tssr,
                         family = nbinom1, data = dat5)

WTSP_nb1_disp_quad <- glmmTMB(total_abundance ~ smoke_status + poly(tssr,2) + (1 | site_group/site),
                         dispformula = ~ tssr,
                         family = nbinom1, data = dat5)

WTSP_nb1_disp_cubic <- glmmTMB(total_abundance ~ smoke_status + poly(tssr,3) + (1 | site_group/site),
                         dispformula = ~ tssr,
                         family = nbinom1, data = dat5)

WTSP_nb1_disp_spline <- glmmTMB(total_abundance ~ smoke_status + ns(tssr, df = 3) + (1 | site_group/site),
                         dispformula = ~ tssr,
                         family = nbinom1, data = dat5)

# Compare AIC
AIC(WTSP_nb1_lin, WTSP_nb1_disp_lin, WTSP_nb1_disp_quad, WTSP_nb1_disp_cubic, WTSP_nb1_disp_spline)

# WTSP_nb1_disp_lin is slightly better, but not by AIC > 2. Check residuals to see if that fixed the issue.

# Set the best model by AIC
final_model <- WTSP_nb1_disp_lin

sim_res <- simulateResiduals(final_model, n = 1000)
plot(sim_res)
plotResiduals(sim_res, form = dat5$tssr)
plotResiduals(sim_res, form = dat5$smoke_status) # Issue is still there


### Check a few more functional forms for tssr modelling

# OG linear model (for comparison)
WTSP_nb1_lin <- glmmTMB(total_abundance ~ smoke_status + tssr +
                          (1 | site_group/site),
                        family = nbinom1, data = dat5)

# Linear tssr + dispersion varying with tssr
WTSP_nb1_lin_disp <- glmmTMB(total_abundance ~ smoke_status + tssr +
                               (1 | site_group/site),
                             dispformula = ~ tssr,
                             family = nbinom1, data = dat5)

# Log tssr
WTSP_nb1_log <- glmmTMB(total_abundance ~ smoke_status + log(tssr) +
                          (1 | site_group/site),
                        family = nbinom1, data = dat5)

# Quadratic tssr 
dat5$tssr_c <- dat5$tssr - mean(dat5$tssr)
WTSP_nb1_quad <- glmmTMB(total_abundance ~ smoke_status + tssr_c + I(tssr_c^2) +
                           (1 | site_group/site),
                         family = nbinom1, data = dat5)

# Spline, 3 df
WTSP_nb1_ns <- glmmTMB(total_abundance ~ smoke_status + ns(tssr, df = 3) +
                         (1 | site_group/site),
                       family = nbinom1, data = dat5)

# Log tssr + dispersion allowed to vary with tssr
WTSP_nb1_log_disp <- glmmTMB(total_abundance ~ smoke_status + log(tssr) +
                               (1 | site_group/site),
                             dispformula = ~ log(tssr),
                             family = nbinom1, data = dat5)

# ---- Compare fit ----
AIC(WTSP_nb1_lin, WTSP_nb1_lin_disp, WTSP_nb1_log, WTSP_nb1_quad, WTSP_nb1_ns, WTSP_nb1_log_disp) # WTSP_nb1_lin still wins..



### Additional diagnostics

# Check if variation in tssr is mostly between-site or within-site
tssr_by_site <- dat5 %>%
  group_by(site) %>%
  summarise(n         = n(),
            mean_tssr = mean(tssr),
            sd_tssr   = sd(tssr),
            min_tssr  = min(tssr),
            max_tssr  = max(tssr))

tssr_by_site %>% arrange(mean_tssr) %>% print(n = 47)

between_var <- var(tssr_by_site$mean_tssr)
within_var  <- mean(tssr_by_site$sd_tssr^2)
between_var / (between_var + within_var)   # proportion of tssr variation that is between sites

# Check conditional residuals (random effects held at their fitted values) to see if pattern holds
set_simcodes(WTSP_nb1_lin$obj, val = "fix")
sim_cond <- simulateResiduals(WTSP_nb1_lin, n = 1000)
plot(sim_cond)
plotResiduals(sim_cond, form = dat5$tssr)

# Conditional residuals look good. Unconditional DHARMa residuals re-simulate the site and
#   site_group random effects in every simulation. With large random-effect
#   variances (site = 0.98, site_group = 0.42), these residuals mostly reflect
#   site-to-site variation rather than observation-level fit.
#   Since tssr varies almost entirely within sites and every site has the same 
#   three-window survey schedule, the 6 visits per site share a site effect, so their 
#   unconditional residuals are correlated, and this correlation lines up along the tssr axis. 
#   DHARMa's quantile tests assume independent residuals, so they overstate the deviation vs tssr.
#   Conditional residuals (random effects fixed at their estimates) test whether the model fits each
#   observation given its site's effect. This is the relevant check for a within-site covariate like tssr.
#   Conditional residuals showed no problems so the unconditional deviation is treated as an artefact, not model misfit.
# -----------------------------------------------------------------------------

### Finally, calculate estimate marginal means

# Final model
final_model <- glmmTMB(total_abundance ~ smoke_status + tssr +
                          (1 | site_group/site),
                        family = nbinom1, data = dat5)
summary(final_model)

# Rate ratio with CI and p
pairs(emmeans(final_model, ~ smoke_status), type = "response", reverse = TRUE)

# Estimated marginal means (median site)
emm  <- emmeans(final_model, ~ smoke_status, type = "response")
pred <- as.data.frame(emm)
pred
decl_VAR <- 1 - 0.779
decl_VAR



### Plot model

# Raw data for the background: mean value per site within each smoke status
site_means <- dat4 %>%
  group_by(site, smoke_status) %>%
  summarise(var = mean(total_abundance), .groups = "drop")

# Make sure smoke_status has the same level order in the raw data and the predictions
site_means$smoke_status <- factor(site_means$smoke_status, levels = levels(pred$smoke_status))

# Build the plot
p <- ggplot() +
  
  # Raw site means: grey, semi-transparent, jittered sideways so points don't stack
  geom_point(data = site_means, aes(x = smoke_status, y = var),
             position = position_jitter(width = 0.08, height = 0, seed = 123),
             shape = 16, size = 1.6, colour = "grey60", alpha = 0.5) +
  
  # 95% confidence band from the model
  geom_ribbon(data = pred, aes(x = smoke_status, ymin = asymp.LCL, ymax = asymp.UCL, group = 1),
              fill = "#0072B2", alpha = 0.2) +
  
  # Model line connecting the two estimated marginal means
  geom_line(data = pred, aes(x = smoke_status, y = response, group = 1),
            colour = "#0072B2", linewidth = 0.9) +
  
  # Estimated marginal means as points
  geom_point(data = pred, aes(x = smoke_status, y = response),
             shape = 21, size = 2.8, fill = "#0072B2", colour = "white", stroke = 0.6) +
  
  # Axes
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  expand_limits(y = 0) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  # scale_x_discrete(labels = c("Non-smoky", "Smoky"), expand = expansion(add = 0.4)) # optional: nicer labels, in level order
  labs(x = NULL, y = "Vocal activity rate (songs/min)") +
  
  # Clean journal theme
  theme_classic(base_size = 10) +
  theme(axis.text         = element_text(colour = "black"),
        axis.ticks        = element_line(colour = "black", linewidth = 0.4),
        axis.line         = element_line(colour = "black", linewidth = 0.4),
        axis.ticks.length = unit(1.5, "mm"))

# View plot
plot(p)

# Save figure (png and PDF versions)
ggsave("Figures/WTSP_VAR_plot.png", plot = p, device = agg_png,
       width = 5, height = 5, units = "in", dpi = 600)

ggsave("Figures/WTSP_VAR_plot.pdf", plot = p, device = cairo_pdf,
       width = 3.5, height = 3.5, units = "in")








##################################
############# YEWA ###############
##################################

# Set the session timezone FIRST, before anything else loads
# (Windows reports "Alberta Standard Time", which R's date tools can't read)
Sys.setenv(TZ = "America/Edmonton")

# Clear env
rm(list = ls())

# Load data
YEWA <- read.csv("Input/Tabular Data/YEWA_data_for_analysis_2026-09-29.csv")

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
# tz = "UTC" in as.Date() stops it from looking up the system timezone
sun <- getSunlightTimes(
  data = dat1 %>%
    transmute(
      date = as.Date(as.character(date), tz = "UTC"),
      lat  = latitude,
      lon  = longitude
    ),
  keep = "sunrise",
  tz   = "America/Edmonton"
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



### Select error family via AIC

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
  zinb1   = safe_fit(form, family = nbinom1,  ziformula = ~1, data = dat2),
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
  obs_zero <- sum(dat2$total_abundance == 0)
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
plotResiduals(res, form = dat2$smoke_status)
plotResiduals(res, form = dat2$site)
# Result: zero-inflated NB1 fits best (AIC ~968)


# Compare i) smoke_status nested in site vs ii) site only
YEWA_model_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site/smoke_status), family = nbinom1, ziformula = ~ 1, data = dat2)
YEWA_model_2 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, ziformula = ~ 1, data = dat2)

# Model summary
summary(YEWA_model_1) # adding smoke status explained very little variance - drop it
summary(YEWA_model_2)

# AIC
AIC(YEWA_model_1)
AIC(YEWA_model_2) # zinb1 with just site RE is best

## Fit model but aggregate to site-level to see if results are consistent
dat3 <- dat2 %>%
  group_by(site, smoke_status) %>%
  summarise(total_abundance = sum(total_abundance), tssr = mean(tssr), .groups = "drop")
YEWA_model_3 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, ziformula = ~ 1, data = dat3)
summary(YEWA_model_3) # results are consistent with point count-level model


### Assess tssr function form
YEWA_tssr_1 <- glmmTMB(total_abundance ~ smoke_status + tssr + (1 | site), family = nbinom1, ziformula = ~ 1, data = dat2) # Linear
YEWA_tssr_2 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 2) + (1 | site), family = nbinom1, ziformula = ~ 1, data = dat2) # Quadratic
YEWA_tssr_3 <- glmmTMB(total_abundance ~ smoke_status + poly(tssr, 3) + (1 | site), family = nbinom1, ziformula = ~ 1, data = dat2) # Cubic

# Compare tssr functional responses using AIC
AIC(YEWA_tssr_1) # Linear is best
AIC(YEWA_tssr_2)
AIC(YEWA_tssr_3)

# Final model
final_model <- YEWA_tssr_1



### Model validation assessed using
# 1) Unconditional residuals: new site effects are drawn in every simulation.
#    Each residual mixes "is this count unusual for its site?" with "is this site unusual?"
#    Recordings from the same site move together, so tests that treat all 162 residuals
#    as independent (e.g. within-group uniformity) which can be prone to false flags.
# 2) Conditional residuals: site effects held at their estimates (BLUPs).
#    Only observation-level noise is simulated. Tests the NB1/ZI part and fixed-effect shapes,
#    but can't see problems at the site level - so we check that level separately.

# Unconditional residuals (random effects re-simulated)
sim_res <- simulateResiduals(fittedModel = final_model, n = 1000)

# Main diagnostic plot: QQ plot (with KS, dispersion, outlier tests) + residual vs. predicted
plot(sim_res)

# Residuals against each predictor
plotResiduals(sim_res, form = dat2$smoke_status)
plotResiduals(sim_res, form = dat2$tssr)

# Formal tests
testDispersion(sim_res)    # check for over/underdispersion
testZeroInflation(sim_res) # check for excess zeros
testOutliers(sim_res)      # check for outliers
# Result: QQ, dispersion, ZI, outliers OK; minor within-group flags vs smoke_status and tssr



# Generate unconditional residuals aggregated to site x smoke_status
# Each site contributes one residual per smoke group, removing pseudoreplication
site_smoke <- interaction(dat2$site, dat2$smoke_status, drop = TRUE)
sim_res_ss <- recalculateResiduals(sim_res, group = site_smoke)

# Smoke status for each site x smoke group, in the same order as the grouped residuals
ss_lookup <- dat2 %>%
  mutate(site_smoke = site_smoke) %>%
  distinct(site_smoke, smoke_status) %>%
  arrange(site_smoke)

plot(sim_res_ss)
plotResiduals(sim_res_ss, form = ss_lookup$smoke_status)
# Result: no issues - smoke_status flag in earlier diagnostics was a clustering artefact


# Generate conditional residuals (site effects fixed at their estimates)
set_simcodes(final_model$obj, val = "fix")
sim_cond <- simulateResiduals(fittedModel = final_model, n = 1000)
plot(sim_cond)
plotResiduals(sim_cond, form = dat2$smoke_status)
plotResiduals(sim_cond, form = dat2$tssr)
testUniformity(sim_cond)
testDispersion(sim_cond)
testZeroInflation(sim_cond)
testOutliers(sim_cond)
# All residual plots look good under conditional residuals

# switch back to the default
set_simcodes(final_model$obj, val = "random")
# Residual plots look good


### Spatial autocorrelation diagnostics

## Create df with one row per site with coordinates, sorted in site factor-level order
site_coords <- dat2 %>%
  group_by(site) %>%
  summarise(longitude = first(longitude),
            latitude  = first(latitude),
            .groups   = "drop") %>%
  arrange(site) %>%
  mutate(site = as.character(site))

## Project coordinates to UTM zone 12N so distances are in meters
site_sf <- st_as_sf(site_coords, coords = c("longitude", "latitude"), crs = 4326)
site_sf <- st_transform(site_sf, 32612)
xy      <- st_coordinates(site_sf)
site_coords$x_km <- xy[, 1] / 1000
site_coords$y_km <- xy[, 2] / 1000

# DHARMa spatial test on residuals summed to the site level
testSpatialAutocorrelation(sim_res_site,
                           x = site_coords$x_km,
                           y = site_coords$y_km)
# No spatial autocorrelation (p ~ 0.5) - yeehaw

# Extract each site's random intercept (its estimated deviation from the average site)
site_re <- ranef(final_model)$cond$site
site_re <- data.frame(site = rownames(site_re),
                      re   = site_re[, "(Intercept)"])
site_coords <- left_join(site_coords, site_re, by = "site")

# Moran's I on site random intercepts using inverse-distance weights
d_km <- as.matrix(dist(cbind(site_coords$x_km, site_coords$y_km)))
if (any(d_km[upper.tri(d_km)] == 0)) warning("Some sites share identical coordinates.")

w <- 1 / d_km
diag(w) <- 0
w[is.infinite(w)] <- 0
w <- w / rowSums(w)
stopifnot(all(is.finite(w)))

Moran.I(site_coords$re, w)

# Create semivariogram-style plot
pair_dist    <- d_km[upper.tri(d_km)]
pair_semivar <- (outer(site_coords$re, site_coords$re, "-")^2 / 2)[upper.tri(d_km)]

semivar_df <- data.frame(dist_km = pair_dist, semivar = pair_semivar)

p_semivar <- ggplot(semivar_df, aes(dist_km, semivar)) +
  geom_point(alpha = 0.3, colour = "grey40") +
  geom_smooth(method = "loess", se = TRUE, colour = "black") +
  labs(x = "Distance between sites (km)",
       y = "Semivariance of site random intercepts") +
  theme_classic(base_size = 14)

plot(p_semivar)

# Map of site random intercepts
p_map <- ggplot(site_coords, aes(x_km, y_km, colour = re, size = abs(re))) +
  geom_point() +
  scale_colour_gradient2(low = "#D55E00", mid = "grey80", high = "#0072B2",
                         midpoint = 0, name = "Site effect\n(log scale)") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  coord_equal() +
  labs(x = "Easting (km)", y = "Northing (km)") +
  theme_classic(base_size = 14)

plot(p_map) # looks good



### Effect sizes and estimated marginal means 

# CIs on the log scale, then back-transformed
ci <- confint(final_model, parm = c("smoke_statusSmoky", "tssr"))
ci
exp(ci)               # multiplicative change in expected abundance
(exp(ci) - 1) * 100   # percent change

# Rate ratio with CI and p
pairs(emmeans(final_model, ~ smoke_status), type = "response", reverse = TRUE)

# Estimated marginal means (median site)
emm  <- emmeans(final_model, ~ smoke_status, type = "response")
pred <- as.data.frame(emm)
pred



### Plot model

# Raw data for the background: mean value per site within each smoke status
site_means <- dat2 %>%
  group_by(site, smoke_status) %>%
  summarise(var = mean(total_abundance), .groups = "drop")

# Make sure smoke_status has the same level order in the raw data and the predictions
site_means$smoke_status <- factor(site_means$smoke_status, levels = levels(pred$smoke_status))

# Build the plot
p <- ggplot() +
  
  # Raw site means: grey, semi-transparent, jittered sideways so points don't stack
  geom_point(data = site_means, aes(x = smoke_status, y = var),
             position = position_jitter(width = 0.08, height = 0, seed = 123),
             shape = 16, size = 1.6, colour = "grey60", alpha = 0.5) +
  
  # 95% confidence band from the model
  geom_ribbon(data = pred, aes(x = smoke_status, ymin = asymp.LCL, ymax = asymp.UCL, group = 1),
              fill = "#0072B2", alpha = 0.2) +
  
  # Model line connecting the two estimated marginal means
  geom_line(data = pred, aes(x = smoke_status, y = response, group = 1),
            colour = "#0072B2", linewidth = 0.9) +
  
  # Estimated marginal means as points
  geom_point(data = pred, aes(x = smoke_status, y = response),
             shape = 21, size = 2.8, fill = "#0072B2", colour = "white", stroke = 0.6) +
  
  # Axes
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  expand_limits(y = 0) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  # scale_x_discrete(labels = c("Non-smoky", "Smoky"), expand = expansion(add = 0.4)) # optional: nicer labels, in level order
  labs(x = NULL, y = "Vocal activity rate (songs/min)") +
  
  # Clean journal theme
  theme_classic(base_size = 10) +
  theme(axis.text         = element_text(colour = "black"),
        axis.ticks        = element_line(colour = "black", linewidth = 0.4),
        axis.line         = element_line(colour = "black", linewidth = 0.4),
        axis.ticks.length = unit(1.5, "mm"))

# View plot
plot(p)

# Save figure (png and PDF versions)
ggsave("Figures/YEWA_VAR_plot.png", plot = p, device = agg_png,
       width = 5, height = 5, units = "in", dpi = 600)

ggsave("Figures/YEWA_VAR_plot.pdf", plot = p, device = cairo_pdf,
       width = 3.5, height = 3.5, units = "in")

