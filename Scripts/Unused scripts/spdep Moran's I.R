# From text
### Clean data for correlogram

############### MAY 15

# Load May 15 smoke data
dat1 <- read.csv("Input/May15_AQD.csv", row.names = NULL)

# Remove unneeded rows
dat1 <- dat1[-c(1:6), ]

# Create new df with station names
stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

# Create df with lat/lon of air monitoring stations
coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns 

# Create PM2.5 value df
smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

# Combine station names, coordinates, and PM2.5 values
dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")

# Remove any stations that are missing PM2.5 values
dat3 <- dat2 %>%
  filter(pm25 >= 1)

# Filter to only include stations in proximity to sites
dat4 <- dat3 %>%
  filter(station %in% c("Chipman", "Edson", "Fort Saskatchewan", "Edmonton", 
                        "Genesee", "Gibbons", "Hinton", "Lamont", "Strathcona County", 
                        "Thorchild County", "Tomahawk", "Red Deer", "St. Albert", "Redwater", 
                        "Power", "Caroline", "Bruderheim1", "Steeper", "Drayton Valley", "Elk Island"))

# Remove those silly row names
dat4 <- as.data.frame(dat4)
rownames(dat4) <- NULL

# Coerce pm2.5 to be numeric
dat4$pm25 <- as.numeric(dat4$pm25)

# Remove duplicate stations, taking the mean of the values
dat5 <- dat4 %>%
  group_by(lat,lon) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")

### Correlogram

# Create matrix with just lat and long
coords <- data.frame(x = dat5$lat, y = dat5$lon)
coords$x <- as.numeric(coords$x)
coords$y <- as.numeric(coords$y)

# Convert to UTMs
pts <- st_as_sf(dat5, coords = c("lon", "lat"), crs = 4326)   # if y = lon, x = lat
pts_utm <- st_transform(pts, 32612)  # UTM zone 12N, adjust if needed
coords_proj <- st_coordinates(pts_utm)

#makeaneighborhoodlist
neigh <- dnearneigh(x=coords_proj, d1 = 0, d2 = 200000, longlat=FALSE)
#plottheneighborhood>
plot(neigh,coords_proj)

wts <- nb2listw(neighbours=neigh,style="W",zero.policy=T)

moran_mc <-moran.mc(x=dat5$pm25,listw=wts,nsim=99,zero.policy=T)




### Now calculate Morans I at different distances

# response
x <- dat5$pm25

# check lengths match
stopifnot(nrow(coords_proj) == length(x))

# define distance bands in metres
breaks <- c(0, 50000, 100000, 150000, 200000)

# storage
correlog_sp <- data.frame(
  d_start = head(breaks, -1),
  d_end   = tail(breaks, -1),
  dist    = head(breaks, -1) + diff(breaks) / 2,
  MoransI = NA_real_,
  Null_lcl = NA_real_,
  Null_ucl = NA_real_,
  Pvalue = NA_real_,
  links = NA_integer_
)

# loop over bands
for (i in seq_len(nrow(correlog_sp))) {
  
  neigh <- dnearneigh(
    x = coords_proj,
    d1 = correlog_sp$d_start[i],
    d2 = correlog_sp$d_end[i],
    longlat = FALSE
  )
  
  correlog_sp$links[i] <- sum(card(neigh))
  
  # skip empty bands
  if (correlog_sp$links[i] == 0) next
  
  wts <- nb2listw(neigh, style = "W", zero.policy = TRUE)
  
  moran_i <- try(
    moran.mc(x = x, listw = wts, nsim = 999, zero.policy = TRUE),
    silent = TRUE
  )
  
  if (inherits(moran_i, "try-error")) {
    print(paste("Failed for band", correlog_sp$d_start[i], "to", correlog_sp$d_end[i]))
    next
  }
  
  correlog_sp$MoransI[i]  <- as.numeric(moran_i$statistic)
  correlog_sp$Null_lcl[i] <- quantile(moran_i$res, 0.025, na.rm = TRUE)
  correlog_sp$Null_ucl[i] <- quantile(moran_i$res, 0.975, na.rm = TRUE)
  correlog_sp$Pvalue[i]   <- moran_i$p.value
}

correlog_sp


plot(
  correlog_sp$dist / 1000,
  correlog_sp$MoransI,
  type = "b",
  xlab = "Distance class midpoint (km)",
  ylab = "Moran's I"
)

abline(h = 0, lty = 2)
lines(correlog_sp$dist / 1000, correlog_sp$Null_lcl, lty = 3)
lines(correlog_sp$dist / 1000, correlog_sp$Null_ucl, lty = 3)









############## May 21
# Load May 15 smoke data
dat1 <- read.csv("Input/May21_AQD.csv")

# Remove unneeded rows
dat1 <- dat1[-c(1:6), ]

# Create new df with station names
stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

# Create df with lat/lon of air monitoring stations
coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns 

# Create PM2.5 value df
smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

# Combine station names, coordinates, and PM2.5 values
dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")

# Remove any stations that are missing PM2.5 values
dat3 <- dat2 %>%
  filter(pm25 >= 1)

# Filter to only include stations in proximity to sites
#dat4 <- dat3 %>%
#  filter(station %in% c("Chipman", "Edson", "Fort Saskatchewan", "Edmonton", 
                        "Genesee", "Gibbons", "Hinton", "Lamont", "Strathcona County", 
                        "Thorchild County", "Tomahawk", "Red Deer", "St. Albert", "Redwater", 
                        "Power", "Caroline", "Bruderheim1", "Steeper", "Drayton Valley", "Elk Island"))


dat4 <- dat3 %>%
  filter(station %in% c("Drayton Valley", "Edson", "Genesee", "Hinton", "Steeper", "Tomahawk", "St. Albert"))

# Remove those silly row names
dat4 <- as.data.frame(dat4)
rownames(dat4) <- NULL

# Coerce pm2.5 to be numeric
dat4$pm25 <- as.numeric(dat4$pm25)

# Remove duplicate stations, taking the mean of the values
dat5 <- dat4 %>%
  group_by(lat,lon) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")

### Correlogram

# Create matrix with just lat and long
coords <- data.frame(x = dat5$lat, y = dat5$lon)
coords$x <- as.numeric(coords$x)
coords$y <- as.numeric(coords$y)

# Convert to UTMs
pts <- st_as_sf(dat5, coords = c("lon", "lat"), crs = 4326)   # if y = lon, x = lat
pts_utm <- st_transform(pts, 32612)  # UTM zone 12N, adjust if needed
coords_proj <- st_coordinates(pts_utm)

#makeaneighborhoodlist
neigh <- dnearneigh(x=coords_proj, d1 = 0, d2 = 200000, longlat=FALSE)
#plottheneighborhood>
plot(neigh,coords_proj)

wts <- nb2listw(neighbours=neigh,style="W",zero.policy=T)

moran_mc <-moran.mc(x=dat5$pm25,listw=wts,nsim=99,zero.policy=T)




### Now calculate Morans I at different distances

# response
x <- dat5$pm25

# check lengths match
stopifnot(nrow(coords_proj) == length(x))

# define distance bands in metres
#breaks <- c(0, 50000, 100000, 150000, 200000)
breaks <- c(0, 100000, 200000, 300000)

# storage
correlog_sp <- data.frame(
  d_start = head(breaks, -1),
  d_end   = tail(breaks, -1),
  dist    = head(breaks, -1) + diff(breaks) / 2,
  MoransI = NA_real_,
  Null_lcl = NA_real_,
  Null_ucl = NA_real_,
  Pvalue = NA_real_,
  links = NA_integer_
)

# loop over bands
for (i in seq_len(nrow(correlog_sp))) {
  
  neigh <- dnearneigh(
    x = coords_proj,
    d1 = correlog_sp$d_start[i],
    d2 = correlog_sp$d_end[i],
    longlat = FALSE
  )
  
  correlog_sp$links[i] <- sum(card(neigh))
  
  # skip empty bands
  if (correlog_sp$links[i] == 0) next
  
  wts <- nb2listw(neigh, style = "W", zero.policy = TRUE)
  
  moran_i <- try(
    moran.mc(x = x, listw = wts, nsim = 999, zero.policy = TRUE),
    silent = TRUE
  )
  
  if (inherits(moran_i, "try-error")) {
    print(paste("Failed for band", correlog_sp$d_start[i], "to", correlog_sp$d_end[i]))
    next
  }
  
  correlog_sp$MoransI[i]  <- as.numeric(moran_i$statistic)
  correlog_sp$Null_lcl[i] <- quantile(moran_i$res, 0.025, na.rm = TRUE)
  correlog_sp$Null_ucl[i] <- quantile(moran_i$res, 0.975, na.rm = TRUE)
  correlog_sp$Pvalue[i]   <- moran_i$p.value
}

correlog_sp


plot(
  correlog_sp$dist / 1000,
  correlog_sp$MoransI,
  type = "b",
  xlab = "Distance class midpoint (km)",
  ylab = "Moran's I"
)

abline(h = 0, lty = 2)
lines(correlog_sp$dist / 1000, correlog_sp$Null_lcl, lty = 3)
lines(correlog_sp$dist / 1000, correlog_sp$Null_ucl, lty = 3)

















########### Variogram

library(sf)
library(gstat)
library(ggplot2)

# make sf object from lon/lat
pts <- st_as_sf(dat5, coords = c("lon", "lat"), crs = 4326)

# project to a metric CRS
pts_proj <- st_transform(pts, 3347)

# convert to a plain data frame with projected coordinates
dat_vg <- cbind(st_drop_geometry(pts_proj), st_coordinates(pts_proj))

# empirical variogram
vg <- variogram(pm25 ~ 1, locations = ~ X + Y, data = dat_vg, cutoff = 200000, width = 25000)

# look at values
vg

# plot with ggplot
ggplot(vg, aes(x = dist / 1000, y = gamma)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Distance (km)",
    y = "Semivariance",
    title = "Empirical variogram of PM2.5"
  ) +
  theme_classic()
