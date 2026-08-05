# Load packages

library(gstat)
library(sp)
library(sf)
library(raster)


# Step 1: Load your data (assuming station_coords and aru_coords are loaded)

station_coords <- read.csv("Input/kriging_data.csv")
aru_coords <- read.csv("Input/site_coords.csv")

# Step 2: Convert dataframes to sf objects for spatial operations
station_coords_sf <- st_as_sf(station_coords, coords = c("long", "lat"), crs = 4326)  # WGS84
aru_coords_sf <- st_as_sf(aru_coords, coords = c("long", "lat"), crs = 4326)          # WGS84

# Step 3: Transform coordinates to UTM Zone 12N (NAD83)
station_coords_utm <- st_transform(station_coords_sf, crs = 26912)  # UTM Zone 12N, NAD83
aru_coords_utm <- st_transform(aru_coords_sf, crs = 26912)

# Step 4: Plot the data (optional)
plot(st_geometry(station_coords_utm), pch = 20, col = "blue", main = "Air Quality Monitoring Stations and ARUs (UTM)")
plot(st_geometry(aru_coords_utm), pch = 19, col = "red", add = TRUE)
legend("bottomleft", legend = c("Stations", "ARUs"), col = c("blue", "red"), pch = c(20, 19))

# Step 5: Create a variogram using sp objects
station_coords_sp <- as_Spatial(station_coords_utm)  # Convert sf to sp object
variogram_pm25 <- variogram(pm25 ~ 1, station_coords_sp)
vgm_model <- fit.variogram(variogram_pm25, model = vgm(1, "Sph", 30000, 1))  # Adjust range for meters
plot(variogram_pm25, model = vgm_model)

# Step 6: Perform ordinary kriging
aru_coords_sp <- as_Spatial(aru_coords_utm)  # Convert sf to sp object
krige_result <- krige(pm25 ~ 1, station_coords_sp, aru_coords_sp, model = vgm_model)

# Step 7: Visualize the kriging results
spplot(krige_result["var1.pred"], main = "Kriged PM2.5 Predictions at ARU Locations (UTM)")

# Step 8: Cross-validation (optional, to evaluate the kriging model)
cv_result <- krige.cv(pm25 ~ 1, station_coords_sp, model = vgm_model, nfold = length(station_coords_sp))

# Print cross-validation results to assess prediction accuracy
summary(cv_result)

# Print the kriged results for the ARU locations
print(krige_result)

