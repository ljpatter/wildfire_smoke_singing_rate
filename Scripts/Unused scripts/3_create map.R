### Load packages

library(tidyverse)
library(dplyr)

### Load files that contain site coordinates

coord <- read.csv("Input/ABMI_Site_Public_Locations.csv")
coord2 <- read.csv("Input/potential_locations.csv")

### Load site names

sites <- read.csv("Output/ttest_data.csv")

# Split Location column in sites

subset_coord <- sites[1:39, ]

# Split "Location" column into two based on the hyphen for rows 1 through 39

split_coord <- separate(subset_coord, col = Location, into = c("Location", "Quadrant"), sep = "-")
split_coord <- subset(split_coord, select = -c(Quadrant))
sites <- rbind(split_coord, sites[40:44, ])

# Rename location column in ABMI file so that it can be merged with "sites"
names(coord)[which(names(coord) == "ABMI.Site.Number")] <- "Location"
names(coord)[which(names(coord) == "ABMI.Public.Latitude")] <- "lat"
names(coord)[which(names(coord) == "ABMI.Public.Longitude")] <- "long"
names(coord2)[which(names(coord2) == "location")] <- "Location"
names(coord2)[which(names(coord2) == "latitude")] <- "lat"
names(coord2)[which(names(coord2) == "longitude")] <- "long"
coord2$Location <- toupper(coord2$Location)

# Merge sites and coord dataframes by "Location" column
merged_data <- merge(sites, coord, by = "Location", all.x = TRUE)
merged_data2 <- merge(merged_data, coord2, by = c("Location"), all.x = TRUE)

# Merge the columns "lat.x" and "lat.y" into a single column called "lat_merged"
merged_data2$lat_merged <- ifelse(!is.na(merged_data2$lat.x), merged_data2$lat.x, merged_data2$lat.y)
merged_data2 <- subset(merged_data2, select = -c(lat.x, lat.y))
merged_data2$long_merged <- ifelse(!is.na(merged_data2$long.x), merged_data2$long.x, merged_data2$long.y)
merged_data2 <- subset(merged_data2, select = -c(long.x, long.y))

# Filter out only the desired columns
filtered_data <- subset(merged_data2, select = c("Location", "lat_merged", "long_merged"))
sites <- filtered_data %>%
  rename(lat = lat_merged,
         long = long_merged)



##### CREATE MAP ######


# Making maps -------------------------------------------------------------
library(sf)
library(tmap)
library(ggspatial)
library(terra)
library(ggplot2)
library(tidyverse)

# To make map, you need to decide your map projection (how you represent 3D structure of the planet in 2D)
# Projection where going with is Lambert conformal conical (if need a different project, search proj4 projection for (coordinate system))
lcc_crs <- "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# Import shapefiles of NA and project to LCC
NAmerica<-st_read("Input/boundaries/BCR_Terrestrial_master.shp") %>%     # equivalent of "Add Data" in ArcGIS
  st_transform(lcc_crs) %>%    # change projection to the one we just specified
  group_by(COUNTRY) %>%
  summarize()    #dissolve polygons within countries
plot(NAmerica["COUNTRY"])

provs <-  st_read("Input/boundaries/BCR_Terrestrial_master.shp") %>%
  filter(COUNTRY == "CANADA") %>%     #limit to Canada
  st_transform(lcc_crs) %>%       #reproject as Lambert Conformal Conic
  group_by(PROVINCE_S) %>%
  summarize()      #dissolve provinces within Canadian provinces
plot(provs["PROVINCE_S"])

alberta<-provs %>%
  filter(PROVINCE_S == "ALBERTA")  # filter out "". Here, limits search to Alberta 
#already dissolved polygons within provinces, so no need to dissolve them here
plot(alberta)

# Add city
City <- data.frame(ID = c("Edmonton","Calgary"), 
                   lat = c(53.5461, 51.0447), 
                   long = c(-113.4937, -114.0719))

city.sf <- st_as_sf(x = City,    # st_as_sf converts something that R doesn't know to a shapefile                     
                    coords = c("long", "lat"),
                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

city.sf<-city.sf%>%
  st_transform(lcc_crs)
str(city.sf)

# Add site points
Sites <- data.frame(ID = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), 
                   lat = c(53.95693, 53.95163, 53.81458, 53.77994, 52.83985, 52.81174, 52.65434, 52.64945, 52.61047, 52.48833, 52.43715, 52.42188, 52.28045, 51.61665, 51.61331, 51.57035, 51.68295, 51.66175), 
                   long = c(-114.1982, -113.9085, -114.2355, -113.9376, -115.7156, -115.1207, -115.7667, -115.4535, -115.1265, -115.7924, -115.4954, -115.2506, -115.5696, -115.0475, -115.2091, -115.1771, -115.3273, -115.0696))

sites.sf <- st_as_sf(x = Sites,    # st_as_sf converts something that R doesn't know to a shapefile                     
                    coords = c("long", "lat"),
                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

sites.sf <-sites.sf%>%
  st_transform(lcc_crs)
str(sites.sf)


#Alberta label
Alberta.Pt <- data.frame(ID = "ALBERTA" , lat = 57, long = -113.4937)
Alberta.sf <- st_as_sf(x = Alberta.Pt,                         
                       coords = c("long", "lat"),
                       crs = "+proj=longlat +ellps=WGS84 
                       +datum=WGS84 +no_defs +towgs84=0,0,0")%>%
  st_transform(lcc_crs)


# Using ggplot to make a nice map
Albertamap <- ggplot() + 
  geom_sf(data = NAmerica, size = 0.5, color = "black", fill = "white") +
  geom_sf(data = provs, size = 0.5, color = "black", fill = "white") + 
  geom_sf(data = alberta, size = 0.25, color = "black", fill = "lightyellow") + 
  ggtitle("Map of Alberta") +
  guides(fill = "none") +
  geom_sf(data = Alberta.sf, 
          color = "lightyellow", 
          fill = "lightyellow") + # We want Alberta point on map but invisible
  geom_sf_text(data = city.sf, 
               aes(label = ID), 
               size = 3, 
               nudge_y = 50000, 
               nudge_x = 50000) + # Display city names as text labels
  geom_sf(data = sites.sf, 
          shape = 19, # square symbol instead of solid circle (default)
          size = 1, 
          color = "black", 
          fill = "black") + # keep the points in sites.sf layer
  geom_sf_text(data = sites.sf, 
               aes(label = ID), 
               size = 3, 
               nudge_y = 50000, 
               nudge_x = 50000) + # Display site names as text labels
  geom_text(data = Alberta.sf, 
            aes(x = -1100000, y = 7700000, label = ID), 
            size = 5, 
            color = "black") + # Display "Alberta" as text
  labs(y = "Latitude", x = "Longitude") +
  xlim(-1585420.5, -806054.1) + ylim(6713525.8, 8052465.1) + 
  # Crop map to Alberta's bounding box
  annotation_scale() + # Add scale bar
  annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20")) + # Add a north arrow
  theme_bw()

# Save your map!
ggsave("Graphics/AlbertaMap.png", Albertamap, width=6, height=8, units="in", dpi=300)
















library(sf)
library(ggplot2)

# To make map, you need to decide your map projection (how you represent 3D structure of the planet in 2D)
# Projection where going with is Lambert conformal conical (if need a different project, search proj4 projection for (coordinate system))
lcc_crs <- "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# Import shapefiles of NA and project to LCC
NAmerica <- st_read("Input/boundaries/BCR_Terrestrial_master.shp") %>%
  st_transform(lcc_crs) %>%
  group_by(COUNTRY) %>%
  summarize()    #dissolve polygons within countries

provs <- st_read("Input/boundaries/BCR_Terrestrial_master.shp") %>%
  filter(COUNTRY == "CANADA") %>%     #limit to Canada
  st_transform(lcc_crs) %>%
  group_by(PROVINCE_S) %>%
  summarize()      #dissolve provinces within Canadian provinces

alberta <- provs %>%
  filter(PROVINCE_S == "ALBERTA")  # filter out "". Here, limits search to Alberta 

# Add city
City <- data.frame(ID = c("Edmonton","Calgary"), 
                   lat = c(53.5461, 51.0447), 
                   long = c(-113.4937, -114.0719))

city.sf <- st_as_sf(x = City,    # st_as_sf converts something that R doesn't know to a shapefile                     
                    coords = c("long", "lat"),
                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

city.sf <- city.sf %>%
  st_transform(lcc_crs)

# Add site points
Sites <- data.frame(ID = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), 
                    lat = c(53.95693, 53.95163, 53.81458, 53.77994, 52.83985, 52.81174, 52.65434, 52.64945, 52.61047, 52.48833, 52.43715, 52.42188, 52.28045, 51.61665, 51.61331, 51.57035, 51.68295, 51.66175), 
                    long = c(-114.1982, -113.9085, -114.2355, -113.9376, -115.7156, -115.1207, -115.7667, -115.4535, -115.1265, -115.7924, -115.4954, -115.2506, -115.5696, -115.0475, -115.2091, -115.1771, -115.3273, -115.0696))

sites.sf <- st_as_sf(x = Sites,    # st_as_sf converts something that R doesn't know to a shapefile                     
                     coords = c("long", "lat"),
                     crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

sites.sf <- sites.sf %>%
  st_transform(lcc_crs)


# Alberta label
Alberta.Pt <- data.frame(ID = "ALBERTA" , lat = 57, long = -113.4937)
Alberta.sf <- st_as_sf(x = Alberta.Pt,                         
                       coords = c("long", "lat"),
                       crs = "+proj=longlat +ellps=WGS84 
                        +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
  st_transform(lcc_crs)


# Using ggplot to make a nice map
Albertamap <- ggplot() + 
  geom_sf(data = NAmerica, size = 0.5, color = "black", fill = "white") +
  geom_sf(data = provs, size = 0.5, color = "black", fill = "white") + 
  geom_sf(data = alberta, size = 0.25, color = "black", fill = "lightyellow") + 
  ggtitle("Map of Alberta") +
  guides(fill = "none") +
  geom_sf(data = Alberta.sf, 
          color = "lightyellow", 
          fill = "lightyellow") + # We want Alberta point on map but invisible
  geom_sf_text(data = city.sf, 
               aes(label = ID), 
               size = 3, 
               nudge_y = 50000, 
               nudge_x = 50000) + # Display city names as text labels
  geom_sf(data = sites.sf, 
          shape = 22, # square symbol instead of solid circle (default)
          size = 1, 
          color = "black", 
          fill = "black") + # keep the points in sites.sf layer
  geom_sf_text(data = sites.sf, 
               aes(label = ID), 
               size = 3, 
               nudge_y = 50000, 
               nudge_x = 50000) + # Display site names as text labels
  geom_text(data = Alberta.sf, 
            aes(x = -1400000, y = 7700000, label = ID), 
            size = 5, 
            color = "black") + # Display "Alberta" as text
  labs(y = "Latitude", x = "Longitude") +
  xlim(-1585420.5, -806054.1) + ylim(6713525.8, 8052465.1) + 
  # Crop map to Alberta's bounding box
  annotation_scale() + # Add scale bar
  annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20")) + # Add a north arrow
  theme_bw()

# Save your map!
ggsave("Graphics/AlbertaMap.png", Albertamap, width=6, height=8, units="in", dpi=300)






write.csv(Sites, "Output/site_coords.csv")






