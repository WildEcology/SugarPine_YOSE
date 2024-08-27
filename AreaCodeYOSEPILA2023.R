# Load required libraries
library(sf)
library(ggplot2)
library(sp)
library(readr)
library(dplyr)
library(tmap)  # For interactive mapping

# Step 1: Load Data and Ensure Valid Format
waypoints <- read_csv("pila_datum.csv") %>%
  mutate(
    plot_beg_UTM_E = as.numeric(plot_beg_UTM_E),
    plot_beg_UTM_N = as.numeric(plot_beg_UTM_N),
    zone = as.numeric(zone)
  ) %>%
  drop_na(plot_beg_UTM_E, plot_beg_UTM_N, zone)  # Remove rows with NA values

# Step 2: Convert UTM to Lat/Lon
convert_utm_to_latlon <- function(easting, northing, zone) {
  proj_string <- paste0("+proj=utm +zone=", zone, " +datum=WGS84")
  coords <- SpatialPoints(cbind(easting, northing), proj4string = CRS(proj_string))
  latlon <- spTransform(coords, CRS("+proj=longlat +datum=WGS84"))
  return(data.frame(lat = latlon@coords[, 2], lon = latlon@coords[, 1]))
}

latlon_coords <- do.call(rbind, mapply(function(easting, northing, zone) {
  tryCatch({
    convert_utm_to_latlon(easting, northing, zone)
  }, error = function(e) {
    return(data.frame(lat = NA, lon = NA))  # Handle conversion errors
  })
}, waypoints$plot_beg_UTM_E, waypoints$plot_beg_UTM_N, waypoints$zone, SIMPLIFY = FALSE))

waypoints <- cbind(waypoints, latlon_coords) %>%
  drop_na(lat, lon)  # Remove rows with invalid lat/lon

# Step 3: Create sf Object and Reproject CRS
waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 32611)  # Change CRS to your specific UTM zone

# Step 4: Calculate Convex Hull and Area for Each Plot
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),
    area_sq_m = st_area(convex_hull)
  ) %>%
  mutate(area_sq_km = as.numeric(area_sq_m) / 1e6) %>%  # Convert to square kilometers
  st_set_geometry(NULL)  # Remove geometry column for merging

# Step 5: Join Areas Back to Original Data
waypoints_with_area <- waypoints %>%
  left_join(plot_areas, by = "plotID")

# Step 6: Create Interactive Map with tmap
waypoints_sf_with_area <- st_as_sf(waypoints_with_area, coords = c("lon", "lat"), crs = 4326)

tmap_mode("view")  # Set tmap to interactive mode

# Create a map showing all waypoints
tm_shape(waypoints_sf_with_area) +
  tm_dots(col = "plotID", palette = "Set1", title = "Plot ID") +
  tm_layout(legend.position = c("left", "bottom"))

# Step 7: Visualize Results with ggplot
ggplot() +
  geom_sf(data = waypoints_sf_with_area, aes(color = factor(plotID))) +
  geom_sf(data = st_as_sf(plot_areas, crs = 4326), aes(fill = area_sq_km), color = "blue", alpha = 0.2) +
  ggtitle("Waypoints with Convex Hull by PlotID") +
  theme(legend.position = "right")

# Optional: Save the Updated Data
write_csv(waypoints_with_area, "pila_datum_with_area.csv")

######### MORE ################
#issue with calculating areas and projections of plots in wrong spots 

# Read the CSV file containing the waypoints data
waypoints <- read.csv("pila_datum.csv")

# Convert the UTM columns to numeric (this will introduce NAs for any non-numeric values)
waypoints$PILA_UTM_E <- as.numeric(waypoints$PILA_UTM_E)
waypoints$PILA_UTM_N <- as.numeric(waypoints$PILA_UTM_N)

# Remove rows with missing or invalid UTM coordinates
waypoints_clean <- waypoints %>%
  filter(!is.na(PILA_UTM_E) & !is.na(PILA_UTM_N))

# Convert the cleaned waypoints to an sf object (assuming UTM zone 11N, EPSG: 32611)
waypoints_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)

# Group by plotID, calculate convex hull, and compute area in square meters
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),  # Calculate the convex hull of the plot
    area_sq_m = st_area(convex_hull)  # Calculate the area of the convex hull in square meters
  )

# Print the area for each plot
print(plot_areas)

# Optional: Visualize the plot and convex hull
ggplot() +
  geom_sf(data = waypoints_sf, aes(color = factor(plotID))) +  # Plot the tree waypoints
  geom_sf(data = plot_areas, aes(fill = factor(plotID)), alpha = 0.2) +  # Plot the convex hulls
  ggtitle("Plot Convex Hulls and Tree Waypoints") +
  theme_minimal()
