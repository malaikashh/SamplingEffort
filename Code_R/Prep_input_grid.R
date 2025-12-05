
#   creating 25 x 25 km grid 
#   Final output: grid_xy (sf object)

library(sf)
library(dplyr)
library(tidyr)


# load input data

# Species occurrence records (must contain: decimalLongitude, decimalLatitude) 
# occurrences are stored in CSV_data folder

occ_df <- TotalOccurencesSampled

# Himalayas boundary polygon
himalayas <- st_read("himalaya boundary/HimalayasBoundary.shp")


# Project data to common CRS
#    Required for correct distance + grid cell size

target_crs <- 6933   # albers equal area projection

occ_sf <- st_as_sf(
  occ_df,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
) %>% 
  st_transform(target_crs)

himalayas <- st_transform(himalayas, target_crs)


#create 25 × 25 km grid clipped to himalaya boundary 


grid <- st_make_grid(
  himalayas,
  cellsize = c(25000, 25000),
  square = TRUE
) %>%
  st_sf() %>%
  st_intersection(himalayas)

# Give each grid cell a unique ID
grid$grid_id <- 1:nrow(grid)


# SPATIAL JOIN — assign each occurrence to a grid cell 


occ_with_grid <- st_join(occ_sf, grid, left = FALSE)  

# left = FALSE keeps only points that fall inside the himalaya boundary


# calculate sampling_effort  (no.of occurrences per grid)


sampling_effort <- occ_with_grid %>%
  group_by(grid_id) %>%
  summarise(num_occurrences = n())


# merge sampling effort back to grid


grid_xy <- grid %>%
  left_join(sampling_effort %>% st_drop_geometry(), by = "grid_id") %>%
  mutate(
    num_occurrences = replace_na(num_occurrences, 0)
  )


# add centroid coordinates (FOR MODELING)


centroids <- st_centroid(grid_xy)

coords <- st_coordinates(centroids)

grid_xy$X <- coords[, 1]
grid_xy$Y <- coords[, 2]

###############################################################
# Final output: grid_xy  (sf object)
# Contains:
#   geometry (grid polygons)
#   grid_id
#   num_occurrences   = sampling effort
#   X, Y              = centroid coordinates
###############################################################
