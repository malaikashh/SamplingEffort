# packages
library(sf)
library(dplyr)
library(raster)
library(exactextractr)
library(tidyr)

# ---- load data ----

# occurrence points
occ_sf <- st_read("PATH/occurrences.shp")

# protected area layers
polygons_pa <- st_read("PATH/WDPA_Polygons.shp")

# road network
roads_sf <- st_read("PATH/RoadNetwork.shp")

# analysis grid (25 × 25 km polygons)
grid_xy <- st_read("PATH/grid_xy.shp")

# rasters
dem <- raster("PATH/Elevation.tif")
pop_raster <- raster("PATH/global_pop_2025.tif")
gdp_raster <- raster("PATH/GDP_nighttimelights_clipped.tif")
travel_raster <- raster("PATH/traveltime_tocities_clipped.tif")


# ---- prepare grid & coordinate systems ----

target_crs <- 6933  # albers/himalaya region

grid_xy <- grid_xy %>%
  mutate(id = row_number()) %>%
  st_transform(target_crs)

occ_xy <- st_transform(st_zm(occ_sf, drop = TRUE), target_crs)
polygons_pa <- st_transform(st_zm(polygons_pa, drop = TRUE), target_crs)


# ---- protected area ratio per grid ----

occ_xy$grid_id <- sapply(
  st_intersects(occ_xy, grid_xy),
  function(x) if (length(x) == 0) NA else x[1]
)

occ_xy$in_pa <- lengths(st_intersects(occ_xy, polygons_pa)) > 0

pa_ratio_by_grid <- occ_xy %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    n_total = n(),
    n_in_pa = sum(in_pa),
    pa_ratio = n_in_pa / n_total,
    .groups = "drop"
  )

grid_xy <- grid_xy %>%
  left_join(pa_ratio_by_grid, by = c("id" = "grid_id")) %>%
  mutate(
    n_total = replace_na(n_total, 0),
    n_in_pa = replace_na(n_in_pa, 0),
    pa_ratio = replace_na(pa_ratio, 0)
  )


# ---- road density ----

roads_xy <- roads_sf %>%
  filter(GP_RTP %in% c(1, 2, 3)) %>%
  st_zm(drop = TRUE) %>%
  st_transform(target_crs)

roads_g <- st_intersection(roads_xy, grid_xy)
roads_g$length <- as.numeric(st_length(roads_g))

road_summary <- roads_g %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(tot_road_length = sum(length), .groups = "drop")

grid_xy <- grid_xy %>%
  left_join(road_summary, by = "id") %>%
  mutate(
    tot_road_length = replace_na(tot_road_length, 0),
    area = as.numeric(st_area(grid_xy)),
    road_density = tot_road_length / area
  )


# ---- topographic roughness & elevation ----

dem <- projectRaster(dem, crs = st_crs(grid_xy)$proj4string)
rough <- terrain(dem, opt = "roughness", unit = "degrees")

grid_xy$mean_roughness <- exact_extract(rough, grid_xy, "mean")
grid_xy$mean_elevation <- exact_extract(dem, grid_xy, "mean")

grid_xy$mean_roughness[is.na(grid_xy$mean_roughness)] <- 0
grid_xy$mean_elevation[is.na(grid_xy$mean_elevation)] <- 0


# ---- population density ----

pop_raster <- projectRaster(pop_raster, crs = st_crs(grid_xy)$proj4string)

grid_xy$mean_pop <- exact_extract(pop_raster, grid_xy, "mean")
grid_xy$mean_pop[is.na(grid_xy$mean_pop)] <- 0


# ---- gdp ----

gdp_raster <- projectRaster(gdp_raster, crs = st_crs(grid_xy)$proj4string)

grid_xy$mean_gdp <- exact_extract(gdp_raster, grid_xy, "mean")
grid_xy$mean_gdp[is.na(grid_xy$mean_gdp)] <- 0


# ---- travel time ----

travel_raster <- projectRaster(travel_raster, crs = st_crs(grid_xy)$proj4string)

grid_xy$mean_travel <- exact_extract(travel_raster, grid_xy, "mean")
grid_xy$mean_travel[is.na(grid_xy$mean_travel)] <- 0


# ---- longitude & latitude in WGS84 ----

centroids_ll <- st_transform(grid_xy, 4326) %>% st_centroid()
coords <- st_coordinates(centroids_ll)

grid_xy$Longitude <- coords[, 1]
grid_xy$Latitude  <- coords[, 2]


# ---- final predictor table ----

grid_xy <- grid_xy %>%
  select(
    id,
    num_occurrences,
    pa_ratio,
    road_density,
    mean_roughness,
    mean_elevation,
    mean_pop,
    mean_gdp,
    mean_travel,
    Longitude, Latitude,
    geometry
  )


###############################################################################



