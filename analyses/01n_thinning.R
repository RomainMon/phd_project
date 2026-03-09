#------------------------------------------------#
# Author: Romain Monassier
# Objective: Spatial thinning
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(ggplot2)
library(spdep)
library(terra)
library(sp)
library(GeoThinneR)

### The objective of the script is to do spatial thinning
# Spatial thinning is random spatial filtering of spatial units (e.g., points) based on a distance metric (e.g., min distance)
# This allows reducing spatial autocorrelation

### Load datasets
data_defor_pixel = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_pixel.rds"))
data_refor_pixel = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_refor_pixel.rds"))
lulc_2024 = terra::rast(here("outputs", "data", "MapBiomas", "Rasters_reclass", "raster_reclass_2024.tif"))

### Deforestation dataset ----------
#### 1) Thinning event cells ---------
data_defor_8 = data_defor_pixel %>% 
  dplyr::filter(type == 8)
# convert to sf
pts = sf::st_as_sf(
  data_defor_8,
  coords = c("x","y"),
  crs = 31983,
  remove = FALSE
)
# transform to WGS84
pts_4326 = sf::st_transform(pts, 4326)
# extract lon lat
coords = sf::st_coordinates(pts_4326)
# Mutate long/lat coordinates (4326)
data_defor_8 = pts_4326 %>%
  dplyr::mutate(
    lon = coords[,1],
    lat = coords[,2]
  ) %>%
  sf::st_drop_geometry()
# Thinning
data_defor_8_thin = GeoThinneR::thin_points(
  data = data_defor_8,  # Dataframe with coordinates
  group_col = "year",
  lon_col = "lon", # Longitude column name
  lat_col = "lat",  # Latitude column name
  method = "distance", # Method for thinning
  thin_dist = 0.2, # Thinning distance in km,
  trials = 3, # Number of trials
  all_trials = FALSE,  # Return all trials
  seed = 123 # Seed for reproducibility
)
# Summary
summary(data_defor_8_thin)
sapply(data_defor_8_thin$retained, sum) # Number of kept points in each trial
data_defor_8_thin_select = GeoThinneR::as_sf(data_defor_8_thin) # Convert to sf

#### 2) Thinning control cells -------
data_defor_1 = data_defor_pixel %>% 
  dplyr::filter(type == 1)
# convert to sf
pts = sf::st_as_sf(
  data_defor_1,
  coords = c("x","y"),
  crs = 31983,
  remove = FALSE
)
# transform to WGS84
pts_4326 = sf::st_transform(pts, 4326)
# extract lon lat
coords = sf::st_coordinates(pts_4326)
# Mutate long/lat coordinates (4326)
data_defor_1 = pts_4326 %>%
  dplyr::mutate(
    lon = coords[,1],
    lat = coords[,2]
  ) %>%
  sf::st_drop_geometry()
# Thinning
data_defor_1_thin = GeoThinneR::thin_points(
  data = data_defor_1,  # Dataframe with coordinates
  group_col = "year",
  lon_col = "lon", # Longitude column name
  lat_col = "lat",  # Latitude column name
  method = "distance", # Method for thinning
  thin_dist = 0.2, # Thinning distance in km,
  trials = 2, # Number of trials
  all_trials = FALSE,  # Return all trials
  seed = 123 # Seed for reproducibility
)
# Summary
summary(data_defor_1_thin)
sapply(data_defor_1_thin$retained, sum) # Number of kept points in each trial
data_defor_1_thin_select = GeoThinneR::as_sf(data_defor_1_thin) # Convert to sf

#### 3) Bind datasets ------
data_defor_thin = dplyr::bind_rows(data_defor_8_thin_select,
                                   data_defor_1_thin_select)
data_defor_thin = data_defor_thin %>% 
  sf::st_drop_geometry() %>% 
  as.data.frame()

#### 4) Select a balanced number of controls per year -------
n_events_year = data_defor_thin %>%
  dplyr::filter(type == 8) %>%
  dplyr::count(year, name = "n_events")
# Selection
controls_balanced = data_defor_thin %>%
  dplyr::filter(type == 1) %>%
  dplyr::left_join(n_events_year, by = "year") %>%
  dplyr::group_by(year) %>%
  dplyr::group_modify(~{
    
    n_events = .x$n_events[1]
    # sample controls for that year
    dplyr::slice_sample(.x, n = min(n_events, nrow(.x)))
    
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n_events)
# Events
events = data_defor_thin %>%
  dplyr::filter(type == 8)
# Final dataset
data_defor_final = dplyr::bind_rows(events, controls_balanced)
# Check
data_defor_final %>%
  dplyr::count(year, type)
# Plot
pts_defor_2024_1 = data_defor_final %>% dplyr::filter(year == 2024 & type == 1) %>% sf::st_as_sf(coords = c("x", "y"))
pts_defor_2024_8 = data_defor_final %>% dplyr::filter(year == 2024 & type == 8) %>% sf::st_as_sf(coords = c("x", "y"))

plot(lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(st_geometry(pts_defor_2024_1), col = "aquamarine", pch = 20, cex = 0.7, add = TRUE)
plot(st_geometry(pts_defor_2024_8), col = "magenta", pch = 20, cex = 0.7, add = TRUE)


### Reforestation dataset ----------
#### 1) Thinning event cells ----------
data_refor_7 = data_refor_pixel %>% 
  dplyr::filter(type == 7)
# convert to sf
pts = sf::st_as_sf(
  data_refor_7,
  coords = c("x","y"),
  crs = 31983,
  remove = FALSE
)
# transform to WGS84
pts_4326 = sf::st_transform(pts, 4326)
# extract lon lat
coords = sf::st_coordinates(pts_4326)
# Mutate long/lat coordinates (4326)
data_refor_7 = pts_4326 %>%
  dplyr::mutate(
    lon = coords[,1],
    lat = coords[,2]
  ) %>%
  sf::st_drop_geometry()
# Thinning
data_refor_7_thin = GeoThinneR::thin_points(
  data = data_refor_7,  # Dataframe with coordinates
  group_col = "year",
  lon_col = "lon", # Longitude column name
  lat_col = "lat",  # Latitude column name
  method = "distance", # Method for thinning
  thin_dist = 0.2, # Thinning distance in km,
  trials = 3, # Number of trials
  all_trials = FALSE,  # Return all trials
  seed = 123 # Seed for reproducibility
)
# Summary
summary(data_refor_7_thin)
sapply(data_refor_7_thin$retained, sum) # Number of kept points in each trial
data_refor_7_thin_select = GeoThinneR::as_sf(data_refor_7_thin) # Convert to sf

#### 2) Thinning control cells  ----------
data_refor_4 = data_refor_pixel %>% 
  dplyr::filter(type == 4)
# convert to sf
pts = sf::st_as_sf(
  data_refor_4,
  coords = c("x","y"),
  crs = 31983,
  remove = FALSE
)
# transform to WGS84
pts_4326 = sf::st_transform(pts, 4326)
# extract lon lat
coords = sf::st_coordinates(pts_4326)
# Mutate long/lat coordinates (4326)
data_refor_4 = pts_4326 %>%
  dplyr::mutate(
    lon = coords[,1],
    lat = coords[,2]
  ) %>%
  sf::st_drop_geometry()
# Thinning
data_refor_4_thin = GeoThinneR::thin_points(
  data = data_refor_4,  # Dataframe with coordinates
  group_col = "year",
  lon_col = "lon", # Longitude column name
  lat_col = "lat",  # Latitude column name
  method = "distance", # Method for thinning
  thin_dist = 0.2, # Thinning distance in km,
  trials = 2, # Number of trials
  all_trials = FALSE,  # Return all trials
  seed = 123 # Seed for reproducibility
)
# Summary
summary(data_refor_4_thin)
sapply(data_refor_4_thin$retained, sum) # Number of kept points in each trial
data_refor_4_thin_select = GeoThinneR::as_sf(data_refor_4_thin) # Convert to sf

#### 3) Bind datasets  ----------
data_refor_thin = dplyr::bind_rows(data_refor_7_thin_select,
                                   data_refor_4_thin_select)
data_refor_thin = data_refor_thin %>% 
  sf::st_drop_geometry() %>% 
  as.data.frame()

#### 4) Select a balanced number of controls per year  ----------
n_events_year = data_refor_thin %>%
  dplyr::filter(type == 7) %>%
  dplyr::count(year, name = "n_events")
# Selection
controls_balanced = data_refor_thin %>%
  dplyr::filter(type == 4) %>%
  dplyr::left_join(n_events_year, by = "year") %>%
  dplyr::group_by(year) %>%
  dplyr::group_modify(~{
    
    n_events = .x$n_events[1]
    # sample controls for that year
    dplyr::slice_sample(.x, n = min(n_events, nrow(.x)))
    
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n_events)
# Events
events = data_refor_thin %>%
  dplyr::filter(type == 7)
# Final dataset
data_refor_final = dplyr::bind_rows(events, controls_balanced)
# Check
data_refor_final %>%
  dplyr::count(year, type)
str(data_refor_final)
# Plot
pts_refor_2024_4 = data_refor_final %>% dplyr::filter(year == 2024 & type == 4) %>% sf::st_as_sf(coords = c("x", "y"))
pts_refor_2024_7 = data_refor_final %>% dplyr::filter(year == 2024 & type == 7) %>% sf::st_as_sf(coords = c("x", "y"))

plot(lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(st_geometry(pts_refor_2024_4), col = "tan1", pch = 20, cex = 0.7, add = TRUE)
plot(st_geometry(pts_refor_2024_7), col = "chartreuse", pch = 20, cex = 0.7, add = TRUE)

#### Export datasets ----------
saveRDS(data_defor_final,
        here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_pixel_thinned.rds"))
saveRDS(data_refor_final,
        here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_refor_pixel_thinned.rds"))
