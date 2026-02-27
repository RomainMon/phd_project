#------------------------------------------------#
# Author: Romain Monassier
# Objective: LULC data preparation for simulation
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(ggplot2)
library(raster)
library(terra)
library(lulcc)
library(landscapemetrics)

### Import rasters -------

## Land use
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_lulc = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_lulc = as.numeric(years_lulc)) %>%
  dplyr::arrange(years_lulc)
# Load rasters in chronological order
rasters_reclass = lapply(raster_df$file, terra::rast)
years_lulc = raster_df$years_lulc
# Check
for (i in seq_along(rasters_reclass)) {
  cat("Year", years_lulc[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_reclass[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))


## Slope
slope_r = terra::rast(here("data", "geo", "TOPODATA", "work", "slope_bbox.tif"))
plot(slope_r)

## Altitude
topo_r = terra::rast(here("data", "geo", "TOPODATA", "work", "topo_bbox.tif"))
plot(topo_r)

### WorldClim
## Precipitations (sum)
base_path = here("outputs", "data", "WorldClim", "prec")
raster_files = list.files(base_path, pattern = "^prec_(sum)_\\d{4}_bbox\\.tif$", full.names = TRUE)

# Extract years
years_climate = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")

# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_climate = as.numeric(years_climate)) %>%
  dplyr::arrange(years_climate)

# Load rasters in chronological order
rasters_prec_sum = lapply(raster_df$file, terra::rast)
years_climate = raster_df$years_climate
# Check
for (i in seq_along(rasters_prec_sum)) {
  cat("Year", years_climate[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_prec_sum[[36]])

## Tmin (mean)
base_path = here("outputs", "data", "WorldClim", "tmin")
raster_files = list.files(base_path, pattern = "^tmin_(mean)_\\d{4}_bbox\\.tif$", full.names = TRUE)

# Extract years
years_climate = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")

# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_climate = as.numeric(years_climate)) %>%
  dplyr::arrange(years_climate)

# Load rasters in chronological order
rasters_tmin_mean = lapply(raster_df$file, terra::rast)
years_climate = raster_df$years_climate
# Check
for (i in seq_along(rasters_tmin_mean)) {
  cat("Year", years_climate[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_tmin_mean[[36]])

## Tmin (max)
base_path = here("outputs", "data", "WorldClim", "tmax")
raster_files = list.files(base_path, pattern = "^tmax_(mean)_\\d{4}_bbox\\.tif$", full.names = TRUE)

# Extract years
years_climate = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")

# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_climate = as.numeric(years_climate)) %>%
  dplyr::arrange(years_climate)

# Load rasters in chronological order
rasters_tmax_mean = lapply(raster_df$file, terra::rast)
years_climate = raster_df$years_climate
# Check
for (i in seq_along(rasters_tmax_mean)) {
  cat("Year", years_climate[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_tmax_mean[[36]])


### Import vectors -----
## CAR (Properties)
car = terra::vect(here("data", "geo", "IBGE", "cadastro_car", "AREA_IMOVEL_RJ_2024", "AREA_IMOVEL_bbox.shp"))
car = terra::project(car, "EPSG:31983")
plot(car)
car_sf = sf::st_as_sf(car)

# Mutate area and id
car_sf = car_sf %>% dplyr::mutate(car_id = row_number(),
                                  car_area_m2 = round(as.numeric(sf::st_area(geometry)),2),
                                  car_area_ha = round((car_area_m2 / 10000), 2))

## CAR (Reserva Legal)
rl = terra::vect(here("data", "geo", "IBGE", "cadastro_car", "RESERVA_LEGAL", "RESERVA_LEGAL_bbox.shp"))
rl = terra::project(rl, "EPSG:31983")
plot(rl)
rl_sf = sf::st_as_sf(rl)

## RPPN
rppn = terra::vect(here("data", "geo", "AMLD", "RPPN", "RPPN_RJ.shp"))
rppn = terra::project(rppn, "EPSG:31983")
plot(rppn)
rppn_sf = sf::st_as_sf(rppn)

## Roads
roads = terra::vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))
roads = terra::project(roads, "EPSG:31983")
plot(roads)
roads_sf = sf::st_as_sf(roads)
# BR 101
br101 = roads_sf %>% 
  dplyr::filter(ref == "BR-101")
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(br101), col="#ff3399", add=TRUE)

## Rivers
rivers = terra::vect(here("data", "geo", "OSM", "work", "Waterway_OSM_AMLD_clean.shp"))
rivers = terra::project(rivers, "EPSG:31983")
plot(rivers)
rivers_sf = sf::st_as_sf(rivers)

## Urban centers
urb = terra::vect(here("data", "geo", "IBGE", "admin", "RJ_Setores_CD2022", "setores_urb_bbox.shp"))
urb = terra::project(urb, "EPSG:31983")
plot(urb)
urb_sf = sf::st_as_sf(urb)
urb_centers = terra::centroids(urb)
plot(urb_centers)
plot(urb, add=TRUE)
urb_centers_sf = sf::st_as_sf(urb_centers)

## APA mld
apa_mld = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "apa_mld.shp"))
apa_mld = terra::project(apa_mld, "EPSG:31983")
plot(apa_mld)
apa_mld_sf = sf::st_as_sf(apa_mld)

## Public reserves
# Poco das Antas
pda = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "poco_das_antas.shp"))
pda = terra::project(pda, "EPSG:31983")
plot(pda)
# Tres Picos
tp = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "tres_picos.shp"))
tp = terra::project(tp, "EPSG:31983")
plot(tp)
# Uniao
uniao = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "uniao.shp"))
uniao = terra::project(uniao, "EPSG:31983")
plot(uniao)
# Merge
pub_res = rbind(pda, tp, uniao)
pub_res_sf = sf::st_as_sf(pub_res)
plot(pub_res)


### Create Explanatory Variables Raster List ------
# EV are represented by an object of class ExpVarRasterList
# Can be either static (one map provided for the study period) or dynamic (one map provided for each year of the study period)

#### Create rasters -------
# Select template raster to create other rasters
template_rast = rasters_reclass[[1]]
plot(template_rast, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
names(rasters_reclass) = years_lulc

##### Legal status -------
## Rasterize data
# Private properties
car_r = terra::rasterize(car_sf, template_rast, field = 1, background = 0)
plot(car_r)
# Public reserve
pub_res_r = terra::rasterize(pub_res_sf, template_rast, field = 1, background = 0)
plot(pub_res_r)
# RPPN
rppn_r = terra::rasterize(rppn_sf, template_rast, field = 1, background = 0)
plot(rppn_r)
# Reserva legal
rl_r = terra::rasterize(rl_sf, template_rast, field = 1, background = 0)
plot(rl_r)
# APA
apa_r = terra::rasterize(apa_mld_sf, template_rast, field = 1, background = 0)
plot(apa_r)

##### Distances -------
# Rivers (static)
rivers_bin = terra::rasterize(rivers_sf, template_rast, field = 1, background = NA)
plot(rivers_bin, col="purple")
dist_rivers_r = terra::distance(rivers_bin)
plot(dist_rivers_r)

## Urban centers (static)
urban_bin = terra::rasterize(urb_sf, template_rast, field = 1, background = NA)
plot(urban_bin, col="purple")
dist_urban_r = terra::distance(urban_bin)
plot(dist_urban_r)

## Roads (dynamic)
dist_roads_list = list()
# Loop over years
for (yr in years_lulc) {
  # Keep roads created that year or before
  roads_sub = roads_sf[roads_sf$date_crea <= yr, ]
  
  # Check if roads exist
  if (nrow(roads_sub) == 0) {
    raster_list[[as.character(yr)]] = NA
    next
  }
  
  # Rasterize roads
  roads_bin = terra::rasterize(roads_sub, template_rast, field = 1, background = NA)
  
  # Compute distance
  dist_r = terra::distance(roads_bin)
  
  # Store the raster into a list
  dist_roads_list[[as.character(yr)]] = dist_r
}
plot(dist_roads_list[[1]])
plot(dist_roads_list[[36]])

## Forest edges (dynamic)
dist_edges_list =list()  # Liste pour stocker les rasters de distance aux bords

# Boucle sur les années
for (yr in years_lulc) {
  cat("  Processing forest edges for year:", yr, "\n")
  
  # LULC raster for a given year
  r_year = rasters_reclass[[as.character(yr)]]
  
  # Create a binary mask (forest = 1, the rest = NA)
  forest_mask = r_year
  forest_mask[forest_mask != 1] = NA
  
  # Compute forest edges
  edge_list = landscapemetrics::get_boundaries(forest_mask, as_NA = TRUE)
  edge_rast = edge_list[[1]]
  
  # Compute distance
  dist_edge_r = terra::distance(edge_rast)
  
  # Store the raster
  dist_edges_list[[as.character(yr)]] = dist_edge_r
}
plot(dist_edges_list[[1]])
plot(dist_edges_list[[36]])


##### LULC in a buffer -------

## Forest
# Initialisation
prop_forest100m_list = list()  # List to store rasters

# Parameters
cl = 1  # Land use of interest
radius = 100 # Radius in meters

for (yr in years_lulc) {
  cat("  Processing LULC proportion for year:", yr, "\n")
  
  # Raster of a given year
  r_year = rasters_reclass[[as.character(yr)]]
  
  # Create binary mask
  r_bin = r_year == cl
  
  # Create a circular focal matrix using the radius parameter
  w = terra::focalMat(r_bin, type = "circle", d = radius, fillNA = TRUE)
  
  # Count pixels in the neighborhood
  focal_count = terra::focal(r_bin, w = w, fun = "sum", na.rm = TRUE)
  
  # Count valid pixels (not NAs) in the neighborhood
  r_valid = !is.na(r_year)
  focal_valid = terra::focal(r_valid, w = w, fun = "sum", na.rm = TRUE)
  
  # Compute the proportion of LULC
  prop = focal_count / focal_valid
  prop[is.infinite(prop) | is.na(prop)] = NA
  
  # Store the result in a list
  prop_forest100m_list[[as.character(yr)]] = prop
}
plot(prop_forest100m_list[[1]])
plot(prop_forest100m_list[[36]])

## Agriculture
# Initialisation
prop_agri100m_list = list()  # List to store rasters

# Parameters
cl = 4  # Land use of interest
radius = 100 # Radius in meters

for (yr in years_lulc) {
  cat("  Processing LULC proportion for year:", yr, "\n")
  
  # Raster of a given year
  r_year = rasters_reclass[[as.character(yr)]]
  
  # Create binary mask
  r_bin = r_year == cl
  
  # Create a circular focal matrix using the radius parameter
  w = terra::focalMat(r_bin, type = "circle", d = radius, fillNA = TRUE)
  
  # Count pixels in the neighborhood
  focal_count = terra::focal(r_bin, w = w, fun = "sum", na.rm = TRUE)
  
  # Count valid pixels (not NAs) in the neighborhood
  r_valid = !is.na(r_year)
  focal_valid = terra::focal(r_valid, w = w, fun = "sum", na.rm = TRUE)
  
  # Compute the proportion of LULC
  prop = focal_count / focal_valid
  prop[is.infinite(prop) | is.na(prop)] = NA
  
  # Store the result in a list
  prop_agri100m_list[[as.character(yr)]] = prop
}
plot(prop_agri100m_list[[1]])
plot(prop_agri100m_list[[36]])

## Urban
# Initialisation
prop_urban100m_list = list()  # List to store rasters

# Parameters
cl = 6  # Land use of interest
radius = 100 # Radius in meters

for (yr in years_lulc) {
  cat("  Processing LULC proportion for year:", yr, "\n")
  
  # Raster of a given year
  r_year = rasters_reclass[[as.character(yr)]]
  
  # Create binary mask
  r_bin = r_year == cl
  
  # Create a circular focal matrix using the radius parameter
  w = terra::focalMat(r_bin, type = "circle", d = radius, fillNA = TRUE)
  
  # Count pixels in the neighborhood
  focal_count = terra::focal(r_bin, w = w, fun = "sum", na.rm = TRUE)
  
  # Count valid pixels (not NAs) in the neighborhood
  r_valid = !is.na(r_year)
  focal_valid = terra::focal(r_valid, w = w, fun = "sum", na.rm = TRUE)
  
  # Compute the proportion of LULC
  prop = focal_count / focal_valid
  prop[is.infinite(prop) | is.na(prop)] = NA
  
  # Store the result in a list
  prop_urban100m_list[[as.character(yr)]] = prop
}
plot(prop_urban100m_list[[1]])
plot(prop_urban100m_list[[36]])


##### North and South of BR101 -------
br_coords = sf::st_coordinates(br101)  # x and y coordinates of BR-1010

# Create an empty raster
ns_raster = terra::rast(template_rast)
ns_raster[] = NA 

# Determine whether a point is North or South of located coords
compute_ns = function(x, y, br_coords) {
  # Find nearest coord (in longitude, x)
  idx = which.min(abs(br_coords[, 1] - x))
  br_y = br_coords[idx, 2] # Y coord of the nearest BR-101 point
  
  # Compare Y with the BR-101 Y coordinate
  if (y > br_y) {
    return(0)  # North
  } else {
    return(1)  # South
  }
}

# Apply to all the raster
# We transform the raster to data frame
raster_df = as.data.frame(ns_raster, xy = TRUE, na.rm = FALSE)

# Apply the function to each X, Y point
raster_df$ns = mapply(
  compute_ns,
  x = raster_df$x,
  y = raster_df$y,
  MoreArgs = list(br_coords = br_coords)
)

# Rebuild raster with 0 (North) and 1 (South)
ns_raster[] = raster_df$ns

# Plot
plot(ns_raster, main = "Nord (0) / Sud (1) de la BR-101", col = c("blue", "red"))

##### Water (constrained) -------
# Here, we create a constraint on land development
# See PLUS documentation: "Users should prepare a binary image of restricted area that only contains value of 0 or 1. The value 0 means no conversion while the value 1 means convertible"
water_list = list()
# Loop over years
for (yr in years_lulc) {
  # LULC raster for a given year
  r_year = rasters_reclass[[as.character(yr)]]
  
  # Create a binary mask (water = 5, the rest = 1)
  water_mask = r_year
  water_mask[water_mask != 5] = 1
  water_mask[water_mask == 5] = 0 # Transform 5 to 0
  
  # Store the raster into a list
  water_list[[as.character(yr)]] = water_mask
}
plot(water_list$`2024`, col=c("blue","grey"))

### Export rasters for PLUS ------

#### Reproject -------

# Template raster
template_rast = rasters_reclass[[1]]
crs(template_rast)
template_wgs84 = terra::project(template_rast, "EPSG:4326", method = "near")
crs(template_wgs84)

## Reproject
# LULC
rasters_reclass_wgs84 = lapply(rasters_reclass, terra::project, template_wgs84, method = "near")

# Slope
slope_r = terra::project(slope_r, template_wgs84, method="bilinear")

# Elevation
topo_r = terra::project(topo_r, template_wgs84, method="bilinear")

# Distances
dist_rivers_r = terra::project(dist_rivers_r, template_wgs84, method="bilinear")
dist_urban_r = terra::project(dist_urban_r, template_wgs84, method="bilinear")
dist_edges_list = lapply(dist_edges_list, terra::project, template_wgs84, method = "near")
dist_roads_list = lapply(dist_roads_list, terra::project, template_wgs84, method = "near")

# Proportions
prop_agri100m_list = lapply(prop_agri100m_list, terra::project, template_wgs84, method = "near")
prop_urban100m_list = lapply(prop_urban100m_list, terra::project, template_wgs84, method = "near")

# Binary rasters
apa_r = terra::project(apa_r, template_wgs84, method="near")
car_r = terra::project(car_r, template_wgs84, method="near")
rl_r = terra::project(rl_r, template_wgs84, method="near")
rppn_r = terra::project(rppn_r, template_wgs84, method="near")
pub_res_r = terra::project(pub_res_r, template_wgs84, method="near")
ns_raster = terra::project(ns_raster, template_wgs84, method="near")
water_list = lapply(water_list, terra::project, template_wgs84, method = "near")

#### Export --------
# Define output folder
output_dir = here("outputs", "data", "PLUS", "lulc")

##### LULC -------
for (i in seq_along(rasters_reclass_wgs84)) {
  year_i = years_lulc[i]
  output_path = file.path(output_dir,
                          paste0("raster_reclass_wgs84_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_reclass_wgs84[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}

##### Distances -------
output_dir = here("outputs", "data", "PLUS", "drivers")

# Distances to edges
terra::writeRaster(dist_edges_list[['2024']], 
                   filename = file.path(output_dir, "dist_edges2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

# Distances to roads
terra::writeRaster(dist_roads_list[['2024']], 
                   filename = file.path(output_dir, "dist_roads2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

# Distance to urban centers
terra::writeRaster(dist_urban_r, 
                   filename = file.path(output_dir, "dist_urban.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

# Distance to rivers
terra::writeRaster(dist_rivers_r, 
                   filename = file.path(output_dir, "dist_rivers.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

##### Legal status -------
terra::writeRaster(apa_r, 
                   filename = file.path(output_dir, "apa.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")
terra::writeRaster(rl_r, 
                   filename = file.path(output_dir, "rl.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")
terra::writeRaster(rppn_r, 
                   filename = file.path(output_dir, "rppn.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")
terra::writeRaster(pub_res_r, 
                   filename = file.path(output_dir, "pub_res.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")
terra::writeRaster(car_r, 
                   filename = file.path(output_dir, "car.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")

##### North/South BR-101 -------
terra::writeRaster(ns_raster, 
                   filename = file.path(output_dir, "ns_br101.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")

##### Climate ------
# Precipitations
terra::writeRaster(rasters_prec_sum[[36]], 
                   filename = file.path(output_dir, "prec2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

# Tmin
terra::writeRaster(rasters_tmin_mean[[36]], 
                   filename = file.path(output_dir, "tmin2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

##### Topography ------
# Slope
terra::writeRaster(slope_r, 
                   filename = file.path(output_dir, "slope.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")


##### Proportions of land uses ------
# Agriculture
terra::writeRaster(prop_agri100m_list[[36]], 
                   filename = file.path(output_dir, "prop_agri100m_2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")
# Urban
terra::writeRaster(prop_urban100m_list[[36]], 
                   filename = file.path(output_dir, "prop_urban100m_2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

##### Other constraints ------
output_dir = here("outputs", "data", "PLUS", "constraint")
# Open water
terra::writeRaster(water_list[['2024']], 
                   filename = file.path(output_dir, "water2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")

#### PLUS parameters --------
## Land use demand
freq(rasters_reclass_wgs84[[1]]) # Frequency of each land use
freq(rasters_reclass_wgs84[[36]]) # Land use demand

## Transition matrix
# Compute transition matrix
tm = terra::crosstab(c(rasters_reclass_wgs84[[1]], rasters_reclass_wgs84[[36]]))
# 0 if < 500, 1 otherwise
tm_binary = ifelse(tm < 500, 0, 1)
print(tm_binary)

## Neighborhood weights
# Based on PLUS documentation, we can determine the neighborhood weight of each land-use type by calculating the ratio of the expansion areas of a land-use type accounting for the total land expansion
# Import land use changes (1989-2024)
lulc_19892024 = terra::rast(here("outputs", "data", "PLUS", "lulc_expansion", "lulc_expansion_19892024_landuse_1to2.tif"))
plot(lulc_19892024)
# Frequency table
freq_lulc19892024 = freq(lulc_19892024)
# Filter lines where value != 0
freq_filtered = freq_lulc19892024[freq_lulc19892024$value != 0, ]
# Sum of LULC changes
tot = sum(freq_filtered$count)
# Proportion of each change
freq_filtered$proportion = freq_filtered$count / tot
# Print result
print(freq_filtered[, c("value", "count", "proportion")])

