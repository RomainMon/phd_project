#------------------------------------------------#
# Author: Romain Monassier
# Objective: LULC simulation
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


## Cumulative rasters
base_path = here("outputs", "data", "MapBiomas", "Rasters_cumulative_tm")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_tm = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_tm = as.numeric(years_tm)) %>%
  dplyr::arrange(years_tm)
# Load rasters in chronological order
rasters_tm = lapply(raster_df$file, terra::rast)
years_tm = raster_df$years_tm
# Check
for (i in seq_along(rasters_tm)) {
  cat("Year", years_tm[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))


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



#### Transform to RasterLayer objects -----
# We must transform each terra Raster to a raster Raster
# lulcc works with raster objects (RasterLayer)

##### Static rasters -------
slope_r = raster::raster(slope_r)
topo_r = raster::raster(topo_r)
apa_r = raster::raster(apa_r)
rl_r = raster::raster(rl_r)
car_r = raster::raster(car_r)
rppn_r = raster::raster(rppn_r)
pub_res_r = raster::raster(pub_res_r)
dist_urban_r = raster::raster(dist_urban_r)
dist_rivers_r = raster::raster(dist_rivers_r)
ns_raster = raster::raster(ns_raster)

##### Dynamic rasters -compareRaster()##### Dynamic rasters ------

# Precipitations
prec_list = list()
# Loop
for (i in seq_along(rasters_prec_sum)) { # Change "rasters_prec_sum" by the list of terra rasters
  # Extract SpatRaster (terra object)
  spat_raster = rasters_prec_sum[[i]]
  
  # ... to RasterLayer (raster object)
  raster_layer = raster::raster(spat_raster)
  
  # Name raster with the corresponding year
  year = years_lulc[i]  # Replace "years_lulc" by the vector of years
  raster_name = paste0("prec_", year) # Replace by what we name the raster
  
  # Store the raster
  prec_list[[raster_name]] = raster_layer
}
# Check
plot(prec_list$prec_1989)
head(freq(prec_list$prec_1989))
head(freq(rasters_prec_sum[[1]]))
head(freq(prec_list$prec_2024))
head(freq(rasters_prec_sum[[36]]))

# Tmin
tmin_list = list()
# Loop
for (i in seq_along(rasters_tmin_mean)) {
  # Extract SpatRaster (terra object)
  spat_raster = rasters_tmin_mean[[i]]
  
  # ... to RasterLayer (raster object)
  raster_layer = raster::raster(spat_raster)
  
  # Name raster with the corresponding year
  year = years_lulc[i]  # Replace "years_lulc" by the vector of years
  raster_name = paste0("tmin_", year) # Replace by what we name the raster
  
  # Store the raster
  tmin_list[[raster_name]] = raster_layer
}

# Tmax
tmax_list = list()
# Loop
for (i in seq_along(rasters_tmax_mean)) {
  # Extract SpatRaster (terra object)
  spat_raster = rasters_tmax_mean[[i]]
  
  # ... to RasterLayer (raster object)
  raster_layer = raster::raster(spat_raster)
  
  # Name raster with the corresponding year
  year = years_lulc[i]  # Replace "years_lulc" by the vector of years
  raster_name = paste0("tmax_", year) # Replace by what we name the raster
  
  # Store the raster
  tmax_list[[raster_name]] = raster_layer
}

# Distances to roads
dist_roads_list_v2 = list()
# Loop
for (i in seq_along(dist_roads_list)) {
  # Extract SpatRaster (terra object)
  spat_raster = dist_roads_list[[i]]
  
  # ... to RasterLayer (raster object)
  raster_layer = raster::raster(spat_raster)
  
  # Name raster with the corresponding year
  year = years_lulc[i]  # Replace "years_lulc" by the vector of years
  raster_name = paste0("dist_road_", year) # Replace by what we name the raster
  
  # Store the raster
  dist_roads_list_v2[[raster_name]] = raster_layer
}

# Distances to forest edges
dist_edges_list_v2 = list()
# Loop
for (i in seq_along(dist_edges_list)) {
  # Extract SpatRaster (terra object)
  spat_raster = dist_edges_list[[i]]
  
  # ... to RasterLayer (raster object)
  raster_layer = raster::raster(spat_raster)
  
  # Name raster with the corresponding year
  year = years_lulc[i]  # Replace "years_lulc" by the vector of years
  raster_name = paste0("dist_edge_", year) # Replace by what we name the raster
  
  # Store the raster
  dist_edges_list_v2[[raster_name]] = raster_layer
}
# Check
plot(dist_edges_list_v2$dist_edge_2024)
head(freq(dist_edges_list$`2024`))
head(freq(dist_edges_list_v2$dist_edge_2024))

# LULC in buffers
# Forest
prop_forest100m_list_v2 = list()
# Loop
for (i in seq_along(prop_forest100m_list)) {
  # Extract SpatRaster (terra object)
  spat_raster = prop_forest100m_list[[i]]
  
  # ... to RasterLayer (raster object)
  raster_layer = raster::raster(spat_raster)
  
  # Name raster with the corresponding year
  year = years_lulc[i]  # Replace "years_lulc" by the vector of years
  raster_name = paste0("prop_forest_", year) # Replace by what we name the raster
  
  # Store the raster
  prop_forest100m_list_v2[[raster_name]] = raster_layer
}

# Agriculture
prop_agri100m_list_v2 = list()
# Loop
for (i in seq_along(prop_agri100m_list)) {
  # Extract SpatRaster (terra object)
  spat_raster = prop_agri100m_list[[i]]
  
  # ... to RasterLayer (raster object)
  raster_layer = raster::raster(spat_raster)
  
  # Name raster with the corresponding year
  year = years_lulc[i]  # Replace "years_lulc" by the vector of years
  raster_name = paste0("prop_agri_", year) # Replace by what we name the raster
  
  # Store the raster
  prop_agri100m_list_v2[[raster_name]] = raster_layer
}

# Urban
prop_urban100m_list_v2 = list()
# Loop
for (i in seq_along(prop_urban100m_list)) {
  # Extract SpatRaster (terra object)
  spat_raster = prop_urban100m_list[[i]]
  
  # ... to RasterLayer (raster object)
  raster_layer = raster::raster(spat_raster)
  
  # Name raster with the corresponding year
  year = years_lulc[i]  # Replace "years_lulc" by the vector of years
  raster_name = paste0("prop_urban_", year) # Replace by what we name the raster
  
  # Store the raster
  prop_urban100m_list_v2[[raster_name]] = raster_layer
}


#### Compile predictors -----
# Maps of different explanatory variables should have the same coordinate reference system but do not have to have the same extent and resolution as long as the minimum extent is that of the study region defined by an ObsLulcRasterStack object. 
# However, maps for different timesteps of the same dynamic variable should have the same extent and resolution because these are stored as RasterStack objects.

## Static predictors
# First, we compile predictors
ef = list(
  ef_001 = apa_r,
  ef_002 = rl_r,
  ef_003 = car_r,
  ef_004 = rppn_r,
  ef_005 = pub_res_r,
  ef_006 = dist_urban_r,
  ef_007 = dist_rivers_r,
  ef_008 = ns_raster,
  ef_009 = topo_r,
  ef_010 = slope_r)

# Then, we create an ExpVarRasterList object
ef_stack = lulcc::ExpVarRasterList(x = ef, pattern = "ef")


### Create LULC Raster List ------

# lulcc needs RasterLayer objects (raster package) and not SpatRaster (terra)


# Name each land use raster
names(rasters_reclass) = paste0("lu_", years_lulc) # Ex: "lu_1989", "lu_1990", etc.
rasters_reclass = rasters_reclass[order(years_lulc)]

# Define categories and labels
categories = c(1, 2, 3, 4, 5, 6)
labels = c("Forest", "Nonforest", "Wetlands", "Agriculture", "Water", "Urban")
obs = lulcc::ObsLulcRasterStack(x=rasters_reclass, 
                                pattern = "lu_",
                                categories=categories, 
                                labels=labels,
                                t=c(0,1,2,3,4,5,6,7,8,9,10,
                                    11,12,13,14,15,16,17,18,19,20,
                                    21,22,23,24,25,26,27,28,29,30,
                                    31,32,33,34,35))

### Transition matrix -------
lulcc::crossTabulate(x=obs, times=c(0,35))
