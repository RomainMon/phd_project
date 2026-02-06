#------------------------------------------------#
# Author: Romain Monassier
# Objective: Crop WorldClim data to study area
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(here)
library(terra)
library(sf)

### Import data -------

## Rasters
# Precipitations
base_path = here("data", "geo", "WorldClim", "raw", "prec") 
prec_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
prec = lapply(prec_files, terra::rast)

# Min temperature
base_path = here("data", "geo", "WorldClim", "raw", "tmin") 
tmin_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
tmin = lapply(tmin_files, terra::rast)

# Max temperature
base_path = here("data", "geo", "WorldClim", "raw", "tmax") 
tmax_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
tmax = lapply(tmax_files, terra::rast)

## Shapefiles
# Import the study area shapefile
base_path = here("data", "geo", "BBOX")
bbox = vect(file.path(base_path, "sampling_units_bbox_31983.shp"))


### Quick check -------
crs(prec[[1]])
ext(prec[[1]])
plot(prec[[1]])  # plot the first layer
crs(bbox)
ext(bbox)

### Crop rasters ------

# Reproject shapefile to match raster CRS
bbox = terra::project(bbox, crs(prec[[1]]))

# Quick check
plot(prec[[1]])  
plot(bbox, add = TRUE, col="yellow")

# Crop (just replace the name of the object in the crop and mask functions)
# Precipitations
prec_cropped = lapply(prec, function(r) {
  r_cropped = crop(r, bbox) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked = mask(r_cropped, bbox) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})
# Tmin
tmin_cropped = lapply(tmin, function(r) {
  r_cropped = crop(r, bbox) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked = mask(r_cropped, bbox) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})
# Tmax
tmax_cropped = lapply(tmax, function(r) {
  r_cropped = crop(r, bbox) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked = mask(r_cropped, bbox) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})

# Check one
plot(prec_cropped[[1]])
plot(tmin_cropped[[1]])
plot(tmax_cropped[[1]])
plot(bbox, add = TRUE, border = "red")

### Reproject cropped rasters --------
# Target CRS
target_crs = "EPSG:31983"

# Precipitations
prec_reprojected = lapply(prec_cropped, function(r) {
  # Use method = "near" for categorical data (like MapBiomas land cover)
  terra::project(r, target_crs, method = "near")
})
# Tmin
tmin_reprojected = lapply(tmin_cropped, function(r) {
  # Use method = "near" for categorical data (like MapBiomas land cover)
  terra::project(r, target_crs, method = "near")
})
# Precipitations
tmax_reprojected = lapply(tmax_cropped, function(r) {
  # Use method = "near" for categorical data (like MapBiomas land cover)
  terra::project(r, target_crs, method = "near")
})

# Quick check
crs(prec_reprojected[[1]])
crs(tmin_reprojected[[1]])
crs(tmax_reprojected[[1]])
plot(prec_reprojected[[1]])

### Export rasters --------
# Get year and month from file names
get_year_month = function(x) {
  sub(".*_(\\d{4}-\\d{2})$", "\\1", tools::file_path_sans_ext(basename(x)))
}
get_year_month("wc2.1_cruts4.09_2.5m_prec_1989-01.tif")

## Precipitations
# Define output folder
output_dir = here("data", "geo", "WorldClim", "work", "prec")

for (i in seq_along(prec_reprojected)) {
  
  r = prec_reprojected[[i]]
  ym = get_year_month(prec_files[i])
  
  file_name = paste0("prec_", ym, "_bbox_31983.tif")
  writeRaster(
    r,
    file.path(output_dir, file_name),
    overwrite = TRUE
  )
}

## Tmin
# Define output folder
output_dir = here("data", "geo", "WorldClim", "work", "tmin")

for (i in seq_along(tmin_reprojected)) {
  
  r = tmin_reprojected[[i]]
  ym = get_year_month(tmin_files[i])
  
  file_name = paste0("tmin_", ym, "_bbox_31983.tif")
  writeRaster(
    r,
    file.path(output_dir, file_name),
    overwrite = TRUE
  )
}

## Tmax
# Define output folder
output_dir = here("data", "geo", "WorldClim", "work", "tmax")

for (i in seq_along(tmax_reprojected)) {
  
  r = tmax_reprojected[[i]]
  ym = get_year_month(tmax_files[i])
  
  file_name = paste0("tmax_", ym, "_bbox_31983.tif")
  writeRaster(
    r,
    file.path(output_dir, file_name),
    overwrite = TRUE
  )
}
