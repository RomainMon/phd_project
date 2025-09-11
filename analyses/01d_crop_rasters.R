#------------------------------------------------#
# Author: Romain Monassier
# Objective: Crop and reproject rasters with a mask
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(terra)
library(sf)

### Import data -------
## Rasters
base_path <- here("data", "geo", "MapBiomas", "colecao_9") 
raster_files <- list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
rasters <- lapply(raster_files, terra::rast)
## Shapefiles
# Import the region shapefile 
base_path <- here("data", "geo", "IBGE", "admin", "BR_UF_2023")
region = vect(file.path(base_path, "RJ_state.shp"))
# Import the Sao Joao Watershed basin shapefile
base_path <- here("data", "geo", "AMLD", "hidrografia") 
watershed = vect(file.path(base_path, "Rio_Sao_Joao_Watershed.shp"))
# Import the sampling locations ("RegionsName") shapefile
base_path = here("data", "geo", "APonchon", "GLT")
sampling_units = vect(file.path(base_path, "RegionsName.shp"))


### Quick check -------
crs(rasters[[1]])
ext(rasters[[1]])
plot(rasters[[1]])  # plot the first layer
crs(region)
ext(region)
plot(region)
crs(watershed)
ext(watershed)
plot(watershed)
crs(sampling_units)
ext(sampling_units)
plot(sampling_units)

### Crop rasters ------

#### With the region ------
# Reproject shapefile to match raster CRS
region <- terra::project(region, crs(rasters[[1]]))

# Quick check
plot(rasters[[1]])  
plot(region, add = TRUE)

# Crop (just replace the name of the object in the crop and mask functions)
rasters_cropped <- lapply(rasters, function(r) {
  r_cropped <- crop(r, region) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked <- mask(r_cropped, region) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})

# Check one
plot(rasters_cropped[[1]])
plot(region, add = TRUE, border = "red")

#### With the watershed ------
# Reproject shapefile to match raster CRS
watershed <- terra::project(watershed, crs(rasters[[1]]))

# Quick check
crs(watershed)

# Crop (just replace the name of the object in the crop and mask functions)
rasters_cropped <- lapply(rasters, function(r) {
  r_cropped <- crop(r, watershed) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked <- mask(r_cropped, watershed) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})

# Check one
plot(rasters_cropped[[1]])
plot(watershed, add = TRUE, border = "red")

#### With the sampling units layer ------
## First, we need to create a bbox polygon from sampling units that can serve as a landscape mask
# We reproject the sampling units layer into a CRS in meters
target_crs <- "EPSG:31983"
sampling_units = terra::project(sampling_units, target_crs)
crs(sampling_units)

# We extract the bounding box of the location units
sampling_bbox = terra::ext(sampling_units)
plot(sampling_bbox)
plot(sampling_units, add=TRUE)

# We add 10 km to the bounding box because the sampling units might be located near the edges
# 10 km corresponds to the maximum dispersal distance recorded for GLTs (Moraes et al. 2018)
bbox_expanded = terra::extend(sampling_bbox, 10000)

# We convert the bbox to SpatVector
bbox_expanded <- terra::as.polygons(bbox_expanded, crs=target_crs)
crs(bbox_expanded)
plot(bbox_expanded)
plot(sampling_units, add=TRUE)

## Crop
# Reproject shapefile to match raster CRS
bbox_expanded_4326 <- terra::project(bbox_expanded, crs(rasters[[1]]))

# Quick check
plot(rasters[[1]])  
plot(bbox_expanded_4326, add = TRUE, border="white")

# Crop (just replace the name of the object in the crop and mask functions)
rasters_cropped <- lapply(rasters, function(r) {
  r_cropped <- crop(r, bbox_expanded_4326) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked <- mask(r_cropped, bbox_expanded_4326) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})

# Check one
plot(rasters_cropped[[1]])
plot(bbox_expanded_4326, add = TRUE, border = "red")

### Reproject cropped rasters --------
# Target CRS
target_crs <- "EPSG:31983"

rasters_reprojected <- lapply(rasters_cropped, function(r) {
  # Use method = "near" for categorical data (like MapBiomas land cover)
  terra::project(r, target_crs, method = "near")
})

# Quick check
crs(rasters_reprojected[[1]])
plot(rasters_reprojected[[1]])
plot(sampling_units, add=TRUE)

### Export rasters --------
# Define output folder
output_dir <- here("outputs", "data", "MapBiomas", "Mask_sampling_bbox")

# Export each raster
for (i in seq_along(rasters_reprojected)) {
  r <- rasters_reprojected[[i]]
  
  # Construct a filename using the original raster name
  file_name <- paste0(names(r)[1], "_sampling_bbox_31983.tif")
  output_path <- file.path(output_dir, file_name)
  
  # Write raster to file
  writeRaster(r, output_path, overwrite = TRUE)
}

### Export shapefiles -----
output_dir <- here("outputs", "data", "MapBiomas", "Mask_sampling_bbox")
terra::writeVector(
  bbox_expanded, 
  filename = file.path(output_dir, "sampling_units_bbox_31983.shp"), 
  filetype = "ESRI Shapefile", 
  overwrite = TRUE
)
