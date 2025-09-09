#------------------------------------------------#
# Author: Romain Monassier
# Objective: Crop and reproject rasters with a mask
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(landscapemetrics)
library(terra)

### Import data -------
## Rasters
base_path <- here("data", "geo", "MapBiomas", "colecao_9") 
raster_files <- list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
rasters <- lapply(raster_files, terra::rast)
# Shapefile
base_path <- here("data", "geo", "IBGE", "admin", "BR_UF_2023")
region = vect(file.path(base_path, "RJ_state.shp"))

### Quick check -------
crs(rasters[[1]])
crs(region)
ext(rasters[[1]])
ext(region)
plot(rasters[[1]])  # plot the first layer
plot(region)

### Crop rasters ------

## Reproject shapefile to match raster CRS
region <- project(region, crs(rasters[[1]]))

## Quick check
plot(rasters[[1]])  
plot(region, add = TRUE)

## Using the RJ state boundaries as a layer
rasters_cropped <- lapply(rasters, function(r) {
  r_cropped <- crop(r, region) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked <- mask(r_cropped, region) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})

# Check one
plot(rasters_cropped[[1]])
plot(region, add = TRUE, border = "red")

### Reproject --------
# Target CRS
target_crs <- "EPSG:31983"

rasters_reprojected <- lapply(rasters_cropped, function(r) {
  # Use method = "near" for categorical data (like MapBiomas land cover)
  project(r, target_crs, method = "near")
})

# Quick check
crs(rasters_reprojected[[1]])
plot(rasters_reprojected[[1]])

# Export rasters
# Define output folder
output_dir <- here("outputs", "data", "MapBiomas")

# Export each raster
for (i in seq_along(rasters_reprojected)) {
  r <- rasters_reprojected[[i]]
  
  # Construct a filename using the original raster name
  file_name <- paste0(names(r)[1], "_RJ_state_31983.tif")
  output_path <- file.path(output_dir, file_name)
  
  # Write raster to file
  writeRaster(r, output_path, overwrite = TRUE)
}
