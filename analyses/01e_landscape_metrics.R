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
library(purrr)

### Import data -------
## Rasters
base_path <- here("outputs", "data", "MapBiomas")
raster_files <- list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
rasters <- lapply(raster_files, terra::rast)

### Quick check -------
crs(rasters[[1]])
ext(rasters[[1]])
plot(rasters[[1]])  # plot the first layer

### Compute landscape metrics ---------
# NB: All functions in landscapemetrics start with lsm_ (for landscape metrics). The second part of the name specifies the level (patch - p, class - c or landscape - l)

## Summary statistics at the landscape scale
# Extract years from filenames
years <- stringr::str_extract(basename(raster_files), "\\d{4}")

# Compute class area (ha) per raster
areas_list <- map2(rasters, years, function(r, y) {
  # class-level metric: total area of each class
  df <- lsm_c_ca(r)
  df$year <- y
  df
})

# Combine into one data.frame
areas_df = bind_rows(areas_list)

## Summary statistics at the patch level
# NB: landscapemetrics works on categorical rasters where classes are integers. Therefore, we need to convert the raster to a categorical raster
# We must define "patches": we focus on value = 3 ("Forest formation" in the 9th MapBiomas collection)