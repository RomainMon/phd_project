#------------------------------------------------#
# Author: Romain Monassier
# Objective: Reclass rasters using land cover data and linear features
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(terra)
library(purrr)
library(stringr)
library(ggplot2)
library(raster)
library(sf)

### Import data -------
## Rasters
base_path = here("outputs", "data", "MapBiomas", "Mask_sampling_bbox")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)
rasters = lapply(raster_files, terra::rast)
years = as.numeric(gsub("\\D", "", basename(raster_files))) # Extract raster years using their file names (i.e., they have to be named as following: raster_YYYY.tif)
stopifnot(length(rasters) == length(years))

## Vectors
roads = vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))
power_lines = vect(here("data", "geo", "OSM", "work", "Power_line_OSM_clean.shp"))
pipelines = vect(here("data", "geo", "OSM", "work", "Pipelines_OSM_clean.shp"))
bridges = vect(here("data", "geo", "AMLD", "Passagens_ARTERIS", "work", "road_overpasses_clean.shp"))
plantios = vect(here("data", "geo", "AMLD", "plantios", "work", "plantios_clean.shp"))


### Quick check -------
crs(rasters[[1]])
crs(roads)
crs(power_lines)
crs(pipelines)
crs(bridges)
crs(plantios)
ext(rasters[[1]])
plot(rasters[[1]])  # plot the first layer
plot(roads, col="white", add=TRUE)


### Functions ---------

#### 1. Reclass rasters ------
# Here, we reclass MapBiomas rasters (colecao 9) using new categories 
# NB: to see what codes refer to, check the "Codigos-da-legenda-colecao-9" file
reclass_fun <- function(xx) {
  forest <- c(3, 4, 5, 6, 49)
  notforest <- c(11, 12, 32, 29, 50, 23)
  agri <- c(15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21)
  water <- c(26, 33, 31)
  artificial <- c(24, 30, 25)
  
  xx[xx == 0] <- NA
  xx[xx %in% forest] <- 1
  xx[xx %in% notforest] <- 2
  xx[xx %in% agri] <- 3
  xx[xx %in% water] <- 4
  xx[xx %in% artificial] <- 5
  xx
}


#### 2. Overlay plantios on rasters depending on the reforestation year -----
# Here, we overlay the vector layer "plantios" (reforested areas) on the rasters, and give these reforested areas the value 1 (forest)
# The overlay is dependent on the condition "date_refor" (which corresponds to the year where the forest had fully grown)
# -> Make sure that the datasets has a variable named "date_refor" with numeric years
add_plantios <- function(raster, year, plantios) {
  # Keep only plantios with a valid reforestation year before or equal to current year
  plantios_valid <- plantios[!is.na(plantios$date_refor) & plantios$date_refor <= year, ]
  if (nrow(plantios_valid) > 0) {
    # Rasterize plantios: forest = 1 inside polygons, NA elsewhere
    pl_rast <- terra::rasterize(plantios_valid, raster, field = 1, background = NA)
    # Only overwrite cells where pl_rast is not NA (i.e., inside plantios)
    raster[!is.na(pl_rast)] <- 1
  }
  raster
}


#### 3. Dilatation-erosion --------
# Here, we apply dilatation-erosion on habitats (value = 1)
# This section is based on Mailys Queru's work

# dilatation_erosion_mailys <- function(raster, seuil) {
#   habitat <- app(raster, fun = function(v) ifelse(v == 1, 1, NA)) # All cells different than 1 become NA
#   dist_hab <- terra::distance(habitat)
#   dist_hab_thresh <- app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA)) # Threshold distance and set 0 to NA 
#   dist_nonhab <- terra::distance(dist_hab_thresh) 
#   dist_nonhab > seuil 
#   }

# Numeric version (applies mask to original raster)
dilatation_erosion <- function(raster, seuil) {
  # Step 1: Habitat mask
  habitat <- app(raster, fun = function(v) ifelse(v == 1, 1, NA))
  
  # Step 2: Dilation
  dist_hab <- terra::distance(habitat)
  dilated_mask <- app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA))
  
  # Step 3: Erosion
  dist_nonhab <- terra::distance(dilated_mask)
  final_mask <- app(dist_nonhab, fun = function(v) ifelse(v > seuil, 1, NA))
  
  # Step 4: Apply mask to original raster
  raster[!is.na(final_mask)] <- 1
  raster
}


#### 4. Apply linear features -----
# Here, we overlay vector linear features to the rasters using a buffer width and assign the intersected cells a new numeric value
# -> Adapt the buffer width and value according to the linear feature (for instance: roads = 15m and 5 for artificial)
apply_linear_feature_single <- function(r, yr, feature, buffer_width, value, use_date = TRUE) {
  feat_valid <- if (use_date && "date_crea" %in% names(feature)) {
    feature[feature$date_crea <= yr, ]
  } else feature
  
  if (nrow(feat_valid) > 0) {
    feat_buff <- terra::buffer(feat_valid, width = buffer_width)
    feat_rast <- terra::rasterize(feat_buff, r, field = value, background = NA)
    r <- cover(feat_rast, r)
  }
  
  return(r)  # single SpatRaster
}


### Processing pipeline --------
# Reclassify → Add plantios → Dilatation–Erosion → Patch–Corridor → Linear features

message("Running full pipeline...")

##### Step 1 – Reclassify ---------
message("Step 1: Reclassifying rasters...")
rasters_reclass <- lapply(seq_along(rasters), function(i) {
  message("  - Reclassifying raster ", i, " (year ", years[i], ")")
  app(rasters[[i]], reclass_fun)
})

plot(rasters_reclass[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))

##### Step 2 – Add plantios ------
message("Step 2: Adding plantios to rasters...")
rasters_plantios <- map2(rasters_reclass, years, function(r, yr) {
  message("  - Adding plantios for year ", yr)
  add_plantios(r, yr, plantios)
})

# Quick check for plantios overlay
year_sel <- 2004  # choose year to inspect
buff <- 1000
pl <- plantios[plantios$date_refor == year_sel, ]

if (nrow(pl) > 0) {
  years_to_plot <- c(year_sel - 1, year_sel, year_sel + 1)
  idx <- match(years_to_plot, years)
  valid <- !is.na(idx)
  years_to_plot <- years_to_plot[valid]
  idx <- idx[valid]
  ext_zoom <- ext(pl) + buff
  par(mfrow=c(1, length(idx)))
  for (j in seq_along(idx)) {
    yr <- years_to_plot[j]
    r_zoom <- crop(rasters_plantios[[idx[j]]], ext_zoom)
    plot(r_zoom, col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"), main=as.character(yr))
    plot(pl, border="black", lwd=2, add=TRUE)
  }
  par(mfrow=c(1,1))
} else {
  message("No plantios with date_refor = ", year_sel)
}

##### Step 3 – Dilatation-erosion ------
message("Step 3: Applying dilatation–erosion...")
rasters_dilate <- lapply(seq_along(rasters_plantios), function(i) {
  message("  - Processing dilatation–erosion for raster ", i, " (year ", years[i], ")")
  dilatation_erosion(rasters_plantios[[i]], seuil = 50)
})
# Quick check
plot(rasters_plantios[[1]], main=paste0("Before dilatation–Erosion ", years[1]), col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_dilate[[1]], main=paste0("After dilatation–Erosion ", years[1]), col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))


#### Step 4 – Apply linear features on top of raster layers -----
message("Step 4: Applying linear features...")
rasters_final <- map2(rasters_dilate, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin <- r
  r_lin <- apply_linear_feature_single(r_lin, yr, roads, buffer_width = 20, value = 5, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 15, value = 2, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, bridges, buffer_width = 15, value = 1, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, power_lines, buffer_width = 50, value = 2, use_date = FALSE)
  r_lin
})

# Check
# Point coordinates
x_center <- 782552.6
y_center <- 7510976.3

# Buffer size
buffer_size <- 1500

# Create bounding box extent
zoom_ext <- ext(
  x_center - buffer_size, x_center + buffer_size,
  y_center - buffer_size, y_center + buffer_size
)

# Raster indices to visualize
subset_indices <- c(1, 17, 35)

# Loop over selected rasters
for (i in subset_indices) {
  # Crop rasters
  r_before <- crop(rasters_dilate[[i]], zoom_ext)
  r_after  <- crop(rasters_final[[i]], zoom_ext)
  
  # Crop linear features
  roads_sub       <- crop(roads, zoom_ext)
  power_lines_sub <- crop(power_lines, zoom_ext)
  bridges_sub     <- crop(bridges, zoom_ext)
  pipelines_sub   <- crop(pipelines, zoom_ext)
  
  # Plot side by side
  par(mfrow=c(1,2))
  
  plot(r_before,
       main = paste0("Before linear features (", years[i], ")"),
       col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  plot(r_after,
       main = paste0("After linear features (", years[i], ")"),
       col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  par(mfrow=c(1,1))
}

### Export rasters ---------
message("Step 6: Exporting rasters...")

# Define output folder
output_dir <- here("outputs", "data", "MapBiomas", "Rasters_reclass")

# Export each raster with year in the filename
for (i in seq_along(rasters_final)) {
  year_i <- years[i]
  output_path <- file.path(output_dir, paste0("raster_reclass_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_final[[i]],
    filename = output_path,
    overwrite = TRUE, # Overwrite existing files or not
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}





