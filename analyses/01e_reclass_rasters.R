#------------------------------------------------#
# Author: Romain Monassier
# Objective: Prepare rasters for landscape metrics analysis
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


### Define the parameters --------
edge_dist <- 100     # distance to consider 'core' from edge (m)
min_core_ha <- 10    # minimal core patch area (ha) to be kept as core


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



#### 4. Identify forest corridors -----
# Here, we distinguish between habitat patches and corridors, defined as narrow strips of vegetation connecting at least 2 habitat patches
# Criteria used are: distance of a forest cell to edge (if far = core area), patch size (if large = core area), adjacency to core cells (if adjacent = edges of core areas)
# -> Corridors are therefore forest cells close to the edge, small, and not habitat edges
# The last step is repeated twice (see below) because we define edges using a threshold distance > cell size
# -> Make sure to adapt the number of repetitions of the last step according to the edge distance threshold used

patch_corridor_analysis <- function(raster_input, edge_dist, min_core_ha) {
  
  # 1. Binary forest
  forest = raster_input
  terra::values(forest) = as.integer(terra::values(forest) == 1)
  forest[forest[[1]] != 1] = NA
  
  # 2. Matrix
  matrix = is.na(forest)
  terra::values(matrix) = as.integer(terra::values(matrix) == 1)
  matrix[matrix[[1]] != 1] = NA
  
  # 3. Exclude core forests (beyond edge threshold)
  dist2edges = terra::distance(matrix)
  core = forest * (dist2edges > edge_dist) # Edge_dist to specify
  core = terra::ifel(core, 1, NA)
  non_core = terra::mask(forest, core, inverse = TRUE)
  
  # 4. Exclude small patches
  core_patches = terra::patches(core, directions = 8)
  cs = terra::cellSize(core_patches, unit = "m") # Cell size in map units
  patch_area = terra::zonal(cs, core_patches, fun="sum", as.raster=TRUE)/10000 # area in ha
  
  large_core = core_patches
  large_core[patch_area < min_core_ha] = NA # Min_core_ha to specify
  large_core_mask = !is.na(large_core)
  
  small_core = core
  small_core = !large_core_mask & !is.na(core) # Reassign small patches as non-core
  non_core_updated = non_core
  non_core_updated[small_core] = 1 # Add small cores into non-core
  
  
  # 5. Landscape classification
  landscape_class = matrix * 10          # matrix = 10
  landscape_class[large_core_mask] = 1   # large core = 1
  landscape_class[non_core_updated == 1] = 2  # non-core = 2
  
  # 6. Assign edges touching core (repeat twice)
  # First time
  non_core_cells = which(values(landscape_class) == 2)
  adj = terra::adjacent(landscape_class, cells=non_core_cells, directions=16, pairs=TRUE)
  core_cells = which(values(landscape_class) == 1)
  touching_core = adj[adj[,2] %in% core_cells, 1]
  values(landscape_class)[unique(touching_core)] <- 1
  
  # Second time
  non_core_cells = which(values(landscape_class) == 2)
  adj = terra::adjacent(landscape_class, cells=non_core_cells, directions=16, pairs=TRUE)
  core_cells = which(values(landscape_class) == 1)
  touching_core = adj[adj[,2] %in% core_cells, 1]
  values(landscape_class)[unique(touching_core)] <- 1
  
  # Select potential corridors (non-core connecting ≥2 cores)
  non_core = terra::ifel(landscape_class == 2, 2, NA)
  noncore_patches = terra::patches(non_core, directions = 8) # Non core patches
  
  core = terra::ifel(landscape_class == 1, 1, NA)
  core_patches = terra::patches(core, directions = 8) # Core patches
  
  adj = terra::adjacent(noncore_patches, cells = which(!is.na(values(noncore_patches))),
                        directions = 8, pairs = TRUE) # Build adjacency (which non-core touches which core)
  
  core_ids = values(core_patches)[adj[,2]] # Get patch IDs from adjacency
  noncore_ids = values(noncore_patches)[adj[,1]] # non-core IDs
  
  connections = data.frame(noncore_id = noncore_ids, core_id = core_ids) # Create data frame of connections
  connections = na.omit(connections)
  
  counts = connections %>%
    dplyr::group_by(noncore_id) %>%
    dplyr::summarise(n_cores = n_distinct(core_id)) # Count distinct neighbors
  
  valid_noncore_ids = counts$noncore_id[counts$n_cores >= 2] # Select only non-core patches touching ≥ 2 cores
  corridors = noncore_patches
  corridors[!values(noncore_patches) %in% valid_noncore_ids] = NA # Mask valid non-core patches
  
  
  # ---- Step 10: Final numeric raster ----
  final_raster = raster_input
  final_raster[landscape_class %in% c(1,2)] = 1   # Forest
  final_raster[!is.na(corridors)] = 6             # Corridors
  # All other land-use codes remain intact
  
  return(final_raster)
}

#### 5. Apply linear features -----
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


##### Step 4 - Patch & corridor analysis -----

###### On an example ---------
# # Select one raster for testing
# example <- rasters_dilate[[1]]
# # Select a sample of the raster
# ext = terra::ext(example)
# # Shrink extent to 10% (take top-left corner)
# x_range = ext[2] - ext[1]
# y_range = ext[4] - ext[3]
# scale = sqrt(0.05)   # scale factor for each dimension
# small_ext = terra::ext(ext[1], ext[1] + scale*x_range, ext[3], ext[3] + scale*y_range)
# example_crop = terra::crop(example, small_ext)
# 
# ## 1. Wrap
# patch_corridor_example <- patch_corridor_analysis(example_crop, edge_dist, min_core_ha)
# # Plot with all classes (adding a bright color for corridors = 6)
# par(mfrow=c(1,2))
# plot(example_crop, main="Before corridor identification", col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
# plot(patch_corridor_example, main = "After corridor identification", col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))
# par(mfrow=c(1,1))
# 
# ## 2. Step by step
# 
# ### Matrix vs forest
# # Forest habitat
# forest = example_crop
# terra::values(forest) = as.integer(terra::values(forest) == 1)
# forest[forest[[1]] != 1] = NA
# plot(forest, main="Binary forest", col="darkgreen")
# 
# # Matrix habitat
# matrix = is.na(forest)
# terra::values(matrix) = as.integer(terra::values(matrix) == 1)
# matrix[matrix[[1]] != 1] = NA
# plot(matrix, main = "Matrix", col="grey")
# 
# ### Exclusion of core areas
# dist2edges = terra::distance(matrix)
# plot(dist2edges)
# core = forest * (dist2edges > 100) # Threshold for distinguishing core and non-core areas
# core = terra::ifel(core, 1, NA)
# plot(core, main="Core forest area", col="darkgreen")
# 
# # Select non-core forest
# non_core = forest
# non_core = terra::mask(forest, core, inverse = TRUE)
# plot(non_core, main="Non-core forest (forest cells not in core)", col="orange")
# 
# 
# ### Exclusion of small patches
# # Cell size in map units (m² if your CRS is projected)
# core_patches = terra::patches(core, directions=8)
# cs = terra::cellSize(core_patches, unit="m")
# 
# # Sum area for each patch
# patch_area = terra::zonal(cs, core_patches, fun="sum", as.raster=TRUE)/10000 # in ha
# 
# # Classify core vs small-core (to be reallocated to non-core)
# min_area = 10  # size threshold (in ha)
# large_core = core_patches
# large_core[patch_area < min_area] = NA   # remove small cores
# plot(large_core, main="Large forest patches")
# large_core_mask = !is.na(large_core)
# 
# # Reassign small patches as non-core
# small_core = core
# small_core = !large_core_mask & !is.na(core)  # TRUE for small core cells
# # Add small cores into non-core
# non_core_updated = non_core
# non_core_updated[small_core] = 1  # 1 = non-core
# 
# # Map
# landscape_class = matrix * 10            # matrix = 10
# landscape_class[large_core_mask] = 1     # large core = 1
# landscape_class[non_core_updated == 1] = 2  # non-core = 2
# 
# # Plot
# plot(landscape_class, col=c("darkgreen","blue","gray"),
#      legend=TRUE, main="Landscape classification")
# 
# ### Exclusion of patch edges 1/2
# # NB: we repeat this step twice to make sure edges are excluded
# # Create mask of core cells
# core_mask = landscape_class == 1
# 
# # Non core cells
# non_core_cells = which(values(landscape_class) == 2)
# 
# # Assign value 1 (core) to non-core cells touching core
# adj = terra::adjacent(landscape_class, cells=non_core_cells, directions=16, pairs=TRUE)
# 
# # Check which non-core cells have at least one neighbor that is core
# core_cells = which(values(landscape_class) == 1)
# touching_core = adj[adj[,2] %in% core_cells, 1]
# 
# # Assign value 1 to these non-core cells
# values(landscape_class)[unique(touching_core)] <- 1
# 
# # Plot
# cols = c("darkgreen", "red", "blue", "gray")  # core=1, corridor=2, non-core=3, matrix=10
# plot(landscape_class, col=cols, legend=TRUE)
# 
# ### Exclusion of patch edges 2/2
# # Create mask of core cells
# core_mask = landscape_class == 1
# 
# # Non core cells
# non_core_cells = which(values(landscape_class) == 2)
# 
# # Assign value 1 (core) to non-core cells touching core
# adj = terra::adjacent(landscape_class, cells=non_core_cells, directions=16, pairs=TRUE)
# 
# # Check which non-core cells have at least one neighbor that is core
# core_cells = which(values(landscape_class) == 1)
# touching_core = adj[adj[,2] %in% core_cells, 1]
# 
# # Assign value 1 to these non-core cells
# values(landscape_class)[unique(touching_core)] <- 1
# 
# # Plot
# cols = c("darkgreen", "red", "blue", "gray")  # core=1, corridor=2, non-core=3, matrix=10
# plot(landscape_class, col=cols, legend=TRUE)
# 
# 
# ### Selection of potential corridors (connecting two patches)
# # Non core patches
# non_core = terra::ifel(landscape_class == 2, 2, NA)
# noncore_patches = terra::patches(non_core, directions = 8)
# 
# # Core patches
# core = terra::ifel(landscape_class == 1, 1, NA)
# core_patches = terra::patches(core, directions = 8)
# 
# # Build adjacency (which non-core touches which core)
# adj = terra::adjacent(noncore_patches, cells = which(!is.na(values(noncore_patches))),
#                       directions = 8, pairs = TRUE)
# 
# # Get patch IDs from adjacency
# core_ids = values(core_patches)[adj[,2]]
# noncore_ids = values(noncore_patches)[adj[,1]]   # focal non-core IDs
# 
# # Create data frame of connections
# connections = data.frame(noncore_id = noncore_ids,
#                          core_id    = core_ids)
# connections = na.omit(connections)
# 
# # Count distinct neighbors
# counts = connections %>%
#   dplyr::group_by(noncore_id) %>%
#   dplyr::summarise(n_cores = n_distinct(core_id))
# 
# # Select only non-core patches touching ≥ 2 cores
# valid_noncore_ids = counts$noncore_id[counts$n_cores >= 2]
# 
# # Mask valid non-core patches
# corridors = noncore_patches
# corridors[!values(noncore_patches) %in% valid_noncore_ids] = NA
# 
# # Plot results
# cols = c("darkgreen", "blue", "gray")  # 1=core, 2=selected non-core, 10=matrix
# plot(landscape_class, col=cols, legend=FALSE,
#      main="Landscape with cores and corridors")
# plot(corridors, col="red", add=TRUE, legend=FALSE)
# legend("topright", legend=c("Core (1)", "Corridor (2+ cores)", "Non-core", "Matrix"),
#        fill=c("darkgreen","red","blue","gray"))
# 
# 
# ### Final land use raster
# # Original land use raster
# final_raster = example_crop
# 
# # Assign forest values (core + non-core)
# final_raster[landscape_class %in% c(1, 2)] = 1
# 
# # Assign corridors
# final_raster[!is.na(corridors)] = 6
# 
# # All other cells remain with their original values
# # (water, agri, artificial, etc. are preserved from original_raster)
# 
# # Quick check
# plot(final_raster, col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "#FA6FFC"), main="Final landscape")
# 
# 

###### Wrap -----
message("Step 4: Identifying forest patches and corridors...")
rasters_patches <- lapply(seq_along(rasters_dilate), function(i) {
  message("Running patch–corridor analysis for raster ", i)
  patch_corridor_analysis(rasters_dilate[[i]], edge_dist, min_core_ha) # edge_dist and min_core_ha must be defined
})

# Check
par(mfrow=c(1,2))
plot(rasters_dilate[[1]], main="Before corridor identification", col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_patches[[1]], main = "After corridor identification", col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))
par(mfrow=c(1,1))

#### Step 5 – Apply linear features on top of raster layers -----
message("Step 5: Applying linear features...")
rasters_final <- map2(rasters_patches, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin <- r
  r_lin <- apply_linear_feature_single(r_lin, yr, roads, buffer_width = 15, value = 5, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 15, value = 2, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, bridges, buffer_width = 15, value = 6, use_date = TRUE)
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

# Land-cover colors
lc_colors <- c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse")

# Loop over selected rasters
for (i in subset_indices) {
  # Crop rasters
  r_before <- crop(rasters_patches[[i]], zoom_ext)
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
       col = lc_colors)
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  plot(r_after,
       main = paste0("After linear features (", years[i], ")"),
       col = lc_colors)
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



# ###### LCPs--------
# # LCP with gdistance
# final_r = raster::raster(rasters_final[[1]])  # RasterLayer
# 
# # Define cost surfaces
# cost_r = final_r
# values(cost_r)[values(cost_r) > 2] = 10
# 
# # Build transition object (8 directions)
# tr = transition(cost_r, function(x) 1/mean(x), directions=8)
# tr = geoCorrection(tr, type="c")
# 
# # Extract centroids of connected patches
# large_core_poly = terra::as.polygons(large_core)
# centroids = terra::centroids(large_core_poly)
# coords = terra::crds(centroids)  # matrix of XY coordinates
# plot(cost_r)
# plot(centroids, add=TRUE)
# 
# # Initialize a list to store paths
# paths <- list()
# 
# # Loop over all pairs of cores
# n = nrow(coords)
# for(i in 1:(n-1)) {
#   for(j in (i+1):n) {
#     # Compute least-cost path between core i and core j
#     path = shortestPath(tr, coords[i,], coords[j,], output="SpatialLines")
#     
#     # Store the path in the list
#     paths[[paste(i, j, sep="-")]] <- path
#   }
# }
# 
# # Plot landscape + shortest paths
# plot(final_r, col=c("darkgreen","gray"), legend=FALSE, main="LCPs")
# plot(large_core_poly, col="green", add=TRUE)
# for(p in paths) plot(p, col="red", lwd=2, add=TRUE)

# # LCP with grainscape
# patchyCost = raster::raster(rasters_final[[1]])
# crs(patchyCost) = CRS("+init=EPSG:31983")
# crs(patchyCost)
# patchyCost_df = grainscape::ggGS(patchyCost)
# patchyCost_df$value = as.factor(patchyCost_df$value)
# ggplot() +
#   ggplot2::geom_raster(
#     data = patchyCost_df,
#     aes(x = x, y = y, fill = value)
#   ) +
#   ggplot2::scale_fill_brewer(
#     type = "qual", palette = "Paired",
#     direction = -1, guide = "legend"
#   ) +
#   guides(fill = guide_legend(title = "Resistance")) +
#   theme_grainscape() +
#   theme(legend.position = "right")
# 
# # Create the minimum planar graph
# patchyMPG = grainscape::MPG(patchyCost, patch = (patchyCost == 1))
# plot(patchyMPG, quick = "mpgPlot", theme = FALSE)
# 
# # Extract all pairwise least cost paths
# LCPs = patchyMPG@lcpLinkId
# 
# # Plot
# plot(final_r,
#      col = c("darkgreen", "blue"),
#      legend = TRUE,
#      main = "Landscape with corridors")
# plot(LCPs, add=TRUE, col="red")




