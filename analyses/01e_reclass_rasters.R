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
raster_files = list.files(base_path,
                           pattern = "\\.tif$",
                           full.names = TRUE)
rasters = lapply(raster_files, terra::rast)

## Vectors
roads = vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))

### Quick check -------
crs(rasters[[1]])
ext(rasters[[1]])
plot(rasters[[1]])  # plot the first layer
plot(roads, col="white", add=TRUE)


##### Exclusion of linear features ------


### Create forest patches ---------
#### Define forest as habitat -------
# Define groups
# NB: to see what codes refer to, check the "Codigos-da-legenda-colecao-9" file
forest = c(3, 4, 5, 6, 49)
notforest = c(11, 12, 32, 29, 50, 23)
agri = c(15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21)
water = c(26, 33, 31)
artificial = c(24, 30, 25)

# Function to reclassify a single raster
reclass_fun <- function(xx) {
  xx[xx == 0] <- NA
  xx[xx %in% forest]    <- 1
  xx[xx %in% notforest] <- 2
  xx[xx %in% agri]      <- 3
  xx[xx %in% water]     <- 4
  xx[xx %in% artificial] <- 5
  return(xx)
}

# Apply to all rasters
rasters_reclass <- lapply(seq_along(rasters), function(i) {
  message("Reclassifying SpatRaster ", i)
  app(rasters[[i]], fun = reclass_fun)
})

# Check
plot(rasters_reclass[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_reclass[[35]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))

# The following section is based on Mailys Queru's work
#### Dilatation-erosion --------
dilatation_erosion = function(raster, seuil) {
  # All cells different than 1 become NA
  habitat = app(raster, fun = function(v) ifelse(v == 1, 1, NA))
  
  # Distance to habitat
  dist_hab = terra::distance(habitat)
  
  # Threshold distance and set 0 to NA
  dist_hab_thresh = app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA))
  
  # Distance to non-habitat
  dist_nonhab = terra::distance(dist_hab_thresh)
  
  # Final threshold
  final_rast = dist_nonhab > seuil
  
  return(final_rast)
}

seuil = 100 # Here, we define the buffer width (dilatation length) (in meters)
raster_dilat = dilatation_erosion(rasters_reclass[[1]], seuil) # We apply the function
plot(raster_dilat, col=c("gray","darkgreen"))


#### Distinction between corridors and patches -----

###### Matrix vs forest --------
# Forest habitat
forest = raster_dilat
terra::values(forest) = as.integer(terra::values(forest) == 1)
forest[forest[[1]] != 1] = NA
plot(forest, main="Binary forest", col="darkgreen")

# Matrix habitat
matrix = is.na(forest)
terra::values(matrix) = as.integer(terra::values(matrix) == 1)
matrix[matrix[[1]] != 1] = NA
plot(matrix, main = "Matrix", col="grey")

###### Exclusion of core areas --------
dist2edges = terra::distance(matrix)
plot(dist2edges)
summary(values(dist2edges))
core = forest * (dist2edges > 100) # Threshold for distinguishing core and non-core areas
core = terra::ifel(core, 1, NA)
plot(core, main="Core forest area", col="darkgreen")

# Select non-core forest
non_core = forest
non_core = terra::mask(forest, core, inverse = TRUE)
plot(non_core, main="Non-core forest (forest cells not in core)", col="orange")


###### Exclusion of small patches -------
# Cell size in map units (m² if your CRS is projected)
core_patches = terra::patches(core, directions=8)
cs = terra::cellSize(core_patches, unit="m")

# Sum area for each patch
patch_area = terra::zonal(cs, core_patches, fun="sum", as.raster=TRUE)/10000 # in ha

# Classify core vs small-core (to be reallocated to non-core)
min_area = 10  # size threshold (in ha)
large_core = core_patches
large_core[patch_area < min_area] = NA   # remove small cores
plot(large_core, main="Large forest patches")
large_core_mask = !is.na(large_core)

# Reassign small patches as non-core
small_core = core
small_core = !large_core_mask & !is.na(core)  # TRUE for small core cells
# Add small cores into non-core
non_core_updated = non_core
non_core_updated[small_core] = 1  # 1 = non-core

# Map
landscape_class = matrix * 10            # matrix = 10
landscape_class[large_core_mask] = 1     # large core = 1
landscape_class[non_core_updated == 1] = 2  # non-core = 2

# Plot
plot(landscape_class, col=c("darkgreen","blue","gray"),
     legend=TRUE, main="Landscape classification")

###### Exclusion of patch edges 1/2 ---------
# Create mask of core cells
core_mask = landscape_class == 1

# Non core cells
non_core_cells = which(values(landscape_class) == 2)

# Assign value 1 (core) to non-core cells touching core
adj = terra::adjacent(landscape_class, cells=non_core_cells, directions=16, pairs=TRUE)

# Check which non-core cells have at least one neighbor that is core
core_cells = which(values(landscape_class) == 1)
touching_core = adj[adj[,2] %in% core_cells, 1]

# Assign value 1 to these non-core cells
values(landscape_class)[unique(touching_core)] <- 1

# Plot
cols = c("darkgreen", "red", "blue", "gray")  # core=1, corridor=2, non-core=3, matrix=10
plot(landscape_class, col=cols, legend=TRUE)

###### Exclusion of patch edges 2/2 ---------
# Create mask of core cells
core_mask = landscape_class == 1

# Non core cells
non_core_cells = which(values(landscape_class) == 2)

# Assign value 1 (core) to non-core cells touching core
adj = terra::adjacent(landscape_class, cells=non_core_cells, directions=16, pairs=TRUE)

# Check which non-core cells have at least one neighbor that is core
core_cells = which(values(landscape_class) == 1)
touching_core = adj[adj[,2] %in% core_cells, 1]

# Assign value 1 to these non-core cells
values(landscape_class)[unique(touching_core)] <- 1

# Plot
cols = c("darkgreen", "red", "blue", "gray")  # core=1, corridor=2, non-core=3, matrix=10
plot(landscape_class, col=cols, legend=TRUE)


##### Selection of potential corridors (connecting two patches) ----------
# Non core patches
non_core = terra::ifel(landscape_class == 2, 2, NA)
noncore_patches = terra::patches(non_core, directions = 8)

# Core patches
core = terra::ifel(landscape_class == 1, 1, NA)
core_patches = terra::patches(core, directions = 8)

# Build adjacency (which non-core touches which core)
adj = terra::adjacent(noncore_patches, cells = which(!is.na(values(noncore_patches))),
                directions = 8, pairs = TRUE)

# Get patch IDs from adjacency
core_ids = values(core_patches)[adj[,2]]
noncore_ids = values(noncore_patches)[adj[,1]]   # focal non-core IDs

# Create data frame of connections
connections = data.frame(noncore_id = noncore_ids,
                          core_id    = core_ids)
connections = na.omit(connections)

# Count distinct neighbors
counts = connections %>%
  dplyr::group_by(noncore_id) %>%
  dplyr::summarise(n_cores = n_distinct(core_id))

# Select only non-core patches touching ≥ 2 cores
valid_noncore_ids = counts$noncore_id[counts$n_cores >= 2]

# Mask valid non-core patches
corridors = noncore_patches
corridors[!values(noncore_patches) %in% valid_noncore_ids] = NA

# Plot results
cols = c("darkgreen", "blue", "gray")  # 1=core, 2=selected non-core, 10=matrix
plot(landscape_class, col=cols, legend=FALSE,
     main="Landscape with cores and corridors")
plot(corridors, col="red", add=TRUE, legend=FALSE)
legend("topright", legend=c("Core (1)", "Corridor (2+ cores)", "Non-core", "Matrix"),
       fill=c("darkgreen","red","blue","gray"))


### Final land use raster -----
# Original land use raster
original_raster = rasters_reclass[[1]]

# Create a copy to store the new classification
final_raster = original_raster

# Assign forest values (core + non-core)
final_raster[landscape_class %in% c(1, 2)] = 1

# Assign corridors
final_raster[!is.na(corridors)] = 6

# All other cells remain with their original values
# (water, agri, artificial, etc. are preserved from original_raster)

# Quick check
plot(rasters_reclass[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"), main="Landscape before patch and corridor identification")
plot(final_raster, col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "#FA6FFC"), main="Final landscape")



# ###### LCPs--------
# # LCP with gdistance
# final_r = raster::raster(landscape_class)  # RasterLayer
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
# patchyCost = raster::raster(final_rast)
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
# plot(final_rast,
#      col = c("darkgreen", "blue"),
#      legend = TRUE,
#      main = "Landscape with corridors")
# plot(LCPs, add=TRUE, col="red")




