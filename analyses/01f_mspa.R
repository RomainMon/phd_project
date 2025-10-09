#------------------------------------------------#
# Author: Romain Monassier
# Objective: Identify forest patches and corridors through morphological analysis (MSPA)
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
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)
rasters = lapply(raster_files, terra::rast)
years = as.numeric(gsub("\\D", "", basename(raster_files))) # Extract raster years using their file names (i.e., they have to be named as following: raster_YYYY.tif)


### MSPA with GuidosToolBox --------

#### Reclass for MSPA ----
# -> GTB requires a 8-byte type formatted input mask (.tif) with 0 = missing data, 1 = background, 2 = habitat
reclass_for_mspa <- function(xx) {
  habitat <- 1
  background <- c(2, 3, 4, 5)
  
  # Make a copy of original values
  v <- xx[]
  
  # Initialize all as 0 (missing data)
  xx[] <- 0
  
  # Assign habitat first using original values
  xx[v == habitat] <- 2
  
  # Then assign background
  xx[v %in% background] <- 1
  
  return(xx)
}

rasters_for_mspa <- lapply(rasters, reclass_for_mspa)

#### Export all reclassed rasters ----
# Define output folder
output_dir <- here("outputs", "data", "MapBiomas", "MSPA")

# Export loop
for(i in seq_along(rasters_for_mspa)) {
  year <- years[i]
  r_mspa <- rasters_for_mspa[[i]]
  
  out_file <- file.path(output_dir, paste0("raster_for_mspa_", year, ".tif"))
  
  terra::writeRaster(
    r_mspa,
    filename = out_file,
    overwrite = TRUE,
    wopt = list(
      datatype = "INT1U",     # 8-bit unsigned integer required by GTB
      gdal = c("COMPRESS=LZW") # optional compression
    )
  )
  
  message("Saved raster: ", out_file)
}


#### Run MSPA -----
# Open GLT > File > Batch Process > PAttern > Morphological > MSPA
# Default parameters are connectivity = 8 (or 4), edge width = 1, transition = on (i.e., bridges connect core areas, if 0 they connect edges), intext = on (distinguished internal from external features)
# The foreground area of a binary image is divided into seven visually distinguished MSPA classes: Core, Islet, Perforation, Edge, Loop, Bridge, and Branch (23 classes in the output raster)

#### Reclass MSPA rasters -----
##### Rename and import GTB outputs -----
# Path to your files
output_dir <- here("outputs", "data", "MapBiomas", "MSPA", "batch_MSPA")

# List all raster files (assuming .tif)
raster_files <- list.files(output_dir, pattern = "\\.tif$", full.names = TRUE)

# Rename files by removing "for_" and "_8_1_1_1"
new_names <- gsub("for_", "", raster_files)
new_names <- gsub("_8_1_0_0", "", new_names)

# Actually rename the files
file.rename(raster_files, new_names)

# Import rasters
rasters_mspa <- lapply(new_names, terra::rast)
unique(values(rasters_mspa[[1]]))
plot(rasters_mspa[[1]])

##### Reclass --------


### Own version (imperfect) -----
# # Here, we distinguish between habitat patches and corridors, defined as narrow strips of vegetation connecting at least 2 habitat patches
# # Criteria used are: distance of a forest cell to edge (if far = core area), patch size (if large = core area), adjacency to core cells (if adjacent = edges of core areas)
# # -> Corridors are therefore forest cells close to the edge, small, and not habitat edges
# # The last step is repeated twice (see below) because we define edges using a threshold distance > cell size
# # -> Make sure to adapt the number of repetitions of the last step according to the edge distance threshold used
# 
# ### Define the parameters
# edge_dist <- 100     # distance to consider 'core' from edge (m)
# min_core_ha <- 10    # minimal core patch area (ha) to be kept as core
# 
# ##### Function -----
# patch_corridor_analysis <- function(raster_input, edge_dist, min_core_ha) {
#   
#   # 1. Binary forest
#   forest = raster_input
#   terra::values(forest) = as.integer(terra::values(forest) == 1)
#   forest[forest[[1]] != 1] = NA
#   
#   # 2. Matrix
#   matrix = is.na(forest)
#   terra::values(matrix) = as.integer(terra::values(matrix) == 1)
#   matrix[matrix[[1]] != 1] = NA
#   
#   # 3. Exclude core forests (beyond edge threshold)
#   dist2edges = terra::distance(matrix)
#   core = forest * (dist2edges > edge_dist) # Edge_dist to specify
#   core = terra::ifel(core, 1, NA)
#   non_core = terra::mask(forest, core, inverse = TRUE)
#   
#   # 4. Exclude small patches
#   core_patches = terra::patches(core, directions = 8)
#   cs = terra::cellSize(core_patches, unit = "m") # Cell size in map units
#   patch_area = terra::zonal(cs, core_patches, fun="sum", as.raster=TRUE)/10000 # area in ha
#   
#   large_core = core_patches
#   large_core[patch_area < min_core_ha] = NA # Min_core_ha to specify
#   large_core_mask = !is.na(large_core)
#   
#   small_core = core
#   small_core = !large_core_mask & !is.na(core) # Reassign small patches as non-core
#   non_core_updated = non_core
#   non_core_updated[small_core] = 1 # Add small cores into non-core
#   
#   
#   # 5. Landscape classification
#   landscape_class = matrix * 10          # matrix = 10
#   landscape_class[large_core_mask] = 1   # large core = 1
#   landscape_class[non_core_updated == 1] = 2  # non-core = 2
#   
#   # 6. Assign edges touching core (repeat twice)
#   # First time
#   non_core_cells = which(values(landscape_class) == 2)
#   adj = terra::adjacent(landscape_class, cells=non_core_cells, directions=16, pairs=TRUE)
#   core_cells = which(values(landscape_class) == 1)
#   touching_core = adj[adj[,2] %in% core_cells, 1]
#   values(landscape_class)[unique(touching_core)] <- 1
#   
#   # Second time
#   non_core_cells = which(values(landscape_class) == 2)
#   adj = terra::adjacent(landscape_class, cells=non_core_cells, directions=16, pairs=TRUE)
#   core_cells = which(values(landscape_class) == 1)
#   touching_core = adj[adj[,2] %in% core_cells, 1]
#   values(landscape_class)[unique(touching_core)] <- 1
#   
#   # Select potential corridors (non-core connecting ≥2 cores)
#   non_core = terra::ifel(landscape_class == 2, 2, NA)
#   noncore_patches = terra::patches(non_core, directions = 8) # Non core patches
#   
#   core = terra::ifel(landscape_class == 1, 1, NA)
#   core_patches = terra::patches(core, directions = 8) # Core patches
#   
#   adj = terra::adjacent(noncore_patches, cells = which(!is.na(values(noncore_patches))),
#                         directions = 8, pairs = TRUE) # Build adjacency (which non-core touches which core)
#   
#   core_ids = values(core_patches)[adj[,2]] # Get patch IDs from adjacency
#   noncore_ids = values(noncore_patches)[adj[,1]] # non-core IDs
#   
#   connections = data.frame(noncore_id = noncore_ids, core_id = core_ids) # Create data frame of connections
#   connections = na.omit(connections)
#   
#   counts = connections %>%
#     dplyr::group_by(noncore_id) %>%
#     dplyr::summarise(n_cores = n_distinct(core_id)) # Count distinct neighbors
#   
#   valid_noncore_ids = counts$noncore_id[counts$n_cores >= 2] # Select only non-core patches touching ≥ 2 cores
#   corridors = noncore_patches
#   corridors[!values(noncore_patches) %in% valid_noncore_ids] = NA # Mask valid non-core patches
#   
#   
#   # Final numeric raster
#   final_raster = raster_input
#   final_raster[landscape_class %in% c(1,2)] = 1   # Forest
#   final_raster[!is.na(corridors)] = 6             # Corridors
#   # All other land-use codes remain intact
#   
#   return(final_raster)
# }
# 
# 
# ##### On an example ---------
# # Select one raster for testing
# example <- rasters[[35]] ### DEFINE RASTER FILE
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
# plot(example_crop, main="Before corridor identification", col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
# plot(patch_corridor_example, main = "After corridor identification", col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))
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
