#------------------------------------------------#
# Author: Romain Monassier
# Objective: Create a dataset on corridor location
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(terra)
library(sf)

### Import datasets -----
#### Road overpasses and AMLD corridors -------
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass_cons_cat")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_cons = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_cons = as.numeric(years_cons)) %>%
  dplyr::arrange(years_cons)
# Load rasters in chronological order
rasters_reclass_cons_cat = lapply(raster_df$file, terra::rast)
years_cons = raster_df$years_cons
# Check
for (i in seq_along(rasters_reclass_cons_cat)) {
  cat("Year", years_cons[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_reclass_cons_cat[[21]], col = c("#FF7F00", "#EE30A7", "darkred"))

#### MSPA corridors  -------
base_path = here("outputs", "data", "MapBiomas", "MSPA", "reclass_w_mspa")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_mspa = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_mspa)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))

#### Patches  -------
base_path = here("outputs", "data", "patchmetrics")
vect_files = list.files(base_path, pattern = "\\.gpkg$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(vect_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
vector_df = data.frame(file = vect_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
patches = lapply(vector_df$file, sf::st_read)
years = vector_df$year
# Check
for (i in seq_along(patches)) {
  cat("Year", years[i], " → raster name:", basename(vector_df$file[i]), "\n")
}
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(patches[[36]]), col=NA, add=TRUE)

### Flag connected patches
# -> Here, we identify corridors connecting two distinct patches
# Steps: 
# 1) Transform corridors to vectors
# 2) For a given year, identify the corridors touching/intersecting >1 patches
# 3) Join patches id to these corridors
# 4) Append back to patch data (for a given year) corridor id, and connected patch id
# 5) Mutate connected variable (1 = YES, 0 = NO)

#### Example -----
# Load corridor raster and convert to vector (polygons)
corridor_raster = rasters_mspa[[36]]
corridor_raster[corridor_raster != 33] = NA
plot(corridor_raster)
corridor_polygons = terra::as.polygons(corridor_raster, aggregate=FALSE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = crs(patches_list[[36]])) %>%
  dplyr::mutate(corridor_id = row_number())  # Assign unique corridor IDs
plot(sf::st_geometry(corridor_polygons))

# Find which patches intersect or touch corridors
patches_2024 = patches[[36]]
patches_2024_filter = patches_2024 %>% 
  dplyr::filter(lengths(sf::st_intersects(patches_2024, corridor_polygons)) > 0)
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(patches_2024_filter, col=NA, add=TRUE)

# Assign corridor_id to patches that intersect or touch corridors
patches_2024_filter = patches_2024 %>%
  sf::st_join(
    corridor_polygons,
    join = st_intersects,  # Includes both intersection and touching
    left = FALSE  # Keep only patches that intersect/touch corridors
  )
