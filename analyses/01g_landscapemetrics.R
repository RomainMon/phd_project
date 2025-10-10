#------------------------------------------------#
# Author: Romain Monassier
# Objective: Compute landscape metrics
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(terra)
library(ggplot2)
library(raster)
library(landscapemetrics)
library(Makurhini)
library(sf)

### Import rasters (with corridors) -------
base_path = here("outputs", "data", "MapBiomas", "MSPA", "reclass_w_mspa")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)
rasters = lapply(raster_files, terra::rast)
years <- stringr::str_extract(basename(raster_files), "\\d{4}")
plot(rasters[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))

### Download Makurhini
# library(devtools)
# library(remotes)
# install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")

### Merge classes -----
# Function to merge classes
merge_classes <- function(r, year, x, y, i) {
  message("Preprocessing raster for year: ", year)
  
  # Remove class 0 (no data)
  r[r == 0] <- NA
  
  # Merge classes x and y into class i
  r[r %in% c(x, y)] <- i
  
  return(r)
}

# Apply on all rasters
rasters_merged <- purrr::map2(
  rasters,
  years,
  ~ merge_classes(.x, .y, x = 1, y = 33, i = 1)
)
plot(rasters_merged[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
unique(values(rasters_merged[[1]]))




### Compute class-level landscape metrics ---------
# NB: All functions in landscapemetrics start with lsm_ (for landscape metrics). The second part of the name specifies the level (patch - p, class - c or landscape - l)
# landscapemetrics works on categorical rasters where classes are integers. check_landscape() verifies if the landscapes are in good format
check_landscape(rasters[[1]])
check_landscape(rasters_merged[[1]])

#### Using landscapemetrics  ---------

##### For all classes -------
# Function to compute class-level metrics for all classes
compute_class_metrics <- function(r, year) {
  message("Processing raster for year: ", year)
  
  # Remove class 0
  r[r == 0] <- NA
  
  # Compute metrics
  metrics <- landscapemetrics::calculate_lsm(
    landscape = r,
    directions = 8,
    what = c("lsm_c_ca", "lsm_c_pland") # Define landscape metrics here
  )
  
  # Round
  metrics <- metrics %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)),
                  year = year, .before = 1)
  
  return(metrics)
}

# Apply to all raster files
class_metrics = purrr::map2_dfr(rasters_merged, years, compute_class_metrics)

# To wide format
class_metrics = class_metrics %>% 
  tidyr::pivot_wider(
    names_from = metric,  # the column whose values will become new column names
    values_from = value   # the column containing the values
  )

# Remove columns
class_metrics = class_metrics %>% 
  dplyr::select(-c(id,layer))


##### For one class only -----
# Function to compute class-level metrics for one class only
compute_one_class_metrics <- function(r, year, class_value, metrics_to_compute) {
  
  message("Processing raster for year: ", year)
  
  # Remove class 0
  r[r == 0] <- NA
  
  # Mask all other classes except the requested one
  r <- terra::mask(r, r != class_value, maskvalues = TRUE)
  
  # Compute metrics
  metrics <- landscapemetrics::calculate_lsm(
    landscape = r, 
    directions = 8, 
    what = metrics_to_compute
  ) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(.x, 2)), # Round numeric values
      year = year, # Extract year from filename
      .before = 1
    )
  
  return(metrics)
}

# On forests (core area + corridors)
forest_class_metrics <- purrr::map2_dfr(
  rasters_merged,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 1, metrics_to_compute = c("lsm_c_area_mn",
                                                                              "lsm_c_area_sd",
                                                                              "lsm_c_ca",
                                                                              "lsm_c_ai",
                                                                              "lsm_c_np"))
)

# On forests (core area)
forest_core_metrics <- purrr::map2_dfr(
  rasters,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 1, metrics_to_compute = c("lsm_c_np"))
)

# On forests (corridors)
forest_corridors_metrics <- purrr::map2_dfr(
  rasters,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 33, metrics_to_compute = c("lsm_c_np"))
)

# To wide format
forest_class_metrics <- forest_class_metrics %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::select(-c(id, layer)) %>%
  dplyr::mutate(type = "forest_all", .before = 1)

forest_core_metrics <- forest_core_metrics %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::select(-c(id, layer)) %>%
  dplyr::mutate(type = "forest_core", .before = 1)

forest_corridors_metrics <- forest_corridors_metrics %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::select(-c(id, layer)) %>%
  dplyr::mutate(type = "forest_corridor", .before = 1)

# Merge all three data frames
forest_metrics_final <- dplyr::bind_rows(
  forest_class_metrics,
  forest_core_metrics,
  forest_corridors_metrics
)

#### Using Makurhini ---------
# NB: Makurhini works on SF polygons

# 1. Transform forests cells as patches
# Specify raster indices and corresponding years
raster_indices <- c(1, 17, 35)
selected_years <- c("1989", "2006", "2023")

# Function to extract patches and convert to sf
extract_patches_sf <- function(r, class_value = 1) {
  # Extract patches
  patches <- landscapemetrics::get_patches(
    landscape = r,
    class = class_value,
    directions = 8,
    return_raster = TRUE
  )
  
  # Get the raster with patch IDs
  patch_raster <- patches[[1]][[1]]  # layer_1$class_1 equivalent
  patch_raster[is.na(patch_raster)] <- NA
  
  # Polygonize patches (dissolve identical patch IDs)
  patches_vect <- terra::as.polygons(patch_raster, dissolve = TRUE, na.rm = TRUE)
  
  # Add patch ID column
  patches_vect$patch_id <- seq_len(nrow(patches_vect))
  
  # Convert to sf
  patches_sf <- sf::st_as_sf(patches_vect)
  
  return(patches_sf)
}

# Apply to selected rasters and create a named list
list_forest_patches <- purrr::map(raster_indices, ~ extract_patches_sf(rasters[[.x]]))
names(list_forest_patches) <- selected_years

# Check format
lapply(list_forest_patches, class)  # should all be "sf" "data.frame"


# 2. Compute metrics

## Compute study area extent
# Suppose r is your raster
r <- rasters[[1]]

# Compute area per cell in m²
cell_areas_m2 <- terra::cellSize(r, unit = "m")  # returns SpatRaster of cell areas

# Sum all non-NA cells to get total area
total_area_m2 <- sum(values(cell_areas_m2), na.rm = TRUE)

# Convert to hectares
total_area_ha <- total_area_m2 / 10000

## Compute ECA
dECA <- Makurhini::MK_dECA(
  nodes= list_forest_patches, 
  attribute = NULL, 
  area_unit = "ha",
  distance = list(type= "centroid"), 
  metric = "PC",
  probability = 0.05,
  distance_thresholds = 8000,
  LA = total_area_ha, 
  plot= c("1989", "2006", "2023"))
dECA
dECA$dECA_table
dECA$dECA_plot



### Compute patch-level metrics   ---------
compute_patch_metrics <- function(r, year, class_value, metrics_to_compute) {
  message("Processing raster for year: ", year, " (class ", class_value, ")")
  
  # Get patches
  patches <- landscapemetrics::get_patches(r, class = class_value, directions = 8)
  
  # Convert to SpatRaster
  patch_rast <- patches[[1]][[1]]  # layer_1$class_X
  patch_rast[is.na(patch_rast)] <- NA
  
  # Compute landscape metrics
  metrics <- landscapemetrics::spatialize_lsm(
    landscape = patch_rast,
    directions = 8,
    what = metrics_to_compute
  )
  
  # Add the year
  metrics <- metrics %>% mutate(year = year, .before = 1)
  
  return(metrics)
}

# Metrics on forest patches
forest_patch_metrics <- purrr::map2(
  rasters,
  years,
  ~ compute_patch_metrics(.x, .y, class_value = 1, metrics_to_compute = c("lsm_p_area", "lsm_p_shape"))
)

# Combine
forest_patch_metrics_df <- bind_rows(forest_patch_metrics_list)



#### Export datasets ------
base_path = here("outputs", "data", "landscapemetrics")
write.csv(class_metrics, file = file.path(base_path, "class_metrics_bbox_1989_2023.csv"), row.names = FALSE)
write.csv(forest_metrics_final, file = file.path(base_path, "forest_class_metrics_bbox_1989_2023.csv"), row.names=FALSE)
