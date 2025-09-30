#------------------------------------------------#
# Author: Romain Monassier
# Objective: Compute landscape metrics on land use rasters
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(landscapemetrics)
library(terra)
library(purrr)
library(stringr)
library(ggplot2)
library(raster)

### Import data -------
## Rasters
base_path <- here("outputs", "data", "MapBiomas", "Mask_sampling_bbox")
raster_files <- list.files(base_path,
                           pattern = "\\.tif$",
                           full.names = TRUE)
rasters <- lapply(raster_files, terra::rast)

## Vectors
roads = vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))

### Quick check -------
crs(rasters[[1]])
ext(rasters[[1]])
plot(rasters[[1]])  # plot the first layer
plot(roads, col="white", add=TRUE)


### Create forest patches ---------
#### Define forest as habitat -------
# Define groups
# NB: to see what codes refer to, check the "Codigos-da-legenda-colecao-9" file
forest <- c(3, 4, 5, 6, 49)
notforest <- c(11, 12, 32, 29, 50, 23)
agri <- c(15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21)
water <- c(26, 33, 31)
artificial <- c(24, 30, 25)

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
plot(rasters_reclass[[1]])
plot(rasters_reclass[[35]])

# The following section is based on Mailys Queru's work
#### Dilatation-erosion --------
dilatation_erosion <- function(raster, seuil) {
  # All cells different than 1 become NA
  habitat <- app(raster, fun = function(v) ifelse(v == 1, 1, NA))
  
  # Distance to habitat
  dist_hab <- distance(habitat)
  
  # Threshold distance and set 0 to NA
  dist_hab_thresh <- app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA))
  
  # Distance to non-habitat
  dist_nonhab <- distance(dist_hab_thresh)
  
  # Final threshold
  final_rast <- dist_nonhab > seuil
  
  return(final_rast)
}

seuil = 100 # Here, we define the buffer width (dilatation length) (in meters)
raster_dilat = dilatation_erosion(rasters_reclass[[1]], seuil)
plot(raster_dilat)

##### Exclusion of linear features ------


### Compute landscape metrics ---------
# NB: All functions in landscapemetrics start with lsm_ (for landscape metrics). The second part of the name specifies the level (patch - p, class - c or landscape - l)

#### At the class scale  ---------

##### On a single raster   ---------

######  For all classes ---------

# Pick the last raster (2023)
last_raster <- rasters[[length(rasters)]]

# NB: landscapemetrics works on categorical rasters where classes are integers. Therefore, we need to convert the raster to a categorical raster
# Check
check_landscape(last_raster)

# Remove class 0 (no identifiable land use)
last_raster[last_raster == 0] <- NA
plot(last_raster)

# Compute metrics
class_metrics <- landscapemetrics::calculate_lsm(landscape = last_raster, directions = 8, 
                                                what = c("lsm_c_area_mn",
                                                         "lsm_c_area_sd",
                                                         "lsm_c_ca",
                                                         "lsm_c_pland"))

# Round
class_metrics <- class_metrics %>% 
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

# Extract year from filename (first 4-digit number)
last_file   <- raster_files[length(raster_files)]
year <- stringr::str_extract(basename(last_file), "\\d{4}")
class_metrics <- class_metrics %>%
  dplyr::mutate(year = year)

######  For one class ---------

# We compute metrics on the Forest class ("Forest formation" in the 9th MapBiomas collection)
# Filter the raster using a Mask and the class value
forest_class <- mask(last_raster, last_raster != 3, maskvalues = TRUE)
plot(forest_class, col="darkgreen")

# NB: landscapemetrics works on categorical rasters where classes are integers. Therefore, we need to convert the raster to a categorical raster
# Check
check_landscape(forest_class)

# Compute metrics
forest_metrics <- landscapemetrics::calculate_lsm(landscape = forest_class, directions = 8, 
                                                       what = c("lsm_c_division",
                                                                "lsm_c_np",
                                                                "lsm_c_pd"))

# Round
forest_metrics <- forest_metrics %>% 
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

# Add year column
last_file   <- raster_files[length(raster_files)]
year <- str_extract(basename(last_file), "\\d{4}")
forest_metrics <- forest_metrics %>%
  dplyr::mutate(year = year)

##### On all rasters ---------

###### For all classes -------
# Function to compute class-level metrics for all classes
compute_class_metrics <- function(r_file) {
  
  # Read raster
  r <- terra::rast(r_file)
  
  # Remove class 0
  r[r == 0] <- NA
  
  # Compute metrics
  metrics <- landscapemetrics::calculate_lsm(landscape = r, directions = 8, 
                                                         what = c("lsm_c_ca",
                                                                  "lsm_c_pland"))
  
  # Round
  metrics <- metrics %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  
  # Extract year from filename
  year <- stringr::str_extract(basename(r_file), "\\d{4}")
  metrics <- metrics %>%
    dplyr::mutate(year = year, .before = 1)
  
  return(metrics)
}

# Apply to all raster files
class_metrics <- purrr::map_dfr(raster_files, compute_class_metrics)

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
compute_one_class_metrics <- function(r_file, class_value) {
  
  # Read raster
  r <- terra::rast(r_file)
  
  # Remove class 0
  r[r == 0] <- NA
  
  # Mask all other classes except the requested one
  r <- terra::mask(r, r != class_value, maskvalues = TRUE)
  
  # Compute metrics
  metrics <- landscapemetrics::calculate_lsm(
    landscape = r, 
    directions = 8, 
    what = c("lsm_c_area_mn",
             "lsm_c_area_sd",
             "lsm_c_ca",
             "lsm_c_ai",
             "lsm_c_np")
  )
  
  # Round numeric values
  metrics <- metrics %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  
  # Extract year from filename
  year <- stringr::str_extract(basename(r_file), "\\d{4}")
  metrics <- metrics %>%
    dplyr::mutate(year = year, .before = 1)
  
  return(metrics)
}


## For forest only (class 3)
forest_class_metrics <- purrr::map_dfr(raster_files, ~ compute_one_class_metrics(.x, class_value = 3))

# To wide format
forest_class_metrics = forest_class_metrics %>% 
  tidyr::pivot_wider(
    names_from = metric,  # the column whose values will become new column names
    values_from = value   # the column containing the values
  )
# Remove columns
forest_class_metrics = forest_class_metrics %>% 
  dplyr::select(-c(id,layer))

#### At the patch level   ---------

##### On a single raster   ---------

# Pick the last raster (2023)
last_raster <- rasters[[length(rasters)]]

# Get forest patches
forest_patches <- landscapemetrics::get_patches(last_raster, class = 3, directions = 8) 
forest_patches = forest_patches$layer_1$class_3 # Convert to SpatRaster
forest_patches_metrics = landscapemetrics::spatialize_lsm(forest_patches, directions = 8, 
                                                          what = c("lsm_p_area", "lsm_p_cai", "lsm_p_enn", "lsm_p_para"))

#### Export datasets ------
base_path = here("outputs", "data", "landscapemetrics")
write.csv(class_metrics, file = file.path(base_path, "class_metrics_bbox_1989_2023.csv"), row.names = FALSE)
write.csv(forest_class_metrics, file = file.path(base_path, "forest_class_metrics_bbox_1989_2023.csv"), row.names=FALSE)
