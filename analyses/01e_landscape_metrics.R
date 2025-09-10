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
library(stringr)

### Import data -------
## Rasters
base_path <- here("outputs", "data", "MapBiomas")
raster_files <- list.files(base_path,
                           pattern = "_SJ_watershed_31983\\.tif$",
                           full.names = TRUE)
rasters <- lapply(raster_files, terra::rast)

### Quick check -------
crs(rasters[[1]])
ext(rasters[[1]])
plot(rasters[[1]])  # plot the first layer

### Compute landscape metrics ---------
# NB: All functions in landscapemetrics start with lsm_ (for landscape metrics). The second part of the name specifies the level (patch - p, class - c or landscape - l)
# NB: landscapemetrics works on categorical rasters where classes are integers. Therefore, we need to convert the raster to a categorical raster

#### At the class scale  ---------

##### On a single raster   ---------

######  For all classes ---------

# Pick the last raster (2023)
last_raster <- rasters[[length(rasters)]]

# Check
check_landscape(last_raster)

# Remove class 0 (no identifiable land use)
last_raster[last_raster == 0] <- NA
plot(last_raster)

# Compute class-level metrics
metrics_list <- list(
  area_mn = lsm_c_area_mn(landscape = last_raster, directions = 8),
  area_sd = lsm_c_area_sd(landscape = last_raster, directions = 8),
  ca      = lsm_c_ca(landscape = last_raster, directions = 8),
  pland   = lsm_c_pland(landscape = last_raster, directions = 8)
)

# Combine into one wide dataframe
metrics_all_classes <- metrics_list %>%
  imap(~ .x %>% select(class, value) %>% rename(!!.y := value)) %>%  # rename value to metric name
  purrr::reduce(left_join, by = "class")  %>%                               # join all metrics by class
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

# Extract year from filename (first 4-digit number)
last_file   <- raster_files[length(raster_files)]
year <- str_extract(basename(last_file), "\\d{4}")
metrics_all_classes <- metrics_all_classes %>%
  dplyr::mutate(year = year)

######  For one class ---------

# We compute metrics on the Forest class ("Forest formation" in the 9th MapBiomas collection)
# Filter the raster using a Mask and the class value
forest_class <- mask(last_raster, last_raster != 3, maskvalues = TRUE)
plot(forest_class, col="darkgreen")

# Check
check_landscape(forest_class)

# Compute the metrics
metrics_list <- list(
  division = lsm_c_division(landscape = forest_class, directions = 8),
  np = lsm_c_np(landscape = forest_class, directions = 8),
  pd = lsm_c_pd(landscape = forest_class, directions = 8)
)

# Combine into one dataframe
metrics_forest <- metrics_list %>%
  dplyr::bind_rows(.id = "metric_name") %>%
  dplyr::select(metric_name, class, value) %>%
  tidyr::pivot_wider(names_from = metric_name, values_from = value) %>% 
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  dplyr::left_join(metrics_all_classes) # Join the other metrics computed at the class level

# Add year column
last_file   <- raster_files[length(raster_files)]
year <- str_extract(basename(last_file), "\\d{4}")
metrics_forest <- metrics_forest %>%
  dplyr::mutate(year = year)


#### At the patch level   ---------
# Compute the metrics
metrics_list <- list(
  area = lsm_p_area(landscape = forest_class, directions = 8),
  cai = lsm_p_cai(landscape = forest_class, directions = 8),
  enn = lsm_p_enn(landscape = forest_class, directions = 8),
  para = lsm_p_para(landscape = forest_class, directions = 8)
  )

# Combine into one dataframe
# Reshape into wide dataframe (one row per patch)
forest_patch_metrics <- metrics_list %>%
  imap(~ .x %>% 
         select(id, class, value) %>% 
         rename(!!.y := value)) %>% 
  purrr::reduce(left_join, by = c("id", "class")) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))
