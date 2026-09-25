#------------------------------------------------#
# Author: Romain Monassier
# Objective: Creating landscape scenarios
#------------------------------------------------#

### Libraries ----

library(sf)
library(dplyr)
library(here)
library(terra)
library(landscapemetrics)

### Data -----
#### Rasters -------
base_path = here("outputs", "data", "landscape_rshifter")
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
plot(rasters_mspa[[39]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "purple", "orange"))
names(rasters_mspa) = years

### Landscape metrics -----

#### Reclassify landscapes -----
# This function merges classes defined in the argument classes_to_merge and gives them the value "new_value"
# r = raster
# year = year of raster
# classes_to_merge = vector of classes to collapse
# new_value = value to assign
merge_classes = function(r, year, classes_to_merge, new_value) {
  
  message("Preprocessing raster for year: ", year)
  
  # remove class 0 (no data)
  r[r == 0] = NA
  
  # merge all classes in classes_to_merge -> new_value
  r[r %in% classes_to_merge] = new_value
  
  return(r)
}


# apply merging
# We create a landscape with matrix to restore (agriculture), matrix that cannot be restored (the rest), habitat
classes = c(2,3,5,6,10)
rasters_simple = purrr::map2(
  rasters_mspa,
  years,
  ~ merge_classes(.x, .y, classes, new_value = 10)
)
# We also assign corridors to forest
classes = c(33)
rasters_simple = purrr::map2(
  rasters_simple,
  years,
  ~ merge_classes(.x, .y, classes, new_value = 1)
)
plot(rasters_simple[['2100']], col=c("darkgreen","lightyellow","gray"))

# Function to compute class-level metrics for all classes
# This function computes landscapemetrics at the class-level for a list of chronological rasters
compute_class_metrics = function(r, year, metrics_to_compute) {
  message("Processing raster for year: ", year)
  
  # Remove class 0
  r[r == 0] = NA
  
  # Compute metrics
  metrics = landscapemetrics::calculate_lsm(
    landscape = r,
    directions = 8,
    what = metrics_to_compute
  )
  
  # Round
  metrics = metrics %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)),
                  year = year, .before = 1)
  
  return(metrics)
}

# Compute metrics
all_lulc_classes = purrr::map2_dfr(
  rasters_simple,
  years,
  ~ compute_class_metrics(.x, 
                          .y, 
                          metrics_to_compute = c("lsm_c_ca", "lsm_c_pland"))
)
