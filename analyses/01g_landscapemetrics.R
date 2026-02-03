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

### Import rasters -------
#### Rasters reclassified ----
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_reclass = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_reclass)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))


#### With forest status --------
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass_forest_cat")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_reclass_forest_cat = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_reclass_forest_cat)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_reclass_forest_cat[[36]], col = c("#32a65e", "#006400", "#CDAD00", "#C1FFC1", "#CDCDC1"))


#### With forest restoration activities --------
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


#### MSPA rasters (with corridors) -------
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
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "yellow"))


#### Cumulative rasters (de/reforestation) -------
base_path = here("outputs", "data", "MapBiomas", "Rasters_cumulative_tm")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
# IMPORTANT: here, years are different from other rasters because 1989 is omitted (baseline)
years_tm = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_tm = as.numeric(years_tm)) %>%
  dplyr::arrange(years_tm)
# Load rasters in chronological order
rasters_tm = lapply(raster_df$file, terra::rast)
years_tm = raster_df$years_tm
# Check
for (i in seq_along(rasters_tm)) {
  cat("Year", years_tm[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))


#### Forest age rasters -------
base_path = here("outputs", "data", "MapBiomas", "Rasters_forest_age_cat")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_forest_age = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_forest_age)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_forest_age[[36]], col = c("#AFC0F1", "#BEC3EB","#CCC7E6", "#EACEDB"))


### Download Makurhini -----
# library(devtools)
# library(remotes)
# install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")


### Compute class-level landscape metrics ---------
# NB: All functions in landscapemetrics start with lsm_ (for landscape metrics). The second part of the name specifies the level (patch - p, class - c or landscape - l)
# landscapemetrics works on categorical rasters where classes are integers. check_landscape() verifies if the landscapes are in good format


# Check landscape validity (for landscape metrics)
check_landscape(rasters_reclass[[36]])
check_landscape(rasters_reclass_forest_cat[[36]])
check_landscape(rasters_reclass_cons_cat[[21]])
check_landscape(rasters_mspa[[36]])
check_landscape(rasters_tm[[35]])
check_landscape(rasters_forest_age[[36]])


#### Using landscapemetrics  ---------

##### Functions -------
### Merge classes
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

# Function to compute class-level metrics for one class only
# This function computes landscapemetrics at the class-level for ONE class exclusively, and on a list of chronological rasters
compute_one_class_metrics = function(r, year, class_value, metrics_to_compute) {
  
  message("Processing raster for year: ", year)
  
  # Remove class 0
  r[r == 0] = NA
  
  # Mask all other classes except the requested one
  r = terra::mask(r, r != class_value, maskvalues = TRUE)
  
  # Compute metrics
  metrics = landscapemetrics::calculate_lsm(
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


##### Global LULC evolution -----
# Here, we're interested in forest evolution (as a whole)
all_lulc_classes = purrr::map2_dfr(
  rasters_reclass,
  years,
  ~ compute_class_metrics(.x, .y, metrics_to_compute = c("lsm_c_ca", "lsm_c_pland", "lsm_c_ed"))
)

# To wide format
all_lulc_classes = all_lulc_classes %>% 
  tidyr::pivot_wider(
    names_from = metric,  # the column whose values will become new column names
    values_from = value   # the column containing the values
  )

# Remove columns
all_lulc_classes = all_lulc_classes %>% 
  dplyr::select(-c(id,layer))

# quick summary
summary(all_lulc_classes)


##### Forest status --------
plot(rasters_reclass_forest_cat[[36]])
freq(rasters_reclass_forest_cat[[36]])
raster_forest_cat_2024 = rasters_reclass_forest_cat[[36]]

# compute metrics
forest_cat_metrics = compute_class_metrics(raster_forest_cat_2024, 2024, metrics_to_compute = c("lsm_c_ca", "lsm_c_pland"))

# to wide format
forest_cat_metrics = forest_cat_metrics %>% 
  tidyr::pivot_wider(names_from = metric, values_from = value) %>% 
  dplyr::select(-id, -layer)

# quick summary
summary(forest_cat_metrics)


##### Conservation activities --------
freq(rasters_reclass_cons_cat[[21]])

# compute metrics
cons_cat_metrics = purrr::map2_dfr(
  rasters_reclass_cons_cat,
  years_cons,
  ~ compute_class_metrics(.x, .y, metrics_to_compute = c("lsm_c_ca", "lsm_c_pland", "lsm_c_np"))
)

# to wide format
cons_cat_metrics = cons_cat_metrics %>% 
  tidyr::pivot_wider(names_from = metric, values_from = value) %>% 
  dplyr::select(-id, -layer)

# quick summary
summary(cons_cat_metrics)


##### Deforestation and reforestation --------
# First, we merge all matrix land uses
# vector of classes to merge:
matrix_classes = c(2,3,4,5,6)

# apply merging
rasters_forest_vs_matrix = purrr::map2(
  rasters_tm,
  years_tm,
  ~ merge_classes(.x, .y, matrix_classes, new_value = 0)
)
freq(rasters_forest_vs_matrix[[35]])

# compute metrics
defor_refor_metrics = purrr::map2_dfr(
  rasters_forest_vs_matrix,
  years_tm,
  ~ compute_class_metrics(.x, .y, metrics_to_compute = c("lsm_c_ca", "lsm_c_pland", "lsm_c_np", "lsm_c_area_mn", "lsm_c_area_sd"))
)

# to wide format
defor_refor_metrics = defor_refor_metrics %>% 
  tidyr::pivot_wider(names_from = metric, values_from = value) %>% 
  dplyr::select(-id, -layer)

# quick summary
summary(defor_refor_metrics)


##### Forest age --------
freq(rasters_forest_age[[36]])

# compute metrics
forest_age_metrics = purrr::map2_dfr(
  rasters_forest_age,
  years,
  ~ compute_class_metrics(.x, .y, metrics_to_compute = c("lsm_c_ca", "lsm_c_pland", "lsm_c_np"))
)

# to wide format
forest_age_metrics = forest_age_metrics %>% 
  tidyr::pivot_wider(names_from = metric, values_from = value) %>% 
  dplyr::select(-id, -layer)

# quick summary
summary(forest_age_metrics)


##### Other forest metrics --------
# Here, we compute new metrics for the forest class only
forest_class_metrics = purrr::map2_dfr(
  rasters_reclass,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 1, metrics_to_compute = c("lsm_c_ca",
                                                                              "lsm_c_tca",
                                                                              "lsm_c_lsi",
                                                                              "lsm_c_mesh",
                                                                              "lsm_c_area_mn",
                                                                              "lsm_c_area_sd",
                                                                              "lsm_c_pd",
                                                                              "lsm_c_np"))
)

# To wide format
forest_class_metrics = forest_class_metrics %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::select(-c(id, layer)) %>%
  dplyr::mutate(type = "forest_all", .before = 1)

# quick check
summary(forest_class_metrics)

##### Core forest --------
# Here, we compute metrics on core forests only (using MSPA)
forest_core_metrics = purrr::map2_dfr(
  rasters_mspa,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 1, metrics_to_compute = c("lsm_c_ca",
                                                                              "lsm_c_ai",
                                                                              "lsm_c_area_mn",
                                                                              "lsm_c_area_sd",
                                                                              "lsm_c_pd",
                                                                              "lsm_c_np"))
)

# to wide format
forest_core_metrics = forest_core_metrics %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::select(-c(id, layer)) %>%
  dplyr::mutate(type = "forest_core", .before = 1)

# quick check
summary(forest_core_metrics)


##### Corridors --------
# Here, we compute metrics on corridor forests only (using MSPA)
forest_corridors_metrics <- purrr::map2_dfr(
  rasters_mspa,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 33, metrics_to_compute = c("lsm_c_ca",
                                                                               "lsm_c_ai",
                                                                               "lsm_c_area_mn",
                                                                               "lsm_c_area_sd",
                                                                               "lsm_c_pd",
                                                                               "lsm_c_np"))
)

# to wide format
forest_corridors_metrics = forest_corridors_metrics %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::select(-c(id, layer)) %>%
  dplyr::mutate(type = "forest_corridor", .before = 1)

# quick check
summary(forest_corridors_metrics)


#### Using Makurhini ---------
# NB: Makurhini works on SF polygons

##### ECA ------
# Here, we compute ECA on forest habitat
# We quantify the level of connectivity of forest habitat in the landscape

# 1. Transform forests cells as patches
# Function to extract patches and convert to sf
extract_patches_sf = function(r, class_value) {
  # Extract patches
  patches = landscapemetrics::get_patches(
    landscape = r,
    class = class_value,
    directions = 8,
    return_raster = TRUE
  )
  
  # Get the raster with patch IDs
  patch_raster = patches[[1]][[1]]  # layer_1$class_1 equivalent
  patch_raster[is.na(patch_raster)] = NA
  
  # Polygonize patches (dissolve identical patch IDs)
  patches_vect = terra::as.polygons(patch_raster, dissolve = TRUE, na.rm = TRUE)
  
  # Add patch ID column
  patches_vect$patch_id = seq_len(nrow(patches_vect))
  
  # Convert to sf
  patches_sf = sf::st_as_sf(patches_vect)
  
  return(patches_sf)
}

# Apply to rasters and create a named list
list_forest_patches = purrr::imap(rasters_reclass, ~{
  message("Processing raster: ", .y)
  extract_patches_sf(.x, class_value = 1)
})
names(list_forest_patches) = years
names(list_forest_patches)

# Check CRS consistency
unique(sapply(list_forest_patches, \(x) st_crs(x)$epsg))

# Check for NULLs
any(sapply(list_forest_patches, is.null))

# Check format
lapply(list_forest_patches, class)  # should all be "sf" "data.frame"

# 2. Compute metrics

## Compute study area extent
r = rasters_reclass[[1]]

# Compute area per cell in ha
cell_areas_ha = terra::cellSize(r, unit = "ha")  # returns SpatRaster of cell areas

# Sum all non-NA cells to get total area
total_area_ha = sum(values(cell_areas_ha), na.rm = TRUE)
total_area_ha

# NB: computing ECA on all rasters makes the computer crash
# Subsets of rasters
list_forest_patches_1 = list_forest_patches[1:7]   # 1989–1995
list_forest_patches_2 = list_forest_patches[8:14]  # 1996–2002
list_forest_patches_3 = list_forest_patches[15:21] # 2003–2009
list_forest_patches_4 = list_forest_patches[22:28] # 2010–2016
list_forest_patches_5 = list_forest_patches[29:36] # 2017–2024

# Compute ECA
dECA1 = Makurhini::MK_dECA(
  nodes= list_forest_patches_1, 
  attribute = NULL, 
  area_unit = "ha",
  distance = list(type= "centroid"), 
  metric = "PC",
  probability = 0.05,
  distance_thresholds = c(2000, 8000),
  LA = total_area_ha,
  plot=names(list_forest_patches_1)
  )
dECA2 = Makurhini::MK_dECA(
  nodes= list_forest_patches_2, 
  attribute = NULL, 
  area_unit = "ha",
  distance = list(type= "centroid"), 
  metric = "PC",
  probability = 0.05,
  distance_thresholds = c(2000, 8000),
  LA = total_area_ha,
  plot=names(list_forest_patches_2)
)
dECA3 = Makurhini::MK_dECA(
  nodes= list_forest_patches_3, 
  attribute = NULL, 
  area_unit = "ha",
  distance = list(type= "centroid"), 
  metric = "PC",
  probability = 0.05,
  distance_thresholds = c(2000, 8000),
  LA = total_area_ha,
  plot=names(list_forest_patches_3)
)
dECA4 = Makurhini::MK_dECA(
  nodes= list_forest_patches_4, 
  attribute = NULL, 
  area_unit = "ha",
  distance = list(type= "centroid"), 
  metric = "PC",
  probability = 0.05,
  distance_thresholds = c(2000, 8000),
  LA = total_area_ha,
  plot=names(list_forest_patches_4)
)
dECA5 = Makurhini::MK_dECA(
  nodes= list_forest_patches_5, 
  attribute = NULL, 
  area_unit = "ha",
  distance = list(type= "centroid"), 
  metric = "PC",
  probability = 0.05,
  distance_thresholds = c(2000, 8000),
  LA = total_area_ha,
  plot=names(list_forest_patches_5)
)


# Combine ECA tables
ECA_total = dplyr::bind_rows(
  dECA1$`2000`$dECA_table,
  dECA1$`8000`$dECA_table,
  dECA2$`2000`$dECA_table,
  dECA2$`8000`$dECA_table,
  dECA3$`2000`$dECA_table,
  dECA3$`8000`$dECA_table,
  dECA4$`2000`$dECA_table,
  dECA4$`8000`$dECA_table,
  dECA5$`2000`$dECA_table,
  dECA5$`8000`$dECA_table) %>%
  dplyr::mutate(Time = as.numeric(as.character(Time))) %>% 
  dplyr::rename(land_area = `Max. Landscape attribute (ha)`,
                hab_area = `Habitat area (ha)`,
                dist_threshold = `Distance threshold`,
                eca_ha = `ECA (ha)`,
                eca_pct_land = `Normalized ECA (% of LA)`,
                eca_pct_hab = `Normalized ECA (% of habitat area)`,
                dA_dECA_compar = `dA/dECA comparisons`,
                change = `Type of change`)

ECA_final = ECA_total %>% 
  # keep only columns that are numeric or relevant
  dplyr::select(Time, dist_threshold, land_area, hab_area, eca_ha, eca_pct_land, eca_pct_hab, dA, dECA, rECA, dA_dECA_compar, change) %>%
  # pivot wider so that distance threshold becomes a suffix
  tidyr::pivot_wider(
    names_from = dist_threshold,
    values_from = c(land_area, hab_area, eca_ha, eca_pct_land, eca_pct_hab, dA, dECA, rECA, dA_dECA_compar, change),
    names_glue = "{.value}_{dist_threshold}"
  )


#### Prepare tables -----------
# 1. Landscape metrics for forest class (overall)
forest_class_metrics_ED = forest_class_metrics %>% 
  dplyr::left_join(dplyr::select(all_lulc_classes, c(year, class, ed, pland)), by=c("year", "class"))

# Compute the forest fragmentation index (Ma et al. 2025, Nat Comm)
# To replicate Ma et al. (2023) index, one must: normalize each metric using min–max scaling, invert MPA’s direction (1-MPA), average the three normalized components equally.
forest_class_metrics_FFI = forest_class_metrics_ED %>% 
  dplyr::mutate(
    # Normalize each variable (min–max scaling)
    ed_norm = (ed - min(ed, na.rm = TRUE)) / (max(ed, na.rm = TRUE) - min(ed, na.rm = TRUE)),
    pd_norm = (pd - min(pd, na.rm = TRUE)) / (max(pd, na.rm = TRUE) - min(pd, na.rm = TRUE)),
    area_mn_norm = 1 - (area_mn - min(area_mn, na.rm = TRUE)) / (max(area_mn, na.rm = TRUE) - min(area_mn, na.rm = TRUE)),
    # Forest fragmentation index
    FFI = (ed_norm + pd_norm + area_mn_norm) / 3
  )

# Merge with ECA
forest_class_metrics_final = dplyr::left_join(forest_class_metrics_FFI, ECA_final, by=c("year"="Time"))


# 2. Landscape metrics for forest core and corridor
forest_core_corridor_metrics_final = bind_rows(forest_core_metrics, forest_corridors_metrics)


### Export datasets ------
base_path = here("outputs", "data", "landscapemetrics")

# Export landscape metrics
write.csv(all_lulc_classes, file = file.path(base_path, "all_lulc_classes_bbox_1989_2024.csv"), row.names = FALSE)
write.csv(forest_class_metrics_final, file = file.path(base_path, "forest_class_metrics_bbox_1989_2024.csv"), row.names=FALSE)
write.csv(forest_core_corridor_metrics_final, file = file.path(base_path, "forest_core_corridors_metrics_bbox_1989_2024.csv"), row.names=FALSE)
write.csv(forest_cat_metrics, file = file.path(base_path, "forest_cat_metrics_bbox_2024.csv"), row.names=FALSE)
write.csv(cons_cat_metrics, file = file.path(base_path, "cons_cat_metrics_bbox_2004_2024.csv"), row.names=FALSE)
write.csv(forest_age_metrics, file = file.path(base_path, "forest_age_metrics_bbox_1989_2024.csv"), row.names=FALSE)
write.csv(defor_refor_metrics, file = file.path(base_path, "defor_refor_metrics_bbox_1989_2024.csv"), row.names=FALSE)
