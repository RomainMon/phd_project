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

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))


### Import vectors -----
# Regions Name
regions = terra::vect(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions = terra::project(regions, "EPSG:31983")
regions_sf = sf::st_as_sf(regions)
plot(regions_sf)
# BBOX
bbox = terra::vect(here("data", "geo", "BBOX", "sampling_units_bbox_31983.shp"))
bbox_sf = sf::st_as_sf(bbox)
minbbox = terra::vect(here("data", "geo", "BBOX", "sampling_units_minbbox_buffer5km.shp"))
plot(minbbox)
plot(regions, add=TRUE)
minbbox_sf = sf::st_as_sf(minbbox)

### Download Makurhini -----
# library(devtools)
# library(remotes)
# install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")

### Merge classes -----
# Function to merge classes
merge_classes = function(r, year, x, y, i) {
  message("Preprocessing raster for year: ", year)
  
  # Remove class 0 (no data)
  r[r == 0] = NA
  
  # Merge classes x and y into class i
  r[r %in% c(x, y)] = i
  
  return(r)
}

# Apply on all rasters
rasters_merged = purrr::map2(
  rasters,
  years,
  ~ merge_classes(.x, .y, x = 1, y = 33, i = 1)
)
plot(rasters_merged[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
unique(values(rasters_merged[[1]]))


### Compute class-level landscape metrics ---------
# NB: All functions in landscapemetrics start with lsm_ (for landscape metrics). The second part of the name specifies the level (patch - p, class - c or landscape - l)
# landscapemetrics works on categorical rasters where classes are integers. check_landscape() verifies if the landscapes are in good format

# Check landscape validity (for landscape metrics)
check_landscape(rasters[[1]])
check_landscape(rasters_merged[[1]])

#### Using landscapemetrics  ---------

##### For all classes -------
# Function to compute class-level metrics for all classes
compute_class_metrics = function(r, year) {
  message("Processing raster for year: ", year)
  
  # Remove class 0
  r[r == 0] = NA
  
  # Compute metrics
  metrics = landscapemetrics::calculate_lsm(
    landscape = r,
    directions = 8,
    what = c("lsm_c_ca", "lsm_c_pland", "lsm_c_ed") # Define landscape metrics here
  )
  
  # Round
  metrics = metrics %>%
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

# On forests (core area + corridors)
forest_class_metrics = purrr::map2_dfr(
  rasters_merged,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 1, metrics_to_compute = c("lsm_c_ca",
                                                                              "lsm_c_ai",
                                                                              "lsm_c_area_mn",
                                                                              "lsm_c_area_sd",
                                                                              "lsm_c_pd",
                                                                              "lsm_c_np"))
)

# On forests (core area)
forest_core_metrics = purrr::map2_dfr(
  rasters,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 1, metrics_to_compute = c("lsm_c_ca",
                                                                              "lsm_c_ai",
                                                                              "lsm_c_area_mn",
                                                                              "lsm_c_area_sd",
                                                                              "lsm_c_pd",
                                                                              "lsm_c_np"))
)

# On forests (corridors)
forest_corridors_metrics <- purrr::map2_dfr(
  rasters,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 33, metrics_to_compute = c("lsm_c_ca",
                                                                               "lsm_c_ai",
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

forest_core_metrics = forest_core_metrics %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::select(-c(id, layer)) %>%
  dplyr::mutate(type = "forest_core", .before = 1)

forest_corridors_metrics = forest_corridors_metrics %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::select(-c(id, layer)) %>%
  dplyr::mutate(type = "forest_corridor", .before = 1)

#### Using Makurhini ---------
# NB: Makurhini works on SF polygons

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
list_forest_patches = purrr::imap(rasters_merged, ~{
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
r = rasters_merged[[1]]

# Compute area per cell in m²
cell_areas_m2 = terra::cellSize(r, unit = "m")  # returns SpatRaster of cell areas

# Sum all non-NA cells to get total area
total_area_m2 = sum(values(cell_areas_m2), na.rm = TRUE)

# Convert to hectares
total_area_ha = total_area_m2 / 10000

##### ECA ---------
# NB: computing ECA on all rasters makes the computer crash
# Subsets of rasters
list_forest_patches_1 = list_forest_patches[1:7]   # 1989–1995
list_forest_patches_2 = list_forest_patches[8:14]  # 1996–2002
list_forest_patches_3 = list_forest_patches[15:21] # 2003–2009
list_forest_patches_4 = list_forest_patches[22:28] # 2010–2016
list_forest_patches_5 = list_forest_patches[29:35] # 2017–2023

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

##### ProtConn ------
# ProtConn = MK_ProtConn(nodes = list_forest_patches[[35]],
#                        region = bbox_sf,
#                        area_unit = "ha",
#                        distance = list(type = "centroid", resistance = NULL),
#                        distance_thresholds = c(2000, 8000),
#                        probability = 0.5,
#                        transboundary = NULL,
#                        transboundary_type = "nodes",
#                        protconn_bound = FALSE)


#### Prepare tables -----------
# 1. Landscape metrics for forest class (overall)
forest_class_metrics_ED = forest_class_metrics %>% 
  dplyr::left_join(dplyr::select(class_metrics, c(year, class, ed, pland)), by=c("year", "class"))

# Compute the forest fragmentation index (Ma et al. 2025, Nat Comm)
# To replicate Ma et al. (2023) index, one must: normalize each metric using min–max scaling, invert MPA’s direction (1-MPA), average the three normalized components equally.
forest_class_metrics_ED = forest_class_metrics_ED %>% 
  dplyr::mutate(
    # Normalize each variable (min–max scaling)
    ed_norm = (ed - min(ed, na.rm = TRUE)) / (max(ed, na.rm = TRUE) - min(ed, na.rm = TRUE)),
    pd_norm = (pd - min(pd, na.rm = TRUE)) / (max(pd, na.rm = TRUE) - min(pd, na.rm = TRUE)),
    area_mn_norm = 1 - (area_mn - min(area_mn, na.rm = TRUE)) / (max(area_mn, na.rm = TRUE) - min(area_mn, na.rm = TRUE)),
    # Composite fragmentation index
    FFI = (ed_norm + pd_norm + area_mn_norm) / 3
  )

# Merge with ECA
forest_class_metrics_final = dplyr::left_join(forest_class_metrics_ED, ECA_final, by=c("year"="Time"))


# 2. Landscape metrics for forest core and corridor
forest_core_corridor_metrics_final = bind_rows(forest_core_metrics, forest_corridors_metrics)


### Compute patch-level metrics   ---------

#### On a single raster -----
##### Filter patches -----

# 1. Identify forest patches
forest_patches = landscapemetrics::get_patches(rasters[[1]], class = 1, directions = 8)
patches_rast = forest_patches[[1]][[1]]  # raster of patch IDs
patches_vect = as.polygons(patches_rast, dissolve = TRUE)
plot(patches_vect)
patches_sf = sf::st_as_sf(patches_vect)
plot(patches_sf)

# 2. Select patches within bbox
# Select patches that intersect any minbbox polygon
patches_in_bbox = patches_sf[sf::st_intersects(patches_sf, minbbox_sf, sparse = FALSE) %>% apply(1, any), ]

# Plot
plot(st_geometry(patches_sf), col = 'lightgreen', border = 'darkgreen')
plot(st_geometry(minbbox_sf), add = TRUE, border = 'red', lwd = 2)
plot(st_geometry(patches_in_bbox), add = TRUE, col = 'orange', border = 'orange')

# 2. Remove very small patches
# Calculate patch area in hectares
patches_in_bbox$area_ha = as.numeric(sf::st_area(patches_in_bbox)) / 10000  # m² → ha

# Filter patches
patches_filtered = patches_in_bbox %>%
  dplyr::filter(area_ha >= 5)

# Plot
plot(st_geometry(patches_in_bbox), col = "lightgrey", border = "grey")
plot(st_geometry(patches_filtered), add = TRUE, col = "darkgreen", border = "green")
plot(regions_sf, add=TRUE, col = "red")

# 3. Ensure patches intersecting GLT locations are also included
patches_w_glt = patches_sf[
  sf::st_intersects(patches_sf, regions_sf, sparse = FALSE) %>% apply(1, any),
]

# Combine both sets, avoiding duplicates
patches_final = dplyr::bind_rows(patches_filtered, patches_w_glt) %>%
  dplyr::distinct(geometry, .keep_all = TRUE)

# Check
cat("Number of patches before:", nrow(patches_filtered), "\n")
cat("Number of region-intersecting patches added:", nrow(patches_w_glt), "\n")
cat("Final patch count:", nrow(patches_final), "\n")

# 4. Plot final layer
plot(st_geometry(patches_sf), col = "lightgrey", border = "grey")
plot(st_geometry(patches_final), add = TRUE, col = "darkgreen", border = "darkgreen")
plot(st_geometry(regions_sf), add = TRUE, col = "red", lwd = 2)

##### Landscapemetrics -----

## Compute landscape metrics spatially
mask_rast = rasterize(patches_final, rasters[[1]], field = 6)
plot(mask_rast, col="darkgreen")
check_landscape(mask_rast)
patch_lm = landscapemetrics::spatialize_lsm(
  mask_rast,
  what = c("lsm_p_cai", "lsm_p_shape"),
  directions = 8
)

# Visualize
plot(patch_lm$layer_1$lsm_p_cai, main = "Patch area (selected patches)")
plot(patch_lm$layer_1$lsm_p_shape, main = "Shape (selected patches)")

# Extract values
patch_cai = patch_lm$layer_1$lsm_p_cai
patch_shape = patch_lm$layer_1$lsm_p_shape

##### Makurhini -----

## Compute PC
PC = MK_dPCIIC(nodes = patches_final, attribute = NULL,
                distance = list(type = "centroid"),
                metric = "PC", probability = 0.05,
                distance_thresholds = 8000)
head(PC)

##### Join metrics -----
# Extract area and shape per patch polygon
patch_cai_sf = terra::extract(
  patch_cai,
  patches_final,
  fun = unique,
  na.rm = TRUE,
  bind = TRUE
)
patch_cai_sf = sf::st_as_sf(patch_cai_sf) %>% 
  dplyr::rename(cai = value) %>% 
  dplyr::select(-area_ha)

patch_shape_sf = terra::extract(
  patch_shape,
  patches_final,
  fun = unique,
  na.rm = TRUE,
  bind = TRUE
)
patch_shape_sf = sf::st_as_sf(patch_shape_sf) %>% 
  dplyr::rename(shape = value) %>% 
  dplyr::select(-area_ha)

# Join
patches_metrics_final = patches_final %>%
  dplyr::left_join(st_drop_geometry(patch_cai_sf), by = "lyr.1") %>%
  dplyr::left_join(st_drop_geometry(patch_shape_sf), by = "lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(dplyr::select(PC, -area_ha)), by = "lyr.1")

#### Function ----
compute_patch_metrics = function(r, year, minbbox_sf, regions_sf) {
  message("Processing year: ", year)
  
  #--------------------------#
  # 1. Extract forest patches
  #--------------------------#
  forest_patches = landscapemetrics::get_patches(r, class = 1, directions = 8)
  patches_rast = forest_patches[[1]][[1]]
  patches_vect = terra::as.polygons(patches_rast, dissolve = TRUE, na.rm = TRUE)
  patches_sf = sf::st_as_sf(patches_vect)
  
  #--------------------------#
  # 2. Filter patches by bbox and size
  #--------------------------#
  patches_in_bbox = patches_sf[
    sf::st_intersects(patches_sf, minbbox_sf, sparse = FALSE) %>% apply(1, any),
  ]
  patches_in_bbox$area_ha = as.numeric(sf::st_area(patches_in_bbox)) / 10000
  patches_filtered = patches_in_bbox %>% filter(area_ha >= 5)
  
  # Ensure patches intersecting GLT regions are included
  patches_w_glt = patches_sf[
    sf::st_intersects(patches_sf, regions_sf, sparse = FALSE) %>% apply(1, any),
  ]
  
  # Combine and deduplicate
  patches_final = bind_rows(patches_filtered, patches_w_glt) %>%
    distinct(geometry, .keep_all = TRUE)
  
  if (nrow(patches_final) == 0) {
    warning("No valid patches found for year ", year)
    return(NULL)
  }
  
  #--------------------------#
  # 3. Compute patch-level LSM
  #--------------------------#
  mask_rast = terra::rasterize(patches_final, r, field = 6)
  patch_lm = landscapemetrics::spatialize_lsm(
    mask_rast,
    what = c("lsm_p_cai", "lsm_p_shape"),
    directions = 8
  )
  
  # Extract per-patch values
  patch_cai = terra::extract(
    patch_lm$layer_1$lsm_p_cai,
    patches_final,
    fun = unique,
    na.rm = TRUE,
    bind = TRUE
  )
  patch_shape = terra::extract(
    patch_lm$layer_1$lsm_p_shape,
    patches_final,
    fun = unique,
    na.rm = TRUE,
    bind = TRUE
  )
  
  patch_cai_sf = sf::st_as_sf(patch_cai) %>% 
    dplyr::rename(cai = value) %>% 
    dplyr::select(-area_ha)
  
  patch_shape_sf = sf::st_as_sf(patch_shape) %>% 
    dplyr::rename(shape = value) %>% 
    dplyr::select(-area_ha)
  
  patch_metrics_sf = patches_final %>%
    dplyr::left_join(st_drop_geometry(patch_cai_sf), by = "lyr.1") %>%
    dplyr::left_join(st_drop_geometry(patch_shape_sf), by = "lyr.1")
  
  #--------------------------#
  # 4. Compute PC (Makurhini)
  #--------------------------#
  PC = tryCatch({
    MK_dPCIIC(
      nodes = patches_final,
      attribute = NULL,
      distance = list(type = "centroid"),
      metric = "PC",
      probability = 0.05,
      distance_thresholds = 8000
    )
  }, error = function(e) {
    warning("Makurhini PC failed for year ", year, ": ", e$message)
    NULL
  })
  
  if (!is.null(PC)) {
    patch_metrics_sf = patch_metrics_sf %>%
      dplyr::left_join(st_drop_geometry(dplyr::select(PC, -area_ha)), by = "lyr.1")
  }
  
  #--------------------------#
  # 5. Add year & return
  #--------------------------#
  patch_metrics_sf$year = year
  return(patch_metrics_sf)
}

### Apply to all rasters ----
patches_metrics_list = purrr::map2(
  rasters,
  years,
  ~ compute_patch_metrics(.x, .y, minbbox_sf, regions_sf)
)

### Export datasets ------
base_path = here("outputs", "data", "landscapemetrics")

# Export landscape metrics
write.csv(class_metrics, file = file.path(base_path, "class_metrics_bbox_1989_2023.csv"), row.names = FALSE)
write.csv(forest_class_metrics_final, file = file.path(base_path, "forest_class_metrics_bbox_1989_2023.csv"), row.names=FALSE)
write.csv(forest_core_corridor_metrics_final, file = file.path(base_path, "forest_core_corridors_metrics_bbox_1989_2023.csv"), row.names=FALSE)

# Export patch metrics
purrr::walk2(
  patches_metrics_list,
  years,
  ~ {
    output_path = file.path(base_path, paste0("patches_metrics_", .y, ".shp"))
    sf::st_write(.x, output_path, delete_dsn = TRUE, quiet = TRUE)
    message("✅ Exported: ", output_path)
  }
)
