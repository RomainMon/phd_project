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
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))

#### ...With forest categories --------
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
plot(rasters_reclass_forest_cat[[36]])

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
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))


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


### Compute class-level landscape metrics ---------
# NB: All functions in landscapemetrics start with lsm_ (for landscape metrics). The second part of the name specifies the level (patch - p, class - c or landscape - l)
# landscapemetrics works on categorical rasters where classes are integers. check_landscape() verifies if the landscapes are in good format

# Check landscape validity (for landscape metrics)
check_landscape(rasters_reclass[[36]])
check_landscape(rasters_reclass_forest_cat[[36]])
check_landscape(rasters_mspa[[36]])

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

# example:
r_example = merge_classes(rasters_reclass_forest_cat[[36]], years[[36]], classes_to_merge = c(2,3,4,5), new_value = 99)
plot(r_example)
unique(values(r_example))
freq(r_example)

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

##### By forest categories --------
# First, we merge all matrix land uses
# vector of classes to merge:
matrix_classes = c(2,3,4,5)

# apply merging
rasters_forest_vs_matrix = purrr::map2(
  rasters_reclass_forest_cat,
  years,
  ~ merge_classes(.x, .y, matrix_classes, new_value = 2)  # or 99, but 2 is nice = “other LULC”
)
freq(rasters_forest_vs_matrix[[36]])

# compute metrics
forest_cat_metrics = purrr::map2_dfr(
  rasters_forest_vs_matrix,
  years,
  ~ compute_class_metrics(.x, .y, metrics_to_compute = c("lsm_c_ca", "lsm_c_pland", "lsm_c_np"))
)

# to wide format
forest_cat_metrics = forest_cat_metrics %>% 
  tidyr::pivot_wider(names_from = metric, values_from = value) %>% 
  dplyr::select(-id, -layer)

# quick summary
summary(forest_cat_metrics)

##### Other forest metrics (core + corridor) --------
# Here, we compute new metrics for the forest class only
forest_class_metrics = purrr::map2_dfr(
  rasters_reclass,
  years,
  ~ compute_one_class_metrics(.x, .y, class_value = 1, metrics_to_compute = c("lsm_c_ca",
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
# Here, we compute ECA on forest habitat (irrespective of their status)
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

##### ProtConn ------
# NB: ProtConn can ne computed with Makurhini, but we can also compute ProtConn manually 
# ProtConn = 100 * (ECA/AL) (AL being landscape area) (Saura et al. 2017, Ecological Indicators)

# ProtConn_1 = lapply(list_forest_patches_1, \(x)
#                     MK_ProtConn(
#                       nodes = x,
#                       region = bbox_sf,
#                       area_unit = "ha",
#                       distance = list(type = "centroid", resistance = NULL),
#                       distance_thresholds = c(2000, 8000),
#                       probability = 0.5,
#                       transboundary = NULL,
#                       transboundary_type = "nodes",
#                       protconn_bound = FALSE))
# ProtConn_2 = lapply(list_forest_patches_2, \(x)
#                     MK_ProtConn(
#                       nodes = x,
#                       region = bbox_sf,
#                       area_unit = "ha",
#                       distance = list(type = "centroid", resistance = NULL),
#                       distance_thresholds = c(2000, 8000),
#                       probability = 0.5,
#                       transboundary = NULL,
#                       transboundary_type = "nodes",
#                       protconn_bound = FALSE))
# ProtConn_3 = lapply(list_forest_patches_3, \(x)
#                     MK_ProtConn(
#                       nodes = x,
#                       region = bbox_sf,
#                       area_unit = "ha",
#                       distance = list(type = "centroid", resistance = NULL),
#                       distance_thresholds = c(2000, 8000),
#                       probability = 0.5,
#                       transboundary = NULL,
#                       transboundary_type = "nodes",
#                       protconn_bound = FALSE))
# ProtConn_4 = lapply(list_forest_patches_4, \(x)
#                     MK_ProtConn(
#                       nodes = x,
#                       region = bbox_sf,
#                       area_unit = "ha",
#                       distance = list(type = "centroid", resistance = NULL),
#                       distance_thresholds = c(2000, 8000),
#                       probability = 0.5,
#                       transboundary = NULL,
#                       transboundary_type = "nodes",
#                       protconn_bound = FALSE))
# ProtConn_5 = lapply(list_forest_patches_5, \(x)
#                     MK_ProtConn(
#                       nodes = x,
#                       region = bbox_sf,
#                       area_unit = "ha",
#                       distance = list(type = "centroid", resistance = NULL),
#                       distance_thresholds = c(2000, 8000),
#                       probability = 0.5,
#                       transboundary = NULL,
#                       transboundary_type = "nodes",
#                       protconn_bound = FALSE))
# 
# # Combine ProtConn tables
# all_ProtConn = c(ProtConn_1, ProtConn_2, ProtConn_3, ProtConn_4, ProtConn_5)
# ProtConn_total = do.call(rbind, lapply(names(all_ProtConn), function(year) {
#   
#   year_obj = all_ProtConn[[year]]
#   
#   # iterate over distance thresholds
#   do.call(rbind, lapply(names(year_obj), function(dist) {
#     df <- year_obj[[dist]]
#     df$year <- year       # add year
#     df$distance <- dist   # add distance
#     df
#   }))
# }))
# 
# # To wide format
# ProtConn_total = as.data.frame(ProtConn_total)
# ProtConn_total = ProtConn_total %>% 
#   tidyr::pivot_wider(names_from = Index, values_from = Value) %>% 
#   tidyr::pivot_wider(names_from = `ProtConn indicator`, values_from = Percentage) %>% 
#   dplyr::group_by(year, distance) %>% 
#   dplyr::summarise(across(everything(), ~ first(na.omit(.x))), .groups = "drop")
# 
# # Rename
# ProtConn_total = ProtConn_total %>% 
#   dplyr::rename(land_area = `Maximum landscape attribute`,
#                 protected_surf = `Protected surface`,
#                 EC_PC = `EC(PC)`) %>% 
#   dplyr::select(-7)
# 
# # Pivot wider
# ProtConn_final = ProtConn_total %>% 
#   dplyr::mutate(year = as.numeric(as.character(year))) %>% 
#   # pivot wider so that distance threshold becomes a suffix
#   tidyr::pivot_wider(
#     names_from = distance,
#     values_from = c(EC_PC:ProtConn_Trans_land),
#     names_glue = "{.value}_{distance}"
#   )



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
    # Composite fragmentation index
    FFI = (ed_norm + pd_norm + area_mn_norm) / 3
  )

# Merge with ECA
forest_class_metrics_final = dplyr::left_join(forest_class_metrics_FFI, ECA_final, by=c("year"="Time"))


# 2. Landscape metrics for forest core and corridor
forest_core_corridor_metrics_final = bind_rows(forest_core_metrics, forest_corridors_metrics)


### Compute patch-level metrics   ---------

#### On a single raster -----
##### Patch selection (core only) ----------

# 1) patches = core forest only, class = 1 (i.e., 2 fragments connected by a corridor are considered as 2 patches)
plot(rasters_mspa[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))
forest_patches = landscapemetrics::get_patches(rasters_mspa[[1]], class = 1, directions = 8)
patches_sf = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))

# visualize
plot(st_geometry(patches_sf), col="darkgreen", main="core patches")

# 2) patches intersecting GLT regions
patch_in = patches_sf[sf::st_intersects(patches_sf, regions_sf, sparse=FALSE) %>%  apply(1, any),]

# 3) patches within 500m of regions (buffer once, reuse)
regions_buffer = sf::st_buffer(regions_sf, 500)
patch_near = patches_sf[st_intersects(patches_sf, regions_buffer, sparse=FALSE) %>%  apply(1, any),]

# 4) final kept patches = in OR near
patches_final = bind_rows(patch_in, patch_near) %>% dplyr::distinct(geometry, .keep_all=TRUE)

# QC plot
plot(st_geometry(patches_sf), col="grey80", border=NA, main="kept patches")
plot(st_geometry(patches_final), add=TRUE, col="darkgreen", border=NA)
plot(st_geometry(regions_sf), add=TRUE, col="red", lwd=2)

##### Landscapemetrics ----------------------------------------------

mask_rast = terra::rasterize(patches_final, rasters_mspa[[1]], field = 1)
plot(mask_rast)
patch_lm = landscapemetrics::spatialize_lsm(
  mask_rast,
  what = c("lsm_p_area","lsm_p_shape"),
  directions=8
)

patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% 
  dplyr::rename(area=value)
patch_shape = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_shape, patches_final, fun=unique, bind=TRUE)) %>%  
  dplyr::rename(shape=value)

##### Connectivity (Makurhini) --------------------------------
## 1) PC with centroid distance
PC_centroid = MK_dPCIIC(nodes = patches_final,
                        distance = list(type="centroid"),
                        metric="PC",
                        probability=0.05,
                        distance_thresholds=c(2000,8000))

## 2) PC with resistance rasters
## resistance raster = rasters with forest core and corridors
mspa = rasters_mspa[[1]]

# Computational times are long so we crop the original raster
# buffer minbbox to avoid cutting important connectors at the border
minbbox2000 = terra::buffer(minbbox, width=2000)
plot(minbbox2000)

# buffer patches 1 km
patches_buf1km = terra::buffer(terra::vect(patches_final), width = 1000) %>% terra::aggregate()
plot(patches_buf1km)

# extend bbox by patch buffer
minbbox_expanded = terra::union(minbbox2000, patches_buf1km)

# crop MSPA to expanded area
r_crop = terra::crop(mspa, minbbox_expanded) %>% 
  terra::mask(minbbox_expanded)

plot(r_crop, col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))
plot(regions, add=TRUE, col="red", lwd=1)

# we compute ECA with corridors
# corridors cost = 1 (same as core)
resist1 = terra::ifel(r_crop==1, 1,
                      terra::ifel(r_crop==33,1,100))
plot(resist1, col=c("darkgreen","gray"))
plot(patches_final, add=TRUE, col="orange")
plot(regions, add=TRUE, col="red", lwy=1)

## PC with least-cost
PC_LC1 = MK_dPCIIC(nodes=patches_final,
                   distance=list(type="least-cost", resistance=resist1),
                   metric="PC",
                   probability=0.05,
                   distance_thresholds=c(2000,8000))


### function to rename PC columns
rename_pc = function(df, prefix){
  df %>% dplyr::rename_with(~ paste0(prefix, .x),
                            .cols = -lyr.1)
}

### rename each PC table

PCc2000  <- rename_pc(PC_centroid$d2000,  "PCc2000_")
PCc8000  <- rename_pc(PC_centroid$d8000,  "PCc8000_")

PCcorr1_2000 <- rename_pc(PC_LC1$d2000,     "PCcorr1_2000_")
PCcorr1_8000 <- rename_pc(PC_LC1$d8000,     "PCcorr1_8000_")

##### Final join ---------------------------

patches_metrics_final =
  patches_final %>% 
  dplyr::left_join(st_drop_geometry(patch_area), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(patch_shape), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCc2000),     by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCc8000),     by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCcorr1_2000),  by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCcorr1_8000),  by="lyr.1")
patches_metrics_final = patches_metrics_final %>% 
  dplyr::mutate(area = round(area,2))


# Quick summaries
message("Final patches in metrics table: ", nrow(patches_metrics_final))
# inspect first rows:
print(dplyr::select(patches_metrics_final, lyr.1, area, PCc8000_dPC, PCcorr1_8000_dPC) %>%  head(10))


#### Apply to all rasters ----------------
compute_patch_metrics <- function(r, year,
                                  regions_sf, regions_buffer, 
                                  minbbox2000,
                                  buffer_patch = 500){
  
  message("Processing year: ", year)
  
  # -------------- PATCH SELECTION
  
  forest_patches = landscapemetrics::get_patches(r, class = 1, directions = 8)
  patches_sf = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
  
  patch_in   = patches_sf[sf::st_intersects(patches_sf, regions_sf,     sparse=FALSE) %>% apply(1, any),]
  patch_near = patches_sf[sf::st_intersects(patches_sf, regions_buffer, sparse=FALSE) %>% apply(1, any),]
  
  patches_final = dplyr::bind_rows(patch_in, patch_near) %>% 
    dplyr::distinct(geometry, .keep_all=TRUE)
  
  # -------------- LANDSCAPE METRICS
  
  mask_rast = terra::rasterize(patches_final, r, field = 1)
  patch_lm  = landscapemetrics::spatialize_lsm(mask_rast,
                                               what = c("lsm_p_area","lsm_p_shape"),
                                               directions=8)
  
  patch_area  = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(area=value)
  patch_shape = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_shape, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(shape=value)
  
  # -------------- CONNECTIVITY
  
  PC_centroid = MK_dPCIIC(nodes = patches_final,
                          distance = list(type="centroid"),
                          metric = "PC",
                          probability = 0.05,
                          distance_thresholds = c(2000,8000))
  
  # buffer patches 1 km
  patches_buf1km = terra::buffer(terra::vect(patches_final), width=1000) |> terra::aggregate()
  
  # extend bbox
  minbbox_expanded = terra::union(minbbox2000, patches_buf1km)
  
  # crop r
  r_crop = terra::crop(r, minbbox_expanded) |> terra::mask(minbbox_expanded)
  
  resist1 = terra::ifel(r_crop==1, 1,
                        terra::ifel(r_crop==33,1,100))
  
  PC_LC1 = MK_dPCIIC(nodes=patches_final,
                     distance=list(type="least-cost", resistance=resist1),
                     metric="PC",
                     probability=0.05,
                     distance_thresholds=c(2000,8000))
  
  rename_pc = function(df, prefix){
    dplyr::rename_with(df, ~ paste0(prefix, .x), .cols = -lyr.1)
  }
  
  PCc2000        <- rename_pc(PC_centroid$d2000,    "PCc2000_")
  PCc8000        <- rename_pc(PC_centroid$d8000,    "PCc8000_")
  PCcorr1_2000   <- rename_pc(PC_LC1$d2000,         "PCcorr1_2000_")
  PCcorr1_8000   <- rename_pc(PC_LC1$d8000,         "PCcorr1_8000_")
  
  patches_metrics_final =
    patches_final %>%
    dplyr::left_join(st_drop_geometry(patch_area),     by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(patch_shape),    by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCc2000),        by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCc8000),        by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCcorr1_2000),   by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCcorr1_8000),   by="lyr.1") %>%
    dplyr::mutate(area = round(area,2), year = year)
  
  return(patches_metrics_final)
}

# Loop
regions_buffer = sf::st_buffer(regions_sf, 500)
minbbox2000    = terra::buffer(minbbox, width=2000)
all_years_metrics = purrr::map2(rasters_mspa, years,
                                ~ compute_patch_metrics(r=.x,
                                                        year=.y,
                                                        regions_sf=regions_sf,
                                                        regions_buffer=regions_buffer,
                                                        minbbox2000=minbbox2000))


### Export datasets ------
base_path = here("outputs", "data", "landscapemetrics")

# Export landscape metrics
write.csv(all_lulc_classes, file = file.path(base_path, "all_lulc_classes_bbox_1989_2024.csv"), row.names = FALSE)
write.csv(forest_class_metrics_final, file = file.path(base_path, "forest_class_metrics_bbox_1989_2024.csv"), row.names=FALSE)
write.csv(forest_core_corridor_metrics_final, file = file.path(base_path, "forest_core_corridors_metrics_bbox_1989_2024.csv"), row.names=FALSE)
write.csv(forest_cat_metrics, file = file.path(base_path, "forest_cat_metrics_bbox_1989_2024.csv"), row.names=FALSE)

# Export patch metrics
purrr::walk2(
  all_years_metrics,
  years,
  ~ {
    output_path = file.path(base_path, paste0("patches_metrics_", .y, ".gpkg"))
    sf::st_write(.x, output_path, layer = "patches", delete_dsn = TRUE, quiet = TRUE)
    message("✅ Exported: ", output_path)
  }
)
