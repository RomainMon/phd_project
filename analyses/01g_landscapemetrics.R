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
regions = terra::vect(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions = terra::project(regions, "EPSG:31983")

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

## Compute ECA
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

### Prepare tables -----------
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


### Transition matrix -----
#### Forest trajectories (rasters) -------
# Here, we identify forest trajectories on rasters
# Codes:
# 1 = intact forest
# 2 = deforested
# 3 = reforested

# First year: intact forest
traj = rasters_merged[[1]]
traj[traj != 1] = NA  # non-forest is NA for now

# Trajectories
trajectories = list()
trajectories[[1]] = traj
prev_r = rasters_merged[[1]]

# Loop from year 2 onward
for (i in 2:length(rasters_merged)) {
  curr_r = rasters_merged[[i]]
  
  # Update intact forest: still forest and was intact before
  intact = (prev_r == 1 & curr_r == 1) & (traj == 1)
  
  # Update deforested: forest lost from intact forest
  deforest = (prev_r == 1 & curr_r != 1) & (traj == 1)
  
  # Update reforested: was non-forest and now forest (any previous non-forest)
  reforest = (curr_r == 1) & (is.na(traj) | traj != 1)
  
  # Update trajectory raster
  traj[intact] = 1
  traj[deforest] = 2
  traj[reforest] = 3
  
  trajectories[[i]] = traj
  prev_r = curr_r
}
names(trajectories) = years

# Plot
cols = c("darkgreen", "red", "blue")  # intact, deforestation, reforestation
plot(trajectories[[2]], col=cols, legend=TRUE, main=paste0("Forest trajectory ", years[2]))
plot(trajectories[[3]], col=cols, legend=TRUE, main=paste0("Forest trajectory ", years[3]))
plot(trajectories[[4]], col=cols, legend=TRUE, main=paste0("Forest trajectory ", years[4]))
plot(trajectories[[35]], col=cols, legend=TRUE, main=paste0("Forest trajectory ", years[35]))

# Here, we compute a transition matrix on the rasters to identify the changes in land use across the landscape and through time
#### Transition matrix on all rasters (year-to-year changes) ----
# transition_matrix = lapply(1:(length(rasters_merged) - 1), function(i) {
#   # Display progress message
#   message("Computing transition matrix: ", years[i], " → ", years[i + 1])
#   # Compute crosstab between consecutive years
#   t = terra::crosstab(c(rasters_merged[[i]], rasters_merged[[i + 1]]))
#   # Return list with metadata and table
#   list(year_from = years[i], year_to = years[i + 1], table = t)
# })
# transition_matrix[[1]]

#### Transition matrix for selected years -------
selected_years = c(1989, 2000, 2012, 2023)
selected_idx = match(selected_years, years)

# Labels
cat_labels = c(
  "1" = "intact forest",
  "2" = "deforested",
  "3" = "reforested",
  "0" = "matrix"
)

# Initialize list for results
df_list = list()

# Baseline (1989)
r0 = trajectories[[selected_idx[1]]]
r0_filled = classify(r0, rcl = matrix(c(NA, NA, 0), ncol = 3, byrow = TRUE))

vals0 = values(r0_filled, mat = FALSE)
tbl0 = table(factor(vals0, levels = c(0, 1, 2, 3)))

df_baseline = data.frame(
  from = 1989,
  to = 1989,
  category = cat_labels[names(tbl0)],
  n = as.numeric(tbl0)
)

df_list[[1]] = df_baseline

# Subsequent intervals
for (i in 2:length(selected_idx)) {
  from_year = selected_years[i - 1]
  to_year = selected_years[i]
  
  r = trajectories[[selected_idx[i]]]  # cumulative raster of "to" year
  
  # Replace NA by 0 for matrix
  r_filled = classify(r, rcl = matrix(c(NA, NA, 0), ncol = 3, byrow = TRUE))
  
  # Count pixels per category
  vals = values(r_filled, mat = FALSE)
  tbl = table(factor(vals, levels = c(0, 1, 2, 3)))
  
  df = data.frame(
    from = from_year,
    to = to_year,
    category = cat_labels[names(tbl)],
    n = as.numeric(tbl)
  )
  
  df_list[[i]] = df
}

# Combine all into one data frame
tm_1989_2023 = bind_rows(df_list)

# Compute area
pixel_area_ha = prod(res(rasters_merged[[1]])) / 10000
total_area_ha = ncell(rasters_merged[[1]]) * pixel_area_ha
tm_1989_2023 = tm_1989_2023 %>%
  dplyr::mutate(area_ha = n * pixel_area_ha) %>% 
  dplyr::mutate(area_perc = area_ha * 100 / total_area_ha)



# ### Compute patch-level metrics   ---------
# # Function to compute patch metrics on filtered patches (intersecting the entities in "vect")
# compute_patch_metrics_filtered = function(raster, class_value, metrics, vect) {
#   # Extract patches for the specified class
#   patches = landscapemetrics::get_patches(raster, class = class_value, directions = 8)
#   patches_rast = patches[[1]][[1]] # layer_1$class_X
#   
#   # Identify which patch IDs intersect the vector file
#   patch_ids = unique(terra::extract(patches_rast, vect)[, 2])
#   patch_ids = patch_ids[!is.na(patch_ids)]
#   
#   # Keep full patches that intersect
#   patches_keep = patches_rast
#   patches_keep[!patches_keep %in% patch_ids] = NA
#   
#   # Compute patch-level metrics
#   metrics_result = landscapemetrics::spatialize_lsm(
#     landscape = patches_keep,
#     directions = 8,
#     what = metrics
#   )
#   
#   return(metrics_result)
# }
# 
# #### On core forest -------
# list_patch_metrics = purrr::imap(rasters, ~{
#   message("Processing raster: ", years[.y])
#   compute_patch_metrics_filtered(
#     raster = .x,
#     class_value = 1,
#     metrics = c("lsm_p_area", "lsm_p_shape"),
#     vect = regions
#   )
# })
# 
# # Assign names for clarity
# names(list_patch_metrics) = years


### Export datasets ------
base_path = here("outputs", "data", "landscapemetrics")
write.csv(class_metrics, file = file.path(base_path, "class_metrics_bbox_1989_2023.csv"), row.names = FALSE)
write.csv(forest_class_metrics_final, file = file.path(base_path, "forest_class_metrics_bbox_1989_2023.csv"), row.names=FALSE)
write.csv(forest_core_corridor_metrics_final, file = file.path(base_path, "forest_core_corridors_metrics_bbox_1989_2023.csv"), row.names=FALSE)
write.csv(tm_1989_2023, file = file.path(base_path, "transition_matrix_bbox_1989_2023.csv"), row.names=FALSE)
