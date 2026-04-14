#------------------------------------------------#
# Author: Romain Monassier
# Objective: Patch-scale metrics
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
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))

### Import vectors ----------
# Import roads
roads = vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))
roads_sf = sf::st_as_sf(roads)
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(roads_sf), add=TRUE, lwd=1.5)

# Import Regions
regions = terra::vect(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions = terra::project(regions, "EPSG:31983")
regions_sf = sf::st_as_sf(regions)
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(regions_sf), pch=1, col="yellow", add=TRUE)

# Import bounding box
# BBOX
bbox = terra::vect(here("data", "geo", "BBOX", "sampling_units_bbox_31983.shp"))
bbox_sf = sf::st_as_sf(bbox)
# Min bbox
minbbox = terra::vect(here("data", "geo", "BBOX", "sampling_units_minbbox_buffer5km.shp"))
minbbox_sf = sf::st_as_sf(minbbox)


### Dilatation-erosion --------
# Here, we apply dilatation-erosion on habitats (value = 1)
# This section is based on Mailys Queru's work

# dilatation_erosion_mailys <- function(raster, seuil) {
#   habitat <- app(raster, fun = function(v) ifelse(v == 1, 1, NA)) # All cells different than 1 become NA
#   dist_hab <- terra::distance(habitat)
#   dist_hab_thresh <- app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA)) # Threshold distance and set 0 to NA 
#   dist_nonhab <- terra::distance(dist_hab_thresh) 
#   dist_nonhab > seuil 
#   }

# Numeric version (applies mask to original raster)
dilatation_erosion = function(raster, seuil) { 
  # Step 1: Habitat mask 
  habitat = app(raster, fun = function(v) ifelse(v == 1, 1, NA)) 
  # Step 2: Dilation 
  dist_hab = terra::distance(habitat) 
  dilated_mask = app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA)) 
  # Step 3: Erosion 
  dist_nonhab = terra::distance(dilated_mask) 
  final_mask = app(dist_nonhab, fun = function(v) ifelse(v > seuil, 1, NA)) 
  # Step 4: Apply mask to original raster 
  raster[!is.na(final_mask)] = 1 
  
  raster 
}

#### Apply dilatation erosion ------
# Warning: choose wisely on which raster applying dilatation-erosion (with or without corridors)
message("Applying dilatation–erosion...")
rasters_dilate = lapply(seq_along(rasters_mspa), function(i) {
  message("  - Processing dilatation–erosion for raster ", i, " (year ", years[i], ")")
  dilatation_erosion(rasters_mspa[[i]], seuil = 30)
})

# Quick check
plot(rasters_mspa[[36]], main=paste0("Before dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e","orange"))
plot(rasters_dilate[[36]], main=paste0("After dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e","orange"))
freq(rasters_mspa[[36]])
freq(rasters_dilate[[36]])

### Overlay linear features -----
# Here, we overlay vector linear features to the rasters using a buffer width and assign the intersected cells a new numeric value
# -> Adapt the buffer width and value according to the linear feature (for instance: roads = 15m)
apply_linear_feature_single = function(r, yr, feature, buffer_width, value, use_date) {
  feat_valid = if (use_date && "date_crea" %in% names(feature)) {
    feature[feature$date_crea <= yr, ]
  } else feature
  
  if (nrow(feat_valid) > 0) {
    feat_buff = terra::buffer(feat_valid, width = buffer_width)
    feat_rast = terra::rasterize(feat_buff, r, field = value, background = NA)
    r = cover(feat_rast, r)
  }
  
  return(r)  # single SpatRaster
}

##### Step 6 – Apply linear features on top of raster layers -----
# First, we distinguish roads based on their nature (primary, secondary, tertiary)
roads_sf %>% sf::st_drop_geometry() %>%  dplyr::select(highway) %>% dplyr::group_by(highway) %>% dplyr::summarise(n=n())
highway = roads_sf %>% dplyr::filter(highway == "motorway" | highway == "trunk" | highway == "primary") %>% terra::vect()
plot(highway)

# Second, we apply linear features
# Main roads take the value "artificial" (6)
# WARNING: at this resolution (30 x 30 m), a minimum of 20 m is needed for linear features to create continuous linear features (below, some cells are not overwritten, and some raster cells are still connected through their vertices)
rasters_lf = purrr::map2(rasters_dilate, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin = r
  # r_lin = apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 20, value = 2, use_date = TRUE)
  r_lin = apply_linear_feature_single(r_lin, yr, highway, buffer_width = 20, value = 6, use_date = TRUE)
  r_lin
})


### Compute patch-level metrics   ---------


#### On a single raster -----
##### Patch selection (core only) ----------

# 1) patches = core forest only, class = 1 (i.e., 2 fragments connected by a corridor are considered as 2 patches)
# On raw rasters (without dilatation-erosion)
forest_patches = landscapemetrics::get_patches(rasters_mspa[[36]], class = 1, directions = 8)
patches_sf = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
# On rasters with dilatation-erosion
forest_patches_dl = landscapemetrics::get_patches(rasters_dilate[[36]], class = 1, directions = 8)
patches_sf_dl = sf::st_as_sf(as.polygons(forest_patches_dl[[1]][[1]], dissolve = TRUE))
# Compare counts
patches_sf %>% sf::st_drop_geometry() %>% dplyr::count()
patches_sf_dl %>% sf::st_drop_geometry() %>% dplyr::count()

# visualize
plot(rasters_dilate[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(patches_sf_dl), col="darkgreen", add=TRUE)

# 2) patches intersecting GLT regions
patch_in = patches_sf_dl[sf::st_intersects(patches_sf_dl, regions_sf, sparse=FALSE) %>%  apply(1, any),]

# 3) patches near GLT regions
regions_buffer = sf::st_buffer(regions_sf, 500)
patch_near = patches_sf_dl[sf::st_intersects(patches_sf_dl, regions_buffer, sparse=FALSE) %>%  apply(1, any),]

# 4) final kept patches = in OR near
patches_final = dplyr::bind_rows(patch_in, patch_near) %>% dplyr::distinct(geometry, .keep_all=TRUE)

# Plot
plot(rasters_dilate[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(st_geometry(patches_sf_dl), col="grey80", border=NA, add=TRUE) # Patches (without borders)
plot(st_geometry(patches_final), add=TRUE, col="darkgreen", border=NA) # Kept patches
plot(st_geometry(regions_sf), add=TRUE, col="yellow", lwd=2) # GLT groups

##### Landscapemetrics ----------------------------------------------

# Create a forest mask using patches
mask_rast = terra::rasterize(patches_final, rasters_mspa[[36]], field = 1)
plot(mask_rast, col = "#32a65e")

# Compute spatial metrics for patches
patch_lm = landscapemetrics::spatialize_lsm(
  mask_rast,
  what = c("lsm_p_area"),
  directions=8
)

patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% 
  dplyr::rename(area=value)

##### Connectivity (Makurhini) --------------------------------
## PC with centroid distance
PC_centroid = Makurhini::MK_dPCIIC(nodes = patches_final,
                        distance = list(type="centroid"),
                        metric="PC",
                        probability=0.05,
                        distance_thresholds=2000)

## 2) PC with resistance rasters
## resistance raster = rasters with forest core and corridors
mspa = rasters_mspa[[36]]

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

plot(r_crop, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "yellow"))
plot(regions, add=TRUE, col="red", lwd=1)

# Resistance matrix
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
                   distance_thresholds=2000)


### function to rename PC columns
# Add prefix
rename_pc = function(df, prefix){
  df %>% dplyr::rename_with(~ paste0(prefix, .x),
                            .cols = -lyr.1)
}

### rename each PC table
PCc2000 = rename_pc(PC_centroid, "PCc2km_")
PCrm2000 = rename_pc(PC_LC1, "PCrm2km_")


##### Final join ---------------------------

patches_metrics_final =
  patches_final %>% 
  dplyr::left_join(st_drop_geometry(patch_area), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCc2000), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCrm2000), by="lyr.1")
patches_metrics_final = patches_metrics_final %>% 
  dplyr::mutate(area = round(area,2))


#### Apply to all rasters ----------------

##### Total version -----
# Patch area, connectivity (centroid + resistance-based)
compute_patch_metrics_tot = function(r, 
                                 year,
                                 sample_loc, 
                                 buf_around_loc, 
                                 minbbox,
                                 buffer_patch){
  
  message("Processing year: ", year)
  
  # PATCH SELECTION
  
  forest_patches = landscapemetrics::get_patches(r, class = 1, directions = 8)
  patches_sf = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
  
  patch_in = patches_sf[sf::st_intersects(patches_sf, sample_loc, sparse=FALSE) %>% apply(1, any),]
  patch_near = patches_sf[sf::st_intersects(patches_sf, buf_around_loc, sparse=FALSE) %>% apply(1, any),]
  
  patches_final = dplyr::bind_rows(patch_in, patch_near) %>% 
    dplyr::distinct(geometry, .keep_all=TRUE)
  
  # LANDSCAPE METRICS
  mask_rast = terra::rasterize(patches_final, r, field = 1)
  patch_lm = landscapemetrics::spatialize_lsm(mask_rast,
                                              what = c("lsm_p_area"),
                                              directions=8)
  
  patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(area=value)

  # CONNECTIVITY
  PC_centroid = MK_dPCIIC(nodes = patches_final,
                          distance = list(type="centroid"),
                          metric = "PC",
                          probability = 0.05,
                          distance_thresholds = 2000)
  
  # buffer patches 1 km
  patches_buf1km = terra::buffer(terra::vect(patches_final), width=1000) %>% terra::aggregate()
  
  # extend bbox
  minbbox_expanded = terra::union(minbbox, patches_buf1km)
  
  # crop r
  r_crop = terra::crop(r, minbbox_expanded) %>% terra::mask(minbbox_expanded)
  
  resist1 = terra::ifel(r_crop==1, 1,
                        terra::ifel(r_crop==33,1,100))
  
  PC_LC1 = MK_dPCIIC(nodes=patches_final,
                     distance=list(type="least-cost", resistance=resist1),
                     metric="PC",
                     probability=0.05,
                     distance_thresholds=2000)
  
  PCc2000 = rename_pc(PC_centroid, "PCc2km_")
  PCrm2000 = rename_pc(PC_LC1, "PCrm2km_")

  patches_metrics_final =
    patches_final %>%
    dplyr::left_join(st_drop_geometry(patch_area), by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCc2000), by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCrm2000), by="lyr.1") %>% 
    dplyr::mutate(area = round(area,2), year = year)
  
  return(patches_metrics_final)
}

# Parameters
regions_buffer = sf::st_buffer(regions_sf, 500)
minbbox2000 = terra::buffer(minbbox, width=2000)
# Loop
all_years_metrics = purrr::map2(rasters_dilate, years,
                                ~ compute_patch_metrics_tot(r = .x,
                                                        year = .y,
                                                        sample_loc = regions_sf,
                                                        buf_around_loc = regions_buffer,
                                                        minbbox = minbbox2000,
                                                        buffer_patch = 500))

##### Simplified version ----------
# Patch area, connectivity (centroid-based only)
compute_patch_metrics_simple = function(r, 
                                        year,
                                        sample_loc,
                                        buf_around_loc){
  
  message("Processing year: ", year)
  
  # PATCH SELECTION
  
  forest_patches = landscapemetrics::get_patches(r, class = 1, directions = 8)
  patches_sf = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
  
  patch_in = patches_sf[sf::st_intersects(patches_sf, sample_loc, sparse=FALSE) %>% apply(1, any),]
  patch_near = patches_sf[sf::st_intersects(patches_sf, buf_around_loc, sparse=FALSE) %>% apply(1, any),]
  
  patches_final = dplyr::bind_rows(patch_in, patch_near) %>% 
    dplyr::distinct(geometry, .keep_all=TRUE)
  
  # LANDSCAPE METRICS
  mask_rast = terra::rasterize(patches_final, r, field = 1)
  patch_lm = landscapemetrics::spatialize_lsm(mask_rast,
                                              what = c("lsm_p_area"),
                                              directions=8)
  
  patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(area=value)
  
  # CONNECTIVITY
  PC_centroid = MK_dPCIIC(nodes = patches_final,
                          distance = list(type="centroid"),
                          metric = "PC",
                          probability = 0.05,
                          distance_thresholds = 2000)
  
  PCc2000 = rename_pc(PC_centroid, "PCc2km_")
  
  patches_metrics_final =
    patches_final %>%
    dplyr::left_join(sf::st_drop_geometry(patch_area), by="lyr.1") %>%
    dplyr::left_join(sf::st_drop_geometry(PCc2000), by="lyr.1") %>%
    dplyr::mutate(area = round(area,2), year = year)
  
  return(patches_metrics_final)
}

# Parameters
regions_buffer = sf::st_buffer(regions_sf, 500)
# Loop
all_years_metrics = purrr::map2(rasters_dilate, years,
                                ~ compute_patch_metrics_simple(r = .x,
                                                               year = .y,
                                                               sample_loc = regions_sf,
                                                               buf_around_loc = regions_buffer))

##### Only landscape metrics ----------
# Patch metrics using landscape metrics only
## Area
compute_patch_metrics_only = function(r,
                                      year){
  
  message("Processing year: ", year)
  
  # PATCH SELECTION
  
  forest_patches = landscapemetrics::get_patches(r, class = 1, directions = 8)
  patches_final = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
  
  # LANDSCAPE METRICS
  mask_rast = terra::rasterize(patches_final, r, field = 1)
  patch_lm = landscapemetrics::spatialize_lsm(mask_rast,
                                              what = c("lsm_p_area"),
                                              directions=8)
  
  patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(area=value)

  patches_metrics_final =
    patches_final %>%
    dplyr::left_join(sf::st_drop_geometry(patch_area), by="lyr.1") %>%
    dplyr::mutate(area = round(area,2), 
                  year = year)
  
  return(patches_metrics_final)
}

## Area and ENN
compute_patch_metrics_only = function(r,
                                      year){
  
  message("Processing year: ", year)
  
  # PATCH SELECTION
  
  forest_patches = landscapemetrics::get_patches(r, class = 1, directions = 8)
  patches_final = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
  
  # LANDSCAPE METRICS
  mask_rast = terra::rasterize(patches_final, r, field = 1)
  patch_lm = landscapemetrics::spatialize_lsm(mask_rast,
                                              what = c("lsm_p_area","lsm_p_enn"),
                                              directions=8)
  
  patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(area=value)
  patch_enn = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_enn, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(enn=value)
  
  patches_metrics_final =
    patches_final %>%
    dplyr::left_join(sf::st_drop_geometry(patch_area), by="lyr.1") %>%
    dplyr::left_join(sf::st_drop_geometry(patch_enn), by="lyr.1") %>%
    dplyr::mutate(area = round(area,2), 
                  year = year)
  
  return(patches_metrics_final)
}

# Loop
all_years_metrics_simple = purrr::map2(rasters_lf, years,
                                ~ compute_patch_metrics_only(r = .x,
                                                             year = .y
                                                             ))

# Export patch metrics
base_path = here("outputs", "data", "patchmetrics")
purrr::walk2(
  all_years_metrics_simple,
  years,
  ~ {
    output_path = file.path(base_path, paste0("patches_metrics_area_", .y, ".gpkg"))
    sf::st_write(.x, output_path, layer = "patches", append=FALSE, delete_dsn = TRUE, quiet = TRUE)
    message("Exported: ", output_path)
  }
)
