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
library(vectormetrics)
library(progressr)
library(RColorBrewer)
# remotes::install_github("r-spatialecology/vectormetrics")

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

# Import UMMPs
ummps = sf::st_read(here("data", "geo", "AMLD", "SIG-LGCI_UMMP-13", "SIG-LGCI_UMMP-13.shp"))
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(ummps), col="yellow", add=TRUE)

# Import census
census = terra::vect(here("data", "glt", "JDietz", "glt_distrib_2013_2018_2022.shp"))
census = terra::project(census, "EPSG:31983")
census_sf = sf::st_as_sf(census)
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(census_sf), pch=1, col="coral", add=TRUE)

# Occupied patches (following census: Ruiz-Miranda et al. 2019, Dietz et al. 2024)
census_occ = census_sf %>%
  dplyr::filter(
    Detect2013 == "P" |
      Detect2018 == "P" |
      Detect2022 == "P"
  )
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(census_sf), pch=1, col="coral", add=TRUE)
plot(sf::st_geometry(census_occ), pch=1, col="darkslategray1", add=TRUE)

# Import pipelines
pipelines = terra::vect(here("data", "geo", "OSM", "work", "Pipelines_OSM_clean.shp"))
pipelines_sf = sf::st_as_sf(pipelines)
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(pipelines_sf), add=TRUE, lwd=1.5)

# ### Dilatation-erosion --------
# # Here, we apply dilatation-erosion on habitats (value = 1)
# # This section is based on Mailys Queru's work
# 
# # dilatation_erosion_mailys <- function(raster, seuil) {
# #   habitat <- app(raster, fun = function(v) ifelse(v == 1, 1, NA)) # All cells different than 1 become NA
# #   dist_hab <- terra::distance(habitat)
# #   dist_hab_thresh <- app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA)) # Threshold distance and set 0 to NA 
# #   dist_nonhab <- terra::distance(dist_hab_thresh) 
# #   dist_nonhab > seuil 
# #   }
# 
# # Numeric version (applies mask to original raster)
# dilatation_erosion = function(raster, seuil) { 
#   # Step 1: Habitat mask 
#   habitat = app(raster, fun = function(v) ifelse(v == 1, 1, NA)) 
#   # Step 2: Dilation 
#   dist_hab = terra::distance(habitat) 
#   dilated_mask = app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA)) 
#   # Step 3: Erosion 
#   dist_nonhab = terra::distance(dilated_mask) 
#   final_mask = app(dist_nonhab, fun = function(v) ifelse(v > seuil, 1, NA)) 
#   # Step 4: Apply mask to original raster 
#   raster[!is.na(final_mask)] = 1 
#   
#   raster 
# }
# 

# # Warning: choose wisely on which raster applying dilatation-erosion (with or without corridors)
# message("Applying dilatation–erosion...")
# rasters_dilate = lapply(seq_along(rasters_mspa), function(i) {
#   message("  - Processing dilatation–erosion for raster ", i, " (year ", years[i], ")")
#   dilatation_erosion(rasters_mspa[[i]], seuil = 30)
# })
# 
# # Quick check
# plot(rasters_mspa[[36]], main=paste0("Before dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e","orange"))
# plot(rasters_dilate[[36]], main=paste0("After dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e","orange"))
# freq(rasters_mspa[[36]])
# freq(rasters_dilate[[36]])

### 1a) Vector metrics on REAL patches -----
# Here, we work with 'real' patches (i.e., fragments either isolated or connected by a narrow corridor)

#### Overlay linear features -----
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

# First, we distinguish roads based on their nature (primary, secondary, tertiary)
roads_sf %>% sf::st_drop_geometry() %>%  dplyr::select(highway) %>% dplyr::group_by(highway) %>% dplyr::summarise(n=n())
highway = roads_sf %>% dplyr::filter(highway == "motorway" | highway == "trunk" | highway == "primary") %>% terra::vect()
plot(highway)

# Second, we apply linear features
# Main roads take the value "artificial" (6)
# WARNING: at this resolution (30 x 30 m), a minimum of 20 m is needed for linear features to create continuous linear features (below, some cells are not overwritten, and some raster cells are still connected through their vertices)
# With MSPA rasters to distinguish patches connected by forest corridors
rasters_lf = purrr::map2(rasters_mspa, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin = r
  r_lin = apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 20, value = 4, use_date = TRUE)
  r_lin = apply_linear_feature_single(r_lin, yr, highway, buffer_width = 20, value = 6, use_date = TRUE)
  r_lin
})

#### Get patches ------

# patches = core forest only, class = 1 (i.e., 2 fragments connected by a corridor are considered as 2 patches)
forest_patches = purrr::map(rasters_lf, ~ landscapemetrics::get_patches(.x, class = 1, directions = 8))
patches_sf = purrr::map(forest_patches, ~ sf::st_as_sf(terra::as.polygons(.x[[1]][[1]], dissolve = TRUE)))

# visualize
plot(rasters_lf[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(patches_sf[[36]]), col="darkgreen", add=TRUE)

#### Clip patches with UMMPs ------
# We want to split some patches that intersect several UMMPs
# Select patches intersecting several UMMPs
# Clip to create multipolygons

aldeia = ummps %>% dplyr::filter(UMMPs == "Aldeia I")
pirineus = ummps %>% dplyr::filter(UMMPs == "Pirineus")

split_patch = function(patches, aldeia, pirineus) {
  
  ranges = sf::st_union(aldeia, pirineus)
  
  idx = which(
    lengths(sf::st_intersects(patches, aldeia)) > 0 &
      lengths(sf::st_intersects(patches, pirineus)) > 0
  )
  
  if (length(idx) != 1)
    stop("Expected exactly one patch intersecting both ranges")
  
  target_patch = patches[idx, ]
  
  patch_aldeia = sf::st_intersection(target_patch, aldeia) %>% 
    dplyr::mutate(part = "aldeia")
  
  patch_pirineus = sf::st_intersection(target_patch, pirineus) %>% 
    dplyr::mutate(part = "pirineus")
  
  patch_rest = sf::st_difference(target_patch, ranges) %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(part = "rest")
  
  dplyr::bind_rows(
    patches[-idx, ],   # keep all other patches
    patch_aldeia,
    patch_pirineus,
    patch_rest
  )
}

patches_split = purrr::map(
  patches_sf,
  split_patch,
  aldeia = aldeia,
  pirineus = pirineus)

# Check
plot(sf::st_geometry(patches_split[[36]]),
     col = ifelse(
       is.na(patches_split[[36]]$part),
       "darkgreen",
       c(aldeia = "red",
         pirineus = "blue",
         rest = "grey")[patches_split[[36]]$part]
     ))

# Remove columns
patches_split = purrr::map(
  patches_split,
  ~ dplyr::select(.x, lyr.1))

#### Patch name --------
# Below, we assign patch id based on group location names (RegionsNames)
# AND on UMMPs names
# Step 1. Assign region-based names:
# - Each patch is first checked for spatial intersection with the regions_sf layer.
# - If a patch intersects a region, it inherits the region name (FragName2) from the first matching region.
# - This becomes the initial patch_id.
# Step 2. Assign UMMP-based names to unnamed patches
# - Patches that did not receive a region name (patch_id = NA) are then processed:
# - They are spatially joined with ummps
# - If a patch intersects a UMMP: It is grouped by UMMP; Ordered by decreasing area; Assigned a name like UMMP_name_p1, UMMP_name_p2, etc.
# - If a patch does not intersect any UMMP: It is assigned a simple sequential ID (p1, p2, …) based on area ranking
# Step 3. Ensure uniqueness of patch IDs
# If a name appears multiple times (e.g., Aldeia I), a suffix (_1, _2, …) is appended within each group using row order

regions_names = regions_sf %>%
  dplyr::select(FragName2)
# Run patch size
patches_split = purrr::map(
  patches_split,
  ~ dplyr::mutate(.x, area_ha = as.numeric(sf::st_area(.x)) / 10000)
)
# Assign names
patches_names = purrr::map(
  patches_split,
  function(x){
    
    # 1. Assign Region names
    idx = sf::st_intersects(x, regions_names)
    
    x$patch_id = purrr::map_chr(
      idx,
      function(i){
        
        if(length(i) == 0){
          return(NA_character_)
        }
        
        regions_names$FragName2[i[1]]
        
      }
    )
    
    # 2. Assign UMMP names to remaining patches
    unnamed = x %>%
      dplyr::filter(is.na(patch_id))
    
    if(nrow(unnamed) > 0){
      
      unnamed = sf::st_join(
        unnamed,
        ummps %>% dplyr::select(UMMPs),
        largest = TRUE
      )
      
      # Patches intersecting a UMMP
      inside_ummp = unnamed %>%
        dplyr::filter(!is.na(UMMPs)) %>%
        dplyr::group_by(UMMPs) %>%
        dplyr::arrange(
          dplyr::desc(area_ha),
          .by_group = TRUE
        ) %>%
        dplyr::mutate(
          patch_id = paste0(
            make.names(UMMPs),
            "_p",
            dplyr::row_number()
          )
        ) %>%
        dplyr::ungroup()
      
      # Patches outside all UMMPs
      outside_ummp = unnamed %>%
        dplyr::filter(is.na(UMMPs)) %>%
        dplyr::arrange(
          dplyr::desc(area_ha)
        ) %>%
        dplyr::mutate(
          patch_id = paste0(
            "p",
            dplyr::row_number()
          )
        )
      
      unnamed = dplyr::bind_rows(
        inside_ummp,
        outside_ummp
      )
      
      x = dplyr::bind_rows(
        x %>% dplyr::filter(!is.na(patch_id)),
        unnamed %>% dplyr::select(names(x))
      )
      
    }
    
    # 3. Ensure uniqueness of patch_id within year
    x = x %>%
      dplyr::mutate(
        base_id = patch_id %>%
          stringr::str_replace("_p\\d+$", "") %>%   # optional cleanup from UMMP step
          stringr::str_replace("_\\d+$", "")
      ) %>%
      dplyr::group_by(base_id) %>%
      dplyr::arrange(area_ha, .by_group = TRUE) %>% 
      dplyr::mutate(
        patch_id = dplyr::case_when(
          is.na(base_id) ~ NA_character_,
          n() == 1 ~ base_id,
          TRUE ~ paste0(base_id, "_", dplyr::row_number())
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(base_id,area_ha))
    
    x
    
  }
)

## Checks
# Check how many times a given name appears
purrr::map2_dfr(
  patches_names,
  years,
  ~ .x %>%
    sf::st_drop_geometry() %>%
    dplyr::select(patch_id) %>%
    dplyr::mutate(year = .y)
) %>%
  dplyr::count(patch_id) %>% 
  dplyr::arrange(dplyr::desc(n))

# Uniqueness across years (should be 0)
purrr::map2_dfr(
  patches_names,
  years,
  ~ .x %>%
    sf::st_drop_geometry() %>%
    dplyr::count(patch_id) %>%
    dplyr::filter(n > 1) %>%
    dplyr::mutate(year = .y)
)

## Remove accents
patches_names = purrr::map(
  patches_names,
  ~ .x %>%
    dplyr::mutate(
      patch_id = iconv(
        patch_id,
        from = "UTF-8",
        to = "ASCII//TRANSLIT"
      )
    )
)

#### Patch dilatation-erosion -------
# Dilatation-erosion on all the raster (as above) may result in connecting close but disconnected patches (see: Boa Esperanca, two close patches but actually connected by a CORRIDOR)
# Hence, we apply dilatation-erosion afterwards
# E.g., without dilatation-erosion, Afetiva remains disconnected from its corridor...
targets = c("Afetiva", "Rio_Vermelho")

patches_dilat = purrr::map(
  patches_names,
  ~ {
    
    x = .x
    
    ## 1. Select target patches
    target = x %>%
      dplyr::filter(patch_id %in% targets) %>%
      sf::st_buffer(50)
    
    if (nrow(target) == 0) return(x)
    
    ## 2. Find all patches touching ANY target
    touching = x[
      sf::st_intersects(x, target, sparse = FALSE)[, 1],
    ]
    
    ## 3. Merge them all into one geometry
    merged_geom = touching %>%
      sf::st_union() %>%
      sf::st_as_sf()
    
    ## 4. Keep attributes from first target (or customise)
    merged_patch = target[1, ]
    sf::st_geometry(merged_patch) = sf::st_geometry(merged_geom)
    
    ## 5. Remove original affected patches
    remaining = x[
      !sf::st_intersects(x, target, sparse = FALSE)[, 1],
    ]
    
    ## 6. Recombine
    dplyr::bind_rows(remaining, merged_patch) %>%
      sf::st_make_valid()
  }
)

# visualize
plot(rasters_lf[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(patches_dilat[[36]]), col="darkgreen", add=TRUE)

#### Export patches ----------
base_path = here("outputs", "data", "patches")
purrr::walk2(
  patches_dilat,
  years,
  ~ {
    output_path = file.path(base_path, paste0("patches_", .y, ".gpkg"))
    sf::st_write(.x, output_path, layer = "patches", append=FALSE, delete_dsn = TRUE, quiet = TRUE)
    message("Exported: ", output_path)
  }
)


### 1b) - Vector metrics on UMMPs patches -----
# Here, we work with patches as sets of patches within a UMMP (i.e., isolated populations)

#### Overlay linear features -----
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

# First, we distinguish roads based on their nature (primary, secondary, tertiary)
roads_sf %>% sf::st_drop_geometry() %>%  dplyr::select(highway) %>% dplyr::group_by(highway) %>% dplyr::summarise(n=n())
highway = roads_sf %>% dplyr::filter(highway == "motorway" | highway == "trunk" | highway == "primary") %>% terra::vect()
plot(highway)

# Second, we apply linear features
# Main roads take the value "artificial" (6)
# WARNING: at this resolution (30 x 30 m), a minimum of 20 m is needed for linear features to create continuous linear features (below, some cells are not overwritten, and some raster cells are still connected through their vertices)
rasters_lf = purrr::map2(rasters_reclass, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin = r
  r_lin = apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 20, value = 4, use_date = TRUE)
  r_lin = apply_linear_feature_single(r_lin, yr, highway, buffer_width = 20, value = 6, use_date = TRUE)
  r_lin
})

#### Get patches ------
# patches = core forest only, class = 1
forest_patches = purrr::map(rasters_lf, ~ landscapemetrics::get_patches(.x, class = 1, directions = 8))
patches_sf = purrr::map(forest_patches, ~ sf::st_as_sf(terra::as.polygons(.x[[1]][[1]], dissolve = TRUE)))

# visualize
plot(rasters_lf[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(patches_sf[[36]]), col="darkgreen", add=TRUE)

#### Dissolve by UMMP ------
# Clip forest patches by UMMPs
# Dissolve all intersecting patches inside each UMMP
# Assign isolated pieces (clipped by UMMPs) to their patches
# Result: 1 (or more) polygons per UMMP representing “core managed patch”

# First, disjoint UMMPs (some are overlapping)
# i.e., cut so that resulting output = patches without overlaps
ummp_split = sf::st_intersection(ummps) %>% dplyr::filter(n.overlaps < 2)

# Check
pt = st_sfc(
  sf::st_point(c(758881.6, 7499332.7)),
  crs = st_crs(ummp_split)
)
buf = st_buffer(pt, dist = 500)
r_crop = crop(rasters_reclass[[36]], buf)
plot(r_crop, col = c("#32a65e", "#FFFFB2"))
plot(st_geometry(ummp_split), col = adjustcolor("grey", alpha.f = 0.6), add = TRUE)
plot(pt, col = "red", pch = 20, add = TRUE)

# Clip with UMMPs
with_progress({
  
  p = progressr::progressor(steps = length(patches_sf))
  
  patches_ummps = lapply(seq_along(patches_sf), function(i) {
    
    p()  # <- updates progress
    
    patches = sf::st_make_valid(patches_sf[[i]])
    
    # 1. Clip patches by UMMPs
    
    inside = sf::st_intersection(patches, ummp_split)
    
    # 2. Dissolve by UMMP
    
    inside_dissolved = inside %>%
      dplyr::group_by(UMMPs) %>%
      dplyr::summarise(
        geometry = sf::st_union(geometry),
        .groups = "drop")
    
    # 3. Outside fragments
    
    outside = sf::st_difference(patches, sf::st_union(ummp_split))
    outside$UMMPs = NA_character_ # Put NA in UMMPs column
    
    # 4. Reassign fragments based on topology AND area
    # touching exactly one UMMP
    # AND sufficiently small
    
    if (nrow(outside) > 0 && nrow(inside_dissolved) > 0) {
      
      # Intersections between patches outside UMMPs and inside UMMPs
      rel = sf::st_intersects(outside, inside_dissolved, sparse = TRUE)
      
      single_match = lengths(rel) == 1
      
      # If outside fragments touch ONE UMMP ONLY
      if (any(single_match)) {
        
        # Area of outside fragments
        outside_area = as.numeric(sf::st_area(outside[single_match, ])/10000)
        
        # Index of touched UMMP patch
        touched_idx = vapply(rel[single_match], function(x) x[1], integer(1))
        
        # Size criterion
        small_enough = outside_area < 500 # Fragment size must be less than X ha
        
        rows_to_assign = which(single_match)[small_enough]
        
        outside$UMMPs[rows_to_assign] = inside_dissolved$UMMPs[touched_idx[small_enough]]
      }
    }
    
    # 5. Merge everything
    
    combined = dplyr::bind_rows(inside_dissolved, outside)
    
    # 6. Final dissolve
    # only for assigned UMMPs (keeping other fragments as several unities)
    
    assigned = combined %>%
      dplyr::filter(!is.na(UMMPs)) %>%
      dplyr::group_by(UMMPs) %>%
      dplyr::summarise(
        geometry = sf::st_union(geometry),
        .groups = "drop")
    
    unassigned = combined %>%
      dplyr::filter(is.na(UMMPs))
    
    combined_final = dplyr::bind_rows(assigned, unassigned)
    
    combined_final
    
  })
  
})

### Verification
# Extract combined result
x = patches_ummps[[36]]

# Split into inside vs outside
inside = x[!is.na(x$UMMPs), ]
outside = x[is.na(x$UMMPs), ]

# Create consistent color palette for UMMPs
ummp_ids = sort(unique(inside$UMMPs))
ummp_cols = setNames(rainbow(length(ummp_ids), alpha = 0.7),
                     ummp_ids)

# Plot base raster
plot(rasters_lf[[36]],col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2","#0000FF", "#d4271e"))

# 1. Plot outside fragments in gray
plot(st_geometry(outside),add = TRUE,col = "grey80",border = "grey50",lwd = 0.5)

# 2. Plot UMMP fragments colored by UMMP
plot(st_geometry(inside),add = TRUE,col = ummp_cols[inside$UMMPs],border = "black",lwd = 0.5)



#### Select relevant patches -----
# to complete...



# 1) Keep patches occupied by GLTs (regions, census) OR intersecting UMMPs
# Filter
patch_in = purrr::map(
  patches_dilat,
  ~ {
    in_region = sf::st_intersects(.x, regions_sf, sparse = FALSE) %>%
      apply(1, any)
    
    in_ummp = sf::st_intersects(.x, ummps, sparse = FALSE) %>%
      apply(1, any)
    
    in_census = sf::st_intersects(.x, census_occ, sparse = FALSE) %>%
      apply(1, any)
    
    .x[in_region | in_ummp | in_census, ]
  }
)

# 2) Keep patches near GLT regions
regions_buffer = sf::st_buffer(regions_sf, 2000) # Buffer size in m
patch_near = purrr::map(
  patches_dilat, ~ .x[sf::st_intersects(.x, regions_buffer, sparse = FALSE) %>% 
                        apply(1, any),])

# 3) Combine patches
patches_all = purrr::map2(
  patch_in,
  patch_near,
  ~ dplyr::bind_rows(.x, .y) %>%
    dplyr::distinct(geometry, .keep_all = TRUE) %>%
    dplyr::mutate(area_ha = as.numeric(sf::st_area(.)) / 10000)
)

# 4) Remove very small patches (stepping stones)
smallest_patches_regions = purrr::map2_dfr(
  patches_all,
  years,
  ~ {
    
    intersects_region = sf::st_intersects(
      .x,
      regions_sf,
      sparse = FALSE
    ) %>%
      apply(1, any)
    
    .x %>%
      dplyr::filter(intersects_region) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(patch_id, area_ha) %>%
      dplyr::arrange(area_ha) %>%
      dplyr::mutate(year = .y)
    
  }
)

# Small patches across years
smallest_patches_regions %>% 
  dplyr::arrange(area_ha) %>% 
  print(n=50)

# Distinct patches names < 5 ha
smallest_patches_regions %>%
  dplyr::filter(area_ha < 5) %>%
  dplyr::distinct(patch_id) %>%
  dplyr::arrange(patch_id)

# Remove small patches (while keeping tiny occupied patches)
patches_final = purrr::map(
  patches_all,
  ~ .x %>%
    dplyr::filter(
      area_ha >= 5 |
        patch_id %in% c(
          "Imbau_I_1",
          "Nova_Esperanca_1",
          "Poco_das_Antas_1"
        )
    )
)

# Plot
plot(rasters_lf[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(st_geometry(patches_dilat[[36]]), col="grey80", border=NA, add=TRUE) # Patches (without borders)
plot(st_geometry(patches_final[[36]]), add=TRUE, col="darkgreen", border=NA) # Kept patches
plot(st_geometry(regions_sf), add=TRUE, col="yellow", lwd=2) # GLT groups

#### Patch metrics --------
patches_metrics = progressr::with_progress({
  p = progressr::progressor(along = patches_final)
  
  purrr::map(
    patches_final,
    ~ {
      p()  # <- updates progress
      
      shape_tbl = vm_p_shape(.x, patch_col = "patch_id") %>%
        dplyr::rename(patch_id = id) %>%
        dplyr::rename(shape = value) %>%
        dplyr::select(patch_id, shape)
      
      enn_tbl = vm_p_enn(.x, patch_col = "patch_id") %>% 
        dplyr::rename(patch_id = id) %>%
        dplyr::rename(enn = value) %>%
        dplyr::select(patch_id, enn)
      
      .x %>%
        dplyr::left_join(shape_tbl, by = "patch_id") %>%
        dplyr::left_join(enn_tbl, by = "patch_id") %>%
        dplyr::mutate(
          area_ha = round(area_ha, 2),
          shape = round(shape, 2),
          enn = round(enn, 2),
        )
    }
  )
})

# Reorder the table
patches_metrics = purrr::map(
  patches_metrics,
  ~ .x %>%
    dplyr::select(
      patch_id,
      lyr.1,
      area_ha,
      shape,
      enn,
      geometry
    )
)

#### Patch connectivity (Makurhini) -------
# See: Godínez-Gómez, O., Correa-Ayram, C., Goicolea, T. & Saura, S. Makurhini: An R package for comprehensive analysis of landscape fragmentation and connectivity. Environmental Modelling & Software 201, 106981 (2026).

### Example
patches2024 = patches_metrics[[36]]
lu2024 = rasters_mspa[[36]]
res2024 = terra::subst(
  lu2024,
  from = c(1, 2, 3, 4, 5, 6, 33),
  to = c(1, 20, 20, 100, 50, 100, 5)
)
plot(res2024, col=c("white","yellow","lightblue","blue","navy"))
plot(sf::st_geometry(patches2024), col="darkgreen", add=TRUE)

# Compute metric
PC_res = Makurhini::MK_dPCIIC(
  nodes = patches2024,
  attribute = "area_ha",
  area_unit = "ha",
  distance = list(
    type = "least-cost",
    resistance = res2024
  ),
  metric = "IIC",
  probability = 0.5, # for a median dispersal distance, use a probability of 0.5 (50%)
  distance_thresholds = 2000
)
# Plot
breaks = classInt::classIntervals(PC_res$dIIC, n = 9, style = "jenks")
PC_res = PC_res %>%
  dplyr::mutate(dPC_q = cut(dIIC,
                            breaks = breaks$brks,
                            include.lowest = TRUE,
                            dig.lab = 5))  
ggplot() +  
  geom_sf(data = patches2024, color = "black") +
  geom_sf(data = PC_res, aes(fill = dPC_q), color = "black", size = 0.1) +
  scale_fill_brewer(palette = "RdYlGn", direction = 1, name = "dIIC (jenks)") +
  theme_minimal() +
  labs(
    title = "dIIC Least-cost distance",
    fill = "dIIC"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )


### Apply to all patches
compute_connectivity = function(patches, r) {
  
  resist_lc = terra::subst(
    r,
    from = c(1, 2, 3, 4, 5, 6, 33),
    to   = c(1, 20, 20, 100, 50, 100, 5)
  )
  
  PC_res = MK_dPCIIC(
    nodes = patches,
    attribute = "area_ha",
    area_unit = "ha",
    distance = list(
      type = "least-cost",
      resistance = resist_lc
    ),
    metric = "IIC",
    probability = 0.5,
    distance_thresholds = 2000
  )
  
  PC_res
}

PC_list = purrr::map2(
  patches_metrics,
  rasters_mspa,
  compute_connectivity
)

#### Export patches ----------
# Patch metrics
base_path = here("outputs", "data", "patchmetrics")
purrr::walk2(
  patches_metrics,
  years,
  ~ {
    output_path = file.path(base_path, paste0("patches_metrics_", .y, ".gpkg"))
    sf::st_write(.x, output_path, layer = "patches", append=FALSE, delete_dsn = TRUE, quiet = TRUE)
    message("Exported: ", output_path)
  }
)

### 2) Landscapemetrics ----------------

##### Example -------
# ##### Patch selection (core only)
# 
# # 1) patches = core forest only, class = 1 (i.e., 2 fragments connected by a corridor are considered as 2 patches)
# # On raw rasters (without dilatation-erosion)
# forest_patches = landscapemetrics::get_patches(rasters_mspa[[36]], class = 1, directions = 8)
# patches_sf = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
# # On rasters with dilatation-erosion
# forest_patches_dl = landscapemetrics::get_patches(rasters_dilate[[36]], class = 1, directions = 8)
# patches_sf_dl = sf::st_as_sf(as.polygons(forest_patches_dl[[1]][[1]], dissolve = TRUE))
# # Compare counts
# patches_sf %>% sf::st_drop_geometry() %>% dplyr::count()
# patches_sf_dl %>% sf::st_drop_geometry() %>% dplyr::count()
# 
# # visualize
# plot(rasters_dilate[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
# plot(sf::st_geometry(patches_sf_dl), col="darkgreen", add=TRUE)
# 
# # 2) patches intersecting GLT regions
# patch_in = patches_sf_dl[sf::st_intersects(patches_sf_dl, regions_sf, sparse=FALSE) %>%  apply(1, any),]
# 
# # 3) patches near GLT regions
# regions_buffer = sf::st_buffer(regions_sf, 500)
# patch_near = patches_sf_dl[sf::st_intersects(patches_sf_dl, regions_buffer, sparse=FALSE) %>%  apply(1, any),]
# 
# # 4) final kept patches = in OR near
# patches_final = dplyr::bind_rows(patch_in, patch_near) %>% dplyr::distinct(geometry, .keep_all=TRUE)
# 
# # Plot
# plot(rasters_dilate[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
# plot(st_geometry(patches_sf_dl), col="grey80", border=NA, add=TRUE) # Patches (without borders)
# plot(st_geometry(patches_final), add=TRUE, col="darkgreen", border=NA) # Kept patches
# plot(st_geometry(regions_sf), add=TRUE, col="yellow", lwd=2) # GLT groups
# 
# ##### Landscapemetrics
# 
# # Create a forest mask using patches
# mask_rast = terra::rasterize(patches_final, rasters_mspa[[36]], field = 1)
# plot(mask_rast, col = "#32a65e")
# 
# # Compute spatial metrics for patches
# patch_lm = landscapemetrics::spatialize_lsm(
#   mask_rast,
#   what = c("lsm_p_area"),
#   directions=8
# )
# 
# patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% 
#   dplyr::rename(area=value)
# 
# ##### Connectivity (Makurhini)
# ## PC with centroid distance
# PC_centroid = Makurhini::MK_dPCIIC(nodes = patches_final,
#                                    distance = list(type="centroid"),
#                                    metric="PC",
#                                    probability=0.05,
#                                    distance_thresholds=2000)
# 
# ## 2) PC with resistance rasters
# ## resistance raster = rasters with forest core and corridors
# mspa = rasters_mspa[[36]]
# 
# # Computational times are long so we crop the original raster
# # buffer minbbox to avoid cutting important connectors at the border
# minbbox2000 = terra::buffer(minbbox, width=2000)
# plot(minbbox2000)
# 
# # buffer patches 1 km
# patches_buf1km = terra::buffer(terra::vect(patches_final), width = 1000) %>% terra::aggregate()
# plot(patches_buf1km)
# 
# # extend bbox by patch buffer
# minbbox_expanded = terra::union(minbbox2000, patches_buf1km)
# 
# # crop MSPA to expanded area
# r_crop = terra::crop(mspa, minbbox_expanded) %>% 
#   terra::mask(minbbox_expanded)
# 
# plot(r_crop, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "yellow"))
# plot(regions, add=TRUE, col="red", lwd=1)
# 
# # Resistance matrix
# # corridors cost = 1 (same as core)
# resist1 = terra::ifel(r_crop==1, 1,
#                       terra::ifel(r_crop==33,1,100))
# plot(resist1, col=c("darkgreen","gray"))
# plot(patches_final, add=TRUE, col="orange")
# plot(regions, add=TRUE, col="red", lwy=1)
# 
# ## PC with least-cost
# PC_LC1 = MK_dPCIIC(nodes=patches_final,
#                    distance=list(type="least-cost", resistance=resist1),
#                    metric="PC",
#                    probability=0.05,
#                    distance_thresholds=2000)
# 
# 
# ### function to rename PC columns
# # Add prefix
# rename_pc = function(df, prefix){
#   df %>% dplyr::rename_with(~ paste0(prefix, .x),
#                             .cols = -lyr.1)
# }
# 
# ### rename each PC table
# PCc2000 = rename_pc(PC_centroid, "PCc2km_")
# PCrm2000 = rename_pc(PC_LC1, "PCrm2km_")
# 
# 
# ##### Final join
# 
# patches_metrics_final =
#   patches_final %>% 
#   dplyr::left_join(st_drop_geometry(patch_area), by="lyr.1") %>% 
#   dplyr::left_join(st_drop_geometry(PCc2000), by="lyr.1") %>% 
#   dplyr::left_join(st_drop_geometry(PCrm2000), by="lyr.1")
# patches_metrics_final = patches_metrics_final %>% 
#   dplyr::mutate(area = round(area,2))

##### Loop -----
# # Patch area, connectivity (centroid + resistance-based)
# compute_patch_metrics_tot = function(r, 
#                                  year,
#                                  sample_loc, 
#                                  buf_around_loc, 
#                                  minbbox,
#                                  buffer_patch){
#   
#   message("Processing year: ", year)
#   
#   # PATCH SELECTION
#   
#   forest_patches = landscapemetrics::get_patches(r, class = 1, directions = 8)
#   patches_sf = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
#   
#   patch_in = patches_sf[sf::st_intersects(patches_sf, sample_loc, sparse=FALSE) %>% apply(1, any),]
#   patch_near = patches_sf[sf::st_intersects(patches_sf, buf_around_loc, sparse=FALSE) %>% apply(1, any),]
#   
#   patches_final = dplyr::bind_rows(patch_in, patch_near) %>% 
#     dplyr::distinct(geometry, .keep_all=TRUE)
#   
#   # LANDSCAPE METRICS
#   mask_rast = terra::rasterize(patches_final, r, field = 1)
#   patch_lm = landscapemetrics::spatialize_lsm(mask_rast,
#                                               what = c("lsm_p_area"),
#                                               directions=8)
#   
#   patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(area=value)
# 
#   # CONNECTIVITY
#   PC_centroid = MK_dPCIIC(nodes = patches_final,
#                           distance = list(type="centroid"),
#                           metric = "PC",
#                           probability = 0.05,
#                           distance_thresholds = 2000)
#   
#   # buffer patches 1 km
#   patches_buf1km = terra::buffer(terra::vect(patches_final), width=1000) %>% terra::aggregate()
#   
#   # extend bbox
#   minbbox_expanded = terra::union(minbbox, patches_buf1km)
#   
#   # crop r
#   r_crop = terra::crop(r, minbbox_expanded) %>% terra::mask(minbbox_expanded)
#   
#   resist1 = terra::ifel(r_crop==1, 1,
#                         terra::ifel(r_crop==33,1,100))
#   
#   PC_LC1 = MK_dPCIIC(nodes=patches_final,
#                      distance=list(type="least-cost", resistance=resist1),
#                      metric="PC",
#                      probability=0.05,
#                      distance_thresholds=2000)
#   
#   PCc2000 = rename_pc(PC_centroid, "PCc2km_")
#   PCrm2000 = rename_pc(PC_LC1, "PCrm2km_")
# 
#   patches_metrics_final =
#     patches_final %>%
#     dplyr::left_join(st_drop_geometry(patch_area), by="lyr.1") %>%
#     dplyr::left_join(st_drop_geometry(PCc2000), by="lyr.1") %>%
#     dplyr::left_join(st_drop_geometry(PCrm2000), by="lyr.1") %>% 
#     dplyr::mutate(area = round(area,2), year = year)
#   
#   return(patches_metrics_final)
# }
# 
# # Parameters
# regions_buffer = sf::st_buffer(regions_sf, 500)
# minbbox2000 = terra::buffer(minbbox, width=2000)
# # Loop
# all_years_metrics = purrr::map2(rasters_dilate, years,
#                                 ~ compute_patch_metrics_tot(r = .x,
#                                                         year = .y,
#                                                         sample_loc = regions_sf,
#                                                         buf_around_loc = regions_buffer,
#                                                         minbbox = minbbox2000,
#                                                         buffer_patch = 500))


