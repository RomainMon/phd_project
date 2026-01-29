#------------------------------------------------#
# Author: Romain Monassier
# Objective: Prepare rasters for patch-scale metrics and analysis
#------------------------------------------------#


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
message("Applying dilatation–erosion...")
rasters_dilate = lapply(seq_along(rasters), function(i) {
  message("  - Processing dilatation–erosion for raster ", i, " (year ", years[i], ")")
  dilatation_erosion(rasters[[i]], seuil = 50)
})

# Quick check
plot(rasters[[36]], main=paste0("Before dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_dilate[[36]], main=paste0("After dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
freq(rasters[[36]])
freq(rasters_dilate[[36]])


### Compute patch-level metrics   ---------

#### On a single raster -----
##### Patch selection (core only) ----------

# 1) patches = core forest only, class = 1 (i.e., 2 fragments connected by a corridor are considered as 2 patches)
plot(rasters_mspa[[1]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "yellow"))
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
plot(mask_rast, col = "#32a65e")
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
## PC with centroid distance
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

plot(r_crop, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "yellow"))
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

PCc2000 <- rename_pc(PC_centroid$d2000, "PCc2000_")
PCc8000 <- rename_pc(PC_centroid$d8000, "PCc8000_")

PCcorr1_2000 <- rename_pc(PC_LC1$d2000, "PCcorr1_2000_")
PCcorr1_8000 <- rename_pc(PC_LC1$d8000, "PCcorr1_8000_")

##### Final join ---------------------------

patches_metrics_final =
  patches_final %>% 
  dplyr::left_join(st_drop_geometry(patch_area), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(patch_shape), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCc2000), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCc8000), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCcorr1_2000), by="lyr.1") %>% 
  dplyr::left_join(st_drop_geometry(PCcorr1_8000), by="lyr.1")
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
  
  # PATCH SELECTION
  
  forest_patches = landscapemetrics::get_patches(r, class = 1, directions = 8)
  patches_sf = sf::st_as_sf(as.polygons(forest_patches[[1]][[1]], dissolve = TRUE))
  
  patch_in = patches_sf[sf::st_intersects(patches_sf, regions_sf,     sparse=FALSE) %>% apply(1, any),]
  patch_near = patches_sf[sf::st_intersects(patches_sf, regions_buffer, sparse=FALSE) %>% apply(1, any),]
  
  patches_final = dplyr::bind_rows(patch_in, patch_near) %>% 
    dplyr::distinct(geometry, .keep_all=TRUE)
  
  # LANDSCAPE METRICS
  
  mask_rast = terra::rasterize(patches_final, r, field = 1)
  patch_lm = landscapemetrics::spatialize_lsm(mask_rast,
                                              what = c("lsm_p_area","lsm_p_shape"),
                                              directions=8)
  
  patch_area = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_area, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(area=value)
  patch_shape = sf::st_as_sf(terra::extract(patch_lm$layer_1$lsm_p_shape, patches_final, fun=unique, bind=TRUE)) %>% dplyr::rename(shape=value)
  
  # CONNECTIVITY
  
  PC_centroid = MK_dPCIIC(nodes = patches_final,
                          distance = list(type="centroid"),
                          metric = "PC",
                          probability = 0.05,
                          distance_thresholds = c(2000,8000))
  
  # buffer patches 1 km
  patches_buf1km = terra::buffer(terra::vect(patches_final), width=1000) %>% terra::aggregate()
  
  # extend bbox
  minbbox_expanded = terra::union(minbbox2000, patches_buf1km)
  
  # crop r
  r_crop = terra::crop(r, minbbox_expanded) %>% terra::mask(minbbox_expanded)
  
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
  
  PCc2000 <- rename_pc(PC_centroid$d2000, "PCc2000_")
  PCc8000 <- rename_pc(PC_centroid$d8000, "PCc8000_")
  PCcorr1_2000 <- rename_pc(PC_LC1$d2000, "PCcorr1_2000_")
  PCcorr1_8000 <- rename_pc(PC_LC1$d8000, "PCcorr1_8000_")
  
  patches_metrics_final =
    patches_final %>%
    dplyr::left_join(st_drop_geometry(patch_area), by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(patch_shape), by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCc2000), by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCc8000), by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCcorr1_2000), by="lyr.1") %>%
    dplyr::left_join(st_drop_geometry(PCcorr1_8000), by="lyr.1") %>%
    dplyr::mutate(area = round(area,2), year = year)
  
  return(patches_metrics_final)
}

# Loop
regions_buffer = sf::st_buffer(regions_sf, 500)
minbbox2000 = terra::buffer(minbbox, width=2000)
all_years_metrics = purrr::map2(rasters_mspa, years,
                                ~ compute_patch_metrics(r=.x,
                                                        year=.y,
                                                        regions_sf=regions_sf,
                                                        regions_buffer=regions_buffer,
                                                        minbbox2000=minbbox2000))

