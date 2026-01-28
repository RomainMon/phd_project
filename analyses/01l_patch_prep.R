#------------------------------------------------#
# Author: Romain Monassier
# Objective: Prepare rasters for patch-scale metrics and analysis
#------------------------------------------------#


#### Dilatation-erosion --------
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