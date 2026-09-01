############################################################################################

# Queru et al. Better modelling habitat patches // Project ERC SCALED - 949812

############################################################################################

# FUNCTION TO PERFORM THE DILATATION - EROSION STEP, MEANING FILLING SMALL GAPS IN HABITAT AREAS

## PARAMETERS REQUIRED: 
# r = raster of habitat layer in binary format 0/1 (0=matrix ; 1=habitat)
# threshold = dilatation - erosion threshold

dilatation_erosion <- function(r,threshold){

    ### Replace 0 (ie matrix) with NA 
    r[r[[1]]== 0] = NA
    
    ### Calcule distance to habitat areas
    distances = terra::distance(r)
    
    ### distance-to-habitat thresholding (cells nb)
    distances_hab = distances>threshold
    
    ### Replace 0 (ie distance-to-habitat < threshold ) with en NA
    distances_hab[distances_hab[[1]]== 0] <- NA
    
    ### Calculating distance to non-habitat
    distances_non_habitat = terra::distance(distances_hab)
    
    ### distance-to-habitat thresholding (cells nb)
    final_rast = as.numeric(distances_non_habitat>threshold)
    
    return(final_rast)

}
