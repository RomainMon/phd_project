############################################################################################

# Queru et al. Better modelling habitat patches // Project ERC SCALED - 949812
# With tiny modifications by Romain Monassier (2026), namely: not exporting rasters but returning spatial objects instead

############################################################################################
## This function provides an overview of the patches present in the habitat file, 
# according to the minimum and maximum size constraints for a habitable area to be 
# considered a patch : too small areas are no longer considered as patches but as stepping stones
# and too large areas can be cut through the cut_patches function

#PARAMETERS
# habitat_layer = a raster layer depicting the habitat with 1 for the habitat and 0 for the matrix (rast object from terra)
# list_fragmenting_elements = a list of fragmenting elements as vectors (same extent as habitat layer)  (vect objects from terra)
# dilatation_erosion_choice = default F, set T if to simplify habitat layer with a dilatation erosion procedure
# dilatation_threshold = default is NULL, put a reasonable distance threshold for the dilatation erosion procedure 
# mini_area,maxi_area = desired patch size range
# nb_colors_wanted = number of colours to deplay maps, default is set to 12
# display = TRUE
# log_scale = F, set to T to display histogram f patch size in a log scale

situation_report_patches <- function(habitat_layer, list_fragmenting_elements=list(),
                                     dilatation_erosion_choice=FALSE,dilatation_threshold=NULL,
                                     mini_area,maxi_area,nb_colors_wanted=12,display=TRUE,log_scale=F) {
  
  #source("functions/fct_patchs_analysis.R")
  #source("functions/fct_dilatation_erosion.R")
  
  ## HABITAT LAYER
  legend = c("Matrix", "Habitat")
  colours = c("#E2DEDE", "#0E9813")
  if (display == TRUE){ 
    x11()
    par(mfrow=c(1,1))
    plot(habitat_layer, 
         type = "classes", 
         levels = legend,
         col = colours, 
         plg = list(cex = 0.7),
         main = "Habitat Layer")
  }
  
  
  ## METADATA DEFINITION
  length_hab = res(habitat_layer)[1]
  width_hab = res(habitat_layer)[2]
  cell_size = length_hab * width_hab
  
  # DEBUG : habitat layer format
  if (min((values(HabitatLayer,na.rm=T))) == 0 && max((values(HabitatLayer,na.rm=T))) == 1 ){   
    
    ## DILATATION - EROSION
    if (dilatation_erosion_choice == TRUE){ 
      
      # before dilatation - erosion
      temp1 = freq(habitat_layer)
      
      # DEBUG : threshold has to be a strictly positive integer
      if (dilatation_threshold%%1 == 0 && dilatation_threshold>0 ){ 
        
        habitat_layer = dilatation_erosion(habitat_layer,dilatation_threshold)
        
        # after dilatation - erosion
        temp2 = freq(habitat_layer)
        temp3 = round(temp2[temp2$value==1,3]/temp1[temp2$value==1,3] -1,2)*100
        print(paste("Using the erosion/dilatation, you gained",temp3, "% of habitat in your habitat layer")) 
        
        if (display == TRUE){ 
          plot(habitat_layer, 
               type = "classes", 
               levels = legend,
               col = colours, 
               plg = list(cex = 0.7),
               main = "Habitat Layer")
        }
      } else {
        print("Dilatation - erosion has not been performed because the chosen threshold has the wrong format (please check that you have entered a strictly positive integer)")
      }
    }
    
    ## EXCLUDING FRAGMENTING ELEMENTS FROM HABITAT
    
    habitat_layer_with_fragm_elem = habitat_layer
    
    for (fragm in list_fragmenting_elements){
      habitat_layer_with_fragm_elem = mask(habitat_layer_with_fragm_elem,fragm, inverse=TRUE)
    }
    
    if (display == TRUE){ 
      plot(habitat_layer_with_fragm_elem, 
           type = "classes", 
           main = "Landscape after excluding fragmenting elements",
           levels = c("Matrix", "Habitat","Fragmenting elements"),
           col = c("#E2DEDE", "#0E9813","white"), 
           plg = list(cex = 0.7))
    }
    
    
    
    ## PATCHES DEFINITION - REGION GROUP + ZONE SORTING IN 3 CATEGORIES
    
    # Patches definition = continuous sets oh habitat cells
    patch = patches(habitat_layer_with_fragm_elem,directions = 8, zeroAsNA=TRUE, allowGaps=FALSE)
    
    maxcol = minmax(patch)[2]
    COL = brewer.pal(n = nb_colors_wanted, name = "Set3")[rep(sample(1:nb_colors_wanted,nb_colors_wanted,replace=F),ceiling(maxcol/nb_colors_wanted))] 
    if (display == TRUE){ 
      plot(patch,col = COL)
    }
    
    # Collect the area of each patch
    patch_area = zonal(patch*0+1, patch, sum, as.raster=TRUE)*cell_size
    
    # CHECKING RESULTS : 
    print("Here are some statistics on the current definition of your patches")
    stats_patchs(r=patch,mini_area=mini_area,maxi_area=maxi_area)
    plot_histo(patch,log_scale=log_scale)
    if(!log_scale)abline(v = mini_area, col = 'red', lwd = 2, lty = 'dashed')
    if(!log_scale)abline(v = maxi_area, col = 'red', lwd = 2, lty = 'dashed')
    if(log_scale)abline(v = log10(mini_area), col = 'red', lwd = 2, lty = 'dashed')
    if(log_scale)abline(v = log10(maxi_area), col = 'red', lwd = 2, lty = 'dashed')
    legend(x = "topright",          # Position
           legend = c("Min. and max. bounds for the desired patch size range"),
           lty = c(1,1),           # Line types
           col = c("red"),           
           lwd = 2)
    print(assess_patch_size(r=patch,min=mini_area,max=maxi_area))
    
    ## A) Extract too-small patches
    
    # Select patches smaller than the minimum area size
    small_patches = ifel(patch_area > mini_area, NA, patch)
    if (display == TRUE){ 
      x11() 
      plot(small_patches,col = COL,
           main = paste("Patches smaller than mini_area - ",mini_area," m2"))
    }
    
    # Remove ID since they are no longer patches
    small_patches[small_patches > 0] <- 1
    
    # Save the patches removed because they are too small as a new file
    # writeRaster(x = small_patches, filename = paste0("patches_too_small_", Landscape,".tif"),overwrite=TRUE)
    
    ## B) Extract correct-size patches, i.e patches that don't need cutting
    
    # select patches between the minimum and maximum surface areas
    patchs_larger_mini = ifel(patch_area < mini_area, NA, patch)
    correct_size_patchs = ifel(patch_area > maxi_area, NA, patchs_larger_mini)
    if (display == TRUE){ 
      x11() 
      plot(correct_size_patchs,col = COL,
           main = paste("Patches between mini_area and maxi_area - ",mini_area,"and",maxi_area,"m2"))
    }
    # # Save the correct-sized patches as a new file
    # writeRaster(x = correct_size_patchs, filename = paste0("patches_correct_size_", Landscape,".tif"),overwrite=TRUE)
    
    
    ## C) Extract too-large patches, i.e patches that need cutting
    
    # Select patches over the maximum surface area
    large_patchs = ifel(patch_area < maxi_area, NA, patch)
    if (display == TRUE){
      x11() 
      plot(large_patchs,col = COL,
           main = paste("Patches larger than maxi_area -",maxi_area,"m2"))
    }
    
    # Save the largest patches that need to be cut as a new file
    # writeRaster(x = large_patchs, filename = paste0("patches_too_large_", Landscape,".tif"),overwrite=TRUE)
    
    ####################################################################################
    # Return the three rasters as objects (THE MAIN CHANGE FROM ORIGINAL SCRIPT IS HERE!)
    ####################################################################################
    return(list(
      TooSmallPatches = small_patches,
      CorrectPatches = correct_size_patchs,
      TooLargePatches = large_patchs
    ))
    
  } else {
    print("Your Habitat Layer raster does not have the right format. It has to be in binary format 0/1 (0=matrix ; 1=habitat)") # DEBUG HABITAT LAYER FORMAT
  }
}
