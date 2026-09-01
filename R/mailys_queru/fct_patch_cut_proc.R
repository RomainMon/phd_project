############################################################################################

# Queru et al. Better modelling habitat patches // Project ERC SCALED - 949812

############################################################################################
## This function allows to cut big patches that are not in the required size range to be 
# considered as a patch, using tesselation principle and mosaic them with patches with correct size

#PARAMETERS
# too_large_p = too-large-patch raster (layer of the patches that are oversized and need to be cut)), in which the tessellation will be performed 
# correct_p = correct-size-patch raster to be combined with the cut ones afterwards
## WARNING, areas that are no habitat must be NA, and others 1 or a patch number
# nb_opt = number of points to be drawn, can come from a search of the optimal number of points with find_random_nb_opt_based_on_range
# type = c("random","regular"), random or regular selection to perform the tesselation
# optimal = type of search, the deault is "range"
# tol = the accuracy wanted when searching for the optimal number of points to be drawn
# distance_to_goal = define how close the optimal needs to be from the goal if optimal == "goal" 
# nb_colors_wanted = number of colours to deplay maps, default is set to 12
# display = TRUE

patch_cut_proc = function(too_large_p, correct_p, nb_opt, type=c("random","regular"),
                          optimal="range", tol=10, distance_to_goal=0.1, 
                          nb_colors_wanted=12, display=TRUE){
  
  # Assign new id to correct patches
  temp = terra::freq(correct_p)
  if(nrow(temp)>0){
    rcl = data.frame(from = temp[,2], to = 1:nrow(temp))
    correct_patch = classify(correct_p,rcl) 
    id_max = minmax(correct_patch, compute=TRUE)[2]
  }
  if(nrow(temp)==0)id_max=0
  
  COL = brewer.pal(n = nb_colors_wanted, name = "Set3")[rep(sample(1:nb_colors_wanted,nb_colors_wanted,replace=F),ceiling(id_max/nb_colors_wanted))] 
  
  # Cut too large patches into smaller patches
  out_cut = patchs_cutting_all(r=too_large_p,n=nb_opt,id=id_max,type="random")
  
  out_mos2 = mosaic(out_cut, correct_patch, fun="first")
  plot(out_mos2,col = COL)
  
  ##Save the new file
  writeRaster(x = out_mos2, filename = paste0("final_patches_",Landscape,".tif"),overwrite=TRUE)
  
}


