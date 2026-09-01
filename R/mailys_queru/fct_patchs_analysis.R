############################################################################################

# Queru et al. Better modelling habitat patches // Project ERC SCALED - 949812

############################################################################################

# FUNCTION TO SUMMERIZE THE CLASSIFICATION OF PATCHES BY SIZE
## PARAMETERS : 
# r = patch raster, in which the tessellation will be performed
# mini_area / maxi_area = minimum and maximum bounds for patch size range
assess_patch_size = function(r, mini_area, maxi_area){
  freq_val = terra::freq(r)
  cell_size = res(r)[1]*res(r)[2]
  mean_area = mean(freq_val$count)*cell_size
  group_area_patch = cut(freq_val$count*cell_size,c(0,mini_area,maxi_area,Inf),labels=c('too small','CORRECT SIZE','too large'))
  stats_area_patch = table(group_area_patch)
  return(list(stats_area_patch))
} 

# FUNCTION TO CALCULATE THE NUMBER OF PATCHES, MEAN AND MEDIAN PATCH SIZE + PLOT PIE 
## PARAMETERS : 
# r = patch raster, each patch has one id, and matrix is considered as 0
# mini_area / maxi_area = minimum and maximum bounds for patch size range
stats_patchs <- function(r,mini_area,maxi_area) {
  freq_patchs = terra::freq(r)
  cell_size = res(r)[1]*res(r)[2]
  nb_patchs = nrow(freq_patchs)
  print(paste(nb_patchs,"patchs",sep=" "))
  
  area_km2 = round(mean(freq_patchs$count/100),2)
  print(paste("Mean area (km²) :", area_km2,sep=" "))
  
  area_ha = paste(area_km2*100)
  print(paste("Mean area (ha) :", area_ha,sep=" "))
  
  area_km2_bis = median(freq_patchs$count/100)
  print(paste("Median area (km²) :", area_km2_bis,sep=" "))
  
  area_ha_bis = median(freq_patchs$count)
  print(paste("Median area (ha) :", area_ha_bis,sep=" "))
  
  #Calculate the percentage of each patch category: too small, correct-size and too large
  group_area_patch = cut(freq_patchs$count*cell_size,c(0,mini_area,maxi_area,Inf),labels=c('too small','CORRECT SIZE','too large'))
  stats_area_patch = table(group_area_patch)
  
  lbls = c('too small','CORRECT SIZE','too large')
  pct = paste(round(100 * stats_area_patch/sum(stats_area_patch), 1), "%")
  #lbls <- paste(lbls,"\n", pct, sep = "")
  pie(stats_area_patch,
      labels = lbls,
      main = "Patches area in three categories")
}


# FUNCTION TO PLOT PATCHS SIZE HISTOGRAMME
## PARAMETERS : 
# r = patch raster, each patch has one id, and matrix is considered as 0
# log_scale = default is F, can be T if to plot in log scale
plot_histo <- function(r,log_scale=F) {
  cell_size = res(r)[1] * res(r)[1]
  freq_patchs = freq(r)
  area_patchs=data.frame("areaCells"= freq_patchs$count,"aream2"=freq_patchs$count*cell_size)
  par(mfrow=c(1,1))
  if(!log_scale){
  hist(area_patchs$aream2,
       xlab = "Area (m²)",
       ylab="Nb patchs",
       main = "Histogram of patches area in the landscape")
  }
  if(log_scale){
  hist(log10(area_patchs$aream2),
       xlab = "log10(Area (m²))",
       ylab="Nb patchs",
       main = "Histogram of patches area in the landscape")
  } 
}
