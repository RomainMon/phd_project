#------------------------------------------------#
# Author: Romain Monassier
# Objective: Patch-cutting procedure
#------------------------------------------------#

### This script is based on Queru et al. (2026), Better defining habitat patches to improve spatially explicit modelling outputs, Methods in Ecology & Evolution

### Load packages
library(terra)
library(raster)
library(deldir)
library(RColorBrewer)
library(rstudioapi)
library(here)
library(dplyr)

### import functions
source(here("R","mailys_queru","fct_dilatation_erosion.R"))
source(here("R","mailys_queru","fct_tesselation_and_cutting.R"))
source(here("R","mailys_queru","fct_situation_report_patches_RM.R")) # <- Replaced with my version
source(here("R","mailys_queru","fct_patchs_analysis.R"))
source(here("R","mailys_queru","fct_patch_cut_proc_RM.R")) # <- Replaced with my version

######################################################################################################################
#### PREPARE DATA AND SET UP PARAMETERS
######################################################################################################################

#### Load habitat map  ----

##### Rasters -------
base_path = here("outputs", "data", "landscape_rshifter")
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
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "purple", "orange"))

##### Patches -----
base_path = here("outputs", "data", "patches_rshifter")
vect_files = list.files(base_path, pattern = "\\.gpkg$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(vect_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
vector_df = data.frame(file = vect_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load vectors in chronological order
patches = lapply(vector_df$file, sf::st_read)
years = vector_df$year
# Check
for (i in seq_along(patches)) {
  cat("Year", years[i], " → raster name:", basename(vector_df$file[i]), "\n")
}
names(patches) = vector_df$year # Name by year
# Plot
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "purple", "orange"))
plot(sf::st_geometry(patches[[36]]), col="lightgreen", add=TRUE)

##### Group locations -----
regions = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))


#### Prepare habitat map -----
# Habitat Layer (must be a binary raster or a text file, containing only 0s and 1s)
# 0 = matrix pixel ; 1 = Habitat pixel


##### Reclassify rasters based on patches
binary_rasters = vector("list", length(rasters_mspa))

for (i in seq_along(rasters_mspa)) {
  
  year_i = years[i]
  
  # Corresponding patch layer
  patch_i = patches[[as.character(year_i)]]
  
  # Convert sf to terra vector
  patch_i = terra::vect(patch_i)
  
  # Make sure CRS matches
  if (!terra::same.crs(rasters_mspa[[i]], patch_i)) {
    patch_i = terra::project(patch_i, terra::crs(rasters_mspa[[i]]))
  }
  
  # Rasterize patches: cells covered by a polygon = 1
  r_binary = terra::rasterize(
    patch_i,
    rasters_mspa[[i]],
    field = 1,
    background = 0
  )
  
  binary_rasters[[i]] = r_binary
  
  cat("Year", year_i, "done\n")
}


# Plot
plot(binary_rasters[[36]])
plot(sf::st_geometry(patches[[as.character(years[36])]]),
     add = TRUE, border = "red")

### 2005 RASTER
names(binary_rasters) = years
HabitatLayer = binary_rasters[["2005"]]

#### Dilatation-erosion (optional) ----
### OPTIONAL STEP - Perform a DILATATION EROSION on habitat map, to remove the small gaps and spurs
# Do you want to perform a dilatation-erosion step? If yes, set TRUE. This additional step allows to fill smal gaps in habitat
# Default value is FALSE
DilatationErosionChoice =  FALSE
# Set here the threshold value for dilatation - erosion, ie the maximum distance from habitat to be dilated (m)
# e.g : threshold = 100, resolution = 100, dilation will be performed on one pixel)
# DilatationThreshold has to be a a strictly positive integer
DilatationThreshold = 30 #m


#### Set up DESIRED PATCHE SIZE RANGE ------
# Set here the minimum and maximum threshold for patch definition, ie the minimum area
# for a habitat area to be considered as a patch (smaller areas can be considered as 
# stepping stones)

### Determine smaller occupied patches
# Intersect with GLT locations
patches_intersect = lapply(seq_along(patches), function(i) {
  
  year_i = names(patches)[i]
  
  patches_i <- patches[[i]] %>%
    dplyr::mutate(
      patch_id = dplyr::row_number(),
      area_m2 = as.numeric(sf::st_area(.)),
      area_m2 = format(area_m2, scientific = FALSE)
    )
  
  # Keep patches intersecting regions
  patches_i_intersect = patches_i[
    apply(
      sf::st_intersects(patches_i, regions, sparse = FALSE),
      1,
      any
    ),
  ]
  
  patches_i_intersect
})

# Name list by year
names(patches_intersect) = names(patches)

# Smallest patches
smallest_patches = dplyr::bind_rows(
  lapply(names(patches_intersect), function(year_i) {
    
    patches_intersect[[year_i]] %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(year = as.numeric(year_i)) %>%
      dplyr::arrange(area_m2) %>%
      dplyr::slice_head(n = 1)
    
  })
)
smallest_patches

### Thresholds
MiniArea = 20000 #m²
MaxiArea = 1500000 #m² # According to the literature on GLT home range (Lucas et al. 2019, Dietz et al. 1997)


#### ESTETHIC PREFERENCES -----
# Would you like to display the intermediate figures : set "TRUE" if you do
# Default value is FALSE
Display = T

# How many colors do you want to plot maps : Must be between 3 and 12
# Default value is 12
NbColorsWanted = 12


######################################################################################################################
#### EVALUATE THE HABITAT LAYER - define how many patches (based on contiguous definition) fall within the desired patch size range
#### AND SHARE THE PATCH LAYERS IN THREE SUB-LAYERS : too small, correct size and too large patches
######################################################################################################################

##### USE THE FUNCTION -----
Landscape = "2005" 
list2env(
  situation_report_patches(
    habitat_layer = HabitatLayer,
    #list_fragmenting_elements = ListFragmentingElements,
    dilatation_erosion_choice = DilatationErosionChoice,
    dilatation_threshold = DilatationThreshold,
    mini_area = MiniArea,
    maxi_area = MaxiArea,
    nb_colors_wanted = NbColorsWanted,
    display = Display,
    log_scale = TRUE
  ),
  envir = environment()
)

# This function returns:
# TooSmallPatches
# CorrectPatches
# TooLargePatches


######################################################################################################################
#### DEFINE MAIN PARAMETERS TO CUT THE TOO LARGE PATCHES AND 
#### CHOOSE THE NUMBER OF POINTS TO BE DRAWN (higher number of points mean more numerous and smaller patches)
######################################################################################################################

### TESSELATION PARAMETERS
# Do you want to draw random points for tesselation ("random") or regular ("regular") within too large patches?
Type = "random"

# Regarding the optimal points to draw, the default option is to maximize the percentage of patches (once cut) that fall within
# the desired path size range (option "range")  
# An alternative option is to minimize the difference between the mean patch size (once cut) and a mean desired patch size (option "goal")
OptimalPointNumberMeth = "range"
# If "range" has been chosen, please define the accuracy of the search with parameter tol, min is 1, default is 10, 
# tol should be quite low to have an accurate value for the optimal number of points
Tol = 5
# If "goal" has been chosen, please define how close you want to be to the goal
# ie. if you accept a deviation of 20%, set 0.2 ..., default = 0.1
DistanceToGoal = 0.1

### FIND THE RIGHT NB OF POINTS TO DRAW -----
# Percent of patches within the desired range will form a bell-shape with increasing number of points, first increasing, then decresing after having reached a maximum
# bounds_min and bounds_max are to be played with to ensure the full range of option is tested around what could be a first approximation: mean_patch_size_of_too_large_patches/mean_wanted_patch_size
# if percent of correctly ranked patches is only increasing (increase bounds_max), if it only decreases, (decrease bounds_min), otherwise test a large range: 0.1 - 4 for instance
# bounds_min cannot be too small, otherwise the number of points to be drawn will be 0 (the function corrects for that)
# bounds_min cannot be too large, otherwise the range explored will be too large and the optimum might be missed, a warning is returned if the number of points to be drawn exceeds 
# the number of availables pixels
# this choice may depend on Type, bounds_max can be larger for "regular" than "random" 

# length.out defines on which first sets of points number the percentage of patches (once cut) that fall within
# the desired path size range will be calculate, the default is length.out=10, 10 values of point number are drawn regularly between the min and max number of points to be tested, 
# between bounds_min * mean_patch_size_of_too_large_patches/mean_wanted_patch_size and bounds_max * mean_patch_size_of_too_large_patches/mean_wanted_patch_size  

# rep defines how many times the procedure should be repeated (drawing N points, cutting, and assessing percent of patch that fall within
# the desired patch size range) for each points number. This process includes some stochasticity, so each repetition will be a bit different, 
# and increasing rep will help see this potential variability. Yet, increasing rep will also lead to higher calculatio time; default is 1

## USE THE FUNCTION
NbOpt = find_random_nb_opt_based_on_range(too_large_p = TooLargePatches,
                                          mini_area = MiniArea, 
                                          maxi_area = MaxiArea, 
                                          type = Type,
                                          tol = Tol, 
                                          bounds_min=0.1, 
                                          bounds_max=4, 
                                          length.out=10, 
                                          rep = 1)

######################################################################################################################
#### CREATE THE FINAL PATCH LAYER
######################################################################################################################

FinalPatches = patch_cut_proc(too_large_p = TooLargePatches,
               correct_p = CorrectPatches,
               nb_opt = NbOpt,
               type = Type,
               optimal = OptimalPointNumberMeth,
               distance_to_goal= DistanceToGoal,
               tol = Tol,
               nb_colors_wanted = NbColorsWanted,
               display = Display) 

# Plot
COL = brewer.pal(n = NbColorsWanted, name = "Set3")[rep(sample(1:NbColorsWanted,NbColorsWanted,replace=F),ceiling(minmax(FinalPatches, compute=TRUE)[2]/NbColorsWanted))] 
plot(FinalPatches,col=COL)

# check characteristics of new patch layer
assess_patch_size(FinalPatches,mini_area=MiniArea,maxi_area=MaxiArea)
plot_histo(FinalPatches,log_scale=T); abline(v=log(MiniArea),col="red");abline(v=log(MaxiArea),col="red") 

# EXPORT FINAL PATCH LAYERS
# Precise the folder location
terra::writeRaster(
  TooSmallPatches,
  here(
    "outputs", "data", "patches_cut",
    paste0("TooSmallPatches_", Landscape, ".tif")
  ),
  overwrite = TRUE
)
terra::writeRaster(
  TooLargePatches,
  here(
    "outputs", "data", "patches_cut",
    paste0("TooLargePatches_", Landscape, ".tif")
  ),
  overwrite = TRUE
)
terra::writeRaster(
  CorrectPatches,
  here(
    "outputs", "data", "patches_cut",
    paste0("CorrectPatches_", Landscape, ".tif")
  ),
  overwrite = TRUE
)
terra::writeRaster(
  FinalPatches,
  here(
    "outputs", "data", "patches_cut",
    paste0("FinalPatches_", Landscape, ".tif")
  ),
  overwrite = TRUE
)

# ### Create a resistance layer from final habitat patches and info on surrounding matrix  
# # reclassify final patches as habitat
# FinalPatchesRec = classify(FinalPatches, matrix(c(0,99999,1,NA,NA,100), ncol=3, byrow=TRUE), include.lowest=TRUE)
# TooSmallPatchesRec = classify(TooSmallPatches, matrix(c(0,99999,10,NA,NA,1), ncol=3, byrow=TRUE), include.lowest=TRUE) 
# # give a breadth to roads
# RoadBuffer = buffer(Road,40)
# RoadBufferRast = rasterize(RoadBuffer,FinalPatches,field="LONGUEUR")
# RoadBufferRastRec = classify(RoadBufferRast, matrix(c(0, 99999, 900, NA, NA, 0), ncol=3, byrow=TRUE), include.lowest=TRUE)
# plot(RoadBufferRastRec)
# # combine
# CostLayer = FinalPatchesRec+RoadBufferRastRec
# CostLayer = CostLayer/TooSmallPatchesRec 
# writeRaster(CostLayer, "cost_layer.tif", overwrite=TRUE)
