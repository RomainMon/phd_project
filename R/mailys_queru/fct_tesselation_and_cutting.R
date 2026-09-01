############################################################################################

# Queru et al. Better modelling habitat patches // Project ERC SCALED - 949812

############################################################################################
# SET OF FUNCTIONS TO PERFORM THE VARIOUS STEPS REGARDING TESSELATION AND PATCH CUTTING

#-------------------------------------------------------------------------------

# FUNCTION TO REALIZE THE ENTIRE PROCEDURE OF TESSELATION AND PATCH CUTTING

## PARAMETERS 
# r = large-patch raster (layer of the patches that are oversized and need to be cut)), in which the tessellation will be performed 
## WARNING, areas that are no habitat must be NA, and others 1 or a patch number
# n = number of points to be drawn
# id = last known patch id from the raster layers with patches that are within the good range of size, default is 0, no other patches will be included
# type = c("random","regular")

patchs_cutting_all = function(r, n, id=0, type=c("random","regular")){
  
  r2 = as(r, "Raster")
  
  # Draw random or regular points
  if(type=="random") point = raster::sampleRandom(r2, size=n, na.rm=TRUE, ext=NULL, cells=FALSE, rowcol=FALSE, xy=TRUE, sp=FALSE, asRaster=FALSE,overwrite=TRUE,values=F)
  if(type=="regular") point = spatSample(r, size=n, method="regular", replace=FALSE, na.rm=TRUE, 
                         as.raster=FALSE, as.df=FALSE, as.points=FALSE, values=FALSE, cells=FALSE, 
                         xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=5, exhaustive=FALSE)

  # Perform tesselation
  x = point[,1]
  y = point[,2]

  # Get the raster extent
  r_extent = ext(r)  # terra function to get extent
  # Pass extent to tessel function
  tesselation = tessel(x, y, raster_extent = r_extent)
  
  # New patchs definition
  new_patchs = patchs_cutting(r=r,tess=tesselation,id=id)
  
  return(new_patchs)
}


#-------------------------------------------------------------------------------

# TO TRANSFORM THE TESSELATION OUTPUT IN A VECTOR
# Function developped by Samuel Ackerman : https://rdrr.io/cran/animalEKF/src/R/tess2spat.r

# PARAMETERS
# obj = voronoi tesselation object, created with "deldir" function
# idvec = vector of id for polygons
tess2spat <- function(obj,idvec=NULL) {
  K <- nrow(obj$summary)
  if (is.null(idvec)) { idvec <- 1:K }
  partition <- vector(mode="list", length=K)
  xy <- lapply(tile.list(obj), "[", i=3:4)
  #form Polygons
  for (i in 1:length(xy)) {
    pcrds <- unique(cbind(xy[[i]]$x, xy[[i]]$y))
    pcrds <- rbind(pcrds, pcrds[ 1,])
    colnames(pcrds) <- c("X","Y")
    
    partition[[i]] <- sp::Polygons(list(sp::Polygon(pcrds)), ID=as.character(idvec[i]))
  }
  partition <- sp::SpatialPolygons(partition)
  partition
}

# ------------------------------------------------------------------------------

# PERFORM A TESSELATION FROM POINTS AND CONVERT FOR TERRA

## PARAMETERS 
# x, y = coordinates of points

tessel <- function(x, y, raster_extent = NULL){
  
  # create a delaunay triangulation from points
  if(!is.null(raster_extent)){
    # Use the raster extent as bounding box
    rw = c(raster_extent$xmin, raster_extent$xmax,
           raster_extent$ymin, raster_extent$ymax)
    t = deldir::deldir(x, y, rw = rw)
  } else {
    # Auto-calculate from points (original behavior)
    t = deldir::deldir(x, y)
  }
  
  # convert for use in terra
  tesselation_polygons = as(tess2spat(t, idvec=NULL), "SpatialPolygonsDataFrame")
  tesselation_polygons_vect = vect(tesselation_polygons)
  
  return(tesselation_polygons_vect)
}
# ------------------------------------------------------------------------------

# CUTTING OF PATCHES ACCORDING TO TESSELATION POLYGONS

## PARAMETERS  
# r = large patch raster, in which the tessellation will be performed
# t = voronoi tesselation object, created with "deldir" function
# i = last known patchs id, default is 0, no other patch to be included

# patchs_cutting <- function(r,tess,id=0) {
# 
#   # Polygons of tesselation definition
#   r[r > 0] <- 1
#   
#   # Cutting patches according to tesselation polygons
#   tess[,1] = (id+1):(id+nrow(data.frame(tess)))
#   names(tess)[1]="id"
#   
#   patch_raster = rasterize(tess, r*0, field = "id")
#   r2 = patch_raster*r
#   #writeRaster(x = patch_raster, filename = "patch_raster.tif",overwrite=TRUE)
#   
#   return(r2)
# }

patchs_cutting <- function(r,tess,id=0) {
  
  # Polygons of tessellation definition
  
  r[r > 0] <- 1
  
  # Cutting patches according to tessellation polygons
  tess[,1] = (id+1):(nrow(data.frame(tess))+id)
  names(tess)[1]="id"
  
  patch_raster = rasterize(tess, r*0, field = "id")
  r2 = patch_raster*r
  r2_vect = as.polygons(r2)
  r2_vect_dis = disagg(r2_vect)
  #r2_vect_dis$id = 1:length(r2_vect_dis$id)
  r2_vect_dis$id = (id+1):(id+length(r2_vect_dis$id)) # correction pour problème ids
  r3 = rasterize(r2_vect_dis, r*0, field = "id")
  #plot(r3,col=1:length(r2_vect_dis$id))
  return(r3)
}


#-------------------------------------------------------------------------------

# FUNCTION TO FIND THE OPTIMAL NUMBER OF POINTS TO BE DRAWN
# BASED ON MAXIMIZING THE NUMBER OF PATCHES IN THE DESIRED SIZE RANGE
# the percentage of patches falling within the desired range is calculated a number of time (length.out) for different number of points (wide range)
# then a ternary search if performed to find the optimum

## PARAMETERS : 
# too_large_p = large-patch raster (layer of the patches that are oversized and need to be cut)), in which the tessellation will be performed 
## WARNING, areas that are no habitat must be NA, and others 1 or a patch number
# mini_area / maxi_area = minimum and maximum bounds for desired patch size range
# type = c("random", "regular") for how points are drawn to perform the tesselation
# tol = default is 10, the tolerated error for the optimal search, i.e. when the search stops, when left and right search bounds are closer than tol. A measurement of accuracy, lower numbers means a longer and more accurate search, minimum possible is 1 
# bounds_min / bounds_max : min and max bounds of point numbers, range within which we search with a ternary search procedure for he optimum number of points, 
# i.e. the one that maximizes the number of patches (derived from points with a tesselation) which size falls within the desired range
# the default is bounds_min = 0.1, bounds_max=4, this means we will search for the optimum number of points between 0.1*theoretical number of points and  4* number of points
# length.out: number of points numbers for which the desired size range is calculated to search for the optimum - numbers of points are along a regular sequence between bounds_min*theoretical_number and bounds_max*theoretical_number, default is length.out = 10, it should be an integer. Note that increasing length.out will increase the accuracy of the search, but will also increase calculation time 
# rep: number of time the cutting and assessment is repeated for each element of the sequence (with length = length.out), default is repet = 1, it should be an integer, increasing this value will increase calculation time, but ~10 reps may help see if the process if variable or not 

find_random_nb_opt_based_on_range = function(too_large_p, mini_area, maxi_area, type=c("random", "regular"), tol = 10, bounds_min=0.1, bounds_max=4, length.out=10, repet = 1){
  
  resol = terra::res(too_large_p)[1]
  freq_large = terra::freq(too_large_p) 
  mean_patch_size = mean(freq_large$count*resol^2)
  
  ## set up a rough theoretical number of points that should be drawn (~mean patch size/expected mean patch size)
  theoretical_number = ceiling(mean_patch_size/mean(c(mini_area,maxi_area)))*nrow(freq_large)
  
  ## define in what range of patch number we need to search for the optimal > i.e. calculate the percentage of correctly sized patches for different number of points
  left_search = round(theoretical_number * bounds_min,0) 
  right_search = round(theoretical_number * bounds_max,0)
  N = round(seq(left_search, right_search, length.out=length.out),0)
  if((N<3)[1]){
    print("WARNING : bounds_min is small, the smallest number of points to draw was 0")
    N = N[N>3]
  }
  if(sum(N>sum(freq_large$count))>0){
    N = N[N<sum(freq_large$count)]
    print("WARNING : bounds_max is large, the largest number of points to draw exceeds the number of available pixels")
  }
  
  ## search for the optimal number of points 
  N_out = matrix(0,ncol=repet,nrow=length(N))
  k = 1
  for(ni in N){
    for(j in 1:ncol(N_out)){
      temp = assess_patch_size(patchs_cutting_all(too_large_p,n=ni,id=0,type=type), mini_area=mini_area, maxi_area=maxi_area)[[1]]
      N_out[k,j] = temp[2]/sum(temp) 
    }
    k = k+1
  }
  N2 = N; N2_out=N_out
  plot(N2,N2_out[,1], xlab = "nuber of points drawn", ylab = "% patches within desired range",ylim=c(0,1))
  if(repet>1)for(re in 2:repet)points(N2,N2_out[,re])
  #points(N2[order(N2_out,decreasing=T)[1:5]],N2_out[order(N2_out,decreasing=T)[1:length.out/2],1],col="grey",pch=20)
  
  ### Ternary search on the half highest points
  k=1 
  left_search_new = min(N2[order(N2_out,decreasing=T)[1:length.out/2]]) ## round(need_to_cut_in * bounds_min,0) 
  right_search_new = max(N2[order(N2_out,decreasing=T)[1:length.out/2]]) ## round(need_to_cut_in * bounds_max,0)
  while(abs(left_search_new-right_search_new)>tol){ 
    N = c(left_search_new+(right_search_new-left_search_new)/5,right_search_new-(right_search_new-left_search_new)/5)
    N_out = NULL 
    for(ni in N){
      temp = assess_patch_size(patchs_cutting_all(too_large_p,n=ni,id=0,type=type), mini_area=mini_area, maxi_area=maxi_area)[[1]]
      N_out =c(N_out,temp[2]/sum(temp))
    }
    points(N,N_out,col=k,cex=2,pch=20)
    if(which.max(N_out)==1) left_search_new = left_search_new; right_search_new = N[2]
    if(which.max(N_out)==2) left_search_new = N[1]; right_search_new = right_search_new
    print(c(left_search_new,right_search_new))
    k=k+1
  }
  
  nb_opt = (left_search_new+right_search_new)/2 
  abline(v=nb_opt)
  #print(paste("Optimal N is ",round(nb_opt,0)))
  return(round(nb_opt,0))
}

