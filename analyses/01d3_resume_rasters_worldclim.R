#------------------------------------------------#
# Author: Romain Monassier
# Objective: Resume WorldClim data
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(here)
library(terra)
library(sf)

### Import data -------

## Rasters
# Precipitations
base_path = here("data", "geo", "WorldClim", "work", "prec") 
prec_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
prec = lapply(prec_files, terra::rast)

# Min temperature
base_path = here("data", "geo", "WorldClim", "work", "tmin") 
tmin_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
tmin = lapply(tmin_files, terra::rast)

# Max temperature
base_path = here("data", "geo", "WorldClim", "work", "tmax") 
tmax_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)                  
tmax = lapply(tmax_files, terra::rast)

### Quick check -------
crs(prec[[1]])
ext(prec[[1]])
plot(prec[[1]]) # plot the first layer

### Resume datasets ------
# We compute yearly statistics for climatic variables (adapt depending on the climatic variable)
get_year = function(x) {
  sub(".*_(\\d{4})-\\d{2}.*", "\\1", basename(x))
}

#### Precipitations ------
prec_years = sapply(prec_files, get_year)
prec_by_year = split(prec, prec_years)
length(prec_by_year[["1989"]])

# Function
compute_yearly_stats = function(r_list) {
  
  r_stack = rast(r_list)
  
  r_sum = terra::app(r_stack, fun = sum, na.rm = TRUE)
  c(r_sum)
}

# Apply
prec_yearly = lapply(prec_by_year, compute_yearly_stats)

# Rename
prec_yearly = lapply(names(prec_yearly), function(yr) {
  
  r = prec_yearly[[yr]]
  names(r) = c(paste0("prec_sum_", yr))
  r
})
names(prec_yearly) = names(prec_by_year)

# Check
plot(prec_yearly[["1989"]][["prec_sum_1989"]])

#### Tmin ------
tmin_years = sapply(tmin_files, get_year)
tmin_by_year = split(tmin, tmin_years)
length(tmin_by_year[["1989"]])

# Function
compute_yearly_stats = function(r_list) {
  
  r_stack = rast(r_list)
  
  r_mean = terra::app(r_stack, fun = mean, na.rm = TRUE)
  c(r_mean)
}

# Apply
tmin_yearly = lapply(tmin_by_year, compute_yearly_stats)

# Rename
tmin_yearly = lapply(names(tmin_yearly), function(yr) {
  
  r = tmin_yearly[[yr]]
  names(r) = c(paste0("tmin_mean_", yr))
  r
})
names(tmin_yearly) = names(tmin_by_year)

# Check
plot(tmin_yearly[["1989"]][["tmin_mean_1989"]])

#### Tmax ------
tmax_years = sapply(tmax_files, get_year)
tmax_by_year = split(tmax, tmax_years)
length(tmax_by_year[["1989"]])

# Function
compute_yearly_stats = function(r_list) {
  
  r_stack = rast(r_list)
  
  r_mean = terra::app(r_stack, fun = mean, na.rm = TRUE)
  c(r_mean)
}

# Apply
tmax_yearly = lapply(tmax_by_year, compute_yearly_stats)

# Rename
tmax_yearly = lapply(names(tmax_yearly), function(yr) {
  
  r = tmax_yearly[[yr]]
  names(r) = c(paste0("tmax_mean_", yr))
  r
})
names(tmax_yearly) = names(tmax_by_year)

# Check
plot(tmax_yearly[["1989"]][["tmax_mean_1989"]])


### Export rasters --------

## Precipitations
out_prec = here("outputs", "data", "WorldClim", "prec")
for (yr in names(prec_yearly)) {
  
  r <- prec_yearly[[yr]]  # multi-layer SpatRaster
  
  for (i in 1:nlyr(r)) {
    
    lyr <- r[[i]]
    fname <- paste0(names(lyr), "_bbox.tif")
    
    writeRaster(
      lyr,
      file.path(out_prec, fname),
      overwrite = TRUE
    )
  }
}

## Tmin
out_tmin = here("outputs", "data", "WorldClim", "tmin")
for (yr in names(tmin_yearly)) {
  
  r <- tmin_yearly[[yr]]  # multi-layer SpatRaster
  
  for (i in 1:nlyr(r)) {
    
    lyr <- r[[i]]
    fname <- paste0(names(lyr), "_bbox.tif")
    
    writeRaster(
      lyr,
      file.path(out_tmin, fname),
      overwrite = TRUE
    )
  }
}

## Tmax
out_tmax = here("outputs", "data", "WorldClim", "tmax")
for (yr in names(tmax_yearly)) {
  
  r <- tmax_yearly[[yr]]  # multi-layer SpatRaster
  
  for (i in 1:nlyr(r)) {
    
    lyr <- r[[i]]
    fname <- paste0(names(lyr), "_bbox.tif")
    
    writeRaster(
      lyr,
      file.path(out_tmax, fname),
      overwrite = TRUE
    )
  }
}
