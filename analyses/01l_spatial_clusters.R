#------------------------------------------------#
# Author: Romain Monassier
# Objective: Delimiting spatial clusters
#------------------------------------------------#

### Load packages
library(dplyr)
library(spdep)
library(sf)
library(here)
library(ggplot2)
library(terra)

### Load datasets -------------
raster_tm_2024 = terra::rast(here("outputs", "data", "MapBiomas", "Rasters_cumulative_tm", "raster_reclass_cumul_tm_2024.tif"))
data_car = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.rds"))
plot(raster_tm_2024)

### Prepare dataset -------------
defor = raster_tm_2024 == 8
refor = raster_tm_2024 == 7

### Deforestation ----
#### Compute local proportions -----
# Grid
bbox = st_as_sfc(st_bbox(raster_tm_2024))
grid = st_make_grid(bbox, cellsize = 2000) %>% 
  st_as_sf()
st_crs(grid) = st_crs(raster_tm_2024)
plot(defor)
plot(grid, add=TRUE)

# Compute proportions in the grid
extract_defor = terra::extract(defor, vect(grid), fun = mean, na.rm = TRUE)
grid$prop_defor = extract_defor[,2]

# Remove NAs
grid = grid %>% 
  dplyr::filter(!is.na(prop_defor))

#### Neighbor matrix  -------------
nb_defor = poly2nb(grid, queen = TRUE)

#### Weigh matrix  -------------
# Computes spatial weighs for neighbours list
# Argument style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
# B is the basic binary coding
# W is row standardised (sums over all links to n)
# C is globally standardised (sums over all links to n)
# U is equal to C divided by the number of neighbours (sums over all links to unity)
# S is the variance-stabilizing coding scheme
lw_defor = nb2listw(nb_defor, style = "W", zero.policy = TRUE)

#### Compute Getis-Ord I --------
gi = spdep::localG(grid$prop_defor, lw_defor)
grid$GiZ = as.numeric(gi)
grid$hotspot = "Non-significant"
grid$hotspot[grid$GiZ > 1.96] = "Hotspot"
grid$hotspot[grid$GiZ < -1.96] = "Coldspot"

#### Plot ----------
plot(raster_tm_2024, col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))

# Overlay the hotspots
plot(st_geometry(grid), add = TRUE, border = "lightgray", lwd = 0.1)
points(st_coordinates(st_centroid(grid)), col = ifelse(grid$hotspot == "Hotspot", "red", ifelse(grid$hotspot == "Coldspot", "blue", "lightgray")), pch = 19, cex = 0.5)

# Legend
legend("topright",
       legend = c("Hotspot", "Coldspot", "Non-significant"),
       col = c("red", "blue", "lightgray"),
       pch = 19, pt.cex = 1)

### Reforestation ----
#### Compute local proportions -----
# Grid
bbox = st_as_sfc(st_bbox(raster_tm_2024))
grid = st_make_grid(bbox, cellsize = 2000) %>% 
  st_as_sf()
st_crs(grid) = st_crs(raster_tm_2024)
plot(refor)
plot(grid, add=TRUE)

# Compute proportions in the grid
extract_refor = terra::extract(refor, vect(grid), fun = mean, na.rm = TRUE)
grid$prop_refor = extract_refor[,2]

# Remove NAs
grid = grid %>% 
  dplyr::filter(!is.na(prop_refor))

#### Neighbor matrix  -------------
nb_refor = poly2nb(grid, queen = TRUE)

#### Weigh matrix  -------------
# Computes spatial weighs for neighbours list
# Argument style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
# B is the basic binary coding
# W is row standardised (sums over all links to n)
# C is globally standardised (sums over all links to n)
# U is equal to C divided by the number of neighbours (sums over all links to unity)
# S is the variance-stabilizing coding scheme
lw_refor = nb2listw(nb_refor, style = "W", zero.policy = TRUE)

#### Compute Getis-Ord I --------
gi = spdep::localG(grid$prop_refor, lw_refor)
grid$GiZ = as.numeric(gi)
grid$hotspot = "Non-significant"
grid$hotspot[grid$GiZ > 1.96] = "Hotspot"
grid$hotspot[grid$GiZ < -1.96] = "Coldspot"

#### Plot ----------
plot(raster_tm_2024, col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))

# Overlay the hotspots
plot(st_geometry(grid), add = TRUE, border = "lightgray", lwd = 0.1)
points(st_coordinates(st_centroid(grid)), col = ifelse(grid$hotspot == "Hotspot", "red", ifelse(grid$hotspot == "Coldspot", "blue", "lightgray")), pch = 19, cex = 0.5)

# Legend
legend("topright",
       legend = c("Hotspot", "Coldspot", "Non-significant"),
       col = c("red", "blue", "lightgray"),
       pch = 19, pt.cex = 1)
