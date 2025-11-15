#------------------------------------------------#
# Author: Romain Monassier
# Objective: Reclass rasters using land cover data and linear features
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(here)
library(terra)
library(purrr)
library(stringr)
library(ggplot2)
library(raster)
library(sf)

### Import data -------
## Rasters
base_path = here("outputs", "data", "MapBiomas", "Mask_sampling_bbox")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}

## Vectors
roads = vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))
power_lines = vect(here("data", "geo", "OSM", "work", "Power_line_OSM_clean.shp"))
pipelines = vect(here("data", "geo", "OSM", "work", "Pipelines_OSM_clean.shp"))
bridges = vect(here("data", "geo", "AMLD", "Passagens_ARTERIS", "work", "road_overpasses_clean.shp"))
plantios = vect(here("data", "geo", "AMLD", "plantios", "work", "plantios_clean.shp"))
car = vect(here("data", "geo", "IBGE", "cadastro_car", "AREA_IMOVEL_RJ_2024", "AREA_IMOVEL_bbox.shp"))
pda = vect(here("data", "geo", "MMA", "protected_areas", "ucs", "poco_das_antas.shp"))
uniao = vect(here("data", "geo", "MMA", "protected_areas", "ucs", "uniao.shp"))

### Quick check -------
# list all objects that must share CRS
layers <- list(
  raster_1     = crs(rasters[[1]]),
  roads        = crs(roads),
  power_lines  = crs(power_lines),
  pipelines    = crs(pipelines),
  bridges      = crs(bridges),
  plantios     = crs(plantios),
  car          = crs(car),
  pda          = crs(pda),
  uniao        = crs(uniao)
)

# test if any differ from first
ref_crs <- layers[[1]]

for(nm in names(layers)) {
  same <- identical(layers[[nm]], ref_crs)
  message(nm, " CRS same as first raster? -> ", same)
}
ext(rasters[[1]])
plot(rasters[[1]])  # plot the first layer
plot(roads, col="white", add=TRUE)

### Functions ---------

#### 1. Reclass rasters ------
# Here, we reclass MapBiomas rasters (colecao 9) using new categories 
# NB: to see what codes refer to, check the "Codigos-da-legenda-colecao-9" file
reclass_fun <- function(xx) {
  forest <- c(3, 4, 5, 6, 49)
  notforest <- c(11, 12, 32, 29, 50, 23)
  agri <- c(15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21)
  water <- c(26, 33, 31)
  artificial <- c(24, 30, 25)
  
  xx[xx == 0] <- NA
  xx[xx %in% forest] <- 1
  xx[xx %in% notforest] <- 2
  xx[xx %in% agri] <- 3
  xx[xx %in% water] <- 4
  xx[xx %in% artificial] <- 5
  xx
}

#### 2. Temporal threshold ---------
# Function to exclude cells based on a temporal threshold
# If a pixel at year i is different from year i–1 AND different from year i+1, then year i is a “single-year noise” value → replace it with the value from year i–1.
# this is done on all land uses

apply_temporal_threshold <- function(rasters, years) {
  
  message("Applying temporal removal filter...")
  n <- length(rasters)
  
  # Extract pixel matrix (rows = pixels, cols = years)
  mat <- sapply(rasters, function(r) {
    v <- values(r)
    if (is.null(v)) v <- terra::values(r)
    return(v)
  })
  
  nr <- nrow(mat)
  
  # Loop through middle years
  for (i in 2:(n - 1)) {
    
    prev <- mat[, i - 1]
    curr <- mat[, i]
    nxt  <- mat[, i + 1]
    
    # NA-safe condition:
    # change only when curr differs from BOTH prev and next
    # AND prev/next are not NA
    spike <- !is.na(curr) &
      !is.na(prev) &
      !is.na(nxt) &
      curr != prev &
      curr != nxt
    
    mat[spike, i] <- prev[spike]
    
    message("Year ", years[i], ": fixed ", sum(spike), " cells.")
  }
  
  # Rebuild raster list
  out <- vector("list", n)
  for (i in seq_len(n)) {
    r <- rasters[[i]]
    r <- setValues(r, mat[, i])
    names(r) <- years[i]
    out[[i]] <- r
  }
  
  return(out)
}


#### 3. Overlay plantios on rasters depending on the reforestation year -----
# Here, we overlay the vector layer "plantios" (reforested areas) on the rasters, and give these reforested areas the value 1 (forest)
# The overlay is dependent on the condition "date_refor" (which corresponds to the year where the forest had fully grown)
# -> Make sure that the datasets has a variable named "date_refor" with numeric years

# Add plantios – set forest cells inside plantios polygons to chosen value
add_plantios <- function(raster, year, plantios, plantio_value) {
  # Keep only plantios with a valid reforestation year before or equal to current year
  plantios_valid <- plantios[!is.na(plantios$date_refor) & plantios$date_refor <= year, ]
  if (nrow(plantios_valid) > 0) {
    # Rasterize plantios and give them the value specified as argument
    pl_rast <- terra::rasterize(plantios_valid, raster, field = plantio_value, background = NA)
    # Only overwrite cells where pl_rast is not NA (i.e., inside plantios)
    raster[!is.na(pl_rast)] <- plantio_value
  }
  raster
}

#### 4. Dilatation-erosion --------
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

dilatation_erosion <- function(raster, seuil) { 
  # Step 1: Habitat mask 
  habitat <- app(raster, fun = function(v) ifelse(v == 1, 1, NA)) 
  # Step 2: Dilation 
  dist_hab <- terra::distance(habitat) 
  dilated_mask <- app(dist_hab, fun = function(v) ifelse(v > seuil, 1, NA)) 
  # Step 3: Erosion 
  dist_nonhab <- terra::distance(dilated_mask) 
  final_mask <- app(dist_nonhab, fun = function(v) ifelse(v > seuil, 1, NA)) 
  # Step 4: Apply mask to original raster 
  raster[!is.na(final_mask)] <- 1 
  
  raster 
  }


#### 5. Apply linear features -----
# Here, we overlay vector linear features to the rasters using a buffer width and assign the intersected cells a new numeric value
# -> Adapt the buffer width and value according to the linear feature (for instance: roads = 15m and 5 for artificial)
apply_linear_feature_single <- function(r, yr, feature, buffer_width, value, use_date = TRUE) {
  feat_valid <- if (use_date && "date_crea" %in% names(feature)) {
    feature[feature$date_crea <= yr, ]
  } else feature
  
  if (nrow(feat_valid) > 0) {
    feat_buff <- terra::buffer(feat_valid, width = buffer_width)
    feat_rast <- terra::rasterize(feat_buff, r, field = value, background = NA)
    r <- cover(feat_rast, r)
  }
  
  return(r)  # single SpatRaster
}


#### 6. Remove isolated cells -------------
# Here, we reclass isolated cells given their neighbors
# i.e., if a cell is isolated (with no other cells of the same value in 8 directions), then reclass as the main land use in the local neighborhood (8 directions)





#### 7. Assign values if intersects --------
# This function tests whether a raster value intersects a vector object, and reclassifies these cells with a new value if TRUE
assign_if_intersect <- function(raster, vect, target_values, new_value) {
  
  if(nrow(vect) == 0) return(raster)  # nothing to do
  
  vect_rast <- terra::rasterize(vect, raster, field = 1, background = NA)
  
  raster[raster %in% target_values & !is.na(vect_rast)] <- new_value
  
  raster
}

### Processing pipeline --------

#### 1. General classification ------
# First, we classify the rasters without distinguishing between forest categories
message("Running full pipeline...")

##### Step 1 – Reclassify ---------
message("Step 1: Reclassifying rasters...")
rasters_reclass <- lapply(seq_along(rasters), function(i) {
  message("  - Reclassifying raster ", i, " (year ", years[i], ")")
  app(rasters[[i]], reclass_fun)
})

plot(rasters_reclass[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))


##### Step 2 – Temporal threshold ------
message("Step 2: Applying temporal consistency filter...")

# Apply on rasters
rasters_filtered <- apply_temporal_threshold(rasters = rasters_reclass, years = years)

# Quick check
freq(rasters_reclass[[35]])
freq(rasters_filtered[[35]])
# Extract matrices
mat_before <- sapply(rasters_reclass, function(r) values(r))
mat_after <- sapply(rasters_filtered, function(r) values(r))

# Identify changed cells
changed_cells <- which(rowSums(mat_before != mat_after, na.rm = TRUE) > 0)

# Pick one changed cell
cell_id <- changed_cells[1000]

# Choose years before and after
i <- which(mat_before[cell_id, ] != mat_after[cell_id, ])[1]

# Makes sure to not choose the first and last rasters
if (i == 1 | i == length(rasters_reclass)) {
  stop("Chosen change is at first/last year — choose another cell.")
}

# Coordinate of the chosen cell
xy <- xyFromCell(rasters_reclass[[1]], cell_id)

# Create extent = 500 m radius around the cell
zoom_ext <- extent(
  xy[1] - 500, xy[1] + 500,
  xy[2] - 500, xy[2] + 500
)

# Plot
par(mfrow = c(2, 3), mar = c(2, 2, 2, 2))

# Row 1 — original
plot(crop(rasters_reclass[[i-1]], zoom_ext), main = paste0("Original ", names(rasters_reclass[[i-1]])))
points(xy[1], xy[2], pch=16, col="red")

plot(crop(rasters_reclass[[i]], zoom_ext), main = paste0("Original ", names(rasters_reclass[[i]])))
points(xy[1], xy[2], pch=16, col="red")

plot(crop(rasters_reclass[[i+1]], zoom_ext), main = paste0("Original ", names(rasters_reclass[[i+1]])))
points(xy[1], xy[2], pch=16, col="red")

# Row 2 — filtered
plot(crop(rasters_filtered[[i-1]], zoom_ext), main = paste0("Filtered ", names(rasters_filtered[[i-1]])))
points(xy[1], xy[2], pch=16, col="red")

plot(crop(rasters_filtered[[i]], zoom_ext), main = paste0("Filtered ", names(rasters_filtered[[i]])))
points(xy[1], xy[2], pch=16, col="red")

plot(crop(rasters_filtered[[i+1]], zoom_ext), main = paste0("Filtered ", names(rasters_filtered[[i+1]])))
points(xy[1], xy[2], pch=16, col="red")

par(mfrow=c(1,1))


##### Step 3 – Add plantios ------
message("Step 2: Adding plantios to rasters...")
rasters_plantios <- purrr::map2(rasters_filtered, years, function(r, yr) {
  message("  - Adding plantios for year ", yr)
  add_plantios(r, yr, plantios, plantio_value = 1) # Plantios are not differentiated from other forests
})

# Quick check for plantios overlay
year_sel <- 2004  # choose year to inspect
buff <- 1000
pl <- plantios[plantios$date_refor == year_sel, ]

if (nrow(pl) > 0) {
  years_to_plot <- c(year_sel - 1, year_sel, year_sel + 1)
  idx <- match(years_to_plot, years)
  valid <- !is.na(idx)
  years_to_plot <- years_to_plot[valid]
  idx <- idx[valid]
  ext_zoom <- ext(pl) + buff
  par(mfrow=c(1, length(idx)))
  for (j in seq_along(idx)) {
    yr <- years_to_plot[j]
    r_zoom <- crop(rasters_plantios[[idx[j]]], ext_zoom)
    plot(r_zoom, col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"), main=as.character(yr))
    plot(pl, border="black", lwd=2, add=TRUE)
  }
  par(mfrow=c(1,1))
} else {
  message("No plantios with date_refor = ", year_sel)
}

##### Step 4 – Dilatation-erosion ------
message("Step 3: Applying dilatation–erosion...")
rasters_dilate <- lapply(seq_along(rasters_plantios), function(i) {
  message("  - Processing dilatation–erosion for raster ", i, " (year ", years[i], ")")
  dilatation_erosion(rasters_plantios[[i]], seuil = 50)
})

# Quick check
plot(rasters_plantios[[36]], main=paste0("Before dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_dilate[[36]], main=paste0("After dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
freq(rasters_plantios[[36]])
freq(rasters_dilate[[36]])

##### Step 5 – Apply linear features on top of raster layers -----
message("Step 4: Applying linear features...")
rasters_lf <- purrr::map2(rasters_dilate, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin <- r
  r_lin <- apply_linear_feature_single(r_lin, yr, power_lines, buffer_width = 50, value = 2, use_date = FALSE)
  r_lin <- apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 15, value = 2, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, roads, buffer_width = 20, value = 5, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, bridges, buffer_width = 15, value = 1, use_date = TRUE) # Here, bridges are classified as forest
  r_lin
})

# Check
# Point coordinates
x_center <- 776928.1
y_center <- 7508359.8

# Buffer size
buffer_size <- 1500

# Create bounding box extent
zoom_ext <- ext(
  x_center - buffer_size, x_center + buffer_size,
  y_center - buffer_size, y_center + buffer_size
)

# Raster indices to visualize
subset_indices <- c(1, 17, 35)

# Loop over selected rasters
for (i in subset_indices) {
  # Crop rasters
  r_before <- crop(rasters_dilate[[i]], zoom_ext)
  r_after  <- crop(rasters_lf[[i]], zoom_ext)
  
  # Crop linear features
  roads_sub       <- crop(roads, zoom_ext)
  power_lines_sub <- crop(power_lines, zoom_ext)
  bridges_sub     <- crop(bridges, zoom_ext)
  pipelines_sub   <- crop(pipelines, zoom_ext)
  
  # Plot side by side
  par(mfrow=c(1,2))
  
  plot(r_before,
       main = paste0("Before linear features (", years[i], ")"),
       col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  plot(r_after,
       main = paste0("After linear features (", years[i], ")"),
       col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  par(mfrow=c(1,1))
}


##### Step 6 – Remove isolated forest cells -----
message("Removing isolated cells...")




# Quick summary
freq(rasters_final[[36]])

##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir <- here("outputs", "data", "MapBiomas", "Rasters_reclass")

# Export each raster with year in the filename
for (i in seq_along(rasters_final)) {
  year_i <- years[i]
  output_path <- file.path(output_dir, paste0("raster_reclass_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_final[[i]],
    filename = output_path,
    overwrite = TRUE, # Overwrite existing files or not
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}


#### 2. Forest categories ------
# Second, we classify the rasters and distinguish between forest categories

##### Step 1 – Add plantios ------------
message("Adding plantios to rasters...")
# We use the output of dilatation erosion process (step 3 above)
rasters_plantios <- purrr::map2(rasters_dilate, years, function(r, yr) {
  message("  - Adding plantios for year ", yr)
  add_plantios(r, yr, plantios, plantio_value = 10) # Plantios are not differentiated from other forests
})

# Quick check for plantios overlay
year_sel <- 2004  # choose year to inspect
buff <- 1000
pl <- plantios[plantios$date_refor == year_sel, ]

if (nrow(pl) > 0) {
  years_to_plot <- c(year_sel - 1, year_sel, year_sel + 1)
  idx <- match(years_to_plot, years)
  valid <- !is.na(idx)
  years_to_plot <- years_to_plot[valid]
  idx <- idx[valid]
  ext_zoom <- ext(pl) + buff
  par(mfrow=c(1, length(idx)))
  for (j in seq_along(idx)) {
    yr <- years_to_plot[j]
    r_zoom <- crop(rasters_plantios[[idx[j]]], ext_zoom)
    plot(r_zoom, col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"), main=as.character(yr))
    plot(pl, border="black", lwd=2, add=TRUE)
  }
  par(mfrow=c(1,1))
} else {
  message("No plantios with date_refor = ", year_sel)
}

##### Step 2 – Apply linear features on top of raster layers -----
message("Applying linear features...")
rasters_lm <- purrr::map2(rasters_plantios, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin <- r
  r_lin <- apply_linear_feature_single(r_lin, yr, power_lines, buffer_width = 50, value = 2, use_date = FALSE)
  r_lin <- apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 15, value = 2, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, roads, buffer_width = 20, value = 5, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, bridges, buffer_width = 15, value = 15, use_date = TRUE) # Here, bridges are distinguished from forest
  r_lin
})

# Check
# Point coordinates
x_center <- 776928.1
y_center <- 7508359.8

# Buffer size
buffer_size <- 1500

# Create bounding box extent
zoom_ext <- ext(
  x_center - buffer_size, x_center + buffer_size,
  y_center - buffer_size, y_center + buffer_size
)

# Raster indices to visualize
subset_indices <- c(1, 17, 35)

# Loop over selected rasters
for (i in subset_indices) {
  # Crop rasters
  r_before <- crop(rasters_plantios[[i]], zoom_ext)
  r_after  <- crop(rasters_lm[[i]], zoom_ext)
  
  # Crop linear features
  roads_sub       <- crop(roads, zoom_ext)
  power_lines_sub <- crop(power_lines, zoom_ext)
  bridges_sub     <- crop(bridges, zoom_ext)
  pipelines_sub   <- crop(pipelines, zoom_ext)
  
  # Plot side by side
  par(mfrow=c(1,2))
  
  plot(r_before,
       main = paste0("Before linear features (", years[i], ")"),
       col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  plot(r_after,
       main = paste0("After linear features (", years[i], ")"),
       col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse"))
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  par(mfrow=c(1,1))
}

##### Step 3 – Reassign forest values -----
message("Classifying forests...")

rasters_final2 <- lapply(rasters_lm, function(r){
  # 1) plantios inside reserves become 11
  r <- assign_if_intersect(r, rbind(uniao,pda), target_values = 10, new_value = 11)
  
  # 2) plantios inside CAR become 12
  r <- assign_if_intersect(r, car, target_values = 10, new_value = 12)
  
  # 3) reserves override original forest
  r <- assign_if_intersect(r, rbind(uniao,pda), target_values = 1, new_value = 13)
  
  # 4) private override original forest
  r <- assign_if_intersect(r, car, target_values = 1, new_value = 14)
  
  r
})

# Check
# pick the rasters
r_before <- rasters_lm[[36]]
r_after  <- rasters_final2[[36]]

# zoom extent from reserve or car region
zoom_ext <- ext(pda) + 5000  # +2 km buffer

# crop rasters
rb <- crop(r_before, zoom_ext)
ra <- crop(r_after,  zoom_ext)

# to sf for plotting
car_sf = sf::st_as_sf(car)

# define a color table
vals_after <- c(1,2,3,4,5,10,11,12,13,14,15)
cols_after <- c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", 
                "lightgreen", "chartreuse", "darkseagreen", "darkolivegreen","darkkhaki", "yellow")

terra::coltab(ra) <- cbind(vals_after, cols_after)

par(mfrow=c(1,2))

plot(rb,
     main="BEFORE",
     col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "lightgreen", "chartreuse"))

plot(pda, add=TRUE, border="yellow", lwd=2)
plot(car_sf, add=TRUE, col=scales::alpha("orange",0.5), border=NA)

plot(ra,
     main="AFTER")  # coltab already defines colors

plot(pda, add=TRUE, border="yellow", lwd=2)
plot(car_sf, add=TRUE, col=scales::alpha("orange",0.5), border=NA)

par(mfrow=c(1,1))


# Quick check
freq(rasters_final[[36]])
freq(rasters_final2[[1]]) # there should not be any 10, 11, or 12 values (no plantios at that time)
freq(rasters_final2[[36]])

##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir <- here("outputs", "data", "MapBiomas", "Rasters_reclass_forest_cat")

# Export each raster with year in the filename
for (i in seq_along(rasters_final2)) {
  year_i <- years[i]
  output_path <- file.path(output_dir, paste0("raster_reclass_forest_cat_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_final2[[i]],
    filename = output_path,
    overwrite = TRUE, # Overwrite existing files or not
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}
