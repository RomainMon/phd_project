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
plantios_sf = sf::st_as_sf(plantios)
car = vect(here("data", "geo", "IBGE", "cadastro_car", "AREA_IMOVEL_RJ_2024", "AREA_IMOVEL_bbox.shp"))
pda = vect(here("data", "geo", "MMA", "protected_areas", "ucs", "poco_das_antas.shp"))
uniao = vect(here("data", "geo", "MMA", "protected_areas", "ucs", "uniao.shp"))
tres_picos = vect(here("data", "geo", "MMA", "protected_areas", "ucs", "tres_picos.shp"))
rppn = vect(here("data", "geo", "AMLD", "RPPN", "RPPN_RJ.shp"))
bbox = vect(here("data", "geo", "BBOX", "sampling_units_bbox_31983.shp"))

### Quick check -------
# list all objects that must share CRS
layers <- list(
  raster_1 = crs(rasters[[1]]),
  roads = crs(roads),
  power_lines = crs(power_lines),
  pipelines = crs(pipelines),
  bridges = crs(bridges),
  plantios = crs(plantios),
  car = crs(car),
  pda = crs(pda),
  uniao = crs(uniao),
  rppn = crs(rppn),
  tres_picos = crs(tres_picos)
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

#### Reclass rasters ------
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

#### Temporal filter ---------
# Function to exclude cells based on a temporal threshold
# If a pixel at year i is different from year i–1 AND different from year i+1, then year i is a “single-year noise” value
# We replace the cell with the value from year i–1.
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
    nxt <- mat[, i + 1]
    
    # NA-safe condition:
    # change only when curr differs from BOTH prev and next
    # AND prev/next are not NA
    spike <- !is.na(curr) & !is.na(prev) & !is.na(nxt) &
      curr != prev & curr != nxt
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

#### Spatial filter -------------
# Here, we reclass isolated cells given their neighbors
# A pixel at year i is reclassified only if all conditions are true:
# It has no neighbors with the same value in the 8-neighborhood.
# It is replaced by the majority value among its neighbors (excluding NA).

remove_isolated_cells <- function(rasters, years) {
  
  message("Removing isolated cells...")
  n <- length(rasters)
  if (n != length(years)) stop("rasters and years must have same length")
  
  # Ensure integer rasters (NA preserved)
  rasters <- lapply(rasters, function(r) {
    r_int <- clamp(r, -Inf, Inf, values = TRUE)  # keep structure
    values(r_int) <- as.integer(values(r))
    r_int
  })
  
  out_list <- vector("list", n)
  
  # 3x3 window (center included)
  w <- matrix(1, 3, 3)
  center_index <- ceiling(length(w) / 2)
  
  for (i in seq_len(n)) {
    
    r <- rasters[[i]]
    message(" Processing year ", years[i], " (", i, "/", n, ")")
    vals <- values(r)
    
    # 1) Count neighbors equal to center (subtract 1 to exclude center)
    same_count_r <- terra::focal(
      r, w,
      fun = function(x, ...) {
        centre <- x[ceiling(length(x) / 2)]
        sum(x == centre, na.rm = TRUE) - 1
      },
      na.policy = "omit",
      filename = "",
      pad = TRUE
    )
    
    same_count <- values(same_count_r)
    
    # Spatial isolation
    is_isolated <- !is.na(vals) & (same_count == 0)
    target_idx <- which(is_isolated)
    
    message("  -> flagged pixels: ", length(target_idx))
    
    if (length(target_idx) == 0) {
      out_list[[i]] <- r
      next
    }
    
    # 2) Compute neighbor MODE (excluding center)
    neighbor_mode_r <- terra::focal(
      r, w,
      fun = function(x, ...) {
        center_pos <- ceiling(length(x) / 2)
        neighs <- x[-center_pos]
        neighs <- neighs[!is.na(neighs)]
        if (length(neighs) == 0) return(NA_integer_)
        tbl <- table(neighs)
        as.integer(names(tbl)[which.max(tbl)])
      },
      na.policy = "omit",
      filename = "",
      pad = TRUE
    )
    
    neighbor_mode <- values(neighbor_mode_r)
    
    # 3) Replacement = neighbor mode
    replacement_vals <- neighbor_mode[target_idx]
    
    # If neighbor mode is NA, keep original value
    fixed_vals <- vals
    fixed_vals[target_idx] <- replacement_vals
    r_fixed <- setValues(r, fixed_vals)
    names(r_fixed) <- as.character(years[i])
    
    out_list[[i]] <- r_fixed
  }
  
  return(out_list)
}

#### Overlay plantios on rasters depending on the reforestation year -----
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


#### Apply linear features -----
# Here, we overlay vector linear features to the rasters using a buffer width and assign the intersected cells a new numeric value
# -> Adapt the buffer width and value according to the linear feature (for instance: roads = 15m and 5 for artificial)
apply_linear_feature_single <- function(r, yr, feature, buffer_width, value, use_date) {
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



#### Assign values if intersects --------
# This function tests whether a raster value intersects a vector object, and reclassifies these cells with a new value if TRUE
assign_if_intersect <- function(raster, vect, target_values, new_value) {
  
  if(nrow(vect) == 0) return(raster)  # nothing to do
  
  vect_rast <- terra::rasterize(vect, raster, field = 1, background = NA)
  
  raster[raster %in% target_values & !is.na(vect_rast)] <- new_value
  
  raster
}

#### Assign values if intersects and attribute matches --------
# This function assigns a value if intersect AND attribute matches AND according to the year
assign_if_intersect_attr_year <- function(raster, year, vect,
                                          year_field,
                                          attr, attr_value,
                                          target_values, new_value){
  
  if (nrow(vect) == 0) return(raster)
  
  # keep only vectors already present at this year (such as reforested areas)
  vect_year <- vect[!is.na(vect[[year_field]]) & vect[[year_field]] <= year, ]
  
  if (nrow(vect_year) == 0) return(raster)
  
  # keep only desired attribute
  vect_sub <- vect_year[vect_year[[attr]] == attr_value, ]
  
  if (nrow(vect_sub) == 0) return(raster)
  
  vect_rast <- terra::rasterize(vect_sub, raster, field = 1, background = NA)
  
  raster[raster %in% target_values & !is.na(vect_rast)] <- new_value
  
  raster
}

# This function assigns a value if intersect AND according to the year
assign_if_intersect_year <- function(raster, year, vect, year_field,
                                     target_values, new_value){
  
  if (nrow(vect) == 0) return(raster)
  
  vect_year <- vect[!is.na(vect[[year_field]]) & vect[[year_field]] <= year, ]
  
  if (nrow(vect_year) == 0) return(raster)
  
  vect_rast <- terra::rasterize(vect_year, raster, field = 1, background = NA)
  
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

# Check
plot(rasters_reclass[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))


##### Step 2 – Temporal filter ------
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
plot(crop(rasters_reclass[[i-1]], zoom_ext), main = paste0("Original ", years[i-1]))
points(xy[1], xy[2], pch=16, col="red")

plot(crop(rasters_reclass[[i]], zoom_ext), main = paste0("Original ", years[i]))
points(xy[1], xy[2], pch=16, col="red")

plot(crop(rasters_reclass[[i+1]], zoom_ext), main = paste0("Original ", years[i+1]))
points(xy[1], xy[2], pch=16, col="red")

# Row 2 — filtered
plot(crop(rasters_filtered[[i-1]], zoom_ext), main = paste0("Filtered ", names(rasters_filtered[[i-1]])))
points(xy[1], xy[2], pch=16, col="red")

plot(crop(rasters_filtered[[i]], zoom_ext), main = paste0("Filtered ", names(rasters_filtered[[i]])))
points(xy[1], xy[2], pch=16, col="red")

plot(crop(rasters_filtered[[i+1]], zoom_ext), main = paste0("Filtered ", names(rasters_filtered[[i+1]])))
points(xy[1], xy[2], pch=16, col="red")

par(mfrow=c(1,1))


##### Step 3 – Spatial filter -----
message("Step 3: Removing isolated cells...")

# apply to all rasters
rasters_spatial_clean <- remove_isolated_cells(rasters_filtered, years)

## Check
freq(rasters_filtered[[1]])
freq(rasters_spatial_clean[[1]])

# Extract matrices
mat_before <- sapply(rasters_filtered[[1]], function(r) values(r))
mat_after  <- sapply(rasters_spatial_clean[[1]], function(r) values(r))

# Identify changed cells
changed_cells <- which(rowSums(mat_before != mat_after, na.rm = TRUE) > 0)

# Pick one changed cell
cell_id <- changed_cells[100]   # or any index you want

# Identify one raster where the value changed
i <- which(mat_before[cell_id, ] != mat_after[cell_id, ])[1]

# Coordinates of the cell
xy <- xyFromCell(rasters_filtered[[i]], cell_id)

# Zoom window (500 m)
zoom_ext <- extent(
  xy[1] - 500, xy[1] + 500,
  xy[2] - 500, xy[2] + 500
)

# Plot ONLY before vs after
par(mfrow = c(1, 2), mar = c(2, 2, 2, 2))

### BEFORE ###
plot(crop(rasters_filtered[[i]], zoom_ext),
     main = paste0("Before ", names(rasters_filtered[[i]])))
points(xy[1], xy[2], pch = 16, col = "red")

### AFTER ###
plot(crop(rasters_spatial_clean[[i]], zoom_ext),
     main = paste0("After ", names(rasters_spatial_clean[[i]])))
points(xy[1], xy[2], pch = 16, col = "red")

par(mfrow = c(1,1))



##### Step 4 – Add plantios ------
message("Step 4: Adding plantios to rasters...")
rasters_plantios <- purrr::map2(rasters_spatial_clean, years, function(r, yr) {
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

##### Step 5 – Dilatation-erosion ------
message("Step 5: Applying dilatation–erosion...")
rasters_dilate <- lapply(seq_along(rasters_plantios), function(i) {
  message("  - Processing dilatation–erosion for raster ", i, " (year ", years[i], ")")
  dilatation_erosion(rasters_plantios[[i]], seuil = 50)
})

# Quick check
plot(rasters_plantios[[36]], main=paste0("Before dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_dilate[[36]], main=paste0("After dilatation–Erosion ", years[36]), col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
freq(rasters_plantios[[36]])
freq(rasters_dilate[[36]])



##### Step 6 – Apply linear features on top of raster layers -----
message("Step 6: Applying linear features...")
rasters_lf <- purrr::map2(rasters_dilate, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin <- r
  r_lin <- apply_linear_feature_single(r_lin, yr, power_lines, buffer_width = 50, value = 2, use_date = FALSE)
  r_lin <- apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 15, value = 2, use_date = TRUE)
  r_lin <- apply_linear_feature_single(r_lin, yr, roads, buffer_width = 20, value = 5, use_date = TRUE)
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

##### Step 7 – Crop again (as linear features are above the extent) -----
rasters_final <- lapply(rasters_lf, function(r) {
  r_cropped <- crop(r, bbox) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked <- mask(r_cropped, bbox) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})

plot(rasters_lf[[36]], col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_final[[36]], col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))


##### Visual demo of all steps ---------
# Quick final summary
freq(rasters_final[[36]])

# Central coordinates
x_center <- 778151.2
y_center <- 7508862.8

# Zoom window
buffer_size <- 1200
zoom_ext <- extent(
  x_center - buffer_size, x_center + buffer_size,
  y_center - buffer_size, y_center + buffer_size
)

# Choose which raster/year to illustrate
index_demo <- 35

# Step 1 - Reclassify
r_step1 <- crop(rasters_reclass[[index_demo]], zoom_ext)

# Step 2 - Temporal filter
r_step2 <- crop(rasters_filtered[[index_demo]], zoom_ext)

# Step 3 - Spatial filter
r_step3 <- crop(rasters_spatial_clean[[index_demo]], zoom_ext)

# Step 4 - Add plantios
r_step4 <- crop(rasters_plantios[[index_demo]], zoom_ext)

# Step 5 - Dilatation erosion
r_step5 <- crop(rasters_dilate[[index_demo]], zoom_ext)

# Step 6 - Linear features
r_step6 <- crop(rasters_lf[[index_demo]], zoom_ext)

roads_sub       <- crop(roads, zoom_ext)
power_lines_sub <- crop(power_lines, zoom_ext)
bridges_sub     <- crop(bridges, zoom_ext)
pipelines_sub   <- crop(pipelines, zoom_ext)


# Plot all steps together

png(here("outputs","plot","01e_reclass_rasters_demo.png"), width = 3000, height = 2000, res = 300)

par(mfrow = c(2, 3), mar = c(2,2,2,2))

# Step 1
plot(r_step1, main = paste0("Step 1 – Reclassify (", years[index_demo], ")"),
     col=c("#32a65e", "#FFFFB2", "#d4271e"))

# Step 2
plot(r_step2, main = "Step 2 – Temporal filter",
     col=c("#32a65e", "#FFFFB2", "#d4271e"))

# Step 3
plot(r_step3, main = "Step 3 – Spatial filter",
     col=c("#32a65e", "#FFFFB2", "#d4271e"))

# Step 4
plot(r_step4, main = "Step 4 – Add plantios",
     col=c("#32a65e", "#FFFFB2", "#d4271e"))

# Step 5
plot(r_step5, main = "Step 5 – Dilatation–Erosion",
     col=c("#32a65e", "#FFFFB2", "#d4271e"))

# Step 6
plot(r_step6, main = "Step 6 – Linear features",
     col=c("#32a65e", "#ad975a", "#FFFFB2", "#d4271e"))
plot(roads_sub,       add=TRUE, col="black")
plot(power_lines_sub, add=TRUE, col="red")
plot(bridges_sub,     add=TRUE, col="blue")
plot(pipelines_sub,   add=TRUE, col="orange")

par(mfrow = c(1,1))
dev.off()


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
# We classify the rasters and distinguish between forest categories

message("Classifying forests depending on their legal status...")

rasters_forest_cat <- lapply(rasters_final, function(r){
  # 1) forests inside private properties (CAR) become 11
  r <- assign_if_intersect(r, car, target_values = 1, new_value = 11)
  
  # 2) forests inside private reserves become 12
  r <- assign_if_intersect(r, rppn, target_values = c(1,11), new_value = 12)
  
  # 3) forests inside public conservation units become 13
  r <- assign_if_intersect(r, rbind(uniao,pda,tres_picos), target_values = c(1,11), new_value = 13)
  
  # 4) remaining forests (value = 1) become unknown status = 100
  r[r == 1] <- 100
  r
})

# Check
# pick the rasters
r_before <- rasters_final[[36]]
r_after  <- rasters_forest_cat[[36]]
plot(rasters_forest_cat[[36]], col = c("#ad975a", "#FFFFB2", "#0000FF", "#d4271e", 
                                 "darkolivegreen", "darkseagreen", "lightgreen", "#32a65e"))
freq(rasters_forest_cat[[36]])

# zoom extent from reserve or car region
zoom_ext <- ext(pda) + 5000

# crop rasters
rb <- crop(r_before, zoom_ext)
ra <- crop(r_after,  zoom_ext)

# to sf for plotting
car_sf = sf::st_as_sf(car)

# define a color table
par(mfrow=c(1,2))

plot(rb,
     main="BEFORE",
     col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))

plot(pda, add=TRUE, border="yellow", lwd=2)
plot(car_sf, add=TRUE, col=scales::alpha("orange",0.5), border=NA)

plot(ra,
     main="AFTER",
     col = c("#ad975a", "#FFFFB2", "#0000FF", "#d4271e", 
       "darkolivegreen", "darkseagreen", "lightgreen", "#32a65e"))

plot(pda, add=TRUE, border="yellow", lwd=2)
plot(car_sf, add=TRUE, col=scales::alpha("orange",0.5), border=NA)

par(mfrow=c(1,1))


##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir <- here("outputs", "data", "MapBiomas", "Rasters_reclass_forest_cat")

# Export each raster with year in the filename
for (i in seq_along(rasters_forest_cat)) {
  year_i <- years[i]
  output_path <- file.path(output_dir, paste0("raster_reclass_forest_cat_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_forest_cat[[i]],
    filename = output_path,
    overwrite = TRUE, # Overwrite existing files or not
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}

#### 3. Conservation categories ------
# We classify the rasters and distinguish between types of connectivity or habitat restoration

# Reclassify plantios
rasters_restor_cat <- purrr::map2(rasters_final, years, function(r, yr){
  
  # 1) forest intersecting plantios with Ecologia == "Corredor" → 51
  r <- assign_if_intersect_attr_year(r, yr, plantios,
                                     year_field = "date_refor",
                                     attr = "Ecologia",
                                     attr_value = "Corredor",
                                     target_values = 1,
                                     new_value = 51)
  
  # 2) remaining forest intersecting any plantios → 50
  r <- assign_if_intersect_year(r, yr, plantios,
                                year_field = "date_refor",
                                target_values = 1,
                                new_value = 50)
  
  r
})

# Apply road overpasses
rasters_cons_cat <- purrr::map2(rasters_restor_cat, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin <- r
  r_lin <- apply_linear_feature_single(r_lin, yr, bridges, buffer_width = 15, value = 52, use_date = TRUE)
  r_lin
})

# Check
plot(rasters_cons_cat[[36]], col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", 
                                     "orange", "purple", "yellow"))
freq(rasters_cons_cat[[1]])
freq(rasters_cons_cat[[36]])

# pick the rasters
r_before <- rasters_final[[36]]
r_after  <- rasters_cons_cat[[36]]

# zoom extent from reserve or car region
zoom_ext <- ext(pda) + 500

# crop rasters
rb <- crop(r_before, zoom_ext)
ra <- crop(r_after,  zoom_ext)
par(mfrow=c(1,2))

plot(rb,
     main="BEFORE",
     col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))
plot(plantios, add=TRUE, border="white", lwd=1)

plot(ra,
     main="AFTER",
     col = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", 
             "orange", "purple", "yellow"))
plot(plantios, add=TRUE, border="white", lwd=1)

par(mfrow=c(1,1))

##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir <- here("outputs", "data", "MapBiomas", "Rasters_reclass_cons_cat")

# Export each raster with year in the filename
for (i in seq_along(rasters_cons_cat)) {
  year_i <- years[i]
  output_path <- file.path(output_dir, paste0("raster_reclass_cons_cat_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_cons_cat[[i]],
    filename = output_path,
    overwrite = TRUE, # Overwrite existing files or not
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}