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
roads_sf = sf::st_as_sf(roads)
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
layers = list(
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
ref_crs = layers[[1]]

for(nm in names(layers)) {
  same = identical(layers[[nm]], ref_crs)
  message(nm, " CRS same as first raster? -> ", same)
}
ext(rasters[[1]])
plot(rasters[[1]])  # plot the first layer
plot(roads, col="white", add=TRUE)
freq(rasters[[36]])


### Functions ---------

#### Reclass rasters ------
# Here, we reclass MapBiomas rasters using new categories 
# NB: to see what codes refer to, check the "Codigos-da-legenda-colecao-10" file*
# Forest (1) includes: forest formation, wooded sandbank vegetation
# Other non-forest habitats include (2) include: beach, dune and sand spot, rocky outcrop, hypersaline tidal flat
# Agriculture (3) includes: pasture, mosaic of uses, soybean, other temporary crops, forest plantation
# Water (4) includes: aquaculture, river, lake and ocean
# Artificial and urban areas include (5): urban areas, other non vegetated areas, mining
# Wetlands (6) include: mangroves and wetlands
reclass_fun <- function(xx) {
  forest <- c(3, 4, 6, 49)
  notforest <- c(12, 32, 29, 50, 23)
  wetlands <- c(5, 11)
  agri <- c(15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21)
  water <- c(26, 33, 31)
  artificial <- c(24, 30, 25)
  
  xx[xx == 0] <- NA
  xx[xx %in% forest] <- 1
  xx[xx %in% notforest] <- 2
  xx[xx %in% wetlands] <- 3
  xx[xx %in% agri] <- 4
  xx[xx %in% water] <- 5
  xx[xx %in% artificial] <- 6
  xx
}

#### Temporal filter ---------
# Function to exclude cells based on a temporal threshold
# Rule 1: If a pixel at year i is different from year i–1 AND different from year i+1, then year i is a “single-year noise” value -> We replace the cell with the value from year i–1 (all LULC)
# Rule 2 (Caballero et al. 2023): If a pixel of natural vegetation (forest, wetlands, other non forest formation) becomes anthropic for 1 or 2 years, and then back to its previous value -> reclassify this sub-sequence into its original value
# Rule 3 (Caballero et al. 2023): If a pixel of anthropic cover (agriculture or not vegetated) becomes natural vegetation for 1-3 years, and then back to anthropic -> reclassify the sub-sequence into the original anthropic cover

apply_temporal_threshold <- function(rasters, years) {
  message("Applying temporal removal filter...")
  n = length(rasters)
  
  NV = c(1, 2, 3)  # natural vegetation
  AN = c(4, 6) # anthropic cover
  
  mat = sapply(rasters, function(r) {
    v = values(r)
    if (is.null(v)) v = terra::values(r)
    v
  })
  
  nr = nrow(mat)
  
  # Rule 1: single-year spike
  for (i in 2:(n - 1)) {
    prev = mat[, i - 1]
    curr = mat[, i]
    nxt  = mat[, i + 1]
    
    # Previous, current and subsequent value is not NA AND current value is different then the previous and the subsequent one
    spike = !is.na(curr) & !is.na(prev) & !is.na(nxt) & curr != prev & curr != nxt
    
    # The flagged pixel gets its previous value
    mat[spike, i] = prev[spike]
    message("Year ", years[i], ": fixed ", sum(spike), " spike cells.")
  }
  
  # Rule 2: NV -> AN (1–2y) -> NV
  for (i in 2:(n - 2)) {
    
    p = mat[, i - 1]
    c1 = mat[, i]
    c2 = mat[, i + 1]
    c3 = mat[, i + 2]
    
    # 1-year anthropic gap: previous is NV, subsequent pixel is anthropic, and pixel returns to the same NV 
    idx1 = !is.na(p) & !is.na(c1) & !is.na(c2) & p %in% NV & c1 %in% AN & c2 == p
    
    # The flagged pixel gets the initial value
    mat[idx1, i] = p[idx1]
    
    # 2-year anthropic gap: previous is NV, subsequent pixels are anthropic (2 years), and pixel returns to the same NV
    idx2 = !is.na(p) & !is.na(c1) & !is.na(c2) & !is.na(c3) & p %in% NV & c1 %in% AN & c2 %in% AN & c3 == p
    
    # The flagged pixel gets the initial value
    mat[idx2, i:(i + 1)] = p[idx2]
    
    message("Year ", years[i], ": fixed ",
            sum(idx1) + sum(idx2), " NV→AN→NV cells.")
  }
  
  # Rule 3: AN -> NV (1–3y) -> AN
  for (i in 2:(n - 3)) {
    
    p = mat[, i - 1]
    c1 = mat[, i]
    c2 = mat[, i + 1]
    c3 = mat[, i + 2]
    c4 = mat[, i + 3]
    
    # 1-year NV gap: pixel is anthropic, becomes NV, then anthropic again
    idx1 <- !is.na(p) & !is.na(c1) & !is.na(c2) & p %in% AN & c1 %in% NV & c2 == p
    
    # Flagged pixel gets the previous value
    mat[idx1, i] = p[idx1]
    
    # 2-year NV gap: pixel is anthropic, becomes NV for 2 years, then anthropic again
    idx2 <- !is.na(p) & !is.na(c1) & !is.na(c2) & !is.na(c3) & p %in% AN & c1 %in% NV & c2 %in% NV & c3 == p
    
    # Flagged pixel gets the previous value
    mat[idx2, i:(i + 1)] = p[idx2]
    
    # 3-year NV gap: pixel is anthropic, becomes NV for 3 years, then anthropic again
    idx3 = !is.na(p) & !is.na(c1) & !is.na(c2) & !is.na(c3) & !is.na(c4) & p %in% AN & c1 %in% NV & c2 %in% NV & c3 %in% NV & c4 == AN
    
    # Flagged pixel gets the previous value
    mat[idx3, i:(i + 2)] = p[idx3]
    
    message("Year ", years[i], ": fixed ",
            sum(idx1) + sum(idx2) + sum(idx3), " AN→NV→AN cells.")
  }
  
  # Rebuild rasters
  out = vector("list", n)
  for (i in seq_len(n)) {
    r = rasters[[i]]
    r = setValues(r, mat[, i])
    names(r) = years[i]
    out[[i]] = r
  }
  
  return(out)
}


#### Spatial filter -------------
# Here, we reclass isolated cells given their neighbors
# A pixel at year i is reclassified only if it has no neighbors with the same value in the 8-neighborhood.
# -> It is replaced by the majority value among its neighbors (excluding NA).

remove_isolated_cells = function(rasters, years) {
  
  message("Removing isolated cells (spatial filter)...")
  n = length(rasters)
  if (n != length(years)) stop("rasters and years must have same length")
  
  w = matrix(1, 3, 3)
  center = 5  # position in 3x3 vector
  
  out_list = vector("list", n)
  
  focal_fun = function(x, ...) {
    cen = x[center]
    if (is.na(cen)) return(NA_integer_)
    
    # count how many times center appears (including itself)
    s = sum(x == cen, na.rm = TRUE)
    
    # if isolated (only itself)
    if (s == 1) {
      neighs = x[-center]
      neighs = neighs[!is.na(neighs)]
      if (length(neighs) == 0) return(cen)
      
      tb = table(neighs)
      return(as.integer(names(tb)[which.max(tb)]))
    }
    
    cen
  }
  
  for (i in seq_len(n)) {
    message(" Processing year ", years[i], " (", i, "/", n, ")")
    
    r = rasters[[i]]
    
    r_fixed = terra::focal(
      r, w = w, fun = focal_fun,
      na.policy = "omit", pad = TRUE, filename = ""
    )
    
    names(r_fixed) = as.character(years[i])
    out_list[[i]] = r_fixed
  }
  
  out_list
}

#### Overlay plantios on rasters depending on the reforestation year -----
# Here, we overlay the vector layer "plantios" (reforested areas) on the rasters, and give these reforested areas the value 1 (forest)
# The overlay is dependent on the condition "date_refor" (which corresponds to the year where the forest had fully grown)
# -> Make sure that the datasets has a variable named "date_refor" with numeric years

# Add plantios – set forest cells inside plantios polygons to chosen value
add_plantios = function(raster, year, plantios, plantio_value) {
  # Keep only plantios with a valid reforestation year before or equal to current year
  plantios_valid = plantios[!is.na(plantios$date_refor) & plantios$date_refor <= year, ]
  if (nrow(plantios_valid) > 0) {
    # Rasterize plantios and give them the value specified as argument
    pl_rast = terra::rasterize(plantios_valid, raster, field = plantio_value, background = NA)
    # Only overwrite cells where pl_rast is not NA (i.e., inside plantios)
    raster[!is.na(pl_rast)] = plantio_value
  }
  raster
}


#### Apply linear features -----
# Here, we overlay vector linear features to the rasters using a buffer width and assign the intersected cells a new numeric value
# -> Adapt the buffer width and value according to the linear feature (for instance: roads = 15m and 5 for artificial)
apply_linear_feature_single = function(r, yr, feature, buffer_width, value, use_date) {
  feat_valid = if (use_date && "date_crea" %in% names(feature)) {
    feature[feature$date_crea <= yr, ]
  } else feature
  
  if (nrow(feat_valid) > 0) {
    feat_buff = terra::buffer(feat_valid, width = buffer_width)
    feat_rast = terra::rasterize(feat_buff, r, field = value, background = NA)
    r = cover(feat_rast, r)
  }
  
  return(r)  # single SpatRaster
}



#### Assign values if intersects --------
# This function tests whether a raster value intersects a vector object, and reclassifies these cells with a new value if TRUE
assign_if_intersect = function(raster, vect, target_values, new_value) {
  if(nrow(vect) == 0) return(raster)  # nothing to do
  vect_rast = terra::rasterize(vect, raster, field = 1, background = NA)
  raster[raster %in% target_values & !is.na(vect_rast)] = new_value
  raster
}

# This function assigns a value if intersect AND according to the year
assign_if_intersect_year = function(raster, year, vect, year_field,
                                     target_values, new_value){
  if (nrow(vect) == 0) return(raster)
  # keep only vectors already present at this year
  vect_year = vect[!is.na(vect[[year_field]]) & vect[[year_field]] <= year, ]
  if (nrow(vect_year) == 0) return(raster)
  vect_rast = terra::rasterize(vect_year, raster, field = 1, background = NA)
  raster[raster %in% target_values & !is.na(vect_rast)] = new_value
  raster
}

# This function assigns a value if intersect AND attribute matches AND according to the year
assign_if_intersect_attr_year <- function(raster, year, vect,
                                          year_field,
                                          attr, attr_value,
                                          target_values, new_value){
  
  if (nrow(vect) == 0) return(raster)
  # keep only vectors already present at this year
  vect_year = vect[!is.na(vect[[year_field]]) & vect[[year_field]] <= year, ]
  if (nrow(vect_year) == 0) return(raster)
  # keep only desired attribute
  vect_sub = vect_year[vect_year[[attr]] == attr_value, ]
  if (nrow(vect_sub) == 0) return(raster)
  vect_rast = terra::rasterize(vect_sub, raster, field = 1, background = NA)
  raster[raster %in% target_values & !is.na(vect_rast)] = new_value
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
plot(rasters_reclass[[1]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
freq(rasters_reclass[[36]])

##### Step 2 – Temporal filter ------
message("Step 2: Applying temporal consistency filter...")

# Apply on rasters
rasters_filtered = apply_temporal_threshold(rasters = rasters_reclass, years = years)

# Quick check
freq(rasters_reclass[[35]])
freq(rasters_filtered[[35]])

## Plot

mat_before = sapply(rasters_reclass, terra::values)
mat_after = sapply(rasters_filtered, terra::values)

# cells that changed at least once
changed_cells = which(rowSums(mat_before != mat_after, na.rm = TRUE) > 0)

# pick a valid change (not first/last year)
set.seed(1)

repeat {
  cell_id = sample(changed_cells, 1)
  diffs = which(mat_before[cell_id, ] != mat_after[cell_id, ])
  i = diffs[1]
  if (i > 1 && i < length(rasters_reclass)) break
}

message("Temporal change check — cell ", cell_id, ", year ", years[i])

xy = terra::xyFromCell(rasters_reclass[[1]], cell_id)

buf = 1500  # map units
zoom_ext = terra::ext(
  xy[1] - buf, xy[1] + buf,
  xy[2] - buf, xy[2] + buf
)

par(mfrow = c(2, 3), mar = c(2, 2, 2, 2))

# before
plot(terra::crop(rasters_reclass[[i-1]], zoom_ext),
     main = paste0("Orig ", years[i-1]))
points(xy[1], xy[2], pch = 16, col = "red")

plot(terra::crop(rasters_reclass[[i]], zoom_ext),
     main = paste0("Orig ", years[i]))
points(xy[1], xy[2], pch = 16, col = "red")

plot(terra::crop(rasters_reclass[[i+1]], zoom_ext),
     main = paste0("Orig ", years[i+1]))
points(xy[1], xy[2], pch = 16, col = "red")

# after
plot(terra::crop(rasters_filtered[[i-1]], zoom_ext),
     main = paste0("TempFilt ", years[i-1]))
points(xy[1], xy[2], pch = 16, col = "red")

plot(terra::crop(rasters_filtered[[i]], zoom_ext),
     main = paste0("TempFilt ", years[i]))
points(xy[1], xy[2], pch = 16, col = "red")

plot(terra::crop(rasters_filtered[[i+1]], zoom_ext),
     main = paste0("TempFilt ", years[i+1]))
points(xy[1], xy[2], pch = 16, col = "red")

par(mfrow = c(1, 1))

##### Step 3 – Spatial filter -----
message("Step 3: Removing isolated cells...")

# apply to all rasters
rasters_spatial_clean = remove_isolated_cells(rasters_filtered, years)

## Check
freq(rasters_filtered[[1]])
freq(rasters_spatial_clean[[1]])

# Extract matrices
mat_before = sapply(rasters_filtered[[1]], function(r) values(r))
mat_after = sapply(rasters_spatial_clean[[1]], function(r) values(r))

# Identify changed cells
changed_cells = which(rowSums(mat_before != mat_after, na.rm = TRUE) > 0)

# Pick one changed cell
cell_id = changed_cells[100]   # or any index you want

# Identify one raster where the value changed
i = which(mat_before[cell_id, ] != mat_after[cell_id, ])[1]

# Coordinates of the cell
xy = xyFromCell(rasters_filtered[[i]], cell_id)

# Zoom window (500 m)
zoom_ext = extent(
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
rasters_plantios = purrr::map2(rasters_spatial_clean, years, function(r, yr) {
  message("  - Adding plantios for year ", yr)
  add_plantios(r, yr, plantios, plantio_value = 1) # Plantios are not differentiated from other forests
})

# Quick check for plantios overlay
year_sel = 2004 # choose year to inspect
buff = 1000
pl = plantios[plantios$date_refor == year_sel, ]

if (nrow(pl) > 0) {
  years_to_plot = c(year_sel - 1, year_sel, year_sel + 1)
  idx = match(years_to_plot, years)
  valid = !is.na(idx)
  years_to_plot = years_to_plot[valid]
  idx = idx[valid]
  ext_zoom = ext(pl) + buff
  par(mfrow=c(1, length(idx)))
  for (j in seq_along(idx)) {
    yr = years_to_plot[j]
    r_zoom = crop(rasters_plantios[[idx[j]]], ext_zoom)
    plot(r_zoom, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"), main=as.character(yr))
    plot(pl, border="black", lwd=2, add=TRUE)
  }
  par(mfrow=c(1,1))
} else {
  message("No plantios with date_refor = ", year_sel)
}


##### Step 5 – Apply linear features on top of raster layers -----
message("Step 5: Applying linear features...")
# First, we distinguish roads based on their nature (primary, secondary, tertiary)
roads_sf %>% sf::st_drop_geometry() %>%  dplyr::select(highway) %>% dplyr::group_by(highway) %>% dplyr::summarise(n=n())
highway = roads_sf %>% dplyr::filter(highway == "motorway" | highway == "trunk" | highway == "primary") %>% terra::vect()
plot(highway)

# Second, we apply linear features
# We do not consider power lines because forests are not necessarily cleared under power lines, and because MapBiomas seem to detect those cleared areas quite well (they are around 60m wide)
# We do not consider secondary, tertiary or unclassified roads because they do not necessarily trim the forests (canopy can remain continuous above)
# Power lines take the value "other non-forest habitat (2)" (cleared herbaceous areas)
# Main roads take the value "artificial" (6)
# WARNING: at this resolution (30 x 30 m), a minimum of 20 m is needed for linear features to create continuous linear features (below, some cells are not overwritten, and some raster cells are still connected through their vertices)
rasters_lf = purrr::map2(rasters_plantios, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin = r
  r_lin = apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 20, value = 2, use_date = TRUE)
  r_lin = apply_linear_feature_single(r_lin, yr, highway, buffer_width = 20, value = 6, use_date = TRUE)
  r_lin
})

# Check
# Point coordinates
x_center = 776928.1
y_center = 7508359.8

# Buffer size
buffer_size = 1500

# Create bounding box extent
zoom_ext = ext(
  x_center - buffer_size, x_center + buffer_size,
  y_center - buffer_size, y_center + buffer_size
)

# Raster indices to visualize
subset_indices = c(1, 17, 35)

# Loop over selected rasters
for (i in subset_indices) {
  # Crop rasters
  r_before = crop(rasters_plantios[[i]], zoom_ext)
  r_after = crop(rasters_lf[[i]], zoom_ext)
  
  # Crop linear features
  roads_sub = crop(roads, zoom_ext)
  power_lines_sub = crop(power_lines, zoom_ext)
  bridges_sub = crop(bridges, zoom_ext)
  pipelines_sub = crop(pipelines, zoom_ext)
  
  # Plot side by side
  par(mfrow=c(1,2))
  
  plot(r_before,
       main = paste0("Before linear features (", years[i], ")"),
       col = c("#32a65e", "#519799", "#FFFFB2", "#d4271e"))
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  plot(r_after,
       main = paste0("After linear features (", years[i], ")"),
       col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#d4271e"))
  plot(roads_sub, add=TRUE, col="black")
  plot(power_lines_sub, add=TRUE, col="red")
  plot(bridges_sub, add=TRUE, col="blue")
  plot(pipelines_sub, add=TRUE, col="orange")
  
  par(mfrow=c(1,1))
}

##### Step 6 – Crop again (as linear features are above the extent) -----
rasters_final = lapply(rasters_lf, function(r) {
  r_cropped = crop(r, bbox) # crop returns a geographic subset of an object as specified by an Extent object
  r_masked = mask(r_cropped, bbox) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
  return(r_masked)
})

plot(rasters_lf[[36]], col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_final[[36]], col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))


##### Visual demo of all steps ---------
# Quick final summary
freq(rasters_final[[36]])

# Central coordinates
x_center = 779535
y_center = 7509233

# Zoom window
buffer_size = 3000
zoom_ext = extent(
  x_center - buffer_size, x_center + buffer_size,
  y_center - buffer_size, y_center + buffer_size
)

# Choose which raster/year to illustrate
index_demo = 34

# Step 0 - Original raster
r_step0 = crop(rasters[[index_demo]], zoom_ext)

# Step 1 - Reclassify
r_step1 = crop(rasters_reclass[[index_demo]], zoom_ext)

# Step 2 - Temporal filter
r_step2 = crop(rasters_filtered[[index_demo]], zoom_ext)

# Step 3 - Spatial filter
r_step3 = crop(rasters_spatial_clean[[index_demo]], zoom_ext)

# Step 4 - Add plantios
r_step4 = crop(rasters_plantios[[index_demo]], zoom_ext)

# Step 5 - Linear features
r_step5 = crop(rasters_lf[[index_demo]], zoom_ext)

roads_sub = crop(roads, zoom_ext)
power_lines_sub = crop(power_lines, zoom_ext)
bridges_sub = crop(bridges, zoom_ext)
pipelines_sub = crop(pipelines, zoom_ext)


# Plot all steps together

png(here("outputs","plot","01e_reclass_rasters_demo.png"), width = 3000, height = 2000, res = 300)

par(mfrow = c(2, 3), mar = c(2,2,2,2))

# Step 0
plot(r_step0, main = paste0("Step 0 – Original (", years[index_demo], ")"))

# Step 1
plot(r_step1, main = paste0("Step 1 – Reclassify (", years[index_demo], ")"),
     col=c("#32a65e", "#519799", "#FFFFB2", "#d4271e"))

# Step 2
plot(r_step2, main = "Step 2 – Temporal filter",
     col=c("#32a65e", "#519799", "#FFFFB2", "#d4271e"))

# Step 3
plot(r_step3, main = "Step 3 – Spatial filter",
     col=c("#32a65e", "#519799", "#FFFFB2", "#d4271e"))

# Step 4
plot(r_step4, main = "Step 4 – Add plantios",
     col=c("#32a65e", "#519799", "#FFFFB2", "#d4271e"))

# Step 5
plot(r_step5, main = "Step 5 – Linear features",
     col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#d4271e"))
plot(roads_sub, add=TRUE, col="black")
plot(power_lines_sub, add=TRUE, col="red")
plot(bridges_sub, add=TRUE, col="blue")
plot(pipelines_sub, add=TRUE, col="orange")

par(mfrow = c(1,1))
dev.off()


##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_reclass")

# Export each raster with year in the filename
for (i in seq_along(rasters_final)) {
  year_i = years[i]
  output_path = file.path(output_dir, paste0("raster_reclass_", year_i, ".tif"))
  
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

cu = rbind(uniao, pda, tres_picos)

rasters_forest_cat = lapply(rasters_final, function(r){
  
  # Start only with forest = 1
  
  # 1) RPPN (overrides everything)
  r = assign_if_intersect(r, rppn, target_values = 1, new_value = 12)
  
  # 2) Private inside conservation units (CAR ∩ CU)
  r = assign_if_intersect(r, intersect(car, cu), target_values = 1, new_value = 14)
  
  # 3) Public conservation units (remaining forest in CU)
  r = assign_if_intersect(r, cu, target_values = 1, new_value = 13)
  
  # 4) Private properties (remaining forest in CAR)
  r = assign_if_intersect(r, car, target_values = 1, new_value = 11)
  
  # 5) Remaining forest
  r[r == 1] = 10
  
  # 6) Everything else = NA
  r[!r %in% c(10,11,12,13,14)] = NA
  
  r
})

# Check
# pick the rasters
r_before = rasters_final[[36]]
r_after = rasters_forest_cat[[36]]
freq(rasters_forest_cat[[36]])

# zoom extent from reserve or car region
zoom_ext = ext(pda) + 5000

# crop rasters
rb = crop(r_before, zoom_ext)
ra = crop(r_after,  zoom_ext)

# to sf for plotting
car_sf = sf::st_as_sf(car)

# define a color table
par(mfrow=c(1,2))

plot(rb,
     main="BEFORE",
     col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))

plot(pda, add=TRUE, border="yellow", lwd=2)
plot(rppn, add=TRUE, border="gold3", lwd=2)
plot(car_sf, add=TRUE, col=scales::alpha("white",0.5), border=NA)

plot(ra,
     main="AFTER",
     col = c("#32a65e", "#006400", "#CDAD00", "#C1FFC1", "#CDCDC1"))

par(mfrow=c(1,1))


##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_reclass_forest_cat")

# Export each raster with year in the filename
for (i in seq_along(rasters_forest_cat)) {
  year_i = years[i]
  output_path = file.path(output_dir, paste0("raster_reclass_forest_cat_", year_i, ".tif"))
  
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
rasters_restor_cat = purrr::map2(rasters_final, years, function(r, yr){
  
  # 1) forest intersecting plantios with Ecologia == "Corredor" → 51
  r = assign_if_intersect_attr_year(r, yr, plantios,
                                     year_field = "date_refor",
                                     attr = "Ecologia",
                                     attr_value = "Corredor",
                                     target_values = 1,
                                     new_value = 51)
  
  # 2) remaining forest intersecting any plantios → 50
  r = assign_if_intersect_year(r, yr, plantios,
                                year_field = "date_refor",
                                target_values = 1,
                                new_value = 50)
  
  r
})

# Apply road overpasses
rasters_cons_cat = purrr::map2(rasters_restor_cat, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin = r
  r_lin = apply_linear_feature_single(r_lin, yr, bridges, buffer_width = 20, value = 52, use_date = TRUE)
  r_lin
})

# Reclass all the rest into NA
codes = c(50, 51, 52)
rasters_cons_cat = lapply(rasters_cons_cat, function(r){
  r[!r %in% codes] = NA
  r
})

# Drop rasters with only NA AND keep matching years
keep = sapply(rasters_cons_cat, function(r) {
  if (is.null(r)) return(FALSE)
  terra::global(!is.na(r), "sum", na.rm = TRUE)[1,1] > 0
})

rasters_cons_cat = rasters_cons_cat[keep]
years_cons_cat = years[keep]


# Check
freq(rasters_cons_cat[[21]])

# pick the rasters
r_before = rasters_final[[21]]
r_after = rasters_cons_cat[[21]]

# zoom extent from reserve or car region
zoom_ext = ext(pda) + 500

# crop rasters
rb = crop(r_before, zoom_ext)
ra = crop(r_after,  zoom_ext)
par(mfrow=c(1,2))

plot(rb,
     main="BEFORE",
     col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(plantios, add=TRUE, border="white", lwd=1)

plot(ra,
     main="AFTER",
     col = c("#FF7F00", "#EE30A7", "darkred"))
plot(plantios, add=TRUE, border="white", lwd=1)

par(mfrow=c(1,1))

##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_reclass_cons_cat")

# Export each raster with year in the filename
for (i in seq_along(rasters_cons_cat)) {
  year_i = years_cons_cat[i]
  output_path = file.path(output_dir,
                           paste0("raster_reclass_cons_cat_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_cons_cat[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}


### Rasters consistency -------
# Check that all rasters have the same extent
ext(rasters_final[[1]])
ext(rasters_forest_cat[[1]])
ext(rasters_cons_cat[[1]])
