#------------------------------------------------#
# Author: Romain Monassier
# Objective: LULC data preparation for simulation in PLUS software
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(ggplot2)
library(raster)
library(terra)
library(landscapemetrics)

### Import rasters -------

## Land use
base_path = here("outputs", "data", "MapBiomas", "Mask_sampling_bbox")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years = as.numeric(years)) %>%
  dplyr::arrange(years)
# Load rasters in chronological order
rasters = lapply(raster_df$file, terra::rast)
years = raster_df$years
# Check
for (i in seq_along(rasters)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}

base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)
plot(rasters[[1]])

## Slope
slope_r = terra::rast(here("data", "geo", "TOPODATA", "work", "slope_bbox.tif"))
plot(slope_r)

## Altitude
topo_r = terra::rast(here("data", "geo", "TOPODATA", "work", "topo_bbox.tif"))
plot(topo_r)

### WorldClim
## Precipitations (sum)
base_path = here("outputs", "data", "WorldClim", "prec")
raster_files = list.files(base_path, pattern = "^prec_(sum)_\\d{4}_bbox\\.tif$", full.names = TRUE)

# Extract years
years_climate = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")

# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_climate = as.numeric(years_climate)) %>%
  dplyr::arrange(years_climate)

# Load rasters in chronological order
rasters_prec_sum = lapply(raster_df$file, terra::rast)
years_climate = raster_df$years_climate
# Check
for (i in seq_along(rasters_prec_sum)) {
  cat("Year", years_climate[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_prec_sum[[36]])

## Tmin (mean)
base_path = here("outputs", "data", "WorldClim", "tmin")
raster_files = list.files(base_path, pattern = "^tmin_(mean)_\\d{4}_bbox\\.tif$", full.names = TRUE)

# Extract years
years_climate = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")

# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_climate = as.numeric(years_climate)) %>%
  dplyr::arrange(years_climate)

# Load rasters in chronological order
rasters_tmin_mean = lapply(raster_df$file, terra::rast)
years_climate = raster_df$years_climate
# Check
for (i in seq_along(rasters_tmin_mean)) {
  cat("Year", years_climate[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_tmin_mean[[36]])

## Tmin (max)
base_path = here("outputs", "data", "WorldClim", "tmax")
raster_files = list.files(base_path, pattern = "^tmax_(mean)_\\d{4}_bbox\\.tif$", full.names = TRUE)

# Extract years
years_climate = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")

# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_climate = as.numeric(years_climate)) %>%
  dplyr::arrange(years_climate)

# Load rasters in chronological order
rasters_tmax_mean = lapply(raster_df$file, terra::rast)
years_climate = raster_df$years_climate
# Check
for (i in seq_along(rasters_tmax_mean)) {
  cat("Year", years_climate[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_tmax_mean[[36]])


### Import vectors -----
## CAR (Properties)
car = terra::vect(here("data", "geo", "IBGE", "cadastro_car", "AREA_IMOVEL_RJ_2024", "AREA_IMOVEL_bbox.shp"))
car = terra::project(car, "EPSG:31983")
plot(car)
car_sf = sf::st_as_sf(car)

## CAR (Reserva Legal)
rl = terra::vect(here("data", "geo", "IBGE", "cadastro_car", "RESERVA_LEGAL", "RESERVA_LEGAL_bbox.shp"))
rl = terra::project(rl, "EPSG:31983")
plot(rl)
rl_sf = sf::st_as_sf(rl)

## RPPN
rppn = terra::vect(here("data", "geo", "AMLD", "RPPN", "RPPN_RJ.shp"))
rppn = terra::project(rppn, "EPSG:31983")
plot(rppn)
rppn_sf = sf::st_as_sf(rppn)

## Roads
roads = terra::vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))
roads = terra::project(roads, "EPSG:31983")
plot(roads)
roads_sf = sf::st_as_sf(roads)

# BR 101
br101 = roads_sf %>% 
  dplyr::filter(ref == "BR-101")

## Rivers
rivers = terra::vect(here("data", "geo", "OSM", "work", "Waterway_OSM_AMLD_clean.shp"))
rivers = terra::project(rivers, "EPSG:31983")
plot(rivers)
rivers_sf = sf::st_as_sf(rivers)

## Urban centers
urb = terra::vect(here("data", "geo", "IBGE", "admin", "RJ_Setores_CD2022", "setores_urb_bbox.shp"))
urb = terra::project(urb, "EPSG:31983")
plot(urb)
urb_sf = sf::st_as_sf(urb)
urb_centers = terra::centroids(urb)
plot(urb_centers)
plot(urb, add=TRUE)
urb_centers_sf = sf::st_as_sf(urb_centers)

## APA mld
apa_mld = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "apa_mld.shp"))
apa_mld = terra::project(apa_mld, "EPSG:31983")
plot(apa_mld)
apa_mld_sf = sf::st_as_sf(apa_mld)

## Public reserves
# Poco das Antas
pda = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "poco_das_antas.shp"))
pda = terra::project(pda, "EPSG:31983")
plot(pda)
# Tres Picos
tp = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "tres_picos.shp"))
tp = terra::project(tp, "EPSG:31983")
plot(tp)
# Uniao
uniao = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "uniao.shp"))
uniao = terra::project(uniao, "EPSG:31983")
plot(uniao)
# Merge
pub_res = rbind(pda, tp, uniao)
pub_res_sf = sf::st_as_sf(pub_res)
plot(pub_res)

## Other linear features
power_lines = vect(here("data", "geo", "OSM", "work", "Power_line_OSM_clean.shp"))
pipelines = vect(here("data", "geo", "OSM", "work", "Pipelines_OSM_clean.shp"))
bridges = vect(here("data", "geo", "AMLD", "Passagens_ARTERIS", "work", "road_overpasses_clean.shp"))

## Plantios
plantios = vect(here("data", "geo", "AMLD", "plantios", "work", "plantios_clean.shp"))
plantios_sf = sf::st_as_sf(plantios)

## Municipalities
municip = vect(here("data", "geo", "IBGE", "admin", "BR_Municipios_2023", "BR_Municipios_2023.shp"))
municip = project(municip, "EPSG:31983")

### Create land use raster for simulations ----
# This code was copied from 01e_reclass_rasters

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

#### Reclass built-up to water ---------
# Some water pixels are misclassified as built-up
# We use municipios to control for this misclassification: all water pixels misclassified as built-up areas are reclassified as water when they do not intersect brazilian towns (which stop at the sea)
reclass_builtup_not_in_municip = function(rasters, years, municip) {
  
  message("Reclassifying overseas built-up (6) to water (5)...")
  n = length(rasters)
  if (n != length(years)) stop("rasters and years must have same length")
  
  # Ensure CRS matches
  if (!terra::same.crs(rasters[[1]], municip)) {
    municip = terra::project(municip, rasters[[1]])
  }
  
  # Rasterize municipios once
  municip_raster = terra::rasterize(
    municip, 
    rasters[[1]], 
    field = 1, 
    background = NA
  )
  
  out_list = vector("list", n)
  
  for (i in seq_len(n)) {
    
    message(" Processing year ", years[i], " (", i, "/", n, ")")
    r = rasters[[i]]
    
    # Reclass only built-up pixels outside municipios
    r_reclass = terra::ifel(
      r == 6 & is.na(municip_raster),
      5,
      r
    )
    
    names(r_reclass) = as.character(years[i])
    out_list[[i]] = r_reclass
  }
  
  return(out_list)
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
assign_if_intersect_attr_year = function(raster, year, vect,
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
rasters_1 <- lapply(seq_along(rasters), function(i) {
  message("  - Reclassifying raster ", i, " (year ", years[i], ")")
  app(rasters[[i]], reclass_fun)
})

# Check
plot(rasters_1[[1]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_1[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))

##### Step 2 – Temporal filter ------
message("Step 2: Applying temporal consistency filter...")

# Apply on rasters
rasters_2 = apply_temporal_threshold(rasters = rasters_1, years = years)

# Quick check
freq(rasters_1[[35]])
freq(rasters_2[[35]])

##### Step 3 – Spatial filter -----
message("Step 3: Removing isolated cells...")

# apply to all rasters
rasters_3 = remove_isolated_cells(rasters_2, years)

## Check
freq(rasters_2[[1]])
freq(rasters_3[[1]])

##### Step 4 – Reclass open water ------
message("Step 4: Reclass open water...")
rasters_4 = reclass_builtup_not_in_municip(rasters_3, years, municip)

# Check
freq(rasters_3[[36]])
freq(rasters_4[[36]])
plot(rasters_3[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(rasters_4[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))


##### Step 5 – Add plantios ------
message("Step 5: Adding plantios to rasters...")
rasters_5 = purrr::map2(rasters_4, years, function(r, yr) {
  message("  - Adding plantios for year ", yr)
  add_plantios(r, yr, plantios, plantio_value = 1) # Plantios are not differentiated from other forests
})

# ##### Step 6 – Apply linear features on top of raster layers -----
# message("Step 6: Applying linear features...")
# # First, we distinguish roads based on their nature (primary, secondary, tertiary)
# roads_sf %>% sf::st_drop_geometry() %>%  dplyr::select(highway) %>% dplyr::group_by(highway) %>% dplyr::summarise(n=n())
# highway = roads_sf %>% dplyr::filter(highway == "motorway" | highway == "trunk" | highway == "primary") %>% terra::vect()
# plot(highway)
# 
# # Second, we apply linear features
# # We do not consider power lines because forests are not necessarily cleared under power lines, and because MapBiomas seem to detect those cleared areas quite well (they are around 60m wide)
# # We do not consider secondary, tertiary or unclassified roads because they do not necessarily trim the forests (canopy can remain continuous above)
# # We do not consider oil and gas pipelines (for LULCC classification) because of errors when simulating future LULCC (cannot be simulated as expanding herbaceous habitats, for instance)
# # Main roads take the value "artificial" (6)
# # WARNING: at this resolution (30 x 30 m), a minimum of 20 m is needed for linear features to create continuous linear features (below, some cells are not overwritten, and some raster cells are still connected through their vertices)
# rasters_6 = purrr::map2(rasters_5, years, function(r, yr) {
#   message("  - Applying linear features for year ", yr)
#   r_lin = r
#   # r_lin = apply_linear_feature_single(r_lin, yr, pipelines, buffer_width = 20, value = 2, use_date = TRUE)
#   r_lin = apply_linear_feature_single(r_lin, yr, highway, buffer_width = 10, value = 6, use_date = TRUE)
#   r_lin
# })
# 
# ##### Step 7 – Crop again (as linear features are above the extent) -----
# rasters_7 = lapply(rasters_6, function(r) {
#   r_cropped = crop(r, bbox) # crop returns a geographic subset of an object as specified by an Extent object
#   r_masked = mask(r_cropped, bbox) # create a new Raster object that has the same values as x, except for the cells that are NA in a 'mask' (either a Raster or a Spatial object)
#   return(r_masked)
# })
# 
# plot(rasters_6[[36]], col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
# plot(rasters_7[[36]], col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))


### Create rasters of explanatory variables ------

#### Create rasters -------
# Select template raster to create other rasters
template_rast = rasters_5[[1]]
plot(template_rast, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
names(rasters_5) = years

##### Legal status -------
## Rasterize data
# Private properties
car_r = terra::rasterize(car_sf, template_rast, field = 1, background = 0)
plot(car_r)
# Public reserve
pub_res_r = terra::rasterize(pub_res_sf, template_rast, field = 1, background = 0)
plot(pub_res_r)
# RPPN
rppn_r = terra::rasterize(rppn_sf, template_rast, field = 1, background = 0)
plot(rppn_r)
# Reserva legal
rl_r = terra::rasterize(rl_sf, template_rast, field = 1, background = 0)
plot(rl_r)
# APA
apa_r = terra::rasterize(apa_mld_sf, template_rast, field = 1, background = 0)
plot(apa_r)

##### Distances -------
# Rivers (static)
rivers_bin = terra::rasterize(rivers_sf, template_rast, field = 1, background = NA)
plot(rivers_bin, col="purple")
dist_rivers_r = terra::distance(rivers_bin)
plot(dist_rivers_r)

## Urban centers (static)
urban_bin = terra::rasterize(urb_sf, template_rast, field = 1, background = NA)
plot(urban_bin, col="purple")
dist_urban_r = terra::distance(urban_bin)
plot(dist_urban_r)

## Roads (dynamic)
dist_roads_list = list()
# Loop over years
for (yr in years) {
  # Keep roads created that year or before
  roads_sub = roads_sf[roads_sf$date_crea <= yr, ]
  
  # Check if roads exist
  if (nrow(roads_sub) == 0) {
    raster_list[[as.character(yr)]] = NA
    next
  }
  
  # Rasterize roads
  roads_bin = terra::rasterize(roads_sub, template_rast, field = 1, background = NA)
  
  # Compute distance
  dist_r = terra::distance(roads_bin)
  
  # Store the raster into a list
  dist_roads_list[[as.character(yr)]] = dist_r
}
plot(dist_roads_list[[1]])
plot(dist_roads_list[[36]])

## Forest edges (dynamic)
dist_edges_list =list()  # Liste pour stocker les rasters de distance aux bords

# Boucle sur les années
for (yr in years) {
  cat("  Processing forest edges for year:", yr, "\n")
  
  # LULC raster for a given year
  r_year = rasters_5[[as.character(yr)]]
  
  # Create a binary mask (forest = 1, the rest = NA)
  forest_mask = r_year
  forest_mask[forest_mask != 1] = NA
  
  # Compute forest edges
  edge_list = landscapemetrics::get_boundaries(forest_mask, as_NA = TRUE)
  edge_rast = edge_list[[1]]
  
  # Compute distance
  dist_edge_r = terra::distance(edge_rast)
  
  # Store the raster
  dist_edges_list[[as.character(yr)]] = dist_edge_r
}
plot(dist_edges_list[[1]])
plot(dist_edges_list[[36]])


##### LULC in a buffer -------

# Function to return a raster of proportion according to a given radius
compute_mw_prop = function(r, class_id, radius_m){
  
  r_class = ifel(r == class_id, 1, 0)
  
  cellsize = res(r)[1]
  radius_cells = ceiling(radius_m / cellsize)
  
  size = 2 * radius_cells + 1
  w = matrix(1, size, size)
  
  mw_class = focal(r_class, w = w, fun = sum, na.rm = TRUE)
  
  r_valid = !is.na(r)
  mw_valid = focal(r_valid, w = w, fun = sum, na.rm = TRUE)
  
  prop = mw_class / mw_valid
  
  return(prop)
}

## Forest
# Store rasters in a list
prop_forest = list()

# Parameters
class_id = 1  # Class value
radius_m = 100  # Radius in meters

# Loop
for (i in seq_along(rasters_5)) {
  yr = years[i]
  r = rasters_5[[i]]
  
  cat("Calculating proportion for year:", yr, "\n")
  # Compute proportion in a given radius
  prop_r = compute_mw_prop(r, class_id, radius_m)
  
  # Output = raster
  prop_forest[[as.character(yr)]] = prop_r
}
plot(prop_forest[[1]])
plot(prop_forest[[36]])

## Agriculture
# Store rasters in a list
prop_agri = list()

# Parameters
class_id = 4  # Class value
radius_m = 100  # Radius in meters

# Loop
for (i in seq_along(rasters_5)) {
  yr = years[i]
  r = rasters_5[[i]]
  
  cat("Calculating proportion for year:", yr, "\n")
  # Compute proportion in a given radius
  prop_r = compute_mw_prop(r, class_id, radius_m)
  
  # Output = raster
  prop_agri[[as.character(yr)]] = prop_r
}
plot(prop_agri[[36]])

## Built-up area
# Store rasters in a list
prop_urban = list()

# Parameters
class_id = 6  # Class value
radius_m = 100  # Radius in meters

# Loop
for (i in seq_along(rasters_5)) {
  yr = years[i]
  r = rasters_5[[i]]
  
  cat("Calculating proportion for year:", yr, "\n")
  # Compute proportion in a given radius
  prop_r = compute_mw_prop(r, class_id, radius_m)
  
  # Output = raster
  prop_urban[[as.character(yr)]] = prop_r
}
plot(prop_urban[[36]])

##### North and South of BR101 -------
br_coords = sf::st_coordinates(br101)  # x and y coordinates of BR-1010

# Create an empty raster
ns_raster = terra::rast(template_rast)
ns_raster[] = NA 

# Determine whether a point is North or South of located coords
compute_ns = function(x, y, br_coords) {
  # Find nearest coord (in longitude, x)
  idx = which.min(abs(br_coords[, 1] - x))
  br_y = br_coords[idx, 2] # Y coord of the nearest BR-101 point
  
  # Compare Y with the BR-101 Y coordinate
  if (y > br_y) {
    return(0)  # North
  } else {
    return(1)  # South
  }
}

# Apply to all the raster
# We transform the raster to a data frame
raster_df = as.data.frame(ns_raster, xy = TRUE, na.rm = FALSE)

# Apply the function to each X, Y point
raster_df$ns = mapply(
  compute_ns,
  x = raster_df$x,
  y = raster_df$y,
  MoreArgs = list(br_coords = br_coords)
)

# Rebuild raster with 0 (North) and 1 (South)
ns_raster[] = raster_df$ns

# Plot
plot(ns_raster, main = "North (0) / South (1) of BR-101", col = c("blue", "red"))

##### Water (constrained) -------
# Here, we create a constraint on land development
# See PLUS documentation: "Users should prepare a binary image of restricted area that only contains value of 0 or 1. The value 0 means no conversion while the value 1 means convertible"
water_list = list()
# Loop over years
for (yr in years) {
  # LULC raster for a given year
  r_year = rasters_5[[as.character(yr)]]
  
  # Create a binary mask (water = 5, the rest = 1)
  water_mask = r_year
  water_mask[water_mask != 5] = 1
  water_mask[water_mask == 5] = 0 # Transform 5 to 0
  
  # Store the raster into a list
  water_list[[as.character(yr)]] = water_mask
}
plot(water_list$`2024`, col=c("blue","grey"))


### Export rasters for PLUS ------

#### Reproject -------

# Template raster
template_rast = rasters_5[[1]]
crs(template_rast)
template_wgs84 = terra::project(template_rast, "EPSG:4326", method = "near")
crs(template_wgs84)

## Reproject
# LULC
rasters_wgs84 = lapply(rasters_5, terra::project, template_wgs84, method = "near")

# Slope
slope_r = terra::project(slope_r, template_wgs84, method="bilinear")

# Elevation
topo_r = terra::project(topo_r, template_wgs84, method="bilinear")

# Distances
dist_rivers_r = terra::project(dist_rivers_r, template_wgs84, method="bilinear")
dist_urban_r = terra::project(dist_urban_r, template_wgs84, method="bilinear")
dist_edges_list = lapply(dist_edges_list, terra::project, template_wgs84, method = "near")
dist_roads_list = lapply(dist_roads_list, terra::project, template_wgs84, method = "near")

# Proportions
prop_agri = lapply(prop_agri, terra::project, template_wgs84, method = "near")
prop_urban = lapply(prop_urban, terra::project, template_wgs84, method = "near")
prop_forest = lapply(prop_forest, terra::project, template_wgs84, method = "near")

# Binary rasters
apa_r = terra::project(apa_r, template_wgs84, method="near")
car_r = terra::project(car_r, template_wgs84, method="near")
rl_r = terra::project(rl_r, template_wgs84, method="near")
rppn_r = terra::project(rppn_r, template_wgs84, method="near")
pub_res_r = terra::project(pub_res_r, template_wgs84, method="near")
ns_raster = terra::project(ns_raster, template_wgs84, method="near")
water_list = lapply(water_list, terra::project, template_wgs84, method = "near")

#### Export --------
# Define output folder
output_dir = here("outputs", "data", "PLUS", "lulc")

##### LULC -------
for (i in seq_along(rasters_wgs84)) {
  year_i = years[i]
  output_path = file.path(output_dir,
                          paste0("raster_plus_wgs84_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_wgs84[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}

##### Distances -------
output_dir = here("outputs", "data", "PLUS", "drivers")

# Distances to edges
terra::writeRaster(dist_edges_list[['2024']], 
                   filename = file.path(output_dir, "dist_edges2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

# Distances to roads
terra::writeRaster(dist_roads_list[['2024']], 
                   filename = file.path(output_dir, "dist_roads2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

# Distance to urban centers
terra::writeRaster(dist_urban_r, 
                   filename = file.path(output_dir, "dist_urban.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

# Distance to rivers
terra::writeRaster(dist_rivers_r, 
                   filename = file.path(output_dir, "dist_rivers.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

##### Legal status -------
terra::writeRaster(apa_r, 
                   filename = file.path(output_dir, "apa.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")
terra::writeRaster(rl_r, 
                   filename = file.path(output_dir, "rl.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")
terra::writeRaster(rppn_r, 
                   filename = file.path(output_dir, "rppn.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")
terra::writeRaster(pub_res_r, 
                   filename = file.path(output_dir, "pub_res.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")
terra::writeRaster(car_r, 
                   filename = file.path(output_dir, "car.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")

##### North/South BR-101 -------
terra::writeRaster(ns_raster, 
                   filename = file.path(output_dir, "ns_br101.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")

##### Climate ------
# Precipitations
terra::writeRaster(rasters_prec_sum[[36]], 
                   filename = file.path(output_dir, "prec2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")
# Tmin
terra::writeRaster(rasters_tmin_mean[[36]], 
                   filename = file.path(output_dir, "tmin2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")
# Tmax
terra::writeRaster(rasters_tmin_mean[[36]], 
                   filename = file.path(output_dir, "tmax2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

##### Topography ------
# Slope
terra::writeRaster(slope_r, 
                   filename = file.path(output_dir, "slope.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")


# Elevation
terra::writeRaster(topo_r, 
                   filename = file.path(output_dir, "elev.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

##### Proportions of land uses ------
# Agriculture
terra::writeRaster(prop_agri[[36]], 
                   filename = file.path(output_dir, "prop_agri100m_2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")
# Urban
terra::writeRaster(prop_urban[[36]], 
                   filename = file.path(output_dir, "prop_urban100m_2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")
# Forest
terra::writeRaster(prop_forest[[36]], 
                   filename = file.path(output_dir, "prop_forest100m_2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "FLT4S")

##### Other constraints ------
output_dir = here("outputs", "data", "PLUS", "constraint")
# Open water
terra::writeRaster(water_list[['2024']], 
                   filename = file.path(output_dir, "water2024.tif"), 
                   overwrite = TRUE, 
                   datatype = "INT1U")



#### PLUS parameters --------
## Land use demand
freq(rasters_wgs84[[1]]) # Frequency of each land use
freq(rasters_wgs84[[36]]) # Land use demand

## Transition matrix
# Compute transition matrix
tm = terra::crosstab(c(rasters_wgs84[[1]], rasters_wgs84[[36]]))
tm_prop = round(tm / rowSums(tm),2) # Proportions by row
tm_prop
# 0 if <0.05 (i.e., less than 5% of X pixels transitioned from X to Y), 1 otherwise
tm_binary = ifelse(tm_prop < 0.05, 0, 1)
print(tm_binary)

## Neighborhood weights
# Based on PLUS documentation, we can determine the neighborhood weight of each land-use type by calculating the ratio of the expansion areas of a land-use type accounting for the total land expansion
# Import land use changes (1989-2024)
lulc_19892024 = terra::rast(here("outputs", "data", "PLUS", "lulc_expansion", "lu1989_2024_landuse_1to2.tif"))
plot(lulc_19892024)
# Frequency table
freq_lulc19892024 = freq(lulc_19892024)
# Filter lines where value != 0
freq_filtered = freq_lulc19892024[freq_lulc19892024$value != 0, ]
# Sum of LULC changes
tot = sum(freq_filtered$count)
# Proportion of each change
freq_filtered$proportion = freq_filtered$count / tot
# Print result
print(freq_filtered[, c("value", "count", "proportion")])

