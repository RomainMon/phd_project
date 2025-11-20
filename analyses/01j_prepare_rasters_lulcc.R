#------------------------------------------------#
# Author: Romain Monassier
# Objective: Prepare rasters for lulcc analysis
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(terra)
library(sf)
library(data.table)
library(stringr)
library(landscapemetrics)

### Import rasters -------

## Land use
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_lulc = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_lulc = as.numeric(years_lulc)) %>%
  dplyr::arrange(years_lulc)
# Load rasters in chronological order
rasters_reclass = lapply(raster_df$file, terra::rast)
years_lulc = raster_df$years_lulc
# Check
for (i in seq_along(rasters_reclass)) {
  cat("Year", years_lulc[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_reclass[[35]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))

## Cumulative rasters
base_path = here("outputs", "data", "MapBiomas", "Rasters_cumulative_tm")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_tm = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_tm = as.numeric(years_tm)) %>%
  dplyr::arrange(years_tm)
# Load rasters in chronological order
rasters_tm = lapply(raster_df$file, terra::rast)
years_tm = raster_df$years_tm
# Check
for (i in seq_along(rasters_tm)) {
  cat("Year", years_tm[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_tm[[35]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "lightgreen", "pink"))


## Years of change
base_path = here("outputs", "data", "MapBiomas", "Rasters_years_forest_change")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)
lulcc_years = lapply(raster_files, terra::rast)
plot(lulcc_years[[1]])

## Slope
slope_r = terra::rast(here("data", "geo", "TOPODATA", "work", "slope_bbox.tif"))
plot(slope_r)

### Import vectors -----
## CAR (Properties)
car = terra::vect(here("data", "geo", "IBGE", "cadastro_car", "AREA_IMOVEL_RJ_2024", "AREA_IMOVEL_bbox.shp"))
car = terra::project(car, "EPSG:31983")
plot(car)
car_sf = sf::st_as_sf(car)

# Mutate area and id
car_sf = car_sf %>% dplyr::mutate(car_id = row_number(),
                           car_area_m2 = round(as.numeric(st_area(geometry)),2),
                           car_area_ha = round((car_area_m2 / 10000), 2))

## CAR (Reserva Legal)
rl = terra::vect(here("data", "geo", "IBGE", "cadastro_car", "RESERVA_LEGAL", "RESERVA_LEGAL_bbox.shp"))
rl = terra::project(rl, "EPSG:31983")
plot(rl)
rl_sf = sf::st_as_sf(rl)

## Roads
roads = terra::vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))
roads = terra::project(roads, "EPSG:31983")
plot(roads)
roads_sf = sf::st_as_sf(roads)

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

## Poco das Antas
pda = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "poco_das_antas.shp"))
pda = terra::project(pda, "EPSG:31983")
plot(pda)
pda_sf = sf::st_as_sf(pda)

## Uniao
uniao = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "uniao.shp"))
uniao = terra::project(uniao, "EPSG:31983")
plot(uniao)
uniao_sf = sf::st_as_sf(uniao)

## APA mld
apa_mld = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "apa_mld.shp"))
apa_mld = terra::project(apa_mld, "EPSG:31983")
plot(apa_mld)
apa_mld_sf = sf::st_as_sf(apa_mld)

## Merge Poco das Antas and Uniao
public_reserves_sf = dplyr::bind_rows(uniao_sf, pda_sf)
plot(st_geometry(public_reserves_sf))

### Prepare the cell-based dataset -----
# Rationale:
# The workflow starts from rasters_tm[[35]], a raster identifying all cells that experienced at least one land-use/land-cover (LULC) change event during the study period. These “changed cells” form the core sample on which all covariates will be calculated.
# For each event row, the pipeline computes a set of covariates that describe biophysical, legal, and landscape-context characteristics of the cell at the time of its change. All covariates are extracted or computed dynamically with respect to the event year.
# Dataset A: reforested (6) cells + a random sample of intact cells (value = 1 in rasters_tm). Sample must match the number of 6-cells per year.
# Dataset B: deforested (7) cells + a random sample of intact cells (value = 1 in rasters_tm). Sample must match the number of 7-cells per year.

#### Parameters ----------
change_codes = c(6,7) # reforest=6, deforest=7 in rasters_tm
template_rast = rasters_reclass[[35]]  # general template/resolution/extent
roads_year_col <- "date_crea" # column name in roads_sf with creation year (may be date/character/numeric)


### Helpers -----------
# Reproject if needed
ensure_crs <- function(sf_obj, target_crs) {
  if (is.null(sf_obj)) return(NULL)
  if (st_crs(sf_obj) != target_crs) return(st_transform(sf_obj, target_crs))
  sf_obj
}

# Rasterize vector -> binary raster (0/NA or 1/0 depending on background)
rasterize_binary <- function(sf_obj, template_rast, value = 1, background = NA) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    r <- rast(template_rast)
    values(r) <- background
    return(r)
  }
  terra::rasterize(terra::vect(sf_obj), template_rast,
                   field = value, background = background)
}

# Simple extractor: always return a numeric vector of values
extract_values <- function(r, coords_df) {
  if (is.null(r)) return(rep(NA_real_, nrow(coords_df)))
  out <- terra::extract(r, coords_df)
  if (is.data.frame(out) && ncol(out) >= 2) {
    return(out[[2]])
  }
  if (is.data.frame(out) && ncol(out) == 1) {
    return(out[[1]])
  }
  as.numeric(out)
}



#### SECTION 0 — Identify changed cells (cells with >=1 change) --------
cat("\nSECTION 0: Identify changed cells\n")

cumulative_mask <- rasters_tm[[35]]

vals <- terra::values(cumulative_mask)
changed_cell_ids <- which(vals %in% change_codes)

stopifnot(length(changed_cell_ids) > 0)

cat(" • Changed cells:", length(changed_cell_ids), "\n")


#### SECTION 1 — Build master table of all change events --------
# For each changed cell, read the up to 10 lulcc_years rasters and convert to long table
cat("\nSECTION 1: Build event table\n")

vals_list <- lapply(lulcc_years, function(r) terra::values(r)[changed_cell_ids])
vals_mat  <- do.call(cbind, vals_list)
colnames(vals_mat) <- paste0("rank_", seq_len(ncol(vals_mat)))
head(vals_mat)

dt0 <- as.data.table(vals_mat)
dt0[, cell_id := changed_cell_ids]

dt_long <- melt(dt0, id.vars = "cell_id",
                variable.name = "change_rank",
                value.name = "change_year")

dt_long <- dt_long[!is.na(change_year)]
dt_long[, change_rank := as.integer(stringr::str_extract(change_rank, "\\d+"))]
head(dt_long)

# add coordinates
xy <- terra::xyFromCell(template_rast, dt_long$cell_id)
dt_long[, `:=`(x = xy[,1], y = xy[,2])]
head(dt_long)

# add change code & type
mask_vals <- vals[dt_long$cell_id]
dt_long[, change_code := mask_vals]
dt_long[, change_type :=
          ifelse(change_code == 6, "reforest",
                 ifelse(change_code == 7, "deforest", NA_character_))]
head(dt_long)

cat(" • Events:", nrow(dt_long), "\n")



#### SECTION 2 — Select controls (i.e., intact forests) by year ---------

cat("\nSECTION 2: Selecting intact control cells\n")

# Step 1: Identify all intact cells (value=1) per year
intact_list <- lapply(seq_along(rasters_tm), function(i){
  r <- rasters_tm[[i]]
  vals <- terra::values(r)
  which(vals == 1)  # intact = 1
})
names(intact_list) <- years_tm
head(intact_list$`1990`)

# Step 2: Count changed cells of each type per year
events_per_year <- dt_long[, .N, by = .(change_year, change_code)]
colnames(events_per_year)[3] <- "n_events"
head(events_per_year)

# Keep only codes 6 (reforest) and 7 (deforest)
events_per_year = events_per_year[change_code %in% c(6,7)]
head(events_per_year)

# Step 3: Year-stratified sampling function
sample_intact <- function(year, n_needed){
  intact_ids <- intact_list[[as.character(year)]]
  if(length(intact_ids) < n_needed){
    warning("Not enough intact cells in year ", year,
            " — taking all available.")
    return(intact_ids)
  }
  sample(intact_ids, n_needed)
}

# Step 4: Select intact controls for reforest (=6)
reforest_events <- events_per_year[change_code == 6]
reforest_controls <- reforest_events[, .(
  sampled_cells = list(
    sample_intact(change_year, n_events)
  )
), by = change_year]
head(reforest_controls)

# Flatten
reforest_controls_dt <- data.table(
  cell_id = unlist(reforest_controls$sampled_cells),
  change_year = rep(reforest_controls$change_year,
                    times = lengths(reforest_controls$sampled_cells))
)
reforest_controls_dt[, change_type := "control"]
reforest_controls_dt[, change_code := 1]  # intact
head(reforest_controls_dt)

# Add coordinates
xy_ref <- terra::xyFromCell(template_rast, reforest_controls_dt$cell_id)
reforest_controls_dt[, `:=`(
  x = xy_ref[,1],
  y = xy_ref[,2]
)]
head(reforest_controls_dt)

# Step 5: Select intact controls for deforest (=7)
deforest_events <- events_per_year[change_code == 7]

deforest_controls <- deforest_events[, .(
  sampled_cells = list(
    sample_intact(change_year, n_events)
  )
), by = change_year]

deforest_controls_dt <- data.table(
  cell_id = unlist(deforest_controls$sampled_cells),
  change_year = rep(deforest_controls$change_year,
                    times = lengths(deforest_controls$sampled_cells))
)
deforest_controls_dt[, change_type := "control"]
deforest_controls_dt[, change_code := 1]

# Add coordinates
xy_def <- terra::xyFromCell(template_rast, deforest_controls_dt$cell_id)
deforest_controls_dt[, `:=`(
  x = xy_def[,1],
  y = xy_def[,2]
)]
head(deforest_controls_dt)

#### SECTION 3 — Build final datasets for modelling -------

# Dataset A: Reforest + controls
data_refor <- rbind(
  dt_long[change_code == 6][, .(cell_id, change_year, change_code, change_type, x, y)],
  reforest_controls_dt
)
head(data_refor)

# Dataset B: Deforest + controls
data_defor <- rbind(
  dt_long[change_code == 7][, .(cell_id, change_year, change_code, change_type, x, y)],
  deforest_controls_dt
)
head(data_defor)


# Quick check
data_refor %>% dplyr::group_by(change_code) %>% dplyr::summarise(n=n())
data_defor %>% dplyr::group_by(change_code) %>% dplyr::summarise(n=n())

cat(" • Dataset A rows:", nrow(data_refor), "\n")
cat(" • Dataset B rows:", nrow(data_defor), "\n")



#### SECTION 4 — LEGAL STATUS (public_reserve, private, urban, reserva_legal) --------
# This block overlays the event cell with several binary spatial layers: public_reserve (União + Poço das Antas), private (CAR registry), urban (urban polygons), reserva_legal
# Each layer is rasterized onto the reference grid and intersected with event coordinates. Legal status is a hierarchical categorical variable computed by combining the four binary indicators.

cat("\nSECTION 4: Legal status\n")

# Rasterize data
car_r <- rasterize_binary(car_sf, template_rast, value = 1, background = 0)
pub_r <- rasterize_binary(public_reserves_sf, template_rast, value = 1, background = 0)
urb_r <- rasterize_binary(urb_sf, template_rast, value = 1, background = 0)
rl_r  <- rasterize_binary(rl_sf,  template_rast, value = 1, background = 0)

## Dataset A
coords_defor <- data_defor[, .(x,y)]

data_defor[, in_car    := extract_values(car_r, coords_defor)]
data_defor[, in_public := extract_values(pub_r, coords_defor)]
data_defor[, in_urban  := extract_values(urb_r, coords_defor)]
data_defor[, in_rl     := extract_values(rl_r,  coords_defor)]

data_defor[, `:=`(
  in_car    = as.integer(in_car == 1),
  in_public = as.integer(in_public == 1),
  in_urban  = as.integer(in_urban == 1),
  in_rl     = as.logical(in_rl == 1)
)]

# Categorize
data_defor[, legal_status :=
          fifelse(in_urban == 1, "urban",
                  fifelse(in_public == 1, "public_reserve",
                          fifelse(in_car == 1, "private", "none")))]
table(data_defor$change_code, data_defor$legal_status)

## Dataset B
coords_refor <- data_refor[, .(x,y)]

data_refor[, in_car    := extract_values(car_r, coords_refor)]
data_refor[, in_public := extract_values(pub_r, coords_refor)]
data_refor[, in_urban  := extract_values(urb_r, coords_refor)]
data_refor[, in_rl     := extract_values(rl_r,  coords_refor)]

data_refor[, `:=`(
  in_car    = as.integer(in_car == 1),
  in_public = as.integer(in_public == 1),
  in_urban  = as.integer(in_urban == 1),
  in_rl     = as.logical(in_rl == 1)
)]

# Categorize
data_refor[, legal_status :=
             fifelse(in_urban == 1, "urban",
                     fifelse(in_public == 1, "public_reserve",
                             fifelse(in_car == 1, "private", "none")))]
table(data_refor$change_code, data_refor$legal_status)

cat(" • Legal status computed\n")


#### SECTION 5 — INTERSECT WITH APA (binary) --------
# A binary indicator (in_APA) is added for each event.

cat("\nSECTION 5: APA intersection\n")

# Rasterize
apa_r <- rasterize_binary(apa_mld_sf, template_rast, value = 1, background = 0)

## Dataset A
data_defor[, in_APA := as.integer(extract_values(apa_r, coords_defor) == 1)]
table(data_defor$change_code, data_defor$in_APA)

## Dataset B
data_refor[, in_APA := as.integer(extract_values(apa_r, coords_refor) == 1)]
table(data_refor$change_code, data_refor$in_APA)

cat(" • APA intersection done\n")


#### SECTION 6 — DISTANCES --------
# Distances are computed from the event cell to different spatial features, all projected to match the template raster.
# For roads: For each event year, only roads whose construction year is ≤ the event year are retained.
# For forest edges: For each event year, a forest/non-forest raster is taken from rasters_reclass. Using landscapemetrics, forest boundaries are computed with get_boundaries().

cat("\nSECTION 6: Distances\n")

# Rasterize
rivers_bin <- rasterize_binary(rivers_sf, template_rast, 1, NA)
urban_bin <- rasterize_binary(urb_centers_sf, template_rast, 1, NA)

# Distances
dist_rivers_r <- terra::distance(rivers_bin)
plot(dist_rivers_r)
dist_urban_r <- terra::distance(urban_bin)
plot(dist_urban_r)


## Dataset A

# Distance to rivers (static)
data_defor[, dist_water_m := extract_values(dist_rivers_r, coords_defor)]
# Distance to urban centers (static)
data_defor[, dist_urban_m := extract_values(dist_urban_r, coords_defor)]

# Distance to roads (dynamic)

# Initialize column
data_defor[, dist_road_m := NA_real_]

unique_years <- sort(unique(data_defor$change_year))

for (yr in unique_years) {
  idx <- which(data_defor$change_year == yr)
  if (length(idx) == 0) next
  
  roads_sub <- roads_sf[roads_sf[[roads_year_col]] <= yr, ]
  
  if (nrow(roads_sub) == 0) {
    data_defor[idx, dist_road_m := NA_real_]
    next
  }
  
  roads_bin <- rasterize_binary(roads_sub, template_rast, 1, NA)
  dist_r <- terra::distance(roads_bin)
  
  data_defor[idx, dist_road_m := extract_values(dist_r, data_defor[idx, .(x,y)])]
}

# Distance to forest edges (dynamic)
# pipeline = forest raster → edge raster → terra::distance() → extract

# Initialize column
data_defor[, dist_edge_m := NA_real_]

# Unique years to loop over
unique_years <- sort(unique(data_defor$change_year))

for (yr in unique_years) {
  
  cat("  Processing forest edges for year:", yr, "\n")
  
  # Identify rows for this year
  idx <- which(data_defor$change_year == yr)
  if (length(idx) == 0) next
  
  # Get the forest raster for the corresponding year
  # ASSUMPTION: rasters_reclass is ordered chronologically: 1989–2024
  r_year <- rasters_reclass[[which(years_lulc == yr)]]
  
  # Build binary forest mask: 1 = forest, NA = other
  forest_mask <- r_year == 1
  forest_mask[forest_mask != 1] <- NA
  
  # landscapemetrics: compute forest boundaries
  # Returns a raster where edge pixels are 1, others NA
  edge_list <- landscapemetrics::get_boundaries(forest_mask,
                                                as_NA = TRUE)
  
  # Result is a list: extract first layer
  edge_rast <- edge_list[[1]]
  
  # Skip if edges missing
  if (all(is.na(values(edge_rast)))) {
    data_defor[idx, dist_edge_m := NA_real_]
    next
  }
  
  # Compute distance raster
  # Distance to nearest edge pixel
  dist_edge_r <- terra::distance(edge_rast)
  
  # Extract distances only for rows of this year
  coords_year <- coords_defor[idx]
  data_defor[idx, dist_edge_m := extract_values(dist_edge_r, coords_year)]
}

data_defor %>% dplyr::group_by(change_code) %>% dplyr::summarise_at(vars(starts_with("dist")), mean, na.rm = TRUE)

## Dataset B

# Distance to rivers (static)
data_refor[, dist_water_m := extract_values(dist_rivers_r, coords_refor)]
# Distance to urban centers (static)
data_refor[, dist_urban_m := extract_values(dist_urban_r, coords_refor)]

# Distance to roads (dynamic)

# Initialize column
data_refor[, dist_road_m := NA_real_]

unique_years <- sort(unique(data_refor$change_year))

for (yr in unique_years) {
  idx <- which(data_refor$change_year == yr)
  if (length(idx) == 0) next
  
  roads_sub <- roads_sf[roads_sf[[roads_year_col]] <= yr, ]
  
  if (nrow(roads_sub) == 0) {
    data_refor[idx, dist_road_m := NA_real_]
    next
  }
  
  roads_bin <- rasterize_binary(roads_sub, template_rast, 1, NA)
  dist_r <- terra::distance(roads_bin)
  
  data_refor[idx, dist_road_m := extract_values(dist_r, data_refor[idx, .(x,y)])]
}

# Distance to forest edges (dynamic)
# pipeline = forest raster → edge raster → terra::distance() → extract

# Initialize column
data_refor[, dist_edge_m := NA_real_]

# Unique years to loop over
unique_years <- sort(unique(data_refor$change_year))

for (yr in unique_years) {
  
  cat("  Processing forest edges for year:", yr, "\n")
  
  # Identify rows for this year
  idx <- which(data_refor$change_year == yr)
  if (length(idx) == 0) next
  
  # Get the forest raster for the corresponding year
  # ASSUMPTION: rasters_reclass is ordered chronologically: 1989–2024
  r_year <- rasters_reclass[[which(years_lulc == yr)]]
  
  # Build binary forest mask: 1 = forest, NA = other
  forest_mask <- r_year == 1
  forest_mask[forest_mask != 1] <- NA
  
  # landscapemetrics: compute forest boundaries
  # Returns a raster where edge pixels are 1, others NA
  edge_list <- landscapemetrics::get_boundaries(forest_mask,
                                                as_NA = TRUE)
  
  # Result is a list: extract first layer
  edge_rast <- edge_list[[1]]
  
  # Skip if edges missing
  if (all(is.na(values(edge_rast)))) {
    data_refor[idx, dist_edge_m := NA_real_]
    next
  }
  
  # Compute distance raster
  # Distance to nearest edge pixel
  dist_edge_r <- terra::distance(edge_rast)
  
  # Extract distances only for rows of this year
  coords_year <- coords_refor[idx]
  data_refor[idx, dist_edge_m := extract_values(dist_edge_r, coords_year)]
}

data_refor %>% dplyr::group_by(change_code) %>% dplyr::summarise_at(vars(starts_with("dist")), mean, na.rm = TRUE)

cat(" Distances computed.\n")



#### SECTION 7 — SLOPE --------
# Slope value is extracted for each event location.
cat("SECTION 7: extracting slope values\n")

# Solution here : https://gis.stackexchange.com/questions/236337/calculating-proportion-of-land-cover-classes-with-moving-window-around-point-in

# Convert coords to SpatVector
coords_all <- rbind(coords_defor, coords_refor)
coords_all <- vect(coords_all, geom = c("x", "y"), crs = crs(rasters_reclass[[1]]))

radius_m <- 1000
target_classes <- c(1, 3, 4, 5)

pland_values <- list()

for (i in seq_along(rasters_reclass)) {
  
  cat("\nYear", years_lulc[i], "\n")
  r <- rasters_reclass[[i]]
  year <- years_lulc[i]
  
  pland_values[[year]] <- list()
  
  # fetch cell values with 1 km buffer once
  vals <- extract(r, coords_all, buffer = radius_m)
  
  # vals is a list: each element = vector of raster values inside the buffer
  # Now compute PLAND per class
  for (cl in target_classes) {
    
    pland_values[[year]][[paste0("pland_", cl)]] <-
      sapply(vals, function(v) mean(v == cl, na.rm = TRUE) * 100)
  }
}

# # Combine coords_defor + coords_refor into one
# coords_all <- rbind(coords_defor, coords_refor)
# 
# # Convert to SpatVector
# coords_all <- vect(coords_all, geom = c("x", "y"), crs = crs(rasters_reclass[[1]]))
# 
# # List of land-use classes to compute PLAND for 
# target_classes <- c(1, 3, 5)
# 
# radius_m <- 1000  # buffer radius
# 
# # Output list
# pland_values <- list()
# 
# for (i in seq_along(rasters_reclass)) {
#   
#   cat("\nProcessing year", years_lulc[i], "\n")
#   r <- rasters_reclass[[i]]
#   year <- years_lulc[i]
#   
#   pland_values[[as.character(year)]] <- list()
#   
#   for (cl in target_classes) {
#     
#     cat("  → class", cl, "\n")
#     
#     # Binary raster (class = 1, others = 0)
#     r_bin <- r == cl
#     
#     # storage vector for this class/year
#     npts <- nrow(coords_all)
#     pland_vec <- numeric(npts)
#     
#     # loop on points (can be parallelized!)
#     for (j in seq_len(npts)) {
#       
#       pt <- coords_all[j, ]
#       
#       # Build a circular buffer around the point
#       win_poly <- buffer(pt, width = radius_m)
#       
#       # Crop binary raster using the circular window
#       win <- crop(r_bin, win_poly)
#       
#       # Extract values
#       v <- values(win, mat = FALSE)
#       
#       # PLAND = % of pixels equal to 1
#       pland_vec[j] <- mean(v == 1, na.rm = TRUE) * 100
#     }
#     
#     # Store results
#     pland_values[[as.character(year)]][[paste0("pland_", cl)]] <- pland_vec
#   }
# }

cat("PLAND computation complete.\n")




## Prepare the property-based dataset ----------
### Compute the forest proportion on CAR properties ------- 
# mask forest only (value == 1)
forest_2023 = rasters[[35]] == 1

# compute fraction of forest per property
forest_frac = terra::extract(forest_2023, car, fun = mean, na.rm = TRUE)

# extract returns a data.frame: first column is ID
forest_frac = forest_frac %>%
  dplyr::rename(car_forest_pct = layer)

# convert to percent
forest_frac$car_forest_pct = forest_frac$car_forest_pct * 100

# join back into car_sf
car_sf = car_sf %>%
  dplyr::left_join(forest_frac, by = c("car_id" = "ID"))
