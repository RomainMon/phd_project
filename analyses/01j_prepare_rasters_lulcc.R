#------------------------------------------------#
# Author: Romain Monassier
# Objective: Prepare rasters for lulcc analysis
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(terra)
library(sf)
library(landscapemetrics)
library(data.table)
library(stringr)

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
slope = terra::rast(here("data", "geo", "TOPODATA", "work", "slope_bbox.tif"))
plot(slope)

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



### Prepare the cell-based dataset -----
# Rationale:
# Start from rasters_tm[[35]] (cells that changed at least once).
# For each changed cell, extract up to X change events from lulcc_years (one row per event: cell_id, change_rank, change_year).
# For each event row compute the requested groups of covariates: 
# Legal status (public_reserve / private / urban / reserva_legal)
# APA intersection
# Distances (roads - time-aware, rivers, urban centroids, forest edges - computed from rasters_reclass for the event year)
# Mean slope
# PLAND (percent cover per class in 1 km) computed on rasters_reclass for event year


# Parameters
change_codes = c(6,7) # reforest=6, deforest=7 in rasters_tm
pland_radius_m = 1000 # radius for PLAND (meters)
pland_classes = c(1,2,3,4,5) # classes present in rasters_reclass (adjust)
template_rast = rasters_reclass[[35]]  # general template/resolution/extent (adjust if needed)
roads_year_col <- "date_crea" # column name in roads_sf with creation year
out_rds <- "changed_cells_all_events.rds"
out_csv <- "changed_cells_all_events.csv"


# Basic checks & reprojection (meters)
# Ensure vectors are in raster CRS
ensure_crs <- function(sf_obj, target_crs) {
  if(is.null(sf_obj)) return(NULL)
  if(st_crs(sf_obj) != target_crs) return(st_transform(sf_obj, target_crs))
  return(sf_obj)
}
r_crs = crs(template_rast) # terra CRS object (projstring)

# Reproject the sf layers (if they exist)
car_sf <- ensure_crs(car_sf, r_crs)
uniao_sf <- ensure_crs(uniao_sf, r_crs)
pda_sf <- ensure_crs(pda_sf, r_crs)
public_reserves_sf <- dplyr::bind_rows(
  uniao_sf %>% dplyr::mutate(source = "uniao"),
  pda_sf %>% dplyr::mutate(source = "pda")
)
urb_sf <- ensure_crs(urb_sf, r_crs)
rl_sf <- ensure_crs(rl_sf, r_crs)
apa_mld_sf <- ensure_crs(apa_mld_sf, r_crs)
roads_sf <- ensure_crs(roads_sf, r_crs)
rivers_sf <- ensure_crs(rivers_sf, r_crs)
urb_centers_sf <- ensure_crs(urb_centers_sf, r_crs)

# Helpers
# rasterize vectors to binary raster (must specify if the background is 0 or NA)
rasterize_binary <- function(sf_obj, template_rast, value = 1, background = NA) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    r <- rast(template_rast)
    values(r) <- background
    return(r)
  }
  v <- terra::vect(sf_obj)
  r <- terra::rasterize(v, template_rast, field = value, background = background)
  return(r)
}

# safe extract wrapper returning vector of values
extract_values <- function(spatr, coords_df) {
  if(is.null(spatr)) return(rep(NA_real_, nrow(coords_df)))
  ex = terra::extract(spatr, coords_df)
  # terra::extract returns ID column then values; if single-layer returns matrix
  if(is.matrix(ex) || is.data.frame(ex)) {
    if(ncol(ex) >= 2) return(as.vector(ex[,2]))
    if(ncol(ex) == 1) return(as.vector(ex[,1]))
  }
  # fallback
  return(as.vector(ex))
}

#### SECTION 0 — Identify changed cells (cells with >=1 change) --------
cat("SECTION 0: identify changed cells from rasters_tm[[35]]\n")

cumulative_mask <- rasters_tm[[35]]
# Extract only transitions 6 (reforest) and 7 (deforest)
vals <- terra::values(cumulative_mask)
changed_cell_ids <- which(vals %in% c(6, 7))
head(changed_cell_ids)

cat(" Number of changed cells:", length(changed_cell_ids), "\n")


#### SECTION 1 — Build master table of all change events --------
# For each changed cell, read the up to 10 lulcc_years rasters and convert to long table
cat("SECTION 1: building event table from lulcc_years\n")

# extract values of each lulcc_year raster at changed cells
vals_list <- lapply(lulcc_years, function(r) {
  # r is a SpatRaster (values are years or NA)
  terra::values(r)[changed_cell_ids]
})
vals_mat <- do.call(cbind, vals_list) # rows = changed cells, cols = change_ranks (1..10)
colnames(vals_mat) <- paste0("rank_", seq_len(ncol(vals_mat)))
head(vals_mat)

# Build data.table
dt0 <- as.data.table(vals_mat)
dt0[, cell_id := changed_cell_ids]

# convert wide -> long (only non-NA rows)
dt_long <- data.table::melt(dt0,
                id.vars = "cell_id",
                variable.name = "change_rank",
                value.name = "change_year",
                variable.factor = FALSE)

# convert change_rank strings to integer rank
dt_long[, change_rank := as.integer(stringr::str_extract(change_rank, "\\d+"))]
# keep only non-NA events
dt_long <- dt_long[!is.na(change_year)]
setorder(dt_long, cell_id, change_rank)
# add coordinates (x,y)
xy_coords <- terra::xyFromCell(template_rast, dt_long$cell_id)
dt_long[, `:=`(x = xy_coords[,1], y = xy_coords[,2])]

# add change type (from rasters_tm value at the cell in cumulative mask; 6 or 7)
# we assume rasters_tm[[35]] keeps code per cell; extract values
mask_vals <- terra::values(cumulative_mask)[dt_long$cell_id]
dt_long[, change_code := mask_vals]
dt_long[, change_type := ifelse(change_code == 6, "reforest", ifelse(change_code == 7, "deforest", NA_character_))]
# create unique event id
dt_long[, event_id := .I]

cat(" Events to process:", nrow(dt_long), " (", length(unique(dt_long$cell_id)), " unique cells )\n")

# Convert to data.table for speed if not already
setDT(dt_long)
head(dt_long)


#### SECTION 2 — LEGAL STATUS (public_reserve, private, urban, reserva_legal) --------
# We rasterize the polygon layers once and extract values at event coords
cat("SECTION 2: computing legal status and reserva legal\n")
car_r <- rasterize_binary(car_sf, template_rast, value = 1, background = 0)
pub_r <- rasterize_binary(public_reserves_sf, template_rast, value = 1, background = 0)
urb_r <- rasterize_binary(urb_sf, template_rast, value = 1, background = 0)
rl_r  <- rasterize_binary(rl_sf, template_rast, value = 1, background = 0)

coords_df <- dt_long[, .(x,y)]
dt_long[, in_car := as.integer(extract_values(car_r, coords_df) == 1)]
dt_long[, in_public := as.integer(extract_values(pub_r, coords_df) == 1)]
dt_long[, in_urban := as.integer(extract_values(urb_r, coords_df) == 1)]
dt_long[, in_rl := as.logical(extract_values(rl_r, coords_df) == 1)]

# Determine legal_status with priority: urban > public_reserve > private > none
dt_long[, legal_status := fifelse(in_urban == 1, "urban",
                                  fifelse(in_public == 1, "public_reserve",
                                          fifelse(in_car == 1, "private", "none")))]
dt_long %>% dplyr::group_by(legal_status) %>% dplyr::summarise(n=dplyr::n_distinct(cell_id))
cat(" Legal status assigned.\n")


#### SECTION 3 — INTERSECT WITH APA (binary) --------
cat("SECTION 3: APA intersection\n")
apa_r <- car_r <- rasterize_binary(apa_mld_sf, template_rast, value = 1, background = 0)
dt_long[, in_APA := as.integer(extract_values(apa_r, coords_df) == 1)]
dt_long %>% 
  dplyr::filter(in_APA == 1) %>% 
  dplyr::summarise(n=dplyr::n_distinct(cell_id))

cat(" APA computed.\n")

#### SECTION 4 — DISTANCES --------
cat("SECTION 4: computing distances (rivers, urban centers, roads, forest edges)\n")

coords_dt <- dt_long[, .(x, y)]

# 4.1 Distance to rivers
rivers_bin <- rasterize_binary(rivers_sf, template_rast, value = 1, background = NA) # Distance only works if background = NA (not 0)
dist_rivers_r <- terra::distance(rivers_bin)
dt_long[, dist_water_m := extract_values(dist_rivers_r, coords_dt)]


# 4.2 Distance to urban centers
urban_bin <- rasterize_binary(urb_centers_sf, template_rast, value = 1, background = NA)
dist_urban_r <- terra::distance(urban_bin)
dt_long[, dist_urban_m := extract_values(dist_urban_r, coords_dt)]


# 4.3 Distance to roads (dynamic by year)
dt_long[, dist_road_m := NA_real_]   # initialize

unique_years <- sort(unique(dt_long$change_year))

for (yr in unique_years) {
  
  # subset rows for this year
  idx <- which(dt_long$change_year == yr)
  if (length(idx) == 0) next
  
  # subset coords
  coords_year <- dt_long[idx, .(x, y)]
  
  # rasterize roads existing that year
  roads_sub <- roads_sf[roads_sf[[roads_year_col]] <= yr, ]
  if (nrow(roads_sub) == 0) {
    dt_long[idx, dist_road_m := NA_real_]
    next
  }
  
  roads_bin <- rasterize_binary(roads_sub, template_rast, value = 1, background = NA)
  dist_r <- terra::distance(roads_bin)
  
  # extract only for those rows
  dt_long[idx, dist_road_m := extract_values(dist_r, coords_year)]
}


# 4.4 Distance to forest edges (dynamic by year)
# # forest mask 
# forest_2023 = rasters[[35]] == 1 
# forest_2023[rasters[[35]] != 1] = NA 
# plot(forest_2023) 
# # compute patches (raster -> list of rasters, one per patch group) 
# p = landscapemetrics::get_patches(forest_2023, class = 1, directions = 8) 
# # convert them to a single terra SpatVector 
# patch_raster = p[[1]][[1]] # layer_1$class_1 equivalent 
# patch_raster[is.na(patch_raster)] = NA 
# # extract edges 
# edge = landscapemetrics::get_boundaries(patch_raster, as_NA = TRUE) 
# plot(edge[[1]], col="lightpink") 
# edge_raster = edge[[1]] 
# # Polygonize edges (dissolve identical patch IDs) 
# edge_vect = terra::as.polygons(edge_raster, dissolve = TRUE, na.rm = TRUE) 
# # Convert to sf 
# edge_sf = sf::st_as_sf(edge_vect) 
# # distance to forest edges 
# d_edge = sf::st_distance(lulc_centroids, edge_sf) 
# dist_to_edge = apply(d_edge, 1, min) # nearest per pixel 
# # join back 
# dist_edge_df = data.frame( cell_id = lulc_2023_sf$cell_id, dist_edge_m = as.numeric(dist_to_edge) ) 
# lulc_2023_sf = lulc_2023_sf %>% dplyr::left_join(dist_edge_df, by = "cell_id")


cat(" Distances computed.\n")

#### SECTION 5 — SLOPE --------
cat("SECTION 5: extracting slope values\n")


#### SECTION 6 — LULC --------
cat("SECTION 6: computing PLAND in buffer\n")


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
