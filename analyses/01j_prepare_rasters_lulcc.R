#------------------------------------------------#
# Author: Romain Monassier
# Objective: Prepare datasets for lulcc analysis
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(terra)
library(sf)
library(data.table)
library(stringr)
library(landscapemetrics)
library(ggplot2)
library(cowplot)
library(exactextractr)

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



### Helpers -----------
# Reproject an sf object only if CRS differs from target
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

# Extract raster values at coordinates, always returning a numeric vector
extract_values <- function(r, coords_df) {
  if (is.null(r)) return(rep(NA_real_, nrow(coords_df)))
  out <- terra::extract(r, coords_df)
  if (is.data.frame(out) && ncol(out) >= 2) return(out[[2]])
  if (is.data.frame(out) && ncol(out) == 1) return(out[[1]])
  as.numeric(out)
}


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

#### SECTION 0 — Identify changed cells (cells with >=1 change) --------
cat("\nSECTION 0: Identify changed cells\n")

# Use last raster in the list to get all changes
cumulative_mask <- rasters_tm[[length(rasters_tm)]]

# Get indices of cells with reforest/deforest events
changed_cell_ids <- which(values(cumulative_mask) %in% change_codes)

# Sanity check
stopifnot(length(changed_cell_ids) > 0)

cat("Changed cells:", length(changed_cell_ids), "\n")


#### SECTION 1 — Build master table of all change events --------
# For each changed cell, read the 10 lulcc_years rasters and convert to long table
cat("\nSECTION 1: Build event table\n")

vals_list <- lapply(lulcc_years, function(r) terra::values(r)[changed_cell_ids])
vals_mat  <- do.call(cbind, vals_list)
colnames(vals_mat) <- paste0("rank_", seq_len(ncol(vals_mat)))
head(vals_mat)

dt0 <- as.data.table(vals_mat)
dt0 <- dt0 %>%
  dplyr::mutate(cell_id = changed_cell_ids)

# to long format
dt_long <- dt0 %>%
  tidyr::pivot_longer(
    cols = -cell_id,
    names_to = "change_rank",
    values_to = "change_year"
  ) %>%
  dplyr::filter(!is.na(change_year)) %>%
  dplyr::mutate(
    change_rank = stringr::str_extract(change_rank, "\\d+") %>% as.integer()
  )
head(dt_long)

# add coordinates
xy <- terra::xyFromCell(template_rast, dt_long$cell_id)
dt_long <- dt_long %>%
  dplyr::mutate(
    x = xy[, 1],
    y = xy[, 2]
  )
head(dt_long)

# add change code & type
mask_vals <- vals[dt_long$cell_id]
dt_long <- dt_long %>%
  dplyr::mutate(
    change_code = mask_vals,
    change_type = dplyr::case_when(
      change_code == 6 ~ "reforest",
      change_code == 7 ~ "deforest",
      TRUE ~ NA_character_
    )
  )
head(dt_long)

cat("Events:", nrow(dt_long), "\n")



#### SECTION 2 — Select controls (i.e., intact forests) by year ---------
# We select balanced controls (i.e., intact forests)
# We determine how many reforest events and deforest events occurred.
# We identify cells with intact forest (value = 1 in rasters_tm[[year]]).
# We randomly sample exactly the same number of intact cells.

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
events_per_year <- dt_long %>%
  dplyr::count(change_year, change_code, name = "N")
colnames(events_per_year)[3] <- "n_events"
head(events_per_year)

# Keep only codes 6 (reforest) and 7 (deforest)
events_per_year <- events_per_year %>%
  dplyr::filter(change_code %in% c(6, 7))
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
# subset reforest events
reforest_events <- events_per_year %>%
  dplyr::filter(change_code == 6)
# create reforest_controls
reforest_controls <- reforest_events %>%
  dplyr::group_by(change_year) %>%
  dplyr::summarise(
    sampled_cells = list(
      sample_intact(change_year, n_events)
    ),
    .groups = "drop"
  )
head(reforest_controls)

# Flatten
# reforest_controls$sampled_cells is a list where each element contains a vector of sampled cell IDs for a given year.
# reforest_controls$change_year is a vector of years (one per list element).
reforest_controls_dt <- tibble::tibble(
  cell_id = unlist(reforest_controls$sampled_cells), # unlist() flattens the list into one long vector of all sampled cell IDs.
  change_year = rep(
    reforest_controls$change_year,
    times = lengths(reforest_controls$sampled_cells) # lengths(reforest_controls$sampled_cells) gives, for each year, how many sampled cells were drawn.
    # rep(..., times = ...) repeats each year as many times as the number of sampled cells for that year — so each sampled cell gets the correct year.
  )
) %>%
  dplyr::mutate(
    change_type = "control",
    change_code = 1 # intact forest
  )
head(reforest_controls_dt)

# Add coordinates
xy_ref <- terra::xyFromCell(template_rast, reforest_controls_dt$cell_id)
reforest_controls_dt <- reforest_controls_dt %>%
  dplyr::mutate(
    x = xy_ref[, 1],
    y = xy_ref[, 2]
  )
head(reforest_controls_dt)

# Step 5: Select intact controls for deforest (=7)
deforest_events <- events_per_year %>%
  dplyr::filter(change_code == 7)

# create deforest_controls by sampling intact cells per year
deforest_controls <- deforest_events %>%
  dplyr::group_by(change_year) %>%
  dplyr::summarise(
    sampled_cells = list(
      sample_intact(change_year, n_events)
    ),
    .groups = "drop"
  )

# convert list of sampled cells into a long data frame
deforest_controls_dt <- tibble::tibble(
  cell_id = unlist(deforest_controls$sampled_cells),
  change_year = rep(
    deforest_controls$change_year,
    times = lengths(deforest_controls$sampled_cells)
  )
) %>%
  dplyr::mutate(
    change_type = "control",  # label as control cells
    change_code = 1            # intact cells
  )

# Add coordinates
xy_def <- terra::xyFromCell(template_rast, deforest_controls_dt$cell_id)
deforest_controls_dt <- deforest_controls_dt %>%
  dplyr::mutate(
    x = xy_def[, 1],
    y = xy_def[, 2]
  )
head(deforest_controls_dt)

#### SECTION 3 — Build final datasets for modelling -------

# Dataset A: Reforest events + control cells
data_refor <- dplyr::bind_rows(
  dt_long %>%
    dplyr::filter(change_code == 6) %>%
    dplyr::select(cell_id, change_year, change_code, change_type, x, y),
  reforest_controls_dt
)
head(data_refor)

# Dataset B: Deforest events + control cells
data_defor <- dplyr::bind_rows(
  dt_long %>%
    dplyr::filter(change_code == 7) %>%
    dplyr::select(cell_id, change_year, change_code, change_type, x, y),
  deforest_controls_dt
)
head(data_defor)


# Quick check
data_refor %>% dplyr::group_by(change_code) %>% dplyr::summarise(n=n())
data_defor %>% dplyr::group_by(change_code) %>% dplyr::summarise(n=n())

cat("Dataset A rows:", nrow(data_refor), "\n")
cat("Dataset B rows:", nrow(data_defor), "\n")



#### SECTION 4 — LEGAL STATUS (public_reserve, private, urban, reserva_legal) --------
# This block overlays the event cell with several binary spatial layers: public_reserve (União + Poço das Antas), private (CAR registry), urban (urban polygons), reserva_legal
# Each layer is rasterized onto the reference grid and intersected with event coordinates. Legal status is a hierarchical categorical variable computed by combining the four binary indicators.

cat("\nSECTION 4: Legal status\n")

# Rasterize data
car_r <- rasterize_binary(car_sf, template_rast, value = 1, background = 0)
pub_r <- rasterize_binary(public_reserves_sf, template_rast, value = 1, background = 0)
urb_r <- rasterize_binary(urb_sf, template_rast, value = 1, background = 0)
rl_r <- rasterize_binary(rl_sf,  template_rast, value = 1, background = 0)

## Dataset A
coords_defor <- data_defor %>%
  dplyr::select(x, y)

data_defor <- data_defor %>%
  dplyr::mutate(
    in_car    = extract_values(car_r, coords_defor),
    in_public = extract_values(pub_r, coords_defor),
    in_urban  = extract_values(urb_r, coords_defor),
    in_rl     = extract_values(rl_r,  coords_defor)
  )

data_defor <- data_defor %>%
  dplyr::mutate(
    in_car    = as.integer(in_car == 1),
    in_public = as.integer(in_public == 1),
    in_urban  = as.integer(in_urban == 1),
    in_rl     = as.logical(in_rl == 1)
  )

# Categorize
data_defor <- data_defor %>%
  dplyr::mutate(
    legal_status = dplyr::case_when(
      in_urban  == 1 ~ "urban",
      in_public == 1 ~ "public_reserve",
      in_car    == 1 ~ "private",
      TRUE            ~ "none"
    )
  )
table(data_defor$change_code, data_defor$legal_status)

## Dataset B
# Extract coordinates
coords_refor <- data_refor %>%
  dplyr::select(x, y)

# Extract raster values and recode columns
data_refor <- data_refor %>%
  dplyr::mutate(
    in_car    = extract_values(car_r, coords_refor),
    in_public = extract_values(pub_r, coords_refor),
    in_urban  = extract_values(urb_r, coords_refor),
    in_rl     = extract_values(rl_r,  coords_refor)
  ) %>%
  dplyr::mutate(
    in_car    = as.integer(in_car == 1),
    in_public = as.integer(in_public == 1),
    in_urban  = as.integer(in_urban == 1),
    in_rl     = as.logical(in_rl == 1)
  ) %>%
  # Categorize legal status
  dplyr::mutate(
    legal_status = dplyr::case_when(
      in_urban  == 1 ~ "urban",
      in_public == 1 ~ "public_reserve",
      in_car    == 1 ~ "private",
      TRUE            ~ "none"
    )
  )
table(data_refor$change_code, data_refor$legal_status)

cat("Legal status computed\n")


#### SECTION 5 — INTERSECT WITH APA (binary) --------
# A binary indicator (in_APA) is added for each event.

cat("\nSECTION 5: APA intersection\n")

# Rasterize
apa_r <- rasterize_binary(apa_mld_sf, template_rast, value = 1, background = 0)

## Dataset A
data_defor <- data_defor %>%
  dplyr::mutate(
    in_APA = as.integer(extract_values(apa_r, coords_defor) == 1)
  )
table(data_defor$change_code, data_defor$in_APA)

## Dataset B
data_refor <- data_refor %>%
  dplyr::mutate(
    in_APA = as.integer(extract_values(apa_r, coords_refor) == 1)
  )
table(data_refor$change_code, data_refor$in_APA)

cat("APA intersection done\n")


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
data_defor <- data_defor %>%
  dplyr::mutate(
    dist_water_m = extract_values(dist_rivers_r, coords_defor),  # distance to rivers
    dist_urban_m = extract_values(dist_urban_r, coords_defor)    # distance to urban centers
  )

# Distance to roads (dynamic)
# To account for creation dates of roads, we subset roads whose creation date ≤ change year

# Initialize column
data_defor$dist_road_m <- NA_real_

for (yr in unique_years) {
  
  # indices of rows for this year
  idx <- which(data_defor$change_year == yr)
  if (length(idx) == 0) next
  
  # subset roads up to this year
  roads_sub <- roads_sf[roads_sf[[roads_year_col]] <= yr, ]
  
  if (nrow(roads_sub) == 0) {
    data_defor$dist_road_m[idx] <- NA_real_
    next
  }
  
  # rasterize roads and compute distance raster
  roads_bin <- rasterize_binary(roads_sub, template_rast, 1, NA)
  dist_r <- terra::distance(roads_bin)
  
  # extract distances for the points of this year
  extracted_dist <- extract_values(dist_r, data_defor[idx, c("x", "y")])
  
  # assign distances directly to the subset of rows
  data_defor$dist_road_m[idx] <- extracted_dist
}


# Distance to forest edges (dynamic)
# To account for the forest dynamics through time, we select the corresponding LULC map from rasters_reclass
# We build binary forest mask (r == 1)
# We use landscapemetrics::get_boundaries() to get edge pixels
# We compute Euclidean distance with terra::distance()

# Initialize column
data_defor <- data_defor %>%
  dplyr::mutate(dist_edge_m = NA_real_)


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
  
  # Compute forest edges
  edge_list <- landscapemetrics::get_boundaries(forest_mask, as_NA = TRUE)
  edge_rast <- edge_list[[1]]
  
  # Skip if edges missing
  if (all(is.na(terra::values(edge_rast)))) {
    data_defor$dist_edge_m[idx] <- NA_real_
    next
  }
  
  # Compute distance raster
  dist_edge_r <- terra::distance(edge_rast)
  
  # Extract distances for rows of this year
  coords_year <- coords_defor[idx, ]
  extracted_dist <- extract_values(dist_edge_r, coords_year)
  
  # Assign distances directly to the relevant rows
  data_defor$dist_edge_m[idx] <- extracted_dist
}

data_defor %>% dplyr::group_by(change_code) %>% dplyr::summarise_at(vars(starts_with("dist")), mean, na.rm = TRUE)


## Dataset B
data_refor <- data_refor %>%
  dplyr::mutate(
    dist_water_m = extract_values(dist_rivers_r, coords_refor),  # distance to rivers
    dist_urban_m = extract_values(dist_urban_r, coords_refor)    # distance to urban centers
  )

# Distance to roads (dynamic)
# To account for creation dates of roads, we subset roads whose creation date ≤ change year

# Initialize column
data_refor$dist_road_m <- NA_real_

for (yr in unique_years) {
  
  # indices of rows for this year
  idx <- which(data_refor$change_year == yr)
  if (length(idx) == 0) next
  
  # subset roads up to this year
  roads_sub <- roads_sf[roads_sf[[roads_year_col]] <= yr, ]
  
  if (nrow(roads_sub) == 0) {
    data_refor$dist_road_m[idx] <- NA_real_
    next
  }
  
  # rasterize roads and compute distance raster
  roads_bin <- rasterize_binary(roads_sub, template_rast, 1, NA)
  dist_r <- terra::distance(roads_bin)
  
  # extract distances for the points of this year
  extracted_dist <- extract_values(dist_r, data_refor[idx, c("x", "y")])
  
  # assign distances directly to the subset of rows
  data_refor$dist_road_m[idx] <- extracted_dist
}


# Distance to forest edges (dynamic)
# To account for the forest dynamics through time, we select the corresponding LULC map from rasters_reclass
# We build binary forest mask (r == 1)
# We use landscapemetrics::get_boundaries() to get edge pixels
# We compute Euclidean distance with terra::distance()

# Initialize column
data_refor <- data_refor %>%
  dplyr::mutate(dist_edge_m = NA_real_)

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
  
  # Compute forest edges
  edge_list <- landscapemetrics::get_boundaries(forest_mask, as_NA = TRUE)
  edge_rast <- edge_list[[1]]
  
  # Skip if edges missing
  if (all(is.na(terra::values(edge_rast)))) {
    data_refor$dist_edge_m[idx] <- NA_real_
    next
  }
  
  # Compute distance raster
  dist_edge_r <- terra::distance(edge_rast)
  
  # Extract distances for rows of this year
  coords_year <- coords_refor[idx, ]
  extracted_dist <- extract_values(dist_edge_r, coords_year)
  
  # Assign distances directly to the relevant rows
  data_refor$dist_edge_m[idx] <- extracted_dist
}

data_refor %>% dplyr::group_by(change_code) %>% dplyr::summarise_at(vars(starts_with("dist")), mean, na.rm = TRUE)

cat(" Distances computed.\n")


#### SECTION 7 — SLOPE --------
# Slope value is extracted for each event location.
cat("SECTION 7: extracting slope values\n")

## Dataset A
data_defor <- data_defor %>%
  dplyr::mutate(
    slope_pct = extract_values(slope_r, coords_defor)
  )
summary(data_defor$slope_pct)

## Dataset B
data_refor <- data_refor %>%
  dplyr::mutate(
    slope_pct = extract_values(slope_r, coords_refor)
  )
summary(data_refor$slope_pct)

cat("Slope extraction completed\n")



#### SECTION 8 - LAND USE AREA ----------

cat("\nSECTION 8: Land use area extraction around buffers\n")
names(rasters_reclass) <- years_lulc

compute_landuse_buffer <- function(dt, rasters_reclass, classes, radius) {
  
  cat("\n--- Starting land use area extraction ---\n")
  
  # resolution and pixel area
  pixel_res  <- res(rasters_reclass[[1]])[1]
  pixel_area <- pixel_res^2
  
  cat("Pixel resolution:", pixel_res, "m\n")
  cat("Pixel area:", pixel_area, "m²\n")
  
  # circular weight matrix (1 km radius)
  cat("Creating focal matrix...\n")
  w <- terra::focalMat(rasters_reclass[[1]], type = "circle", d = radius, fillNA = TRUE)
  
  # list of unique years present in this dataset
  years <- sort(unique(dt$change_year))
  
  # loop over years
  for (year in years) {
    
    cat("\nProcessing YEAR:", year, "\n")
    
    # select raster for this year
    r_year <- rasters_reclass[[as.character(year)]]
    
    # extract the coordinates for this year
    pts_year <- dt %>%
      dplyr::filter(change_year == year) %>%
      dplyr::select(x, y)
    
    pts_vect <- terra::vect(pts_year, geom = c("x", "y"), crs = crs(r_year))
    
    # loop over classes
    for (cl in classes) {
      
      cat("   → Class", cl, "...\n")
      
      # binary raster for that class
      r_bin <- r_year == cl
      
      # moving window sum
      focal_count <- terra::focal(r_bin, w = w, fun = "sum", na.rm = TRUE)
      
      # convert pixel count to area
      focal_area_m2 <- focal_count * pixel_area
      
      # extract area values for the points
      area_values <- terra::extract(focal_area_m2, pts_vect)[, 2]
      
      # assign values back into the dataframe
      col_name <- paste0("area_m2_class_", cl)
      dt[[col_name]][dt$change_year == year] <- area_values
    }
  }
  
  cat("\n--- Extraction finished successfully ---\n")
  return(dt)
}

data_defor <- compute_landuse_buffer(data_defor, rasters_reclass, classes = c(1,3,5), radius = 1000)
data_refor <- compute_landuse_buffer(data_refor, rasters_reclass, classes = c(1,3,5), radius = 1000)

data_defor %>% dplyr::group_by(change_code) %>% dplyr::summarise_at(vars(starts_with("area")), mean, na.rm = TRUE)
data_refor %>% dplyr::group_by(change_code) %>% dplyr::summarise_at(vars(starts_with("area")), mean, na.rm = TRUE)

cat("Land use area computation complete.\n")

#### Export datasets ----------
saveRDS(data_defor,
        here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_pixel.rds"))

saveRDS(data_refor,
        here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_refor_pixel.rds"))


#### Illustration ----------
### Select one point from each dataset
set.seed(123)

pt_def <- data_defor %>% dplyr::slice_sample(n = 1)
pt_refor <- data_refor %>% dplyr::slice_sample(n = 1)

cat("Selected defor point:", pt_def$cell_id, "year:", pt_def$change_year, "\n")
cat("Selected refor point:", pt_refor$cell_id, "year:", pt_refor$change_year, "\n")

### Helper function to make map for 1 point
make_point_plot <- function(pt, rasters_reclass, years_lulc,
                            roads_sf, rivers_sf, car_sf, rl_sf, urb_sf,
                            public_reserves_sf, apa_mld_sf,
                            radius = 1000) {
  
  year <- pt$change_year
  r_year <- rasters_reclass[[which(years_lulc == year)]]
  
  # convert point → sf
  pt_sf <- st_as_sf(pt, coords = c("x","y"), crs = crs(r_year))
  
  # 1 km buffer
  buf <- st_buffer(pt_sf, dist = radius)
  
  # crop raster to 2 km window for clarity
  r_crop <- terra::crop(r_year, vect(st_buffer(pt_sf, 2000)))
  
  # convert raster → dataframe for ggplot
  r_df <- as.data.frame(r_crop, xy = TRUE)
  colnames(r_df)[3] <- "lulc"
  
  # crop vectors
  roads_c  <- st_intersection(roads_sf, st_buffer(pt_sf, 2000))
  rivers_c <- st_intersection(rivers_sf, st_buffer(pt_sf, 2000))
  car_c    <- st_intersection(car_sf, st_buffer(pt_sf, 2000))
  rl_c     <- st_intersection(rl_sf, st_buffer(pt_sf, 2000))
  urb_c    <- st_intersection(urb_sf, st_buffer(pt_sf, 2000))
  pub_c    <- st_intersection(public_reserves_sf, st_buffer(pt_sf, 2000))
  apa_c    <- st_intersection(apa_mld_sf, st_buffer(pt_sf, 2000))
  
  # COLORS for lulc: same as your plot
  lut <- c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e")
  names(lut) <- as.character(sort(unique(r_df$lulc)))
  
  # Make ggplot
  p <- ggplot() +
    geom_raster(data = r_df, aes(x, y, fill = factor(lulc))) +
    scale_fill_manual(values = lut, name = "LULC") +
    geom_sf(data = roads_c,  color = "black", size = 0.4) +
    geom_sf(data = rivers_c, color = "blue", size = 0.5) +
    geom_sf(data = car_c,    fill = NA, color = "brown", linetype = 3) +
    geom_sf(data = rl_c,     fill = NA, color = "green3", linetype = 2) +
    geom_sf(data = urb_c,    fill = NA, color = "magenta", alpha = 0.4) +
    geom_sf(data = pub_c,    fill = NA, color = "red", size = 0.7) +
    geom_sf(data = apa_c,    fill = NA, color = "orange", linetype = 2) +
    
    geom_sf(data = buf, fill = NA, color = "yellow", linetype = 2, size = 1) +
    geom_sf(data = pt_sf, shape = 21, fill = "red", size = 3) +
    coord_sf() +
    theme_minimal()
  
  ### Covariate text box
  txt <- paste0(
    "change_code = ", pt$change_code, "\n",
    "legal = ", pt$legal_status, "\n",
    "dist_water_m = ", round(pt$dist_water_m), "\n",
    "dist_urban_m = ", round(pt$dist_urban_m), "\n",
    "dist_road_m = ", round(pt$dist_road_m), "\n",
    "dist_edge_m = ", round(pt$dist_edge_m), "\n",
    "slope_pct = ", round(pt$slope_pct, 1), "\n",
    "area_class_1 = ", round(pt$area_m2_class_1), "\n",
    "area_class_3 = ", round(pt$area_m2_class_3), "\n",
    "area_class_5 = ", round(pt$area_m2_class_5)
  )
  
  p_label <- ggdraw() + draw_label(txt, x = 0, y = 1, hjust = 0, vjust = 1, size = 11)
  
  return(plot_grid(p, p_label, ncol = 2, rel_widths = c(1.3, 0.9)))
}


### Generate both plots
p_def   <- make_point_plot(pt_def,   rasters_reclass, years_lulc,
                           roads_sf, rivers_sf, car_sf, rl_sf, urb_sf,
                           public_reserves_sf, apa_mld_sf)

p_refor <- make_point_plot(pt_refor, rasters_reclass, years_lulc,
                           roads_sf, rivers_sf, car_sf, rl_sf, urb_sf,
                           public_reserves_sf, apa_mld_sf)

### Show side-by-side
final_plot <- plot_grid(
  p_def,
  p_refor,
  ncol = 1,
  labels = c("DEFORESTATION SAMPLE", "REFORESTATION SAMPLE"),
  label_size = 14
)

print(final_plot)

# Export PNG (high resolution)
ggsave(
  filename = here("outputs", "plot", "01j_defor_refor_dataset_demo.png"),
  plot = final_plot,
  width = 12,
  height = 16,
  dpi = 300,
  bg="white"
)



## Prepare the property-based dataset ----------
# Here, the spatial unit is the private property (features in CAR)
# We measure covariates at the property scale

# Base dataset from car_sf
car_dt <- tibble::tibble(
  car_id = car_sf$car_id,
  cod_imovel = car_sf$cod_imovel,
  car_area_m2 = car_sf$car_area_m2,
  car_area_ha = car_sf$car_area_ha
)

# Pixel area
r <- rasters_tm[[35]]
px_area_m2 <- terra::res(r)[1] * terra::res(r)[2]
px_area_ha <- px_area_m2 / 10000
px_area_ha

# Remove properties < 1 pixel
car_dt = car_dt %>% 
  dplyr::filter(car_area_m2 > px_area_m2)

# Properties filtered to match car_dt
car_sf_filtered <- car_sf %>%
  dplyr::filter(car_id %in% car_dt$car_id)

#### SECTION 1 — Compute reforestation and deforestation areas ------

# Use the last transition map
tm_last <- rasters_tm[[length(rasters_tm)]]

# Extract values for each CAR property
ext_vals <- terra::extract(tm_last, vect(car_sf))

# Convert
ext_dt <- tibble::as_tibble(ext_vals)
# Identify the value column (all columns except "ID")
val_col <- setdiff(names(ext_dt), "ID")
# Rename that column to "value"
ext_dt <- ext_dt %>%
  dplyr::rename(value = dplyr::all_of(val_col))

# Count pixels per CAR
pixel_summary <- ext_dt %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    n_deforest  = sum(value == 7, na.rm = TRUE),
    n_reforest  = sum(value == 6, na.rm = TRUE),
    n_total_pix = sum(!is.na(value)),
    .groups = "drop"
  )

# Rename ID → car_id for consistent join
pixel_summary <- pixel_summary %>%
  dplyr::rename(car_id = ID)

# Merge with base property table
car_dt <- merge(
  car_dt,
  pixel_summary,
  by = "car_id",
  all.x = TRUE
)

# Compute reforested/deforested surface areas in ha
car_dt <- car_dt %>%
  dplyr::mutate(
    area_deforest_ha = round(n_deforest * px_area_ha, 2),
    area_reforest_ha = round(n_reforest * px_area_ha, 2)
  )

# Compute proportions
car_dt <- car_dt %>%
  dplyr::mutate(
    prop_deforest  = round(n_deforest / n_total_pix, 2),
    prop_reforest  = round(n_reforest / n_total_pix, 2)
  )


#### SECTION 2 — Compute land use on properties --------
# Use first land-use raster (1989)
lu1989 <- rasters_reclass[[1]]

# Extract values per property
ext_vals_1989 <- terra::extract(lu1989, vect(car_sf))

# Convert
dt1989 <- tibble::as_tibble(ext_vals_1989)
dt1989 <- dt1989 %>%
  # Identify the value column and rename it to "value"
  dplyr::rename(value = dplyr::all_of(setdiff(names(dt1989), "ID"))) %>%
  # Keep only the classes of interest
  dplyr::filter(value %in% c(1, 3, 5))

# Count per class and property
lu1989_summary <- dt1989 %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    n_forest       = sum(value == 1, na.rm = TRUE),
    n_agriculture  = sum(value == 3, na.rm = TRUE),
    n_urban        = sum(value == 5, na.rm = TRUE),
    
    ha_forest      = round(sum(value == 1, na.rm = TRUE) * px_area_ha, 2),
    ha_agriculture = round(sum(value == 3, na.rm = TRUE) * px_area_ha, 2),
    ha_urban       = round(sum(value == 5, na.rm = TRUE) * px_area_ha, 2),
    
    .groups = "drop"
  )

# Rename ID → car_id to merge properly
lu1989_summary <- lu1989_summary %>%
  dplyr::rename(car_id = ID)

# Merge with car_dt
car_dt <- merge(
  car_dt,
  lu1989_summary,
  by = "car_id",
  all.x = TRUE
)

# Remove potential NAs
car_dt <- car_dt %>%
  dplyr::mutate(
    n_forest = dplyr::coalesce(n_forest, 0),
    n_agriculture  = dplyr::coalesce(n_agriculture, 0),
    n_urban = dplyr::coalesce(n_urban, 0),
    ha_forest = dplyr::coalesce(ha_forest, 0),
    ha_agriculture = dplyr::coalesce(ha_agriculture, 0),
    ha_urban  = dplyr::coalesce(ha_urban, 0)
  )

# Add proportions
car_dt <- car_dt %>%
  dplyr::mutate(
    prop_forest = round(n_forest / n_total_pix, 3),
    prop_agriculture = round(n_agriculture / n_total_pix, 3),
    prop_urban = round(n_urban / n_total_pix, 3)
  )

# Quick correlations
cor(car_dt$n_deforest, car_dt$n_forest)
cor(car_dt$n_deforest, car_dt$n_agriculture)
cor(car_dt$n_deforest, car_dt$n_urban)
cor(car_dt$n_reforest, car_dt$n_forest)
cor(car_dt$n_reforest, car_dt$n_agriculture)
cor(car_dt$n_reforest, car_dt$n_urban)


#### SECTION 3 — Compute land use around properties --------
# 1 km buffer (in metres)
car_buffer <- terra::buffer(vect(car_sf), width = 1000)
plot(car_buffer)

# Extract land use (first year)
# 1989 raster
lu1989 <- rasters_reclass[[1]]

# Extract values inside buffers
ext1989 <- terra::extract(lu1989, car_buffer)

dt1989_buf <- tibble::as_tibble(ext1989)
# Identify the value column and rename it to "value"
val_col <- setdiff(names(dt1989_buf), "ID")
dt1989_buf <- dt1989_buf %>%
  dplyr::rename(value = dplyr::all_of(val_col))

# Keep only classes 1, 3, 5
dt1989_buf <- dt1989_buf %>%
  dplyr::filter(value %in% c(1, 3, 5))

# Summarize
buf1989_summary <- dt1989_buf %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    buf1989_n_forest      = sum(value == 1, na.rm = TRUE),
    buf1989_n_agriculture = sum(value == 3, na.rm = TRUE),
    buf1989_n_urban       = sum(value == 5, na.rm = TRUE),
    
    buf1989_ha_forest      = round(sum(value == 1, na.rm = TRUE) * px_area_ha, 2),
    buf1989_ha_agriculture = round(sum(value == 3, na.rm = TRUE) * px_area_ha, 2),
    buf1989_ha_urban       = round(sum(value == 5, na.rm = TRUE) * px_area_ha, 2),
    
    .groups = "drop"
  ) %>%
  dplyr::rename(car_id = ID)

# Extract land use (last year)
lu2024 <- rasters_reclass[[length(rasters_reclass)]]

ext2024 <- terra::extract(lu2024, car_buffer)

dt2024_buf <- tibble::as_tibble(ext2024)

# Identify the value column and rename it to "value"
val_col <- setdiff(names(dt2024_buf), "ID")
dt2024_buf <- dt2024_buf %>%
  dplyr::rename(value = dplyr::all_of(val_col)) %>%
  dplyr::filter(value %in% c(1, 3, 5))

# Summarise counts and areas per class
buf2024_summary <- dt2024_buf %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    buf2024_n_forest      = sum(value == 1, na.rm = TRUE),
    buf2024_n_agriculture = sum(value == 3, na.rm = TRUE),
    buf2024_n_urban       = sum(value == 5, na.rm = TRUE),
    
    buf2024_ha_forest      = round(sum(value == 1, na.rm = TRUE) * px_area_ha, 2),
    buf2024_ha_agriculture = round(sum(value == 3, na.rm = TRUE) * px_area_ha, 2),
    buf2024_ha_urban       = round(sum(value == 5, na.rm = TRUE) * px_area_ha, 2),
    
    .groups = "drop"
  ) %>%
  dplyr::rename(car_id = ID)

# Merge
car_dt <- merge(car_dt, buf1989_summary, by = "car_id", all.x = TRUE)
car_dt <- merge(car_dt, buf2024_summary, by = "car_id", all.x = TRUE)

# Compute % evolution
car_dt <- car_dt %>%
  dplyr::mutate(
    # Absolute change in PIXELS
    buf_change_pix_forest      = buf2024_n_forest      - buf1989_n_forest,
    buf_change_pix_agriculture = buf2024_n_agriculture - buf1989_n_agriculture,
    buf_change_pix_urban       = buf2024_n_urban       - buf1989_n_urban,
    
    # Replace all NAs with 0 BEFORE pct calculations
    buf1989_n_forest      = dplyr::if_else(is.na(buf1989_n_forest), 0L, buf1989_n_forest),
    buf1989_n_agriculture = dplyr::if_else(is.na(buf1989_n_agriculture), 0L, buf1989_n_agriculture),
    buf1989_n_urban       = dplyr::if_else(is.na(buf1989_n_urban), 0L, buf1989_n_urban),
    
    buf2024_n_forest      = dplyr::if_else(is.na(buf2024_n_forest), 0L, buf2024_n_forest),
    buf2024_n_agriculture = dplyr::if_else(is.na(buf2024_n_agriculture), 0L, buf2024_n_agriculture),
    buf2024_n_urban       = dplyr::if_else(is.na(buf2024_n_urban), 0L, buf2024_n_urban),
    
    # Percent change = (Δpixels / initial pixels) * 100
    buf_change_pct_forest      = dplyr::if_else(buf1989_n_forest      > 0, 
                                                100 * buf_change_pix_forest      / buf1989_n_forest,      0),
    buf_change_pct_agriculture = dplyr::if_else(buf1989_n_agriculture > 0, 
                                                100 * buf_change_pix_agriculture / buf1989_n_agriculture, 0),
    buf_change_pct_urban       = dplyr::if_else(buf1989_n_urban       > 0, 
                                                100 * buf_change_pix_urban       / buf1989_n_urban,       0),
    # Change in HECTARES
    buf_change_ha_forest      = buf_change_pix_forest      * px_area_ha,
    buf_change_ha_agriculture = buf_change_pix_agriculture * px_area_ha,
    buf_change_ha_urban       = buf_change_pix_urban       * px_area_ha
  )

# Correlations with reforested pixels
cor_reforest <- c(
  forest      = cor(car_dt$n_reforest, car_dt$buf_change_pix_forest),
  agriculture = cor(car_dt$n_reforest, car_dt$buf_change_pix_agriculture),
  urban       = cor(car_dt$n_reforest, car_dt$buf_change_pix_urban)
)

# Correlations with deforested pixels
cor_deforest <- c(
  forest      = cor(car_dt$n_deforest, car_dt$buf_change_pix_forest),
  agriculture = cor(car_dt$n_deforest, car_dt$buf_change_pix_agriculture),
  urban       = cor(car_dt$n_deforest, car_dt$buf_change_pix_urban)
)

cor_reforest
cor_deforest


#### SECTION 4 — Compute distances --------

## Centroids (sf)
car_centroids_sf <- car_sf_filtered %>%
  dplyr::mutate(geometry = sf::st_centroid(geometry))


### 1. Distance to urban centers
dist_urban <- sf::st_distance(car_centroids_sf, urb_centers_sf)
dist_urban_min <- apply(dist_urban, 1, min)

dist_urb_dt <- tibble::tibble(
  car_id          = car_sf_filtered$car_id,
  dist_to_urban_m = as.numeric(dist_urban_min)
)

car_dt <- merge(car_dt, dist_urb_dt, by = "car_id", all.x = TRUE)



### 2. Distance to ROADS
dist_roads <- sf::st_distance(car_centroids_sf, roads_sf)
dist_roads_min <- apply(dist_roads, 1, min)

dist_roads_dt <- tibble::tibble(
  car_id        = car_sf_filtered$car_id,
  dist_to_road_m = as.numeric(dist_roads_min)
)

car_dt <- merge(car_dt, dist_roads_dt, by = "car_id", all.x = TRUE)


### 3. Distance to RIVERS
dist_rivers <- sf::st_distance(car_centroids_sf, rivers_sf)
dist_rivers_min <- apply(dist_rivers, 1, min)

dist_rivers_dt <- tibble::tibble(
  car_id          = car_sf_filtered$car_id,
  dist_to_river_m = as.numeric(dist_rivers_min)
)

car_dt <- merge(car_dt, dist_rivers_dt, by = "car_id", all.x = TRUE)

### 4. Distance to FOREST EDGES (1989 vs 2024)
# Helper function
compute_edge_distance <- function(rast) {
  
  # Forest mask: 1 = forest, NA otherwise
  forest_mask <- rast == 1
  forest_mask[forest_mask != 1] <- NA
  
  # Compute forest boundaries (edge pixels)
  edge_list <- landscapemetrics::get_boundaries(forest_mask, as_NA = TRUE)
  edge_rast <- edge_list[[1]]
  
  # If no boundaries → return NA
  if (all(is.na(terra::values(edge_rast)))) {
    return(rep(NA_real_, nrow(car_centroids_sf)))
  }
  
  # Distance raster: Euclidean distance to nearest edge
  dist_r <- terra::distance(edge_rast)
  
  # Extract distances to property centroids
  d <- terra::extract(dist_r, vect(car_centroids_sf))[,2]
  return(as.numeric(d))
}


### Compute distance for 1989 (first raster)
lu1989 <- rasters_reclass[[1]]
dist_edge_1989 <- compute_edge_distance(lu1989)

### Compute distance for 2024 (last raster)
lu2024 <- rasters_reclass[[length(rasters_reclass)]]
dist_edge_2024 <- compute_edge_distance(lu2024)

### Bind to car_dt
dist_edge_dt <- tibble::tibble(
  car_id = car_sf_filtered$car_id,
  dist_edge_1989_m = dist_edge_1989,
  dist_edge_2024_m = dist_edge_2024
)
car_dt <- merge(car_dt, dist_edge_dt, by = "car_id", all.x = TRUE)


### Compute change category
car_dt <- car_dt %>%
  dplyr::mutate(
    forest_edge_change_m = dist_edge_2024_m - dist_edge_1989_m,
    forest_edge_change = dplyr::case_when(
      forest_edge_change_m < 0  ~ "closer",
      forest_edge_change_m > 0  ~ "further",
      TRUE                      ~ "same"
    )
  )

# Quick correlations
cor(car_dt$n_reforest, car_dt$dist_to_urban_m)
cor(car_dt$n_reforest, car_dt$dist_to_road_m)
cor(car_dt$n_reforest, car_dt$dist_to_river_m)
cor(car_dt$n_reforest, car_dt$dist_edge_1989_m)
cor(car_dt$n_reforest, car_dt$forest_edge_change_m)
car_dt %>% dplyr::group_by(forest_edge_change) %>% dplyr::summarise(mean=mean(n_reforest))

cor(car_dt$n_deforest, car_dt$dist_to_urban_m)
cor(car_dt$n_deforest, car_dt$dist_to_road_m)
cor(car_dt$n_deforest, car_dt$dist_to_river_m)
cor(car_dt$n_deforest, car_dt$dist_edge_1989_m)
cor(car_dt$n_deforest, car_dt$forest_edge_change_m)
car_dt %>% dplyr::group_by(forest_edge_change) %>% dplyr::summarise(mean=mean(n_deforest))


#### SECTION 5 — Intersects properties with spatial vectors --------
# helper that returns a named data.table
compute_intersection <- function(car_sf, target_sf, colname) {
  
  inter <- sf::st_intersects(car_sf, target_sf, sparse = TRUE)
  
  dt <- tibble::tibble(
    car_id = car_sf$car_id,
    flag   = as.integer(lengths(inter) > 0)
  )
  
  setnames(dt, "flag", colname)
  
  return(dt)
}

# 1. Rivers
int_rivers <- compute_intersection(
  car_sf_filtered,
  rivers_sf,
  "intersects_river"
)

# 2. APA
int_apa <- compute_intersection(
  car_sf_filtered,
  apa_mld_sf,
  "intersects_apa"
)

# 3. Public reserves (PDA + Uniao merged)
int_public <- compute_intersection(
  car_sf_filtered,
  public_reserves_sf,
  "intersects_public_reserve"
)

# Merge
car_dt <- car_dt %>%
  dplyr::left_join(int_rivers, by = "car_id") %>%
  dplyr::left_join(int_apa, by = "car_id") %>%
  dplyr::left_join(int_public, by = "car_id")

# Quick correlations
cor(car_dt$n_deforest, car_dt$intersects_river)
cor(car_dt$n_deforest, car_dt$intersects_apa)
cor(car_dt$n_deforest, car_dt$intersects_public_reserve)

cor(car_dt$n_reforest, car_dt$intersects_river)
cor(car_dt$n_reforest, car_dt$intersects_apa)
cor(car_dt$n_reforest, car_dt$intersects_public_reserve)



#### SECTION 6 — Reserva legal coverage --------
# Get intersection between CAR and RL
rl_inter <- sf::st_intersection(
  car_sf_filtered %>% dplyr::select(car_id, geometry),
  rl_sf %>% dplyr::select(geometry)
)

# Compute area of the intersected region (in m2)
rl_inter$rl_area_m2 <- as.numeric(sf::st_area(rl_inter$geometry))
rl_inter$rl_area_ha <- round(rl_inter$rl_area_m2 / 10000, 2)

# Sum per property
# Intersection can produce multiple polygons → we aggregate
rl_summary <- rl_inter %>%
  dplyr::group_by(car_id) %>%
  dplyr::summarise(
    rl_area_m2 = sum(rl_area_m2),
    rl_area_ha = sum(rl_area_ha)
  ) %>%
  dplyr::ungroup()

# Drop the geometry
rl_summary_clean <- rl_summary %>%
  sf::st_drop_geometry() %>% # drop geometry here
  dplyr::select(car_id, rl_area_m2, rl_area_ha)

# Join
car_dt <- car_dt %>%
  dplyr::left_join(rl_summary_clean, by = "car_id") %>%
  dplyr::mutate(
    rl_area_m2 = dplyr::coalesce(rl_area_m2, 0),
    rl_area_ha = dplyr::coalesce(rl_area_ha, 0),
    rl_prop = round(rl_area_ha / car_area_ha, 2)
  )

# Quick correlation
cor(car_dt$n_deforest, car_dt$rl_area_ha)
cor(car_dt$n_reforest, car_dt$rl_area_ha)


#### SECTION 7 — Mean slope --------
# Compute mean slope per property
mean_slope <- exact_extract(slope_r, car_sf_filtered, 'mean')

# Add the results to car_sf
car_sf_filtered$mean_slope <- mean_slope

# Join 
car_dt = car_sf_filtered %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(car_id, mean_slope) %>% 
  dplyr::right_join(car_dt)

# Quick correlations
cor(car_dt$n_deforest, car_dt$mean_slope, use = "complete.obs")
cor(car_dt$n_reforest, car_dt$mean_slope, use = "complete.obs")

#### Export datasets ----------
saveRDS(car_dt,
        here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.rds"))