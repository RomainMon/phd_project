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
library(sp)
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
plot(rasters_reclass[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))


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
plot(rasters_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))


## Forest age
base_path = here("outputs", "data", "MapBiomas", "Rasters_forest_age")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_age = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_age = as.numeric(years_age)) %>%
  dplyr::arrange(years_age)
# Load rasters in chronological order
rasters_forest_age = lapply(raster_df$file, terra::rast)
years_age = raster_df$years_age
# Check
for (i in seq_along(rasters_forest_age)) {
  cat("Year", years_age[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_forest_age[[36]])

## Forest status
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass_forest_cat")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_for_cat = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_for_cat = as.numeric(years_for_cat)) %>%
  dplyr::arrange(years_for_cat)
# Load rasters in chronological order
rasters_forest_status = lapply(raster_df$file, terra::rast)
years_for_cat = raster_df$years_for_cat
# Check
for (i in seq_along(rasters_forest_status)) {
  cat("Year", years_for_cat[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_forest_status[[36]])

## Slope
slope_r = terra::rast(here("data", "geo", "TOPODATA", "work", "slope_bbox.tif"))
plot(slope_r)

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

# Mutate area and id
car_sf = car_sf %>% dplyr::mutate(car_id = row_number(),
                           car_area_m2 = round(as.numeric(st_area(geometry)),2),
                           car_area_ha = round((car_area_m2 / 10000), 2))

## CAR (Reserva Legal)
rl = terra::vect(here("data", "geo", "IBGE", "cadastro_car", "RESERVA_LEGAL", "RESERVA_LEGAL_bbox.shp"))
rl = terra::project(rl, "EPSG:31983")
plot(rl)
rl_sf = sf::st_as_sf(rl)

## RPPN
rppn = terra::vect(here("data", "geo", "AMLD", "RPPN", "RPPN_RJ.shp"))
rppn = terra::project(rppn, "EPSG:31983")
plot(rppn)

## Roads
roads = terra::vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))
roads = terra::project(roads, "EPSG:31983")
plot(roads)
roads_sf = sf::st_as_sf(roads)
br101 = roads_sf %>% 
  dplyr::filter(ref == "BR-101")
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(st_geometry(br101), col="#ff3399", add=TRUE)

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

### Prepare the cell-based dataset -----
# Rationale:
# The workflow starts from rasters_tm[[35]], a raster identifying all cells that experienced at least one land-use/land-cover (LULC) change event during the study period. These “changed cells” form the core sample on which all covariates will be calculated.
# For each event row, the pipeline computes a set of covariates that describe biophysical, legal, and landscape-context characteristics of the cell at the time of its change. All covariates are extracted or computed dynamically with respect to the event year.
# Dataset A: reforested cells + a random sample of intact cells (agricultural cells in rasters_tm). Sample must match the number of reforested cells per year.
# Dataset B: deforested cells + a random sample of intact cells (intact forests in rasters_tm). Sample must match the number of deforested cells per year.

#### Parameters -----
template_rast = rasters_reclass[[36]]

#### 1. Build table of all change events --------
# We create a table with: the cell id, the coordinates, the type of change (deforestation, reforestation), the previous land use, the following land use, the year of change
cat("\nSECTION 1: Build event table\n")

## Example
# Select two rasters
prev = values(rasters_reclass[[1]])
curr = values(rasters_reclass[[2]])

# Work on non-NA cells
ok = !is.na(prev) & !is.na(curr)
head(ok)

# Detect deforested cells (forest to other land use)
idx_def = which(ok & prev == 1 & curr %in% 2:6)
head(idx_def)
coords_def = raster::xyFromCell(rasters_reclass[[2]], idx_def)
head(coords_def)
# Create a data.frame
defor_1989_1990 = data.frame(cell_id = idx_def,
                             year = years_lulc[2],
                             from = 1,
                             to = curr[idx_def],
                             type = 8L,
                             x = coords_def[,1],
                             y = coords_def[,2])

# Check
# Center coordinates
x_center = 778151.2
y_center = 7508862.8

# Zoom buffer (in same units as raster CRS)
buffer_size = 2000
zoom_ext = ext(
  x_center - buffer_size, x_center + buffer_size,
  y_center - buffer_size, y_center + buffer_size
)

# Crop the raster to the zoom window
cumul_crop = crop(rasters_tm[[1]], zoom_ext)

# Plot the cropped raster
plot(cumul_crop, main = "Cumulative Raster 1990 (Zoomed)", col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#d4271e", "chartreuse", "pink"))

# Add deforestation points within the same window
# Filter points inside the zoom window
points_zoom = subset(defor_1989_1990,
                     x >= xmin(zoom_ext) & x <= xmax(zoom_ext) & y >= ymin(zoom_ext) & y <= ymax(zoom_ext))

points(points_zoom$x, points_zoom$y, col = "red", pch = 20, cex = 0.5)


## This function detects each forest change event between pairs of rasters and store them in a table
# Therefore: if a cell changed twice, it could appear twice
detect_forest_transitions = function(rasters, years) {

  stopifnot(length(rasters) == length(years))

  events = list()

  # Loop over consecutive years (t-1 → t)
  for (i in 2:length(rasters)) {

    message("Detecting transitions: ", years[i - 1], " → ", years[i], " (raster ", i - 1, " → ", i, ")")

    prev = values(rasters[[i - 1]])
    curr = values(rasters[[i]])

    # Work on non-NA cells
    ok = !is.na(prev) & !is.na(curr)

    # Detect deforested cells (forest to other land use)
    idx_def = which(ok & prev == 1 & curr %in% 2:6)
    if (length(idx_def) > 0) {
      coords_def = raster::xyFromCell(rasters[[i]], idx_def)
      # Append the new data.frame as the next element in the events list
      events[[length(events) + 1]] =
        data.frame(cell_id = idx_def,
                   year = years[i],
                   from = 1,
                   to = curr[idx_def],
                   type = 8L,
                   x = coords_def[,1],
                   y = coords_def[,2])
      }

    # Detect reforested cells (other land use to forest)
    idx_ref = which(ok & prev %in% 2:6 & curr == 1)
    if (length(idx_ref) > 0) {
      coords_ref = raster::xyFromCell(rasters[[i]], idx_ref)
      # Append the new data.frame as the next element in the events list
      events[[length(events) + 1]] =
        data.frame(cell_id = idx_ref,
                   year = years[i],
                   from = prev[idx_ref],
                   to = 1,
                   type = 7L,
                   x = coords_ref[,1],
                   y = coords_ref[,2])
    }
  }

  changes = dplyr::bind_rows(events)
  changes
}

# Apply
changes = detect_forest_transitions(rasters = rasters_reclass, years = years_lulc)

# Mutate change order
changes = changes %>%
  dplyr::arrange(cell_id, year) %>%
  dplyr::group_by(cell_id) %>%
  dplyr::mutate(change_order = dplyr::row_number()) %>%
  dplyr::ungroup()

# Inspect results
head(changes)
changes[["year"]] == 1990 # To check that changes between baseline (1989) and the right-up foloowing year were taken into account (TRUE means changes occurred between those years)

# Results
cat("Changed cells:", length(changes$cell_id), "\n")
cat("Unique changed cells:", length(unique(changes$cell_id)), "\n")
f = terra::freq(rasters_tm[[35]])
cat("... Now check that it equals the total number of cells that have underwent reforestation or deforestation in cumulative rasters, being:", sum(f$count[f$value %in% c(7,8)]))

# Some statistics
# Types of reforestation
reforest_summary = changes %>% 
  dplyr::filter(type == 7) %>% 
  dplyr::group_by(from, to) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prop = n * 100 / sum(n))

cat("Reforestation events:\n")
for(i in 1:nrow(reforest_summary)) {
  cat(sprintf("- %d cells changed from %d to %d (%.1f%% of all reforestation events)\n",
              reforest_summary$n[i],
              reforest_summary$from[i],
              reforest_summary$to[i],
              reforest_summary$prop[i]))
}

# Types of deforestation
deforest_summary = changes %>% 
  dplyr::filter(type == 8) %>% 
  dplyr::group_by(from, to) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prop = n * 100 / sum(n))

cat("Deforestation events:\n")
for(i in 1:nrow(deforest_summary)) {
  cat(sprintf("- %d cells changed from %d to %d (%.1f%% of all deforestation events)\n",
              deforest_summary$n[i],
              deforest_summary$from[i],
              deforest_summary$to[i],
              deforest_summary$prop[i]))
}

# Maximum number of LULCC per pixel
changes %>%
  dplyr::count(cell_id, name = "n_changes") %>%
  dplyr::count(n_changes, name = "n_cells") %>%
  dplyr::arrange(n_changes) %>% 
  dplyr::mutate(cum_n = cumsum(n_cells))

#### 2. Select controls ---------
# We select controls, i.e., cells that were never deforested nor reforested during the study period
# Using the last raster ensures that: controls are still intact at the end, controls never experienced LULCC, we avoid “false controls” that are intact early but changed later
# Control cells were selected among pixels that remained intact throughout the entire study period, and were randomly sampled to match the number of events per year

cat("\nSECTION 2: Selecting intact control cells\n")

# Step 1: Select relevant reforestation/deforestation events
# We remove cells that have changes more than 3 times
changes_subset = changes %>% 
  dplyr::group_by(cell_id) %>% 
  dplyr::filter(dplyr::n() <= 3) %>% 
  dplyr::ungroup()
changes_subset %>% 
  dplyr::summarise(n=dplyr::n_distinct(cell_id))
# We subset reforested events to agricultural pixels becoming forests
# We subset deforested events to forests becoming agricultural pixels
changes_subset = changes_subset %>% 
  dplyr::filter((from == 4 & to == 1) | (from == 1 & to == 4))

# Step 2: Count changed cells of each type per year
events_per_year = changes_subset %>%
  dplyr::count(year, type, name = "n_events")
head(events_per_year)

# Step 3: Select intact controls for reforestation
# Select the last cumulative raster (where intact cells are displayed)
last_rast = rasters_tm[[length(rasters_tm)]]
# Binary mask: intact agriculture only
agri_mask = last_rast == 4
plot(agri_mask)
# Cell indices of intact agriculture
agri_cells = which(terra::values(agri_mask) == 1)

# Are there more pixels of agriculture than of reforested cells?
length(agri_cells) > length(changes$type[[7]])

# Number of cells to be selected each year
reforest_events = events_per_year %>%
  dplyr::filter(type == 7)

# Random selection of cells according to the number of changes
controls_agri = vector("list", length = nrow(reforest_events))
names(controls_agri) = reforest_events$year

for (i in seq_len(nrow(reforest_events))) {
  yr = reforest_events$year[i]
  n  = reforest_events$n_events[i]
  
  # Sample
  cells = sample(agri_cells, size = n, replace = FALSE)
  
  # Convert to coordinates
  coords = terra::xyFromCell(last_rast, cells)
  
  controls_agri[[i]] = data.frame(
    year = yr,
    cell_id = cells,
    type = 4, # Indicate value
    x = coords[, 1],
    y = coords[, 2]
  )
}

# Combine into one data frame
controls_agri_df = dplyr::bind_rows(controls_agri)

# Quick check
controls_2024 = controls_agri_df %>%
  dplyr::filter(year == 2024)
plot(rasters_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))
points(controls_2024$x, controls_2024$y, pch = 20, col = "magenta", cex = 0.4)
terra::extract(rasters_tm[[35]], controls_2024[, c("x", "y")]) # Check the value

# Step 4: Select intact controls for deforestation
# Select the last cumulative raster (where intact cells are displayed)
last_rast = rasters_tm[[length(rasters_tm)]]
# Binary mask: intact forest only
forest_mask = last_rast == 1
plot(forest_mask)
# Cell indices of intact agriculture
forest_cells = which(terra::values(forest_mask) == 1)

# Are there more pixels of forest than of deforested cells?
length(forest_cells) > length(changes$type[[8]])

# Number of cells to be selected each year
deforest_events = events_per_year %>%
  dplyr::filter(type == 8)

# Random selection of cells according to the number of changes
controls_forest = vector("list", length = nrow(deforest_events))
names(controls_forest) = deforest_events$year

for (i in seq_len(nrow(deforest_events))) {
  yr = deforest_events$year[i]
  n  = deforest_events$n_events[i]
  
  # Sample
  cells = sample(forest_cells, size = n, replace = FALSE)
  
  # Convert to coordinates
  coords = terra::xyFromCell(last_rast, cells)
  
  controls_forest[[i]] = data.frame(
    year = yr,
    cell_id = cells,
    type = 1,
    x = coords[, 1],
    y = coords[, 2]
  )
}

# Combine into one data frame
controls_forest_df = dplyr::bind_rows(controls_forest)

# Quick check
controls_2024 = controls_forest_df %>%
  dplyr::filter(year == 2024)
controls_2023 = controls_forest_df %>%
  dplyr::filter(year == 2023)
plot(rasters_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))
points(controls_2024$x, controls_2024$y, pch = 20, col = "magenta", cex = 0.4)
points(controls_2023$x, controls_2023$y, pch = 20, col = "darkslategray1", cex = 0.4) # Check that controls differ each year
terra::extract(rasters_tm[[35]], controls_2024[, c("x", "y")]) # Check the value

# Summary
# Number of events per type of change
events_per_year %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(n_cells = sum(n_events))
# Check consistency
cat("Controls (deforestation):", nrow(controls_forest_df), "\n")
cat("Controls (reforestation):", nrow(controls_agri_df), "\n")



#### 3. Build final datasets for modelling -------

# Dataset A: Reforest events + control cells (intact agriculture)
data_refor = dplyr::bind_rows(
  changes_subset %>% # Take the filtered dataset!
    dplyr::filter(type == 7) %>%
    dplyr::select(year, cell_id, type, x, y),
  controls_agri_df)
head(data_refor)

# Dataset B: Deforest events + control cells (intact forest)
data_defor = dplyr::bind_rows(
  changes_subset %>% # Take the filtered dataset!
    dplyr::filter(type == 8) %>%
    dplyr::select(year, cell_id, type, x, y),
  controls_forest_df)
head(data_defor)

# Quick check
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n())
data_defor %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n())

# Summary
cat("Dataset A rows:", nrow(data_refor), "\n")
cat("Dataset B rows:", nrow(data_defor), "\n")



#### 4. Legal status --------
# This block overlays the pixel with the legal status
# Each layer is rasterized onto the reference grid and intersected with event coordinates.

cat("\nSECTION 4: Legal status\n")

## Rasterize data
# Private properties
car_r = terra::rasterize(car, template_rast, field = 1, background = 0)
plot(car_r)
# Public reserve
pub_res_r = terra::rasterize(pub_res, template_rast, field = 1, background = 0)
plot(pub_res_r)
# RPPN
rppn_r = terra::rasterize(rppn, template_rast, field = 1, background = 0)
plot(rppn_r)
# Reserva legal
rl_r = terra::rasterize(rl, template_rast, field = 1, background = 0)
plot(rl_r)

## Extract for deforestation dataset
# NB: ID = FALSE returns only the extracted values
# Using [,1] extracts the values as a simple vector instead of a one-column data frame
data_defor = data_defor %>% 
  dplyr::mutate(in_car = terra::extract(car_r, data_defor[, c("x", "y")], ID = FALSE)[,1],
                in_pub_res = terra::extract(pub_res_r, data_defor[, c("x", "y")], ID = FALSE)[,1],
                in_rppn = terra::extract(rppn_r, data_defor[, c("x", "y")], ID = FALSE)[,1],
                in_rl = terra::extract(rl_r, data_defor[, c("x", "y")], ID = FALSE)[,1])

# Combinations
data_defor %>%
  dplyr::select(in_car, in_pub_res, in_rppn, in_rl) %>%
  dplyr::distinct() %>%
  dplyr::arrange(in_car, in_pub_res, in_rppn, in_rl)

# Reclass
data_defor = data_defor %>%
  dplyr::mutate(legal_status = dplyr::case_when(in_car == 0 & in_pub_res == 0 & in_rppn == 1 & in_rl == 0 ~ "rppn",
                                                in_car == 0 & in_pub_res == 1 & in_rppn == 0 & in_rl == 0 ~ "public_reserve",
                                                in_car == 0 & in_pub_res == 1 & in_rppn == 1 & in_rl == 0 ~ "private_within_reserve",
                                                in_car == 1 & in_pub_res == 0 & in_rppn == 0 & in_rl == 0 ~ "private",
                                                in_car == 1 & in_pub_res == 0 & in_rppn == 0 & in_rl == 1 ~ "rl",
                                                in_car == 1 & in_pub_res == 0 & in_rppn == 1 & in_rl == 0 ~ "rppn",
                                                in_car == 1 & in_pub_res == 0 & in_rppn == 1 & in_rl == 1 ~ "rppn",
                                                in_car == 1 & in_pub_res == 1 & in_rppn == 0 & in_rl == 0 ~ "private_within_reserve",
                                                in_car == 1 & in_pub_res == 1 & in_rppn == 0 & in_rl == 1 ~ "private_within_reserve",
                                                in_car == 1 & in_pub_res == 1 & in_rppn == 1 & in_rl == 0 ~ "private_within_reserve",
                                                in_car == 1 & in_pub_res == 1 & in_rppn == 1 & in_rl == 1 ~ "private_within_reserve",
                                                in_car == 0 & in_pub_res == 0 & in_rppn == 0 & in_rl == 0 ~ "unknown",
                                                TRUE ~ "unknown"))
table(data_defor$type, data_defor$legal_status)


## Extract for reforestation dataset
# NB: ID = FALSE returns only the extracted values
# Using [,1] extracts the values as a simple vector instead of a one-column data frame
data_refor = data_refor %>% 
  dplyr::mutate(in_car = terra::extract(car_r, data_refor[, c("x", "y")], ID = FALSE)[,1],
                in_pub_res = terra::extract(pub_res_r, data_refor[, c("x", "y")], ID = FALSE)[,1],
                in_rppn = terra::extract(rppn_r, data_refor[, c("x", "y")], ID = FALSE)[,1],
                in_rl = terra::extract(rl_r, data_refor[, c("x", "y")], ID = FALSE)[,1])

# Combinations
data_refor %>%
  dplyr::select(in_car, in_pub_res, in_rppn, in_rl) %>%
  dplyr::distinct() %>%
  dplyr::arrange(in_car, in_pub_res, in_rppn, in_rl)

# Reclass
data_refor = data_refor %>%
  dplyr::mutate(legal_status = dplyr::case_when(in_car == 0 & in_pub_res == 0 & in_rppn == 1 & in_rl == 0 ~ "rppn",
                                                in_car == 0 & in_pub_res == 1 & in_rppn == 0 & in_rl == 0 ~ "public_reserve",
                                                in_car == 0 & in_pub_res == 1 & in_rppn == 1 & in_rl == 0 ~ "private_within_reserve",
                                                in_car == 0 & in_pub_res == 1 & in_rppn == 0 & in_rl == 1 ~ "private_within_reserve",
                                                in_car == 1 & in_pub_res == 0 & in_rppn == 0 & in_rl == 0 ~ "private",
                                                in_car == 1 & in_pub_res == 0 & in_rppn == 0 & in_rl == 1 ~ "rl",
                                                in_car == 1 & in_pub_res == 0 & in_rppn == 1 & in_rl == 0 ~ "rppn",
                                                in_car == 1 & in_pub_res == 0 & in_rppn == 1 & in_rl == 1 ~ "rppn",
                                                in_car == 1 & in_pub_res == 1 & in_rppn == 0 & in_rl == 0 ~ "private_within_reserve",
                                                in_car == 1 & in_pub_res == 1 & in_rppn == 0 & in_rl == 1 ~ "private_within_reserve",
                                                in_car == 1 & in_pub_res == 1 & in_rppn == 1 & in_rl == 0 ~ "private_within_reserve",
                                                in_car == 1 & in_pub_res == 1 & in_rppn == 1 & in_rl == 1 ~ "private_within_reserve",
                                                in_car == 0 & in_pub_res == 0 & in_rppn == 0 & in_rl == 0 ~ "unknown",
                                                TRUE ~ "unknown"))
table(data_refor$type, data_refor$legal_status)

cat("Legal status computed\n")


#### 5. Intersect with APA--------
# A binary indicator (in_APA) is added for each event.

cat("\nSECTION 5: APA intersection\n")

# Rasterize
apa_mld_r = terra::rasterize(apa_mld, template_rast, field = 1, background = 0)
plot(apa_mld_r)

## Dataset A
data_defor = data_defor %>%
  dplyr::mutate(in_apa = terra::extract(apa_mld_r, data_defor[, c("x", "y")], ID = FALSE)[,1],)
table(data_defor$type, data_defor$in_apa)

## Dataset B
data_refor = data_refor %>%
  dplyr::mutate(in_apa = terra::extract(apa_mld_r, data_refor[, c("x", "y")], ID = FALSE)[,1],)
table(data_refor$type, data_refor$in_apa)

cat("APA intersection done\n")


#### 6. Distances --------
# Distances are computed from the event cell to different spatial features, all projected to match the template raster.
# For roads: For each event year, only roads whose construction year is ≤ the event year are retained.
# For forest edges: For each event year, a forest/non-forest raster is taken from rasters_reclass. Using landscapemetrics, forest boundaries are computed with get_boundaries().

cat("\nSECTION 6: Distances\n")

# Rasterize
# NB: Background is NA here
rivers_bin = terra::rasterize(rivers_sf, template_rast, field = 1, background = NA)
plot(rivers_bin, col="purple")
urban_bin = terra::rasterize(urb_sf, template_rast, field = 1, background = NA)
plot(urban_bin, col="purple")

## Distances to rivers and urban centers
dist_rivers_r = terra::distance(rivers_bin)
plot(dist_rivers_r)
dist_urban_r = terra::distance(urban_bin)
plot(dist_urban_r)


# Dataset A
data_defor = data_defor %>%
  dplyr::mutate(dist_river_m = terra::extract(dist_rivers_r, data_defor[, c("x", "y")], ID = FALSE)[,1],
                dist_urban_m = terra::extract(dist_urban_r, data_defor[, c("x", "y")], ID = FALSE)[,1],)
# Dataset B
data_refor = data_refor %>%
  dplyr::mutate(dist_river_m = terra::extract(dist_rivers_r, data_refor[, c("x", "y")], ID = FALSE)[,1],
                dist_urban_m = terra::extract(dist_urban_r, data_refor[, c("x", "y")], ID = FALSE)[,1],)


## Distance to roads (dynamic)
# To account for creation dates of roads, we subset roads by date_crea ≤ change_year
# We recompute distance year by year
# We extract distances only for events in that year

# Initialize column
data_defor$dist_road_m = NA_real_
data_refor$dist_road_m = NA_real_

# Loop over years (deforestation dataset)
unique_years = sort(unique(data_defor$year))
for (yr in sort(unique(data_defor$year))) {
  
  idx = which(data_defor$year == yr)
  if (length(idx) == 0) next
  
  # Roads existing up to that year
  roads_sub = roads_sf[roads_sf$date_crea <= yr, ]
  
  if (nrow(roads_sub) == 0) next
  
  # Rasterize roads
  roads_bin = terra::rasterize(roads_sub, template_rast, field = 1, background = NA)
  
  # Distance to nearest road
  dist_r = terra::distance(roads_bin)
  
  # Extract distances
  d = terra::extract(dist_r, data_defor[idx, c("x", "y")], ID = FALSE)[,1]
  data_defor$dist_road_m[idx] = d
}

# Loop over years (reforestation dataset)
unique_years = sort(unique(data_refor$year))
for (yr in sort(unique(data_refor$year))) {
  
  idx = which(data_refor$year == yr)
  if (length(idx) == 0) next
  
  # Roads existing up to that year
  roads_sub = roads_sf[roads_sf$date_crea <= yr, ]
  
  if (nrow(roads_sub) == 0) next
  
  # Rasterize roads
  roads_bin = terra::rasterize(roads_sub, template_rast, field = 1, background = NA)
  
  # Distance to nearest road
  dist_r = terra::distance(roads_bin)
  
  # Extract distances
  d = terra::extract(dist_r, data_refor[idx, c("x", "y")], ID = FALSE)[,1]
  data_refor$dist_road_m[idx] = d
}

## Distance to forest edges (dynamic)
# To account for the forest dynamics through time, we select the corresponding LULC map from rasters_reclass
# We build binary forest mask (r == 1)
# We use landscapemetrics::get_boundaries() to get edge pixels
# We compute Euclidean distance with terra::distance()

# Initialize column
data_defor$dist_edge_m = NA_real_
data_refor$dist_edge_m = NA_real_

# Loop over years (deforestation dataset)
unique_years = sort(unique(data_defor$year))
for (yr in unique_years) {
  
  cat("  Processing forest edges for year:", yr, "\n")
  
  # Identify rows for this year
  idx = which(data_defor$year == yr)
  if (length(idx) == 0) next
  
  # Get the forest raster for the corresponding year
  r_year = rasters_reclass[[which(years_lulc == yr)]]
  
  # Build binary forest mask: 1 = forest, NA = other
  forest_bin = r_year
  forest_bin[forest_bin != 1] = NA
  
  # Compute forest edges
  edge_list = landscapemetrics::get_boundaries(forest_mask, as_NA = TRUE)
  edge_rast = edge_list[[1]]
  
  # Compute distance raster
  dist_edge_r = terra::distance(edge_rast)
  
  # Extract distances for rows of this year
  d = terra::extract(dist_edge_r, data_defor[idx, c("x", "y")], ID = FALSE)[,1]
  data_defor$dist_edge_m[idx] = d
}

# Loop over years (reforestation dataset)
unique_years = sort(unique(data_refor$year))
for (yr in unique_years) {
  
  cat("  Processing forest edges for year:", yr, "\n")
  
  # Identify rows for this year
  idx = which(data_refor$year == yr)
  if (length(idx) == 0) next
  
  # Get the forest raster for the corresponding year
  r_year = rasters_reclass[[which(years_lulc == yr)]]
  
  # Build binary forest mask: 1 = forest, NA = other
  forest_bin = r_year
  forest_bin[forest_bin != 1] = NA
  
  # Compute forest edges
  edge_list = landscapemetrics::get_boundaries(forest_mask, as_NA = TRUE)
  edge_rast = edge_list[[1]]
  
  # Compute distance raster
  dist_edge_r = terra::distance(edge_rast)
  
  # Extract distances for rows of this year
  d = terra::extract(dist_edge_r, data_refor[idx, c("x", "y")], ID = FALSE)[,1]
  data_refor$dist_edge_m[idx] = d
}

# Statistics
data_defor %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise_at(vars(starts_with("dist")), mean, na.rm = TRUE)
data_refor %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise_at(vars(starts_with("dist")), mean, na.rm = TRUE)

cat(" Distances computed.\n")


#### 7. Slope --------
# Slope value is extracted for each event location.
cat("SECTION 7: extracting slope values\n")

data_defor = data_defor %>%
  dplyr::mutate(slope_pct = terra::extract(slope_r, data_defor[, c("x", "y")], ID = FALSE)[,1],)
data_refor = data_refor %>%
  dplyr::mutate(slope_pct = terra::extract(slope_r, data_refor[, c("x", "y")], ID = FALSE)[,1],)

data_defor %>% dplyr::group_by(type) %>% dplyr::summarise(mean_slope = mean(slope_pct, na.rm=TRUE))
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise(mean_slope = mean(slope_pct, na.rm=TRUE))

cat("Slope extraction completed\n")


#### 8. Land use area ----------
# We extract land-use areas within a buffer around each point, year by year, for specified land-use classes
# Numerous options: sample_lsm (landscapemetrics), packahe multilandr, exact_extractr... but all these options are slow
# Instead, we can use a moving window approach (faster with numerous points)

cat("\nSECTION 8: Land use area extraction around buffers\n")

## On an example
# Pick one class
cl = 1
# Pick one raster
r = rasters_reclass[[2]]
# Pick one point
pt = data_defor %>%
  dplyr::filter(year == 1990) %>%
  dplyr::slice(1) %>%
  dplyr::select(x, y)
pt_vect = terra::vect(pt, geom = c("x", "y"), crs = crs(r))
# Zoom window
zoom_radius = 250  # meters around point
ext_zoom = ext(
  pt$x - zoom_radius, pt$x + zoom_radius,
  pt$y - zoom_radius, pt$y + zoom_radius
)
# Binary raster
r_bin = r == cl
plot(r_bin,
     main = paste("Binary raster: class", cl),
     col = c("lightgrey", "darkgreen"))
# Moving window
radius = 100 # meters
w = terra::focalMat(r_bin, type = "circle", d = radius)
image(w, main = "Focal window")
focal_count = terra::focal(r_bin, w = w, fun = "sum", na.rm = TRUE)
plot(focal_count, main = "Forest pixel count")
# Valid landscape (for proportion)
r_valid = !is.na(r)
focal_valid = terra::focal(r_valid, w = w, fun = "sum", na.rm = TRUE)
plot(focal_valid, main = "Valid pixel count")
# Convert pixel count to area
pixel_area = prod(res(r)) # m²
focal_area_m2 = focal_count * pixel_area
valid_area_m2 = focal_valid * pixel_area
plot(focal_area_m2, main = "Forest area (m²)")
plot(valid_area_m2, main = "Valid area (m²)")
# Extract value
area_at_point = terra::extract(focal_area_m2, pt_vect)[, 2]
valid_at_point = terra::extract(valid_area_m2, pt_vect)[, 2]
area_at_point
area_at_point/valid_at_point
# Plot everything zoomed around point
par(mfrow = c(2, 2))
plot(r_bin, main = paste("Binary raster: class", cl), col = c("lightgrey", "darkgreen"), ext = ext_zoom)
points(pt$x, pt$y, pch = 20, col = "red")

plot(focal_count, main = "Forest pixel count", ext = ext_zoom)
points(pt$x, pt$y, pch = 20, col = "red")

plot(focal_area_m2, main = "Forest area (m²)", ext = ext_zoom)
points(pt$x, pt$y, pch = 20, col = "red")

plot(valid_area_m2, main = "Valid area (m²)", ext = ext_zoom)
points(pt$x, pt$y, pch = 20, col = "red")
par(mfrow = c(1, 1))


## Function
names(rasters_reclass) = years_lulc
compute_landuse_buffer = function(dt, rasters, classes, radii) {
  
  cat("\n--- Starting land use area extraction ---\n")
  
  # Pixel area
  pixel_area = prod(terra::res(rasters[[1]]))
  cat("Pixel area:", pixel_area, "m²\n")
  
  # list of unique years present in this dataset
  years = sort(unique(dt$year))
  
  for (radius in radii) {
    
    cat("\n### Buffer radius:", radius, "m ###\n")
  
    # circular weight matrix
    w = terra::focalMat(rasters[[1]], type = "circle", d = radius, fillNA = TRUE)
    
    # loop over years
    for (yr in years) {
      cat("\nProcessing YEAR:", yr, "\n")
      
      # select raster for this year
      r_year = rasters[[as.character(yr)]]
      
      # extract the coordinates for this year
      pts_year = dt %>%
        dplyr::filter(year == yr) %>%
        dplyr::select(x, y)
      pts_vect = terra::vect(pts_year, geom = c("x", "y"), crs = crs(r_year))
      
      # Extract what cells are "valid" (NOT NA)
      r_valid = !is.na(r_year)
      # Here, we compute the "valid area" to compute the proportion of LULC classes within each moving window
      # This way, we account for cells at the border of the landscape
      focal_valid = terra::focal(r_valid, w = w, fun = "sum", na.rm = TRUE)
      valid_area_m2 = focal_valid * pixel_area
      
      # loop over classes
      for (cl in classes) {
        cat("   → Class", cl, "...\n")
        # binary raster for that class
        r_bin = r_year == cl
        # moving window sum
        focal_count = terra::focal(r_bin, w = w, fun = "sum", na.rm = TRUE) # NA values are ignored in the sum
        # convert pixel count to area
        focal_area_m2 = focal_count * pixel_area
        # extract values for the points
        area_values = terra::extract(focal_area_m2, pts_vect)[, 2]
        valid_vals = terra::extract(valid_area_m2, pts_vect)[, 2]
        # assign values back into the dataframe
        col_area_name = paste0("area_m2_class_", cl)
        dt[[col_area_name]][dt$year == yr] = area_values
        col_prop_name = paste0("prop_class_", cl)
        dt[[col_prop_name]][dt$year == yr] = area_values / valid_vals
      }
    }
  }
  
  cat("\n--- Extraction finished successfully ---\n")
  return(dt)
}


data_defor = compute_landuse_buffer(dt = data_defor, rasters = rasters_reclass, classes = c(1,4,6), radii = c(100,250,500))
data_refor = compute_landuse_buffer(dt = data_refor, rasters = rasters_reclass, classes = c(1,4,6), radii = c(100,250,500))

# Summary statistics
data_defor %>% dplyr::group_by(type) %>% dplyr::summarise_at(vars(starts_with("area")), mean, na.rm = TRUE)
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise_at(vars(starts_with("area")), mean, na.rm = TRUE)

cat("Land use area computation complete.\n")

#### 9. Forest age ----------
# We extract the forest age for deforested/intact cells only
# To do so, we extract the age one year before the change
# In the end, we add +1 to the final value (i.e., forest age when deforested/remained intact)
cat("Forest age extraction for the deforestation dataset.\n")

# Loop over years (deforestation dataset)
data_defor$forest_age = NA_real_

names(rasters_forest_age) = years_age

for (yr in sort(unique(data_defor$year))) {
  
  idx = which(data_defor$year == yr)
  if (!as.character(yr - 1) %in% names(rasters_forest_age)) next
  
  r_prev = rasters_forest_age[[as.character(yr - 1)]] # Select previous raster
  
  vals = terra::extract(
    r_prev,
    data_defor[idx, c("x", "y")],
    ID = FALSE
  )[,1]
  
  data_defor$forest_age[idx] = vals + 1 # Add +1
}

# Statistics
data_defor %>% 
  dplyr::group_by(type) %>% dplyr::summarise(mean_age = mean(forest_age))

cat("Forest age extraction completed.\n")


#### 10. Climate ----------
# Climatic values are extracted for each event location.
cat("Extracting climate variables (precipitations and temperatures).\n")

# Function
# Inputs = dataframe, climate rasters, name of the output variable (var_name) (e.g., prec_mean)
# We extract the value at point location
# Some points may not have a value (rasters do not overlay all locations); therefore, the nearest value is taken
names(rasters_prec_sum) = years_climate
names(rasters_tmax_mean) = years_climate
names(rasters_tmin_mean) = years_climate

compute_climate = function(dt, rasters, var_name) {
  
  cat("\n--- Extracting", var_name, "---\n")
  
  dt[[var_name]] = NA_real_
  
  for (yr in sort(unique(dt$year))) {
    
    if (!as.character(yr) %in% names(rasters)) next
    
    idx = which(dt$year == yr)
    if (length(idx) == 0) next
    
    r = rasters[[as.character(yr)]]
    
    pts = terra::vect(
      dt[idx, c("x", "y")],
      geom = c("x", "y"),
      crs = crs(r)
    )
    
    vals = terra::extract(r, pts, ID = FALSE)[,1]
    
    # identify NA
    na_idx = which(is.na(vals))
    
    if (length(na_idx) > 0) {
      nearest = terra::extract(
        r,
        pts[na_idx],
        method = "simple"
      )[,1]
      vals[na_idx] = nearest
    }
    
    dt[[var_name]][idx] = vals
  }
  
  dt
}

# Extract for deforestation 
data_defor = compute_climate(data_defor, rasters_prec_sum, "prec_sum")
data_defor = compute_climate(data_defor, rasters_tmin_mean, "tmin_mean")
data_defor = compute_climate(data_defor, rasters_tmax_mean, "tmax_mean")

# Extract for reforestation
data_refor = compute_climate(data_refor, rasters_prec_sum, "prec_sum")
data_refor = compute_climate(data_refor, rasters_tmin_mean, "tmin_mean")
data_refor = compute_climate(data_refor, rasters_tmax_mean, "tmax_mean")

# Presence of NAs
sum(is.na(data_refor$prec_sum))
sum(is.na(data_refor$tmin_mean))
sum(is.na(data_refor$tmax_mean))

# Summary
data_defor %>% dplyr::group_by(type) %>% dplyr::summarise(mean_prec = mean(prec_sum),
                                                       mean_tmin = mean(tmin_mean),
                                                       mean_tmax = mean(tmax_mean))
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise(mean_prec = mean(prec_sum),
                                                          mean_tmin = mean(tmin_mean),
                                                          mean_tmax = mean(tmax_mean))

cat("Climatic variables extraction completed.\n")

#### 11. North and South of BR-101 ----------
cat("Extracting whether pixels are located North or South of the highway.\n")

# Sample regular points along BR-101
br101_sp = as_Spatial(br101)
pts_br101 = spsample(br101_sp, n = 1000, type = "regular")
pts_br101_sf = sf::st_as_sf(pts_br101)

# Add x and y coordinates as columns
pts_br101_sf$x = sf::st_coordinates(pts_br101_sf)[,1]
pts_br101_sf$y = sf::st_coordinates(pts_br101_sf)[,2]

# Compute North/South of BR-101 using X and Y coordinates

# Function to compute North/South
compute_ns = function(x0, y0, br_x, br_y) {
  # Find index of closest longitude
  idx = which.min(abs(br_x - x0))
  if (length(idx) == 0) return(NA_character_)
  if (y0 > br_y[idx]) {
    return("North")
  } else {
    return("South")
  }
}

# Apply function to all points
# Deforestation dataset
data_defor = data_defor %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ns_br101 = compute_ns(x, y, pts_br101_sf$x, pts_br101_sf$y)
  ) %>%
  dplyr::ungroup()

# Reforestation dataset
data_refor = data_refor %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ns_br101 = compute_ns(x, y, pts_br101_sf$x, pts_br101_sf$y)
  ) %>%
  dplyr::ungroup()

# Check
pts_plot = data_refor %>% dplyr::sample_n(300) %>% sf::st_as_sf(coords = c("x", "y"), crs = st_crs(br101))
# Plot
ggplot() +
  geom_sf(data = br101, color = "red", size = 1) +   # BR-101 line
  geom_sf(data = pts_plot, aes(color = ns_br101), size = 2) +  # Points colored by North/South
  scale_color_manual(values = c("North" = "blue", "South" = "green")) +
  theme_minimal() +
  labs(color = "Position relative to BR-101",
       title = "North/South of points relative to BR-101")


# Summary
table(data_defor$type, data_defor$ns_br101)
table(data_refor$type, data_refor$ns_br101)

cat("Relative location of points regarding the BR-101 highway determined\n")

#### Export datasets ----------
saveRDS(data_defor,
        here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_pixel.rds"))

saveRDS(data_refor,
        here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_refor_pixel.rds"))


#### Illustration ----------
### Select one point from each dataset
set.seed(123)

pt_defor = data_defor %>% dplyr::slice_sample(n = 1)

cat("Selected defor point:", pt_defor$cell_id, "year:", pt_defor$year, "\n")

### Function to make map for 1 point
make_point_plot = function(pt, rasters_reclass, years_lulc,
                            roads_sf, rivers_sf, car_sf, rl_sf, pub_res_sf, apa_mld_sf,
                            radius) {
  
  year = pt$year
  r_year = rasters_reclass[[which(years_lulc == year)]]
  
  # convert point → sf
  pt_sf = sf::st_as_sf(pt, coords = c("x","y"), crs = crs(r_year))
  
  # 1 km buffer
  buf = sf::st_buffer(pt_sf, dist = radius)
  
  # crop raster to 1 km window
  r_crop = terra::crop(r_year, vect(st_buffer(pt_sf, 1000)))
  
  # convert raster to dataframe for ggplot
  r_df = as.data.frame(r_crop, xy = TRUE)
  colnames(r_df)[3] <- "lulc"
  
  # crop vectors
  roads_c = st_intersection(roads_sf, st_buffer(pt_sf, 1000))
  rivers_c = st_intersection(rivers_sf, st_buffer(pt_sf, 1000))
  car_c = st_intersection(car_sf, st_buffer(pt_sf, 1000))
  rl_c = st_intersection(rl_sf, st_buffer(pt_sf, 1000))
  pub_c = st_intersection(pub_res_sf, st_buffer(pt_sf, 1000))
  apa_c = st_intersection(apa_mld_sf, st_buffer(pt_sf, 1000))
  
  # LULCC colors
  lut = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e")
  names(lut) = as.character(sort(unique(r_df$lulc)))
  
  # Make ggplot
  p = ggplot() +
    geom_raster(data = r_df, aes(x, y, fill = factor(lulc))) +
    scale_fill_manual(values = lut, name = "LULC") +
    geom_sf(data = roads_c,  color = "black", size = 0.4) +
    geom_sf(data = rivers_c, color = "blue", size = 0.5) +
    geom_sf(data = car_c, fill = NA, color = "brown", linetype = 3) +
    geom_sf(data = rl_c, fill = NA, color = "green3", linetype = 2) +
    geom_sf(data = pub_c, fill = NA, color = "red", size = 0.7) +
    geom_sf(data = apa_c, fill = NA, color = "orange", linetype = 2) +
    geom_sf(data = buf, fill = NA, color = "yellow", linetype = 2, size = 1) +
    geom_sf(data = pt_sf, shape = 21, fill = "red", size = 3) +
    coord_sf() +
    theme_minimal()
  
  ### Covariate text box
  txt = paste0(
    "type = ", pt$type, "\n",
    "legal = ", pt$legal_status, "\n",
    "dist_water_m = ", round(pt$dist_river_m), "\n",
    "dist_urban_m = ", round(pt$dist_urban_m), "\n",
    "dist_road_m = ", round(pt$dist_road_m), "\n",
    "dist_edge_m = ", round(pt$dist_edge_m), "\n",
    "slope_pct = ", round(pt$slope_pct, 1), "\n",
    "forest_age = ", round(pt$forest_age), "\n",
    "prec_sum = ", round(pt$prec_sum), "\n",
    "tmin_mean = ", round(pt$tmin_mean), "\n",
    "tmax_mean = ", round(pt$tmax_mean), "\n",
    "ns_br101 = ", pt$ns_br101
  )
  
  p_label = ggdraw() + draw_label(txt, x = 0, y = 1, hjust = 0, vjust = 1, size = 11)
  
  return(plot_grid(p, p_label, ncol = 2, rel_widths = c(1.3, 0.9)))
}


### Generate both plots
final_plot = make_point_plot(pt_defor, rasters_reclass, years_lulc,
                           roads_sf, rivers_sf, car_sf, rl_sf, pub_res_sf, apa_mld_sf,
                radius=500)

# Export PNG (high resolution)
ggsave(
  filename = here("outputs", "plot", "01j_defor_dataset_demo.png"),
  plot = final_plot,
  width = 8,
  height = 5,
  dpi = 300,
  bg="white"
)



## Prepare the property-based dataset ----------
# Here, the spatial unit is the private property (features in CAR)
# We measure covariates at the property scale

#### 1. Prepare the property-dataset ------

## Filter initial dataset
# Base dataset from car_sf
car_sf_filtered = car_sf %>% 
  dplyr::select(c(car_id, cod_imovel, car_area_m2, car_area_ha))

## Remove too small properties
# Pixel area
r = rasters_tm[[35]]
px_area_m2 = prod(terra::res(r))
px_area_ha = px_area_m2 / 10000
px_area_ha

# Remove properties < 1 pixel
car_sf_filtered = car_sf_filtered %>% 
  dplyr::filter(car_area_m2 > px_area_m2)

## Number of intersecting properties
car_sf_filtered %>% 
  dplyr::mutate(
    n_overlaps = lengths(st_intersects(.))) %>%
  sf::st_drop_geometry() %>% 
  dplyr::filter(n_overlaps > 1) %>% # >1 because each feature intersects itself
  dplyr::summarise(n = n())


#### 2. Compute reforestation and deforestation areas ------

## Deforestation map
# Use the last transition map
tm_defor = rasters_tm[[length(rasters_tm)]]
tm_defor[tm_defor != 8] = NA
plot(tm_defor, col="pink")

## Reforestation map
# Use the last transition map
tm_refor = rasters_tm[[length(rasters_tm)]]
tm_refor[tm_refor != 7] = NA
plot(tm_refor, col="chartreuse")

## Extract values for each CAR property
car_sf_filtered$defor_pixels = exact_extract(tm_defor, car_sf_filtered, 'count')
car_sf_filtered$refor_pixels = exact_extract(tm_refor, car_sf_filtered, 'count')

## Compute reforested/deforested surface areas in ha
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(area_deforest_ha = round(defor_pixels * px_area_ha, 2),
                area_reforest_ha = round(refor_pixels * px_area_ha, 2),
                prop_deforest = round(area_deforest_ha * 100 / car_area_ha, 2),
                prop_reforest = round(area_reforest_ha * 100 / car_area_ha, 2))

## Quick statistics
cor(car_sf_filtered$car_area_ha, car_sf_filtered$area_deforest_ha)
cor(car_sf_filtered$car_area_ha, car_sf_filtered$area_reforest_ha)
cor(car_sf_filtered$car_area_ha, car_sf_filtered$prop_deforest)
cor(car_sf_filtered$car_area_ha, car_sf_filtered$prop_reforest)


#### 3. Compute land use ON properties --------

## Select LULC rasters
# Forest
lu1989_1 = rasters_reclass[[1]]
lu1989_1[lu1989_1 != 1] = NA
lu2024_1 = rasters_reclass[[length(rasters_reclass)]]
lu2024_1[lu2024_1 != 1] = NA
# Agriculture
lu1989_4 = rasters_reclass[[1]]
lu1989_4[lu1989_4 != 4] = NA
lu2024_4 = rasters_reclass[[length(rasters_reclass)]]
lu2024_4[lu2024_4 != 4] = NA
# Urban
lu1989_6 = rasters_reclass[[1]]
lu1989_6[lu1989_6 != 6] = NA
lu2024_6 = rasters_reclass[[length(rasters_reclass)]]
lu2024_6[lu2024_6 != 6] = NA

## Extract values for each CAR property
# Forest
car_sf_filtered$forest_pixels_1989 = exact_extract(lu1989_1, car_sf_filtered, 'count')
car_sf_filtered$forest_pixels_2024 = exact_extract(lu2024_1, car_sf_filtered, 'count')
# Agriculture
car_sf_filtered$agri_pixels_1989 = exact_extract(lu1989_4, car_sf_filtered, 'count')
car_sf_filtered$agri_pixels_2024 = exact_extract(lu2024_4, car_sf_filtered, 'count')
# Urban
car_sf_filtered$urb_pixels_1989 = exact_extract(lu1989_6, car_sf_filtered, 'count')
car_sf_filtered$urb_pixels_2024 = exact_extract(lu2024_6, car_sf_filtered, 'count')

## Surface, proportions, evolutions
# Surface and proportions
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(area_forest_1989_ha = round(forest_pixels_1989 * px_area_ha, 2),
                area_forest_2024_ha = round(forest_pixels_2024 * px_area_ha, 2),
                prop_forest_1989 = round(area_forest_1989_ha * 100 / car_area_ha, 2),
                prop_forest_2024 = round(area_forest_2024_ha * 100 / car_area_ha, 2),
                area_agri_1989_ha = round(agri_pixels_1989 * px_area_ha, 2),
                area_agri_2024_ha = round(agri_pixels_2024 * px_area_ha, 2),
                prop_agri_1989 = round(area_agri_1989_ha * 100 / car_area_ha, 2),
                prop_agri_2024 = round(area_agri_2024_ha * 100 / car_area_ha, 2),
                area_urb_1989_ha = round(urb_pixels_1989 * px_area_ha, 2),
                area_urb_2024_ha = round(urb_pixels_2024 * px_area_ha, 2),
                prop_urb_1989 = round(area_urb_1989_ha * 100 / car_area_ha, 2),
                prop_urb_2024 = round(area_urb_2024_ha * 100 / car_area_ha, 2))

# Compute the evolution
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    forest_evol_pct = dplyr::if_else(prop_forest_1989 > 0,
                                     round(100 * (prop_forest_2024 - prop_forest_1989) / prop_forest_1989, 2),
                                     NA_real_),
    agri_evol_pct = dplyr::if_else(prop_agri_1989 > 0,
                                   round(100 * (prop_agri_2024 - prop_agri_1989) / prop_agri_1989, 2),
                                   NA_real_),
    urb_evol_pct = dplyr::if_else(prop_urb_1989 > 0,
                                   round(100 * (prop_urb_2024 - prop_urb_1989) / prop_urb_1989, 2),
                                   NA_real_))

## Quick correlations
# Deforestation data
cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$area_forest_1989_ha)
cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$area_forest_2024_ha)
cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$area_agri_1989_ha)
cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$area_agri_2024_ha)
cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$area_urb_1989_ha)
cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$area_urb_2024_ha)
# Reforestation data
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$area_forest_1989_ha)
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$area_forest_2024_ha)
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$area_agri_1989_ha)
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$area_agri_2024_ha)
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$area_urb_1989_ha)
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$area_urb_2024_ha)


#### 4. Compute land use AROUND properties --------
# We create buffers around properties
# We extract the values in the buffers
# The number of pixels outside properties equals: total pixels - pixels inside

## Create buffers
car_buffer = sf::st_buffer(car_sf_filtered, 500) # in meters
plot(car_buffer$geometry)

## Extract LULC in buffers (TOTAL: inside + outside properties)
# Forest
car_sf_filtered$forest_buf_tot_1989 = exact_extract(lu1989_1, car_buffer, 'count')
car_sf_filtered$forest_buf_tot_2024 = exact_extract(lu2024_1, car_buffer, 'count')

# Agriculture
car_sf_filtered$agri_buf_tot_1989 = exact_extract(lu1989_4, car_buffer, 'count')
car_sf_filtered$agri_buf_tot_2024 = exact_extract(lu2024_4, car_buffer, 'count')

# Urban
car_sf_filtered$urb_buf_tot_1989 = exact_extract(lu1989_6, car_buffer, 'count')
car_sf_filtered$urb_buf_tot_2024 = exact_extract(lu2024_6, car_buffer, 'count')

## Pixels AROUND properties = buffer total − pixels ON properties
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    forest_buf_pix_1989 = forest_buf_tot_1989 - forest_pixels_1989,
    forest_buf_pix_2024 = forest_buf_tot_2024 - forest_pixels_2024,
    
    agri_buf_pix_1989 = agri_buf_tot_1989 - agri_pixels_1989,
    agri_buf_pix_2024 = agri_buf_tot_2024 - agri_pixels_2024,
    
    urb_buf_pix_1989 = urb_buf_tot_1989 - urb_pixels_1989,
    urb_buf_pix_2024 = urb_buf_tot_2024 - urb_pixels_2024) %>% 
  dplyr::select(-c(forest_buf_tot_1989, agri_buf_tot_1989, urb_buf_tot_1989))

## Buffer area
# Total valid pixels in buffer
car_sf_filtered$buffer_pixels_tot =
  exact_extract(!is.na(lu1989_1), car_buffer, 'count')
# Pixels inside property
car_sf_filtered$property_pixels =
  exact_extract(!is.na(lu1989_1), car_sf_filtered, 'count')
# Pixels AROUND property
car_sf_filtered$buffer_pixels_ring =
  car_sf_filtered$buffer_pixels_tot - car_sf_filtered$property_pixels
# Area in ha
car_sf_filtered$buffer_area_ha =
  car_sf_filtered$buffer_pixels_ring * px_area_ha

## Surface areas and proportions
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    area_forest_buf_1989_ha = round(forest_buf_pix_1989 * px_area_ha, 2),
    area_forest_buf_2024_ha = round(forest_buf_pix_2024 * px_area_ha, 2),
    prop_forest_buf_1989 = round(area_forest_buf_1989_ha * 100 / buffer_area_ha, 2),
    prop_forest_buf_2024 = round(area_forest_buf_2024_ha * 100 / buffer_area_ha, 2),
    
    area_agri_buf_1989_ha = round(agri_buf_pix_1989 * px_area_ha, 2),
    area_agri_buf_2024_ha = round(agri_buf_pix_2024 * px_area_ha, 2),
    prop_agri_buf_1989 = round(area_agri_buf_1989_ha * 100 / buffer_area_ha, 2),
    prop_agri_buf_2024 = round(area_agri_buf_2024_ha * 100 / buffer_area_ha, 2),
    
    area_urb_buf_1989_ha = round(urb_buf_pix_1989 * px_area_ha, 2),
    area_urb_buf_2024_ha = round(urb_buf_pix_2024 * px_area_ha, 2),
    prop_urb_buf_1989 = round(area_urb_buf_1989_ha * 100 / buffer_area_ha, 2),
    prop_urb_buf_2024 = round(area_urb_buf_2024_ha * 100 / buffer_area_ha, 2)
  )

## Evolution
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    forest_buf_evol_pct = if_else(
      prop_forest_buf_1989 > 0,
      round(100 * (prop_forest_buf_2024 - prop_forest_buf_1989) / prop_forest_buf_1989, 2),
      NA_real_
    ),
    agri_buf_evol_pct = if_else(
      prop_agri_buf_1989 > 0,
      round(100 * (prop_agri_buf_2024 - prop_agri_buf_1989) / prop_agri_buf_1989, 2),
      NA_real_
    ),
    urb_buf_evol_pct = if_else(
      prop_urb_buf_1989 > 0,
      round(100 * (prop_urb_buf_2024 - prop_urb_buf_1989) / prop_urb_buf_1989, 2),
      NA_real_
    )
  )


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