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
rasters_cumul_tm = lapply(raster_df$file, terra::rast)
years_tm = raster_df$years_tm
# Check
for (i in seq_along(rasters_cumul_tm)) {
  cat("Year", years_tm[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_cumul_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))

## NON cumulative rasters
base_path = here("outputs", "data", "MapBiomas", "Rasters_non_cumul_tm")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_tm = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_tm = as.numeric(years_tm)) %>%
  dplyr::arrange(years_tm)
# Load rasters in chronological order
rasters_non_cumul_tm = lapply(raster_df$file, terra::rast)
years_tm = raster_df$years_tm
# Check
for (i in seq_along(rasters_non_cumul_tm)) {
  cat("Year", years_tm[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_non_cumul_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))

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
rasters_cumul_tmin_mean = lapply(raster_df$file, terra::rast)
years_climate = raster_df$years_climate
# Check
for (i in seq_along(rasters_cumul_tmin_mean)) {
  cat("Year", years_climate[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_cumul_tmin_mean[[36]])

## Tmin (max)
base_path = here("outputs", "data", "WorldClim", "tmax")
raster_files = list.files(base_path, pattern = "^tmax_(mean)_\\d{4}_bbox\\.tif$", full.names = TRUE)

# Extract years
years_climate = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")

# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, years_climate = as.numeric(years_climate)) %>%
  dplyr::arrange(years_climate)

# Load rasters in chronological order
rasters_cumul_tmax_mean = lapply(raster_df$file, terra::rast)
years_climate = raster_df$years_climate
# Check
for (i in seq_along(rasters_cumul_tmax_mean)) {
  cat("Year", years_climate[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_cumul_tmax_mean[[36]])


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
# The workflow starts from the raster identifying all cells that experienced at least one land-use/land-cover (LULC) change event during the study period. These “changed cells” form the core sample on which all covariates will be calculated.
# For each event row, the pipeline computes a set of covariates that describe biophysical, legal, and landscape-context characteristics of the cell at the time of its change. All covariates are extracted or computed dynamically with respect to the event year.
# Dataset A: reforested cells + a random sample of intact cells (agricultural cells). Sample matches the number of reforested cells per year.
# Dataset B: deforested cells + a random sample of intact cells (intact forests). Sample matches the number of deforested cells per year.

#### Parameters -----
template_rast = rasters_reclass[[36]]

#### 1. Build table of all change events --------
# We create a table with: the cell id, the coordinates, the type of change (deforestation, reforestation), the previous land use, the following land use, the year of change
# Detects transitions between consecutive rasters (t-1 → t)
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
coords_def = terra::xyFromCell(rasters_reclass[[2]], idx_def)
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
cumul_crop = crop(rasters_cumul_tm[[1]], zoom_ext)

# Plot the cropped raster
plot(cumul_crop, main = "Cumulative Raster 1990 (Zoomed)", col=c("#32a65e", "#519799", "#FFFFB2", "#d4271e", "chartreuse", "pink"))

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
      coords_def = terra::xyFromCell(rasters[[i]], idx_def)
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
      coords_ref = terra::xyFromCell(rasters[[i]], idx_ref)
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
cat("Unique changed cells:", length(unique(changes$cell_id)), "\n") # Number of cells that ever experienced at least one transition over the period
f = terra::freq(rasters_cumul_tm[[35]])
cat("... Now check that it equals the total number of cells that have underwent reforestation or deforestation in cumulative rasters, being:", 
    sum(f$count[f$value %in% c(7,8)]))

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
  dplyr::mutate(cum_n = cumsum(n_cells)) %>% 
  dplyr::mutate(prop = n_cells * 100 / sum(n_cells))


#### 2. Select controls ---------
# We select controls, i.e., cells that were never deforested nor reforested during the study period
# Using the last raster ensures that: controls are still intact at the end, controls never experienced LULCC, we avoid “false controls” that are intact early but changed later
# Control cells were selected among pixels that remained intact throughout the entire study period, and were randomly sampled to match the number of events per year

cat("\nSECTION 2: Selecting intact control cells\n")

# Step 1: Select relevant reforestation/deforestation events
# Rationale:
# - If a pixel changed multiple times, retain only its first change event.
# - Then retain agriculture to forest (for reforestation) and forest to agriculture (for deforestation)

## Reforestation events
# We select reforestation events
changes_refor_subset = changes %>% 
  dplyr::filter(type == 7) 
# We keep first event per pixel
changes_refor_subset = changes_refor_subset %>%
  dplyr::arrange(cell_id, change_order) %>%
  dplyr::group_by(cell_id) %>%
  dplyr::slice_min(change_order, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()
# We subset reforested events to agricultural pixels becoming forests
changes_refor_subset = changes_refor_subset %>% 
  dplyr::filter((from == 4 & to == 1))

## Deforestation events
# We select deforestation events
changes_defor_subset = changes %>% 
  dplyr::filter(type == 8) 
# We keep first event per pixel
changes_defor_subset = changes_defor_subset %>%
  dplyr::arrange(cell_id, change_order) %>%
  dplyr::group_by(cell_id) %>%
  dplyr::slice_min(change_order, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()
# We subset deforested events to forests becoming agricultural pixels
changes_defor_subset = changes_defor_subset %>% 
  dplyr::filter((from == 1 & to == 4))

# Step 2: Count changed cells of each type per year
## Reforestation
refor_events_per_year = changes_refor_subset %>%
  dplyr::count(year, name = "n_events")
head(refor_events_per_year)

## Reforestation
defor_events_per_year = changes_defor_subset %>%
  dplyr::count(year, name = "n_events")
head(defor_events_per_year)

# Step 3: Select intact controls for reforestation
# Select the last cumulative raster (where intact cells are displayed)
last_rast = rasters_cumul_tm[[length(rasters_cumul_tm)]]

# Binary mask: intact agriculture only
agri_mask = last_rast == 4
plot(agri_mask)

# Cell indices of intact agriculture
agri_cells = which(terra::values(agri_mask) == 1)

# Remove ALL cells that ever changed
changed_ids = unique(changes$cell_id)

# Keep only true never-changed intact agriculture cells
agri_cells = setdiff(agri_cells, changed_ids)

# Create shrinking pool to avoid duplicates across years
available_agri = agri_cells

# Random selection of cells according to the number of changes
controls_agri = vector("list", length = nrow(refor_events_per_year))
names(controls_agri) = refor_events_per_year$year

for (i in seq_len(nrow(refor_events_per_year))) {
  yr = refor_events_per_year$year[i]
  n = refor_events_per_year$n_events[i]
  
  if (length(available_agri) < n) {
    stop("Not enough unique agriculture cells left to sample controls.")
  }
  
  # Sample
  cells = sample(available_agri, size = n, replace = FALSE)
  
  # Remove used cells permanently
  available_agri = setdiff(available_agri, cells)
  
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
controls_2023 = controls_agri_df %>%
  dplyr::filter(year == 2023)
plot(rasters_cumul_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))
points(controls_2024$x, controls_2024$y, pch = 20, col = "magenta", cex = 0.4)
points(controls_2023$x, controls_2023$y, pch = 20, col = "darkslategray1", cex = 0.4)
terra::extract(rasters_cumul_tm[[35]], controls_2024[, c("x", "y")]) # Check the value

# Step 4: Select intact controls for deforestation
# Select the last cumulative raster (where intact cells are displayed)
last_rast = rasters_cumul_tm[[length(rasters_cumul_tm)]]

# Binary mask: intact forest only
forest_mask = last_rast == 1
plot(forest_mask)

# Cells of intact forest
forest_cells = which(terra::values(forest_mask) == 1)

# Remove ALL cells that ever changed
forest_cells = setdiff(forest_cells, changed_ids)

# Create shrinking pool
available_forest = forest_cells

# Random selection of cells according to the number of changes
controls_forest = vector("list", length = nrow(defor_events_per_year))
names(controls_forest) = defor_events_per_year$year

for (i in seq_len(nrow(defor_events_per_year))) {
  yr = defor_events_per_year$year[i]
  n  = defor_events_per_year$n_events[i]
  
  if (length(available_forest) < n) {
    stop("Not enough unique forest cells left to sample controls.")
  }
  
  # Sample
  cells = sample(available_forest, size = n, replace = FALSE)
  
  # Remove used cells permanently
  available_forest = setdiff(available_forest, cells)
  
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
plot(rasters_cumul_tm[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))
points(controls_2024$x, controls_2024$y, pch = 20, col = "magenta", cex = 0.4)
points(controls_2023$x, controls_2023$y, pch = 20, col = "darkslategray1", cex = 0.4) # Check that controls differ each year
terra::extract(rasters_cumul_tm[[35]], controls_2024[, c("x", "y")]) # Check the value


# Summary
cat("Controls (deforestation):", nrow(controls_forest_df), "\n")
cat("Controls (reforestation):", nrow(controls_agri_df), "\n")

#### 3. Build final datasets for modelling -------

# Dataset A: Reforest events + control cells (intact agriculture)
data_refor = dplyr::bind_rows(
  changes_refor_subset %>% # Take the filtered dataset!
    dplyr::select(year, cell_id, type, x, y),
  controls_agri_df)
head(data_refor)

# Dataset B: Deforest events + control cells (intact forest)
data_defor = dplyr::bind_rows(
  changes_defor_subset %>% # Take the filtered dataset!
    dplyr::select(year, cell_id, type, x, y),
  controls_forest_df)
head(data_defor)

# Quick check
# Number of rows per type of change
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n())
data_defor %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n())

# Number of cells per type of change
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n_distinct(cell_id))
data_defor %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n_distinct(cell_id))

# Is the number of cell_id equals the number of rows? Should be true if there is no duplicates
data_refor %>% dplyr::summarise(n_cells = dplyr::n_distinct(cell_id)) == nrow(data_refor)
data_defor %>% dplyr::summarise(n_cells = dplyr::n_distinct(cell_id)) == nrow(data_defor)

# Are there duplicated cell_id (pseudoreplication) ? Should return 0 rows
data_refor %>%
  dplyr::count(cell_id) %>%
  dplyr::filter(n > 1)
data_defor %>%
  dplyr::count(cell_id) %>%
  dplyr::filter(n > 1)

# Verify no overlap between controls and cells that changed (should return 0)
# Between reforested and agricultural cells
intersect(changes_refor_subset %>% dplyr::pull(cell_id), 
          controls_agri_df$cell_id)

# Between deforested and forest cells
intersect(changes_defor_subset %>% dplyr::pull(cell_id), 
          controls_forest_df$cell_id)

# Year balance (events and controls should match perfectly)
data_defor %>% dplyr::group_by(type, year) %>% dplyr::count() %>% dplyr::arrange(year)
data_refor %>% dplyr::group_by(type, year) %>% dplyr::count() %>% dplyr::arrange(year)

# Intersection between both datasets (cells that were deforested AND reforested)
intersect(
  data_refor$cell_id[data_refor$type == 7],
  data_defor$cell_id[data_defor$type == 8]
)

## Compare pixels in modeling datasets with cumulative rasters
# Unique event pixels in modelling datasets
event_pixels_model = union(data_refor$cell_id[data_refor$type == 7], data_defor$cell_id[data_defor$type == 8])
length(event_pixels_model)
# Event pixels in cumulative raster
f = terra::freq(rasters_cumul_tm[[35]])
sum(f$count[f$value %in% c(7,8)])
# Conclusion : minor differences (around 4000 pixels in the cumulative raster are lacking in the modeling dataset) 
# This difference is normal because rasters_cumul_tm includes all transitions (ex: 1→6, 5→1) while modeling datasets is focused on 1-4 and 4-1

# Are pixels in the modeling dataset all comprised in the cumulative raster ?
pixels_cumul = which(values(rasters_cumul_tm[[35]]) %in% c(7,8))
all(event_pixels_model %in% pixels_cumul) # Should be true, i.e., all pixels used for modeling are comprised in the cumulative raster

# Plot datasets
plot(rasters_cumul_tm[[35]], col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))
points(data_refor$x[data_refor$type == 4 & data_refor$year == 2024],
       data_refor$y[data_refor$type == 4 & data_refor$year == 2024],
       col = "magenta", pch = 20, cex = 0.5)
points(data_refor$x[data_refor$type == 7 & data_refor$year == 2024],
       data_refor$y[data_refor$type == 7 & data_refor$year == 2024],
       col = "darkslategray1", pch = 20, cex = 0.5)

plot(rasters_cumul_tm[[35]], col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))
points(data_defor$x[data_defor$type == 1 & data_defor$year == 2024],
       data_defor$y[data_defor$type == 1 & data_defor$year == 2024],
       col = "magenta", pch = 20, cex = 0.5)
points(data_defor$x[data_defor$type == 8 & data_defor$year == 2024],
       data_defor$y[data_defor$type == 8 & data_defor$year == 2024],
       col = "darkslategray1", pch = 20, cex = 0.5)

# Summary
cat("Dataset 'Reforestation' rows:", nrow(data_refor), "\n")
cat("Dataset 'Deforestation' rows:", nrow(data_defor), "\n")


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

## Area covered by public reserves
# Get spatial resolution
resolution = res(pub_res_r)[1] 
pixel_area_ha = (resolution^2) / 10000  # Resolution in ha
# Surface covered by public reserves
freq_table = freq(pub_res_r)
freq_table$surface_ha = freq_table$count * pixel_area_ha
# Compute proportion
total_pixels = sum(freq_table$count)
freq_table$percentage = (freq_table$count / total_pixels) * 100

# Afficher le résultat
print(freq_table[, c("value", "count", "surface_ha", "percentage")])
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
# The logic is: Distance to the existing forest edge at the time deforestation happens (answering: How close was this loss to an existing forest edge?)
# Which is slightly different for reforestation: For reforestation events, we compute distance to forest edge in year t − 1 (answering: How far from existing forest did reforestation occur?)

# Initialize column
data_defor$dist_edge_m = NA_real_
data_refor$dist_edge_m = NA_real_

# DEFORESTATION — distance to forest edge in same year
unique_years = sort(unique(data_defor$year))
for (yr in unique_years) {
  
  cat("  Processing forest edges for year:", yr, "\n")
  
  # Identify rows for this year
  idx = which(data_defor$year == yr)
  if (length(idx) == 0) next
  
  # Get the forest raster for the corresponding year
  r_year = rasters_reclass[[which(years_lulc == yr)]]
  
  # Build binary forest mask: 1 = forest, NA = other
  forest_mask = r_year
  forest_mask[forest_mask != 1] = NA
  
  # Compute forest edges
  edge_list = landscapemetrics::get_boundaries(forest_mask, as_NA = TRUE)
  edge_rast = edge_list[[1]]
  
  # Compute distance raster
  dist_edge_r = terra::distance(edge_rast)
  
  # Extract distances for rows of this year
  d = terra::extract(dist_edge_r, data_defor[idx, c("x", "y")], ID = FALSE)[,1]
  data_defor$dist_edge_m[idx] = d
}

# REFORESTATION — distance to forest edge in previous year (t − 1)
unique_years = sort(unique(data_refor$year))
for (yr in unique_years) {
  
  cat("  Processing forest edges for year:", yr, "\n")
  
  # Identify rows for this year
  idx = which(data_refor$year == yr)
  if (length(idx) == 0) next
  
  yr_pos = which(years_lulc == yr)
  if (yr_pos <= 1) next   # no previous year
  
  r_prev = rasters_reclass[[yr_pos - 1]]
  
  # Build binary forest mask: 1 = forest, NA = other
  forest_mask = r_prev
  forest_mask[forest_mask != 1] = NA
  
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


#### 7. Slope and altitude --------
# Slope and altitude values are extracted for each event location.
# The extraction returns some NAs so we fix that by extracting closest values.
cat("SECTION 7: extracting topo values\n")

## Summary of elevation in the landscape
summary(topo_r, size=10000000, warn=TRUE)
        
compute_topo = function(dt, raster, var_name) {
  
  cat("\n--- Extracting", var_name, "---\n")
  
  dt[[var_name]] = NA_real_
  
  # build point vector once
  pts = terra::vect(
    dt[, c("x", "y")],
    geom = c("x", "y"),
    crs = terra::crs(raster)
  )
  
  # initial extraction (cell value)
  vals = terra::extract(raster, pts, ID = FALSE)[,1]
  
  # identify NA values
  na_idx = which(is.na(vals))
  
  if (length(na_idx) > 0) {
    cat("  → Fixing", length(na_idx), "NA values using nearest cell\n")
    
    nearest = terra::extract(
      raster,
      pts[na_idx],
      method = "near"
    )[,1]
    
    vals[na_idx] = nearest
  }
  
  dt[[var_name]] = vals
  return(dt)
}

data_defor = compute_topo(data_defor, slope_r, var_name = "slope_pct")
data_refor = compute_topo(data_refor, slope_r, var_name = "slope_pct")

data_defor = compute_topo(data_defor, topo_r, var_name = "alt_m")
data_refor = compute_topo(data_refor, topo_r, var_name = "alt_m")

data_defor %>% dplyr::group_by(type) %>% dplyr::summarise(mean_slope = mean(slope_pct, na.rm=TRUE))
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise(mean_slope = mean(slope_pct, na.rm=TRUE))

data_defor %>% dplyr::group_by(type) %>% dplyr::summarise(mean_alt = mean(alt_m, na.rm=TRUE))
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise(mean_alt = mean(alt_m, na.rm=TRUE))

cat("Slope & altitude extraction completed\n")


#### 8. Land use area ----------
# We extract land-use areas within a buffer around each point, year by year, for specified land-use classes
# Numerous options: sample_lsm (landscapemetrics), package multilandr, exact_extractr... but all these options are slow
# Instead, we can use a moving window approach (faster with numerous points)
# We create a binary raster for the class
# Compute moving window counts
# Compute valid area to correct borders
# Calculate proportion
# Extracts values per year for the point

cat("\nSECTION 8: Land use area extraction around buffers\n")

## Example
# Select raster
r = rasters_reclass[[2]]
# Select class
class_id = 1
r_class = ifel(r == class_id, 1, 0)
plot(r_class)
# Moving window
radius_m = 500
cellsize = res(r)[1]
radius_cells = ceiling(radius_m / cellsize) # Number of cells for the radius
size = 2 * radius_cells + 1 # MW size
w = matrix(1, size, size)
sum(w) # Max cells
# Count class pixels
mw_class = focal(r_class, w = w, fun = sum, na.rm = TRUE)
max(mw_class) # Check the max : must be <= sum(w)
# Count valid pixels
r_valid = !is.na(r)
mw_valid = focal(r_valid, w = w, fun = sum, na.rm = TRUE)
plot(mw_valid)
# Proportion
class_prop = mw_class / mw_valid
plot(class_prop)


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

# Extract into datasets
mw_lulc = function(rasters, years, data, class_id, radius_m, varname){
  
  results = vector("list", length(rasters))
  
  for(i in seq_along(rasters)){
    
    message("Computing MW for year ", years[i])
    
    r = rasters[[i]]
    
    mw = compute_mw_prop(
      r = r,
      class_id = class_id,
      radius_m = radius_m
    )
    
    df_year = data[data$year == years[i],]
    
    if(nrow(df_year) > 0){
      
      vals = terra::extract(
        mw,
        df_year[,c("x","y")]
      )
      
      df_year[[varname]] = vals[,2]
      
      results[[i]] = df_year
    }
    
  }
  
  dplyr::bind_rows(results)
}

# Apply function
# On deforestation datasets
# forest
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 1, radius_m = 100, varname = "prop_forest_100m")
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 1, radius_m = 500, varname = "prop_forest_500m")
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 1, radius_m = 1000, varname = "prop_forest_1000m")
# agriculture
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 4, radius_m = 100, varname = "prop_agri_100m")
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 4, radius_m = 500, varname = "prop_agri_500m")
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 4, radius_m = 1000, varname = "prop_agri_1000m")
# built-up areas
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 6, radius_m = 100, varname = "prop_urb_100m")
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 6, radius_m = 500, varname = "prop_urb_500m")
data_defor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_defor, class_id = 6, radius_m = 1000, varname = "prop_urb_1000m")
# deforestation
data_defor = mw_lulc(rasters = rasters_non_cumul_tm, years = years_tm, data = data_defor, class_id = 8, radius_m = 100, varname = "prop_defor_100m")
data_defor = mw_lulc(rasters = rasters_non_cumul_tm, years = years_tm, data = data_defor, class_id = 8, radius_m = 500, varname = "prop_defor_500m")
data_defor = mw_lulc(rasters = rasters_non_cumul_tm, years = years_tm, data = data_defor, class_id = 8, radius_m = 1000, varname = "prop_defor_1000m")

# On reforestation datasets
# forest
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 1, radius_m = 100, varname = "prop_forest_100m")
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 1, radius_m = 500, varname = "prop_forest_500m")
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 1, radius_m = 1000, varname = "prop_forest_1000m")
# agriculture
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 4, radius_m = 100, varname = "prop_agri_100m")
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 4, radius_m = 500, varname = "prop_agri_500m")
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 4, radius_m = 1000, varname = "prop_agri_1000m")
# built-up areas
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 6, radius_m = 100, varname = "prop_urb_100m")
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 6, radius_m = 500, varname = "prop_urb_500m")
data_refor = mw_lulc(rasters = rasters_reclass, years = years_lulc, data = data_refor, class_id = 6, radius_m = 1000, varname = "prop_urb_1000m")
# reforestation
data_refor = mw_lulc(rasters = rasters_non_cumul_tm, years = years_tm, data = data_refor, class_id = 7, radius_m = 100, varname = "prop_refor_100m")
data_refor = mw_lulc(rasters = rasters_non_cumul_tm, years = years_tm, data = data_refor, class_id = 7, radius_m = 500, varname = "prop_refor_500m")
data_refor = mw_lulc(rasters = rasters_non_cumul_tm, years = years_tm, data = data_refor, class_id = 7, radius_m = 1000, varname = "prop_refor_1000m")

# Test
test = data_defor[1,]
terra::extract(class_prop, test[,c("x","y")]) # Value with our manual function
data_defor[1,19] # Value extracted with automatic extraction
# Check that both values match!

# Summary statistics
data_defor %>% dplyr::group_by(type) %>% dplyr::summarise_at(vars(starts_with("prop")), mean, na.rm = TRUE)
data_refor %>% dplyr::group_by(type) %>% dplyr::summarise_at(vars(starts_with("prop")), mean, na.rm = TRUE)

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

## Summary of precipitations in the landscape
summary(rasters_prec_sum[[36]], size=10000000, warn=TRUE)

# Function
# Inputs = dataframe, climate rasters, name of the output variable (var_name) (e.g., prec_mean)
# We extract the value at point location
# Some points may not have a value (rasters do not overlay all locations); therefore, the nearest value is taken
names(rasters_prec_sum) = years_climate
names(rasters_cumul_tmax_mean) = years_climate
names(rasters_cumul_tmin_mean) = years_climate

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
        method = "near"
      )[,1]
      vals[na_idx] = nearest
    }
    
    dt[[var_name]][idx] = vals
  }
  
  dt
}

# Extract for deforestation 
data_defor = compute_climate(data_defor, rasters_prec_sum, "prec_sum")
data_defor = compute_climate(data_defor, rasters_cumul_tmin_mean, "tmin_mean")
data_defor = compute_climate(data_defor, rasters_cumul_tmax_mean, "tmax_mean")

# Extract for reforestation
data_refor = compute_climate(data_refor, rasters_prec_sum, "prec_sum")
data_refor = compute_climate(data_refor, rasters_cumul_tmin_mean, "tmin_mean")
data_refor = compute_climate(data_refor, rasters_cumul_tmax_mean, "tmax_mean")

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
r = rasters_cumul_tm[[35]]
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
tm_defor = rasters_cumul_tm[[length(rasters_cumul_tm)]]
tm_defor[tm_defor != 8] = NA
plot(tm_defor, col="pink")

## Reforestation map
# Use the last transition map
tm_refor = rasters_cumul_tm[[length(rasters_cumul_tm)]]
tm_refor[tm_refor != 7] = NA
plot(tm_refor, col="chartreuse")

## Extract values for each CAR property
# "Count" = the sum of fractions of raster cells with non-NA values covered by the polygon (exactextractr)
# This returns fractional pixels
car_sf_filtered$defor_pixels = exact_extract(tm_defor, car_sf_filtered, 'count')
car_sf_filtered$refor_pixels = exact_extract(tm_refor, car_sf_filtered, 'count')

## Compute reforested/deforested surface areas in ha
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(area_deforest_ha = round(defor_pixels * px_area_ha, 2),
                area_reforest_ha = round(refor_pixels * px_area_ha, 2),
                prop_deforest = ifelse(area_deforest_ha > 0, area_deforest_ha / car_area_ha, 0),
                prop_reforest = ifelse(area_reforest_ha > 0, area_reforest_ha / car_area_ha, 0))

## Quick statistics
cor(car_sf_filtered$car_area_ha, car_sf_filtered$area_deforest_ha)
cor(car_sf_filtered$car_area_ha, car_sf_filtered$area_reforest_ha)
cor(car_sf_filtered$car_area_ha, car_sf_filtered$prop_deforest)
cor(car_sf_filtered$car_area_ha, car_sf_filtered$prop_reforest)


#### 3. Land use ON properties --------

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

## Surface
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(# Area
                area_forest_1989_ha = round(forest_pixels_1989 * px_area_ha, 2),
                area_forest_2024_ha = round(forest_pixels_2024 * px_area_ha, 2),
                area_agri_1989_ha = round(agri_pixels_1989 * px_area_ha, 2),
                area_agri_2024_ha = round(agri_pixels_2024 * px_area_ha, 2),
                area_urb_1989_ha = round(urb_pixels_1989 * px_area_ha, 2),
                area_urb_2024_ha = round(urb_pixels_2024 * px_area_ha, 2),
                # Proportions
                prop_forest_1989 = area_forest_1989_ha / car_area_ha,
                prop_forest_2024 = area_forest_2024_ha / car_area_ha,
                prop_agri_1989 = area_agri_1989_ha / car_area_ha,
                prop_agri_2024 = area_agri_2024_ha / car_area_ha,
                prop_urb_1989 = area_urb_1989_ha / car_area_ha,
                prop_urb_2024 = area_urb_2024_ha / car_area_ha,
                # Evolution
                evol_forest_pct = ifelse(area_forest_1989_ha > 0,
                                         100 * (area_forest_2024_ha - area_forest_1989_ha) / area_forest_1989_ha,
                                         0),
                evol_agri_pct = ifelse(area_agri_1989_ha > 0,
                                       100 * (area_agri_2024_ha - area_agri_1989_ha) / area_agri_1989_ha,
                                       0),
                evol_urb_pct = ifelse(area_urb_1989_ha > 0,
                                      100 * (area_urb_2024_ha - area_urb_1989_ha) / area_urb_1989_ha,
                                      0))

#### 4. Land use AROUND properties --------
# We create buffers around properties
# We extract the values in the buffers
# The number of pixels outside properties equals: total pixels - pixels inside

##### 100 m ---------
## Create buffers
car_buffer = sf::st_buffer(car_sf_filtered, 100) # in meters
car_buffer = crop(vect(car_buffer), template_rast) # Crop to raster extent
car_buffer = sf::st_as_sf(car_buffer)
plot(template_rast, col = "lightgray")
plot(st_geometry(car_buffer), add = TRUE, border = "red")

## Extract LULC in buffers (TOTAL: inside + outside properties)
# Forest
car_sf_filtered$forest_buf100_tot_1989 = exact_extract(lu1989_1, car_buffer, 'count')
car_sf_filtered$forest_buf100_tot_2024 = exact_extract(lu2024_1, car_buffer, 'count')

# Agriculture
car_sf_filtered$agri_buf100_tot_1989 = exact_extract(lu1989_4, car_buffer, 'count')
car_sf_filtered$agri_buf100_tot_2024 = exact_extract(lu2024_4, car_buffer, 'count')

# Urban
car_sf_filtered$urb_buf100_tot_1989 = exact_extract(lu1989_6, car_buffer, 'count')
car_sf_filtered$urb_buf100_tot_2024 = exact_extract(lu2024_6, car_buffer, 'count')

# Reforestation and deforestation
car_sf_filtered$refor_buf100_tot_2024 = exact_extract(tm_refor, car_buffer, 'count')
car_sf_filtered$defor_buf100_tot_2024 = exact_extract(tm_defor, car_buffer, 'count')

## Pixels AROUND properties = buffer total − pixels ON properties
# With pmax : if value < 0 → return 0
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    forest_buf100_pix_1989 = pmax(0, forest_buf100_tot_1989 - forest_pixels_1989),
    forest_buf100_pix_2024 = pmax(0, forest_buf100_tot_2024 - forest_pixels_2024),
    agri_buf100_pix_1989 = pmax(0, agri_buf100_tot_1989 - agri_pixels_1989),
    agri_buf100_pix_2024 = pmax(0, agri_buf100_tot_2024 - agri_pixels_2024),
    urb_buf100_pix_1989 = pmax(0, urb_buf100_tot_1989 - urb_pixels_1989),
    urb_buf100_pix_2024 = pmax(0, urb_buf100_tot_2024 - urb_pixels_2024),
    refor_buf100_pix_2024 = pmax(0, refor_buf100_tot_2024 - refor_pixels),
    defor_buf100_pix_2024 = pmax(0, defor_buf100_tot_2024 - defor_pixels))

## Surface areas
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    # Area
    area_forest_buf100_1989_ha = round(forest_buf100_pix_1989 * px_area_ha, 2),
    area_forest_buf100_2024_ha = round(forest_buf100_pix_2024 * px_area_ha, 2),
    area_agri_buf100_1989_ha = round(agri_buf100_pix_1989 * px_area_ha, 2),
    area_agri_buf100_2024_ha = round(agri_buf100_pix_2024 * px_area_ha, 2),
    area_urb_buf100_1989_ha = round(urb_buf100_pix_1989 * px_area_ha, 2),
    area_urb_buf100_2024_ha = round(urb_buf100_pix_2024 * px_area_ha, 2),
    area_refor_buf100_2024_ha = round(refor_buf100_pix_2024 * px_area_ha, 2),
    area_defor_buf100_2024_ha = round(defor_buf100_pix_2024 * px_area_ha, 2)
  )

## Proportions
# Area of the buffer ring
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    area_buf100_ring_ha = as.numeric(sf::st_area(car_buffer) / 10000) - car_area_ha
  )
# Proportions and evolution
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    
    # proportions in the surrounding landscape
    prop_forest_buf100_1989 = area_forest_buf100_1989_ha / area_buf100_ring_ha,
    prop_forest_buf100_2024 = area_forest_buf100_2024_ha / area_buf100_ring_ha,
    
    prop_agri_buf100_1989 = area_agri_buf100_1989_ha / area_buf100_ring_ha,
    prop_agri_buf100_2024 = area_agri_buf100_2024_ha / area_buf100_ring_ha,
    
    prop_urb_buf100_1989 = area_urb_buf100_1989_ha / area_buf100_ring_ha,
    prop_urb_buf100_2024 = area_urb_buf100_2024_ha / area_buf100_ring_ha,
    
    prop_refor_buf100_2024 = area_refor_buf100_2024_ha / area_buf100_ring_ha,
    prop_defor_buf100_2024 = area_defor_buf100_2024_ha / area_buf100_ring_ha,
    
    # percentage change
    evol_forest_buf100_pct =
      ifelse(area_forest_buf100_1989_ha > 0,
             100 * (area_forest_buf100_2024_ha - area_forest_buf100_1989_ha) /
               area_forest_buf100_1989_ha,
             0),
    
    evol_agri_buf100_pct =
      ifelse(area_agri_buf100_1989_ha > 0,
             100 * (area_agri_buf100_2024_ha - area_agri_buf100_1989_ha) /
               area_agri_buf100_1989_ha,
             0),
    
    evol_urb_buf100_pct =
      ifelse(area_urb_buf100_1989_ha > 0,
             100 * (area_urb_buf100_2024_ha - area_urb_buf100_1989_ha) /
               area_urb_buf100_1989_ha,
             0)
  )



##### 500 m ---------
## Create buffers
car_buffer = sf::st_buffer(car_sf_filtered, 500) # in meters
car_buffer = crop(vect(car_buffer), template_rast) # Crop to raster extent
car_buffer = sf::st_as_sf(car_buffer)
plot(template_rast, col = "lightgray")
plot(st_geometry(car_buffer), add = TRUE, border = "red")

## Extract LULC in buffers (TOTAL: inside + outside properties)
# Forest
car_sf_filtered$forest_buf500_tot_1989 = exact_extract(lu1989_1, car_buffer, 'count')
car_sf_filtered$forest_buf500_tot_2024 = exact_extract(lu2024_1, car_buffer, 'count')

# Agriculture
car_sf_filtered$agri_buf500_tot_1989 = exact_extract(lu1989_4, car_buffer, 'count')
car_sf_filtered$agri_buf500_tot_2024 = exact_extract(lu2024_4, car_buffer, 'count')

# Urban
car_sf_filtered$urb_buf500_tot_1989 = exact_extract(lu1989_6, car_buffer, 'count')
car_sf_filtered$urb_buf500_tot_2024 = exact_extract(lu2024_6, car_buffer, 'count')

# Reforestation and deforestation
car_sf_filtered$refor_buf500_tot_2024 = exact_extract(tm_refor, car_buffer, 'count')
car_sf_filtered$defor_buf500_tot_2024 = exact_extract(tm_defor, car_buffer, 'count')

## Pixels AROUND properties = buffer total − pixels ON properties
# With pmax : if value < 0 → return 0
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    forest_buf500_pix_1989 = pmax(0, forest_buf500_tot_1989 - forest_pixels_1989),
    forest_buf500_pix_2024 = pmax(0, forest_buf500_tot_2024 - forest_pixels_2024),
    agri_buf500_pix_1989 = pmax(0, agri_buf500_tot_1989 - agri_pixels_1989),
    agri_buf500_pix_2024 = pmax(0, agri_buf500_tot_2024 - agri_pixels_2024),
    urb_buf500_pix_1989 = pmax(0, urb_buf500_tot_1989 - urb_pixels_1989),
    urb_buf500_pix_2024 = pmax(0, urb_buf500_tot_2024 - urb_pixels_2024),
    refor_buf500_pix_2024 = pmax(0, refor_buf500_tot_2024 - refor_pixels),
    defor_buf500_pix_2024 = pmax(0, defor_buf500_tot_2024 - defor_pixels))

## Surface areas
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    # Area
    area_forest_buf500_1989_ha = round(forest_buf500_pix_1989 * px_area_ha, 2),
    area_forest_buf500_2024_ha = round(forest_buf500_pix_2024 * px_area_ha, 2),
    area_agri_buf500_1989_ha = round(agri_buf500_pix_1989 * px_area_ha, 2),
    area_agri_buf500_2024_ha = round(agri_buf500_pix_2024 * px_area_ha, 2),
    area_urb_buf500_1989_ha = round(urb_buf500_pix_1989 * px_area_ha, 2),
    area_urb_buf500_2024_ha = round(urb_buf500_pix_2024 * px_area_ha, 2),
    area_refor_buf500_2024_ha = round(refor_buf500_pix_2024 * px_area_ha, 2),
    area_defor_buf500_2024_ha = round(defor_buf500_pix_2024 * px_area_ha, 2)
  )

## Proportions
# Area of the buffer ring
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    area_buf500_ring_ha = as.numeric(sf::st_area(car_buffer) / 10000) - car_area_ha
  )
# Proportions and evolution
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    
    # proportions in the surrounding landscape
    prop_forest_buf500_1989 = area_forest_buf500_1989_ha / area_buf500_ring_ha,
    prop_forest_buf500_2024 = area_forest_buf500_2024_ha / area_buf500_ring_ha,
    
    prop_agri_buf500_1989 = area_agri_buf500_1989_ha / area_buf500_ring_ha,
    prop_agri_buf500_2024 = area_agri_buf500_2024_ha / area_buf500_ring_ha,
    
    prop_urb_buf500_1989 = area_urb_buf500_1989_ha / area_buf500_ring_ha,
    prop_urb_buf500_2024 = area_urb_buf500_2024_ha / area_buf500_ring_ha,
    
    prop_refor_buf500_2024 = area_refor_buf500_2024_ha / area_buf500_ring_ha,
    prop_defor_buf500_2024 = area_defor_buf500_2024_ha / area_buf500_ring_ha,
    
    # percentage change
    evol_forest_buf500_pct =
      ifelse(area_forest_buf500_1989_ha > 0,
             100 * (area_forest_buf500_2024_ha - area_forest_buf500_1989_ha) /
               area_forest_buf500_1989_ha,
             0),
    
    evol_agri_buf500_pct =
      ifelse(area_agri_buf500_1989_ha > 0,
             100 * (area_agri_buf500_2024_ha - area_agri_buf500_1989_ha) /
               area_agri_buf500_1989_ha,
             0),
    
    evol_urb_buf500_pct =
      ifelse(area_urb_buf500_1989_ha > 0,
             100 * (area_urb_buf500_2024_ha - area_urb_buf500_1989_ha) /
               area_urb_buf500_1989_ha,
             0)
  )




##### 1000 m ---------
## Create buffers
car_buffer = sf::st_buffer(car_sf_filtered, 1000) # in meters
car_buffer = crop(vect(car_buffer), template_rast) # Crop to raster extent
car_buffer = sf::st_as_sf(car_buffer)
plot(template_rast, col = "lightgray")
plot(st_geometry(car_buffer), add = TRUE, border = "red")

## Extract LULC in buffers (TOTAL: inside + outside properties)
# Forest
car_sf_filtered$forest_buf1000_tot_1989 = exact_extract(lu1989_1, car_buffer, 'count')
car_sf_filtered$forest_buf1000_tot_2024 = exact_extract(lu2024_1, car_buffer, 'count')

# Agriculture
car_sf_filtered$agri_buf1000_tot_1989 = exact_extract(lu1989_4, car_buffer, 'count')
car_sf_filtered$agri_buf1000_tot_2024 = exact_extract(lu2024_4, car_buffer, 'count')

# Urban
car_sf_filtered$urb_buf1000_tot_1989 = exact_extract(lu1989_6, car_buffer, 'count')
car_sf_filtered$urb_buf1000_tot_2024 = exact_extract(lu2024_6, car_buffer, 'count')

# Reforestation and deforestation
car_sf_filtered$refor_buf1000_tot_2024 = exact_extract(tm_refor, car_buffer, 'count')
car_sf_filtered$defor_buf1000_tot_2024 = exact_extract(tm_defor, car_buffer, 'count')

## Pixels AROUND properties = buffer total − pixels ON properties
# With pmax : if value < 0 → return 0
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    forest_buf1000_pix_1989 = pmax(0, forest_buf1000_tot_1989 - forest_pixels_1989),
    forest_buf1000_pix_2024 = pmax(0, forest_buf1000_tot_2024 - forest_pixels_2024),
    agri_buf1000_pix_1989 = pmax(0, agri_buf1000_tot_1989 - agri_pixels_1989),
    agri_buf1000_pix_2024 = pmax(0, agri_buf1000_tot_2024 - agri_pixels_2024),
    urb_buf1000_pix_1989 = pmax(0, urb_buf1000_tot_1989 - urb_pixels_1989),
    urb_buf1000_pix_2024 = pmax(0, urb_buf1000_tot_2024 - urb_pixels_2024),
    refor_buf1000_pix_2024 = pmax(0, refor_buf1000_tot_2024 - refor_pixels),
    defor_buf1000_pix_2024 = pmax(0, defor_buf1000_tot_2024 - defor_pixels))

## Surface areas
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    # Area
    area_forest_buf1000_1989_ha = round(forest_buf1000_pix_1989 * px_area_ha, 2),
    area_forest_buf1000_2024_ha = round(forest_buf1000_pix_2024 * px_area_ha, 2),
    area_agri_buf1000_1989_ha = round(agri_buf1000_pix_1989 * px_area_ha, 2),
    area_agri_buf1000_2024_ha = round(agri_buf1000_pix_2024 * px_area_ha, 2),
    area_urb_buf1000_1989_ha = round(urb_buf1000_pix_1989 * px_area_ha, 2),
    area_urb_buf1000_2024_ha = round(urb_buf1000_pix_2024 * px_area_ha, 2),
    area_refor_buf1000_2024_ha = round(refor_buf1000_pix_2024 * px_area_ha, 2),
    area_defor_buf1000_2024_ha = round(defor_buf1000_pix_2024 * px_area_ha, 2)
  )

## Proportions
# Area of the buffer ring
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    area_buf1000_ring_ha = as.numeric(sf::st_area(car_buffer) / 10000) - car_area_ha
  )
# Proportions and evolution
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    
    # proportions in the surrounding landscape
    prop_forest_buf1000_1989 = area_forest_buf1000_1989_ha / area_buf1000_ring_ha,
    prop_forest_buf1000_2024 = area_forest_buf1000_2024_ha / area_buf1000_ring_ha,
    
    prop_agri_buf1000_1989 = area_agri_buf1000_1989_ha / area_buf1000_ring_ha,
    prop_agri_buf1000_2024 = area_agri_buf1000_2024_ha / area_buf1000_ring_ha,
    
    prop_urb_buf1000_1989 = area_urb_buf1000_1989_ha / area_buf1000_ring_ha,
    prop_urb_buf1000_2024 = area_urb_buf1000_2024_ha / area_buf1000_ring_ha,
    
    prop_refor_buf1000_2024 = area_refor_buf1000_2024_ha / area_buf1000_ring_ha,
    prop_defor_buf1000_2024 = area_defor_buf1000_2024_ha / area_buf1000_ring_ha,
    
    # percentage change
    evol_forest_buf1000_pct =
      ifelse(area_forest_buf1000_1989_ha > 0,
             100 * (area_forest_buf1000_2024_ha - area_forest_buf1000_1989_ha) /
               area_forest_buf1000_1989_ha,
             0),
    
    evol_agri_buf1000_pct =
      ifelse(area_agri_buf1000_1989_ha > 0,
             100 * (area_agri_buf1000_2024_ha - area_agri_buf1000_1989_ha) /
               area_agri_buf1000_1989_ha,
             0),
    
    evol_urb_buf1000_pct =
      ifelse(area_urb_buf1000_1989_ha > 0,
             100 * (area_urb_buf1000_2024_ha - area_urb_buf1000_1989_ha) /
               area_urb_buf1000_1989_ha,
             0)
  )




##### Remove unnecessary variables -----------
colnames(car_sf_filtered)
car_sf_filtered = car_sf_filtered %>% 
  dplyr::select(-c(defor_pixels,
                   refor_pixels,
                   forest_pixels_1989,
                   forest_pixels_2024,
                   agri_pixels_1989,
                   agri_pixels_2024,
                   urb_pixels_1989,
                   urb_pixels_2024,
                   
                   area_buf100_ring_ha,
                   area_buf500_ring_ha,
                   area_buf1000_ring_ha,
                   
                   forest_buf100_tot_1989,
                   forest_buf100_tot_2024,
                   agri_buf100_tot_1989,
                   agri_buf100_tot_2024,
                   urb_buf100_tot_1989,
                   urb_buf100_tot_2024,
                   refor_buf100_tot_2024,
                   defor_buf100_tot_2024,
                   forest_buf100_pix_1989,
                   forest_buf100_pix_2024,
                   agri_buf100_pix_1989,
                   agri_buf100_pix_2024,
                   urb_buf100_pix_1989,
                   urb_buf100_pix_2024,
                   refor_buf100_pix_2024,
                   defor_buf100_pix_2024,
                   
                   forest_buf500_tot_1989,
                   forest_buf500_tot_2024,
                   agri_buf500_tot_1989,
                   agri_buf500_tot_2024,
                   urb_buf500_tot_1989,
                   urb_buf500_tot_2024,
                   refor_buf500_tot_2024,
                   defor_buf500_tot_2024,
                   forest_buf500_pix_1989,
                   forest_buf500_pix_2024,
                   agri_buf500_pix_1989,
                   agri_buf500_pix_2024,
                   urb_buf500_pix_1989,
                   urb_buf500_pix_2024,
                   refor_buf500_pix_2024,
                   defor_buf500_pix_2024,
                   
                   forest_buf1000_tot_1989,
                   forest_buf1000_tot_2024,
                   agri_buf1000_tot_1989,
                   agri_buf1000_tot_2024,
                   urb_buf1000_tot_1989,
                   urb_buf1000_tot_2024,
                   refor_buf1000_tot_2024,
                   defor_buf1000_tot_2024,
                   forest_buf1000_pix_1989,
                   forest_buf1000_pix_2024,
                   agri_buf1000_pix_1989,
                   agri_buf1000_pix_2024,
                   urb_buf1000_pix_1989,
                   urb_buf1000_pix_2024,
                   refor_buf1000_pix_2024,
                   defor_buf1000_pix_2024))

#### 5. Distances --------

## Centroids (sf)
car_centroids_sf = car_sf_filtered %>%
  dplyr::mutate(geometry = sf::st_centroid(geometry))
plot(sf::st_geometry(car_centroids_sf))

# Extract centroids coordinates
car_sf_filtered = car_sf_filtered %>%
  dplyr::mutate(
    centroid_x = sf::st_coordinates(sf::st_centroid(geometry))[, 1],
    centroid_y = sf::st_coordinates(sf::st_centroid(geometry))[, 2]
  )

### 1. Distance to urban centers
nearest_id = sf::st_nearest_feature(car_centroids_sf, urb_centers_sf)
dist_m =
  sf::st_distance(car_centroids_sf, urb_centers_sf[nearest_id, ],
                  by_element = TRUE)
car_sf_filtered$dist_to_urban_m = as.numeric(dist_m)


### 2. Distance to ROADS
nearest_id = sf::st_nearest_feature(car_centroids_sf, roads_sf)
dist_m =
  sf::st_distance(car_centroids_sf, roads_sf[nearest_id, ],
                  by_element = TRUE)
car_sf_filtered$dist_to_road_m = as.numeric(dist_m)


### 3. Distance to RIVERS
nearest_id = sf::st_nearest_feature(car_centroids_sf, rivers_sf)
dist_m =
  sf::st_distance(car_centroids_sf, rivers_sf[nearest_id, ],
                  by_element = TRUE)
car_sf_filtered$dist_to_river_m = as.numeric(dist_m)

# Quick correlations
cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$dist_to_urban_m)
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$dist_to_urban_m)

cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$dist_to_road_m)
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$dist_to_road_m)

cor(car_sf_filtered$area_deforest_ha, car_sf_filtered$dist_to_river_m)
cor(car_sf_filtered$area_reforest_ha, car_sf_filtered$dist_to_river_m)


#### 6. Intersections --------
# 1. Rivers
car_sf_filtered$intersects_river =
  as.integer(lengths(sf::st_intersects(car_sf_filtered, rivers_sf)) > 0)

# 2. APA
car_sf_filtered$intersects_apa =
  as.integer(lengths(sf::st_intersects(car_sf_filtered, apa_mld_sf)) > 0)

# 3. Public reserves
car_sf_filtered$intersects_pub_res =
  as.integer(lengths(sf::st_intersects(car_sf_filtered, pub_res_sf)) > 0)

# 4. RPPN
car_sf_filtered$intersects_rppn =
  as.integer(lengths(sf::st_intersects(car_sf_filtered, sf::st_as_sf(rppn))) > 0)

# 5. RL
car_sf_filtered$intersects_rl =
  as.integer(lengths(sf::st_intersects(car_sf_filtered, sf::st_as_sf(rl))) > 0)


#### 7. Coverage --------
## Prepare rasters
apa_mld_r = terra::rasterize(apa_mld, template_rast, field = 1, background = NA)
pub_res_r = terra::rasterize(pub_res_sf, template_rast, field = 1, background = NA)
rppn_r = terra::rasterize(rppn, template_rast, field = 1, background = NA)
rl_r = terra::rasterize(rl, template_rast, field = 1, background = NA)
plot(apa_mld_r)
plot(pub_res_r)
plot(rppn_r)
plot(rl_r)

## Compute coverage
# 1. APA
car_sf_filtered$apa_cover = exact_extract(apa_mld_r, car_sf_filtered, 'count')

# 2. Public reserves
car_sf_filtered$pub_res_cover = exact_extract(pub_res_r, car_sf_filtered, 'count')

# 3. RPPN
car_sf_filtered$rppn_cover = exact_extract(rppn_r, car_sf_filtered, 'count')

# 4. RL
car_sf_filtered$rl_cover = exact_extract(rl_r, car_sf_filtered, 'count')

## Surface
car_sf_filtered = car_sf_filtered %>% 
  dplyr::mutate(apa_cover_ha = round(apa_cover * px_area_ha, 2),
                pub_res_cover_ha = round(pub_res_cover * px_area_ha, 2),
                rppn_cover_ha = round(rppn_cover * px_area_ha, 2),
                rl_cover_ha = round(rl_cover * px_area_ha, 2),
                
                prop_apa_cover = ifelse(apa_cover_ha > 0, apa_cover_ha / car_area_ha, 0),
                prop_pub_res_cover = ifelse(pub_res_cover_ha > 0, pub_res_cover_ha / car_area_ha, 0),
                prop_rppn_cover = ifelse(rppn_cover_ha > 0, rppn_cover_ha / car_area_ha, 0),
                prop_rl_cover = ifelse(rl_cover_ha > 0, rl_cover_ha / car_area_ha, 0))

## Remove variables
car_sf_filtered = car_sf_filtered %>% 
  dplyr::select(-c(apa_cover, pub_res_cover, rppn_cover, rl_cover))

#### 8. Slope, elevation and climatic variables --------
# Slope
car_sf_filtered$slope_mean = exact_extract(slope_r, car_sf_filtered, 'mean')
car_sf_filtered$slope_sd = exact_extract(slope_r, car_sf_filtered, 'stdev')
car_sf_filtered$slope_cv = exact_extract(slope_r, car_sf_filtered, 'coefficient_of_variation')

# Elevation
car_sf_filtered$alt_mean = exact_extract(topo_r, car_sf_filtered, 'mean')
car_sf_filtered$alt_sd = exact_extract(topo_r, car_sf_filtered, 'stdev')
car_sf_filtered$alt_cv = exact_extract(topo_r, car_sf_filtered, 'coefficient_of_variation')

# Precipitations
car_sf_filtered$prec_2024_mean = exact_extract(rasters_prec_sum[[36]], car_sf_filtered, 'mean')
car_sf_filtered$prec_2024_sd = exact_extract(rasters_prec_sum[[36]], car_sf_filtered, 'stdev')
car_sf_filtered$prec_2024_cv = exact_extract(rasters_prec_sum[[36]], car_sf_filtered, 'coefficient_of_variation')

# Tmin
car_sf_filtered$tmin_2024_mean = exact_extract(rasters_cumul_tmin_mean[[36]], car_sf_filtered, 'mean')
car_sf_filtered$tmin_2024_sd = exact_extract(rasters_cumul_tmin_mean[[36]], car_sf_filtered, 'stdev')
car_sf_filtered$tmin_2024_cv = exact_extract(rasters_cumul_tmin_mean[[36]], car_sf_filtered, 'coefficient_of_variation')

# Tmax
car_sf_filtered$tmax_2024_mean = exact_extract(rasters_cumul_tmax_mean[[36]], car_sf_filtered, 'mean')
car_sf_filtered$tmax_2024_sd = exact_extract(rasters_cumul_tmax_mean[[36]], car_sf_filtered, 'stdev')
car_sf_filtered$tmax_2024_cv = exact_extract(rasters_cumul_tmax_mean[[36]], car_sf_filtered, 'coefficient_of_variation')


#### 9 North and South of BR-101 ----------
# Sample regular points along BR-101
br101_sp = as_Spatial(br101)
pts_br101 = sp::spsample(br101_sp, n = 1000, type = "regular")
pts_br101_sf = sf::st_as_sf(pts_br101)

coords_br = sf::st_coordinates(pts_br101_sf)
br_x = coords_br[, 1]
br_y = coords_br[, 2]

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

# CAR centroids
car_centroids_sf = sf::st_centroid(car_sf_filtered)
coords_car = sf::st_coordinates(car_centroids_sf)
car_x = coords_car[, 1]
car_y = coords_car[, 2]

# Function
car_sf_filtered$ns_br101 =
  mapply(
    compute_ns,
    x0 = car_x,
    y0 = car_y,
    MoreArgs = list(br_x = br_x, br_y = br_y)
  )

# Plot
plot(st_geometry(br101), col = "black", lwd = 2)
plot(st_geometry(car_sf_filtered[car_sf_filtered$ns_br101 == "North", ]),
     col = "blue", pch = 16, add = TRUE)
plot(st_geometry(car_sf_filtered[car_sf_filtered$ns_br101 == "South", ]),
     col = "red", pch = 16, add = TRUE)

#### Export datasets ----------
saveRDS(car_sf_filtered,
        here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.rds"))
sf::write_sf(
  car_sf_filtered,
  here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.gpkg"))
