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
library(multilandr)

### Import rasters -------
## Land use
base_path = here("outputs", "data", "MapBiomas", "Rasters_cumulative_tm")
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
plot(rasters[[35]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "lightgreen", "pink"))

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

#### Polygonize land use -------
# Select 2023
lulc_2023 = rasters[[35]]
lulc_2023[!(lulc_2023 %in% c(1,6,7))] = NA # Put all values different to NA
plot(lulc_2023)

# A sample
# sample 2% of non-NA cells
cells = which(!is.na(lulc_2023[]))
sample_cells = sample(cells, size = round(length(cells) * 0.02))

# mask everything except sampled cells
lulc_sample = lulc_2023
lulc_sample[-sample_cells] = NA

# polygonize
lulc_2023_poly = terra::as.polygons(lulc_sample, aggregate=FALSE, values=TRUE, na.rm=TRUE) # If aggregate=TRUE, cells are dissolved based on their value and proximity
plot(lulc_2023_poly)

# to sf
lulc_2023_sf = sf::st_as_sf(lulc_2023_poly)

# Make sure polygons have an ID
lulc_2023_sf = lulc_2023_sf %>% dplyr::mutate(cell_id = row_number())

#### Determine legal status for each raster cell -----

##### 1st classification -----
## FIRST, we determine whether cells are public (reserve), private (car), or urban

# Ensure all layers share the same CRS
stopifnot(st_crs(lulc_2023_sf) == sf::st_crs(car_sf))
stopifnot(st_crs(uniao_sf) == sf::st_crs(pda_sf))
stopifnot(sf::st_crs(urb_sf) == sf::st_crs(car_sf))

# Combine public reserves into one layer
public_reserves_sf = dplyr::bind_rows(
  uniao_sf %>% dplyr::mutate(source = "uniao"),
  pda_sf   %>% dplyr::mutate(source = "pda")
)

# Intersection with CAR (private)
inter_car = sf::st_intersection(
  lulc_2023_sf %>% dplyr::select(cell_id, geometry),
  car_sf %>% dplyr::select(geometry)
) %>%
  dplyr::mutate(
    area_overlap = as.numeric(sf::st_area(geometry)),
    legal_status = "private"
  )

# Intersection with public reserves (uniao + pda)
inter_public = sf::st_intersection(
  lulc_2023_sf %>% dplyr::select(cell_id, geometry),
  public_reserves_sf %>% dplyr::select(geometry, source)
) %>%
  dplyr::mutate(
    area_overlap = as.numeric(sf::st_area(geometry)),
    legal_status = "public_reserve"
  )

# Intersection with urban centers (urb)
inter_urban = sf::st_intersection(
  lulc_2023_sf %>% dplyr::select(cell_id, geometry),
  urb_sf %>% dplyr::select(geometry)
) %>%
  dplyr::mutate(
    area_overlap = as.numeric(sf::st_area(geometry)),
    legal_status = "urban"
  )

# Combine all intersection results
inter_all = dplyr::bind_rows(inter_car, inter_public, inter_urban)

# For cells that intersect multiple zones, keep the one with the largest overlap or private if it intersects "private"
legal_status_df = inter_all %>%
  dplyr::group_by(cell_id) %>%
  dplyr::slice_max(order_by = area_overlap, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    legal_status = ifelse(cell_id %in% inter_car$cell_id, 
                          "private", 
                          legal_status)
  ) %>%
  dplyr::select(cell_id, legal_status)

# Join back to raster cell polygons
lulc_2023_sf = lulc_2023_sf %>%
  dplyr::left_join(sf::st_drop_geometry(legal_status_df), by = "cell_id")

# Replace NAs by "none" if some cells don’t intersect anything
lulc_2023_sf = lulc_2023_sf %>%
  dplyr::mutate(legal_status = tidyr::replace_na(legal_status, "none"))

# Quick check
table(lulc_2023_sf$legal_status)

##### 2nd classification  ------
### reserve ; urban ; private_reserve ; private_no_status

# TRUE/FALSE vector: does each cell intersect at least 1 RL polygon?
hits = sf::st_intersects(lulc_2023_sf, rl_sf)
lulc_2023_sf$rl = lengths(hits) > 0

# now the new legal_status2 rules
lulc_2023_sf = lulc_2023_sf %>%
  dplyr::mutate(
    legal_status2 =
      dplyr::case_when(
        legal_status == "public_reserve" ~ "public_reserve",
        legal_status == "urban"          ~ "urban",
        rl %in% TRUE                     ~ "private_reserve",
        legal_status == "private"        ~ "private_no_status",
        TRUE                             ~ "none"
      )
  )

# Quick check
table(lulc_2023_sf$legal_status)
table(lulc_2023_sf$legal_status2)

##### Intersects APA -----
# intersection
inter_apa = sf::st_intersects(
  lulc_2023_sf %>% dplyr::select(cell_id, geometry),
  apa_mld_sf %>% dplyr::select(geometry),
  sparse = FALSE
)

lulc_2023_sf = lulc_2023_sf %>%
  dplyr::mutate(apa_mld = ifelse(apply(inter_apa, 1, any), 1, 0))

#### Distance to features -------

##### Centroids ---------
# compute pixel centroids
lulc_centroids = sf::st_centroid(lulc_2023_sf)

##### Distance computation -----
# roads: distance to nearest road
d_road = sf::st_distance(lulc_centroids, roads_sf)
dist_to_road = apply(d_road, 1, min)

# rivers: distance to nearest river
d_water = sf::st_distance(lulc_centroids, rivers_sf)
dist_to_water = apply(d_water, 1, min)

# urban: distance to nearest urban centroid
d_urban = sf::st_distance(lulc_centroids, urb_centers_sf)
dist_to_urban = apply(d_urban, 1, min)

# join distances
# put distances in a data.frame with cell_id
dist_df = data.frame(
  cell_id = lulc_2023_sf$cell_id,
  dist_road_m  = as.numeric(dist_to_road),
  dist_water_m = as.numeric(dist_to_water),
  dist_urban_m = as.numeric(dist_to_urban)
)

# now join by cell_id
lulc_2023_sf = lulc_2023_sf %>%
  dplyr::left_join(dist_df, by = "cell_id")

#### Distance to forest edges -------
# forest mask
forest_2023 = rasters[[35]] == 1
forest_2023[rasters[[35]] != 1] = NA
plot(forest_2023)

# compute patches (raster -> list of rasters, one per patch group)
p = landscapemetrics::get_patches(forest_2023, class = 1, directions = 8)

# convert them to a single terra SpatVector
patch_raster = p[[1]][[1]]  # layer_1$class_1 equivalent
patch_raster[is.na(patch_raster)] = NA

# extract edges
edge = landscapemetrics::get_boundaries(patch_raster, as_NA = TRUE)
plot(edge[[1]], col="lightpink")
edge_raster = edge[[1]]

# Polygonize edges (dissolve identical patch IDs)
edge_vect = terra::as.polygons(edge_raster, dissolve = TRUE, na.rm = TRUE)

# Convert to sf
edge_sf = sf::st_as_sf(edge_vect)

# distance to forest edges
d_edge = sf::st_distance(lulc_centroids, edge_sf)
dist_to_edge = apply(d_edge, 1, min)  # nearest per pixel

# join back
dist_edge_df = data.frame(
  cell_id = lulc_2023_sf$cell_id,
  dist_edge_m = as.numeric(dist_to_edge)
)

lulc_2023_sf = lulc_2023_sf %>%
  dplyr::left_join(dist_edge_df, by = "cell_id")

#### Slope -----
# extract mean slope inside each cell polygon
mean_slope = terra::extract(slope, lulc_2023_poly, fun = mean, na.rm = TRUE)
mean_slope = mean_slope %>%
  dplyr::rename(mean_slope = slope_bbox) %>%
  dplyr::mutate(cell_id = lulc_2023_sf$cell_id)

# join back
lulc_2023_sf = lulc_2023_sf %>%
  dplyr::left_join(mean_slope %>% dplyr::select(cell_id, mean_slope), by = "cell_id")

#### Land use in buffers -----

##### With multilandr ------
# 1. Create multiland object
# Rasters and points objects
r2023 = rasters[[35]] # SpatRaster object
points = terra::vect(lulc_centroids)
points = sample(points, 100)

# Class names
cl_names = c(1, "Forest",
             2, "Notforest",
             3, "Agriculture",
             4, "Water",
             5, "Urban",
             6, "Reforested",
             7, "Deforested") 

# Multiland object
buff_centroids = mland(points_layer = points,
                  rast_layer = r2023,
                  radii = 1000,
                  class_names = list(cl_names),
                  site_ref = "cell_id",
                  on_the_fly = TRUE,
                  bufftype = "round",
                  progress = TRUE)

# 2. Plot Multiland object
mland_plot(buff_centroids, points=10) # plot an example

# 3. Calculation of metrics
ml_metrics = mland_metrics(buff_centroids, 
                            level = "class", 
                            metric = c("pland"),
                            absence_values = list("pland" = 0))
ml_metrics
head(ml_metrics@data)

# 4. Join back to dataset
# clean df of metrics
ml_df = ml_metrics@data %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(cell_id = site) %>% 
  dplyr::select(cell_id, classname, value) %>% 
  tidyr::pivot_wider(names_from = classname,
              values_from = value,
              names_prefix = "pland_")

# join
lulc_2023_sf = lulc_2023_sf %>% 
  dplyr::left_join(ml_df, by = "cell_id")

# quick check
### set 1 random point to inspect
set.seed(42)
check1 = lulc_2023_sf2 %>% 
  dplyr::filter(!is.na(pland_Forest)) %>%
  dplyr::slice_sample(n=1)

### 1 km buffer
buff = sf::st_buffer(check1, dist = 1000)

### crop raster around the buffer (slightly bigger so you see context)
r_zoom = terra::crop(r2023, vect(sf::st_buffer(buff, 200)))

cols = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink")
plot(r_zoom, col=cols, legend=TRUE, main="MapBiomas 2023, 1 km QA/QC")

plot(buff$geometry, border="black", lwd=2, add=TRUE)
plot(check1$geometry, pch=19, col="black", cex=1, add=TRUE)

### add PLAND values
txt = paste0(
  "cell_id = ", check1$cell_id, "\n",
  "Forest = ", round(check1$pland_Forest,1), "%\n",
  "Notforest = ", round(check1$pland_Notforest, 1), "%\n",
  "Agriculture = ", round(check1$pland_Agriculture,1), "%\n",
  "Water = ", round(check1$pland_Water,1), "%\n",
  "Urban = ", round(check1$pland_Urban,1), "%\n",
  "Reforested = ", round(check1$pland_Reforested,1), "%\n",
  "Deforested = ", round(check1$pland_Deforested,1), "%"
)

legend("top", legend=txt, bty="n")


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
