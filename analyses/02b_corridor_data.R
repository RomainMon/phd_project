#------------------------------------------------#
# Author: Romain Monassier
# Objective: Create a dataset on corridor location
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(terra)
library(sf)
library(ggplot2)

### Import datasets -----
#### Road overpasses and AMLD corridors -------
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass_cons_cat")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years_cons = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df_cons = data.frame(file = raster_files, years_cons = as.numeric(years_cons)) %>%
  dplyr::arrange(years_cons)
# Load rasters in chronological order
rasters_reclass_cons_cat = lapply(raster_df_cons$file, terra::rast)
years_cons = raster_df_cons$years_cons
# Check
for (i in seq_along(rasters_reclass_cons_cat)) {
  cat("Year", years_cons[i], " → raster name:", basename(raster_df_cons$file[i]), "\n")
}
plot(rasters_reclass_cons_cat[[21]], col = c("#FF7F00", "#EE30A7", "darkred"))
names(rasters_reclass_cons_cat) = raster_df_cons$years_cons # Name by year

#### MSPA corridors  -------
base_path = here("outputs", "data", "MapBiomas", "MSPA", "reclass_w_mspa")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_mspa = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_mspa)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
names(rasters_mspa) = raster_df$year # Name by year

#### Patches  -------
base_path = here("outputs", "data", "patches")
vect_files = list.files(base_path, pattern = "\\.gpkg$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(vect_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
vector_df = data.frame(file = vect_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load vectors in chronological order
patches = lapply(vector_df$file, sf::st_read)
years = vector_df$year
# Check
for (i in seq_along(patches)) {
  cat("Year", years[i], " → raster name:", basename(vector_df$file[i]), "\n")
}
plot(rasters_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(patches[[36]]), col=NA, add=TRUE)
names(patches) = vector_df$year # Name by year

#### GLT locations --------
regions = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))

#### AMLD corridors --------
amld_corridors = sf::st_read(here("data", "geo", "AMLD", "plantios", "work", "plantios_clean.shp"))
amld_corridors = amld_corridors %>% 
  dplyr::filter(Ecologia == "Corredor" & !is.na(date_refor))

#### Road overpasses --------
road_overpass = sf::st_read(here("data", "geo", "AMLD", "Passagens_ARTERIS", "work", "road_overpasses_clean.shp"))

### Flag connected patches ---------
# -> Here, we identify corridors connecting two distinct patches
# Steps: 
# 1) Transform corridors to vectors
# 2) For a given year, identify the corridors touching/intersecting >1 patches
# 3) Join patches id to these corridors

#### Example -----
# # Patches 2024
# patches_2024 = patches[[36]] %>% 
#   dplyr::rename(patch_id = lyr.1) %>% 
#   dplyr::mutate(patch_id = as.character(patch_id))
# 
# 
# # Load corridor raster and convert to vector (polygons)
# corridor_raster = rasters_mspa[[36]]
# corridor_raster[corridor_raster != 33] = NA
# corridor_polygons = landscapemetrics::get_patches(corridor_raster, class = 33, directions = 8)
# corridor_sf = sf::st_as_sf(as.polygons(corridor_polygons[[1]][[1]], dissolve = TRUE)) %>% 
#   dplyr::rename(corridor_id = lyr.1)  # Assign unique corridor IDs
# plot(sf::st_geometry(corridor_sf))
# 
# # Small buffer to avoid topology issues
# corridor_sf_buff = sf::st_buffer(corridor_sf, 1)
# 
# # Assign patch_id to corridors that intersect patches
# connections = corridor_sf_buff %>%
#   sf::st_join(
#     patches_2024 %>% dplyr::select(patch_id),
#     join = st_intersects,  # Includes both intersection and touching
#     left = FALSE  # Keep only patches that intersect/touch corridors
#   ) %>% 
#   sf::st_drop_geometry()
# 
# # Keep corridors connecting >1 patch
# connections_long = connections %>% 
#   dplyr::group_by(corridor_id) %>% 
#   dplyr::filter(dplyr::n() > 1) %>%
#   dplyr::ungroup()
# 
# # Join back corridor_id to patch data
# # Create pairwise patch connections (Each patch is linked to all other patches in same corridor)
# patch_connections = connections_long %>%
#   dplyr::inner_join(
#     connections_long %>%
#       dplyr::rename(patch_to = patch_id), by = "corridor_id") %>%
#   dplyr::rename(patch_from = patch_id) %>%
#   dplyr::filter(patch_from != patch_to) %>%
#   dplyr::mutate(pair = purrr::map2_chr(
#       patch_from,
#       patch_to,
#       ~ paste(sort(c(.x, .y)), collapse = "_")
#     )) %>%
#   dplyr::distinct(pair, corridor_id, .keep_all = TRUE) %>%
#   dplyr::select(corridor_id, patch_from, patch_to)
# 
# # 1. Corridor geometries (sf)
# corridor_geometry = corridor_sf %>%
#   dplyr::select(corridor_id, geometry)
# 
# # 2. Long-format connection table (no geometry)
# patch_connections

#### Function to create corridor connections -----
# Returns:
# 1) corridor geometries (sf)
# 2) long-format patch connections (data.frame)
build_corridor_connections = function(patches_list,
                                      raster_list,
                                      corridor_value,
                                      patch_id_col,
                                      corridor_type,
                                      buffer_dist = 5) {
  
  patch_years = as.numeric(names(patches_list))
  raster_years = as.numeric(names(raster_list))
  
  # Outputs
  corridor_geometries = list()
  patch_connections = list()
  
  for (year in patch_years) {
    
    message("Processing year: ", year)
    
    # Patches
    patches_i = patches_list[[as.character(year)]] %>%
      dplyr::rename(patch_id = all_of(patch_id_col)) %>%
      dplyr::mutate(patch_id = as.character(patch_id))
    
    # Skip if raster missing
    if (!year %in% raster_years) next
    
    # Raster
    r = raster_list[[as.character(year)]]
    r[r != corridor_value] = NA
    
    # Skip empty rasters
    if (all(is.na(terra::values(r)))) next
    
    # Corridor polygons
    r_patch = landscapemetrics::get_patches(
      r,
      class = corridor_value,
      directions = 8
    )
    
    corridor_sf = terra::as.polygons(
      r_patch[[1]][[1]],
      dissolve = TRUE
    ) %>%
      sf::st_as_sf() %>%
      dplyr::rename(
        corridor_id = lyr.1
      ) %>%
      dplyr::mutate(
        corridor_id = paste0(corridor_type, "_", year, "_", corridor_id),
        year = year
      )
    
    # Save corridor geometries
    corridor_geometries[[as.character(year)]] = corridor_sf %>%
      dplyr::select(corridor_id, year, geometry)
    
    # Small buffer
    corridor_sf_buff = sf::st_buffer(corridor_sf, buffer_dist)
    
    # Corridor-patch intersections
    connections = corridor_sf_buff %>%
      sf::st_join(
        patches_i %>%
          dplyr::select(patch_id),
        join = sf::st_intersects,
        left = FALSE
      ) %>%
      sf::st_drop_geometry()
    
    # Keep only corridors connected to >1 patch
    connections_long = connections %>%
      dplyr::group_by(corridor_id) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::ungroup()
    
    # Skip empty years
    if (nrow(connections_long) == 0) next
    
    # Pairwise patch connections
    edges = connections_long %>%
      dplyr::inner_join(
        connections_long %>%
          dplyr::rename(
            patch_to = patch_id
          ),
        by = "corridor_id",
        relationship = "many-to-many"
      ) %>%
      dplyr::rename(
        patch_from = patch_id
      ) %>%
      dplyr::filter(
        patch_from != patch_to
      ) %>%
      dplyr::mutate(
        year = year,
        corridor_type = corridor_type
      ) %>%
      dplyr::distinct(
        corridor_id,
        patch_from,
        patch_to,
        .keep_all = TRUE
      ) %>%
      dplyr::select(
        corridor_id,
        corridor_type,
        year,
        patch_from,
        patch_to
      )
    
    # Save long-format connection table
    patch_connections[[as.character(year)]] = edges
  }
  
  # Merge outputs across years
  corridor_geometries_all = dplyr::bind_rows(
    corridor_geometries
  )
  patch_connections_all = dplyr::bind_rows(
    patch_connections
  )
  
  return(
    list(
      corridor_geometry = corridor_geometries_all,
      patch_connections = patch_connections_all
    )
  )
}

# Build all corridor types
# MSPA
corridors_mspa = build_corridor_connections(
  patches_list = patches,
  raster_list = rasters_mspa,
  corridor_value = 33,
  patch_id_col = "patch_id",
  corridor_type = "MSPA"
)

# Road overpasses
corridors_road = build_corridor_connections(
  patches_list = patches,
  raster_list = rasters_reclass_cons_cat,
  corridor_value = 52,
  patch_id_col = "patch_id",
  corridor_type = "ROAD"
)

# Merge all corridor geometries
corridor_geometry_all = dplyr::bind_rows(
  corridors_mspa$corridor_geometry,
  corridors_road$corridor_geometry
)

# Merge all patch connections
patch_connections_all = dplyr::bind_rows(
  corridors_mspa$patch_connections,
  corridors_road$patch_connections
)

### Mutate corridor type -----
# Initialize
corridor_geometry_all = corridor_geometry_all %>%
  dplyr::mutate(corridor_type = "MSPA")

# Road overpass
road_idx = lengths(
  sf::st_intersects(corridor_geometry_all, road_overpass)
) > 0

corridor_geometry_all$corridor_type[road_idx] = "road_overpass"

# AMLD corridor
amld_idx = lengths(
  sf::st_intersects(corridor_geometry_all, amld_corridors)
) > 0

corridor_geometry_all$corridor_type[
  !road_idx & amld_idx
] = "amld_corr"

# Propagate to patch connections
patch_connections_all = patch_connections_all %>%
  dplyr::select(-corridor_type) %>%
  dplyr::left_join(
    corridor_geometry_all %>%
      sf::st_drop_geometry() %>%
      dplyr::select(corridor_id, corridor_type),
    by = "corridor_id"
  )

### Export --------

### Export corridor geometries by year
base_path = here("outputs", "data", "corridor")
# Split by year
corridor_geometry_years = split(
  corridor_geometry_all,
  corridor_geometry_all$year
)

# Export one geopackage per year
purrr::iwalk(
  corridor_geometry_years,
  function(x, y) {
    
    file_name = paste0(
      "corridor_",
      y,
      ".gpkg"
    )
    
    file_path = file.path(
      base_path,
      file_name
    )
    
    sf::st_write(
      x,
      file_path,
      delete_dsn = TRUE,
      quiet = TRUE
    )
    
    message("Exported: ", file_name)
  }
)

### Export patches connections
readr::write_csv(
  patch_connections_all,
  here(
    "outputs",
    "data",
    "corridor",
    "patch_connections_all.csv"
  ))
