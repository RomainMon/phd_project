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
base_path = here("outputs", "data", "patchmetrics")
vect_files = list.files(base_path, pattern = "\\.gpkg$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(vect_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
vector_df = data.frame(file = vect_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
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

### Flag connected patches
# -> Here, we identify corridors connecting two distinct patches
# Steps: 
# 1) Transform corridors to vectors
# 2) For a given year, identify the corridors touching/intersecting >1 patches
# 3) Join patches id to these corridors

### Corridor-patch (example) -----
# Patches 2024
patches_2024 = patches[[36]] %>% 
  dplyr::rename(patch_id = lyr.1)

# Load corridor raster and convert to vector (polygons)
corridor_raster = rasters_mspa[[36]]
corridor_raster[corridor_raster != 33] = NA
plot(corridor_raster)
corridor_polygons = landscapemetrics::get_patches(corridor_raster, class = 33, directions = 8)
corridor_sf = sf::st_as_sf(as.polygons(corridor_polygons[[1]][[1]], dissolve = TRUE)) %>% 
  dplyr::rename(corridor_id = lyr.1)  # Assign unique corridor IDs
plot(sf::st_geometry(corridor_sf))

# Assign patch_id to corridors that intersect patches
connections = corridor_sf %>%
  sf::st_join(
    patches_2024 %>% dplyr::select(patch_id),
    join = st_intersects,  # Includes both intersection and touching
    left = FALSE  # Keep only patches that intersect/touch corridors
  )

# Keep corridors connecting >1 patch
connections_long = connections %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(corridor_id) %>% 
  dplyr::filter(dplyr::n() > 1) 

# Join back corridor_id to patch data
# Create pairwise patch connections (Each patch is linked to all other patches in same corridor)
patch_corridor_2024 = connections_long %>%
  dplyr::inner_join(
    connections_long %>% dplyr::rename(connected_to = patch_id),
    by = "corridor_id"
  ) %>%
  dplyr::filter(patch_id != connected_to) %>% 
  dplyr::distinct(patch_id, corridor_id, connected_to)

# Collapse to patch level
patch_summary = patch_corridor_2024 %>%
  dplyr::group_by(patch_id) %>%
  dplyr::summarise(
    connected_to = paste(sort(unique(connected_to)), collapse = ";"),
    corridor = paste(sort(unique(corridor_id)), collapse = ";"),
    reconnected = 1,
    .groups = "drop"
  )

# Join back to patches
patch_corridor_2024_final = patches_2024 %>%
  dplyr::left_join(patch_summary, by = "patch_id") %>%
  dplyr::mutate(
    reconnected = ifelse(is.na(reconnected), 0, reconnected)
  )

### Function to create corridor connections -----
build_corridor_connections = function(patches_list,
                                      raster_list,
                                      corridor_value,
                                      patch_id_col) {
  
  patch_years  = as.numeric(names(patches_list))
  raster_years = as.numeric(names(raster_list))
  
  out_list = list()
  
  for (year in patch_years) {
    
    message("Processing year: ", year)
    
    patches_i = patches_list[[as.character(year)]] %>%
      dplyr::rename(patch_id = all_of(patch_id_col)) %>%
      dplyr::mutate(patch_id = as.character(patch_id))
    
    if (!year %in% raster_years) next
    
    r = raster_list[[as.character(year)]]
    r[r != corridor_value] = NA
    
    if (all(is.na(terra::values(r)))) next
    
    # Convert to polygons
    r_patch = landscapemetrics::get_patches(r, class = corridor_value, directions = 8)
    
    corridor_sf = terra::as.polygons(r_patch[[1]][[1]], dissolve = TRUE) %>%
      sf::st_as_sf() %>%
      dplyr::rename(corridor_id = lyr.1)
    
    # Intersections
    connections = sf::st_join(
      corridor_sf,
      patches_i %>% dplyr::select(patch_id),
      join = sf::st_intersects,
      left = FALSE
    )
    
    connections_long = connections %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(corridor_id) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::ungroup()
    
    if (nrow(connections_long) == 0) next
    
    # Keep edges instead of summarising
    edges = connections_long %>%
      dplyr::inner_join(
        connections_long %>% dplyr::rename(patch_to = patch_id),
        by = "corridor_id"
      ) %>%
      dplyr::rename(patch_from = patch_id) %>%
      dplyr::filter(patch_from != patch_to) %>%
      dplyr::distinct(patch_from, patch_to, corridor_id) %>%
      dplyr::mutate(year = year)
    
    # Remove duplicate geometries
    edges = edges %>%
      dplyr::mutate(
        pair = purrr::map2_chr(patch_from, patch_to, ~ paste(sort(c(.x, .y)), collapse = "_"))
      ) %>%
      dplyr::distinct(pair, corridor_id, .keep_all = TRUE) %>%
      dplyr::select(-pair)
    
    # Attach corridor geometry
    edges_sf = edges %>%
      dplyr::left_join(corridor_sf, by = "corridor_id") %>%
      sf::st_as_sf()
    
    out_list[[as.character(year)]] = edges_sf
  }
  return(out_list)
}

# With MSPA rasters
corridors_mspa = build_corridor_connections(
  patches_list = patches,
  raster_list  = rasters_mspa,
  corridor_value = 33,
  patch_id_col = "lyr.1"
)

# With AMLD corridors
corridors_amld = build_corridor_connections(
  patches_list = patches,
  raster_list  = rasters_reclass_cons_cat,
  corridor_value = 51,
  patch_id_col = "lyr.1"
)

# With road overpasses
corridors_road = build_corridor_connections(
  patches_list = patches,
  raster_list  = rasters_reclass_cons_cat,
  corridor_value = 52,
  patch_id_col = "lyr.1"
)

# Bind corridors by year
corridors_all = lapply(names(patches), function(y) {
  # Extract each for the given year
  mspa = corridors_mspa[[y]]
  amld = corridors_amld[[y]]
  road = corridors_road[[y]]
  
  # Helper to safely add type
  add_type = function(x, type_name) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x %>%
      dplyr::mutate(
        year = as.numeric(y),
        type = type_name
      )
  }
  # Bind all types
  dplyr::bind_rows(
    add_type(mspa, "MSPA"),
    add_type(amld, "AMLD"),
    add_type(road, "road")
  )
})

# Name list by year
names(corridors_all) = names(patches)
corridors_all[["2015"]]

### Export --------
base_path = here("outputs", "data", "corridor")
for (year in names(corridors_all)) {
  
  file_name = paste0("corridor_", year, ".gpkg")
  file_path = file.path(base_path, file_name)
  
  sf::st_write(
    corridors_all[[year]],
    file_path,
    append = FALSE,
    delete_dsn = TRUE,
    quiet = TRUE
  )
}
