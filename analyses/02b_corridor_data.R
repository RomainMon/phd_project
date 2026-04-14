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

#### GLT locations --------
regions = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))

### Flag connected patches
# -> Here, we identify corridors connecting two distinct patches
# Steps: 
# 1) Transform corridors to vectors
# 2) For a given year, identify the corridors touching/intersecting >1 patches
# 3) Join patches id to these corridors
# 4) Append back to patch data (for a given year) corridor id, and connected patch id

### Example -----
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

### Function -----
build_patch_connectivity = function(patches_list,
                                    raster_list,
                                    corridor_value,
                                    patch_id_col,
                                    type) {
  
  # 1. Define output column names (dynamic by type)
  # These columns will store:
  # - whether a patch is reconnected
  # - which patches it connects to
  # - which corridor IDs are involved
  
  col_reconnected   = paste0("reconnec_", type)
  col_connected_to  = paste0("connec_to_", type)
  col_corridor      = paste0("corr_", type)
  
  # 2. Extract available years from inputs
  patch_years  = as.numeric(names(patches_list))
  raster_years = as.numeric(names(raster_list))
  
  # Initialize output list (one element per year)
  out_list = list()
  
  # 3. Loop over each year
  for (year in patch_years) {
    
    message("Processing year: ", year)
    
    # 3.1 Prepare patch dataset for this year
    # - Standardize patch ID column name
    # - Convert IDs to character for safe joins
    
    patches_i = patches_list[[as.character(year)]] %>%
      dplyr::rename(patch_id = all_of(patch_id_col)) %>%
      dplyr::mutate(patch_id = as.character(patch_id))
    
    # 3.2 Skip if no raster exists for this year
    if (!year %in% raster_years) {
      patches_i[[col_reconnected]]  = 0
      patches_i[[col_connected_to]] = NA
      patches_i[[col_corridor]]     = NA
      
      out_list[[as.character(year)]] = patches_i
      next
    }
    
    # 3.3 Select and filter raster (corridor class)
    # Keep only pixels corresponding to corridor_value
    
    r = raster_list[[as.character(year)]]
    r[r != corridor_value] = NA
    
    # If no corridor pixels remain → skip
    if (all(is.na(terra::values(r)))) {
      patches_i[[col_reconnected]]  = 0
      patches_i[[col_connected_to]] = NA
      patches_i[[col_corridor]]     = NA
      
      out_list[[as.character(year)]] = patches_i
      next
    }
    
    # 3.4 Convert raster corridors to polygons
    # Identify contiguous corridor patches (8-neighbour connectivity)
    
    r_bin   = !is.na(r)
    r_patch = landscapemetrics::get_patches(r, class = corridor_value, directions = 8)
    
    corridor_sf = terra::as.polygons(r_patch[[1]][[1]], dissolve = TRUE) %>%
      sf::st_as_sf() %>%
      dplyr::rename(corridor_id = lyr.1)
    
    # 3.5 Identify intersections between corridors and patches
    # Each corridor polygon is linked to intersecting patches
    
    connections = sf::st_join(
      corridor_sf,
      patches_i %>% dplyr::select(patch_id),
      join = sf::st_intersects,
      left = FALSE
    )
    
    # 3.6 Keep only corridors connecting ≥ 2 patches
    connections_long = connections %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(corridor_id) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::ungroup()
    
    # 3.7 Build patch-to-patch connections
    if (nrow(connections_long) > 0) {
      
      # Create pairwise connections between patches sharing a corridor
      edges = connections_long %>%
        dplyr::inner_join(
          connections_long %>% dplyr::rename(connected_to = patch_id),
          by = "corridor_id"
        ) %>%
        dplyr::filter(patch_id != connected_to) %>%
        dplyr::distinct()
      
      # Summarize connections at patch level
      patch_summary = edges %>%
        dplyr::group_by(patch_id) %>%
        dplyr::summarise(
          !!col_connected_to := paste(unique(connected_to), collapse = ";"),
          !!col_corridor     := paste(unique(corridor_id), collapse = ";"),
          !!col_reconnected  := 1,
          .groups = "drop"
        )
      
    } else {
      
      # No valid connections → create empty summary
      patch_summary = patches_i %>%
        sf::st_drop_geometry() %>%
        dplyr::select(patch_id) %>%
        dplyr::mutate(
          !!col_connected_to := NA_character_,
          !!col_corridor     := NA_character_,
          !!col_reconnected  := 0
        )
    }
    
    # 3.8 Join connectivity info back to patches
    patches_out = patches_i %>%
      dplyr::left_join(patch_summary, by = "patch_id")
    
    # Replace NA in reconnected column with 0
    patches_out[[col_reconnected]] = ifelse(
      is.na(patches_out[[col_reconnected]]),
      0,
      patches_out[[col_reconnected]]
    )
    
    # Store result
    out_list[[as.character(year)]] = patches_out
  }
  
  # 4. Return list of patch datasets
  return(out_list)
}

# Name your lists by year
names(patches) = vector_df$year
names(rasters_mspa) = raster_df$year
names(rasters_reclass_cons_cat) = raster_df_cons$years_cons

# Run the function with MSPA corridors
patches_mspa = build_patch_connectivity(
  patches_list = patches,
  raster_list = rasters_mspa,
  corridor_value = 33,
  patch_id_col = "lyr.1",
  type = "mspa"
)

# With road overpasses
patches_overpass = build_patch_connectivity(
  patches_list = patches,
  raster_list = rasters_reclass_cons_cat,
  corridor_value = 52,
  patch_id_col = "lyr.1",
  type = "overpass"
)

# With AMLD corridors
patches_amld = build_patch_connectivity(
  patches_list = patches,
  raster_list = rasters_reclass_cons_cat,
  corridor_value = 51,
  patch_id_col = "lyr.1",
  type = "amld"
)

# Bind patches
patches_all = lapply(names(patches_mspa), function(year) {
  
  mspa = patches_mspa[[year]]
  over = patches_overpass[[year]] %>% sf::st_drop_geometry() %>% dplyr::select(c(patch_id, reconnec_overpass, connec_to_overpass, corr_overpass))
  amld = patches_amld[[year]] %>% sf::st_drop_geometry() %>% dplyr::select(c(patch_id, reconnec_amld, connec_to_amld, corr_amld))
  
  out = mspa %>%
    dplyr::left_join(over, by = "patch_id") %>%
    dplyr::left_join(amld, by = "patch_id")
  
  return(out)
})
names(patches_all) = names(patches_mspa)

# Export
base_path = here("outputs", "data", "patchmetrics")

for (year in names(patches_all)) {
  
  file_name = paste0("patch_corridor_", year, ".gpkg")
  file_path = file.path(base_path, file_name)
  
  sf::st_write(
    patches_all[[year]],
    file_path,
    append = FALSE,
    delete_dsn = TRUE,
    quiet = TRUE
  )
}