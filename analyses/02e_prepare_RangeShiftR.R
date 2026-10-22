#------------------------------------------------#
# Author: Romain Monassier
# Objective: Preparing RangeShiftR data and parameters
#------------------------------------------------#


### Load packages ------
library(dplyr)
library(here)
library(terra)

### Import data -------
#### Rasters reclassified ----
base_path = here("outputs", "data", "landscape_rshifter")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_rshifter = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_rshifter)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
names(rasters_rshifter) = years
plot(rasters_rshifter[['2023']], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "purple","orange"))

#### GLT distribution ----
glt_occ = sf::st_read(here("data", "glt", "JDietz", "glt_distrib_2013_2018_2022.shp"))
plot(glt_occ)
crs(glt_occ)

#### Species long-term monitoring -----
regions = sf::st_read(here::here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
plot(regions)

#### Patches (vector) -------
# These correspond to vector patches created WITHOUT Queru's et al. (2026) patch-cutting procedure
base_path = here("outputs", "data", "patches_rshifter")
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
plot(rasters_rshifter[[35]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "purple","orange"))
plot(sf::st_geometry(patches[[35]]), col=NA, add=TRUE)
names(patches) = vector_df$year # Name by year

#### Patches (raster) -------
# These correspond to raster patches created WITH Queru's et al. (2026) patch-cutting procedure
patches_cut = terra::rast(here("outputs", "data", "patches_cut", 
                               "2023", # CHOOSE THE PATCHES OF THE YEAR OF INTEREST
                               "FinalPatches_2023.tif"))
plot(patches_cut)

#### Stepping stones (raster) -------
# These correspond to raster patches created WITH Queru's et al. (2026) identification of small patches
stepping_stones = terra::rast(here("outputs", "data", "patches_cut", 
                                   "2023", # CHOOSE THE PATCHES OF THE YEAR OF INTEREST
                                   "TooSmallPatches_2023.tif"))
plot(stepping_stones, col="orange")

### Maps -----

#### Landscape --------

##### Reclass rasters (binary) ----
# IMPORTANT : the basis landscape should have integer values 
# WITH MATRIX = 1, NAs = -999
reclass <- function(xx) {
  habitat <- 1
  corridor <- 33
  background <- c(2, 3, 4, 5, 6, 10)
  
  # Make a copy of original values
  v <- xx[]
  
  # Initialize all missing data
  xx[] <- -999
  
  # Assign habitat first using original values
  xx[v == habitat] <- 2
  
  # Assign corridor
  xx[v == corridor] <- 2
  
  # Then assign background
  xx[v %in% background] <- 1
  
  return(xx)
}

## Apply to all rasters
rasters_binary = lapply(rasters_rshifter, reclass)

##### Reclass rasters (all land uses) -----
# IMPORTANT : the basis landscape should have integer values (with matrix = 1, NAs = -999)
reclass <- function(xx) {
  habitat <- 1
  corridor <- 33
  notforest <- 2
  wetlands <- 3
  agri <- 4
  water <- 5
  built <- 6
  high_for <- 10
  
  # Make a copy of original values
  v <- xx[]
  
  # Initialize all missing data
  xx[] <- -999
  
  # Assign habitat first using original values
  xx[v == habitat] <- 2
  
  ## Assign corridor
  xx[v == corridor] <- 3
  
  # Assign matrix land uses
  xx[v == agri] <- 1
  xx[v %in% c(notforest,wetlands,high_for)] <- 4
  xx[v == water] <- 5
  xx[v == built] <- 6
  
  
  return(xx)
}

## Apply to all rasters
rasters_resist = lapply(rasters_rshifter, reclass)


##### Select landscape of interest ------

# CHOOSE THE YEAR OF INTEREST
rast = rasters_resist[['2023']]

# plot(rast, col=c("white","gray","darkgreen")) # Binary raster
plot(rast, col=c("white","#FFFFB2","#32a65e","orange","#035209","#0000FF","#d4271e"))
freq(rast)


##### Overlay stepping stones -----
# Small patches are considered stepping stones
rast[stepping_stones == 1] = 3 # Adjust the value depending on the type of landscape (binary or resistance-based)
freq(rast)


#### Patches --------
# IMPORTANT : patches should have an integer value corresponding to patch id
# Background must be set to 0
# IF NOT: message error "Found Patch NA in valid habitat cell"

##### IF VECTOR -----
### We create a correspondence table to store unique patchs ids with their patches original ids (names, such as Afetiva, etc.

# Create a permanent numeric ID in vector patches
patches = lapply(patches, function(x){
  x %>%
    mutate(unique_id = row_number())
})
anyDuplicated(patches[['2023']]$lyr.1) # Duplicated ids
anyDuplicated(patches[['2023']]$patch_id) # GOOD but not integer value
anyDuplicated(patches[['2023']]$unique_id) # ALL GOOD

# Vector patches
# CHOOSE THE YEAR OF INTEREST
patches_sf = patches[['2023']]

# Correspondence table between patch name and id
patch_corres_id = patches_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(patch_id, unique_id)

### We rasterize patches based on a unique id (numeric value)
# Rasterize
patch_rasters = vector("list", length(patches))
names(patch_rasters) = names(patches)

for (yr in names(patches)) {
  
  cat("Rasterizing patches:", yr, "\n")
  
  # Template raster
  r_template = rasters_rshifter[[yr]]
  
  # Convert to SpatVector
  p = terra::vect(patches[[yr]])
  
  # Rasterize using unique patch IDs
  r_patch = terra::rasterize(
    p,
    r_template,
    field = "unique_id",
    background = 0
  )
  
  # Preserve nodata cells from template
  r_patch[is.na(r_template)] = -999
  
  patch_rasters[[yr]] = r_patch
}

## Raster patches

# CHOOSE THE YEAR OF INTEREST
patches_r = patch_rasters[['2023']]

##### IF RASTER ----
### i.e., Patches are already rasterized (e.g., using Queru et al. 2026 procedure)
### -> We need a correspondence between raster patches and vector patches (e.g., 1087 is in Poço das Antas...)
### raster patch ID → original vector patch ID, based on which vector polygon contains/intersects the raster cells belonging to that raster patch.
### one-to-many correspondence if because one vector patch can contain several raster patches (in the case of previous patch-cutting, Queru et al. 2026)

### Select patches
patches_r = patches_cut
unique(patches_r) # Unique id per patch?

# NA → 0, except where the landscape itself is -999
patches_r[is.na(patches_r) & rast != -999] <- 0

# Preserve true NoData
patches_r[rast == -999] <- -999

# Plot
plot(patches_r)

### Correspondence with vectors (join patch_id)
## Vectorize cut patches
patches_cut_v = terra::as.polygons(
  patches_cut,
  dissolve = TRUE,
  values = TRUE
)

# Convert to sf
patches_cut_sf = sf::st_as_sf(patches_cut_v)
plot(patches_cut_sf)

# Rename the id explicitly
patches_cut_sf = patches_cut_sf %>%
  dplyr::rename(cut_patch_id = id)

# Spatial intersection with vector patches
#! one cut patch may get several patches ids
patch_intersection = sf::st_intersection(
  patches_cut_sf,
  patches_sf
)

# We only keep the ID of the VECTOR PATCH with highest overlap with the RASTER PATCH
patch_intersection = patch_intersection %>%
  dplyr::mutate(
    intersection_area = sf::st_area(geometry)
  )

# Select the vector patch with the largest intersection
patch_correspondence = patch_intersection %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(cut_patch_id) %>%
  dplyr::slice_max(
    order_by = intersection_area,
    n = 1,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::select(-intersection_area) %>% 
  dplyr::rename(uncut_patch_id = unique_id) %>% 
  dplyr::rename(uncut_patch_name = patch_id)


#### Species distribution --------
# We select patches occupied by GLTs at a given moment
# IMPORTANT : species distribution file should only contain either 0 (species absent) or 1 (species present)
# Species distribution:
# 1 = species present
# 0 = species absent
# -999 = no data

### 2005
# For 2005: name explicitely the occupied patches (the data on GLT census does not exist)

# Compare the patches occupied in 2005 (see Holst et al. 2006) with patches names (given in 01o_patch_prep)
# In Holst et al. 2006, occupied patches are:
# V -> Vendaval
# RV -> Rio Vermelho
# BE -> Boa Esperanca II
# I -> Imbau I + Sta Helena II + Afetiva
# A -> Sta Helena + Sta Helena I
# B -> Aldeia I
# PDA -> Poco das Antas
# U -> Uniao N
# BEN -> Nova Esperanca
# SER -> Pirineus
# We ignore patches where GLTs were removed through translocation (south west)
# PS: we don't have GLT occurrence data for that year, hence the "manual" selection

# Between parentheses, provide the NAMES of the patches
# patches_select = patches_sf %>%
#   dplyr::filter(patch_id %in% c("Vendaval",
#                                 "Rio_Vermelho",
#                                 "Boa_Esperanca_II",
#                                 "Imbau_I_2",
#                                 "Sta_Helena_II",
#                                 "Sta_Helena_I",
#                                 "Sta_Helena",
#                                 "Aldeia_I_1",
#                                 "Aldeia_I_2",
#                                 "Aldeia_I_3",
#                                 "Poco_das_Antas",
#                                 "Poco.das.Antas_192", # Cambucas
#                                 "Uniao_N_2",
#                                 "Uniao_S",
#                                 "Nova_Esperanca_2",
#                                 "Afetiva",
#                                 "Pirineus_114",
#                                 "Sao_Joao_II"))


### 2023
# For 2023: we use the spatial data (GLT census)
# IMPORTANT: occupied patches are UNCUT patches intersecting monitored groups and occupied transects from Dietz et al. 2024
# EXCEPT FOR some patches (the largests) where the entire patches are likely not occupied
# IN THAT CASE, we select the CUT patches (i.e., territories) instead

# Patches that should use CUT portions instead of uncut patches
special_patches = c(
  "Aldeia_I_4",
  "Pirineus_121",
  "Poco_das_Antas",
  "Gavioes_160",
  "Gavioes_162"
)

# Census detections from 2022-2023
glt_census = glt_occ %>%
  dplyr::filter(Detect2022 == "P")

### Select UNCUT patches

# Identify uncut patches intersecting census detections
patches_glt = patches_sf[
  lengths(
    sf::st_intersects(patches_sf, glt_census)
  ) > 0,
]

# Identify uncut patches intersecting occupied regions
patches_regions = patches_sf[
  lengths(
    sf::st_intersects(patches_sf, regions)
  ) > 0,
]

# Standard selection:
# retain occupied uncut patches, EXCEPT the special patches
patches_select_uncut = patches_sf %>%
  dplyr::filter(
    patch_id %in% c(
      patches_glt$patch_id,
      patches_regions$patch_id
    ),
    !patch_id %in% special_patches
  )

### Select CUT portions for the special patches

# Match cut patches to their original uncut patch names
special_cut_ids = patch_correspondence %>%
  dplyr::filter(
    uncut_patch_name %in% special_patches
  ) %>%
  dplyr::pull(cut_patch_id) %>%
  base::unique()

# Keep cut portions that intersect census detections
cut_intersect_glt = patches_cut_sf[
  lengths(
    sf::st_intersects(patches_cut_sf, glt_census)
  ) > 0,
]

# Keep cut portions that intersect occupied regions
cut_intersect_regions = patches_cut_sf[
  lengths(
    sf::st_intersects(patches_cut_sf, regions)
  ) > 0,
]

# Combine the cut portions intersecting either census or regions
patches_select_cut = dplyr::bind_rows(
  cut_intersect_glt,
  cut_intersect_regions
) %>%
  dplyr::filter(
    base::as.character(cut_patch_id) %in%
      base::as.character(special_cut_ids)
  ) %>%
  dplyr::distinct(cut_patch_id, .keep_all = TRUE)

### Select additional territories
# In Pirineus, the simulated population size is lower than the real
# -> we select additional territories using a buffer around P detections

# Buffer distance in metres (adjust after visual inspection)
pirineus_buffer_m = 1000

# Separate present and absent census locations
census_present = glt_occ %>%
  dplyr::filter(Detect2022 == "P")

census_absent = glt_occ %>%
  dplyr::filter(Detect2022 == "N")

# Transform census data to the CRS of the cut patches
census_present_cut = sf::st_transform(
  census_present,
  sf::st_crs(patches_cut_sf)
)

census_absent_cut = sf::st_transform(
  census_absent,
  sf::st_crs(patches_cut_sf)
)

regions_cut = sf::st_transform(
  regions,
  sf::st_crs(patches_cut_sf)
)

# Check that the CRS uses metres for buffering
if (sf::st_is_longlat(patches_cut_sf)) {
  stop(
    "patches_cut_sf uses a geographic CRS. ",
    "Transform the patches and census data to a projected CRS in metres before buffering."
  )
}

# Cut portions belonging to Pirineus_121
pirineus_cut_ids = patch_correspondence %>%
  dplyr::filter(
    uncut_patch_name == "Pirineus_121"
  ) %>%
  dplyr::pull(cut_patch_id) %>%
  base::unique()

pirineus_cut = patches_cut_sf %>%
  dplyr::filter(
    base::as.character(cut_patch_id) %in%
      base::as.character(pirineus_cut_ids)
  )

# Portions already selected by P detections or occupied regions
pirineus_initial = patches_select_cut %>%
  dplyr::filter(
    base::as.character(cut_patch_id) %in%
      base::as.character(pirineus_cut_ids)
  )

# Buffer around census locations where GLTs are present
present_buffer = sf::st_buffer(
  sf::st_union(
    sf::st_geometry(census_present_cut)
  ),
  dist = pirineus_buffer_m
)

# Candidate cut portions intersecting the buffer
pirineus_candidates = pirineus_cut[
  lengths(
    sf::st_intersects(
      pirineus_cut,
      present_buffer
    )
  ) > 0,
]

# Exclude any cut portion intersecting a census location
# where GLTs were recorded as absent ("N")
if (nrow(census_absent_cut) > 0) {
  
  absent_intersections = lengths(
    sf::st_intersects(
      pirineus_candidates,
      census_absent_cut
    )
  ) > 0
  
  pirineus_candidates = pirineus_candidates[
    !absent_intersections,
  ]
}

# Combine initially selected and additional Pirineus portions
# Retain all eligible portions within the buffer
pirineus_selected = dplyr::bind_rows(
  pirineus_initial,
  pirineus_candidates
) %>%
  dplyr::distinct(cut_patch_id, .keep_all = TRUE)

# Keep the original selection for the other special patches
patches_select_cut_other = patches_select_cut %>%
  dplyr::filter(
    !base::as.character(cut_patch_id) %in%
      base::as.character(pirineus_cut_ids)
  )

# Final cut-patch selection
patches_select_cut = dplyr::bind_rows(
  patches_select_cut_other,
  pirineus_selected
) %>%
  dplyr::distinct(cut_patch_id, .keep_all = TRUE)

### Combine standard uncut patches + selected cut portions

# Combine both selections into one sf object
patches_select = dplyr::bind_rows(
  patches_select_uncut %>%
    dplyr::rename(geometry = geom) %>%
    dplyr::select(geometry),
  patches_select_cut %>%
    dplyr::select(geometry)
)

# Visual check
plot(sf::st_geometry(patches_sf))

plot(
  sf::st_geometry(patches_select_uncut),
  col = "lightgreen",
  add = TRUE
)

plot(
  sf::st_geometry(patches_select_cut),
  col = "red",
  add = TRUE
)

### Rasterize the combined species distribution

# Use the landscape raster as the template
template = rast

# Initialize all cells as absent
terra::values(template) = 0

patch_w_glt = terra::rasterize(
  terra::vect(patches_select),
  template,
  field = 1,
  background = 0
)

# Keep selected patches as present only in habitat cells
patch_w_glt = terra::ifel(
  rast == 2 & patch_w_glt == 1,
  1,
  0
)

# Restore no-data cells
patch_w_glt[rast == -999] = -999

# Plot the final distribution
plot(
  patch_w_glt,
  col = c("white", "gray", "darkgreen")
)

# Overlay uncut patch boundaries
plot(
  sf::st_geometry(patches_sf),
  add = TRUE,
  border = "black",
  lwd = 0.3
)


#### Resample (OPTIONAL) -----
# # RangeShifter prefers integer resolutions (e.g., 30 m)
# res(patches_r)
# res(rast)
# res(patch_w_glt)
# 
# # Patches
# r = patches_r
# 
# xmin = floor(xmin(r) / 30) * 30
# xmax = ceiling(xmax(r) / 30) * 30
# ymin = floor(ymin(r) / 30) * 30
# ymax = ceiling(ymax(r) / 30) * 30
# 
# template30 = rast(
#   xmin = xmin,
#   xmax = xmax,
#   ymin = ymin,
#   ymax = ymax,
#   resolution = 30,
#   crs = crs(r)
# )
# 
# patches_r30 = resample(
#   r,
#   template30,
#   method = "near"
# )
# 
# res(patches_r30) # should be exactly 30 30
# 
# # Landscape
# r = lulc_2005_resist
# 
# xmin = floor(xmin(r) / 30) * 30
# xmax = ceiling(xmax(r) / 30) * 30
# ymin = floor(ymin(r) / 30) * 30
# ymax = ceiling(ymax(r) / 30) * 30
# 
# template30 = rast(
#   xmin = xmin,
#   xmax = xmax,
#   ymin = ymin,
#   ymax = ymax,
#   resolution = 30,
#   crs = crs(r)
# )
# 
# lulc_2005_resist_r30 = resample(
#   r,
#   template30,
#   method = "near"
# )
# 
# res(lulc_2005_resist_r30) # should be exactly 30 30
# 
# # Species distribution
# r = patch_w_glt
# 
# xmin = floor(xmin(r) / 30) * 30
# xmax = ceiling(xmax(r) / 30) * 30
# ymin = floor(ymin(r) / 30) * 30
# ymax = ceiling(ymax(r) / 30) * 30
# 
# template30 = rast(
#   xmin = xmin,
#   xmax = xmax,
#   ymin = ymin,
#   ymax = ymax,
#   resolution = 30,
#   crs = crs(r)
# )
# 
# patch_w_glt_r30 = resample(
#   r,
#   template30,
#   method = "near"
# )
# 
# res(patch_w_glt_r30) # should be exactly 30 30

#### Checks ----
land = rast
patch = patches_r

# 1. Geometry
print(compareGeom(land, patch, stopOnError = FALSE))

# 2. Number of cells
cat("Landscape cells:", ncell(land), "\n")
cat("Patch cells:", ncell(patch), "\n")

# 3. Habitat cells without patch
habitat_no_patch = land == 2 & patch <= 0

cat(
  "Habitat cells without patch:",
  sum(values(habitat_no_patch), na.rm = TRUE),
  "\n"
)

# It's OK if cells are considered habitat outside patches (it could be corridors or stepping stones)

# 4. Habitat cells with NA patch
habitat_patch_NA = land == 2 & is.na(patch)

cat(
  "Habitat cells with NA patch:",
  sum(values(habitat_patch_NA), na.rm = TRUE),
  "\n"
)

# 5. Matrix cells incorrectly assigned to patches
matrix_with_patch = land == 1 & patch > 0

cat(
  "Matrix cells assigned to a patch:",
  sum(values(matrix_with_patch), na.rm = TRUE),
  "\n"
)

# 6. NoData landscape cells with patch
nodata_with_patch = land == -999 & patch > 0

cat(
  "NoData cells assigned to a patch:",
  sum(values(nodata_with_patch), na.rm = TRUE),
  "\n"
)

#### Export -------
### IMPORTANT: all NAs values must take an integer value (e.g., -999)
### When you open the ASCII .txt file, you should see: NODATA_value -999
### Use NAflag = -999 
### Use INT2S = signed 16-bit integer

# Binary rasters as ASCII file
output_dir = here("data", 
                  "rangeshifter", 
                  "tests", 
                  "test_resist_25", # UPDATE HERE
                  "Inputs")

# Landscape
plot(rast)
writeRaster(
  rast,
  filename = file.path(output_dir, "raster_reclass_2023.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# Patches
plot(patches_r)
writeRaster(
  patches_r,
  filename = file.path(output_dir, "patches_2023.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# Species distribution
plot(patch_w_glt)
writeRaster(
  patch_w_glt,
  filename = file.path(output_dir, "patches_w_glt_2023.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# (Optional) Correspondence table
# CHOOSE THE PROPER CORRESPONDENCE TABLE (depending on the type of patches used)
write.csv(
  patch_correspondence,
  file.path(output_dir, "patch_corres_id_2023.csv"),
  row.names = FALSE
)
