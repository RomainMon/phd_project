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
plot(rasters_rshifter[['2024']], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "purple","orange"))

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
plot(rasters_rshifter[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "purple","orange"))
plot(sf::st_geometry(patches[[36]]), col=NA, add=TRUE)
names(patches) = vector_df$year # Name by year

#### Patches (raster) -------
# These correspond to raster patches created WITH Queru's et al. (2026) patch-cutting procedure
patches_cut = terra::rast(here("outputs", "data", "patches_cut", 
                               "2024", # CHOOSE THE PATCHES OF THE YEAR OF INTEREST
                               "FinalPatches_2024.tif"))
plot(patches_cut)

#### Stepping stones (raster) -------
# These correspond to raster patches created WITH Queru's et al. (2026) identification of small patches
stepping_stones = terra::rast(here("outputs", "data", "patches_cut", 
                                   "2024", # CHOOSE THE PATCHES OF THE YEAR OF INTEREST
                                   "TooSmallPatches_2024.tif"))
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
rast = rasters_resist[['2024']] # CHOOSE THE YEAR OF INTEREST
# plot(rast, col=c("white","gray","darkgreen")) # Binary raster
plot(rast, col=c("white","yellow","darkgreen","green","brown","blue","red"))
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
anyDuplicated(patches[['2024']]$lyr.1) # Duplicated ids
anyDuplicated(patches[['2024']]$patch_id) # GOOD but not integer value
anyDuplicated(patches[['2024']]$unique_id) # ALL GOOD

# Vector patches
patches_sf = patches[['2024']]

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
patches_r = patch_rasters[['2024']]

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

#### Select patches -----
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
# 2005
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

# 2024
# Patches intersecting GLTs detected in 2022
patches_glt = patches_sf[
  lengths(
    sf::st_intersects(
      patches_sf,
      glt_occ %>% dplyr::filter(Detect2022 == "P")
    )
  ) > 0,
]

# Patches intersecting regions
patches_regions = patches_sf[
  lengths(
    sf::st_intersects(
      patches_sf,
      regions
    )
  ) > 0,
]

# Keep patches intersecting either a GLT OR a region
patches_select = patches_sf %>% 
  dplyr::filter(
    patch_id %in% c(
      patches_glt$patch_id,
      patches_regions$patch_id
    )
  )

# Select these patches
plot(sf::st_geometry(patches_sf))
plot(sf::st_geometry(patches_glt), col="darkgreen", add=TRUE)
plot(sf::st_geometry(patches_regions), col="lightgreen", add=TRUE)

## To raster
# Reference raster
template = rast

# Create a 0-valued raster with same geometry
values(template) = 0

# Rasterize selected patches as 1
patch_w_glt = terra::rasterize(
  terra::vect(patches_select),
  template,
  field = 1,
  background = 0
)

# Keep selected patches as 1 ONLY where the landscape is habitat (rast == 2)
# Selected-patch cells overlapping matrix remain 0
patch_w_glt = terra::ifel(
  rast == 2 & patch_w_glt == 1,
  1,
  0
)

# Restore no-data cells from landscape
patch_w_glt[rast == -999] = -999

plot(
  patch_w_glt,
  col = c("white", "gray", "darkgreen")
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
                  "test_resist_17", # UPDATE HERE
                  "Inputs")

# Landscape
plot(rast)
writeRaster(
  rast,
  filename = file.path(output_dir, "raster_reclass_2024.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# Patches
plot(patches_r)
writeRaster(
  patches_r,
  filename = file.path(output_dir, "patches_2024.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# Species distribution
plot(patch_w_glt)
writeRaster(
  patch_w_glt,
  filename = file.path(output_dir, "patches_w_glt_2024.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# (Optional) Correspondence table
# CHOOSE THE PROPER CORRESPONDENCE TABLE (depending on the type of patches used)
write.csv(
  patch_correspondence,
  file.path(output_dir, "patch_corres_id_2024.csv"),
  row.names = FALSE
)
