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
base_path = here("outputs", "data", "MapBiomas", "MSPA", "reclass_w_MSPA")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_reclass_mspa = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_reclass_mspa)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
names(rasters_reclass_mspa) = years
plot(rasters_reclass_mspa[['2024']], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))

#### GLT distribution ----
# 2013-2018
glt_2013_2018 = sf::st_read(here("data", "glt", "JDietz", "glt_distrib_2013_2018.shp"))
plot(glt_2013_2018)
crs(glt_2013_2018)
# 2023
glt_2022 = sf::st_read(here("data", "glt", "JDietz", "glt_distrib_2013_2018_2022.shp"))
plot(glt_2022)
crs(glt_2022)

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
plot(rasters_reclass_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(patches[[36]]), col=NA, add=TRUE)
names(patches) = vector_df$year # Name by year

#### Patches (raster) -------
# These correspond to raster patches created WITH Queru's et al. (2026) patch-cutting procedure
patches_cut = terra::rast(here("outputs", "data", "patches_cut", "FinalPatches_2005.tif"))
plot(patches_cut)

#### Stepping stones (raster) -------
# These correspond to raster patches created WITH Queru's et al. (2026) identification of small patches
stepping_stones = terra::rast(here("outputs", "data", "patches_cut", "TooSmallPatches_2005.tif"))
plot(stepping_stones)

#### Elevation ----
topo_r = terra::rast(here("data", "geo", "TOPODATA", "work", "topo_bbox.tif"))
plot(topo_r)

### Maps -----

#### Patches --------
# IMPORTANT : patches should have an integer value corresponding to patch id, and background must be set to 0


##### IF VECTOR -> Rasterize patches -----

# Create a permanent numeric ID
patches = lapply(patches, function(x){
  x %>%
    mutate(unique_id = row_number())
})
anyDuplicated(patches[['2005']]$lyr.1) # Duplicated ids
anyDuplicated(patches[['2005']]$patch_id) # GOOD but not integer value
anyDuplicated(patches[['2005']]$unique_id) # ALL GOOD

# Raster
patch_rasters = vector("list", length(patches))
names(patch_rasters) = names(patches)

for (yr in names(patches)) {
  
  cat("Rasterizing patches:", yr, "\n")
  
  # Template raster
  r_template = rasters_reclass_mspa[[yr]]
  
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

##### IF RASTER ----
# Select patches 2005
# patches2005_r = patch_rasters[['2005']]
patches2005_r = patches_cut
plot(patches2005_r)
unique(patches2005_r) # Unique id per patch?

#### Landscape --------

##### Selected landscape -----
lulc_2005 = rasters_reclass_mspa[['2005']]

##### Overlay patches ----
# It is necessary to overlay patches if patches are different from base raster (e.g., due to dilatation-erosion)
# Any patch cell becomes habitat (=1)
# Cells belonging to a patch
lulc_2005[patches2005_r > 0] = 1
# Preserve NoData
lulc_2005[patches2005_r == -999] = -999
plot(lulc_2005)

##### Reclass rasters (binary) ----
# IMPORTANT : the basis landscape should have integer values (with matrix = 1, NAs = -999)
reclass <- function(xx) {
  habitat <- 1
  corridor <- 33
  background <- c(2, 3, 4, 5, 6)
  
  # Make a copy of original values
  v <- xx[]
  
  # Initialize all missing data
  xx[] <- -999
  
  # Assign habitat first using original values
  xx[v == habitat] <- 2
  
  ## Assign corridor
  xx[v == corridor] <- 2
  
  # Then assign background
  xx[v %in% background] <- 1
  
  return(xx)
}

## Apply to all rasters
rasters_rshifter = lapply(rasters_reclass_mspa, reclass)
r2005 = rasters_rshifter[['2005']]

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
  
  # Make a copy of original values
  v <- xx[]
  
  # Initialize all missing data
  xx[] <- -999
  
  # Assign habitat first using original values
  xx[v == habitat] <- 2
  
  ## Assign corridor
  xx[v == corridor] <- 3
  
  # Assign matrix land uses
  xx[v %in% c(notforest,wetlands)] <- 1
  xx[v == agri] <- 4
  xx[v == water] <- 5
  xx[v == built] <- 6
  
  
  return(xx)
}

## Apply
lulc_2005_resist = reclass(lulc_2005)
plot(lulc_2005_resist, col=c("white","darkgreen","orange","lightgreen","yellow","blue","red"))

##### Overlay -----
### High forests
# GLTs do not occupy forests above 500 m -> they are reclassified 
reclass_high_forest = function(r, topo, forest_value,
                               elev_threshold,
                               new_value) {
  
  # Align DEM if needed
  if (!compareGeom(r, topo, stopOnError = FALSE)) {
    topo = terra::resample(topo, r, method = "bilinear")
  }
  
  r_out = r
  
  # Forest cells above threshold
  idx = r == forest_value & topo > elev_threshold
  
  r_out[idx] = new_value
  
  return(r_out)
}

r2005 = reclass_high_forest(r2005, topo = topo_r, 
                                       forest_value = 2, 
                                       elev_threshold = 500, 
                                       new_value = 1)
plot(r2005)

### Stepping stones
# Small patches are considered stepping stones and take another resistance value
plot(stepping_stones)
r2005[stepping_stones == 1] = 2

##### Plot ----
plot(r2005)

#### Species distribution --------
# IMPORTANT : species distribution file should only contain either 0 (species absent) or 1 (species present)
# Species distribution:
# 1 = species present
# 0 = species absent
# -999 = no data

patches2005 = patches[['2005']]

### Correspondence table between patch name and id
patch_corres_id = patches2005 %>%
  sf::st_drop_geometry() %>%
  dplyr::select(patch_id, unique_id)

# Select patches
# Compare the patches occupied in 2005 (see Holst et al. 2006) with patches names (given in 01o_patch_prep)
# In Holst et al. 2006, occupied patches are V:
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

patches2005_select = patches2005 %>% 
  dplyr::filter(patch_id %in% c("Vendaval",
                                "Rio_Vermelho",
                                "Boa_Esperanca_II",
                                "Imbau_I_2",
                                "Sta_Helena_II",
                                "Sta_Helena_I",
                                "Sta_Helena",
                                "Aldeia_I_1",
                                "Aldeia_I_2",
                                "Poco_das_Antas",
                                "Uniao_N_2",
                                "Nova_Esperanca_2",
                                "Afetiva",
                                "Pirineus_114")) # Check Pirineus name in QGIS!
plot(sf::st_geometry(patches2005))
plot(sf::st_geometry(patches2005_select), col="darkgreen", add=TRUE)

## To raster
# Reference raster
template = lulc_2005_resist
# Create a 0-valued raster with same geometry
values(template) = 0
# Rasterize selected patches as 1
patch_w_glt_2005 = terra::rasterize(
  terra::vect(patches2005_select),
  template,
  field = 1,
  background = 0
)

# Ensure occupied patches are 1
patch_w_glt_2005[patch_w_glt_2005 > 0] = 1

# Matrix and other valid landscape cells become 0
patch_w_glt_2005[patch_w_glt_2005 == 0] = 0

# Restore no-data cells from landscape
patch_w_glt_2005[lulc_2005_resist == -999] = -999
plot(patch_w_glt_2005, col=c("white","gray","darkgreen"))

#### Resample (OPTIONAL) -----
# RangeShifter prefers integer resolutions (e.g., 30 m)
res(patches2005_r)
res(r2005)
res(patch_w_glt_2005)

# Patches
r = patches2005_r

xmin = floor(xmin(r) / 30) * 30
xmax = ceiling(xmax(r) / 30) * 30
ymin = floor(ymin(r) / 30) * 30
ymax = ceiling(ymax(r) / 30) * 30

template30 = rast(
  xmin = xmin,
  xmax = xmax,
  ymin = ymin,
  ymax = ymax,
  resolution = 30,
  crs = crs(r)
)

patches2005_r30 = resample(
  r,
  template30,
  method = "near"
)

res(patches2005_r30) # should be exactly 30 30

# Landscape
r = lulc_2005_resist

xmin = floor(xmin(r) / 30) * 30
xmax = ceiling(xmax(r) / 30) * 30
ymin = floor(ymin(r) / 30) * 30
ymax = ceiling(ymax(r) / 30) * 30

template30 = rast(
  xmin = xmin,
  xmax = xmax,
  ymin = ymin,
  ymax = ymax,
  resolution = 30,
  crs = crs(r)
)

lulc_2005_resist_r30 = resample(
  r,
  template30,
  method = "near"
)

res(lulc_2005_resist_r30) # should be exactly 30 30

# Species distribution
r = patch_w_glt_2005

xmin = floor(xmin(r) / 30) * 30
xmax = ceiling(xmax(r) / 30) * 30
ymin = floor(ymin(r) / 30) * 30
ymax = ceiling(ymax(r) / 30) * 30

template30 = rast(
  xmin = xmin,
  xmax = xmax,
  ymin = ymin,
  ymax = ymax,
  resolution = 30,
  crs = crs(r)
)

patch_w_glt_2005_r30 = resample(
  r,
  template30,
  method = "near"
)

res(patch_w_glt_2005_r30) # should be exactly 30 30

#### Export -------
### IMPORTANT: all NAs values must take an integer value (e.g., -999)
### When you open the ASCII .txt file, you should see: NODATA_value -999
### Use NAflag = -999 
### Use INT2S = signed 16-bit integer

# Binary rasters as ASCII file
output_dir = here("data", 
                  "rangeshifter", 
                  "tests", 
                  "test_resist_1", # UPDATE HERE
                  "Inputs")

# Landscape
plot(r2005)
writeRaster(
  r2005,
  filename = file.path(output_dir, "raster_reclass_2005.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# Patches
plot(patches2005_r)
writeRaster(
  patches2005_r,
  filename = file.path(output_dir, "patches_2005.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# Species distribution
plot(patch_w_glt_2005)
writeRaster(
  patch_w_glt_2005,
  filename = file.path(output_dir, "patches_w_glt_2005.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

# Correspondence table
write.csv(
  patch_corres_id,
  file.path(output_dir, "patch_corres_id_2005.csv"),
  row.names = FALSE
)
