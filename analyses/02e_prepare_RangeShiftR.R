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
plot(rasters_reclass_mspa[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "orange"))
plot(sf::st_geometry(patches[[36]]), col=NA, add=TRUE)
names(patches) = vector_df$year # Name by year

### Maps -----

#### Patches --------
# IMPORTANT : patches should have an integer value corresponding to patch id, and background must be set to 0

# Create a permanent numeric ID
patches = lapply(patches, function(x){
  x %>%
    mutate(unique_id = row_number())
})
anyDuplicated(patches[['2005']]$lyr.1) # Duplicated ids
anyDuplicated(patches[['2005']]$unique_id) # ALL GOOD

# Rasterize patches
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

# Example
patches2005_r = patch_rasters[['2005']]
plot(patches2005_r)

#### Rasters with habitat, matrix, corridor --------

## Overlay rasters with GLT patches
# Important step if patches are different from base raster (e.g., due to dilatation-erosion)
# Any patch cell becomes habitat (=1)

rasters_w_patches= vector("list", length(rasters_reclass_mspa))
names(rasters_w_patches) = names(rasters_reclass_mspa)

for (yr in names(rasters_reclass_mspa)) {
  
  cat("Processing", yr, "\n")
  
  r = rasters_reclass_mspa[[yr]]
  p = patch_rasters[[yr]]
  
  r_out = r
  
  # Cells belonging to a patch
  r_out[p > 0] = 1
  
  # Preserve NoData
  r_out[p == -999] = -999
  
  rasters_w_patches[[yr]] = r_out
}
plot(rasters_w_patches[['2005']])

## Reclass rasters
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
  xx[v == corridor] <- 3
  
  # Then assign background
  xx[v %in% background] <- 1
  
  return(xx)
}

## Apply to all rasters
rasters_rshifter = lapply(rasters_w_patches, reclass)
plot(rasters_rshifter[['2005']], col=c("white", "gray", "darkgreen", "orange"))
r2005 = rasters_rshifter[['2005']]

#### Species distribution --------
# IMPORTANT : species distribution file should only contain either 0 (species absent) or 1 (species present)
# Species distribution:
# 1 = species present
# 0 = species absent
# -999 = no data

##### 2005 -------
patches2005 = patches[['2005']]

### Correspondence table between patch name and id
patch_corres_id = patches2005 %>%
  sf::st_drop_geometry() %>%
  dplyr::select(patch_id, unique_id)

# Select patches
# Based on patch name and delimitation
patches2005_select = patches2005 %>% 
  dplyr::filter(patch_id %in% c("Aldeia_I_1",
                                "Aldeia_I_2",
                                "Sta_Helena",
                                "Imbau_I_2",
                                "Afetiva",
                                "Nova_Esperanca_2",
                                "Pirineus_111",
                                "Poco_das_Antas",
                                "Rio_Vermelho",
                                "Uniao_N_2"))
plot(sf::st_geometry(patches2005))
plot(sf::st_geometry(patches2005_select), col="darkgreen", add=TRUE)

## To raster
# Reference raster
template = r2005
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
patch_w_glt_2005[r2005 == -999] = -999
plot(patch_w_glt_2005, col=c("white","gray","darkgreen"))


#### Export -------
### IMPORTANT: all NAs values must take an integer value (e.g., -999)
### When you open the ASCII .txt file, you should see: NODATA_value -999
### Use NAflag = -999 
### Use INT2S = signed 16-bit integer

# Binary rasters as ASCII file
output_dir = here("data", "rangeshifter", "tests")

# Landscape
plot(r2005)
writeRaster(
  r2005,
  filename = file.path(output_dir, "raster_reclass_binary_2005.txt"),
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
