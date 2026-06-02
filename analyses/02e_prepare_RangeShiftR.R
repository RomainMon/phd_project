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
glt_2013_2018 = sf::st_read(here("data", "glt", "JDietz", "glt_distrib_2013_2018.shp"))
plot(glt_2013_2018)
crs(glt_2013_2018)

#### Patches names ----
patch_name = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
crs(patch_name)

### Maps -----

#### Rasters with habitat, matrix, corridor --------
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
rasters_binary = lapply(rasters_reclass_mspa, reclass)
plot(rasters_binary[['2005']], col=c("white", "gray", "darkgreen", "orange"))
r2005 = rasters_binary[['2005']]

#### Patches --------
# IMPORTANT : patches should have an integer value corresponding to patch id, and background must be set to 0

# Identify contiguous habitat patches
# Keep only habitat
patches2005 = r2005
# Habitat = 1, everything else = 0
habitat = ifel(r2005 == 2, 1, 0)

# Identify patches
patches2005 = terra::patches(
  habitat,
  directions = 8,
  zeroAsNA = TRUE
)

# For patches, we need to convert background NAs to 0
patches2005[is.na(patches2005)] = 0
patches2005[r2005 == -999] = -999
plot(patches2005)

#### Species distribution --------
# IMPORTANT : species distribution file should only contain either 0 (species absent) or 1 (species present)
# Species distribution:
# 1 = species present
# 0 = species absent
# -999 = no data

##### 2005 -------
# Create polygons from original patch raster
patches2005_poly = patches2005

# Remove matrix and nodata
patches2005_poly[patches2005_poly <= 0] = NA

patches2005_v = sf::st_as_sf(
  as.polygons(patches2005_poly, dissolve = TRUE)
)
plot(patches2005_v)

# Join patch name
patches2005_v = sf::st_join(
  patches2005_v,
  patch_name,
  join = st_intersects,
  largest = TRUE
)
unique(patches2005_v$FragName2)

# Select patches
patches2005_v_select = patches2005_v %>% 
  dplyr::filter(FragName2 %in% c("Large_frag_North",
                                 "Sta_Helena",
                                 "Sta_Helena_I",
                                 "Poco_das_Antas",
                                 "Uniao_N",
                                 "Uniao_S",
                                 "Sao_Joao",
                                 "Recanto_Preservar",
                                 "Boa_Esperanca_II",
                                 "Vendaval",
                                 "Nova_Esperanca",
                                 "Afetiva",
                                 "Large_frag_West",
                                 "Rio_Vermelho"))
plot(sf::st_geometry(patches2005_v))
plot(sf::st_geometry(patches2005_v_select), col="darkgreen", add=TRUE)

# To raster
# Reference raster
template = rasters_binary[['2005']]
# Create a 0-valued raster with same geometry
patches_raster = template
values(patches_raster) = 0
# Rasterize selected patches as 1
patch_w_glt_2005 = terra::rasterize(
  terra::vect(patches2005_v_select),
  patches_raster,
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
output_dir = here("data", "rangeshifter", "Inputs")

writeRaster(
  r2005,
  filename = file.path(output_dir, "raster_reclass_binary_2005.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

writeRaster(
  patches2005,
  filename = file.path(output_dir, "patches_2005.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)

writeRaster(
  patch_w_glt_2005,
  filename = file.path(output_dir, "patches_w_glt_2005.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  datatype = "INT2S",
  NAflag = -999
)
