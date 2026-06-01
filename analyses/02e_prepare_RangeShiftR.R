#------------------------------------------------#
# Author: Romain Monassier
# Objective: Preparing RangeShiftR data and parameters
#------------------------------------------------#


### Load packages ------
library(dplyr)
library(here)
library(terra)

### Import rasters -------
#### Rasters reclassified ----
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_reclass = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_reclass)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
names(rasters_reclass) = years
plot(rasters_reclass[['2024']], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))

### Maps -----

#### Binary raster --------
reclass_binary <- function(xx) {
  habitat <- 1
  background <- c(2, 3, 4, 5, 6)
  
  # Make a copy of original values
  v <- xx[]
  
  # Initialize all as 0 (missing data)
  xx[] <- 0
  
  # Assign habitat first using original values
  xx[v == habitat] <- 2
  
  # Then assign background
  xx[v %in% background] <- 1
  
  return(xx)
}

## Apply to all rasters
rasters_binary = lapply(rasters_reclass, reclass_binary)
plot(rasters_binary[['2005']], col=c("white", "grey", "#32a65e"))
r2005 = rasters_binary[['2005']]


#### Patches --------
# Identify contiguous habitat patches
# Keep only habitat
habitat = r2005
habitat[habitat != 2] = NA

# Identify contiguous habitat patches
patches = terra::patches(
  habitat,
  directions = 8
)
plot(patches)

##### Export -------
# Binary rasters as ASCII file
output_dir = here("rangeshifter", "Inputs")

writeRaster(
  r2005,
  filename = file.path(output_dir, "raster_reclass_binary_2005.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE
)

writeRaster(
  patches,
  filename = file.path(output_dir, "patches_2005.txt"),
  filetype = "AAIGrid",
  overwrite = TRUE
)
