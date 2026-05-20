#------------------------------------------------#
# Author: Romain Monassier
# Objective: Join corridor information to patches
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(purrr)
library(sf)


### Import patch datasets ------
base_path = here("outputs", "data", "patchmetrics")

patch_files = list.files(
  base_path,
  pattern = "\\.gpkg$",
  full.names = TRUE
)

# Extract years
years = stringr::str_extract(
  basename(patch_files),
  "(?<!\\d)\\d{4}(?!\\d)"
)

# Create dataframe
patch_df = data.frame(
  file = patch_files,
  year = as.numeric(years)
) %>%
  dplyr::arrange(year)

# Import patches
patches = purrr::map(
  patch_df$file,
  sf::st_read,
  quiet = TRUE
)

names(patches) = patch_df$year

### Import corridor datasets ------
base_path = here("outputs", "data", "corridor")

corridor_files = list.files(
  base_path,
  pattern = "\\.gpkg$",
  full.names = TRUE
)

# Extract years
years_corr = stringr::str_extract(
  basename(corridor_files),
  "(?<!\\d)\\d{4}(?!\\d)"
)

# Create dataframe
corridor_df = data.frame(
  file = corridor_files,
  year = as.numeric(years_corr)
) %>%
  dplyr::arrange(year)

# Import corridors
corridors = purrr::map(
  corridor_df$file,
  sf::st_read,
  quiet = TRUE
)

names(corridors) = corridor_df$year

### Build long-format connection table ------
