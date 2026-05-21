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

### Compute distance matrix between all patches -------
distance_tables = purrr::imap(
  patches,
  function(patch_sf, yr) {
    
    # Real patch IDs
    ids = as.character(patch_sf$lyr.1)
    
    # Compute pairwise distances
    dm = sf::st_distance(patch_sf)
    
    # Assign IDs to rows/columns
    rownames(dm) = ids
    colnames(dm) = ids
    
    # Convert to long table
    dm_long = as.data.frame(
      as.table(as.matrix(dm))
    ) %>%
      dplyr::rename(
        from = Var1,
        to = Var2,
        distance = Freq
      ) %>%
      dplyr::mutate(
        year = as.numeric(yr)
      ) %>%
      dplyr::select(
        year,
        from,
        to,
        distance
      )
    
    return(dm_long)
  }
)

# Example
distance_tables$`1989`

### Corridor-patches tables for all years ---------

patches_long = purrr::imap(
  patches,
  function(patch_sf, yr) {
    
    # Retrieve corresponding objects
    corridors_sf = corridors[[yr]]
    distances_df = distance_tables[[yr]]
    
    ## Base long table
    patches_long_year = patch_sf %>%
      dplyr::mutate(
        lyr.1 = as.character(lyr.1)
      ) %>%
      dplyr::left_join(
        corridors_sf %>%
          sf::st_drop_geometry() %>%
          dplyr::mutate(
            patch_from = as.character(patch_from),
            patch_to = as.character(patch_to)
          ),
        by = c("lyr.1" = "patch_from")
      )
    
    ## Destination patch metrics
    patch_metrics_to = patch_sf %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        lyr.1,
        area_ha
      ) %>%
      dplyr::mutate(
        lyr.1 = as.character(lyr.1)
      ) %>%
      dplyr::rename(
        patch_to = lyr.1,
        area_to = area_ha
      )
    
    patches_long_year = patches_long_year %>%
      dplyr::left_join(
        patch_metrics_to,
        by = "patch_to"
      )
    
    ## Join distances
    distances_join = distances_df %>%
      dplyr::select(-year) %>% 
      dplyr::mutate(
        from = as.character(from),
        to = as.character(to)
      )
    
    patches_long_year = patches_long_year %>%
      dplyr::mutate(
        patch_to = as.character(patch_to)
      ) %>%
      dplyr::left_join(
        distances_join,
        by = c(
          "lyr.1" = "from",
          "patch_to" = "to"
        )
      )
    
    ## Add year column
    patches_long_year = patches_long_year %>%
      dplyr::mutate(
        year = as.numeric(yr)
      )
    
    return(patches_long_year)
  }
)

# Names preserved
names(patches_long) = names(patches)

# Combine into one long table
patches_long_all = dplyr::bind_rows(patches_long, .id = "year_list")
afetiva = patches_long_all %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(FragName2 == "Afetiva") %>% 
  dplyr::select(-year_list)
