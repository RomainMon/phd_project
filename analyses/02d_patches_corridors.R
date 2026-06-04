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
patch_connections_all = readr::read_csv(
  file.path(base_path, "patch_connections_all.csv"),
  show_col_types = FALSE
)

### Compute distance matrix between all patches -------
distance_tables = purrr::imap(
  patches,
  function(patch_sf, yr) {
    
    # Real patch IDs
    ids = as.character(patch_sf$patch_id)
    
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
        distance_ij = Freq
      ) %>%
      dplyr::mutate(
        year = as.numeric(yr)
      ) %>%
      dplyr::select(
        year,
        from,
        to,
        distance_ij
      )
    
    return(dm_long)
  }
)

# Example
distance_tables$`1989`

### Corridor-patches tables for all years ---------
# Below, the function creates a long-format table (for each year) by joining to patches:
# - their connections to corridors
# - if they are connected, to which patches (ids)
# - the metrics of the connected patches
# - distance between i and j patches
# NB: in the output, rows are DUPLICATED if a patch has >1 connections
patches_long = purrr::imap(
  patches,
  function(patch_sf, yr) {
    
    ## Retrieve corresponding objects
    distances_df = distance_tables[[yr]]
    
    # Corridor table for current year
    corridors_year = patch_connections_all %>%
      dplyr::filter(
        year == as.numeric(yr)
      ) %>%
      dplyr::mutate(
        patch_from = as.character(patch_from),
        patch_to = as.character(patch_to)
      )
    
    ## Base long table
    patches_long_year = patch_sf %>%
      dplyr::left_join(
        corridors_year,
        by = c(
          "patch_id" = "patch_from"
        )
      )
    
    ## Destination patch metrics
    patch_metrics_to = patch_sf %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        patch_id,
        area_ha
      ) %>%
      dplyr::rename(
        patch_to = patch_id,
        area_to = area_ha
      )
    
    patches_long_year = patches_long_year %>%
      dplyr::left_join(
        patch_metrics_to,
        by = "patch_to"
      )
    
    ## Join distances
    distances_join = distances_df %>%
      dplyr::select(
        -year
      ) %>%
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
          "patch_id" = "from",
          "patch_to" = "to"
        )
      )
    
    ## Add year
    patches_long_year = patches_long_year %>%
      dplyr::mutate(
        year = as.numeric(yr)
      )
    return(patches_long_year)
  }
)

# Preserve names
names(patches_long) = names(patches)

# Combine all years
patches_long_all = dplyr::bind_rows(
  patches_long,
  .id = "year_list"
) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-year_list) %>% 
  dplyr::mutate(
    distance_ij = as.numeric(distance_ij)) # Convert distances into numeric format

# Compute binary connected/not connected variable
patches_long_all = patches_long_all %>% 
  dplyr::mutate(connected = dplyr::case_when(!is.na(corridor_id) ~ 1,
                                             TRUE ~ 0))

# Example
example = patches_long_all %>%
  dplyr::filter(
    patch_id == "Iguape"
  )

### Export patch history ------
readr::write_csv(
  patches_long_all,
  here(
    "outputs",
    "data",
    "corridor",
    "patch_connect_history.csv"
  ))
