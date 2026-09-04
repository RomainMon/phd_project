#------------------------------------------------#
# Author: Romain Monassier
# Objective: Identifying patches occupied by GLTs
#------------------------------------------------#

### Libraries ----

library(sf)
library(dplyr)
library(here)


### Data ----

#### Census -----
census = sf::st_read(here("data", "glt", "JDietz", "glt_distrib_2013_2018_2022.shp"))
sf::st_crs(census)

#### Group locations -----
regions = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
sf::st_crs(regions)

#### Patches -----
patches = sf::st_read(here("outputs", "data", "patches_rshifter", "patches_rshifter_2005.gpkg"))
sf::st_crs(patches)


### Identification ----
#### Keep census polygons where GLTs were detected ------
census_2013_P = census %>%
  dplyr::filter(Detect2013 == "P")


#### Find patches intersecting the occupied census polygons -----
# If a census polygon intersects several patches
# -> Keep the largest patch

# Find all patch/census intersections
patches_census_inter = sf::st_intersection(
  patches %>% dplyr::select(patch_id, geom),
  census_2013_P %>% dplyr::select(ID2, geometry)
) %>%
  dplyr::mutate(
    intersection_area = as.numeric(sf::st_area(geom))
  )

# For each census polygon, keep the patch with the largest overlap
patches_census = patches_census_inter %>%
  dplyr::group_by(ID2) %>%
  dplyr::slice_max(
    order_by = intersection_area,
    n = 1,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup()

# Resume
patches_census = patches_census %>% 
  sf::st_drop_geometry() %>% 
  dplyr::distinct(patch_id)

#### Keep only patches intersecting Regions -----

patches_regions = patches %>%
  dplyr::filter(
    lengths(
      sf::st_intersects(
        geom,
        regions
      )
    ) > 0
  ) %>% 
  sf::st_drop_geometry()



#### Bind data -----
patches_occupied_2013 = dplyr::bind_rows(patches_census, patches_regions) %>% 
  dplyr::distinct(patch_id)

nrow(patches_occupied_2013)


#### Save the list ----
write.csv(
  patches_occupied_2013,
  here(
    "outputs",
    "data",
    "patches_rshifter",
    "patches_occupied_2013.csv"
  ),
  row.names = FALSE
)
