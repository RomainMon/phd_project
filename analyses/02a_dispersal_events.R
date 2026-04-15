#------------------------------------------------#
# Author: Romain Monassier
# Objective: Extract dispersal events
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(cli)
library(xlsx)
library(terra)
library(sf)

### Import datasets -----

# GLT data
load(here("data", "glt", "APonchon", "data_clean_long_final.RData"))

# Group locations
regions = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions.csv = read.table(here("data", "geo", "APonchon", "GLT", "RegionsName.csv"),
                header=T,sep=",")

# LULC
raster_lulc_2024 = terra::rast(here("outputs", "data", "MapBiomas", "Rasters_reclass", "raster_reclass_2024.tif"))

# Corridors
base_path = here("outputs", "data", "corridor")
vect_files = list.files(base_path, pattern = "\\.gpkg$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(vect_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
vector_df = data.frame(file = vect_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load vectors in chronological order
corridors = lapply(vector_df$file, sf::st_read)
years = vector_df$year
# Check
for (i in seq_along(corridors)) {
  cat("Year", years[i], " → raster name:", basename(vector_df$file[i]), "\n")
}
names(corridors) = vector_df$year # Name by year

# Patches
base_path = here("outputs", "data", "patchmetrics")
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
names(patches) = vector_df$year # Name by year

# Plot
plot(raster_lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(corridors[[36]]), col=NA, add=TRUE)
plot(sf::st_geometry(patches[[36]]), col=NA, add=TRUE)
plot(sf::st_geometry(regions), col="magenta", add=TRUE)

### Parameters ------
# Bad GLTs (e.g., unidentified individuals)
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490") # Last list of problematic individuals (see Aurore Zenodo)

### Dispersal events ------
# This code uses Aurore's code (section 4. from document 1- Formatting data) 
# Here, we identify individuals located in at least two groups
# But it does not mean it's dispersal: it can be prospection

# Filter out "bad" individuals, dead individuals, and individuals not observed at a given time (disp == 1)
data.filter = data.clean.final %>% 
  dplyr::filter(!GLT %in% bad & Disp!=1 & Death==0) %>% 
  dplyr::arrange(DateObs, GLT) %>% 
  dplyr::group_by(GLT) 

# Get the name of all individuals observed in more than one group
dispind = data.filter %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(N = dplyr::n_distinct(Group)) %>% 
  dplyr::filter(N >1)

# Create a data frame with GLT, origin-destination
# Warning: the table may keep individuals who both PROSPECT (i.e., come back to native group) AND DISPERSE (i.e., reproduce and stay in another group)
data.disp = data.filter %>% 
  dplyr::filter(GLT %in% dispind$GLT) %>% # Select dispersing individuals
  dplyr::arrange(GLT,DateObs) %>% # Arrange by GLT and date of observation
  dplyr::mutate(FromGroup=dplyr::lag(Group),
                ToGroup=Group,
                FromRegion=dplyr::lag(Region),
                ToRegion=Region,
                FromUMMP=dplyr::lag(UMMPs),
                ToUMMP=UMMPs) %>% 
  dplyr::mutate(Transloc=ifelse(dplyr::lag(Disp==2) & Disp==0 & dplyr::lag(GLT)==GLT,
                                1,0)) %>% 
  dplyr::filter(ObsOrder > 1) %>% # remove first observation
  
  dplyr::mutate(Change=ifelse(ToGroup != FromGroup, 1,0)) %>% 
  dplyr::filter(Change > 0 & Transloc == 0) %>% 
  dplyr::select(Year,DateObs,GLT,Tattoo,SexOK,BirthOK,IdadeOK,
                FromGroup,ToGroup,FromRegion,ToRegion,FromUMMP,ToUMMP) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(disp_id = paste0("disp", row_number()))


### Spatial filter  ------
# Keep only dispersal between known locations
# Create a vector of all unique group names from FromGroup and ToGroup
valid_groups = data.disp %>%
  dplyr::select(FromGroup, ToGroup) %>%
  unlist() %>%
  unique()

# Filter to keep only rows where both FromGroup and ToGroup are known locations
data.disp.filter.grp = data.disp %>%
  dplyr::filter(
    FromGroup %in% regions.csv$Abreviation,
    ToGroup %in% regions.csv$Abreviation
  )

### Summary statistics -----
# 1. Number of distinct GLTs
data.disp.filter.grp %>%
  dplyr::distinct(GLT) %>%
  nrow()

# 2. Observation period
paste(
  min(data.disp.filter.grp$Year),
  "to",
  max(data.disp.filter.grp$Year)
)

# 3. Number of observations per year
data.disp.filter.grp %>%
  dplyr::count(Year, name = "Observations") %>%
  dplyr::arrange(Year) %>% 
  print(n=31)

# 4. Sex ratio
data.disp.filter.grp %>%
  dplyr::count(SexOK) %>%
  dplyr::mutate(Proportion = n / sum(n))

# 5. Frequency of individuals by age
data.disp.filter.grp %>%
  dplyr::count(IdadeOK, name = "Frequency") %>%
  dplyr::arrange(dplyr::desc(Frequency))

# 6. Number of groups GLTs departed from or immigrated to
data.disp.filter.grp %>%
  dplyr::distinct(FromGroup) %>%
  nrow()
data.disp.filter.grp %>%
  dplyr::distinct(ToGroup) %>%
  nrow()

# 7. To-From associations (Groups, Regions, UMMPs)
data.disp.filter.grp %>%
  dplyr::count(FromGroup, ToGroup) %>% 
  dplyr::arrange(dplyr::desc(n))

data.disp.filter.grp %>%
  dplyr::count(FromRegion, ToRegion)  %>% 
  dplyr::arrange(dplyr::desc(n))

data.disp.filter.grp %>%
  dplyr::count(FromUMMP, ToUMMP)  %>% 
  dplyr::arrange(dplyr::desc(n))

### Join spatial information ------
## Based on group location (regions file)
# Reformat regions
grp.loc = regions.csv %>% 
  dplyr::rename(Long=CENTROIDE_X_UTM_SAD69_23S,
                Lat=CENTROIDE_Y_UTM_SAD69_23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter_all(any_vars(!is.na(.)))

# Join group location
data.disp.from = data.disp.filter.grp %>% 
  dplyr::left_join(grp.loc %>% dplyr::select(c(Group,Long,Lat)), by=c("FromGroup"="Group"), multiple="first")
data.disp.to = data.disp.filter.grp %>% 
  dplyr::left_join(grp.loc %>% dplyr::select(c(Group,Long,Lat)), by=c("ToGroup"="Group"), multiple="first")

# Create origins and destination sf objects
data.disp.from.sf = data.disp.from %>% 
  sf::st_as_sf(coords=c("Long","Lat"), crs=sf::st_crs(regions))
data.disp.to.sf = data.disp.to %>% 
  sf::st_as_sf(coords=c("Long","Lat"), crs=sf::st_crs(regions))

# Join patch id
# This function joins patches id depending on the year
# Two methods: intersection and if still NA, take closest patch
join_patch_by_year = function(df, patches, patch_col = "patch_id") {
  
  dplyr::bind_rows(
    lapply(split(df, df$Year), function(d) {
      
      y = unique(d$Year)
      patches_y = patches[[as.character(y)]]
      
      patches_y = patches_y %>%
        dplyr::rename(patch_id = all_of(patch_col)) %>%
        dplyr::select(patch_id)
      
      # First: intersection join
      d_join = sf::st_join(d, patches_y, join = sf::st_intersects, left = TRUE)
      
      # Identify NAs
      na_idx = which(is.na(d_join$patch_id))
      
      if (length(na_idx) > 0) {
        
        # Compute nearest patch for NA rows
        nearest_idx = sf::st_nearest_feature(d_join[na_idx, ], patches_y)
        d_join$patch_id[na_idx] = patches_y$patch_id[nearest_idx]
      }
      
      return(d_join)
    })
  )
}

# Apply to origins
data.disp.from.sf = join_patch_by_year(
  data.disp.from.sf,
  patches,
  patch_col = "lyr.1") %>%
  dplyr::rename(patch_from = patch_id)
# Apply to destinations
data.disp.to.sf = join_patch_by_year(
  data.disp.to.sf,
  patches,
  patch_col = "lyr.1") %>%
  dplyr::rename(patch_to = patch_id)

# Flag NAs
sum(is.na(data.disp.from.sf$patch_from))
sum(is.na(data.disp.to.sf$patch_to))

### To lines -----
# Create LINESTRING geometries
disp.lines = rbind(data.disp.from, data.disp.to) # Duplicated rows for creating dispersal lines
disp.lines = disp.lines %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), na.fail = FALSE, crs=crs(regions)) %>% 
  dplyr::group_by(disp_id) %>% 
  dplyr::summarize() %>%
  dplyr::filter(sf::st_geometry_type(.) == "MULTIPOINT") %>%
  sf::st_cast("LINESTRING")

# Plot the lines
plot(raster_lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(regions), col="magenta", add=TRUE)
plot(sf::st_geometry(disp.lines), col = "deeppink", lwd = 1.5, add=TRUE)

### Join to lines -----
# Recombine
disp.final = disp.lines %>%
  dplyr::left_join(data.disp.from.sf %>% sf::st_drop_geometry() %>% dplyr::select(disp_id, patch_from), by="disp_id") %>%
  dplyr::left_join(data.disp.to.sf %>% sf::st_drop_geometry() %>% dplyr::select(disp_id, patch_to), by="disp_id")

# Flag inter- and intra-patch movement
# If the origin and destination are the same patches -> 1
disp.final = disp.final %>%
  dplyr::mutate(
    intra_patch = ifelse(patch_from == patch_to, 1, 0)
  )
table(disp.final$intra_patch)

# Join GLT information
disp.final.glt = disp.final %>% 
  dplyr::left_join(data.disp.filter.grp %>% dplyr::select(c(Year,DateObs,GLT,Tattoo,disp_id,SexOK,IdadeOK)), by=c("disp_id"))

# Join corridor info
# Bind all corridors into one large dataset
corridor_all = do.call(rbind, corridors) %>%
  dplyr::mutate(
    corridor_id = as.character(corridor_id),
    year = as.numeric(year)
  )
# Create pairs of patches
corridor_all = corridor_all %>%
  dplyr::mutate(
    patch_pair = purrr::map2_chr(patch_from, patch_to,
                           ~ paste(sort(c(.x, .y)), collapse = "_"))
  )
disp.final.glt = disp.final.glt %>%
  dplyr::mutate(patch_from = as.character(patch_from),
                patch_to   = as.character(patch_to),
                patch_pair = purrr::map2_chr(patch_from, patch_to,
                         ~ paste(sort(c(.x, .y)), collapse = "_"))
)
# Join corridors to data
disp.final.glt.corr = disp.final.glt %>%
  dplyr::left_join(
    corridor_all %>%
      sf::st_drop_geometry() %>% 
      dplyr::select(corridor_id, year, patch_pair, type),
    by = c("Year" = "year", "patch_pair")
  )