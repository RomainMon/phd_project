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

### Load functions -----
source(here::here("R","APonchon","progress_bar.R"))

### Import datasets -----
load(here("data", "glt", "APonchon", "data_clean_long_final.RData"))
regions = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions.csv = read.table(here("data", "geo", "APonchon", "GLT", "RegionsName.csv"),
                header=T,sep=";")
raster_lulc_2024 = terra::rast(here("outputs", "data", "MapBiomas", "Rasters_reclass", "raster_reclass_2024.tif"))
patches_2024 = sf::st_read(here("outputs","data","patchmetrics","patches_metrics_area_enn_2024.gpkg"))
  
# Plot
plot(raster_lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(regions), col="magenta", add=TRUE)
plot(sf::st_geometry(patches_2024), col=NA, add=TRUE)

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

# Filter data.disp to keep only rows where both FromGroup and ToGroup are in regions
data.disp.filter = data.disp %>%
  dplyr::filter(
    FromGroup %in% regions.csv$Abreviation,
    ToGroup %in% regions.csv$Abreviation
  )

# Summary statistics
# 1. Number of distinct GLTs
data.disp.filter %>%
  dplyr::distinct(GLT) %>%
  nrow()

# 2. Observation period
paste(
  min(data.disp.filter$Year),
  "to",
  max(data.disp.filter$Year)
)

# 3. Number of observations per year
data.disp.filter %>%
  dplyr::count(Year, name = "Observations") %>%
  dplyr::arrange(Year) %>% 
  print(n=31)

# 4. Sex ratio
data.disp.filter %>%
  dplyr::count(SexOK) %>%
  dplyr::mutate(Proportion = n / sum(n))

# 5. Frequency of individuals by age
data.disp.filter %>%
  dplyr::count(IdadeOK, name = "Frequency") %>%
  dplyr::arrange(dplyr::desc(Frequency))

# 6. Number of groups GLTs departed from or immigrated to
data.disp.filter %>%
  dplyr::distinct(FromGroup) %>%
  nrow()
data.disp.filter %>%
  dplyr::distinct(ToGroup) %>%
  nrow()

# 7. To-From associations (Groups, Regions, UMMPs)
data.disp.filter %>%
  dplyr::count(FromGroup, ToGroup) %>% 
  dplyr::arrange(dplyr::desc(n))

data.disp.filter %>%
  dplyr::count(FromRegion, ToRegion)  %>% 
  dplyr::arrange(dplyr::desc(n))

data.disp.filter %>%
  dplyr::count(FromUMMP, ToUMMP)  %>% 
  dplyr::arrange(dplyr::desc(n))

### To dispersal lines ------
# Reformat regions
loc = regions.csv %>% 
  dplyr::rename(Long=CENTROIDE_X_UTM_SAD69_23S,
                Lat=CENTROIDE_Y_UTM_SAD69_23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter_all(any_vars(!is.na(.)))

# Join group location
data.disp.from = data.disp.filter %>% 
  dplyr::left_join(loc %>% dplyr::select(c(Group,Long,Lat)), by=c("FromGroup"="Group"), multiple="first")
data.disp.to = data.disp.filter %>% 
  dplyr::left_join(loc %>% dplyr::select(c(Group,Long,Lat)), by=c("ToGroup"="Group"), multiple="first")
data.disp.spatial = rbind(data.disp.from, data.disp.to)

# Create LINESTRING geometries
disp.lines = data.disp.spatial %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), na.fail = FALSE, crs=crs(regions)) %>% 
  dplyr::group_by(disp_id) %>% 
  dplyr::summarize() %>%
  dplyr::filter(sf::st_geometry_type(.) == "MULTIPOINT") %>%
  sf::st_cast("LINESTRING")

# Plot the lines
plot(raster_lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(regions), col="magenta", add=TRUE)
plot(sf::st_geometry(disp.lines), col = "deeppink", lwd = 1.5, add=TRUE)

# Join information
disp.lines = disp.lines %>% 
  dplyr::left_join(data.disp.filter, by="disp_id")
