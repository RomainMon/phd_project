#------------------------------------------------#
# Author: Romain Monassier
# Objective: Reclass rasters -part 2 (forest categories and conservation activities)
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(here)
library(terra)
library(purrr)
library(stringr)
library(ggplot2)
library(raster)
library(sf)

### Import data -------
## Rasters
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}

## Vectors
bridges = vect(here("data", "geo", "AMLD", "Passagens_ARTERIS", "work", "road_overpasses_clean.shp"))
plantios = vect(here("data", "geo", "AMLD", "plantios", "work", "plantios_clean.shp"))
plantios_sf = sf::st_as_sf(plantios)
car = vect(here("data", "geo", "IBGE", "cadastro_car", "AREA_IMOVEL_RJ_2024", "AREA_IMOVEL_bbox.shp"))
pda = vect(here("data", "geo", "MMA", "protected_areas", "ucs", "poco_das_antas.shp"))
uniao = vect(here("data", "geo", "MMA", "protected_areas", "ucs", "uniao.shp"))
tres_picos = vect(here("data", "geo", "MMA", "protected_areas", "ucs", "tres_picos.shp"))
rppn = vect(here("data", "geo", "AMLD", "RPPN", "RPPN_RJ.shp"))
bbox = vect(here("data", "geo", "BBOX", "sampling_units_bbox_31983.shp"))

### Functions ---------

#### Apply linear features -----
# Here, we overlay vector linear features to the rasters using a buffer width and assign the intersected cells a new numeric value
# -> Adapt the buffer width and value according to the linear feature (for instance: roads = 15m and 5 for artificial)
apply_linear_feature_single = function(r, yr, feature, buffer_width, value, use_date) {
  feat_valid = if (use_date && "date_crea" %in% names(feature)) {
    feature[feature$date_crea <= yr, ]
  } else feature
  
  if (nrow(feat_valid) > 0) {
    feat_buff = terra::buffer(feat_valid, width = buffer_width)
    feat_rast = terra::rasterize(feat_buff, r, field = value, background = NA)
    r = cover(feat_rast, r)
  }
  
  return(r)  # single SpatRaster
}



#### Assign values if intersects --------
# This function tests whether a raster value intersects a vector object, and reclassifies these cells with a new value if TRUE
assign_if_intersect = function(raster, vect, target_values, new_value) {
  if(nrow(vect) == 0) return(raster)  # nothing to do
  vect_rast = terra::rasterize(vect, raster, field = 1, background = NA)
  raster[raster %in% target_values & !is.na(vect_rast)] = new_value
  raster
}

# This function assigns a value if intersect AND according to the year
assign_if_intersect_year = function(raster, year, vect, year_field,
                                    target_values, new_value){
  if (nrow(vect) == 0) return(raster)
  # keep only vectors already present at this year
  vect_year = vect[!is.na(vect[[year_field]]) & vect[[year_field]] <= year, ]
  if (nrow(vect_year) == 0) return(raster)
  vect_rast = terra::rasterize(vect_year, raster, field = 1, background = NA)
  raster[raster %in% target_values & !is.na(vect_rast)] = new_value
  raster
}

# This function assigns a value if intersect AND attribute matches AND according to the year
assign_if_intersect_attr_year = function(raster, year, vect,
                                         year_field,
                                         attr, attr_value,
                                         target_values, new_value){
  
  if (nrow(vect) == 0) return(raster)
  # keep only vectors already present at this year
  vect_year = vect[!is.na(vect[[year_field]]) & vect[[year_field]] <= year, ]
  if (nrow(vect_year) == 0) return(raster)
  # keep only desired attribute
  vect_sub = vect_year[vect_year[[attr]] == attr_value, ]
  if (nrow(vect_sub) == 0) return(raster)
  vect_rast = terra::rasterize(vect_sub, raster, field = 1, background = NA)
  raster[raster %in% target_values & !is.na(vect_rast)] = new_value
  raster
}

### Reclass ------
#### 1. Forest categories ------
# We classify the rasters and distinguish between forest categories

message("Classifying forests depending on their legal status...")

cu = rbind(uniao, pda, tres_picos)

rasters_forest_cat = lapply(rasters, function(r){
  
  # Start only with forest = 1
  
  # 1) RPPN (overrides everything)
  r = assign_if_intersect(r, rppn, target_values = 1, new_value = 12)
  
  # 2) Private inside conservation units (CAR ∩ CU)
  r = assign_if_intersect(r, intersect(car, cu), target_values = 1, new_value = 14)
  
  # 3) Public conservation units (remaining forest in CU)
  r = assign_if_intersect(r, cu, target_values = 1, new_value = 13)
  
  # 4) Private properties (remaining forest in CAR)
  r = assign_if_intersect(r, car, target_values = 1, new_value = 11)
  
  # 5) Remaining forest
  r[r == 1] = 10
  
  # 6) Everything else = NA
  r[!r %in% c(10,11,12,13,14)] = NA
  
  r
})

# Check
# pick the rasters
r_before = rasters[[36]]
r_after = rasters_forest_cat[[36]]
freq(rasters_forest_cat[[36]])

# zoom extent from reserve or car region
zoom_ext = ext(pda) + 5000

# crop rasters
rb = crop(r_before, zoom_ext)
ra = crop(r_after,  zoom_ext)

# to sf for plotting
car_sf = sf::st_as_sf(car)

# define a color table
par(mfrow=c(1,2))

plot(rb,
     main="BEFORE",
     col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))

plot(pda, add=TRUE, border="yellow", lwd=2)
plot(rppn, add=TRUE, border="gold3", lwd=2)
plot(car_sf, add=TRUE, col=scales::alpha("white",0.5), border=NA)

plot(ra,
     main="AFTER",
     col = c("#32a65e", "#006400", "#CDAD00", "#C1FFC1", "#CDCDC1"))

par(mfrow=c(1,1))


##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_reclass_forest_cat")

# Export each raster with year in the filename
for (i in seq_along(rasters_forest_cat)) {
  year_i = years[i]
  output_path = file.path(output_dir, paste0("raster_reclass_forest_cat_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_forest_cat[[i]],
    filename = output_path,
    overwrite = TRUE, # Overwrite existing files or not
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}

#### 2. Conservation categories ------
# We classify the rasters and distinguish between types of connectivity or habitat restoration

# Reclassify plantios
rasters_restor_cat = purrr::map2(rasters, years, function(r, yr){
  
  # 1) forest intersecting plantios with Ecologia == "Corredor" → 51
  r = assign_if_intersect_attr_year(r, yr, plantios,
                                    year_field = "date_refor",
                                    attr = "Ecologia",
                                    attr_value = "Corredor",
                                    target_values = 1,
                                    new_value = 51)
  
  # 2) remaining forest intersecting any plantios → 50
  r = assign_if_intersect_year(r, yr, plantios,
                               year_field = "date_refor",
                               target_values = 1,
                               new_value = 50)
  
  r
})

# Apply road overpasses
rasters_cons_cat = purrr::map2(rasters_restor_cat, years, function(r, yr) {
  message("  - Applying linear features for year ", yr)
  r_lin = r
  r_lin = apply_linear_feature_single(r_lin, yr, bridges, buffer_width = 20, value = 52, use_date = TRUE)
  r_lin
})

# Reclass all the rest into NA
codes = c(50, 51, 52)
rasters_cons_cat = lapply(rasters_cons_cat, function(r){
  r[!r %in% codes] = NA
  r
})

# Drop rasters with only NA AND keep matching years
keep = sapply(rasters_cons_cat, function(r) {
  if (is.null(r)) return(FALSE)
  terra::global(!is.na(r), "sum", na.rm = TRUE)[1,1] > 0
})

rasters_cons_cat = rasters_cons_cat[keep]
years_cons_cat = years[keep]


# Check
freq(rasters_cons_cat[[21]])

# pick the rasters
r_before = rasters[[21]]
r_after = rasters_cons_cat[[21]]

# zoom extent from reserve or car region
zoom_ext = ext(pda) + 500

# crop rasters
rb = crop(r_before, zoom_ext)
ra = crop(r_after,  zoom_ext)
par(mfrow=c(1,2))

plot(rb,
     main="BEFORE",
     col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(plantios, add=TRUE, border="white", lwd=1)

plot(ra,
     main="AFTER",
     col = c("#FF7F00", "#EE30A7", "darkred"))
plot(plantios, add=TRUE, border="white", lwd=1)

par(mfrow=c(1,1))

##### Export rasters ---------
message("Exporting rasters...")

# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_reclass_cons_cat")

# Export each raster with year in the filename
for (i in seq_along(rasters_cons_cat)) {
  year_i = years_cons_cat[i]
  output_path = file.path(output_dir,
                          paste0("raster_reclass_cons_cat_", year_i, ".tif"))
  
  message("  - Writing raster for year ", year_i)
  
  terra::writeRaster(
    rasters_cons_cat[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}


### Rasters consistency -------
# Check that all rasters have the same extent
ext(rasters[[1]])
ext(rasters_forest_cat[[1]])
ext(rasters_cons_cat[[1]])

# Spatial coordinates
round(ext(project(rasters[[36]], "EPSG:4326")),2)
expanse(terra::vect(ext(rasters[[36]]), "EPSG:31983")) / 1e6 # Area in km²
