#------------------------------------------------#
# Author: Romain Monassier
# Objective: Correct regions shapefile (group locations)
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(sf)

### Import dataset ------
## Regions
regions = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))

## Rasters reclassified
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
plot(rasters_reclass[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))


### Correct ------
regions = regions %>% 
  dplyr::mutate(Farm2 = dplyr::case_when(
    Farm == "Faz. Boa Esperanca" & Abreviatio == "OL2" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Boa Esperanca" & Abreviatio == "AF8" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Cabana nova Conquista" & Abreviatio == "JG" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Sao Joao" & Abreviatio == "IR" ~ "Faz. Sao Joao 2",
    Farm == "Faz. Iguape" & Abreviatio == "SK" ~ "Faz. Igarape",
    Farm == "Faz.Afetiva" | Farm == "Fazenda Afetiva" ~ "Faz. Afetiva",
    Farm == "Faz. Sao Francisco" & Abreviatio == "KS" ~ "Faz. Igarape",
    Farm == "Rebio Uniao" & Abreviatio == "LB" ~ "Rebio Uniao Sul",
    TRUE ~ Farm
  ))

### Check inconsistencies -------
sf::st_crs(regions)
crs(rasters_reclass[[36]])
farms = unique(regions$Farm2)

for (f in farms) {
  
  # Subset farm polygons
  reg_f = regions %>% dplyr::filter(Farm2 == f)
  
  # Get bbox
  bbox = sf::st_bbox(reg_f)
  
  # Expand bbox by 500 m
  ext_bbox = terra::ext(
    bbox$xmin - 500,
    bbox$xmax + 500,
    bbox$ymin - 500,
    bbox$ymax + 500
  )
  
  # Crop raster
  raster_crop = terra::crop(rasters_reclass[[36]], ext_bbox)
  
  # Plot
  plot(raster_crop,
       main = paste("Farm:", f),
       col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
  
  plot(sf::st_geometry(reg_f), 
       add = TRUE, 
       border = "red", 
       lwd = 2)
  
  readline(prompt = "Press [enter] for next farm")
}

### Correct column names -----
regions.csv = regions %>% 
  sf::st_drop_geometry() %>% 
  dplyr::rename(Abreviation = Abreviatio,
                CENTROIDE_X_UTM_SAD69_23S = CENTROIDE_,
                CENTROIDE_Y_UTM_SAD69_23S = CENTROID_1)


### Export corridors ----------
base_path = here("data", "geo", "APonchon", "GLT")
sf::st_write(regions, file.path(base_path, "RegionsName.shp"), append=FALSE, delete_dsn = TRUE, quiet = TRUE)
write.csv(regions.csv, file = file.path(base_path, "RegionsName.csv"), row.names = FALSE)
