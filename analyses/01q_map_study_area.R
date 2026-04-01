#------------------------------------------------#
# Author: Romain Monassier
# Objective: Map the study area
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(terra)
library(sf)
library(data.table)
library(stringr)
library(ggplot2)
library(ggspatial)
library(ggpattern)
library(sp)

### Import rasters -------

# Land use in 2024
raster_lulc_2024 = terra::rast(here("outputs", "data", "MapBiomas", "Rasters_reclass", "raster_reclass_2024.tif"))
plot(raster_lulc_2024)

### Import vectors -----

# Roads
roads = terra::vect(here("data", "geo", "OSM", "work", "Highway_OSM_clean.shp"))
roads = terra::project(roads, "EPSG:31983")
roads_sf = sf::st_as_sf(roads)
primary_roads = roads_sf %>% 
  dplyr::filter(highway == "motorway" | highway == "trunk" | highway == "primary")
plot(raster_lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(st_geometry(primary_roads), col="#ff3399", add=TRUE)

# SJ watershed
sj_watershed = terra::vect(here("data", "geo", "AMLD", "hidrografia", "Rio_Sao_Joao_Watershed.shp"))
sj_watershed = terra::project(sj_watershed, "EPSG:31983")
sj_watershed_sf = st_as_sf(sj_watershed)

# APA mld
apa_mld = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "apa_mld.shp"))
apa_mld = terra::project(apa_mld, "EPSG:31983")
apa_mld_sf = st_as_sf(apa_mld)

# Poco das Antas
pda = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "poco_das_antas.shp"))
pda = terra::project(pda, "EPSG:31983")
pda_sf = st_as_sf(pda)

# Tres Picos
tp = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "tres_picos.shp"))
tp = terra::project(tp, "EPSG:31983")
tp_sf = st_as_sf(tp)

# Uniao
uniao = terra::vect(here("data", "geo", "MMA", "protected_areas", "ucs", "uniao.shp"))
uniao = terra::project(uniao, "EPSG:31983")
uniao_sf = st_as_sf(uniao)

# Combine PAs
pa_sf = dplyr::bind_rows(
  apa_mld_sf %>% dplyr::mutate(name = "São João River Basin APA"),
  pda_sf %>% dplyr::mutate(name = "Poço das Antas"),
  tp_sf %>% dplyr::mutate(name = "Três Picos"),
  uniao_sf %>% dplyr::mutate(name = "União")
)

### Crop to extent ----
# Study area (from raster)
study_area = st_as_sfc(st_bbox(raster_lulc_2024))

# Crop vectors
primary_roads = sf::st_intersection(primary_roads, study_area)
sj_watershed_sf = sf::st_intersection(sj_watershed_sf, study_area)
pa_sf = sf::st_intersection(pa_sf, study_area)

### Map -----
# Raster to dataframe
lulc_df = as.data.frame(raster_lulc_2024, xy = TRUE, na.rm = TRUE)
names(lulc_df)[3] = "value"

lulc_df$value = factor(lulc_df$value,
                       levels = 1:6,
                       labels = c("Forest",
                                  "Non-forest formation",
                                  "Wetland",
                                  "Agriculture",
                                  "Water",
                                  "Built-up"))

# LULC colors
cols_lulc = c("#32a65e", "#ad975a", "#519799",
              "#FFFFB2", "#0000FF", "#d4271e")

# PA colors
cols_pa = c(
  "São João River Basin APA" = "#a6cee3",
  "Poço das Antas" = "#b2df8a",
  "Três Picos" = "#fb9a99",
  "União" = "#fdbf6f"
)

# Plot
png(here("outputs","plot","01p_paper1_study_area.png"), width = 3000, height = 1700, res = 300)

ggplot() +
  geom_raster(data = lulc_df,
              aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = cols_lulc,
                    name = "Land use") +
  geom_sf_pattern(
    data = pa_sf,
    aes(pattern = name,
        pattern_color = name),
    fill = NA,
    color = NA,
    pattern_fill = NA,
    pattern_density = 0.4,
    pattern_spacing = 0.025,
    linewidth = 0.6) +
  geom_sf(
    data = sj_watershed_sf,
    color = "blue4",
    fill = NA,
    linewidth = 1,
    linetype = "dashed") +
  geom_sf(
    data = primary_roads,
    color = "#444444",
    linewidth = 1) +
  geom_sf(
    data = primary_roads,
    color = "white",
    linewidth = 0.3) +
  scale_pattern_manual(
    values = c(
      "São João River Basin APA" = "circle",
      "Poço das Antas" = "stripe",
      "Três Picos" = "crosshatch",
      "União" = "stripe"
    ),
    name = "Protected areas"
  ) +
  scale_pattern_color_manual(
    values = cols_pa,
    name = "Protected areas"
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    style = "ticks",
    width_hint = 0.15,
    text_cex = 0.6
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering,
    height = unit(1, "cm"),
    width  = unit(1, "cm")
  ) +
  coord_sf() +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

dev.off()