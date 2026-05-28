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
library(extrafont)
library(sp)
library(cowplot)
library(lemon)
library(magick)
loadfonts()


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
primary_roads = primary_roads %>%
  dplyr::mutate(road_type = dplyr::case_when(
    ref == "BR-101" ~ "BR-101",
    TRUE ~ "Main roads"
  ))

# SJ watershed
sj_watershed = terra::vect(here("data", "geo", "AMLD", "hidrografia", "Rio_Sao_Joao_Watershed.shp"))
sj_watershed = terra::project(sj_watershed, "EPSG:31983")
sj_watershed_sf = st_as_sf(sj_watershed)
sj_watershed_sf = sj_watershed_sf %>%
  dplyr::mutate(feature = "SJ watershed")

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

# Brazil
brazil = terra::vect(here("data", "geo", "IBGE", "admin", "BR_Pais_2023", "BR_Pais_2023.shp"))
brazil = terra::project(brazil, "EPSG:31983")
brazil_sf = st_as_sf(brazil)
plot(sf::st_geometry(brazil_sf))


### Crop to extent ----
# Study area (from raster)
study_area = st_as_sfc(st_bbox(raster_lulc_2024))

# Crop vectors
primary_roads = sf::st_intersection(primary_roads, study_area)
sj_watershed_sf = sf::st_intersection(sj_watershed_sf, study_area)
pa_sf = sf::st_intersection(pa_sf, study_area)

### Map -----
#### Inset map (Brazil) -------
study_centroid = sf::st_centroid(study_area)
study_coords = sf::st_coordinates(study_centroid)

inset = ggplot() +
  geom_sf(data = brazil_sf,
          fill = "white",
          color = "black",
          linewidth = 0.2) +
  geom_point(
    aes(x = study_coords[1],
        y = study_coords[2]),
    color = "red",
    size = 2
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey95"),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 0.4)
  )

#### Study area -----
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
              "#D1D100", "#0000FF", "#d4271e")

# PA colors
cols_pa = c(
  "São João River Basin APA" = "black",
  "Poço das Antas" = "black",
  "Três Picos" = "black",
  "União" = "grey60"
)

# Colors for line features
line_cols = c(
  "SJ watershed" = "blue4",
  "Main roads" = "#444444",
  "BR-101" = "#ff3399"
)

# Plot
main = ggplot() +
  geom_raster(data = lulc_df,
              aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = cols_lulc,
                    name = "Land use") +
  
  # Protected areas parameters (alpha, spacing...)
  geom_sf_pattern(
    data = pa_sf,
    aes(pattern = name,
        pattern_color = name),
    fill = NA,
    color = NA,
    pattern_alpha = 1,
    pattern_fill = NA,
    pattern_density = 0.2,
    pattern_spacing = 0.025,
    linewidth = 0.3) +
  
  # Watershed
  geom_sf(
    data = sj_watershed_sf,
    aes(color = feature),
    fill = NA,
    linewidth = 1,
    linetype = "dashed") +
  
  # Roads
  geom_sf(
    data = primary_roads,
    aes(color = road_type),
    linewidth = 1) +
  
  # Optional white casing effect (no legend)
  geom_sf(
    data = primary_roads,
    color = "white",
    linewidth = 0.3,
    show.legend = FALSE) +
  
  # Combined legend for lines
  scale_color_manual(
    name = "Other features",
    values = line_cols,
    breaks = c("SJ watershed", "Main roads", "BR-101")
  ) +
  
  scale_pattern_manual(
    values = c(
      "São João River Basin APA" = "circle",
      "Poço das Antas" = "stripe",
      "Três Picos" = "crosshatch",
      "União" = "weave"
    ),
    name = "Protected areas"
  ) +
  scale_pattern_color_manual(
    values = cols_pa,
    name = "Protected areas"
  ) +
  
  ggspatial::annotation_scale(
    location = "br",
    style = "bar",
    bar_cols = "black",
    width_hint = 0.18,
    pad_x = unit(0.3, "cm"),
    pad_y = unit(0.3, "cm"),
    text_cex = 0.7,
    line_width = 0.7
  ) +
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = ggspatial::north_arrow_nautical(),
    pad_x = unit(0.3, "cm"),
    pad_y = unit(1.2, "cm"),
    height = unit(1.8, "cm"),
    width  = unit(1.8, "cm")
  ) +
  coord_sf(expand = FALSE) +
  theme_bw(base_family = "Arial Narrow") +
  theme(
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
  )

#### Legend --------
legend_plot = ggplot() +
  # Land use legend
  geom_raster(
    data = lulc_df,
    aes(x = x, y = y, fill = value)
  ) +
  # Protected areas legend
  geom_sf_pattern(
    data = pa_sf,
    aes(pattern = name,
        pattern_color = name),
    fill = NA,
    color = NA,
    pattern_alpha = 1,
    pattern_fill = NA,
    pattern_density = 0.2,
    pattern_spacing = 0.025,
    linewidth = 0.3) +
  
  scale_fill_manual(
    values = cols_lulc,
    name = "Land use"
  ) +
  
  scale_pattern_manual(
    values = c(
      "São João River Basin APA" = "circle",
      "Poço das Antas" = "stripe",
      "Três Picos" = "crosshatch",
      "União" = "weave"
    ),
    name = "Protected areas"
  ) +
  scale_pattern_color_manual(
    values = cols_pa,
    guide = "none"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    legend.text = element_text(size = 9)
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      nrow = 1,
      order = 1
    ),
    
    pattern = guide_legend(
      title.position = "top",
      nrow = 1,
      order = 2,
      override.aes = list(
        pattern_colour = c("black", "black", "black", "grey40"),
        fill = "white"
      )
    )
  )
legend = lemon::g_legend(legend_plot)

#### Combine ------
combo = ggdraw() +
  cowplot::draw_plot(main) +
  cowplot::draw_plot(
    inset,
    x = 0.056,
    y = 0.779,
    width = 0.20,
    height = 0.20
  )

### Export ------
png(here("outputs","plot","01p_paper1_study_area.png"), width = 1800, height = 1500, res = 300, type="cairo")

gridExtra::grid.arrange(combo,
             legend,
             nrow=2, 
             heights=c(1,0.25))

dev.off()
