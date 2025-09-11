#------------------------------------------------#
# Author: Romain Monassier
# Objective: Plot landscape metrics through time
#------------------------------------------------#

library(tidyr)
library(ggplot2)
library(readr)


#### Import datasets ----------------
base_path = here("outputs", "data", "landscapemetrics")
class_metrics <- readr::read_csv(
  file.path(base_path, "class_metrics_bbox_1989_2023.csv"),
  show_col_types = FALSE
)
forest_class_metrics <- readr::read_csv(
  file.path(base_path, "forest_class_metrics_bbox_1989_2023.csv"),
  show_col_types = FALSE
)

#### Stacked area chart ----------------
# Create color palette using the Legend codes provided by MapBiomas
class_colors <- tibble::tibble(
  Class_ID = factor(c(1,3,4,5,6,49,10,11,12,32,29,50,14,15,18,19,39,20,40,62,41,36,46,47,35,48,9,21,22,23,24,30,25,26,33,31,27)),
  Description = c(
    "Forest","Forest Formation","Savanna Formation","Mangrove","Floodable Forest","Wooded Sandbank Vegetation",
    "Herbaceous and Shrubby Vegetation","Wetland","Grassland","Hypersaline Tidal Flat","Rocky Outcrop",
    "Herbaceous Sandbank Vegetation","Farming","Pasture","Agriculture","Temporary Crop","Soybean","Sugar cane",
    "Rice","Cotton (beta)","Other Temporary Crops","Perennial Crop","Coffee","Citrus","Palm Oil",
    "Other Perennial Crops","Forest Plantation","Mosaic of Uses","Non vegetated area","Beach, Dune and Sand Spot",
    "Urban Area","Mining","Other non Vegetated Areas","Water","River, Lake and Ocean","Aquaculture","Not Observed"
  ),
  Color = c(
    "#32a65e","#1f8d49","#7dc975","#04381d","#026975","#02d659",
    "#ad975a","#519799","#d6bc74","#fc8114","#ffaa5f","#ad5100",
    "#FFFFB2","#edde8e","#E974ED","#C27BA0","#f5b3c8","#db7093",
    "#c71585","#ff69b4","#f54ca9","#d082de","#d68fe2","#9932cc",
    "#9065d0","#e6ccff","#7a5900","#ffefc3","#d4271e","#ffa07a",
    "#d4271e","#9c0027","#db4d4f","#0000FF","#2532e4","#091077","#ffffff"
  )
)

##### Plot proportions of area covered by each land use ----------
# Select data
data = class_metrics %>% 
  dplyr::select(year, class, pland) %>% 
  dplyr::mutate(
    year  = as.integer(year),          # x axis must be numeric/continuous
    class = as.factor(class),          # fill should be discrete
    pland = as.numeric(pland)          # ensure numeric
  )

# Collapse all classes < 1% per year into "Other"
data = data %>% 
  dplyr::group_by(year, class) %>%
  dplyr::summarise(pland = sum(pland, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(class = dplyr::if_else(pland < 1, "Other", as.character(class))) %>%
  dplyr::group_by(year, class) %>%
  dplyr::summarise(pland = sum(pland), .groups = "drop") %>%
  dplyr::mutate(class = factor(class))

# Add "Other" to the color palette
class_colors <- class_colors %>% 
  dplyr::add_row(
    Class_ID = factor("Other"),
    Description = "Other",
    Color = "#808080"
  )

# Add color codes
data = data %>% 
  dplyr::left_join(class_colors, by = c("class" = "Class_ID"))

# Plot
ggplot(data, aes(x = year, y = pland, fill = Description, group = Description)) +
  geom_area(alpha = 0.7, position = "stack") +  # Stacked areas
  geom_line(aes(color = Description), size = 1, position = "stack") +  # Lines colored by class
  geom_point(aes(color = Description), size = 1, position = "stack") + # Points colored by class
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  scale_color_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  labs(x = "Year", y = "Percentage of landscape", fill = "Land use", color = "Land use") +
  theme_classic()


##### Plot total areas of each land use ----------
# Select data
data = class_metrics %>% 
  dplyr::select(year, class, ca) %>% 
  dplyr::filter(class == "3" | class == "15" | class == "21" | class == "24") %>% 
  dplyr::mutate(
    year  = as.integer(year),          # x axis must be numeric/continuous
    class = as.factor(class),          # fill should be discrete
    ca = as.numeric(ca)          # ensure numeric
  )

# Add color codes
data = data %>% 
  dplyr::left_join(class_colors, by = c("class" = "Class_ID"))

# Plot
ggplot(data, aes(x = year, y = ca, fill = Description, group = Description)) +
  geom_area(position = "identity", alpha = 0.5) +        # Non-stacked areas
  geom_line(aes(color = Description), size = 1) +        # Line on top of each area
  geom_point(aes(color = Description), size = 2) +       # Points at each data value
  scale_fill_manual(values = unique(data$Color)) +       # Use your custom colors
  scale_color_manual(values = unique(data$Color)) +      # Lines/points match areas
  labs(x = "Year", y = "Total area", fill = "Land use", color = "Land use") +
  theme_classic()


#### Line plot -----------
# Pivot metrics to long format
data = forest_class_metrics %>%
  dplyr::filter(class==3) %>% 
  dplyr::select(-c(level, area_sd))
data = data %>% 
  tidyr::pivot_longer(
    cols = -c(year, class),
    names_to = "metric",
    values_to = "value"
  ) %>%
  dplyr::mutate(year = as.integer(year)) %>% 
  dplyr::mutate(
    metric = recode(metric,
                    ca = "Total forest area",
                    np = "Number of patches",
                    area_mn = "Mean patch area",
                    ai = "Aggregation index"),
    metric = factor(metric, levels = c("Total forest area",
                                       "Number of patches",
                                       "Mean patch area",
                                       "Aggregation index"))
  )

# Create faceted line plot
ggplot(data, aes(x = year, y = value, color = metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +  # two columns
  scale_color_brewer(palette = "Set2") +               # distinct colors
  labs(
    x = "Year",
    y = "Value",
    color = "Metric",
    title = "Forest Class Metrics Through Time"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

