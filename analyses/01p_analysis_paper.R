#------------------------------------------------#
# Authors: Romain Monassier, Valéria Romano, Anne-Marie Farnet Da Silva
# Script accompanying the paper entitled: "..."
#------------------------------------------------#

library(here)
library(dplyr)
library(scales)
library(ggplot2)
library(ggbreak)
library(ggspatial)
library(extrafont)
library(patchwork)
library(glmmTMB)
library(MASS)
library(DHARMa)
library(pROC)
library(performance)
library(spdep)
library(spatialreg)
library(MuMIn)
library(caret)
library(terra)
loadfonts()

# 0. Data -------

### Import datasets ------
## Landscape metrics
base_path = here("outputs", "data", "landscapemetrics")
# All land uses
all_lulc_metrics = readr::read_csv(
  file.path(base_path, "all_lulc_classes_bbox_1989_2100.csv"),
  show_col_types = FALSE)
# Metrics for forest class only
forest_class_metrics = readr::read_csv(
  file.path(base_path, "forest_class_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE)
# Forest age
forest_age_metrics = readr::read_csv(
  file.path(base_path, "forest_age_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE)

## Modeling datasets
base_path = here("outputs", "data", "MapBiomas", "LULCC_datasets")
# Property-scale deforestation dataset
train_data_car_defor = sf::st_read(
  file.path(base_path, "train_data_car_defor.gpkg"))
test_data_car_defor = sf::st_read(
  file.path(base_path, "test_data_car_defor.gpkg"))
# Property-scale reforestation dataset
train_data_car_refor = sf::st_read(
  file.path(base_path, "train_data_car_refor.gpkg"))
test_data_car_refor = sf::st_read(
  file.path(base_path, "test_data_car_refor.gpkg"))
# Pixel-scale deforestation dataset
train_data_pixel_defor = readr::read_csv(
  file.path(base_path, "train_data_pixel_defor.csv"),
  show_col_types = FALSE)
test_data_pixel_defor = readr::read_csv(
  file.path(base_path, "test_data_pixel_defor.csv"),
  show_col_types = FALSE)
# Pixel-scale reforestation dataset
train_data_pixel_refor = readr::read_csv(
  file.path(base_path, "train_data_pixel_refor.csv"),
  show_col_types = FALSE)
test_data_pixel_refor = readr::read_csv(
  file.path(base_path, "test_data_pixel_refor.csv"),
  show_col_types = FALSE)
# Overall property-scale dataset
data_car = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.rds"))

## Cumulative LULCC
raster_tm_2024 = terra::rast(here("outputs", "data", "MapBiomas", "Rasters_cumulative_tm", "raster_reclass_cumul_tm_2024.tif"))
plot(raster_tm_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))

### Summary ------

## Number of sampling units
# Number of pixels (deforestation)
n = train_data_pixel_defor %>% 
  dplyr::bind_rows(test_data_pixel_defor) %>% 
  dplyr::summarise(n=dplyr::n_distinct(cell_id)) %>% 
  dplyr::pull(n)
cat("There are:", n, "pixels in the total (training + testing) deforestation dataset")
# Number of pixels (reforestation)
n = train_data_pixel_refor %>% 
  dplyr::bind_rows(test_data_pixel_refor) %>% 
  dplyr::summarise(n=dplyr::n_distinct(cell_id)) %>% 
  dplyr::pull(n)
cat("There are:", n, "pixels in the total (training + testing) reforestation dataset")
# Number of properties (deforestation)
n = train_data_car_defor %>% 
  dplyr::bind_rows(test_data_car_defor) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::summarise(n=dplyr::n_distinct(car_id))%>% 
  dplyr::pull(n)
cat("There are:", n, "distinct properties in the total (training + testing) deforestation dataset")
# Number of properties (reforestation)
n = train_data_car_refor %>% 
  dplyr::bind_rows(test_data_car_refor) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::summarise(n=dplyr::n_distinct(car_id)) %>% 
  dplyr::pull(n)
cat("There are:", n, "distinct properties in the total (training + testing) reforestation dataset")

## Number of pixels per year
# Deforestation
top_years = train_data_pixel_defor %>%
  dplyr::bind_rows(test_data_pixel_defor) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n = dplyr::n_distinct(cell_id)) %>%
  dplyr::mutate(prop = n * 100 / sum(n)) %>%
  dplyr::arrange(desc(prop)) %>%
  dplyr::slice(1:3)
cat(
  "The 3 years with the most deforestation changes are:",
  paste(top_years$year, collapse = ", "),
  "with respectively",
  paste(round(top_years$prop, 2), "%", collapse = ", "),
  "of the total deforestation pixels."
)
# Reforestation
top_years = train_data_pixel_refor %>%
  dplyr::bind_rows(test_data_pixel_refor) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n = dplyr::n_distinct(cell_id)) %>%
  dplyr::mutate(prop = n * 100 / sum(n)) %>%
  dplyr::arrange(desc(prop)) %>%
  dplyr::slice(1:3)
cat(
  "The 3 years with the most reforestation changes are:",
  paste(top_years$year, collapse = ", "),
  "with respectively",
  paste(round(top_years$prop, 2), "%", collapse = ", "),
  "of the total deforestation pixels."
)

## Amount of deforestation and reforestation
resolution = res(raster_tm_2024)[1]
# Convert resolution to cell area in m²
cell_area_m2 = resolution * resolution
# Count pixels for each value
freq_7 = freq(raster_tm_2024, value = 7)
freq_8 = freq(raster_tm_2024, value = 8)
# Calculate total area in m² and ha
area_7_m2 = freq_7$count * cell_area_m2
area_7_ha = area_7_m2 / 10000
area_8_m2 = freq_8$count * cell_area_m2
area_8_ha = area_8_m2 / 10000

# Print results
cat(
  "Surface area of reforestation (value 7):\n",
  paste(round(area_7_m2, 2), "m² (", round(area_7_ha, 2),"ha)\n"),
  "\nSurface area of deforestation (value 8):\n",
  paste(round(area_8_m2, 2), "m² (", round(area_8_ha, 2),"ha)\n")
)

## Amount of forest, deforestation, and reforestation by property
# Less then 20% of forest in 2024
data_car = data_car %>% 
  dplyr::mutate(Less20For2024 = dplyr::case_when(prop_forest_2024 <= 0.20 ~ 1, TRUE ~ 0),
                Less20For1989 = dplyr::case_when(prop_forest_1989 <= 0.20 ~ 1, TRUE ~ 0))
# Calculate proportions of properties with <20% forest in 1989 and 2024
less20_1989 = data_car %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Less20For1989) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(prop = n * 100 / sum(n))
less20_2024 = data_car %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Less20For2024) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(prop = n * 100 / sum(n))
cat(
  "Proportion of properties with less than 20% forest:\n",
  "\n1989:",
  paste(round(less20_1989$prop[less20_1989$Less20For1989 == 1], 2), "%"),
  "\n2024:",
  paste(round(less20_2024$prop[less20_2024$Less20For2024 == 1], 2), "%"))

# Calculate mean and sd for deforestation and reforestation
def_mean_sd = data_car %>%
  sf::st_drop_geometry() %>%
  dplyr::summarise(mean = mean(area_deforest_ha, na.rm = TRUE),
                   sd = sd(area_deforest_ha, na.rm = TRUE))
refor_mean_sd = data_car %>%
  sf::st_drop_geometry() %>%
  dplyr::summarise(mean = mean(area_reforest_ha, na.rm = TRUE),
                   sd = sd(area_reforest_ha, na.rm = TRUE))
# Print results
cat(
  "\nMean ± SD deforestation (ha):",
  paste(round(def_mean_sd$mean, 2), "±", round(def_mean_sd$sd, 2)),
  "\nMean ± SD reforestation (ha):",
  paste(round(refor_mean_sd$mean, 2), "±", round(refor_mean_sd$sd, 2))
)
# Calculate mean and sd for deforestation and reforestation (>0)
def_mean_sd = data_car %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(area_deforest_ha>0) %>% 
  dplyr::summarise(mean = mean(area_deforest_ha, na.rm = TRUE),
                   sd = sd(area_deforest_ha, na.rm = TRUE))
refor_mean_sd = data_car %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(area_reforest_ha>0) %>% 
  dplyr::summarise(mean = mean(area_reforest_ha, na.rm = TRUE),
                   sd = sd(area_reforest_ha, na.rm = TRUE))
# Print results
cat(
  "\nMean ± SD deforestation (ha) for properties that have deforested:",
  paste(round(def_mean_sd$mean, 2), "±", round(def_mean_sd$sd, 2)),
  "\nMean ± SD reforestation (ha) for properties that have reforested:",
  paste(round(refor_mean_sd$mean, 2), "±", round(refor_mean_sd$sd, 2))
)


# 1. Landscape changes ----------

### Summary -------

#### Land use in 2024 -------
data = all_lulc_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::select(class, ca, pland) %>%
  dplyr::arrange(desc(pland))
text = data %>%
  dplyr::mutate(txt = paste0(class, " covered ", round(ca), " ha (", round(pland,1), "%) in 2024.")) %>%
  dplyr::pull(txt)
cat("Land-use composition in 2024 was as follows:\n")
cat(paste0("* ", text, collapse="\n"), "\n\n")

#### LULCC from 1989 to 2024 -------
data = all_lulc_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(
    pland1989 = pland[year==1989],
    pland2024 = pland[year==2024],
    d_pland = pland2024 - pland1989
  ) %>%
  dplyr::ungroup()
text = data %>%
  dplyr::mutate(
    txt = paste0(
      class, " cover represented ", round(pland1989,1), "% of the landscape in 1989 ",
      "while it represents ", round(pland2024,1), "% of the landscape in 2024 (",
      ifelse(d_pland>=0, "+",""), round(d_pland,1), " %)."
    )
  ) %>%
  dplyr::pull(txt)
cat("Changes between 1989 and 2024 were:\n")
cat(paste0("* ", text, collapse="\n"), "\n")

#### LULCC from 2024 to 2100 -------
data = all_lulc_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(
    pland2024 = pland[year==2024],
    pland2100 = pland[year==2100],
    d_pland = pland2100 - pland2024
  ) %>%
  dplyr::ungroup()
text = data %>%
  dplyr::mutate(
    txt = paste0(
      class, " cover represented ", round(pland2024,1), "% of the landscape in 2024 ",
      "while it represents ", round(pland2100,1), "% of the landscape in 2100 (",
      ifelse(d_pland>=0, "+",""), round(d_pland,1), " %)."
    )
  ) %>%
  dplyr::pull(txt)
cat("Predicted changes between 2024 and 2100 are:\n")
cat(paste0("* ", text, collapse="\n"), "\n")

#### LULCC from 2024 to 2100 (change in surface) -------
data = all_lulc_metrics %>%
  dplyr::select(year, class, ca) %>% 
  dplyr::mutate(
    year = as.integer(year),
    class = as.numeric(class),
    ca = as.numeric(ca)
  )
data = data %>%
  dplyr::group_by(class) %>%
  dplyr::arrange(year) %>%
  dplyr::summarise(
    year_end  = dplyr::last(year),
    ca_end = dplyr::last(ca),
    ca_start = dplyr::first(ca),
    change_ha = ca_end - ca_start,
    pct_change = 100 * change_ha / ca_start,
    .groups = "drop"
  )
text = data %>%
  dplyr::mutate(
    txt = paste0(
      class, " ",
      ifelse(change_ha >= 0, "increased", "decreased"),
      " by ", scales::comma(abs(round(change_ha))), " ha (",
      sprintf("%+.1f", pct_change), "%).")
  ) %>%
  dplyr::pull(txt)
cat("Long-term land-use changes are:\n")
cat(paste0("* ", text, collapse = "\n"), "\n\n")

#### Forest metrics (1989-2024) -------
data = forest_class_metrics %>%
  dplyr::select(year, ca, np, area_mn, FFI) %>% 
  dplyr::filter(year %in% c(1989, 2024)) %>%
  dplyr::group_by(year) %>%
  tidyr::pivot_longer(cols = -year, names_to = "metric", values_to = "value") %>% 
  tidyr::pivot_wider(names_from = year, values_from = value) %>%
  dplyr::mutate(
    change = `2024` - `1989`,
    pct_change = 100 * change / `1989`
  ) %>%
  dplyr::select(metric, `1989`, `2024`, change, pct_change) %>% 
  dplyr::mutate(`1989` = round(`1989`,2),
                `2024` = round(`2024`, 2))
cat("Forest metrics change from 1989 to 2024:\n")
text = data %>%
  dplyr::mutate(
    txt = paste0(
      metric, ": ",
      `1989`, " to ", `2024`, " (",
      ifelse(change >= 0, "+", "-"), abs(round(change, 2)), ", ",
      sprintf("%+.1f", pct_change), "%)")
  ) %>%
  dplyr::pull(txt)
cat(paste0("* ", text, collapse = "\n"), "\n")

#### Forest age ------
data = forest_age_metrics %>%
  dplyr::mutate(
    AgeClass = factor(
      dplyr::case_when(
        class == 10 ~ "<=10 years",
        class == 20 ~ "11–20 years",
        class == 30 ~ "21–30 years",
        class == 31 ~ ">30 years"),
      levels = c("<=10 years", "11–20 years", "21–30 years", ">30 years"))) %>% 
  dplyr::filter(year == 2024)
text = data %>%
  dplyr::mutate(txt = paste0(AgeClass, " covered ", round(ca), " ha (", round(pland,1), "%) in 2024.")) %>%
  dplyr::pull(txt)
cat("Forest age in 2024 was as follows:\n")
cat(paste0("* ", text, collapse="\n"), "\n\n")

### Figures -------
# Color palette using the Legend codes provided by MapBiomas
class_colors = tibble::tibble(
  class = c(1,2,3,4,5,6),
  Description = c("Forest", "Non-forest formation", "Wetland", "Agriculture", "Water", "Built-up"
  ),
  Color = c(
    "#32a65e", "#ad975a", "#519799", "#D1D100", "#0000FF", "#d4271e"
  )
)

#### Land use in 1989, 2024, 2100 ----
## 1989
data_1989 = all_lulc_metrics %>%
  dplyr::filter(year == 1989) %>%
  dplyr::left_join(class_colors, by="class") %>% 
  dplyr::group_by(Description) %>%
  dplyr::summarise(ca = sum(ca), .groups="drop") %>%
  dplyr::mutate(p = round(ca/sum(ca),3)) %>% 
  dplyr::left_join(class_colors, by="Description")
data_1989 = data_1989 %>%
  dplyr::mutate(pct_label = paste0(Description, " (", round(p*100, 2), "%)")) %>%
  dplyr::mutate(n = round(p*100)) %>%
  dplyr::mutate(n = ifelse(n==0,1,n)) %>%
  dplyr::mutate(n = round(n/sum(n)*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = dplyr::row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10)
waffle1989 = ggplot(data_1989, aes(x, y, fill = Description)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_manual(
    values = setNames(class_colors$Color, class_colors$Description),
    labels = setNames(data_1989$pct_label, data_1989$Description),
    name = NULL
  ) +
  theme_void() +
  ggtitle("b) LULC in 1989") +
  theme(legend.position = "none",
        plot.title = element_text(
          family = "Arial Narrow",
          face = "bold",
          size = 12,
          hjust = 0.5))


## 2024
data_2024 = all_lulc_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::left_join(class_colors, by="class") %>% 
  dplyr::group_by(Description) %>%
  dplyr::summarise(ca = sum(ca), .groups="drop") %>%
  dplyr::mutate(p = round(ca/sum(ca),3)) %>% 
  dplyr::left_join(class_colors, by="Description")
data_2024 = data_2024 %>%
  dplyr::mutate(pct_label = paste0(Description, " (", round(p*100, 2), "%)")) %>%
  dplyr::mutate(n = round(p*100)) %>%
  dplyr::mutate(n = ifelse(n==0,1,n)) %>%
  dplyr::mutate(n = round(n/sum(n)*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = dplyr::row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10)
waffle2024 = ggplot(data_2024, aes(x, y, fill = Description)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_manual(
    values = setNames(class_colors$Color, class_colors$Description),
    labels = setNames(data_2024$pct_label, data_2024$Description),
    name = NULL
  ) +
  theme_void() +
  ggtitle("c) LULC in 2024") +
  theme(legend.position = "none",
        plot.title = element_text(
          family = "Arial Narrow",
          face = "bold",
          size = 12,
          hjust = 0.5))


## 2100
data_2100 = all_lulc_metrics %>%
  dplyr::filter(year == 2100) %>%
  dplyr::left_join(class_colors, by="class") %>% 
  dplyr::group_by(Description) %>%
  dplyr::summarise(ca = sum(ca), .groups="drop") %>%
  dplyr::mutate(p = round(ca/sum(ca),3)) %>% 
  dplyr::left_join(class_colors, by="Description")
data_2100 = data_2100 %>%
  dplyr::mutate(pct_label = paste0(Description, " (", round(p*100, 2), "%)")) %>%
  dplyr::mutate(n = round(p*100)) %>%
  dplyr::mutate(n = ifelse(n==0,1,n)) %>%
  dplyr::mutate(n = round(n/sum(n)*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = dplyr::row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10)
# Remove one row (total != 100) 
data_2100 = data_2100 %>% 
  dplyr::filter(i != 92) %>% 
  dplyr::mutate(
    x = ifelse(i > 92, x - 1, x),
    x = ifelse(i == 101, 9, x),
    y = ifelse(i == 101, 9, y)
  )
waffle2100 = ggplot(data_2100, aes(x, y, fill = Description)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_manual(
    values = setNames(class_colors$Color, class_colors$Description),
    labels = setNames(data_2100$pct_label, data_2100$Description),
    name = NULL
  ) +
  theme_void() +
  ggtitle("d) Predicted LULC in 2100") +
  theme(legend.position = "none",
        plot.title = element_text(
          family = "Arial Narrow",
          face = "bold",
          size = 12,
          hjust = 0.5))


#### LULCC from 2024 to 2100 ------
# Select data
data = all_lulc_metrics %>%
  dplyr::select(year, class, ca) %>% 
  dplyr::mutate(
    year = as.integer(year),
    class = as.numeric(class),
    ca = as.numeric(ca)
  )

# Add color codes
data = data %>% 
  dplyr::left_join(class_colors, by = "class")

# Compute change per class
label_data = data %>%
  dplyr::group_by(Description) %>%
  dplyr::arrange(year) %>%
  dplyr::summarise(
    year_end  = dplyr::last(year),
    ca_end = dplyr::last(ca),
    ca_start = dplyr::first(ca),
    change_ha = ca_end - ca_start,
    pct_change = 100 * change_ha / ca_start,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    label = paste0(
      ifelse(change_ha >= 0, "+", "−"),
      comma(abs(round(change_ha))),
      " ha (",
      sprintf("%+.1f%%", pct_change),
      ")"
    )
  )

label_data = label_data %>%
  dplyr::mutate(ca_end_adj = dplyr::case_when(
    Description == "Forest" ~ ca_end + 4000,
    Description == "Agriculture" ~ ca_end - 2500,
    Description == "Built-up" ~ ca_end + 1000,
    Description == "Water" ~ ca_end + 4500,
    Description == "Wetland" ~ ca_end + 1500,
    Description == "Non-forest formation" ~ ca_end - 2500,
    TRUE ~ ca_end
  ))
# Define last observed year
obs_end = 2024
data = data %>%
  dplyr::mutate(period = ifelse(year <= obs_end, "Observed", "Simulated"))
# Segments and ribbon
transition_data = data %>%
  dplyr::filter(year %in% c(2024, 2050)) %>%
  dplyr::arrange(Description, year) %>%
  dplyr::group_by(Description) %>%
  dplyr::summarise(
    x = first(year),
    xend = last(year),
    y = first(ca),
    yend = last(ca),
    .groups = "drop")
transition_ribbon = transition_data %>%
  dplyr::rowwise() %>%
  dplyr::do({
    data.frame(
      Description = .$Description,
      year = seq(.$x, .$xend, length.out = 30),
      ca = seq(.$y, .$yend, length.out = 30)
    )
  }) %>%
  dplyr::ungroup()
# Plot
lulcc_plot = ggplot() +
  # Observed ribbon
  geom_ribbon(
    data = dplyr::filter(data, period == "Observed"),
    aes(x = year, ymin = ca - 2000, ymax = ca + 2000, fill = Description),
    alpha = 0.18
  ) +
  # Observed lines
  geom_line(
    data = dplyr::filter(data, period == "Observed"),
    aes(x = year, y = ca, color = Description, group = Description),
    linewidth = 1
  ) +
  geom_point(
    data = dplyr::filter(data, period == "Observed"),
    aes(x = year, y = ca, color = Description),
    size = 1
  ) +
  # Simulated lines (de-emphasized)
  geom_line(
    data = dplyr::filter(data, period == "Simulated"),
    aes(x = year, y = ca, color = Description, group = Description),
    linewidth = 1,
    linetype = "dashed",
    alpha = 0.7
  ) +
  geom_point(
    data = dplyr::filter(data, period == "Simulated"),
    aes(x = year, y = ca, color = Description),
    size = 1,
    alpha = 0.7
  ) +
  geom_ribbon(
    data = dplyr::filter(data, period == "Simulated"),
    aes(x = year, ymin = ca - 2000, ymax = ca + 2000, fill = Description),
    alpha = 0.18
  ) +
  # Labels
  geom_text(
    data = label_data,
    aes(x = year_end + 0.6, y = ca_end_adj, label = label, color = Description),
    hjust = 0, size = 3.5, fontface = "bold", show.legend = FALSE,
    family = "Arial Narrow"
  ) +
  geom_blank(aes(x = max(data$year) + 35, y = 0)) +
  scale_color_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description), guide = "none") +
  labs(x = "Year", y = "Area (ha)", color = "Land use class") +
  scale_x_break(c(2024, 2040), scales = 0.15, space=0) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  ggtitle("a) LULCC (1989–2100)") +
  theme(
    legend.position = "right",
    plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(family = "Arial Narrow", face = "bold", size = 12, hjust = 0),
    axis.title = element_text(size = 10, family = "Arial Narrow"),
    axis.text = element_text(size = 8, family = "Arial")
  ) +
  geom_segment(
    data = transition_data,
    aes(x = x, xend = xend, y = y, yend = yend, color = Description),
    linetype = "dashed",
    linewidth = 1,
    alpha = 0.7
  ) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_ribbon(
    data = transition_ribbon,
    aes(
      x = year,
      ymin = ca - 2000,
      ymax = ca + 2000,
      fill = Description,
      group = Description),
    alpha = 0.2,
    inherit.aes = FALSE)

# Combine plots

final_plot = lulcc_plot + (waffle1989 + waffle2024 + waffle2100) + plot_layout(ncol=1)
plot(final_plot)

png(here("outputs","plot","01p_paper1_LULC_1989-2100.png"), width = 3000, height = 2000, res = 300)
plot(final_plot)
dev.off()

#### Forest age -------
data = forest_age_metrics %>%
  dplyr::mutate(
    AgeClass = factor(
      dplyr::case_when(
        class == 10 ~ "<=10 years",
        class == 20 ~ "11–20 years",
        class == 30 ~ "21–30 years",
        class == 31 ~ ">30 years"),
      levels = c("<=10 years", "11–20 years", "21–30 years", ">30 years")))

total_forest = all_lulc_metrics %>%
  dplyr::filter(class == 1) %>% 
  dplyr::filter(year <= 2024) %>% 
  dplyr::select(year, ca)
age_cols = c(
  "<=10 years" = "#9ad9f5",
  "11–20 years" = "#49b5e7",
  "21–30 years" = "#138fcf",
  ">30 years"  = "#0b5fa5"
)
total_2024 = total_forest %>%
  dplyr::filter(year == 2024) %>% 
  dplyr::pull(ca)
data_comp = data %>%
  dplyr::filter(year == 2024) %>%
  dplyr::mutate(
    ca = ca,
    prop = round(100 * ca / total_2024,1))  %>% 
  dplyr::select(year, AgeClass, ca, prop)
data_comp = data_comp %>% 
  dplyr::mutate(pos = dplyr::case_when(AgeClass == "<=10 years" ~ 168000,
                                       AgeClass == "11–20 years" ~ 162000,
                                       AgeClass == "21–30 years" ~ 155000,
                                       AgeClass == ">30 years"~ 100000))
data_old_young = data %>% 
  dplyr::mutate(AgeClass = factor(dplyr::case_when(AgeClass %in% c("<=10 years", "11–20 years", "21–30 years") ~ "Young (<30 years)",
                                                   AgeClass %in% c(">30 years") ~ "Old (>30 years)"), 
                                  levels = c("Young (<30 years)", "Old (>30 years)"))) %>% 
  dplyr::group_by(AgeClass, year) %>% 
  dplyr::summarise(ca = sum(ca), .groups = "drop") %>% 
  dplyr::filter(year != 2024) %>% 
  dplyr::ungroup()
age_cols = c(
  "<=10 years" = "#9ad9f5",
  "11–20 years" = "#49b5e7",
  "21–30 years" = "#138fcf",
  ">30 years"  = "#0b5fa5",
  "Young (<30 years)" = "#9ad9f5",
  "Old (>30 years)"  = "#0b5fa5"
)
# Bar + line plot
png(here("outputs","plot","01h_lm_forest_age_barplot.png"), width = 2500, height = 1500, res = 300)
ggplot() +
  ## Stacked bars = forest age structure
  geom_col(data = data_old_young,
           aes(x = year, y = ca, fill = AgeClass),
           width = 0.9,
           color = "white",
           linewidth = 0.2) +
  ## Last bar: AgeClass composition
  geom_col(data = data_comp,
           aes(x = year + 0.5, y = ca, fill = AgeClass),
           stat = "identity",
           width = 2,
           color = "white",
           linewidth = 0.2,
           show.legend = FALSE) +
  ## Proportion labels
  geom_text(data = data_comp,
            aes(x = year + 3.8, y = pos, label = AgeClass),
            size = 4,
            family = "Arial Narrow") +
  geom_text(data = data_comp,
            aes(x = year + 0.5, y = pos, label = paste0(prop, "%")),
            size = 4,
            family = "Arial Narrow") +
  ## Total forest cover line
  geom_line(data = total_forest,
            aes(x = year, y = ca),
            color = "black",
            linewidth = 1) +
  geom_point(data = total_forest,
             aes(x = year, y = ca),
             color = "black",
             size = 2) +
  scale_fill_manual(
    values = age_cols,
    breaks = c("Young (<30 years)", "Old (>30 years)"),
    name = "Native forest age"
  ) +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 2)) +
  labs(
    x = "Year",
    y = "Forest area (ha)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, family = "Arial Narrow"),
    axis.title = element_text(size = 12, family = "Arial Narrow"),
    axis.text  = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Arial"),
    legend.title = element_text(size = 11, family = "Arial Narrow"),
    legend.text  = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
dev.off()

#### Forest metrics -----
data = forest_class_metrics %>%
  dplyr::select(
    year,
    ca,
    np,
    area_mn,
    FFI) %>% 
  dplyr::mutate(year = as.numeric(year)) %>%
  tidyr::pivot_longer(
    cols = -year,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Metric = dplyr::case_when(
      Metric == "ca" ~ "Surface area (ha)",
      Metric == "np" ~ "Number of patches",
      Metric == "area_mn" ~ "Mean patch size (ha)",
      Metric == "FFI" ~ "Forest Fragmentation Index (FFI)",
      TRUE ~ Metric))

# Set facet order
data$Metric = factor(data$Metric, levels=c("Surface area (ha)", "Number of patches", 
                                           "Mean patch size (ha)", "Forest Fragmentation Index (FFI)"))

# Plot facets
png(here("outputs","plot","01h_lm_forest_metrics_lineplot.png"), width = 2000, height = 2000, res = 300)

ggplot(data, aes(x = year, y = Value)) +
  geom_line(color = "#011809", linewidth = 1) +
  geom_point(color = "#32a65e", size = 2.4) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2) +
  labs(
    x = "Year",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12, family = "Arial Narrow"),
    plot.title = element_blank(),
    axis.title = element_text(size = 12, family = "Arial Narrow"),
    axis.text = element_text(size = 10, family = "Arial"),
    panel.grid.minor = element_blank()
  )

dev.off()

# 2. Drivers of forest change ----------
## Pixel-scale -------
### Deforestation ------
# To factor
train_data_pixel_defor = train_data_pixel_defor %>% 
  dplyr::mutate(in_car = factor(in_car, levels=c("Outside","Inside")),
                in_pub_res = factor(in_pub_res, levels=c("Outside","Inside")),
                in_rppn = factor(in_rppn, levels=c("Outside","Inside")),
                in_rl = factor(in_rl, levels=c("Outside","Inside")),
                in_apa = factor(in_apa, levels=c("Outside","Inside")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id),
                sp_block = factor(sp_block))
test_data_pixel_defor = test_data_pixel_defor %>% 
  dplyr::mutate(in_car = factor(in_car, levels=c("Outside","Inside")),
                in_pub_res = factor(in_pub_res, levels=c("Outside","Inside")),
                in_rppn = factor(in_rppn, levels=c("Outside","Inside")),
                in_rl = factor(in_rl, levels=c("Outside","Inside")),
                in_apa = factor(in_apa, levels=c("Outside","Inside")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id),
                sp_block = factor(sp_block))
  
# Model
mod_glmm = glmmTMB(formula = 
                     type ~ 
                     in_car + in_pub_res + in_rppn + in_rl + in_apa + ns_br101 +
                     prop_forest_100m_sc + prop_urb_100m_sc + 
                     dist_river_m_sc + dist_road_m_sc + dist_urban_m_sc + dist_edge_m_sc + 
                     slope_pct_sc + prec_sum_sc + tmin_mean_sc + 
                     (1|year) + (1|sp_block),
                   REML=F, family=binomial, data=train_data_pixel_defor)

# Validation
simulationOutput = simulateResiduals(fittedModel = mod_glmm, plot = F)
plotQQunif(simulationOutput) # Residuals
testDispersion(simulationOutput) # Dispersion test
plotResiduals(simulationOutput, train_data_pixel_defor$sp_block)
plotResiduals(simulationOutput, train_data_pixel_defor$year)
res2 = recalculateResiduals(simulationOutput, group = train_data_pixel_defor$year)
testSpatialAutocorrelation(res2, 
                           x = aggregate(train_data_pixel_defor$x, list(train_data_pixel_defor$year), mean)$x,
                           y = aggregate(train_data_pixel_defor$y, list(train_data_pixel_defor$year), mean)$x) # Spatial autocorrelation

# Cross-validation
pred_train = predict(mod_glmm, type="response", re.form = NA) # we remove random effects
roc_train = pROC::roc(train_data_pixel_defor$type, pred_train)
auc_train = pROC::auc(roc_train)
cat("Train AUC:", auc_train, "\n")
pred_test = predict(mod_glmm, newdata=test_data_pixel_defor, type="response", re.form = NA)
roc_test = pROC::roc(test_data_pixel_defor$type, pred_test)
auc_test = pROC::auc(roc_test)
cat("Test AUC:", auc_test, "\n")
plot(roc_train, col="blue", lwd=2, main="ROC Curves")
lines(roc_test, col="red", lwd=2)

# Intraclass Correlation Coefficient
performance::icc(mod_glmm, by_group=TRUE)
# 0.012 (i.e., 1%) of variance explained by inter-annual differences
# 0.046 (i.e., 5%) of variance explained by spatial blocks

# Interpretation
summary(mod_glmm)

# R2
r2_nakagawa(mod_glmm)

# Plot
coef_defor = broom.mixed::tidy(mod_glmm, effects = "fixed", conf.int=TRUE) %>%
  dplyr::filter(term != "(Intercept)")
x_labels = c(
  "in_carInside" = "Inside private property",
  "dist_river_m_sc" = "Distance to nearest river",
  "dist_urban_m_sc" = "Distance to nearest town",
  "dist_road_m_sc" = "Distance to nearest road",
  "prec_sum_sc" = "Total precipitations",
  "tmin_mean_sc" = "Mean min. temperature",
  "prop_urb_100m_sc" = "Proportion of built-up area",
  "in_apaInside" = "Inside APA",
  "slope_pct_sc" = "Slope",
  "ns_br101South" = "South of BR-101",
  "in_pub_resInside" = "Inside public reserve",
  "in_rppnInside" = "Inside RPPN",
  "in_rlInside" = "Inside Legal Reserve",
  "dist_edge_m_sc" = "Distance to forest edge",
  "prop_forest_100m_sc" = "Proportion of forest area"
)
coef_defor = coef_defor %>%
  dplyr::mutate(term = dplyr::recode(term, !!!x_labels))
coef_defor = coef_defor %>%
  dplyr::mutate(
    group = dplyr::case_when(
      term %in% c("Total precipitations", "Mean min. temperature", "Slope") ~ "Biophysical",
      term %in% c("Distance to nearest river", "Distance to nearest town", "Distance to nearest road", "Distance to forest edge") ~ "Distances",
      term %in% c("Proportion of built-up area", "Proportion of forest area") ~ "Land use",
      term %in% c("Inside private property", "Inside APA", "Inside public reserve", "Inside RPPN", "Inside Legal Reserve") ~ "Legal status",
      term %in% c("South of BR-101") ~ "Location",
      TRUE ~ "Other"
    )
  )
# Compute odds ratios (OR)
coef_defor = coef_defor %>%
  dplyr::mutate(OR = exp(estimate),
                OR_low = exp(conf.low),
                OR_high = exp(conf.high))
coef_defor = coef_defor %>%
  dplyr::mutate(
    sig = ifelse(conf.low > 0, "positive",
                 ifelse(conf.high < 0, "negative", "none")),
    color = dplyr::case_when(
      sig == "positive" ~ "deepskyblue",
      sig == "negative" ~ "tomato",
      TRUE ~ "darkgray"
    )
  )
all_terms_order = c(
  "Inside private property",
  "Inside APA",
  "Inside public reserve",
  "Inside RPPN",
  "Inside Legal Reserve",
  "South of BR-101",
  "Distance to nearest river",
  "Distance to nearest town",
  "Distance to nearest road",
  "Distance to forest edge",
  "Total precipitations",
  "Mean min. temperature",
  "Slope",
  "Proportion of built-up area",
  "Proportion of forest area"
)
coef_defor = coef_defor %>%
  dplyr::arrange(group, OR) %>%
  dplyr::mutate(term_ordered = factor(term, levels = all_terms_order))

# Forest plot for odds ratios
defor_coeff_plot = ggplot(coef_defor, aes(x = OR, y = term_ordered, color = color)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = OR_low, xmax = OR_high), width = 0.4, linewidth = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_identity() +
  theme_minimal() +
  ggtitle('a) Deforestation') +
  labs(x = "Standardized odds ratio (OR)") +
  theme(
    panel.border = element_rect(color="black", fill = NA, linewidth = 0.5),
    text = element_text(family = "Arial Narrow"),
    plot.title = element_text(family = "Arial Narrow", face = "bold", size = 12, hjust = 0),
    axis.title.y = element_blank(),
    axis.text.y = element_text(hjust = 0),
    strip.text.y.left = element_text(angle = 90, face = "bold"),
    strip.placement = "outside"
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch="y")

### Reforestation -------
# To factor
train_data_pixel_refor = train_data_pixel_refor %>% 
  dplyr::mutate(in_car = factor(in_car, levels=c("Outside","Inside")),
                in_pub_res = factor(in_pub_res, levels=c("Outside","Inside")),
                in_rppn = factor(in_rppn, levels=c("Outside","Inside")),
                in_rl = factor(in_rl, levels=c("Outside","Inside")),
                in_apa = factor(in_apa, levels=c("Outside","Inside")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id),
                sp_block = factor(sp_block))
test_data_pixel_refor = test_data_pixel_refor %>% 
  dplyr::mutate(in_car = factor(in_car, levels=c("Outside","Inside")),
                in_pub_res = factor(in_pub_res, levels=c("Outside","Inside")),
                in_rppn = factor(in_rppn, levels=c("Outside","Inside")),
                in_rl = factor(in_rl, levels=c("Outside","Inside")),
                in_apa = factor(in_apa, levels=c("Outside","Inside")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id),
                sp_block = factor(sp_block))

# Model
mod_glmm = glmmTMB(formula = 
                     type ~ 
                     in_car + in_pub_res + in_rppn + in_rl + in_apa + ns_br101 +
                     prop_forest_100m_sc + prop_urb_100m_sc + 
                     dist_river_m_sc + dist_road_m_sc + dist_urban_m_sc + dist_edge_m_sc + 
                     slope_pct_sc + prec_sum_sc + tmin_mean_sc + 
                     (1|year) + (1|sp_block),
                   REML=F, family=binomial, data=train_data_pixel_refor)

# Validation
simulationOutput = simulateResiduals(fittedModel = mod_glmm, plot = F)
plotQQunif(simulationOutput) # Residuals
testDispersion(simulationOutput) # Dispersion test
plotResiduals(simulationOutput, train_data_pixel_refor$sp_block)
plotResiduals(simulationOutput, train_data_pixel_refor$year)
res2 = recalculateResiduals(simulationOutput, group = train_data_pixel_refor$year)
testSpatialAutocorrelation(res2, 
                           x = aggregate(train_data_pixel_refor$x, list(train_data_pixel_refor$year), mean)$x,
                           y = aggregate(train_data_pixel_refor$y, list(train_data_pixel_refor$year), mean)$x) # Spatial autocorrelation

# Cross-validation
pred_train = predict(mod_glmm, type="response", re.form = NA) # we remove random effects
roc_train = pROC::roc(train_data_pixel_refor$type, pred_train)
auc_train = pROC::auc(roc_train)
cat("Train AUC:", auc_train, "\n")
pred_test = predict(mod_glmm, newdata=test_data_pixel_refor, type="response", re.form = NA)
roc_test = pROC::roc(test_data_pixel_refor$type, pred_test)
auc_test = pROC::auc(roc_test)
cat("Test AUC:", auc_test, "\n")
plot(roc_train, col="blue", lwd=2, main="ROC Curves")
lines(roc_test, col="red", lwd=2)

# Intraclass Correlation Coefficient
performance::icc(mod_glmm, by_group=TRUE)
# 0.006 (i.e., <1%) of variance explained by inter-annual differences
# 0.017 (i.e., 2%) of variance explained by spatial blocks

# Interpretation
summary(mod_glmm)

# R2
r2_nakagawa(mod_glmm)

# Plot
coef_refor = broom.mixed::tidy(mod_glmm, effects = "fixed", conf.int=TRUE) %>%
  dplyr::filter(term != "(Intercept)")
x_labels = c(
  "in_carInside" = "Inside private property",
  "dist_river_m_sc" = "Distance to nearest river",
  "dist_urban_m_sc" = "Distance to nearest town",
  "dist_road_m_sc" = "Distance to nearest road",
  "prec_sum_sc" = "Total precipitations",
  "tmin_mean_sc" = "Mean min. temperature",
  "prop_urb_100m_sc" = "Proportion of built-up area",
  "in_apaInside" = "Inside APA",
  "slope_pct_sc" = "Slope",
  "ns_br101South" = "South of BR-101",
  "in_pub_resInside" = "Inside public reserve",
  "in_rppnInside" = "Inside RPPN",
  "in_rlInside" = "Inside Legal Reserve",
  "dist_edge_m_sc" = "Distance to forest edge",
  "prop_forest_100m_sc" = "Proportion of forest area"
)
coef_refor = coef_refor %>%
  dplyr::mutate(term = dplyr::recode(term, !!!x_labels))
coef_refor = coef_refor %>%
  dplyr::mutate(
    group = dplyr::case_when(
      term %in% c("Total precipitations", "Mean min. temperature", "Slope") ~ "Biophysical",
      term %in% c("Distance to nearest river", "Distance to nearest town", "Distance to nearest road", "Distance to forest edge") ~ "Distances",
      term %in% c("Proportion of built-up area", "Proportion of forest area") ~ "Land use",
      term %in% c("Inside private property", "Inside APA", "Inside public reserve", "Inside RPPN", "Inside Legal Reserve") ~ "Legal status",
      term %in% c("South of BR-101") ~ "Location",
      TRUE ~ "Other"
    )
  )
# Compute odds ratios (OR)
coef_refor = coef_refor %>%
  dplyr::mutate(OR = exp(estimate),
                OR_low = exp(conf.low),
                OR_high = exp(conf.high))
coef_refor = coef_refor %>%
  dplyr::mutate(
    sig = ifelse(conf.low > 0, "positive",
                 ifelse(conf.high < 0, "negative", "none")),
    color = dplyr::case_when(
      sig == "positive" ~ "deepskyblue",
      sig == "negative" ~ "tomato",
      TRUE ~ "darkgray"
    )
  )
coef_refor = coef_refor %>%
  dplyr::arrange(group, OR) %>%
  dplyr::mutate(term_ordered = factor(term, levels = all_terms_order))

# Forest plot for odds ratios
refor_coeff_plot = ggplot(coef_refor, aes(x = OR, y = term_ordered, color = color)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = OR_low, xmax = OR_high), width = 0.4, linewidth = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_identity() +
  theme_minimal() +
  ggtitle('b) Reforestation') +
  labs(x = "Standardized odds ratio (OR)") +
  theme(
    panel.border = element_rect(color="black", fill = NA, linewidth = 0.5),
    text = element_text(family = "Arial Narrow"),
    plot.title = element_text(family = "Arial Narrow", face = "bold", size = 12, hjust = 0),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.y.left = element_blank(),
    strip.placement = "outside"
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y")

### Figure ------
forest_all = defor_coeff_plot + refor_coeff_plot +
  plot_layout(ncol = 2, widths = c(1, 2.5), axis_titles = "collect")
forest_all

png(here("outputs","plot","01p_paper1_or_pixel.png"), width = 3000, height = 2000, res = 300)
plot(forest_all)
dev.off()

## Property-scale ------

### Deforestation ------
rownames(train_data_car_defor) = train_data_car_defor$car_id
rownames(test_data_car_defor) = test_data_car_defor$car_id

# Binary neighborhood
mat = train_data_car_defor %>% sf::st_drop_geometry()
xy = data.matrix(mat[,c("centroid_x", "centroid_y")])
nb.gab = spdep::graph2nb(spdep::gabrielneigh(xy), sym=TRUE)
listw.gab = spdep::nb2listw(nb.gab)
plot(sf::st_geometry(train_data_car_defor), border = "lightgray")
spdep::plot.nb(nb.gab, xy, add = TRUE)

# Inverse distances
dlist = spdep::nbdists(nb.gab, xy)
dlist = lapply(dlist, function(x) 1/x)
listw.d1 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)
dlist = lapply(dlist, function(x) 1/x^2)
listw.d2 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)

# Fixed number of neighbors
col.knn.nb = knn2nb(knearneigh(sf::st_centroid(train_data_car_defor), k = 10))
listw.knn = nb2listw(col.knn.nb, style = "W")
plot(st_geometry(train_data_car_defor), border = "lightgray")
plot.nb(col.knn.nb, xy, add = TRUE)

# Observe spatial autocorrelation
spdep::moran.test(train_data_car_defor$area_deforest_log, listw.gab)

# SAR
mod_sar1 = spatialreg::errorsarlm(area_deforest_log ~ 
                                    car_area_log + 
                                    area_defor_buf100_2024_log +
                                    area_agri_buf100_2024_log, 
                                  listw=listw.gab,
                                  data=train_data_car_defor)
mod_sar2 = spatialreg::errorsarlm(area_deforest_log ~ 
                                    car_area_log + 
                                    area_defor_buf100_2024_log +
                                    area_agri_buf100_2024_log, 
                                  listw=listw.d1,
                                  data=train_data_car_defor)
mod_sar3 = spatialreg::errorsarlm(area_deforest_log ~ 
                                    car_area_log + 
                                    area_defor_buf100_2024_log +
                                    area_agri_buf100_2024_log, 
                                  listw=listw.d2,
                                  data=train_data_car_defor)
mod_sar4 = spatialreg::errorsarlm(area_deforest_log ~ 
                                    car_area_log + 
                                    area_defor_buf100_2024_log +
                                    area_agri_buf100_2024_log, 
                                  listw=listw.knn,
                                  data=train_data_car_defor)

# Comparison (AICc)
Models = list(mod_sar1=mod_sar1, 
              mod_sar2=mod_sar2,
              mod_sar3=mod_sar3,
              mod_sar4=mod_sar4)
data.frame(AICc = sapply(Models, MuMIn::AICc)) %>% 
  dplyr::mutate(delta = AICc - min(AICc)) %>%
  dplyr::arrange(delta)


# Validation
residuals = residuals(mod_sar2)
fitted_values = fitted(mod_sar2)
ggplot(data.frame(fitted = fitted_values, resid = residuals), aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Standardized Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

hist(residuals(mod_sar2))

residuals = residuals(mod_sar2)
qqplot_data = data.frame(res = residuals)
ggplot(qqplot_data, aes(sample = res)) +
  stat_qq(distribution = qnorm) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "QQ Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

ggplot(train_data_car_defor, aes(x = car_area_log, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()
ggplot(train_data_car_defor, aes(x = area_defor_buf100_2024_log, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()
ggplot(train_data_car_defor, aes(x = area_agri_buf100_2024_log, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()

# Spatial autocorrelation
moran.mc(resid(mod_sar2), listw.d1, nsim = 999)

# Cross-validation
mat = test_data_car_defor %>% sf::st_drop_geometry()
xy = data.matrix(mat[,c("centroid_x", "centroid_y")])
nb.gab = spdep::graph2nb(spdep::gabrielneigh(xy), sym=TRUE)
listw.gab = spdep::nb2listw(nb.gab)
plot(sf::st_geometry(test_data_car_defor), border = "lightgray")
spdep::plot.nb(nb.gab, xy, add = TRUE)

# Inverse distances
dlist = spdep::nbdists(nb.gab, xy)
dlist = lapply(dlist, function(x) 1/x)
listw.d1 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)

pred_test = predict(mod_sar2, 
                    newdata = test_data_car_defor,
                    listw = listw.d1,
                    pred.type = "TS")
pred_test_exp = exp(pred_test) # Back transformation

# RMSE and MAE
data.frame(
  RMSE = caret::RMSE(pred_test_exp, test_data_car_defor$area_deforest_log),
  MAE = caret::MAE(pred_test_exp, test_data_car_defor$area_deforest_log)
)

# Interpretation
summary(mod_sar2, Nagelkerke = TRUE)

#### Figure -------
mod = mod_sar2

## X1
# Predictions
newdata = data.frame(
  car_area_log = seq(min(train_data_car_defor$car_area_log, na.rm=TRUE),
                     max(train_data_car_defor$car_area_log, na.rm=TRUE), length.out=100),
  area_defor_buf100_2024_log = mean(train_data_car_defor$area_defor_buf100_2024_log, na.rm=TRUE),
  area_agri_buf100_2024_log = mean(train_data_car_defor$area_agri_buf100_2024_log, na.rm=TRUE)
)
X = model.matrix(~ car_area_log + area_defor_buf100_2024_log + area_agri_buf100_2024_log, data=newdata)
beta = mod$coefficients
V = mod$Hcov
fit = as.vector(X %*% beta)
se = sqrt(diag(X %*% V %*% t(X)))
pred_manual = data.frame(
  x = newdata$car_area_log,
  predicted = fit,
  conf.low = fit - 1.96 * se,
  conf.high = fit + 1.96 * se
)

# Plot
plot_x1 = ggplot() +
  geom_line(data = pred_manual, aes(x, predicted), linewidth = 1, color="red") +
  geom_jitter(
    data = train_data_car_defor,
    aes(x = car_area_log, y = area_deforest_log),
    width = 1, size = 2.5, alpha = 0.15, color = "black"
  ) +
  geom_ribbon(
    data = pred_manual,
    aes(x, ymin = conf.low, ymax = conf.high),
    color = NA, alpha = 0.2, fill = "red"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "Arial Narrow", face = "bold", size = 14, hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.17, 0.85),
    text = element_text(family = "Arial Narrow", size=12)
  ) +
  xlab("Log of property size (ha)") +
  ylab("Log of deforested area (ha)") +
  ggtitle("a) Effect of property size")

## X2
# Predictions (effect of a single variable while holding others at their mean)
newdata = data.frame(
  car_area_log = mean(train_data_car_defor$car_area_log, na.rm=TRUE),
  area_defor_buf100_2024_log = seq(min(train_data_car_defor$area_defor_buf100_2024_log, na.rm=TRUE),
                                   max(train_data_car_defor$area_defor_buf100_2024_log, na.rm=TRUE), length.out=100),
  area_agri_buf100_2024_log = mean(train_data_car_defor$area_agri_buf100_2024_log, na.rm=TRUE)
)
X = model.matrix(~ car_area_log + area_defor_buf100_2024_log + area_agri_buf100_2024_log, data=newdata)
beta = mod$coefficients
V = mod$Hcov
fit = as.vector(X %*% beta)
se = sqrt(diag(X %*% V %*% t(X)))
pred_manual = data.frame(
  x = newdata$area_defor_buf100_2024_log,
  predicted = fit,
  conf.low = fit - 1.96 * se,
  conf.high = fit + 1.96 * se
)

# Plot
plot_x2 = ggplot() +
  geom_line(data = pred_manual, aes(x, predicted), linewidth = 1, color="red") +
  geom_jitter(
    data = train_data_car_defor,
    aes(x = area_defor_buf100_2024_log, y = area_deforest_log),
    width = 1, size = 2.5, alpha = 0.15, color = "black"
  ) +
  geom_ribbon(
    data = pred_manual,
    aes(x, ymin = conf.low, ymax = conf.high),
    color = NA, alpha = 0.2, fill = "red"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "Arial Narrow", face = "bold", size = 14, hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.17, 0.85),
    axis.title.y = element_blank(),
    text = element_text(family = "Arial Narrow", size=12)
  ) +
  xlab("Log of surrounding deforested area (ha)") +
  ggtitle("b) Effect of surrounding deforested area")

#### Merge figures -----
plot_defor_mod_car = plot_x1 + plot_x2 +
  plot_layout(ncol = 2, widths = c(1, 1), axis_titles = "collect")
plot_defor_mod_car

png(here("outputs","plot","01p_paper1_effect_property_defor.png"), width = 2300, height = 1700, res = 300)
plot(plot_defor_mod_car)
dev.off()

### Reforestation -------
rownames(train_data_car_refor) = train_data_car_refor$car_id
rownames(test_data_car_refor) = test_data_car_refor$car_id

# Binary neighborhood
mat = train_data_car_refor %>% sf::st_drop_geometry()
xy = data.matrix(mat[,c("centroid_x", "centroid_y")])
nb.gab = spdep::graph2nb(spdep::gabrielneigh(xy), sym=TRUE)
listw.gab = spdep::nb2listw(nb.gab)
plot(sf::st_geometry(train_data_car_refor), border = "lightgray")
spdep::plot.nb(nb.gab, xy, add = TRUE)

# Inverse distances
dlist = spdep::nbdists(nb.gab, xy)
dlist = lapply(dlist, function(x) 1/x)
listw.d1 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)
dlist = lapply(dlist, function(x) 1/x^2)
listw.d2 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)

# Fixed number of neighbors
col.knn.nb = knn2nb(knearneigh(sf::st_centroid(train_data_car_refor), k = 10))
listw.knn = nb2listw(col.knn.nb, style = "W")
plot(st_geometry(train_data_car_refor), border = "lightgray")
plot.nb(col.knn.nb, xy, add = TRUE)

# Observe spatial autocorrelation
spdep::moran.test(train_data_car_refor$area_reforest_log, listw.gab)

# SAR
mod_sar1 = spatialreg::errorsarlm(area_reforest_log ~ 
                                    car_area_log + 
                                    area_refor_buf100_2024_log +
                                    area_forest_buf100_1989_log +
                                    factor(Less20For2024), 
                                  listw=listw.gab,
                                  data=train_data_car_refor)
mod_sar2 = spatialreg::errorsarlm(area_reforest_log ~ 
                                    car_area_log + 
                                    area_refor_buf100_2024_log +
                                    area_forest_buf100_1989_log +
                                    factor(Less20For2024), 
                                  listw=listw.d1,
                                  data=train_data_car_refor)
mod_sar3 = spatialreg::errorsarlm(area_reforest_log ~ 
                                    car_area_log + 
                                    area_refor_buf100_2024_log +
                                    area_forest_buf100_1989_log +
                                    factor(Less20For2024), 
                                  listw=listw.d2,
                                  data=train_data_car_refor)
mod_sar4 = spatialreg::errorsarlm(area_reforest_log ~ 
                                    car_area_log + 
                                    area_refor_buf100_2024_log +
                                    area_forest_buf100_1989_log +
                                    factor(Less20For2024), 
                                  listw=listw.knn,
                                  data=train_data_car_refor)

# Comparison (AICc)
Models = list(mod_sar1=mod_sar1, 
              mod_sar2=mod_sar2,
              mod_sar3=mod_sar3,
              mod_sar4=mod_sar4)
data.frame(AICc = sapply(Models, MuMIn::AICc)) %>% 
  dplyr::mutate(delta = AICc - min(AICc)) %>%
  dplyr::arrange(delta)

# Validation
residuals = residuals(mod_sar2)
fitted_values = fitted(mod_sar2)
ggplot(data.frame(fitted = fitted_values, resid = residuals), aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Standardized Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

hist(residuals(mod_sar2))

residuals = residuals(mod_sar2)
qqplot_data = data.frame(res = residuals)
ggplot(qqplot_data, aes(sample = res)) +
  stat_qq(distribution = qnorm) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "QQ Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

ggplot(train_data_car_refor, aes(x = car_area_log, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()
ggplot(train_data_car_refor, aes(x = area_refor_buf100_2024_log, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()
ggplot(train_data_car_refor, aes(x = area_forest_buf100_1989_log, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()
ggplot(train_data_car_refor, aes(x = factor(Less20For2024), y = residuals)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()

# Spatial autocorrelation
moran.mc(resid(mod_sar2), listw.d1, nsim = 999)

# Cross-validation
mat = test_data_car_refor %>% sf::st_drop_geometry()
xy = data.matrix(mat[,c("centroid_x", "centroid_y")])
nb.gab = spdep::graph2nb(spdep::gabrielneigh(xy), sym=TRUE)
listw.gab = spdep::nb2listw(nb.gab)
plot(sf::st_geometry(test_data_car_refor), border = "lightgray")
spdep::plot.nb(nb.gab, xy, add = TRUE)

# Inverse distances
dlist = spdep::nbdists(nb.gab, xy)
dlist = lapply(dlist, function(x) 1/x)
listw.d1 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)

pred_test = predict(mod_sar2, 
                    newdata = test_data_car_refor,
                    listw = listw.d1,
                    pred.type = "TS")
pred_test_exp = exp(pred_test) # Back transformation

# RMSE and MAE
data.frame(
  RMSE = caret::RMSE(pred_test_exp, test_data_car_refor$area_reforest_log),
  MAE = caret::MAE(pred_test_exp, test_data_car_refor$area_reforest_log)
)

# Interpretation
summary(mod_sar2, Nagelkerke = TRUE)

#### Figure -------
mod = mod_sar2

## X1
# Predictions
# Create newdata for both levels of Less20For2024
newdata_0 = data.frame(
  car_area_log = seq(min(train_data_car_refor$car_area_log, na.rm=TRUE),
                     max(train_data_car_refor$car_area_log, na.rm=TRUE), length.out=100),
  area_refor_buf100_2024_log = mean(train_data_car_refor$area_refor_buf100_2024_log, na.rm=TRUE),
  area_forest_buf100_1989_log = mean(train_data_car_refor$area_forest_buf100_1989_log, na.rm=TRUE),
  Less20For2024 = factor(0, levels = c(0, 1))
)

newdata_1 = newdata_0
newdata_1$Less20For2024 = factor(1, levels = c(0, 1))

# Combine both datasets
newdata = rbind(newdata_0, newdata_1)

# Get model matrix and betas
X = model.matrix(~ car_area_log + area_refor_buf100_2024_log + area_forest_buf100_1989_log + Less20For2024, data=newdata)
beta = mod$coefficients
V = mod$Hcov

# Calculate predicted values and standard errors
fit = as.vector(X %*% beta)
se = sqrt(diag(X %*% V %*% t(X)))
pred_manual = data.frame(
  x = newdata$car_area_log,
  predicted = fit,
  conf.low = fit - 1.96 * se,
  conf.high = fit + 1.96 * se,
  Less20For2024 = newdata$Less20For2024
)

# Plot
plot_x1 = ggplot(pred_manual, aes(x = x, y = predicted, color = Less20For2024, fill = Less20For2024)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_jitter(
    data = train_data_car_refor,
    aes(x = car_area_log, y = area_reforest_log, color = factor(Less20For2024)),
    inherit.aes = FALSE,
    width = 1, size = 2.5, alpha = 0.15
  ) +
  xlab("Log of property size (ha)") +
  ylab("Log of reforested area (ha)") +
  ggtitle("a) Effect of property size") +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("<20% forest cover", ">20% forest cover"),
    name = "Forest cover in 2024"
  ) +
  scale_fill_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("<20% forest cover", ">20% forest cover"),
    name = "Forest cover in 2024"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "Arial Narrow", face = "bold", size = 14, hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    text = element_text(family = "Arial Narrow", size = 12)
  )

## X2
# Predictions
# Create newdata for both levels of Less20For2024
newdata_0 = data.frame(
  car_area_log = mean(train_data_car_refor$car_area_log, na.rm=TRUE),
  area_refor_buf100_2024_log = seq(min(train_data_car_refor$area_refor_buf100_2024_log, na.rm=TRUE),
                                   max(train_data_car_refor$area_refor_buf100_2024_log, na.rm=TRUE), length.out=100),
  area_forest_buf100_1989_log = mean(train_data_car_refor$area_forest_buf100_1989_log, na.rm=TRUE),
  Less20For2024 = factor(0, levels = c(0, 1))
)

newdata_1 = newdata_0
newdata_1$Less20For2024 = factor(1, levels = c(0, 1))

# Combine both datasets
newdata = rbind(newdata_0, newdata_1)

# Get model matrix and betas
X = model.matrix(~ car_area_log + area_refor_buf100_2024_log + area_forest_buf100_1989_log + Less20For2024, data=newdata)
beta = mod$coefficients
V = mod$Hcov

# Calculate predicted values and standard errors
fit = as.vector(X %*% beta)
se = sqrt(diag(X %*% V %*% t(X)))
pred_manual = data.frame(
  x = newdata$area_refor_buf100_2024_log,
  predicted = fit,
  conf.low = fit - 1.96 * se,
  conf.high = fit + 1.96 * se,
  Less20For2024 = newdata$Less20For2024
)

# Plot
plot_x2 = ggplot(pred_manual, aes(x = x, y = predicted, color = Less20For2024, fill = Less20For2024)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_jitter(
    data = train_data_car_refor,
    aes(x = area_refor_buf100_2024_log, y = area_reforest_log, color = factor(Less20For2024)),
    inherit.aes = FALSE,
    width = 1, size = 2.5, alpha = 0.15
  ) +
  xlab("Log of surrounding reforested area (ha)") +
  ggtitle("b) Effect of surrounding reforested area (2024)") +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("<20% forest cover", ">20% forest cover"),
    name = "Forest cover in 2024"
  ) +
  scale_fill_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("<20% forest cover", ">20% forest cover"),
    name = "Forest cover in 2024"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "Arial Narrow", face = "bold", size = 14, hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    text = element_text(family = "Arial Narrow", size = 12),
    axis.title.y = element_blank()
  )

## X3
# Predictions
# Create newdata for both levels of Less20For2024
newdata_0 = data.frame(
  car_area_log = mean(train_data_car_refor$car_area_log, na.rm=TRUE),
  area_refor_buf100_2024_log = mean(train_data_car_refor$area_refor_buf100_2024_log, na.rm=TRUE),
  area_forest_buf100_1989_log = seq(min(train_data_car_refor$area_forest_buf100_1989_log, na.rm=TRUE),
                                  max(train_data_car_refor$area_forest_buf100_1989_log, na.rm=TRUE), length.out=100),
  Less20For2024 = factor(0, levels = c(0, 1))
)

newdata_1 = newdata_0
newdata_1$Less20For2024 = factor(1, levels = c(0, 1))

# Combine both datasets
newdata = rbind(newdata_0, newdata_1)

# Get model matrix and betas
X = model.matrix(~ car_area_log + area_refor_buf100_2024_log + area_forest_buf100_1989_log + Less20For2024, data=newdata)
beta = mod$coefficients
V = mod$Hcov

# Calculate predicted values and standard errors
fit = as.vector(X %*% beta)
se = sqrt(diag(X %*% V %*% t(X)))
pred_manual = data.frame(
  x = newdata$area_forest_buf100_1989_log,
  predicted = fit,
  conf.low = fit - 1.96 * se,
  conf.high = fit + 1.96 * se,
  Less20For2024 = newdata$Less20For2024
)

# Plot
plot_x3 = ggplot(pred_manual, aes(x = x, y = predicted, color = Less20For2024, fill = Less20For2024)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_jitter(
    data = train_data_car_refor,
    aes(x = area_forest_buf100_1989_log, y = area_reforest_log, color = factor(Less20For2024)),
    inherit.aes = FALSE,
    width = 1, size = 2.5, alpha = 0.15
  ) +
  xlab("Log of surrounding forest area (ha)") +
  ggtitle("c) Effect of surrounding forest area (1989)") +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("<20% forest cover", ">20% forest cover"),
    name = "Forest cover in 2024"
  ) +
  scale_fill_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("<20% forest cover", ">20% forest cover"),
    name = "Forest cover in 2024"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "Arial Narrow", face = "bold", size = 14, hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    text = element_text(family = "Arial Narrow", size = 12),
    axis.title.y = element_blank()
  )

#### Merge figures -----
plot_defor_mod_car = plot_x1 + plot_x2 + plot_x3 +
  plot_layout(ncol = 3, widths = c(1, 1, 1), axis_titles = "collect")
plot_defor_mod_car

png(here("outputs","plot","01p_paper1_effect_property_refor.png"), width = 3500, height = 1700, res = 300)
plot(plot_defor_mod_car)
dev.off()

# 3. Hotspots of reforestation and deforestation ----------

defor = raster_tm_2024 == 8
refor = raster_tm_2024 == 7

# Grid
bbox = st_as_sfc(st_bbox(raster_tm_2024))
grid = st_make_grid(bbox, cellsize = 5000, square = TRUE) %>% 
  st_as_sf()
st_crs(grid) = st_crs(raster_tm_2024)

### Deforestation ----
# Compute proportions in the grid
extract_defor = terra::extract(defor, vect(grid), fun = mean, na.rm = TRUE)
grid$prop_defor = extract_defor[,2]

# Remove NAs
grid = grid %>% 
  dplyr::filter(!is.na(prop_defor))

# Neighbor matrix
nb_defor = poly2nb(grid, queen = TRUE) # Queen contiguity

# Weigh matrix
lw_defor = nb2listw(nb_defor, style = "W", zero.policy = TRUE)

# Compute Moran's I
lmoran = spdep::localmoran(grid$prop_defor, lw_defor, alternative="two.sided")

# Extract values
grid$lmI = lmoran[, "Ii"] # local Moran's I
grid$lmZ = lmoran[, "Z.Ii"] # z-scores
grid$lmp = lmoran[, "Pr(z != E(Ii))"] # p-values
mp = moran.plot(as.vector(scale(grid$prop_defor)), lw_defor)
grid$quadrant = NA
# high-high
grid[(mp$x >= 0 & mp$wx >= 0) & (grid$lmp <= 0.05), "quadrant"]= 1
# low-low
grid[(mp$x <= 0 & mp$wx <= 0) & (grid$lmp <= 0.05), "quadrant"]= 2
# high-low
grid[(mp$x >= 0 & mp$wx <= 0) & (grid$lmp <= 0.05), "quadrant"]= 3
# low-high
grid[(mp$x <= 0 & mp$wx >= 0) & (grid$lmp <= 0.05), "quadrant"]= 4
# non-significant
grid[(grid$lmp > 0.05), "quadrant"] = 5
unique(grid$quadrant)
# Make quadrant a factor with labels
grid$quadrant = factor(grid$quadrant,
                       levels = c(1, 2, 3, 4, 5),
                       labels = c("High-High", "Low-Low", "High-Low", "Low-High", "Non-significant"))

# Colors
cols = c("red", "blue", "lightpink", "skyblue2", "white")

#### Map -----------
hotspots = grid %>% dplyr::filter(quadrant == "High-High")
hotspots = sf::st_crop(hotspots, st_bbox(raster_tm_2024))
# Raster to dataframe
defor_df = as.data.frame(raster_tm_2024, xy = TRUE)
# Convert to factor with labels
defor_df$value = factor(defor_df$`2024`,
                        levels = 1:8,
                        labels = c("Forest",
                                   "Non-forest formation",
                                   "Wetland",
                                   "Agriculture",
                                   "Water",
                                   "Built-up",
                                   "Reforested",
                                   "Deforested"))
cols = c("#32a65e", "#ad975a", "#519799", "#FFFFB2",
         "#0000FF", "#d4271e", "chartreuse", "pink")

# Plot
map_hotspots_defor = ggplot() +
  geom_raster(data = defor_df,
              aes(x = x, y = y, fill = value)) +
  scale_fill_manual(name = "Land use",
                    values = cols) +
  geom_sf(data = hotspots,
          fill = "red",
          color = "black",
          alpha = 0.5,
          size = 0.2) +
  labs(title = "a) Deforestation hotspots",
       x = NULL, y = NULL) + 
  annotation_scale(
    location = "tr",
    text_cex = 0.6,
    pad_x = unit(1, "cm"),
    pad_y = unit(0.5, "cm"),
    style = "ticks",
    line_width = unit(1, "cm"),
    width_hint = 0.15
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    pad_x = unit(1, "cm"),
    pad_y = unit(0.5, "cm"),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


### Reforestation ----
# Compute proportions in the grid
extract_refor = terra::extract(refor, vect(grid), fun = mean, na.rm = TRUE)
grid$prop_refor = extract_refor[,2]

# Remove NAs
grid = grid %>% 
  dplyr::filter(!is.na(prop_refor))

# Neighbor matrix
nb_refor = poly2nb(grid, queen = TRUE) # Queen contiguity

# Weigh matrix
lw_refor = nb2listw(nb_refor, style = "W", zero.policy = TRUE)

# Compute Moran's I
lmoran = spdep::localmoran(grid$prop_refor, lw_refor, alternative="two.sided")

# Extract values
grid$lmI = lmoran[, "Ii"] # local Moran's I
grid$lmZ = lmoran[, "Z.Ii"] # z-scores
grid$lmp = lmoran[, "Pr(z != E(Ii))"] # p-values
mp = moran.plot(as.vector(scale(grid$prop_refor)), lw_refor)
grid$quadrant = NA
# high-high
grid[(mp$x >= 0 & mp$wx >= 0) & (grid$lmp <= 0.05), "quadrant"]= 1
# low-low
grid[(mp$x <= 0 & mp$wx <= 0) & (grid$lmp <= 0.05), "quadrant"]= 2
# high-low
grid[(mp$x >= 0 & mp$wx <= 0) & (grid$lmp <= 0.05), "quadrant"]= 3
# low-high
grid[(mp$x <= 0 & mp$wx >= 0) & (grid$lmp <= 0.05), "quadrant"]= 4
# non-significant
grid[(grid$lmp > 0.05), "quadrant"] = 5
unique(grid$quadrant)
# Make quadrant a factor with labels
grid$quadrant = factor(grid$quadrant,
                       levels = c(1, 2, 3, 4, 5),
                       labels = c("High-High", "Low-Low", "High-Low", "Low-High", "Non-significant"))

# Colors
cols = c("red", "blue", "lightpink", "skyblue2", "white")

#### Map -----------
hotspots = grid %>% dplyr::filter(quadrant == "High-High")
hotspots = sf::st_crop(hotspots, st_bbox(raster_tm_2024))
# Raster to dataframe
refor_df = as.data.frame(raster_tm_2024, xy = TRUE)
# Convert to factor with labels
refor_df$value = factor(refor_df$`2024`,
                        levels = 1:8,
                        labels = c("Forest",
                                   "Non-forest formation",
                                   "Wetland",
                                   "Agriculture",
                                   "Water",
                                   "Built-up",
                                   "Reforested",
                                   "Deforested"))
cols = c("#32a65e", "#ad975a", "#519799", "#FFFFB2",
         "#0000FF", "#d4271e", "chartreuse", "pink")

# Plot
map_hotspots_refor = ggplot() +
  geom_raster(data = refor_df,
              aes(x = x, y = y, fill = value)) +
  scale_fill_manual(name = "Land use",
                    values = cols) +
  geom_sf(data = hotspots,
          fill = "red",
          color = "black",
          alpha = 0.5,
          size = 0.2) +
  labs(title = "b) Reforestation hotspots",
       x = NULL, y = NULL) + 
  annotation_scale(
    location = "tr",
    text_cex = 0.6,
    pad_x = unit(1, "cm"),
    pad_y = unit(0.5, "cm"),
    style = "ticks",
    line_width = unit(1, "cm"),
    width_hint = 0.15
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    pad_x = unit(1, "cm"),
    pad_y = unit(0.5, "cm"),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

## Merge maps
hotspots_all = map_hotspots_defor + map_hotspots_refor +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

png(here("outputs","plot","01p_paper1_hotspots.png"), width = 3000, height = 1700, res = 300)
plot(hotspots_all)
dev.off()