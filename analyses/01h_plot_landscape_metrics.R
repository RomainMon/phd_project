#------------------------------------------------#
# Author: Romain Monassier
# Objective: Plot landscape metrics through time
#------------------------------------------------#

library(tidyr)
library(ggplot2)
library(readr)
library(ggalluvial)
library(here)
library(grid)
library(patchwork)
library(stringr)


### Import datasets ----------------
base_path = here("outputs", "data", "landscapemetrics")
class_metrics = readr::read_csv(
  file.path(base_path, "class_metrics_bbox_1989_2023.csv"),
  show_col_types = FALSE
)
forest_class_metrics = readr::read_csv(
  file.path(base_path, "forest_class_metrics_bbox_1989_2023.csv"),
  show_col_types = FALSE
)
forest_core_corridor_metrics = readr::read_csv(
  file.path(base_path, "forest_core_corridors_metrics_bbox_1989_2023.csv"),
  show_col_types = FALSE
)

### Class metrics (overall) ----------------

#### Summary statistics ------
# Evolution per class per year
class_metrics %>%
  dplyr::arrange(class, year) %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(pland_change_year = round((pland - dplyr::lag(pland)), 1),
                ca_change_year = ca - dplyr::lag(ca),
                ca_change_year_pct = (ca - dplyr::lag(ca)) / dplyr::lag(ca) * 100) %>%
  dplyr::ungroup()

# Changes betwenn 1989 and 2023
class_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(pland_change_total = round(pland[year == max(year)] - pland[year == min(year)], 1),
                   ca_change_total = ca[year == max(year)] - ca[year == min(year)])

# Between sets of years
class_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(
    pland_change_1989_2000 = round(pland[year == 2000] - pland[year == 1989], 1),
    pland_change_2000_2012 = round(pland[year == 2012] - pland[year == 2000], 1),
    pland_change_2012_2023 = round(pland[year == 2023] - pland[year == 2012], 1),
    ca_change_1989_2000 = ca[year == 2000] - ca[year == 1989],
    ca_change_2000_2012 = ca[year == 2012] - ca[year == 2000],
    ca_change_2012_2023 = ca[year == 2023] - ca[year == 2012]) %>%
  dplyr::ungroup()


#### Plots -------
##### Stacked area chart ------
# Create color palette using the Legend codes provided by MapBiomas
class_colors = tibble::tibble(
  Class_ID = c(1,2,3,4,5),
  Description = c("Forest","Other non-forest formation","Agriculture","Water","Artificial"
  ),
  Color = c(
    "#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"
  )
)

# Select data
data = class_metrics %>% 
  dplyr::select(year, class, pland) %>% 
  dplyr::mutate(
    year  = as.integer(year),          # x axis must be numeric/continuous
    class = as.numeric(class),          # fill should be discrete
    pland = as.numeric(pland)          # ensure numeric
  )

# Add color codes
data = data %>% 
  dplyr::left_join(class_colors, by = c("class" = "Class_ID"))

# Plot
ggplot(data, aes(x = year, y = pland, fill = Description, group = Description)) +
  geom_area(alpha = 0.7, position = "stack") +  # Stacked areas
  geom_line(aes(color = Description), linewidth = 1, position = "stack") +  # Lines colored by class
  geom_point(aes(color = Description), size = 1, position = "stack") + # Points colored by class
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  scale_color_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  labs(x = "Year", y = "Percentage of landscape", fill = "Land use", color = "Land use") +
  theme_classic()

##### Stacked barplot (in %) ------
ggplot(data, aes(x = year, y = pland, fill = Description)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.2) +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  labs(
    x = "Year",
    y = "Percentage of landscape (%)",
    fill = "Land use"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


##### Barplot (positive vs negative changes) ------
data_diff = class_metrics %>%
  dplyr::select(year, class, ca) %>%
  dplyr::mutate(
    year = as.integer(year),
    class = as.integer(class),
    ca = as.numeric(ca)
  ) %>%
  # Keep only Forest (1), Agriculture (3), Artificial (5)
  dplyr::filter(class %in% c(1, 3, 5)) %>%
  # Compute change in surface
  dplyr::group_by(class) %>%
  dplyr::mutate(delta_ca = ca - dplyr::lag(ca, order_by = year)) %>%
  dplyr::filter(!is.na(delta_ca)) %>%
  dplyr::ungroup() %>%
  # Add readable names and colors
  dplyr::mutate(
    class_name = dplyr::case_when(
      class == 1 ~ "Forest",
      class == 3 ~ "Agriculture",
      class == 5 ~ "Artificial",
      TRUE ~ as.character(class)
    ),
    color = dplyr::case_when(
      class == 1 ~ "#32a65e",
      class == 3 ~ "#FFFFB2",
      class == 5 ~ "#d4271e" 
    )
  )

# Plot horizontal barplot
ggplot(data_diff, aes(x = factor(year), y = delta_ca, fill = class_name)) +
  geom_col(position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  scale_fill_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FFFFB2",
    "Artificial" = "#d4271e"
  )) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  labs(
    x = "Year",
    y = "Change in surface (ca)",
    fill = "Land use",
    title = "Year-to-year evolution in surface (Forest, Agriculture, Artificial)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


### Forest class metrics -----------
#### Summary statistics ----
# Evolution per class per year
forest_class_metrics %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(area_change = (area_mn - dplyr::lag(area_mn)) / dplyr::lag(area_mn) * 100,
                np_change = (np - dplyr::lag(np)) / dplyr::lag(np) * 100,
                FFI_change = (FFI - dplyr::lag(FFI)) / dplyr::lag(FFI) * 100,
                ECA_change_2km = (eca_pct_land_2000 - dplyr::lag(eca_pct_land_2000)) / dplyr::lag(eca_pct_land_2000) * 100,
                ECA_change_8km = (eca_pct_land_8000 - dplyr::lag(eca_pct_land_8000)) / dplyr::lag(eca_pct_land_8000) * 100) %>% 
  dplyr::select(year, area_change, np_change, FFI_change, ECA_change_2km, ECA_change_8km) %>% 
  print(n=35)

# Changes betwenn 1989 and 2023
forest_class_metrics %>%
  dplyr::summarize(area_change = area_mn[year == max(year)] - area_mn[year == min(year)],
                   np_change = np[year == max(year)] - np[year == min(year)],
                   FFI_change = FFI[year == max(year)] - FFI[year == min(year)],
                   ECA_change_2km = eca_pct_land_2000[year == max(year)] - eca_pct_land_2000[year == min(year)],
                   ECA_change_8km = eca_pct_land_8000[year == max(year)] - eca_pct_land_8000[year == min(year)],
                   area_change_perc = ((area_mn[year == max(year)] - area_mn[year == min(year)]) / area_mn[year == min(year)]) * 100,
                   np_change_perc = ((np[year == max(year)] - np[year == min(year)]) / np[year == min(year)]) * 100,
                   FFI_change_perc = ((FFI[year == max(year)] - FFI[year == min(year)]) / FFI[year == min(year)]) * 100,
                   ECA_change_2km_perc = ((eca_pct_land_2000[year == max(year)] - eca_pct_land_2000[year == min(year)]) / eca_pct_land_2000[year == min(year)]) * 100,
                   ECA_change_8km_perc = ((eca_pct_land_8000[year == max(year)] - eca_pct_land_8000[year == min(year)]) / eca_pct_land_8000[year == min(year)]) * 100)


# Between sets of years
forest_class_metrics %>%
  dplyr::summarize(
    # --- Absolute changes ---
    area_change_1989_2000 = area_mn[year == 2000] - area_mn[year == 1989],
    area_change_2000_2012 = area_mn[year == 2012] - area_mn[year == 2000],
    area_change_2012_2023 = area_mn[year == 2023] - area_mn[year == 2012],
    
    np_change_1989_2000 = np[year == 2000] - np[year == 1989],
    np_change_2000_2012 = np[year == 2012] - np[year == 2000],
    np_change_2012_2023 = np[year == 2023] - np[year == 2012],
    
    FFI_change_1989_2000 = FFI[year == 2000] - FFI[year == 1989],
    FFI_change_2000_2012 = FFI[year == 2012] - FFI[year == 2000],
    FFI_change_2012_2023 = FFI[year == 2023] - FFI[year == 2012],
    
    ECA_change_1989_2000_2km = eca_pct_land_2000[year == 2000] - eca_pct_land_2000[year == 1989],
    ECA_change_2000_2012_2km = eca_pct_land_2000[year == 2012] - eca_pct_land_2000[year == 2000],
    ECA_change_2012_2023_2km = eca_pct_land_2000[year == 2023] - eca_pct_land_2000[year == 2012],
    
    # --- Percentage changes ---
    area_change_perc_1989_2000 = ((area_mn[year == 2000] - area_mn[year == 1989]) / area_mn[year == 1989]) * 100,
    area_change_perc_2000_2012 = ((area_mn[year == 2012] - area_mn[year == 2000]) / area_mn[year == 2000]) * 100,
    area_change_perc_2012_2023 = ((area_mn[year == 2023] - area_mn[year == 2012]) / area_mn[year == 2012]) * 100,
    
    np_change_perc_1989_2000 = ((np[year == 2000] - np[year == 1989]) / np[year == 1989]) * 100,
    np_change_perc_2000_2012 = ((np[year == 2012] - np[year == 2000]) / np[year == 2000]) * 100,
    np_change_perc_2012_2023 = ((np[year == 2023] - np[year == 2012]) / np[year == 2012]) * 100,
    
    FFI_change_perc_1989_2000 = ((FFI[year == 2000] - FFI[year == 1989]) / FFI[year == 1989]) * 100,
    FFI_change_perc_2000_2012 = ((FFI[year == 2012] - FFI[year == 2000]) / FFI[year == 2000]) * 100,
    FFI_change_perc_2012_2023 = ((FFI[year == 2023] - FFI[year == 2012]) / FFI[year == 2012]) * 100,
    
    ECA_change_perc_1989_2000_2km = ((eca_pct_land_2000[year == 2000] - eca_pct_land_2000[year == 1989]) / 
                                       eca_pct_land_2000[year == 1989]) * 100,
    ECA_change_perc_2000_2012_2km = ((eca_pct_land_2000[year == 2012] - eca_pct_land_2000[year == 2000]) / 
                                       eca_pct_land_2000[year == 2000]) * 100,
    ECA_change_perc_2012_2023_2km = ((eca_pct_land_2000[year == 2023] - eca_pct_land_2000[year == 2012]) / 
                                       eca_pct_land_2000[year == 2012]) * 100
  )



#### Plots -----
##### Line plot -----
data_long = forest_class_metrics %>%
  dplyr::select(
    year,
    ca,
    np,
    area_mn,
    FFI,
    eca_pct_land_2000,
    eca_pct_land_8000
  ) %>% 
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
      Metric == "eca_pct_land_2000" ~ "Equivalent Connected Area (ECA, 2km)",
      Metric == "eca_pct_land_8000" ~ "Equivalent Connected Area (ECA, 8km)",
      TRUE ~ Metric
    )
  )

# Plot facets
ggplot(data_long, aes(x = year, y = Value)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "forestgreen", size = 3) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Temporal Evolution of Forest Landscape Metrics",
    x = "Year",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )


### Forest core and corridor metrics -----------
#### Summary statistics -----
# Evolution per class per year
forest_core_corridor_metrics %>%
  dplyr::arrange(class, year) %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(area_change = (area_mn - dplyr::lag(area_mn)) / dplyr::lag(area_mn) * 100,
                np_change = (np - dplyr::lag(np)) / dplyr::lag(np) * 100,
                ca_change = (ca - dplyr::lag(ca)) / dplyr::lag(ca) * 100) %>% 
  dplyr::select(year, area_change, np_change, ca_change) %>% 
  print(n=70)

# Changes betwenn 1989 and 2023
forest_core_corridor_metrics %>%
  dplyr::group_by(class) %>% 
  dplyr::summarize(area_change = area_mn[year == max(year)] - area_mn[year == min(year)],
                   np_change = np[year == max(year)] - np[year == min(year)],
                   ca_change = ca[year == max(year)] - ca[year == min(year)],
                   area_change_perc = ((area_mn[year == max(year)] - area_mn[year == min(year)]) / area_mn[year == min(year)]) * 100,
                   np_change_perc = ((np[year == max(year)] - np[year == min(year)]) / np[year == min(year)]) * 100,
                   ca_change_perc = ((ca[year == max(year)] - ca[year == min(year)]) / ca[year == min(year)]) * 100)

# Between sets of years
forest_core_corridor_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(
    np_1989_2000 = np[year == 2000] - np[year == 1989],
    np_2000_2012 = np[year == 2012] - np[year == 2000],
    np_2012_2023 = np[year == 2023] - np[year == 2012]) %>%
  dplyr::ungroup()

#### Plots ----
##### Line plot -----
# Prepare long data (with explicit dplyr:: prefixes)
data_long = forest_core_corridor_metrics %>%
  dplyr::select(type, year, area_mn, ca, np) %>%
  tidyr::pivot_longer(
    cols = c(area_mn, ca, np),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Metric = dplyr::case_when(
      Metric == "area_mn" ~ "Mean patch size (ha)",
      Metric == "ca"      ~ "Surface area (ha)",
      Metric == "np"      ~ "Number of patches",
      TRUE ~ Metric
    ),
    type = dplyr::case_when(
      grepl("core", type, ignore.case = TRUE)     ~ "Core",
      grepl("corridor", type, ignore.case = TRUE) ~ "Corridor",
      TRUE ~ type
    )
  )

# split
data_core = data_long %>% dplyr::filter(type == "Core")
data_corr = data_long %>% dplyr::filter(type == "Corridor")

# Core plot: 3 rows (one metric per row), free y per panel
core_plot = ggplot(data_core, aes(x = year, y = Value)) +
  geom_line(color = "forestgreen", linewidth = 1) +
  geom_point(color = "forestgreen", size = 2.5) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  labs(title = "Core", x = "Year", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Corridor plot: 3 rows, free y per panel
corr_plot = ggplot(data_corr, aes(x = year, y = Value)) +
  geom_line(color = "#1f78b4", linewidth = 1) +
  geom_point(color = "#1f78b4", size = 2.5) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  labs(title = "Corridor", x = "Year", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Combine side-by-side: Core left, Corridor right
combined = core_plot + corr_plot + plot_layout(ncol = 2, widths = c(1, 1))

# Print
combined
