#------------------------------------------------#
# Author: Romain Monassier
# Objective: Plot landscape metrics through time
#------------------------------------------------#

library(tidyr)
library(ggplot2)
library(readr)
library(ggalluvial)
library(here)
library(PantaRhei)


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
transition_matrix = readr::read_csv(
  file.path(base_path, "transition_matrix_bbox_1989_2023.csv"),
  show_col_types = FALSE
)

### Class metrics (overall) ----------------

#### Stacked area chart ------
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


#### Create stacked barplot (in %) ------
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


#### Positive vs negative changes throughout time (barplot) ------
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
#### Line plot -----
data_long = forest_class_metrics %>%
  dplyr::select(
    year,
    ca,
    np,
    area_mn,
    FFI,
    `Normalized ECA (% of LA)`
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
      Metric == "Normalized ECA (% of LA)" ~ "Equivalent Connected Area (ECA, 8 km)",
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
#### Line plot -----
data_long = forest_core_corridor_metrics %>%
  dplyr::select(
    year,
    core_area_mn,
    core_np,
    corr_area_mn,
    corr_np
  ) %>% 
  dplyr::mutate(year = as.numeric(year)) %>%
  tidyr::pivot_longer(
    cols = -year,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Metric = dplyr::case_when(
      Metric == "core_area_mn" ~ "Mean patch size (core forest, ha)",
      Metric == "core_np" ~ "Number of patches (core forest)",
      Metric == "corr_area_mn" ~ "Mean patch size (forest corridors, ha)",
      Metric == "corr_np" ~ "Number of patches (forest corridors)",
      TRUE ~ Metric
    )
  )

# Plot facets
ggplot(data_long, aes(x = year, y = Value)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "forestgreen", size = 3) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Temporal Evolution of Forest Landscape Metrics (Core and Corridor)",
    x = "Year",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

### Transition matrix ----
#### Sankey diagram (deforestation) -----
## With ggalluvial
transition_summary = transition_matrix %>%
  dplyr::filter(year_1989 == 1,
                !year_2001 %in% c(2, 4),
                !year_2013 %in% c(2, 4),
                !year_2023 %in% c(2, 4)) %>% 
  dplyr::group_by(year_1989, year_2001, year_2013, year_2023) %>%
  dplyr::summarise(Freq = n(), .groups = "drop") %>%
  dplyr::arrange(desc(Freq))
head(transition_summary, 10)

# Plot
ggplot(transition_summary,
       aes(axis1 = as.factor(year_1989),
           axis2 = as.factor(year_2001),
           axis3 = as.factor(year_2013),
           axis4 = as.factor(year_2023),
           y = Freq)) +
  geom_alluvium(aes(fill = as.factor(year_2001)), width = 1/12, alpha = 0.85) +
  geom_stratum(width = 1/12, fill = "grey90", color = "grey40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_x_discrete(labels = c("1989", "2001", "2013", "2023"), expand = c(.2, .05)) +
  scale_fill_manual(values = c(
    "1" = "#32a65e",
    "3" = "#FFFFB2",
    "5" = "#d4271e")) +
  ggtitle("Land cover trajectories of deforestation", subtitle = "From 1989 to 2023") +
  theme_void(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

## With PantaRhei





#### Sankey diagram (reforestation) -----
## With ggalluvial
transition_summary = transition_matrix %>%
  dplyr::filter(!year_1989 %in% c(1, 4),
                year_2001 == 1,
                !year_2013 %in% c(4),
                year_2023 == 1) %>% 
  dplyr::group_by(year_1989, year_2001, year_2013, year_2023) %>%
  dplyr::summarise(Freq = n(), .groups = "drop") %>%
  dplyr::arrange(desc(Freq))
head(transition_summary, 10)

# Plot
ggplot(transition_summary,
       aes(axis1 = as.factor(year_1989),
           axis2 = as.factor(year_2001),
           axis3 = as.factor(year_2013),
           axis4 = as.factor(year_2023),
           y = Freq)) +
  geom_alluvium(aes(fill = as.factor(year_2013)), width = 1/12, alpha = 0.85) +
  geom_stratum(width = 1/12, fill = "grey90", color = "grey40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_x_discrete(labels = c("1989", "2001", "2013", "2023"), expand = c(.2, .05)) +
  scale_fill_manual(values = c(
    "1" = "#32a65e",
    "2" = "#ad975a",
    "3" = "#FFFFB2",
    "5" = "#d4271e")) +
  ggtitle("Land cover trajectories of reforestation", subtitle = "From 1989 to 2023") +
  theme_void(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")
