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
  Class_ID = factor(c(1,2,3,4,5,33)),
  Description = c("Core forest","Other natural habitats","Agriculture","Water","Artificial","Forest corridors"
  ),
  Color = c(
    "#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e","chartreuse"
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


#### Line plot -----------
# Pivot metrics to long format
data = forest_class_metrics %>%
  dplyr::filter(class==1) %>% 
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


# # Plot
# ggplot(forest_class_metrics_ED, aes(x = as.numeric(year), y = FFI)) +
#   geom_line(color = "darkgreen", linewidth = 1) +
#   geom_point(color = "forestgreen", size = 3) +
#   labs(
#     title = "Temporal Evolution of the Forest Fragmentation Index (FFI)",
#     x = "Year",
#     y = "FFI (normalized 0–1)"
#   ) +
#   theme_minimal(base_size = 14)
# 
# # Compute scaling factor for np (to align visually with FFI)
# scale_factor = max(forest_class_metrics_ED$np, na.rm = TRUE)
# # Plot
# ggplot(forest_class_metrics_ED, aes(x = as.numeric(year))) +
#   # FFI (solid)
#   geom_line(aes(y = FFI, color = "FFI"), linewidth = 1) +
#   geom_point(aes(y = FFI, color = "FFI"), size = 3) +
#   # np (dashed)
#   geom_line(aes(y = np / scale_factor, color = "Number of patches (NP)"),
#             linetype = "dashed", linewidth = 1) +
#   geom_point(aes(y = np / scale_factor, color = "Number of patches (NP)"),
#              size = 2, shape = 1) +
#   # Dual Y axes
#   scale_y_continuous(
#     name = "FFI (normalized 0–1)",
#     sec.axis = sec_axis(~ . * scale_factor, name = "Number of patches (NP)")
#   ) +
#   # Colors and legend
#   scale_color_manual(
#     name = "",
#     values = c("FFI" = "darkgreen", "Number of patches (NP)" = "black"),
#     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed")))
#   ) +
#   labs(
#     title = "Temporal Evolution of Forest Fragmentation Index (FFI) and Number of Forest Patches",
#     x = "Year"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "top",
#     axis.title.y.left = element_text(color = "darkgreen"),
#     axis.title.y.right = element_text(color = "black")
#   )
# 
# 
# # Compute scaling factor to align pland with FFI visually
# scale_factor = max(forest_class_metrics_ED$pland, na.rm = TRUE)
# ggplot(forest_class_metrics_ED, aes(x = as.numeric(year))) +
#   # FFI line
#   geom_line(aes(y = FFI, color = "FFI"), linewidth = 1) +
#   geom_point(aes(y = FFI, color = "FFI"), size = 3) +
#   # pland line (scaled to same range as FFI for plotting)
#   geom_line(aes(y = pland / scale_factor, color = "Forest cover (%)"), 
#             linetype = "dashed", linewidth = 1) +
#   geom_point(aes(y = pland / scale_factor, color = "Forest cover (%)"), size = 2, shape = 1) +
#   # Axes and legend
#   scale_y_continuous(
#     name = "FFI (normalized 0–1)",
#     sec.axis = sec_axis(~ . * scale_factor, name = "Forest cover (%)")
#   ) +
#   scale_color_manual(
#     name = "",
#     values = c("FFI" = "darkgreen", "Forest cover (%)" = "black"),
#     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed")))
#   ) +
#   labs(
#     title = "Temporal Evolution of Forest Fragmentation Index (FFI) and Forest Cover",
#     x = "Year"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "top",
#     axis.title.y.right = element_text(color = "black"),
#     axis.title.y.left = element_text(color = "darkgreen")
#   )

