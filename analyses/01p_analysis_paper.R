#------------------------------------------------#
# Authors: Romain Monassier, Valéria Romano, Anne-Marie Farnet Da Silva
# Script accompanying the paper entitled: "..."
#------------------------------------------------#

library(here)
library(dplyr)
library(scales)
library(ggplot2)
library(ggbreak)
library(extrafont)
library(patchwork)
library(glmmTMB)
library(MASS)
library(DHARMa)
library(pROC)
library(performance)
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

### Summary ------

## Number of sampling units
# Number of pixels (deforestation)
train_data_pixel_defor %>% 
  dplyr::bind_rows(test_data_pixel_defor) %>% 
  dplyr::summarise(n=dplyr::n_distinct(cell_id))
# Number of pixels (reforestation)
train_data_pixel_refor %>% 
  dplyr::bind_rows(test_data_pixel_refor) %>% 
  dplyr::summarise(n=dplyr::n_distinct(cell_id))
# Number of properties (deforestation)
train_data_car_defor %>% 
  dplyr::bind_rows(test_data_car_defor) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::summarise(n=dplyr::n_distinct(car_id))
# Number of properties (reforestation)
train_data_car_refor %>% 
  dplyr::bind_rows(test_data_car_refor) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::summarise(n=dplyr::n_distinct(car_id))

## Number of pixels per year
# Deforestation
train_data_pixel_defor %>% 
  dplyr::bind_rows(test_data_pixel_defor) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(n=dplyr::n_distinct(cell_id)) %>% 
  dplyr::mutate(prop = n*100/sum(n)) %>% 
  dplyr::arrange(desc(prop)) %>% 
  print(n=35)
# Reforestation
train_data_pixel_refor %>% 
  dplyr::bind_rows(test_data_pixel_refor) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(n=dplyr::n_distinct(cell_id)) %>% 
  dplyr::mutate(prop = n*100/sum(n)) %>% 
  dplyr::arrange(desc(prop)) %>% 
  print(n=35)

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
    Description == "Forest" ~ ca_end + 5000,
    Description == "Agriculture" ~ ca_end - 4000,
    Description == "Built-up" ~ ca_end + 6000,
    Description == "Water" ~ ca_end + 4000,
    Description == "Wetland" ~ ca_end + 1000,
    Description == "Non-forest formation" ~ ca_end - 4000,
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
    legend.position = "none",
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
waffles_col = waffle1989 / waffle2024 / waffle2100 +
  plot_layout(heights = c(1, 1, 1))

final_plot = lulcc_plot | waffles_col +
  plot_layout(widths = c(2.7, 1)) &   # slightly shrink gap visually
  theme(plot.margin = margin(2, 2, 2, 2))
plot(final_plot)


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
      term %in% c("South of BR-101") ~ "Geographical",
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
coef_defor = coef_defor %>%
  dplyr::arrange(group, OR) %>%
  dplyr::mutate(term_ordered = factor(term, levels = term))

# Forest plot for odds ratios
ggplot(coef_defor, aes(x = OR, y = term_ordered, color = color)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = OR_low, xmax = OR_high), width = 0.4, linewidth = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_identity() +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.y = element_text(hjust = 0),
    strip.text.y = element_text(face = "bold")
  ) +
  labs(
    x = "Standardized odds ratio",
    y = NULL
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y")

### Reforestation -------
## Property-scale ------
### Deforestation ------
### Reforestatio -------

# 3. Hotspots of reforestation and deforestation ----------