
# Author: Romain Monassier
# Objective: Plot landscape metrics through time
#------------------------------------------------#

library(tidyr)
library(ggplot2)
library(readr)
library(here)
library(grid)
library(patchwork)
library(stringr)
library(tibble)
library(scales)
library(ggrepel)
# remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(ggtern)

### Import datasets ----------------
base_path = here("outputs", "data", "landscapemetrics")
all_lulc_metrics = readr::read_csv(
  file.path(base_path, "all_lulc_classes_bbox_1989_2024.csv"),
  show_col_types = FALSE
)
forest_class_metrics = readr::read_csv(
  file.path(base_path, "forest_class_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE
)
forest_core_corridor_metrics = readr::read_csv(
  file.path(base_path, "forest_core_corridors_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE
)
forest_cat_metrics = readr::read_csv(
  file.path(base_path, "forest_cat_metrics_bbox_2024.csv"),
  show_col_types = FALSE
)
cons_cat_metrics = readr::read_csv(
  file.path(base_path, "cons_cat_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE
)
forest_age_metrics = readr::read_csv(
  file.path(base_path, "forest_age_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE
)
defor_refor_metrics = readr::read_csv(
  file.path(base_path, "defor_refor_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE
)


#### Plots -------
# Create color palette using the Legend codes provided by MapBiomas
class_colors = tibble::tibble(
  class = c(1,2,3,4,5,6),
  Description = c("Forest","Other non-forest formation", "Wetlands and mangroves", "Agriculture","Water","Artificial"
  ),
  Color = c(
    "#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"
  )
)


##### Donut plot --------
# prepare data for donut
data <- all_lulc_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::left_join(class_colors, by="class") %>%
  dplyr::select(Description, ca, pland) %>%
  dplyr::arrange(desc(pland))

donut <- data %>%
  dplyr::mutate(
    fraction = pland / sum(pland),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    labelPos = (ymax + ymin) / 2,
    label = paste0(round(pland,1),"%")
  )

# donut plot
png(here("outputs","plot","01h_lm_pland_2024_donut.png"), width = 1500, height = 800, res = 300)

ggplot(donut, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Description)) +
  geom_rect() +
  geom_text_repel(
    aes(x = 4.2, y = labelPos, label = label),
    size = 3,
    nudge_x = 0.5,
    segment.color = "grey50",
    direction = "y"
  ) +
  coord_polar(theta = "y") +
  xlim(c(0, 4.8)) +
  theme_void() +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  ggtitle("Land use composition in 2024") +
  theme(legend.position = "right")

dev.off()

###### Text -------
res_2024 <- data %>%
  dplyr::mutate(txt = paste0(Description, " covered ", round(ca), " ha (", round(pland,1), "%) in 2024.")) %>%
  dplyr::pull(txt)
cat("Land-use composition in 2024 was as follows:\n")
cat(paste0("* ", res_2024, collapse="\n"), "\n\n")


##### Stacked area chart ------
# Select data
data = all_lulc_metrics %>% 
  dplyr::select(year, class, pland) %>% 
  dplyr::mutate(
    year  = as.integer(year),          # x axis must be numeric/continuous
    class = as.numeric(class),          # fill should be discrete
    pland = as.numeric(pland)          # ensure numeric
  )

# Add color codes
data = data %>% 
  dplyr::left_join(class_colors, by = c("class"))

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
png(here("outputs","plot","01h_lm_pland_barplot.png"), width = 2000, height = 1000, res = 300)

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

dev.off()

###### Text --------
# Evolution from 1989 and 2024
data <- all_lulc_metrics %>%
  dplyr::left_join(class_colors, by="class") %>%
  dplyr::group_by(Description) %>%
  dplyr::summarise(
    ca1989 = ca[year==1989],
    ca2024 = ca[year==2024],
    d_ca = ca2024 - ca1989,
    ca_pct_change = 100 * d_ca / ca1989
  ) %>%
  dplyr::ungroup()

# Text
res_change <- data %>%
  dplyr::mutate(
    txt = paste0(
      Description, " changed by ",
      ifelse(d_ca>=0, "+",""), round(d_ca), " ha (",
      ifelse(ca_pct_change>=0, "+",""), round(ca_pct_change,1), " percentage points) between 1989 and 2024."
    )
  ) %>%
  dplyr::pull(txt)

cat("Changes between 1989 and 2024 were:\n")
cat(paste0("* ", res_change, collapse="\n"), "\n")

##### Line plot -----
# Create color palette using the Legend codes provided by MapBiomas
class_colors = tibble::tibble(
  class = c(1,2,3,4,5,6),
  Description = c(
    "Forest",
    "Other non-forest formation",
    "Wetlands and mangroves",
    "Agriculture",
    "Water",
    "Artificial"
  ),
  Color = c(
    "#32a65e", "#ad975a", "#519799", "#FAD991", "#0000FF", "#d4271e"
  )
)

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
label_data <- data %>%
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

label_data <- label_data %>%
  dplyr::mutate(ca_end_adj = dplyr::case_when(
    Description == "Artificial" ~ ca_end + 6000,
    Description == "Water" ~ ca_end + 4000,
    Description == "Wetlands and mangroves" ~ ca_end + 1000,
    Description == "Other non-forest formation" ~ ca_end - 4000,
    TRUE ~ ca_end
  ))

# Plot
png(here("outputs","plot","01h_lm_ca_all_lulcc_line_plot.png"), width = 2700, height = 1900, res = 300)

x_max <- max(data$year)
ggplot(data, aes(x = year, y = ca, color = Description, group = Description)) +
  geom_ribbon(aes(ymin = ca - 2000, ymax = ca + 2000, fill = Description), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  geom_text(data = label_data,
            aes(x = year_end + 0.6, y = ca_end_adj, label = label, color = Description),
            hjust = 0, size = 5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description), guide = "none") +
  labs(x = "Year", y = "Area (ha)", color = "Land use") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +   # small left margin + larger right margin
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(legend.position = "bottom", plot.margin = margin(10, 80, 10, 10))

dev.off()

###### Text -------
res_trend <- label_data %>%
  dplyr::mutate(
    txt = paste0(
      Description, " ",
      ifelse(change_ha >= 0, "increased", "decreased"),
      " by ", scales::comma(abs(round(change_ha))), " ha (",
      sprintf("%+.1f", pct_change), "%).")
  ) %>%
  dplyr::pull(txt)

cat("Long-term land-use changes were:\n")
cat(paste0("* ", res_trend, collapse = "\n"), "\n\n")

##### Barplot (positive vs negative changes) ------
# Prepare data
data = all_lulc_metrics %>%
  dplyr::select(year, class, ca) %>%
  dplyr::mutate(
    year = as.integer(year),
    class = as.integer(class),
    ca = as.numeric(ca)
  ) %>%
  # Keep only Forest (1), Agriculture (4), Artificial (6)
  dplyr::filter(class %in% c(1, 4, 6)) %>%
  # Compute change in surface
  dplyr::group_by(class) %>%
  dplyr::mutate(delta_ca = ca - dplyr::lag(ca, order_by = year)) %>%
  dplyr::filter(!is.na(delta_ca)) %>%
  dplyr::ungroup() %>%
  # Add readable names and colors
  dplyr::mutate(
    class_name = dplyr::case_when(
      class == 1 ~ "Forest",
      class == 4 ~ "Agriculture",
      class == 6 ~ "Artificial",
      TRUE ~ as.character(class)
    ),
    color = dplyr::case_when(
      class == 1 ~ "#32a65e",
      class == 4 ~ "#FAD991",
      class == 6 ~ "#d4271e" 
    )
  )

# 1) Plot: dodged bars (Δha) + connecting lines & points per class (years numeric)
ggplot() +
  
  # dodged bars (keeps your original visual)
  geom_col(
    data = data,
    aes(x = year, y = delta_ca, fill = class_name),
    position = position_dodge(width = 0.7),
    color = "black",
    width = 0.6
  ) +
  
  # connecting lines: use numeric year so lines connect centers across years
  geom_line(
    data = data,
    aes(x = year, y = delta_ca, group = class_name, color = class_name),
    linewidth = 0.5
  ) +
  
  # points at each year on the lines
  geom_point(
    data = data,
    aes(x = year, y = delta_ca, color = class_name),
    size = 1.8
  ) +
  
  scale_fill_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FAD991",
    "Artificial" = "#d4271e"
  )) +
  scale_color_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FAD991",
    "Artificial" = "#d4271e"
  )) +
  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  
  # show all year ticks nicely
  scale_x_continuous(breaks = unique(data$year), labels = unique(data$year)) +
  
  labs(
    x = "Year",
    y = "Change in surface (ca)",
    fill = "Land use",
    color = "Land use",
    title = "Year-to-year evolution in surface (Forest, Agriculture, Artificial)"
  ) +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# With a smoothed line
png(here("outputs","plot","01h_lm_pland_barplot0.png"), width = 2500, height = 1500, res = 300)

ggplot() +
  geom_col(
    data = data,
    aes(x = year, y = delta_ca, fill = class_name),
    position = position_dodge(width = 0.7),
    color = "black",
    width = 0.6
  ) +
  geom_smooth(
    data = data,
    aes(x = year, y = delta_ca, color = class_name),
    method = "loess",
    se = FALSE,
    linewidth = 1,
    linetype = "longdash",
    span = 0.300 # change the span to change the smoothening of the line
  ) +
  scale_fill_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FAD991",
    "Artificial" = "#d4271e"
  )) +
  scale_color_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FAD991",
    "Artificial" = "#d4271e"
  )) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  scale_x_continuous(breaks = unique(data$year), labels = unique(data$year)) +
  labs(
    x = "Year",
    y = "Change in surface (ca)",
    fill = "Land use",
    color = "Land use",
    title = "Year-to-year evolution in surface (Forest, Agriculture, Artificial)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

dev.off()

# 3) ... and the forest dynamics
# Identify forest increase/decrease runs
forest_dyn <- data %>%
  dplyr::filter(class_name == "Forest") %>%
  dplyr::mutate(trend = ifelse(delta_ca > 0, "inc", "dec")) %>%
  dplyr::group_by(grpid = data.table::rleid(trend)) %>%
  dplyr::summarise(
    start = min(as.integer(year)),
    end   = max(as.integer(year)),
    trend = dplyr::first(trend),
    n     = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::filter(n > 1) %>%
  dplyr::mutate(
    label = dplyr::case_when(
      trend == "inc" ~ "forest increasing",
      trend == "dec" ~ "forest decreasing"
    ),
    start = start - 0.5,
    end = end + 0.5
  )

# Define bracket positions (larger gaps, scaling order)
forest_dyn <- forest_dyn %>%
  dplyr::mutate(
    min_y = min(data$delta_ca, na.rm = TRUE),
    y_base = min_y * 0.8 - (dplyr::row_number() - 1) * abs(min_y) * 0.12,  # progressive lowering
    height = abs(min_y) * 0.04
  )

# Colors for rectangles
rect_colors <- c(
  "forest increasing" = "#66BB6A",
  "forest decreasing" = "#E57373"
)

# Final plot
p_forest <- ggplot(data, aes(x = year, y = delta_ca, fill = class_name)) +
  # Shaded rectangles (forest dynamics, not in legend)
  geom_rect(
    data = forest_dyn,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = label
    ),
    inherit.aes = FALSE,
    alpha = 0.15,
    show.legend = FALSE
  ) +
  
  # Columns and smoothed lines
  geom_col(position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_smooth(aes(color = class_name), se = FALSE, span = 0.3, linewidth = 1.2, show.legend = FALSE) +
  
  # Black brackets (below, staggered)
  geom_segment(
    data = forest_dyn,
    aes(x = start, xend = end, y = y_base, yend = y_base),
    inherit.aes = FALSE,
    linewidth = 0.9,
    colour = "black"
  ) +
  geom_segment(
    data = forest_dyn,
    aes(x = start, xend = start, y = y_base, yend = y_base + height),
    inherit.aes = FALSE,
    linewidth = 0.9,
    colour = "black"
  ) +
  geom_segment(
    data = forest_dyn,
    aes(x = end, xend = end, y = y_base, yend = y_base + height),
    inherit.aes = FALSE,
    linewidth = 0.9,
    colour = "black"
  ) +
  
  # Text below brackets (no arrows)
  geom_text(
    data = forest_dyn,
    aes(x = (start + end) / 2, y = y_base - height * 1.2, label = label),
    inherit.aes = FALSE,
    size = 4,
    fontface = "italic"
  ) +
  
  # Unified legend (fill only, no color legend)
  scale_fill_manual(
    values = c(
      "Forest" = "#32a65e",
      "Agriculture" = "#FFFFB2",
      "Artificial" = "#d4271e",
      "forest increasing" = "#66BB6A",
      "forest decreasing" = "#E57373"
    ),
    breaks = c("Forest", "Agriculture", "Artificial"),
    labels = c("Forest", "Agriculture", "Artificial"),
    name = "Land use"
  ) +
  scale_color_manual(
    values = c(
      "Forest" = "#32a65e",
      "Agriculture" = "#FFFFB2",
      "Artificial" = "#d4271e"
    ),
    guide = "none"
  ) +
  
  # Labels & Theme
  labs(
    x = "Year",
    y = "Change in surface (ha)",
    title = "Year-to-year evolution in surface of main land uses"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

p_forest


###### Text -------
# filter only Forest time series
data <- all_lulc_metrics %>%
  dplyr::filter(class == 1) %>%
  dplyr::arrange(year)

# create the period table again
forest_dyn <- data %>%
  dplyr::mutate(delta = ca - dplyr::lag(ca),
                trend = ifelse(delta > 0,"inc","dec")) %>%
  dplyr::filter(!is.na(trend)) %>%
  dplyr::mutate(group = data.table::rleid(trend)) %>%
  dplyr::group_by(group, trend) %>%
  dplyr::summarise(start_year = min(year),
                   end_year = max(year),
                   ca_start= ca[year == start_year],
                   ca_end = ca[year == end_year],
                   .groups="drop") %>%
  # remove single-year groups
  dplyr::filter(end_year > start_year) %>%
  dplyr::mutate(delta_total = ca_end - ca_start)

forest_text <- forest_dyn %>%
  dplyr::mutate(
    txt = dplyr::case_when(
      trend == "inc" ~ paste0("From ", start_year, " to ", end_year, 
                              " the forest area increased by ",
                              round(delta_total,0), " ha."),
      TRUE ~ paste0("From ", start_year, " to ", end_year, 
                    " the forest area decreased by ",
                    abs(round(delta_total,0)), " ha.")
    )
  ) %>%
  dplyr::pull(txt)

# print the text
cat("Forest temporal dynamics were structured in the following distinct periods:\n")
cat(paste0("* ", forest_text, collapse="\n"), "\n\n")


##### Ternary plot --------
# 1. Filter for the classes we need
data <- all_lulc_metrics %>%
  dplyr::filter(class %in% c(1, 4, 6)) %>%
  dplyr::select(year, class, pland)

# 2. Pivot data so each row is a year and columns are forest, agriculture, urban
data <- data %>%
  dplyr::mutate(class_name = case_when(
    class == 1 ~ "Forest",
    class == 4 ~ "Agriculture",
    class == 6 ~ "Artificial"
  )) %>%
  dplyr::select(-class) %>%
  tidyr::pivot_wider(names_from = class_name, values_from = pland)
data$Forest = data$Forest/100
data$Agriculture = data$Agriculture/100
data$Artificial = data$Artificial/100
summary(data[, c("Forest", "Agriculture", "Artificial")])

# 3. Plot ternary diagram
ggtern(data = data, aes(x = Agriculture, y = Artificial, z = Forest)) +
  geom_point(aes(color = year), size = 3) +
  scale_T_continuous(limits = c(0.1, 0.2)) + # artificial
  scale_L_continuous(limits = c(0.3, 0.4)) + # agriculture
  scale_R_continuous(limits = c(0.5, 0.6)) + # forest
  theme_rgbw()


# Forest categories metrics --------
#### Summary statistics ------
# Evolution per class per year
forest_cat_metrics %>%
  dplyr::arrange(class, year) %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(pland_change_year = round((pland - dplyr::lag(pland)), 1),
                ca_change_year = ca - dplyr::lag(ca),
                ca_change_year_pct = (ca - dplyr::lag(ca)) / dplyr::lag(ca) * 100) %>%
  dplyr::ungroup()

# Changes betwenn first and last year
forest_cat_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(pland_change_total = round(pland[year == max(year)] - pland[year == min(year)], 1),
                   ca_change_total = ca[year == max(year)] - ca[year == min(year)])

# Between sets of years
forest_cat_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(
    pland_change_1989_2000 = round(pland[year == 2000] - pland[year == 1989], 1),
    pland_change_2000_2012 = round(pland[year == 2012] - pland[year == 2000], 1),
    pland_change_2012_2024 = round(pland[year == 2024] - pland[year == 2014], 1),
    ca_change_1989_2000 = ca[year == 2000] - ca[year == 1989],
    ca_change_2000_2012 = ca[year == 2012] - ca[year == 2000],
    ca_change_2012_2024 = ca[year == 2024] - ca[year == 2014]) %>%
  dplyr::ungroup()

#### Plots ----------
##### Stacked barplot ------
# keep only forest categories
data = forest_cat_metrics %>%
  dplyr::filter(class %in% c(1,10,11,12,13,14,15)) %>%
  dplyr::select(year, class, pland)

# sum class 15 into class 1
data = data %>%
  dplyr::mutate(class = ifelse(class == 15, 1, class)) %>%
  dplyr::group_by(year, class) %>%
  dplyr::summarise(pland = sum(pland), .groups="drop")

# add readable labels
data = data %>%
  dplyr::mutate(class_label = dplyr::case_when(
    class == 1  ~ "Other forests",
    class == 10 ~ "Other assisted restoration",
    class == 11 ~ "Assisted restoration in reserves",
    class == 12 ~ "Assisted restoration in private properties",
    class == 13 ~ "Forests in reserves",
    class == 14 ~ "Private forests"
  ))

# plot
ggplot(data, aes(x = year, y = pland, fill = class_label)) +
  geom_bar(stat = "identity", position = "fill", color = "black", linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage of forest landscape (%)",
    fill = "Forest category"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

##### Line plot ------
# prepare data: sum 15 into 1
data = forest_cat_metrics %>%
  dplyr::filter(class %in% c(1,10,11,12,13,14,15)) %>%
  dplyr::select(year, class, np) %>%
  dplyr::mutate(class = ifelse(class == 15, 1, class)) %>%
  dplyr::group_by(year, class) %>%
  dplyr::summarise(np = sum(np), .groups="drop") %>%
  dplyr::mutate(class_label = dplyr::case_when(
    class == 1  ~ "Other forests",
    class == 10 ~ "Other assisted restoration",
    class == 11 ~ "Assisted restoration in reserves",
    class == 12 ~ "Assisted restoration in private properties",
    class == 13 ~ "Forests in reserves",
    class == 14 ~ "Private forests"
  ))

# line plot
ggplot(data, aes(x = year, y = np)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "forestgreen", size = 2) +
  facet_wrap(~ class_label, scales = "free_y", ncol = 2) +
  labs(
    x = "Year",
    y = "Number of patches (np)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )


##### Waffle plot -----

# Forest cat colors
greens <- c("#afd69b","#485B4D","#a6dbbc","#8fb6ab")

### Whole landscape
top <- all_lulc_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::mutate(grp = ifelse(class == 1, "Forest", "Matrix")) %>%
  dplyr::group_by(grp) %>%
  dplyr::summarise(ca = sum(ca), .groups="drop") %>%
  dplyr::mutate(p = ca/sum(ca)) %>%
  dplyr::mutate(pct_label = paste0(grp, " (", round(p*100), "%)")) %>%
  dplyr::mutate(n = round(p*100)) %>%
  dplyr::mutate(n = ifelse(n==0,1,n)) %>%
  dplyr::mutate(n = round(n/sum(n)*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = dplyr::row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10,
                block = "a) Whole landscape (forest vs matrix)")

### Forest subcategories
forest_sub <- forest_cat_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::filter(class != 2) %>% 
  dplyr::mutate(class = ifelse(class == 15, 1, class),
                class = dplyr::case_when(class %in% c(10,11,12) ~ 100,
                                         TRUE ~ class)) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(ca = sum(ca), .groups="drop") %>%
  dplyr::mutate(label = dplyr::case_when(
    class == 1   ~ "Other forest (unknown status)",
    class == 100 ~ "Assisted restored forests",
    class == 13  ~ "Forests in reserves",
    class == 14  ~ "Private forests"
  )) %>%
  dplyr::mutate(p = ca / sum(ca)) %>%
  dplyr::mutate(
    pct_label = paste0(label, " (",
                       ifelse(p*100 < 1 & p*100 > 0, "<1", round(p*100)), "%)") # Here we make sure that 0% becomes <1% on the legend
  ) %>%
  dplyr::mutate(n = round(p*100)) %>%
  dplyr::mutate(n = ifelse(n == 0, 1, n)) %>%
  dplyr::mutate(n = round(n/sum(n)*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = dplyr::row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10,
                block = "b) Forest status") %>%
  dplyr::rename(grp = label)

### Matrix subcategories
matrix_sub <- all_lulc_metrics %>%
  dplyr::filter(year == 2024, class %in% c(2,3,4,5)) %>% # aggregate matrix types
  dplyr::group_by(class) %>%
  dplyr::summarise(ca = sum(ca), .groups="drop") %>%
  dplyr::mutate(label = dplyr::case_when(
    class == 2 ~ "Other non-forest formation",
    class == 3 ~ "Agriculture",
    class == 4 ~ "Water",
    class == 5 ~ "Artificial"
  )) %>%
  dplyr::mutate(p = ca/sum(ca)) %>%
  dplyr::mutate(pct_label = paste0(label, " (", round(p*100), "%)")) %>%
  dplyr::mutate(n = round(p*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = dplyr::row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10,
                block = "c) Types of matrix land uses") %>%
  dplyr::rename(grp = label)

### colors
col_vec <- c(
  "Forest" = "#32A65E",
  "Matrix" = "#aa98a9",
  "Other forest (unknown status)" = greens[1],
  "Assisted restored forests"     = greens[2],
  "Forests in reserves"           = greens[3],
  "Private forests"               = greens[4],
  "Other non-forest formation"    = "#ad975a",
  "Agriculture"                   = "#FFFFB2",
  "Water"                         = "#0000FF",
  "Artificial"                    = "#d4271e"
)

### Plots
p_landscape <- ggplot(top, aes(x, y, fill = grp)) +
  geom_tile(color = "white") +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_manual(
    values = col_vec,
    labels = setNames(top$pct_label, top$grp),
    name = NULL
  ) +  # remove legend title
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12)) +
  ggtitle("a) Whole landscape (forest vs matrix)")

p_forest <- ggplot(forest_sub, aes(x, y, fill = grp)) +
  geom_tile(color = "white") +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_manual(
    values = col_vec,
    labels = setNames(forest_sub$pct_label, forest_sub$grp),
    name = NULL
  ) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12)) +
  ggtitle("b) Forest status")

p_matrix <- ggplot(matrix_sub, aes(x, y, fill = grp)) +
  geom_tile(color = "white") +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_manual(
    values = col_vec,
    labels = setNames(matrix_sub$pct_label, matrix_sub$grp),
    name = NULL
  ) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12)) +
  ggtitle("c) Types of matrix land uses")

### Combine layout and add global title
design <- "AB
AC"

waffles <- p_landscape + p_forest + p_matrix +
  plot_layout(design = design,
              widths = c(0.58, 0.42),
              heights = c(1, 1))

png(here("outputs","plot","01h_lm_pland_2024_waffle.png"), width = 2700, height = 1800, res = 300)

plot(waffles)

dev.off()


### Forest class metrics -----------
#### Plots -----
##### Line plot - all -----
data = forest_class_metrics %>%
  dplyr::select(
    year,
    ca,
    np,
    area_mn,
    FFI
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
      TRUE ~ Metric
    )
  )

# Plot facets
png(here("outputs","plot","01h_lm_forest_metrics_lineplot.png"), width = 2000, height = 2000, res = 300)

ggplot(data, aes(x = year, y = Value)) +
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

dev.off()

##### Line plot - ECA -----
# Prepare ECA data
eca_data <- forest_class_metrics %>%
  dplyr::select(
    year, eca_ha_2000, eca_ha_8000, change_2000
  ) %>%
  tidyr::pivot_longer(
    cols = c(eca_ha_2000, eca_ha_8000),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Metric = dplyr::case_when(
      Metric == "eca_ha_2000" ~ "ECA (2 km)",
      Metric == "eca_ha_8000" ~ "ECA (8 km)"
    ),
    year = as.numeric(year)
  )

# Simple plot
png(here("outputs","plot","01h_lm_eca_lineplot.png"), width = 2400, height = 2000, res = 300)

ggplot(eca_data, aes(x = year, y = Value, color = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Temporal Evolution of Equivalent Connected Area (ECA)",
    x = "Year",
    y = "ECA (ha)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(values = c(
    "ECA (2 km)" = "#BFB8FF",
    "ECA (8 km)" = "#0C0075"
  ))

dev.off()

# Identify runs of consecutive same ECA change
change_df <- forest_class_metrics %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(change_2000 = as.character(change_2000)) %>%
  # Merge losses before grouping
  dplyr::mutate(
    change_2000 = dplyr::case_when(
      change_2000 %in% c("+ Habitat loss", "+ Connectivity loss") ~ "Habitat or connectivity loss",
      TRUE ~ change_2000
    )
  ) %>%
  dplyr::mutate(run_id = cumsum(c(1, diff(as.numeric(as.factor(change_2000))) != 0)))

# Summarize runs and keep only relevant ones
brackets_df <- change_df %>%
  dplyr::group_by(run_id, change_2000) %>%
  dplyr::summarise(
    xmin = min(year),
    xmax = max(year),
    n_years = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_years > 1 & !is.na(change_2000)) %>%
  dplyr::arrange(xmin)

# Merge consecutive brackets with same label
brackets_df <- brackets_df %>%
  dplyr::mutate(group_merge = cumsum(dplyr::lag(change_2000, default = "") != change_2000)) %>%
  dplyr::group_by(change_2000, group_merge) %>%
  dplyr::summarise(
    xmin = min(xmin),
    xmax = max(xmax),
    .groups = "drop"
  ) %>%
  dplyr::arrange(xmin)

# Define vertical positions
brackets_df <- brackets_df %>%
  dplyr::mutate(
    global_max = max(eca_data$Value, na.rm = TRUE),
    stack_i = dplyr::row_number(),
    y_base = global_max * (1 + 0.015 * stack_i),
    height = global_max * 0.01
  )

# Colors for rectangles
rect_colors <- c(
  "Habitat or connectivity gain" = "#66BB6A",
  "Habitat or connectivity loss" = "#E57373"
)

# Plot ECA metrics
p_eca <- ggplot(eca_data, aes(x = year, y = Value, color = Metric)) +
  geom_rect(
    data = brackets_df,
    aes(
      xmin = xmin - 0.5,    # -0.5 offset
      xmax = xmax + 0.5,    # +0.5 offset
      ymin = -Inf,
      ymax = Inf,
      fill = change_2000
    ),
    inherit.aes = FALSE,
    alpha = 0.25, # how transparent the rectangles are
    linewidth = 0
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("ECA (2 km)" = "#BFB8FF", "ECA (8 km)" = "#0C0075")) +
  scale_fill_manual(values = rect_colors) +
  labs(
    title = "Changes in Equivalent Connected Area (ECA)",
    x = "Year",
    y = "ECA (ha)",
    color = NULL,
    fill = "Change type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Brackets directly over rectangles
p_final <- p_eca +
  coord_cartesian(clip = "off") +
  geom_segment(
    data = brackets_df,
    aes(x = xmin, xend = xmax, y = y_base, yend = y_base),
    inherit.aes = FALSE,
    linewidth = 0.8,
    colour = "black"
  ) +
  geom_segment(
    data = brackets_df,
    aes(x = xmin, xend = xmin, y = y_base, yend = y_base - height),
    inherit.aes = FALSE,
    linewidth = 0.8,
    colour = "black"
  ) +
  geom_segment(
    data = brackets_df,
    aes(x = xmax, xend = xmax, y = y_base, yend = y_base - height),
    inherit.aes = FALSE,
    linewidth = 0.8,
    colour = "black"
  ) +
  geom_text(
    data = brackets_df,
    aes(x = (xmin + xmax) / 2, y = y_base + (global_max * 0.005), label = change_2000),
    inherit.aes = FALSE,
    size = 4,
    fontface = "italic"
  )

p_final


###### Text --------------
forest_series <- forest_class_metrics %>%
  dplyr::arrange(year)

# Select start and end years
start_year <- 1989
end_year   <- 2024

# Metrics to summarize
metrics <- c("ca", "area_mn", "area_sd", "np", "FFI", "eca_ha_2000", "eca_ha_8000")

# Values in 2024
values_2024 <- forest_series %>%
  dplyr::filter(year == end_year) %>%
  dplyr::select(all_of(metrics)) %>%
  as.list()

# Overall change 1989 → 2024
values_1989 <- forest_series %>%
  dplyr::filter(year == start_year) %>%
  dplyr::select(all_of(metrics)) %>%
  as.list()

overall_change <- mapply(function(start, end) end - start, values_1989, values_2024)

# Text
cat("Forest metrics in 2024:\n")
for(m in metrics){
  cat(paste0("* ", m, ": ", round(values_2024[[m]], 2), "\n"))
}

cat("\nOverall change from 1989 to 2024:\n")
for(m in metrics){
  change <- overall_change[[m]]
  direction <- ifelse(change >= 0, "increased", "decreased")
  cat(paste0("* ", m, " ", direction, " by ", round(abs(change), 2), "\n"))
}


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
  print(n=72)

# Changes betwenn first and last years
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
    np_2012_2024 = np[year == 2024] - np[year == 2012]) %>%
  dplyr::ungroup()

#### Plots ----
##### Line plot -----
# Prepare long data (with explicit dplyr:: prefixes)
data = forest_core_corridor_metrics %>%
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
data_core = data %>% dplyr::filter(type == "Core")
data_corr = data %>% dplyr::filter(type == "Corridor")

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
