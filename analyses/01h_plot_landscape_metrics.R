#------------------------------------------------#
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
library(extrafont)
font_import()
loadfonts()
fonttable()

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

### All land uses metrics -----
# Create color palette using the Legend codes provided by MapBiomas
class_colors = tibble::tibble(
  class = c(1,2,3,4,5,6),
  Description = c("Forest","Other non-forest formation", "Wetlands and mangroves", "Agriculture","Water","Non vegetated areas"
  ),
  Color = c(
    "#32a65e", "#ad975a", "#519799", "#D1D100", "#0000FF", "#d4271e"
  )
)


##### Donut plot --------
# prepare data for donut
data = all_lulc_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::left_join(class_colors, by="class") %>%
  dplyr::select(Description, ca, pland) %>%
  dplyr::arrange(desc(pland))

donut = data %>%
  dplyr::mutate(
    fraction = pland / sum(pland),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    labelPos = (ymax + ymin) / 2,
    label = paste0(round(pland,1),"%")
  )

# donut plot
png(here("outputs","plot","01h_lm_pland_2024_donut.png"), width = 1200, height = 800, res = 300)

ggplot(donut, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Description)) +
  geom_rect() +
  geom_text_repel(
    aes(x = 4.2, y = labelPos, label = label),
    size = 2.5,
    nudge_x = 0.6,
    segment.color = "grey50",
    direction = "y",
    family = "Arial Narrow"
  ) +
  coord_polar(theta = "y") +
  xlim(c(0, 4.8)) +
  theme_void() +
  scale_fill_manual(name = "Land use class", values = setNames(class_colors$Color, class_colors$Description)) +
  theme(legend.position = "right",
        legend.title = element_text(size = 9, family = "Arial Narrow"),
        legend.text  = element_text(size = 7, family = "Arial Narrow"),
        legend.key.width  = unit(0.4, "cm"),
        legend.key.height = unit(0.3, "cm")
        )

dev.off()

###### Text -------
res_2024 = data %>%
  dplyr::mutate(txt = paste0(Description, " covered ", round(ca), " ha (", round(pland,1), "%) in 2024.")) %>%
  dplyr::pull(txt)
cat("Land-use composition in 2024 was as follows:\n")
cat(paste0("* ", res_2024, collapse="\n"), "\n\n")


##### Stacked area chart ------
# Select data
data = all_lulc_metrics %>% 
  dplyr::select(year, class, pland) %>% 
  dplyr::mutate(
    year  = as.integer(year),   # x axis must be numeric/continuous
    class = as.numeric(class),  # fill should be discrete
  )

# Add color codes
data = data %>% 
  dplyr::left_join(class_colors, by = c("class"))


##### Stacked barplot (in %) ------
png(here("outputs","plot","01h_lm_pland_barplot.png"), width = 1500, height = 1000, res = 300)

ggplot(data, aes(x = year, y = pland, fill = Description)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.2) +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  labs(
    x = "Year",
    y = "Percentage of landscape (%)",
    fill = "Land use class"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 10, family = "Arial Narrow"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Arial"),
    legend.position = "right",
    legend.title = element_text(size = 10, family = "Arial Narrow"),
    legend.text  = element_text(size = 8, family = "Arial Narrow"),
    legend.key.width  = unit(0.4, "cm"),
    legend.key.height = unit(0.3, "cm")
  )

dev.off()

###### Text --------
# Evolution from 1989 and 2024
data = all_lulc_metrics %>%
  dplyr::left_join(class_colors, by="class") %>%
  dplyr::group_by(Description) %>%
  dplyr::summarise(
    pland1989 = pland[year==1989],
    pland2024 = pland[year==2024],
    d_pland = pland2024 - pland1989
  ) %>%
  dplyr::ungroup()

# Text
res_change = data %>%
  dplyr::mutate(
    txt = paste0(
      Description, " cover represented ", round(pland1989,1), "% of the landscape in 1989 ",
      "while it represents ", round(pland2024,1), "% of the landscape in 2024 (",
      ifelse(d_pland>=0, "+",""), round(d_pland,1), " %)."
    )
  ) %>%
  dplyr::pull(txt)

cat("Changes between 1989 and 2024 were:\n")
cat(paste0("* ", res_change, collapse="\n"), "\n")

##### Line plot -----
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
    Description == "Non vegetated areas" ~ ca_end + 6000,
    Description == "Water" ~ ca_end + 4000,
    Description == "Wetlands and mangroves" ~ ca_end + 1000,
    Description == "Other non-forest formation" ~ ca_end - 4000,
    TRUE ~ ca_end
  ))

# Plot
png(here("outputs","plot","01h_lm_ca_all_lulcc_line_plot.png"), width = 2000, height = 1500, res = 300)

x_max = max(data$year)
ggplot(data, aes(x = year, y = ca, color = Description, group = Description)) +
  geom_ribbon(aes(ymin = ca - 2000, ymax = ca + 2000, fill = Description), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_text(data = label_data,
            aes(x = year_end + 0.6, y = ca_end_adj, label = label, color = Description),
            hjust = 0, size = 3.5, fontface = "bold", show.legend = FALSE, family = "Arial Narrow") +
  scale_color_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description), guide = "none") +
  labs(x = "Year", y = "Area (ha)", color = "Land use class") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +   # small left margin + larger right margin
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 10, family = "Arial Narrow"),
        axis.text = element_text(size = 8, family = "Arial"),
        legend.title = element_text(size = 10, family = "Arial Narrow"),
        legend.text = element_text(size = 8, family = "Arial Narrow"),
        legend.key.spacing = unit(2, "pt"),
        legend.key.width = unit(0.3, "cm"),
        plot.margin = margin(10, 80, 10, 10))

dev.off()

###### Text -------
res_trend = label_data %>%
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
  dplyr::left_join(class_colors, by = "class")

# 1) Plot: dodged bars (delta ha) + connecting lines & points per class (years numeric)
ggplot(data, aes(x = year, y = delta_ca, group = Description)) +
  geom_col(data = data,aes(x = year, y = delta_ca, fill = Description),
           position = position_dodge(width = 0.7),
           color = "black",width = 0.6) +
  geom_line(data = data, aes(x = year, y = delta_ca, color = Description), linewidth = 0.5) +
  geom_point(data = data, aes(x = year, y = delta_ca, color = Description), size = 1.8) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  scale_x_continuous( breaks = seq(min(data$year), max(data$year), by = 2)) +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description), name = "Land use class") +
  scale_color_manual(values = setNames(class_colors$Color, class_colors$Description), name = "Land use class") +
  labs(
    x = "Year",
    y = "Change in surface (ha)",
    fill = "Land use",
    color = "Land use",
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


# 2) With a smoothed line
png(here("outputs","plot","01h_lm_pland_barplot0.png"), width = 2500, height = 1500, res = 300)

ggplot(data, aes(x = year, y = delta_ca, group = Description)) +
  geom_col(data = data,aes(x = year, y = delta_ca, fill = Description),
           position = position_dodge(width = 0.5),
           color = "white", linewidth = 0.2, width = 1) +
  geom_smooth(data = data, aes(x = year, y = delta_ca, color = Description), 
              method = "loess",
              se = FALSE,
              linewidth = 1,
              linetype = "longdash",
              span = 0.300) + # change the span to change the smoothening of the line)
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  scale_x_continuous( breaks = seq(min(data$year), max(data$year), by = 2)) +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description), name = "Land use class") +
  scale_color_manual(values = setNames(class_colors$Color, class_colors$Description), name = "Land use class") +
  labs(
    x = "Year",
    y = "Change in surface (ha)",
    fill = "Land use class",
    color = "Land use class",
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        axis.title = element_text(size = 10, family = "Arial Narrow"),
        axis.text = element_text(size = 8, family = "Arial"),
        legend.title = element_text(size = 10, family = "Arial Narrow"),
        legend.text  = element_text(size = 8, family = "Arial Narrow"),
        legend.key.width  = unit(0.4, "cm"),
        legend.key.height = unit(0.3, "cm"))

dev.off()

# 3) ... and the forest dynamics
# Identify forest increase/decrease runs
forest_dyn = data %>% dplyr::filter(Description == "Forest") %>% 
  dplyr::mutate(trend = ifelse(delta_ca > 0, "inc", "dec")) %>% 
  dplyr::mutate(grpid = data.table::rleid(trend)) %>% 
  dplyr::group_by(grpid) %>% 
  dplyr::summarise(start = min(year), end = max(year), trend = dplyr::first(trend), n = dplyr::n(), .groups = "drop") %>% 
  dplyr::filter(n > 1) %>% 
  dplyr::mutate(label = dplyr::if_else(trend == "inc", "forest increasing", "forest decreasing"),
                start = start - 0.5, end = end + 0.5)

# Define bracket positions (larger gaps, scaling order)
forest_dyn = forest_dyn %>% 
  dplyr::mutate(min_y = min(data$delta_ca, na.rm = TRUE),
                y_base = min_y * 0.8 - (dplyr::row_number() - 1) * abs(min_y) * 0.12,
                height = abs(min_y) * 0.04)


# Final plot
ggplot(data, aes(x = year, y = delta_ca, fill = Description)) +
  geom_rect(data = forest_dyn, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
            inherit.aes = FALSE, alpha = 0.15, show.legend = FALSE) +
  geom_col(position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_smooth(aes(color = Description, group = Description), se = FALSE, span = 0.3, linewidth = 1.2, show.legend = FALSE) +
  geom_segment(data = forest_dyn, aes(x = start, xend = end, y = y_base, yend = y_base),
               inherit.aes = FALSE, linewidth = 0.9, colour = "black") +
  geom_segment(data = forest_dyn, aes(x = start, xend = start, y = y_base, yend = y_base + height),
               inherit.aes = FALSE, linewidth = 0.9, colour = "black") +
  geom_segment(data = forest_dyn, aes(x = end, xend = end, y = y_base, yend = y_base + height),
               inherit.aes = FALSE, linewidth = 0.9, colour = "black") +
  geom_text(data = forest_dyn, aes(x = (start + end) / 2, y = y_base - height * 1.2, label = label),
            inherit.aes = FALSE, size = 4, fontface = "italic") +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 5),
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_fill_manual(values = c("Forest" = "#32a65e",
                               "Agriculture" = "#D1D100",
                               "Non vegetated areas" = "#d4271e",
                               "forest increasing" = "#B8E0FF",
                               "forest decreasing" = "#FFA8A8"),
    breaks = c("Forest", "Agriculture", "Non vegetated areas"),
    labels = c("Forest", "Agriculture", "Non vegetated areas"),
    name = "Land use class") +
  scale_color_manual(values = c("Forest" = "#32a65e",
                                "Agriculture" = "#D1D100",
                                "Non vegetated areas" = "#d4271e"),
                     guide = "none") +
  labs(x = "Year", y = "Change in surface (ha)") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right", legend.text = element_text(size = 10))


###### Text -------
# filter only Forest time series
data = all_lulc_metrics %>%
  dplyr::filter(class == 1) %>%
  dplyr::arrange(year)

# create the period table again
forest_dyn = data %>%
  dplyr::mutate(delta = ca - dplyr::lag(ca),
                trend = ifelse(delta > 0,"inc","dec")) %>%
  dplyr::filter(!is.na(trend))
forest_dyn = forest_dyn %>% 
  dplyr::mutate(group = data.table::rleid(trend))
forest_dyn = forest_dyn %>% 
  dplyr::group_by(group, trend) %>%
  dplyr::summarise(start_year = min(year),
                   end_year = max(year),
                   ca_start= ca[year == start_year],
                   ca_end = ca[year == end_year],
                   .groups="drop")
forest_dyn = forest_dyn %>% 
  # remove single-year groups
  dplyr::filter(end_year > start_year) %>%
  dplyr::mutate(delta_total = ca_end - ca_start)

forest_text = forest_dyn %>%
  dplyr::mutate(
    txt = dplyr::case_when(
      trend == "inc" ~ paste0("From ", start_year, " to ", end_year, 
                              " the forest area increased by ",
                              round(delta_total,1), " ha."),
      TRUE ~ paste0("From ", start_year, " to ", end_year, 
                    " the forest area decreased by ",
                    abs(round(delta_total,1)), " ha.")
    )
  ) %>%
  dplyr::pull(txt)

# print the text
cat("Forest temporal dynamics were structured in the following distinct periods:\n")
cat(paste0("* ", forest_text, collapse="\n"), "\n\n")

##### Waffle plot -------
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

# Waffle plot 
png(here("outputs","plot","01h_lm_pland_waffle2024.png"), width = 1200, height = 1200, res = 300)
ggplot(data_2024, aes(x, y, fill = Description)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_manual(
    values = setNames(class_colors$Color, class_colors$Description),
    labels = setNames(data_2024$pct_label, data_2024$Description),
    name = NULL
  ) +
  theme_void() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, family = "Arial Narrow"),
        legend.title = element_text(size = 10, family = "Arial Narrow"),
        legend.key.width  = unit(0.4, "cm"),
        legend.key.height = unit(0.3, "cm")) +
  guides(fill=guide_legend(title="Land use class (2024)"))
dev.off()


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

# Waffle plot 
png(here("outputs","plot","01h_lm_pland_waffle1989.png"), width = 1200, height = 1200, res = 300)
ggplot(data_1989, aes(x, y, fill = Description)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_manual(
    values = setNames(class_colors$Color, class_colors$Description),
    labels = setNames(data_1989$pct_label, data_1989$Description),
    name = NULL
  ) +
  theme_void() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, family = "Arial Narrow"),
        legend.title = element_text(size = 10, family = "Arial Narrow"),
        legend.key.width  = unit(0.4, "cm"),
        legend.key.height = unit(0.3, "cm")) +
  guides(fill=guide_legend(title="Land use class (1989)"))
dev.off()

##### Ternary plot --------
# # 1. Filter for the classes we need
# data = all_lulc_metrics %>%
#   dplyr::filter(class %in% c(1, 4, 6)) %>%
#   dplyr::select(year, class, pland)
# 
# # 2. Pivot data so each row is a year and columns are forest, agriculture, urban
# data = data %>%
#   dplyr::mutate(class_name = dplyr::case_when(
#     class == 1 ~ "Forest",
#     class == 4 ~ "Agriculture",
#     class == 6 ~ "Non_vegetated_areas"
#   )) %>%
#   dplyr::select(-class) %>%
#   tidyr::pivot_wider(names_from = class_name, values_from = pland)
# data$Forest = data$Forest/100
# data$Agriculture = data$Agriculture/100
# data$Non_vegetated_areas = data$Non_vegetated_areas/100
# summary(data[, c("Forest", "Agriculture", "Non_vegetated_areas")])
# 
# # 3. Plot ternary diagram
# ggtern(data = data, aes(x = Agriculture, y = Non_vegetated_areas, z = Forest)) +
#   geom_point(aes(color = year), size = 3) +
#   scale_T_continuous(limits = c(0.1, 0.2)) + # artificial
#   scale_L_continuous(limits = c(0.3, 0.4)) + # agriculture
#   scale_R_continuous(limits = c(0.5, 0.6)) + # forest
#   theme_rgbw()


### Forest categories metrics --------
# Create color palette using the Legend codes provided by MapBiomas
class_colors = tibble::tibble(
  class = c(11,12,13,14,10),
  Description = c("Private forests",
                  "Forests in private reserves (RPPN)", 
                  "Forests in public reserves", 
                  "Private forests in public reserves",
                  "Unknown status"
  ),
  Color = c(
    "#E79090", "#F2DE46", "#A0CB60", "#7AB558", "#F1F0E9"
  )
)

##### Donut plot ------
# prepare data for donut
data = forest_cat_metrics %>%
  dplyr::left_join(class_colors, by="class") %>%
  dplyr::select(Description, ca, pland) %>%
  dplyr::arrange(desc(pland))

donut = data %>%
  dplyr::mutate(
    fraction = pland / sum(pland),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    labelPos = (ymax + ymin) / 2,
    label = paste0(round(pland,1),"%")
  )

# donut plot
png(here("outputs","plot","01h_lm_forest_cat_2024_donut.png"), width = 1200, height = 800, res = 300)

ggplot(donut, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Description)) +
  geom_rect() +
  geom_text_repel(
    aes(x = 4.2, y = labelPos, label = label),
    size = 2.5,
    nudge_x = 0.6,
    segment.color = "grey50",
    direction = "y",
    family = "Arial Narrow"
  ) +
  coord_polar(theta = "y") +
  xlim(c(0, 4.8)) +
  theme_void() +
  scale_fill_manual(name = "Forest status", values = setNames(class_colors$Color, class_colors$Description)) +
  theme(legend.position = "right",
        legend.title = element_text(size = 9, family = "Arial Narrow"),
        legend.text  = element_text(size = 7, family = "Arial Narrow"),
        legend.key.width  = unit(0.4, "cm"),
        legend.key.height = unit(0.3, "cm")
  )

dev.off()

###### Text -------
res_2024 = data %>%
  dplyr::mutate(txt = paste0(Description, " covered ", round(ca), " ha (", round(pland,1), "%) in 2024.")) %>%
  dplyr::pull(txt)
cat("Forest status in 2024 was as follows:\n")
cat(paste0("* ", res_2024, collapse="\n"), "\n\n")


### Forest class metrics -----------

##### Line plot - all -----
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
    title = "Temporal Evolution of Forest Landscape Metrics",
    x = "Year",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12, family = "Arial Narrow"),
    plot.title = element_text(face = "bold", hjust = 0.5, family = "Arial Narrow"),
    axis.title = element_text(size = 12, family = "Arial Narrow"),
    axis.text = element_text(size = 10, family = "Arial"),
    panel.grid.minor = element_blank()
  )

dev.off()

##### Line plot - ECA -----
# Prepare ECA data
eca_data = forest_class_metrics %>%
  dplyr::select(
    year, eca_ha_2000, eca_ha_8000, change_2000) %>%
  tidyr::pivot_longer(
    cols = c(eca_ha_2000, eca_ha_8000),
    names_to = "Metric",
    values_to = "Value") %>%
  dplyr::mutate(
    Metric = dplyr::case_when(
      Metric == "eca_ha_2000" ~ "ECA (2 km)",
      Metric == "eca_ha_8000" ~ "ECA (8 km)"),
    year = as.numeric(year))

# Plot
png(here("outputs","plot","01h_lm_eca_lineplot.png"), width = 2400, height = 2000, res = 300)

ggplot(eca_data, aes(x = year, y = Value, color = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4) +
  labs(
    title = "Temporal Evolution of Equivalent Connected Area (ECA)",
    x = "Year",
    y = "ECA (ha)",
    color = NULL) +
  scale_color_manual(values = c(
    "ECA (2 km)" = "#FDB777",
    "ECA (8 km)" = "#FF6200")) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, family = "Arial Narrow"),
    axis.title = element_text(size = 12, family = "Arial Narrow"),
    axis.text = element_text(size = 10, family = "Arial"),
    legend.text = element_text(size = 10, family = "Arial Narrow"))

dev.off()


###### Text --------------
forest_series = forest_class_metrics %>%
  dplyr::arrange(year)

# Metrics to summarize
metrics <- c("ca", "area_mn", "area_sd", "np", "FFI", "eca_ha_2000", "eca_ha_8000")

# Values in last year
values_2024 = forest_series %>%
  dplyr::filter(year == dplyr::last(year)) %>%
  dplyr::select(all_of(metrics)) %>%
  as.list()

# Overall change
values_1989 = forest_series %>%
  dplyr::filter(year == dplyr::first(year)) %>%
  dplyr::select(all_of(metrics)) %>%
  as.list()

overall_change = mapply(function(start, end) end - start, values_1989, values_2024)

# Text
cat("Forest metrics in 2024:\n")
for(m in metrics){
  cat(paste0("* ", m, ": ", round(values_2024[[m]], 2), "\n"))
}

cat("\nOverall change from 1989 to 2024:\n")
for(m in metrics){
  change = overall_change[[m]]
  direction = ifelse(change >= 0, "increased", "decreased")
  cat(paste0("* ", m, " ", direction, " by ", round(abs(change), 2), "\n"))
}


### Conservation activities -----------

##### Line plot -----
data = cons_cat_metrics %>%
  dplyr::select(class, year, ca, np) %>%
  tidyr::pivot_longer(cols = c(ca, np), names_to = "Metric", values_to = "Value") %>%
  dplyr::mutate(Metric = dplyr::case_when(
    Metric == "ca" ~ "Surface area (ha)",
    Metric == "np" ~ "Number of patches",
    TRUE ~ Metric
  )) %>%
  dplyr::mutate(class = dplyr::case_when(
    class == 50 ~ "Active forest restoration",
    class == 51 ~ "Forest corridor creation",
    class == 52 ~ "Road overpasses",
    TRUE ~ as.character(class)
  ))

# split
data_road = data %>% dplyr::filter(class == "Road overpasses")
data_corr = data %>% dplyr::filter(class == "Forest corridor creation")
data_rest = data %>% dplyr::filter(class == "Active forest restoration")


# First plot
road = ggplot(data_road, aes(x = year, y = Value)) +
  geom_line(color = "#F694C1", linewidth = 1) +
  geom_point(color = "#F694C1", size = 2.5) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  labs(title = "Road overpasses", x = "Year", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", family = "Arial Narrow"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, family = "Arial Narrow"),
    axis.title = element_text(size = 12, family = "Arial Narrow"),
    axis.text = element_text(size = 10, family = "Arial"))

# Second plot
corr = ggplot(data_corr, aes(x = year, y = Value)) +
  geom_line(color = "#E4C1F9", linewidth = 1) +
  geom_point(color = "#E4C1F9", size = 2.5) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  labs(title = "Forest corridor creation", x = "Year", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", family = "Arial Narrow"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, family = "Arial Narrow"),
    axis.title = element_text(size = 12, family = "Arial Narrow"),
    axis.text = element_text(size = 10, family = "Arial"))

# Third plot
rest = ggplot(data_rest, aes(x = year, y = Value)) +
  geom_line(color = "#D3F8E2", linewidth = 1) +
  geom_point(color = "#D3F8E2", size = 2.5) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  labs(title = "Active forest restoration", x = "Year", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", family = "Arial Narrow"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, family = "Arial Narrow"),
    axis.title = element_text(size = 12, family = "Arial Narrow"),
    axis.text = element_text(size = 10, family = "Arial"))

# Combine side-by-side: Core left, Corridor right
combined = road + corr + rest + plot_layout(ncol = 3, widths = c(1, 1))

# Print
combined

###### Text ---------
res_table = cons_cat_metrics %>%
  dplyr::select(class, year, ca, np) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    ca_last = ca[year == last_year],
    np_last = np[year == last_year],
    .groups = "drop") %>% 
  dplyr::filter(!is.na(class)) %>% 
  dplyr::mutate(class = dplyr::case_when(
    class == 50 ~ "Active forest restoration",
    class == 51 ~ "Forest corridor creation",
    class == 52 ~ "Road overpasses",
    TRUE ~ as.character(class)))

res_text = res_table %>%
  dplyr::mutate(
    txt = paste0(
      class, " started in ", first_year, 
      ", with ", round(np_last, 0), " patches and ",
      round(ca_last, 1), " ha in the last monitored year (", last_year, ")."
    )
  ) %>%
  dplyr::pull(txt)

cat("Conservation actions summary:\n")
cat(paste0("* ", res_text, collapse = "\n"), "\n")


### Forest age -----------

##### Line plot -----
data = forest_age_metrics %>%
  mutate(
    AgeClass = case_when(
      class == 5 ~ "1-5 years",
      class == 15 ~ "6-15 years",
      class == 25 ~ "16-25 years",
      class == 99 ~ ">25 years",
      TRUE ~ as.character(class)
    )
  )

total_forest = all_lulc_metrics %>%
  dplyr::filter(class == 1) %>% 
  dplyr::select(year, ca)

age_cols = c(
  "1-5 years"   = "#9ad9f5",
  "6-15 years" = "#49b5e7",
  "16-25 years" = "#138fcf",
  ">25 years"   = "#0b5fa5"
)  

# Bar + line plot
ggplot() +
  ## Stacked bars = forest age structure
  geom_col(data = data,
           aes(x = year, y = ca, fill = AgeClass),
           width = 0.9,
           color = "white",
           linewidth = 0.2) +
  ## Total forest cover line
  geom_line(data = total_forest,
            aes(x = year, y = ca),
            color = "black",
            linewidth = 1) +
  geom_point(data = total_forest,
             aes(x = year, y = ca),
             color = "black",
             size = 2) +
  scale_fill_manual(values = age_cols, name = "Native forest age") +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 2)) +
  labs(
    x = "Year",
    y = "Forest area (ha)",
    title = "Temporal evolution of native forest cover and age structure"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, family = "Arial Narrow"),
    axis.title = element_text(size = 12, family = "Arial Narrow"),
    axis.text  = element_text(size = 10, family = "Arial"),
    legend.title = element_text(size = 11, family = "Arial Narrow"),
    legend.text  = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )
