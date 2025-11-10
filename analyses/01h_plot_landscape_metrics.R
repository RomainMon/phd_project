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
library(ggforce)
library(ggrepel)
library(tibble)
library(scales)
# remotes::install_github("davidsjoberg/ggstream")
library(ggstream)

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
  file.path(base_path, "forest_cat_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE
)

### Class metrics (overall) ----------------

#### Summary statistics ------
# Evolution per class per year
all_lulc_metrics %>%
  dplyr::arrange(class, year) %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(pland_change_year = round((pland - dplyr::lag(pland)), 1),
                ca_change_year = ca - dplyr::lag(ca),
                ca_change_year_pct = (ca - dplyr::lag(ca)) / dplyr::lag(ca) * 100) %>%
  dplyr::ungroup()

# Changes betwenn first and last year
all_lulc_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(pland_change_total = round(pland[year == max(year)] - pland[year == min(year)], 1),
                   ca_change_total = ca[year == max(year)] - ca[year == min(year)])

# Between sets of years
all_lulc_metrics %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(
    pland_change_1989_2000 = round(pland[year == 2000] - pland[year == 1989], 1),
    pland_change_2000_2012 = round(pland[year == 2012] - pland[year == 2000], 1),
    pland_change_2012_2024 = round(pland[year == 2024] - pland[year == 2014], 1),
    ca_change_1989_2000 = ca[year == 2000] - ca[year == 1989],
    ca_change_2000_2012 = ca[year == 2012] - ca[year == 2000],
    ca_change_2012_2024 = ca[year == 2024] - ca[year == 2014]) %>%
  dplyr::ungroup()


#### Plots -------
##### Stacked area chart ------
# Create color palette using the Legend codes provided by MapBiomas
class_colors = tibble::tibble(
  class = c(1,2,3,4,5),
  Description = c("Forest","Other non-forest formation","Agriculture","Water","Artificial"
  ),
  Color = c(
    "#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"
  )
)

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


##### Stream chart -----
data = all_lulc_metrics %>% 
  dplyr::select(year, class, ca) %>% 
  dplyr::left_join(class_colors, by="class")

data %>%
  dplyr::mutate(
    year = as.numeric(year)
  ) %>%
  ggplot(aes(
    x = year,
    y = ca,
    fill = Description,
    color = Description,
    label = Description
  )) +
  geom_stream(
    extra_span = 0.013,
    type = "proportion",
    n_grid = 3000,
    bw = 0.78
  ) +
  scale_fill_manual(values = setNames(data$Color, data$Description)) +
  scale_color_manual(values = setNames(data$Color, data$Description)) +
  cowplot::theme_minimal_vgrid(font_size = 18) +
  theme(legend.position = "none") +
  labs(
    title = "Share of land cover (proportion of total surface)",
    x = NULL,
    y = NULL
  )


###### Text --------
# Evolution from 1989 and 2024
data <- all_lulc_metrics %>%
  dplyr::left_join(class_colors, by="class") %>%
  dplyr::group_by(Description) %>%
  dplyr::summarise(
    ca1989  = ca[year==1989],
    ca2024  = ca[year==2024],
    pl1989  = pland[year==1989],
    pl2024  = pland[year==2024],
    d_ca    = ca2024 - ca1989,
    d_pl    = pl2024 - pl1989,
    d_pl_pct = (pl2024 - pl1989) # already percentage unit
  ) %>%
  dplyr::ungroup()

# Text
res_change <- data %>%
  dplyr::mutate(
    txt = paste0(
      Description, " changed by ",
      ifelse(d_ca>=0, "+",""), round(d_ca), " ha (",
      ifelse(d_pl>=0, "+",""), round(d_pl,1), " percentage points) between 1989 and 2024."
    )
  ) %>%
  dplyr::pull(txt)

cat("Changes between 1989 and 2024 were:\n")
cat(paste0("* ", res_change, collapse="\n"), "\n")

##### Barplot (positive vs negative changes) ------
# 1) With a line
data = all_lulc_metrics %>%
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

# Plot: dodged bars (Δha) + connecting lines & points per class (years numeric)
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
    size = 0.9
  ) +
  
  # points at each year on the lines
  geom_point(
    data = data,
    aes(x = year, y = delta_ca, color = class_name),
    size = 1.8
  ) +
  
  scale_fill_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FFFFB2",
    "Artificial" = "#d4271e"
  )) +
  scale_color_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FFFFB2",
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


# 2) With a smoothed line
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
    linewidth = 1.2,
    span = 0.300 # change the span to change the smoothening of the line
  ) +
  
  scale_fill_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FFFFB2",
    "Artificial" = "#d4271e"
  )) +
  scale_color_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FFFFB2",
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

# 3) ... and the forest dynamics
forest_dyn = data %>%
  dplyr::filter(class_name == "Forest") %>%
  dplyr::mutate(trend = ifelse(delta_ca > 0, "inc", "dec")) %>%
  dplyr::group_by(grpid = data.table::rleid(trend)) %>%
  dplyr::summarise(
    start = min(as.integer(year)),
    end   = max(as.integer(year)),
    trend = first(trend),
    n     = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1) %>%        # REMOVE single-year segments
  dplyr::mutate(
    y = -7000,             # position on the Y axis
    label = ifelse(trend == "inc", "forest increasing", "forest decreasing")
  )

ggplot(data, aes(x = year, y = delta_ca, fill = class_name)) +
  geom_col(position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  scale_fill_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FFFFB2",
    "Artificial" = "#d4271e"
  )) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  # smoothed lines for each class
  geom_smooth(aes(color = class_name), se = FALSE, span = 0.3, linewidth = 1.2) +
  scale_color_manual(values = c(
    "Forest" = "#32a65e",
    "Agriculture" = "#FFFFB2",
    "Artificial" = "#d4271e"
  )) +
  # 3) brackets (segments)
  # downward green squared brackets (forest)
  geom_segment(data = forest_dyn,
               aes(x = start, xend = end, y = y, yend = y),
               inherit.aes = FALSE,
               color = "#32a65e", linewidth = 2) +       # top bar (at y = -7000)
  geom_text(data = forest_dyn,
            aes(x = (start + end)/2, y = y - 300,
                label = ifelse(trend == "inc", "↑", "↓")),
            inherit.aes = FALSE,
            color = "#32a65e",
            size = 5) +
  labs(
    x = "Year",
    y = "Change in surface (ca)",
    fill = "Land use",
    color = "Land use",
    title = "Year-to-year evolution in surface of main land uses"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

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
                   end_year   = max(year),
                   ca_start   = ca[year == start_year],
                   ca_end     = ca[year == end_year],
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
ggplot(donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Description)) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPos, label=label), size=3) +
  coord_polar(theta="y") +
  xlim(c(0,4)) +
  theme_void() +
  scale_fill_manual(values = setNames(class_colors$Color, class_colors$Description)) +
  ggtitle("Land use composition in 2024") +
  theme(legend.position="right")

###### Text -------
res_2024 <- data %>%
  dplyr::mutate(txt = paste0(Description, " covered ", round(ca), " ha (", round(pland,1), "%) in 2024.")) %>%
  dplyr::pull(txt)
cat("Land-use composition in 2024 was as follows:\n")
cat(paste0("* ", res_2024, collapse="\n"), "\n\n")


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
  dplyr::mutate(class_label = case_when(
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
  dplyr::mutate(class_label = case_when(
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


##### Nested donut plot -------
### main class colors
class_colors <- tibble::tibble(
  class    = c(1,2,3,4,5),
  Description = c("Forest","Other non-forest formation","Agriculture","Water","Artificial"),
  Color       = c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e")
)

# 1) Inner ring
lulc_2024 <- all_lulc_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::select(class, pland) %>%
  dplyr::mutate(pland = as.numeric(pland),
                pland = pland / sum(pland, na.rm = TRUE)) %>%
  dplyr::left_join(class_colors, by="class") %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(xmin = dplyr::lag(cumsum(pland), default=0),
                xmax = cumsum(pland),
                label_inner = paste0(Description, " (", percent(pland, accuracy=0.1), ")"))

# forest arc
forest_inner <- lulc_2024 %>% dplyr::filter(class == 1)

# 2) Outer ring (collapse assisted restoration)
forest_2024 <- forest_cat_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::filter(class != 2) %>%
  dplyr::mutate(class = ifelse(class == 15, 1, class),
                class = dplyr::case_when(
                  class %in% c(10,11,12) ~ 100, # merged bucket ID
                  TRUE ~ class
                )) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(pland = sum(as.numeric(pland), na.rm = TRUE), .groups="drop") %>%
  dplyr::filter(class %in% c(1,100,13,14)) %>%
  dplyr::mutate(pland = pland / sum(pland, na.rm=TRUE)) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(label_outer = case_when(
    class == 1   ~ "Other forest (unknown status)",
    class == 100 ~ "Assisted restored forests",
    class == 13  ~ "Forests in reserves",
    class == 14  ~ "Private forests"
  )) %>%
  dplyr::mutate(label_outer = paste0(label_outer, " (", percent(pland, accuracy=0.1), " of forest)"))

# 4 green gradient
greens <- colorRampPalette(c("#32a65e", "#165733"))(4) # light->dark

forest_2024 <- forest_2024 %>%
  dplyr::mutate(Color = greens[row_number()])

# angle mapping
forest_span <- forest_inner$xmax - forest_inner$xmin

forest_2024 <- forest_2024 %>%
  dplyr::mutate(xmin_rel = dplyr::lag(cumsum(pland), default=0),
                xmax_rel = cumsum(pland),
                xmin = forest_inner$xmin + xmin_rel*forest_span,
                xmax = forest_inner$xmin + xmax_rel*forest_span)

# plot
ggplot() +
  # inner donut
  ggforce::geom_arc_bar(
    data = lulc_2024,
    aes(
      x0 = 0, y0 = 0, r0 = 0.3, r = 1.0,
      start = 2*pi*xmin, end = 2*pi*xmax, fill = Description
    ),
    color="white", size=0.3
  ) +
  # outer donut
  ggforce::geom_arc_bar(
    data = forest_2024,
    aes(
      x0=0,y0=0,r0=1.0,r=1.7,
      start = 2*pi*xmin, end = 2*pi*xmax, fill=label_outer
    ),
    color="white", size=0.25
  ) +
  # add % values on inner ring (just the %)
  geom_text(
    data = lulc_2024,
    aes(x = sin(2*pi*(xmin+xmax))*0.8,
        y = -cos(2*pi*(xmin+xmax))*0.8,
        label = percent(pland, accuracy = 0.1)),
    size = 3,
    fontface = "bold"
  ) +
  # add % values on outer ring
  geom_text(
    data = forest_2024,
    aes(x = sin(2*pi*(xmin+xmax))*1.35,
        y = -cos(2*pi*(xmin+xmax))*1.35,
        label = percent(pland, accuracy = 0.1)),
    size = 3,
    fontface = "bold"
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_manual(values = c(
    setNames(lulc_2024$Color, lulc_2024$Description),
    setNames(forest_2024$Color, forest_2024$label_outer)
  )) +
  labs(fill=NULL)



##### Waffle plot -----

# Forest cat colors
greens <- c("#afd69b","#485B4D","#a6dbbc","#8fb6ab")

### TOP waffle = whole landscape
top <- all_lulc_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::mutate(grp = ifelse(class == 1, "Forest", "Matrix")) %>%
  dplyr::group_by(grp) %>%
  dplyr::summarise(ca = sum(ca), .groups="drop") %>%
  dplyr::mutate(p = ca/sum(ca)) %>%
  dplyr::mutate(n = round(p*100)) %>%
  dplyr::mutate(n = ifelse(n==0,1,n)) %>%
  dplyr::mutate(n = round(n/sum(n)*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10,
                block = "Landscape")

### MID waffle = forest subcategories

forest_sub <- forest_cat_metrics %>%
  dplyr::filter(year == 2024) %>%
  dplyr::filter(class != 2) %>% 
  dplyr::mutate(class = ifelse(class == 15, 1, class), # aggregate 15 and 1
                class = dplyr::case_when(class %in% c(10,11,12) ~ 100, # aggregate 10, 11 and 12
                                         TRUE ~ class)) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(ca = sum(ca), .groups="drop") %>%
  dplyr::mutate(label = dplyr::case_when(
    class == 1   ~ "Other forest (unknown status)",
    class == 100 ~ "Assisted restored forests",
    class == 13  ~ "Forests in reserves",
    class == 14  ~ "Private forests"
  )) %>%
  dplyr::mutate(p = ca/sum(ca)) %>%
  dplyr::mutate(n = round(p*100)) %>%
  dplyr::mutate(n = ifelse(n==0,1,n)) %>%
  dplyr::mutate(n = round(n/sum(n)*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10,
                block = "Forest only")
forest_sub <- forest_sub %>%
  dplyr::rename(grp = label)

### BOT waffle = matrix subcategories

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
  dplyr::mutate(n = round(p*100)) %>%
  tidyr::uncount(n) %>%
  dplyr::mutate(i = row_number(),
                x = (i-1) %% 10,
                y = (i-1) %/% 10,
                block = "Matrix only")
matrix_sub <- matrix_sub %>%
  dplyr::rename(grp = label)

### bind all
tiles <- bind_rows(top, forest_sub, matrix_sub)

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

### plot
ggplot(tiles, aes(x, y, fill = grp %||% type)) +
  geom_tile(color="white") +
  scale_y_reverse() +
  coord_fixed() +
  facet_wrap(~ block, ncol=1) +
  theme_void() +
  scale_fill_manual(values = col_vec) +
  labs(fill=NULL)


### Forest class metrics -----------
#### Summary statistics ----
# Evolution per class per year
forest_class_metrics %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(area_change = (area_mn - dplyr::lag(area_mn)) / dplyr::lag(area_mn) * 100,
                np_change = (np - dplyr::lag(np)) / dplyr::lag(np) * 100,
                FFI_change = (FFI - dplyr::lag(FFI)) / dplyr::lag(FFI) * 100,
                ECA_change_2km = (eca_ha_2000 - dplyr::lag(eca_ha_2000)) / dplyr::lag(eca_ha_2000) * 100,
                ECA_change_8km = (eca_ha_8000 - dplyr::lag(eca_ha_8000)) / dplyr::lag(eca_ha_8000) * 100) %>% 
  dplyr::select(year, area_change, np_change, FFI_change, ECA_change_2km, ECA_change_8km) %>% 
  print(n=36)

# Changes betwenn first and last year
forest_class_metrics %>%
  dplyr::summarize(area_change = area_mn[year == max(year)] - area_mn[year == min(year)],
                   np_change = np[year == max(year)] - np[year == min(year)],
                   FFI_change = FFI[year == max(year)] - FFI[year == min(year)],
                   ECA_change_2km = eca_ha_2000[year == max(year)] - eca_ha_2000[year == min(year)],
                   ECA_change_8km = eca_ha_8000[year == max(year)] - eca_ha_8000[year == min(year)],
                   area_change_perc = ((area_mn[year == max(year)] - area_mn[year == min(year)]) / area_mn[year == min(year)]) * 100,
                   np_change_perc = ((np[year == max(year)] - np[year == min(year)]) / np[year == min(year)]) * 100,
                   FFI_change_perc = ((FFI[year == max(year)] - FFI[year == min(year)]) / FFI[year == min(year)]) * 100,
                   ECA_change_2km_perc = ((eca_ha_2000[year == max(year)] - eca_ha_2000[year == min(year)]) / eca_pct_land_2000[year == min(year)]) * 100,
                   ECA_change_8km_perc = ((eca_ha_8000[year == max(year)] - eca_ha_8000[year == min(year)]) / eca_pct_land_8000[year == min(year)]) * 100)


# Between sets of years
forest_class_metrics %>%
  dplyr::summarize(
    # --- Absolute changes ---
    area_change_1989_2000 = area_mn[year == 2000] - area_mn[year == 1989],
    area_change_2000_2012 = area_mn[year == 2012] - area_mn[year == 2000],
    area_change_2012_2024 = area_mn[year == 2024] - area_mn[year == 2012],
    
    np_change_1989_2000 = np[year == 2000] - np[year == 1989],
    np_change_2000_2012 = np[year == 2012] - np[year == 2000],
    np_change_2012_2024 = np[year == 2024] - np[year == 2012],
    
    FFI_change_1989_2000 = FFI[year == 2000] - FFI[year == 1989],
    FFI_change_2000_2012 = FFI[year == 2012] - FFI[year == 2000],
    FFI_change_2012_2024 = FFI[year == 2024] - FFI[year == 2012],
    
    ECA_change_1989_2000_2km = eca_ha_2000[year == 2000] - eca_ha_2000[year == 1989],
    ECA_change_2000_2012_2km = eca_ha_2000[year == 2012] - eca_ha_2000[year == 2000],
    ECA_change_2012_2024_2km = eca_ha_2000[year == 2024] - eca_ha_2000[year == 2012],
    
    # --- Percentage changes ---
    area_change_perc_1989_2000 = ((area_mn[year == 2000] - area_mn[year == 1989]) / area_mn[year == 1989]) * 100,
    area_change_perc_2000_2012 = ((area_mn[year == 2012] - area_mn[year == 2000]) / area_mn[year == 2000]) * 100,
    area_change_perc_2012_2024 = ((area_mn[year == 2024] - area_mn[year == 2012]) / area_mn[year == 2012]) * 100,
    
    np_change_perc_1989_2000 = ((np[year == 2000] - np[year == 1989]) / np[year == 1989]) * 100,
    np_change_perc_2000_2012 = ((np[year == 2012] - np[year == 2000]) / np[year == 2000]) * 100,
    np_change_perc_2012_2024 = ((np[year == 2024] - np[year == 2012]) / np[year == 2012]) * 100,
    
    FFI_change_perc_1989_2000 = ((FFI[year == 2000] - FFI[year == 1989]) / FFI[year == 1989]) * 100,
    FFI_change_perc_2000_2012 = ((FFI[year == 2012] - FFI[year == 2000]) / FFI[year == 2000]) * 100,
    FFI_change_perc_2012_2024 = ((FFI[year == 2024] - FFI[year == 2012]) / FFI[year == 2012]) * 100,
    
    ECA_change_perc_1989_2000_2km = ((eca_ha_2000[year == 2000] - eca_ha_2000[year == 1989]) / 
                                       eca_ha_2000[year == 1989]) * 100,
    ECA_change_perc_2000_2012_2km = ((eca_ha_2000[year == 2012] - eca_ha_2000[year == 2000]) / 
                                       eca_ha_2000[year == 2000]) * 100,
    ECA_change_perc_2012_2024_2km = ((eca_ha_2000[year == 2024] - eca_ha_2000[year == 2012]) / 
                                       eca_ha_2000[year == 2012]) * 100
  )



#### Plots -----
##### Line plot -----
data = forest_class_metrics %>%
  dplyr::select(
    year,
    ca,
    np,
    area_mn,
    FFI,
    eca_ha_2000,
    eca_ha_8000
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
      Metric == "eca_ha_2000" ~ "Equivalent Connected Area (ECA, 2km)",
      Metric == "eca_ha_8000" ~ "Equivalent Connected Area (ECA, 8km)",
      TRUE ~ Metric
    )
  )

# Plot facets
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
  filter(year == end_year) %>%
  select(all_of(metrics)) %>%
  as.list()

# Overall change 1989 → 2024
values_1989 <- forest_series %>%
  filter(year == start_year) %>%
  select(all_of(metrics)) %>%
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
