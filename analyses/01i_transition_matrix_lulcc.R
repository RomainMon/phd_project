#------------------------------------------------#
# Author: Romain Monassier
# Objective: Compute rasters transition matrix
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(terra)
library(ggplot2)
library(raster)
library(sf)
library(PantaRhei) # For the Sankey diagram

### Import rasters (with corridors) -------
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}
plot(rasters[[1]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))

### Transition matrix -----
#### Forest trajectories (rasters) -------
# Here, we identify forest trajectories on rasters

# Reclassify all non-forest (2–5) to 10 (matrix)
rasters = lapply(rasters, function(r) classify(r, cbind(2:5, 10)))

# Initialize with the first raster (1989)
final = rasters[[1]]
final[final == 1] = 1     # forest
final[final == 10] = 10   # matrix
plot(final)

# Store cumulative rasters if you want to plot multiple years
final_list = list(final)

# Loop through the rest of the years
for (i in 2:length(rasters)) {
  curr = rasters[[i]]
  prev = final
  
  # Apply transition rules
  reforested = ( (prev == 10 | prev == 3) & curr == 1 )
  deforested = ( (prev == 1 | prev == 2) & curr == 10 )
  
  # Update cumulative map
  final[reforested] = 2
  final[deforested] = 3
  
  # Store this year's cumulative result (optional)
  final_list[[i]] = final
}

# Plot maps
plot(final_list[[2]], col = c("darkgreen", "lightgreen", "red", "grey70"), 
     main = paste0("Forest Dynamics ", years[[2]]),
     axes = FALSE, legend = TRUE)
plot(final_list[[35]], col = c("darkgreen", "lightgreen", "red", "grey70"), 
     main = paste0("Forest Dynamics ", years[[35]]),
     axes = FALSE, legend = TRUE)
freq(final)


# Below, we compute a transition matrix on the rasters to identify the changes in land use across the landscape and through time
#### Transition matrix on all rasters (year-to-year changes) ----

# transition_matrix = lapply(1:(length(rasters_merged) - 1), function(i) {
#   # Display progress message
#   message("Computing transition matrix: ", years[i], " → ", years[i + 1])
#   # Compute crosstab between consecutive years
#   t = terra::crosstab(c(rasters_merged[[i]], rasters_merged[[i + 1]]))
#   # Return list with metadata and table
#   list(year_from = years[i], year_to = years[i + 1], table = t)
# })
# transition_matrix[[1]]

#### Transition matrix for selected years ----

# Define years of interest
target_years = c(1989, 2000, 2012, 2023)

# Get their indices in your final_list
idx = match(target_years, years)

# Function to compute transition matrix between two rasters
transition_stats = function(r1, r2, year1, year2) {
  
  # Cross-tabulate transitions
  tab = terra::crosstab(c(r1, r2), long = TRUE)
  colnames(tab) = c("from", "to", "n_cells")
  
  # Compute area (1 cell = resolution_x * resolution_y, convert m² → ha)
  res_m = res(r1)
  cell_area_ha = prod(res_m) / 10000
  
  # Add area and percentages
  total_cells = sum(tab$n_cells)
  tab = tab %>%
    dplyr::mutate(area_ha = n_cells * cell_area_ha,
           perc = 100 * n_cells / total_cells,
           transition = paste0(from, "_to_", to),
           period = paste0(year1, "-", year2))
  
  return(tab)
}

# Compute transitions for all intervals
transitions = list()
for (i in 1:(length(idx) - 1)) {
  transitions[[i]] <- transition_stats(
    final_list[[idx[i]]],
    final_list[[idx[i + 1]]],
    target_years[i],
    target_years[i + 1]
  )
}

# Combine results
transition_df = bind_rows(transitions)

# Add category names for readability ---
class_labels = c("1"="Forest", "10"="Matrix", "2"="Reforested", "3"="Deforested")
transition_df = transition_df %>%
  dplyr::mutate(from_class = class_labels[as.character(from)],
         to_class   = class_labels[as.character(to)]) %>%
  dplyr::select(period, from_class, to_class, n_cells, area_ha, perc)


### Plot transition matrix ----

## With PantaRhei
# Extract year pairs
transition_df = transition_df %>%
  tidyr::separate(period, into = c("year_from", "year_to"), sep = "-", convert = TRUE)

# Build flows
flows = transition_df %>%
  dplyr::mutate(
    from = paste0(from_class, "_", year_from),
    to = paste0(to_class, "_", year_to),
    substance = case_when(
      from_class == "Forest" & to_class == "Forest" ~ "Forest_stay",
      from_class == "Matrix" & to_class == "Matrix" ~ "Matrix_stay",
      from_class == "Forest" & to_class == "Deforested" ~ "Deforestation",
      from_class == "Matrix" & to_class == "Reforested" ~ "Reforestation",
      from_class == "Deforested" & to_class == "Deforested" ~ "Deforested_stay",
      from_class == "Reforested" & to_class == "Reforested" ~ "Reforested_stay",
      from_class == "Deforested" & to_class == "Reforested" ~ "Reforestation",
      from_class == "Reforested" & to_class == "Deforested" ~ "Deforestation",
      from_class == "Forest" & to_class == "Reforested" ~ "Reforestation",
      from_class == "Matrix" & to_class == "Deforested" ~ "Deforestation",
      TRUE ~ "Other"
    ),
    quantity = perc
  ) %>%
  dplyr::select(from, to, substance, quantity)

# Build nodes
# This defines the structure and positions for plotting
nodes = tibble::tribble(
  ~ID, ~label, ~label_pos, ~label_align, ~x, ~y, ~dir,
  
  # 1989 (start year)
  "Forest_1989", "Forest 1989", "left", "", -6, 2, "right",
  "Matrix_1989", "Matrix 1989", "left", "", -6, -2, "right",
  
  # 2000 (second year)
  "Forest_2000", "Forest 2000", "below", "left", -3.5, 2, "right",
  "Matrix_2000", "Matrix 2000", "below", "left", -3.5, -2, "right",
  "Deforested_2000", "Deforested 2000", "below", "left", -3.5, 0.5, "right",
  "Reforested_2000", "Reforested 2000", "below", "left", -3.5, -0.5, "right",
  
  # 2012
  "Forest_2012", "Forest 2012", "below", "left", -1, 2, "right",
  "Matrix_2012", "Matrix 2012", "below", "left", -1, -2, "right",
  "Deforested_2012", "Deforested 2012", "below", "left", -1, 0.5, "right",
  "Reforested_2012", "Reforested 2012", "below", "left", -1, -0.5, "right",
  
  # 2023
  "Forest_2023", "Forest 2023", "right", "", 2, 2, "right",
  "Matrix_2023", "Matrix 2023", "right", "", 2, -2, "right",
  "Deforested_2023", "Deforested 2023", "right", "", 2, 0.5, "right",
  "Reforested_2023", "Reforested 2023 (Secondary forest)", "right", "", 2, -0.5, "right"
)

# Define palette
palette = tibble::tribble(
  ~substance, ~color,
  "Forest_stay", "#32a65e",
  "Matrix_stay", "#7B68EE",
  "Reforestation", "#61FA95",
  "Deforestation", "#D95F02",
  "Deforested_stay", "orange",
  "Reforested_stay", "lightgreen"
)

# Node style and title
ns = list(
  type = "arrow",
  gp = grid::gpar(fill = "#00008B", col = "white", lwd = 2),
  length = 0.7,
  label_gp = grid::gpar(col = "#00008B", fontsize = 9),
  mag_pos = "label",
  mag_fmt = "%.1f %%", # Define the label (for ha: %.0f ha means 0 decimal followed by ha; for percentages: %.1f %% means 1 decimal followed by %)
  mag_gp = grid::gpar(fontsize = 9, fontface = "bold", col = "#00008B")
)

title_txt = "Forest dynamics (1989–2023)"
attr(title_txt, "gp") = grid::gpar(fontsize = 16, fontface = "bold", col = "#00008B")

# Plot Sankey diagram
sankey_plot = PantaRhei::sankey(
  nodes, flows, palette,
  node_style = ns,
  max_width = 0.1,
  rmin = 0.5,
  legend = FALSE,
  page_margin = c(0.15, 0.05, 0.25, 0.20),
  title = title_txt
)

# Export PDF
pdf("sankey_forest_trajectories_1989_2023.pdf", width = 13, height = 7)
PantaRhei::sankey(
  nodes, flows, palette,
  node_style = ns,
  max_width = 0.1,
  rmin = 0.5,
  legend = FALSE,
  page_margin = c(0.15, 0.05, 0.25, 0.20),
  title = title_txt
)
dev.off()

### Doughnut plot
# Summarize land use composition per year and class
# 1989
initial_year = transition_df %>%
  dplyr::filter(year_from == min(year_from)) %>%
  dplyr::group_by(from_class) %>%
  dplyr::summarise(area_ha = sum(area_ha), .groups = "drop") %>%
  dplyr::mutate(year = min(transition_df$year_from)) %>%
  dplyr::rename(class = from_class)

# Compute composition for other years
subsequent_years = transition_df %>%
  dplyr::group_by(year = year_to, class = to_class) %>%
  dplyr::summarise(area_ha = sum(area_ha), .groups = "drop")

# Combine all years
donut_df = dplyr::bind_rows(initial_year, subsequent_years) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(perc = area_ha / sum(area_ha) * 100) %>%
  dplyr::ungroup()

# Doughnut plot
# Same colors as Sankey
category_colors = c(
  "Forest" = "#32a65e",
  "Matrix" = "#7B68EE",
  "Reforested" = "#61FA95",
  "Deforested" = "#D95F02"
)

ggplot(donut_df, aes(x = 2, y = perc, fill = class)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5) +
  facet_wrap(~year, nrow = 1) +
  scale_fill_manual(values = category_colors) +
  theme_void() +
  ggtitle("Land-use composition by year") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", color = "#00008B"),
    strip.text = element_text(size = 11, face = "bold")
  )
