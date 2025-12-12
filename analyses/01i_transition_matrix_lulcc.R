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
library(RColorBrewer)

### Import rasters -------

# Rasters reclassified (including corridors as forests)
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
plot(rasters[[36]], col=c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e"))

# Rasters reclassified with forest categories
base_path = here("outputs", "data", "MapBiomas", "Rasters_reclass_forest_cat")
raster_files = list.files(base_path, pattern = "\\.tif$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(raster_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
raster_df = data.frame(file = raster_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load rasters in chronological order
rasters_forest_cat = lapply(raster_df$file, terra::rast)
years = raster_df$year
# Check
for (i in seq_along(rasters_forest_cat)) {
  cat("Year", years[i], " → raster name:", basename(raster_df$file[i]), "\n")
}

raster2024 = rasters_forest_cat[[36]]
vals_after <- c(1,2,3,4,5,10,11,12,13,14,15)
cols_after <- c("#32a65e", "#ad975a", "#FFFFB2", "#0000FF", "#d4271e", 
                "lightgreen", "chartreuse", "darkseagreen", "darkolivegreen","darkkhaki", "yellow")
terra::coltab(raster2024) <- cbind(vals_after, cols_after)
plot(raster2024)
freq(raster2024)

### Transition matrix -----
#### Spatial trajectories (rasters) -------

##### 1. REMOVE ROAD OVERPASSES (not forest) ------
# We override road overpasses (cannot be considered as "reforestation")
rasters_corrected <- vector("list", length(rasters))

for (i in seq_along(rasters)) {
  r_orig <- rasters[[i]]
  r_cat  <- rasters_forest_cat[[i]]
  
  # Extract values
  v_orig <- values(r_orig)
  v_cat  <- values(r_cat)
  
  # Replace: wherever forest_cat == 15 → set original raster to 5
  v_orig[v_cat == 15] <- 5
  
  # Write back
  rasters_corrected[[i]] <- setValues(r_orig, v_orig)
  names(rasters_corrected)[i] <- names(rasters)[i]
}


##### 2a. YEAR-TO-YEAR TRANSITIONS -------
# Create a set of transition rasters where each cell encodes its land-use change from the previous year
# These rasters show all annual land-use transitions.
# The output value = (prev * 10) + curr (for instance, 13 = Previously forest, now agriculture)

# Function to compute year-to-year land-use transitions
compute_transitions <- function(rasters, years) {
  # Check inputs
  if (length(rasters) != length(years)) {
    stop("Length of 'rasters' and 'years' must be the same.")
  }
  
  # Initialize list for transition rasters
  transition_list <- vector("list", length(rasters) - 1)
  
  # Loop through consecutive pairs of years
  for (i in 2:length(rasters)) {
    message("Processing transition: ", years[i - 1], " → ", years[i])
    
    prev <- rasters[[i - 1]]
    curr <- rasters[[i]]
    
    # Compute transition code (e.g. 13 = 1→3)
    transition <- (prev * 10) + curr
    
    # Assign name to the SpatRaster layer
    names(transition) <- paste0("t", years[i - 1], "_", years[i])
    
    # Store in list with meaningful name
    transition_list[[i - 1]] <- transition
  }
  
  # Assign names to list elements
  names(transition_list) <- paste0("t", years[-length(years)], "_", years[-1])
  
  # Return list of SpatRasters
  return(transition_list)
}

# Example usage:
yty_transitions <- compute_transitions(rasters_corrected, years)
plot(yty_transitions[[1]], main = names(yty_transitions)[1])
unique(values(yty_transitions[[1]]))

## Check
# Choose a transition code you want to inspect (e.g., 15 = 1→5)
target_transition <- 15

# Select the transition raster to inspect
trans_rast <- yty_transitions[[1]]   # example for tYYYY_YYYY

# Find ALL cell indices where the transition occurred
cells_changed <- which(values(trans_rast) == target_transition)

# Choose ONE cell at random (or the first one)
cell_id <- cells_changed[1]

# Get coordinates of that raster cell
cell_xy <- xyFromCell(trans_rast, cell_id)

# Create a geographic buffer around that point
# Buffer radius in map units
buffer_radius <- 200
buf <- buffer(vect(cell_xy, crs=crs(trans_rast)), width = buffer_radius)

# Crop rasters to the buffered window
prev <- rasters[[1]]
curr <- rasters[[2]]

# Plot
prev_crop <- crop(prev, buf)
curr_crop <- crop(curr, buf)
trans_crop <- crop(trans_rast, buf)
par(mfrow=c(1,3))

plot(prev_crop, main="Before")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot(curr_crop, main="After")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot(trans_crop, main=paste("Transition =", target_transition))
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

par(mfrow=c(1,1))


##### 2b. CUMULATIVE TRANSITIONS -------
# Contrary to 1a, here the transitions are cumulative, i.e., the cell value appends each time the cell land use changes.
# Rules:
# If the pixel does not change, its value stays the same.
# If it does change, the new transition code is appended (e.g., 1 → 2 → 1 becomes 121).
# Each raster in the output list represents the cumulative state up to that time step. This preserves the full temporal history of each pixel.

# Function to compute cumulative land-use transitions
compute_cumulative_transitions <- function(rasters, years) {
  
  if (length(rasters) != length(years))
    stop("Length of 'rasters' and 'years' must be the same.")
  
  # Initial cumulative raster = first-year raster (already numeric categories)
  cumulative_r <- rasters[[1]]
  cum_vals <- values(cumulative_r)
  
  out <- vector("list", length(rasters) - 1)
  
  for (i in 2:length(rasters)) {
    curr_vals <- values(rasters[[i]])
    prev_vals <- values(rasters[[i - 1]])
    
    changed <- !is.na(prev_vals) & !is.na(curr_vals) & curr_vals != prev_vals
    cum_vals[changed] <- cum_vals[changed] * 10 + curr_vals[changed]
    
    cumulative_r <- setValues(cumulative_r, cum_vals)
    names(cumulative_r) <- years[i]
    
    out[[i - 1]] <- cumulative_r
  }
  
  names(out) <- years[-1]
  out
}

# Example usage:
cumulative_transitions <- compute_cumulative_transitions(rasters_corrected, years)
plot(cumulative_transitions[[1]], main = names(cumulative_transitions)[1])
freq(cumulative_transitions[[1]])
freq(cumulative_transitions[[length(cumulative_transitions)]])

## Check
# Choose the cumulative code to inspect
target_traj <- 131

# Final cumulative raster (contains the full trajectory)
final_cum <- cumulative_transitions[[length(cumulative_transitions)]]

# Get cell indices where this exact trajectory occurred
cells_traj <- which(values(final_cum) == target_traj)
cell_id <- cells_traj[1]   # pick the first match

# Coordinates of the focal pixel
cell_xy <- xyFromCell(final_cum, cell_id)

# Define buffer radius (map units)
buffer_radius <- 500
buf <- buffer(vect(cell_xy, crs = crs(final_cum)), width = buffer_radius)

# Re-read the raw values for this pixel across all years
pixel_values <- sapply(rasters, function(r) values(r)[cell_id])

# Pair them with the years
trajectory <- data.frame(
  year = years,
  value = pixel_values
)
trajectory

# Crop relevant rasters
years_to_plot <- c(1994, 1995, 1998)
rasters_subset <- rasters[match(years_to_plot, years)]

rasters_crop <- lapply(rasters_subset, function(r) crop(r, buf))
names(rasters_crop) <- years_to_plot

# Crop transition rasters
trans_crop_94_95 <- crop(cumulative_transitions[[6]], buf)
trans_crop_95_98 <- crop(cumulative_transitions[[9]], buf)

# Plot
par(mfrow=c(2,3), mar=c(3,3,2,1))
plot(rasters_crop[[1]], main="1994")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot(rasters_crop[[2]], main="1995")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot(rasters_crop[[3]], main="1998")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

# Transition rasters
plot(trans_crop_94_95, main="Transition 1994→1995")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot(trans_crop_95_98, main="Transition 1995→1998")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot.new()   # empty panel
par(mfrow=c(1,1))


##### 3. RECLASS TRANSITION RASTERS ------
# Reclassify cumulative trajectories to identify reforestation (6) and deforestation (7) events
# Deforested cells (7) → all cells that are currently non-forest (2–5) but were forest (1) at least once in the past.
# Reforested cells (6) → all cells that are currently forest (1) but were non-forest (2–5) at least once in the past.
# Stable cells (1 or other) → all other cells keep their current value.
# Otherwise: Keep current land-cover value at year

# Reclassify cumulative transitions into reforestation (6) and deforestation (7)
reclass_cumulative_transitions <- function(cumulative_transitions) {
  
  out <- vector("list", length(cumulative_transitions))
  
  for (i in seq_along(cumulative_transitions)) {
    message("Reclassifying cumulative raster #", i,
            " (", names(cumulative_transitions)[i], ")")
    
    tr   <- cumulative_transitions[[i]]
    vals <- values(tr)
    
    # Convert cumulative code to character
    sval <- as.character(vals)
    
    # Extract the most recent land-cover value (last digit)
    last_digit <- as.numeric(substr(sval, nchar(sval), nchar(sval)))
    
    # Re-initialize output values: default = current land cover
    newvals <- last_digit
    
    # Extract history string (all digits except the last)
    history <- substr(sval, 1, nchar(sval) - 1)
    
    # Detect reforestation (6): now 1, but history contains 2–5
    reforest_idx <- which(
      !is.na(sval) &
        last_digit == 1 &
        grepl("[2-5]", history)
    )
    
    # Detect deforestation (7): now 2–5, but history contains 1
    deforest_idx <- which(
      !is.na(sval) &
        last_digit %in% 2:5 &
        grepl("1", history)
    )
    
    # Apply reclassification
    newvals[reforest_idx] <- 6
    newvals[deforest_idx] <- 7
    
    # Write reclassified raster
    out[[i]] <- setValues(tr, newvals)
    names(out[[i]]) <- names(cumulative_transitions)[i]
  }
  
  names(out) <- names(cumulative_transitions)
  out
}

# Example usage:
reclass_cumul_trans <- reclass_cumulative_transitions(cumulative_transitions)

# Quick check
freq(reclass_cumul_trans[[1]]) # 1990
freq(reclass_cumul_trans[[35]]) # 2024


##### 4. DETECT YEAR OF LULCC ----------
# For each reforested or deforested cell, we identify WHEN (which year) the change occurred
# To do so, these cells take the value of the year a cell has changed (e.g., 1998)
# NB: some cells changed several times (reforested-deforested-reforested, etc.), hence we create several rasters
# The first is the first time of change; the second, the second time the cell changed, etc.

# Function
detect_all_forest_changes <- function(reclass_list, baseline_raster) {
  # Template raster to match extent, resolution, and number of cells
  r_template <- reclass_list[[1]]
  
  # List to store detected changes
  changes <- list()
  
  # Loop over all reclassified rasters
  for (i in seq_along(reclass_list)) {
    curr_vals <- values(reclass_list[[i]])
    
    # Previous raster: baseline for first year, otherwise previous year
    prev_vals <- if (i == 1) values(baseline_raster) else values(reclass_list[[i - 1]])
    
    # Identify cells that changed AND are reforestation (6) or deforestation (7)
    forest_change <- !is.na(prev_vals) & !is.na(curr_vals) & curr_vals != prev_vals & curr_vals %in% c(6, 7)
    
    # Skip if no changes
    if (!any(forest_change)) next
    
    # Store cell IDs, year, and type of forest change
    changes[[length(changes) + 1]] <- data.frame(
      cell_id     = which(forest_change),
      year        = as.numeric(names(reclass_list)[i]),
      change_type = curr_vals[forest_change]
    )
  }
  
  # Combine all change records into one table and assign order per cell
  change_table <- dplyr::bind_rows(changes) %>%
    dplyr::arrange(cell_id, year) %>%
    dplyr::group_by(cell_id) %>%
    dplyr::mutate(change_order = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  # Maximum number of changes for any cell
  max_changes <- max(change_table$change_order)
  
  # Create one raster per change order
  year_rasters <- lapply(seq_len(max_changes), function(k) {
    r <- rast(r_template); values(r) <- NA
    subset_k <- dplyr::filter(change_table, change_order == k)
    if (nrow(subset_k) > 0) r[subset_k$cell_id] <- subset_k$year
    names(r) <- paste0("year_change_", k)
    r
  })
  
  # Return a list: change table + rasters for each change order
  list(change_table = change_table, year_change_rasters = year_rasters)
}


# Example usage:
# baseline must be the first-year raster of raw classes
baseline_1989 <- rasters_corrected[[1]]
years_forest_change <- detect_all_forest_changes(reclass_cumul_trans, baseline_1989)

# Inspect results
head(years_forest_change$change_table, n=100)
plot(years_forest_change$year_change_rasters[[1]], main="Year of 1st forest change")

## Check (focusing on a cell)
# Randomly select one changed cell (first change)
set.seed(1)
first_changes <- years_forest_change$change_table %>%
  dplyr::filter(change_order == 1)

chosen <- first_changes[sample(nrow(first_changes), 1), ]
chosen

# Identify year of change and index of that year in your `years` vector
year_change <- chosen$year
year_idx <- which(years == year_change)
year_before <- years[year_idx - 1]

message("Selected cell ID: ", chosen$cell_id)
message("   → First change year: ", year_change)
message("   → Comparing ", year_before, " (before) vs ", year_change, " (after)")

# Extract rasters from the correct list indices
r_before <- rasters[[year_idx - 1]]
r_after  <- rasters[[year_idx]]
merged_after <- reclass_cumul_trans[[year_idx]]
year_change_r <- years_forest_change$year_change_rasters[[1]]  # first change raster

# Get coordinates of the chosen cell
xy_chosen <- xyFromCell(r_before, chosen$cell_id)

# Define small zoom window around that cell
zoom_box <- ext(
  xy_chosen[1] - 250,
  xy_chosen[1] + 250,
  xy_chosen[2] - 250,
  xy_chosen[2] + 250
)

# Crop for zoomed visualization
r_before_crop <- crop(r_before, zoom_box)
r_after_crop <- crop(r_after, zoom_box)
merged_after_crop <- crop(merged_after, zoom_box)
year_change_crop <- crop(year_change_r, zoom_box)

# Plot all panels
par(mfrow = c(2, 2))

plot(r_before_crop,
     main = paste("Before (", year_before, ")", sep=""),
     col = c("#32a65e", "#FFFFB2", "#d4271e"))
points(xy_chosen, pch = 16, cex = 1.2)

plot(r_after_crop,
     main = paste("After (", year_change, ")", sep=""),
     col = c("#32a65e", "#FFFFB2", "#d4271e"))
points(xy_chosen, pch = 16, cex = 1.2)

plot(merged_after_crop,
     main = paste("Merged (", year_change, ")", sep=""),
     col = c("#32a65e", "#FFFFB2", "chartreuse", "pink"))
points(xy_chosen, pch = 16, cex = 1.2)

plot(year_change_crop,
     main = "Year of 1st forest change",
     col = terrain.colors(10))
points(xy_chosen, pch = 16, cex = 1.2)

par(mfrow = c(1, 1))


##### Visual demo of all steps ---------

# Central coordinates
x_center <- 778151.2
y_center <- 7508862.8

# Zoom window
buffer_size <- 1200
zoom_ext <- ext(
  x_center - buffer_size, x_center + buffer_size,
  y_center - buffer_size, y_center + buffer_size
)

# Choose a year index in rasters (1–36)
index_demo <- 7 # do not select the first raster (to plot changes, we need at least the first year + 1)
year_demo  <- years[index_demo]

cat("Raster year selected:", year_demo, "\n")

### STEP 0 – Raw imported raster (no corrections)
r_step0 <- crop(rasters[[index_demo]], zoom_ext)

### STEP 2 – Cumulative transitions (exists only from index 2)
r_step2 <- crop(cumulative_transitions[[index_demo - 1]], zoom_ext)

### STEP 3 – Re/deforestation classification
r_step3 <- crop(reclass_cumul_trans[[index_demo - 1]], zoom_ext)

### STEP 4 – Year of 1st change
r_step4 <- crop(years_forest_change$year_change_rasters[[1]], zoom_ext)


### PLOT

png(here("outputs","plot","01i_tm_lulc_demo.png"), width = 3000, height = 2000, res = 300)
par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))

## STEP 0 – Raw imported
plot(
  r_step0,
  main = paste0("Step 0 – Raw imported (", year_demo, ")"),
  col = c("#32a65e", "#ad975a", "#FFFFB2", "#d4271e")
)
points(x_center, y_center, pch=16, col="red")

## STEP 2 – Cumulative transitions
vals2 <- unique(values(r_step2))
n_classes <- length(vals2[!is.na(vals2)])
plot(
  r_step2,
  main = "Step 2 – Cumulative trajectory code",
  col = brewer.pal(max(3, min(n_classes, 8)), "Pastel1")
)
points(x_center, y_center, pch=16, col="red")

## STEP 3 – Re/deforestation classification
plot(
  r_step3,
  main = "Step 3 – Re/deforestation classification",
  col = c(
    "#32a65e",     # 1 = forest
    "#ad975a",     # 2
    "#FFFFB2",     # 3
    "#d4271e",     # 5
    "chartreuse",  # 6 = reforest
    "pink"      # 7 = deforest
  )
)
points(x_center, y_center, pch=16, col="red")

## STEP 4 – Year of 1st change
vals4 <- unique(values(r_step4))
n_classes <- length(vals4[!is.na(vals4)])
plot(
  r_step4,
  main = "Step 4 – Year of 1st forest change",
  col = brewer.pal(max(3, min(n_classes, 8)), "Greys")
)
points(x_center, y_center, pch=16, col="red")

par(mfrow = c(1,1))
dev.off()

##### Summary statistics — last raster ------

## Select last raster
r_last <- reclass_cumul_trans[[length(reclass_cumul_trans)]]
year_label <- names(reclass_cumul_trans)[length(reclass_cumul_trans)]

message("🟩 Computing summary statistics for year ", year_label, " ...")

## Number of cells per category
tab <- freq(r_last) %>% as.data.frame()
colnames(tab) <- c("layer", "class", "n_cells")

## Surface (ha) and percentage (%)
res_m <- res(r_last)
cell_area_ha <- prod(res_m) / 10000

tab <- tab %>%
  dplyr::mutate(area_ha = n_cells * cell_area_ha,
         perc_tot = 100 * n_cells / sum(n_cells))

## Add readable class labels
class_labels <- c(
  "1" = "Forest",
  "2" = "Other non-forest habitat",
  "3" = "Agriculture",
  "4" = "Water",
  "5" = "Urban areas",
  "6" = "Reforested (secondary forest)",
  "7" = "Deforested"
)

tab_plot <- tab %>%
  dplyr::mutate(class = class_labels[as.character(class)])

## Print summary sentences
cat("\n=== Land-use summary for", year_label, "===\n")
tab_plot %>%
  dplyr::arrange(desc(perc_tot)) %>%
  dplyr::mutate(
    sentence = sprintf(
      "- %s covers %.0f ha (%.1f%% of the total area).",
      class, area_ha, perc_tot
    )
  ) %>%
  dplyr::pull(sentence) %>%
  cat(sep = "\n")

## Donut plot — Land-use composition
category_colors <- c(
  "Forest" = "#32a65e",
  "Other non-forest habitat" = "#ad975a",
  "Agriculture" = "#FFFFB2",
  "Water" = "#0000FF",
  "Urban areas" = "#d4271e",
  "Reforested (secondary forest)" = "chartreuse",
  "Deforested" = "pink"
)

ggplot(tab_plot, aes(x = 2, y = perc_tot, fill = class)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = paste0(round(perc_tot, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  scale_fill_manual(values = category_colors) +
  theme_void() +
  ggtitle(paste0("Land-use composition · ", year_label)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

## Forest-only breakdown (Intact + Secondary)
forest <- tab %>%
  dplyr::filter(class %in% c(1, 6)) %>%
  dplyr::mutate(
    class = case_when(
      class == 1 ~ "Intact forest",
      class == 6 ~ "Secondary forest"
    ),
    area_ha = n_cells * cell_area_ha
  ) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(area_ha = sum(area_ha), .groups = "drop") %>%
  dplyr::mutate(perc_tot = area_ha / sum(area_ha) * 100)

## Print forest composition sentences
cat("\n=== Forest composition for", year_label, "===\n")
forest %>%
  dplyr::arrange(desc(perc_tot)) %>%
  dplyr::mutate(
    sentence = sprintf(
      "- %s represents %.0f ha (%.1f%% of total forest area).",
      class, area_ha, perc_tot
    )
  ) %>%
  dplyr::pull(sentence) %>%
  cat(sep = "\n")

## Donut plot — Forest composition
forest_colors <- c(
  "Intact forest" = "#32a65e",
  "Secondary forest" = "#61FA95"
)

ggplot(forest, aes(x = 2, y = perc_tot, fill = class)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = paste0(round(perc_tot, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  scale_fill_manual(values = forest_colors) +
  theme_void() +
  ggtitle(paste0("Forest composition · ", year_label)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

##### Export rasters -----------
message("Exporting rasters...")


# Export reclass_cumul_trans (starts from year 2)
# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_cumulative_tm")

message("Exporting reclass_cumul_trans rasters (re/deforestation classes)...")

for (i in seq_along(reclass_cumul_trans)) {
  year_i <- years[i + 1]  # because element 1 = transition from years[1]→years[2]
  output_path <- file.path(output_dir, paste0("raster_reclass_cumul_tm_", year_i, ".tif"))
  
  message("  - Writing reclass raster for year ", year_i)
  
  terra::writeRaster(
    reclass_cumul_trans[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )
}


# Export years_forest_change rasters
# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_years_forest_change")

message("Exporting rasters for years of forest change...")

for (i in seq_along(years_forest_change$year_change_rasters)) {
  
  output_path <- file.path(
    output_dir,
    sprintf("raster_year_lulc_change_%02d.tif", i) # nicely padded index
  )
  
  message("  - Writing change-event raster #", i)
  
  terra::writeRaster(
    years_forest_change$year_change_rasters[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT2U", gdal = c("COMPRESS=LZW"))
  )
}


# #### Non-spatial trajectories --------
# # Below, we compute a transition matrix on the rasters to identify the changes in land use across the landscape and through time
# ## On all rasters (year-to-year changes)
# 
# transition_matrix = lapply(1:(length(rasters_merged) - 1), function(i) {
#   # Display progress message
#   message("Computing transition matrix: ", years[i], " → ", years[i + 1])
#   # Compute crosstab between consecutive years
#   t = terra::crosstab(c(rasters_merged[[i]], rasters_merged[[i + 1]]))
#   # Return list with metadata and table
#   list(year_from = years[i], year_to = years[i + 1], table = t)
# })
# transition_matrix[[1]]
# 
# ## For selected years
# 
# # Reclass rasters where matrix types 2-5 -> 10
# reclass_cumul_trans_mat10 <- lapply(reclass_cumul_trans, function(r) {
#   v <- values(r)
#   v[v %in% 2:5] <- 10             # collapse matrix types into class 10
#   r2 <- setValues(r, v)
#   return(r2)
# })
# names(reclass_cumul_trans_mat10) <- names(reclass_cumul_trans)  # preserve year names
# 
# # Find indices of the target years inside the reclass list
# # reclass_cumul_trans elements are named by year
# target_years = c(1990, 2000, 2012, 2023)
# years_reclass <- as.numeric(names(reclass_cumul_trans_mat10))
# idx_reclass <- match(target_years, years_reclass)
# 
# # Transition_stats function
# transition_stats <- function(r1, r2, year1, year2) {
#   # Cross-tabulate transitions (long format)
#   tab <- terra::crosstab(c(r1, r2), long = TRUE)
#   colnames(tab) <- c("from", "to", "n_cells")
# 
#   # Compute area (1 cell = resolution_x * resolution_y, convert m² → ha)
#   res_m <- res(r1)
#   cell_area_ha <- prod(res_m) / 10000
# 
#   # Add area and percentages
#   total_cells <- sum(tab$n_cells)
#   tab <- tab %>%
#     dplyr::mutate(area_ha = n_cells * cell_area_ha,
#                   perc = 100 * n_cells / total_cells,
#                   transition = paste0(from, "_to_", to),
#                   period = paste0(year1, "-", year2))
# 
#   return(tab)
# }
# 
# # Compute transitions using the MAT10 reclass list
# transitions <- list()
# for (i in seq_len(length(idx_reclass) - 1)) {
#   r1 <- reclass_cumul_trans_mat10[[ idx_reclass[i] ]]
#   r2 <- reclass_cumul_trans_mat10[[ idx_reclass[i + 1] ]]
# 
#   transitions[[i]] <- transition_stats(
#     r1, r2,
#     year1 = target_years[i],
#     year2 = target_years[i + 1]
#   )
# }
# 
# # Combine results and add readable class labels
# transition_df <- bind_rows(transitions)
# 
# # Recompute class labels: now matrix types collapsed as 10
# class_labels <- c("1" = "Forest", "10" = "Matrix", "6" = "Reforested", "7" = "Deforested")
# 
# transition_df <- transition_df %>%
#   dplyr::mutate(
#     from_class = class_labels[as.character(from)],
#     to_class   = class_labels[as.character(to)]
#   ) %>%
#   dplyr::select(period, from_class, to_class, n_cells, area_ha, perc)
# 
# 
# ##### Plot Sankey diagram of transition matrix
# 
# ## With PantaRhei
# # Extract year pairs
# transition_df = transition_df %>%
#   tidyr::separate(period, into = c("year_from", "year_to"), sep = "-", convert = TRUE)
# 
# # Build flows
# flows = transition_df %>%
#   dplyr::mutate(
#     from = paste0(from_class, "_", year_from),
#     to = paste0(to_class, "_", year_to),
#     substance = case_when(
#       from_class == "Forest" & to_class == "Forest" ~ "Forest_stay",
#       from_class == "Matrix" & to_class == "Matrix" ~ "Matrix_stay",
#       from_class == "Forest" & to_class == "Deforested" ~ "Deforestation",
#       from_class == "Matrix" & to_class == "Reforested" ~ "Reforestation",
#       from_class == "Deforested" & to_class == "Deforested" ~ "Deforested_stay",
#       from_class == "Reforested" & to_class == "Reforested" ~ "Reforested_stay",
#       from_class == "Deforested" & to_class == "Reforested" ~ "Reforestation",
#       from_class == "Reforested" & to_class == "Deforested" ~ "Deforestation",
#       from_class == "Forest" & to_class == "Reforested" ~ "Reforestation",
#       from_class == "Matrix" & to_class == "Deforested" ~ "Deforestation",
#       TRUE ~ "Other"
#     ),
#     quantity = perc
#   ) %>%
#   dplyr::select(from, to, substance, quantity)
# 
# # Build nodes
# # This defines the structure and positions for plotting
# nodes = tibble::tribble(
#   ~ID, ~label, ~label_pos, ~label_align, ~x, ~y, ~dir,
# 
#   # 1990
#   "Forest_1990", "Forest 1990", "left", "", -6, 2, "right",
#   "Matrix_1990", "Matrix 1990", "left", "", -6, -2, "right",
# 
#   # 2000
#   "Forest_2000", "Forest 2000", "below", "left", -3.5, 2, "right",
#   "Matrix_2000", "Matrix 2000", "below", "left", -3.5, -2, "right",
#   "Deforested_2000", "Deforested 2000", "below", "left", -3.5, 0.5, "right",
#   "Reforested_2000", "Reforested 2000", "below", "left", -3.5, -0.5, "right",
# 
#   # 2012
#   "Forest_2012", "Forest 2012", "below", "left", -1, 2, "right",
#   "Matrix_2012", "Matrix 2012", "below", "left", -1, -2, "right",
#   "Deforested_2012", "Deforested 2012", "below", "left", -1, 0.5, "right",
#   "Reforested_2012", "Reforested 2012", "below", "left", -1, -0.5, "right",
# 
#   # 2023
#   "Forest_2023", "Forest 2023", "right", "", 2, 2, "right",
#   "Matrix_2023", "Matrix 2023", "right", "", 2, -2, "right",
#   "Deforested_2023", "Deforested 2023", "right", "", 2, 0.5, "right",
#   "Reforested_2023", "Reforested 2023 (Secondary forest)", "right", "", 2, -0.5, "right"
# )
# 
# # Define palette
# palette = tibble::tribble(
#   ~substance, ~color,
#   "Forest_stay", "#32a65e",
#   "Matrix_stay", "#7B68EE",
#   "Reforestation", "#61FA95",
#   "Deforestation", "#D95F02",
#   "Deforested_stay", "orange",
#   "Reforested_stay", "lightgreen"
# )
# 
# # Node style and title
# ns = list(
#   type = "arrow",
#   gp = grid::gpar(fill = "#00008B", col = "white", lwd = 2),
#   length = 0.7,
#   label_gp = grid::gpar(col = "#00008B", fontsize = 9),
#   mag_pos = "label",
#   mag_fmt = "%.1f %%", # Define the label (for ha: %.0f ha means 0 decimal followed by ha; for percentages: %.1f %% means 1 decimal followed by %)
#   mag_gp = grid::gpar(fontsize = 9, fontface = "bold", col = "#00008B")
# )
# 
# title_txt = "Forest dynamics (1990–2023)"
# attr(title_txt, "gp") = grid::gpar(fontsize = 16, fontface = "bold", col = "#00008B")
# 
# # Plot Sankey diagram
# sankey_plot = PantaRhei::sankey(
#   nodes, flows, palette,
#   node_style = ns,
#   max_width = 0.1,
#   rmin = 0.5,
#   legend = FALSE,
#   page_margin = c(0.15, 0.05, 0.25, 0.20),
#   title = title_txt
# )
# 
# # Export PDF
# pdf("sankey_forest_trajectories_1990_2023.pdf", width = 13, height = 7)
# PantaRhei::sankey(
#   nodes, flows, palette,
#   node_style = ns,
#   max_width = 0.1,
#   rmin = 0.5,
#   legend = FALSE,
#   page_margin = c(0.15, 0.05, 0.25, 0.20),
#   title = title_txt
# )
# dev.off()