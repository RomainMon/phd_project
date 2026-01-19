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
library(RColorBrewer)

### Import rasters -------

# Rasters reclassified
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
plot(rasters[[36]], col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))


### Transition matrix -----
#### Spatial trajectories (rasters) -------

##### 1. CUMULATIVE TRANSITIONS -------
# Create a set of transition rasters where each cell encodes its land-use change from the previous year
# These rasters show all annual land-use transitions.
# IMPORTANT: Transitions are cumulative, i.e., the cell value appends each time the cell land use changes.
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

# Apply
cumulative_transitions <- compute_cumulative_transitions(rasters, years)
plot(cumulative_transitions[[1]], main = names(cumulative_transitions)[1])
freq(cumulative_transitions[[1]])
freq(cumulative_transitions[[length(cumulative_transitions)]])

## Check
# Choose the cumulative code to inspect
target_traj <- 141

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
years_to_plot <- c(1992, 1993, 2001) # change according to the trajectory
rasters_subset <- rasters[match(years_to_plot, years)]

rasters_crop <- lapply(rasters_subset, function(r) crop(r, buf))
names(rasters_crop) <- years_to_plot

# Crop transition rasters
trans_crop_1 <- crop(cumulative_transitions[[4]], buf) # change accordingly with the good raster index (i.e., the cumul raster showing the new raster value) (NB: 1st cumulative raster = second year)
trans_crop_2 <- crop(cumulative_transitions[[12]], buf) # change accordingly with the good raster index (i.e., the cumul raster showing the new raster value) (NB: 1st cumulative raster = second year)

# Plot
par(mfrow=c(2,3), mar=c(3,3,2,1))
plot(rasters_crop[[1]], main="1992", col=c("#32a65e", "#FFFFB2", "#d4271e"))
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot(rasters_crop[[2]], main="1993", col=c("#32a65e", "#FFFFB2", "#d4271e"))
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot(rasters_crop[[3]], main="2001", col=c("#32a65e", "#FFFFB2", "#d4271e"))
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

# Transition rasters
plot(trans_crop_1, main="Transition")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot(trans_crop_2, main="Transition")
points(cell_xy[1], cell_xy[2], pch=20, col="red", cex=1.5)

plot.new()   # empty panel
par(mfrow=c(1,1))


##### 3. RECLASS TRANSITION RASTERS ------
# Reclassify cumulative trajectories to identify reforestation and deforestation events
# Deforested cells → all cells that are currently non-forest (2–6) but were forest (1) at least once in the past.
# Reforested cells → all cells that are currently forest (1) but were non-forest (2–6) at least once in the past.
# Stable cells (1 or other) → all other cells keep their current value.
# Otherwise: Keep current land-cover value at year

# Reclassify cumulative transitions into reforestation (6) and deforestation (7)
reclass_cumulative_transitions <- function(cumulative_transitions) {
  
  out <- vector("list", length(cumulative_transitions))
  names(out) <- names(cumulative_transitions)
  
  for (i in seq_along(cumulative_transitions)) {
    
    message("Reclassifying cumulative raster #", i,
            " (", names(cumulative_transitions)[i], ")")
    
    tr <- cumulative_transitions[[i]]
    vals <- values(tr)
    
    # Initialize output as NA
    newvals <- rep(NA_integer_, length(vals))
    
    # Identify valid cells
    ok <- !is.na(vals)
    sval <- as.character(vals[ok])
    
    # Last land-cover class (current state)
    last_digit <- as.integer(substr(sval, nchar(sval), nchar(sval)))
    
    # Default: keep current class
    newvals[ok] <- last_digit
    
    # History (excluding last digit)
    history <- substr(sval, 1, nchar(sval) - 1)
    
    # Reforestation: now forest (1), previously non-forest (2–6)
    reforest <- last_digit == 1 & grepl("[2-6]", history) # change other LULC values accordingly
    
    # Deforestation: now non-forest (2–5), previously forest (1)
    deforest <- last_digit %in% 2:6 & grepl("1", history) # change other LULC values accordingly
    
    newvals[ok][reforest] <- 7L # value for reforestation
    newvals[ok][deforest] <- 8L # value for deforestation
    
    out[[i]] <- setValues(tr, newvals)
  }
  
  out
}

# Apply
reclass_cumul_trans <- reclass_cumulative_transitions(cumulative_transitions)

# Quick check
freq(reclass_cumul_trans[[1]]) # 1990
freq(reclass_cumul_trans[[35]]) # 2024


##### 4. DETECT YEAR OF LULCC ----------
# For each reforested or deforested cell, we identify WHEN (which year) the change occurred
# To do so, these cells take the value of the year a cell has changed (e.g., 1998)
# NB: some cells changed several times (reforested-deforested-reforested, etc.), hence we create several rasters
# The first is the first time of change; the second, the second time the cell changed, etc.
# We work directly on rasters_corrected, we detect all events, and store them in a table

# Function
detect_forest_transitions <- function(rasters, years) {
  
  # List to collect all transition events
  events <- list()
  
  # Loop over consecutive years (t-1 → t)
  for (i in 2:length(rasters)) {
    
    # Land-cover values at t-1 and t
    prev <- values(rasters[[i - 1]])
    curr <- values(rasters[[i]])
    
    # Valid (non-NA) pixels
    ok <- !is.na(prev) & !is.na(curr)
    
    # Deforestation: forest (1) → non-forest (2–6)
    events[[length(events) + 1]] <-
      data.frame(
        cell_id = which(ok & prev == 1 & curr %in% 2:6),
        year = years[i],
        change_type = 8L   # deforestation
      )
    
    # Reforestation: non-forest (2–6) → forest (1)
    events[[length(events) + 1]] <-
      data.frame(
        cell_id = which(ok & prev %in% 2:6 & curr == 1),
        year = years[i],
        change_type = 7L   # reforestation
      )
  }
  
  # Merge all events and order them in time for each pixel
  changes <- dplyr::bind_rows(events) %>% 
    dplyr::arrange(cell_id, year) %>% 
    dplyr::group_by(cell_id) %>% 
    dplyr::mutate(change_order = dplyr::row_number()) %>% 
    dplyr::ungroup()
  
  # Create one raster per change order (1st change, 2nd change, etc.)
  year_rasters <- lapply(seq_len(max(changes$change_order)), function(k) {
    r <- rast(rasters[[1]])
    values(r) <- NA
    idx <- changes$cell_id[changes$change_order == k]
    r[idx] <- changes$year[changes$change_order == k]
    r
  })
  
  # Return transition table + year-of-change rasters
  list(
    change_table = changes,
    year_rasters = year_rasters
  )
}

# Apply
years_forest_change <- detect_forest_transitions(rasters = rasters, years = years)

# Inspect results
head(years_forest_change$change_table)
plot(years_forest_change$year_rasters[[1]])
years_forest_change[["change_table"]][["year"]] == 1990 # To check that changes between baseline (1989) and the right-up foloowing year were taken into account (TRUE means changes occurred between those years)

## Check (focusing on a cell)
# Randomly select one changed cell (first change)
set.seed(1)

first_changes <- years_forest_change$change_table %>%
  dplyr::filter(change_order == 1)

chosen <- first_changes[sample(nrow(first_changes), 1), ]
chosen

# Identify year of change and index in the years vector
year_change <- chosen$year
year_idx <- which(years == year_change)
year_before <- years[year_idx - 1]

message("Selected cell ID: ", chosen$cell_id)
message("→ First change year: ", year_change)
message("→ Comparing ", year_before, " (before) vs ", year_change, " (after)")

# Extract rasters BEFORE and AFTER the transition
r_before <- rasters[[year_idx - 1]]
r_after <- rasters[[year_idx]]

# Optional: cumulative / reclassified raster for visual comparison
merged_after <- reclass_cumul_trans[[year_idx]]

# Year-of-change raster (first change)
year_change_r <- years_forest_change$year_rasters[[1]]

# Get coordinates of the chosen cell
xy_chosen <- xyFromCell(r_before, chosen$cell_id)

# Define small zoom window around that cell
zoom_box <- ext(
  xy_chosen[1] - 250,
  xy_chosen[1] + 250,
  xy_chosen[2] - 250,
  xy_chosen[2] + 250
)

# Crop rasters for zoomed visualization
r_before_crop <- crop(r_before, zoom_box)
r_after_crop  <- crop(r_after, zoom_box)
merged_after_crop <- crop(merged_after, zoom_box)
year_change_crop  <- crop(year_change_r, zoom_box)

# Plot all panels
par(mfrow = c(2, 2))

plot(r_before_crop,
     main = paste("Before (", year_before, ")", sep = ""),
     col = c("#32a65e", "#FFFFB2"))
points(xy_chosen, pch = 16, cex = 1.2)

plot(r_after_crop,
     main = paste("After (", year_change, ")", sep = ""),
     col = c("#32a65e", "#FFFFB2"))
points(xy_chosen, pch = 16, cex = 1.2)

plot(merged_after_crop,
     main = paste("Cumulative class (", year_change, ")", sep = ""),
     col = c("#32a65e", "#FFFFB2", "chartreuse", "pink"))
points(xy_chosen, pch = 16, cex = 1.2)

plot(year_change_crop,
     main = "Year of 1st forest change",
     col = terrain.colors(10))
points(xy_chosen, pch = 16, cex = 1.2)

par(mfrow = c(1, 1))


##### 5. COMPUTE FOREST AGE AND TRAJECTORY ----------
# We use the workflow in Silva Junior et al. (2020), Scientific Data
# i) We reclass the rasters into binary 1/0 rasters 
# ii) We compute the age as Age(t)=(Age(t-1)+Forest(t))×Forest(t)
# iii) We reclass forest age

### STEP 1: Reclass rasters
reclass_for_age <- function(r) {
  app(r, fun = function(x) {
    x[x == 1] <- 1
    x[x %in% 2:6] <- 0 # other LULC values
    x
  })
}

# Apply to all rasters
rasters_forest_age <- lapply(rasters, reclass_for_age)

# Check
plot(rasters_forest_age[[1]])
plot(rasters_forest_age[[36]])
freq(rasters_forest_age[[36]])

### STEP 2: Compute age
n <- length(rasters_forest_age)

age_rasters <- vector("list", n)

# Baseline (1989): forest = 1, matrix = 0
age_prev <- rasters_forest_age[[1]] # already 1 for forest
names(age_prev) <- paste0("age_", years[1])
age_rasters[[1]] <- age_prev

# Following years
for (i in 2:n) {
  
  forest_t <- rasters_forest_age[[i]]
  
  # Age rule
  age_t <- (age_prev + 1) * forest_t
  names(age_t) <- paste0("age_", years[i])
  age_rasters[[i]] <- age_t
  age_prev <- age_t
}

# Check
freq(rasters[[1]])
freq(age_rasters[[1]])
plot(age_rasters[[1]], main = years[1])
plot(age_rasters[[2]], main = years[2])
plot(age_rasters[[36]], main = years[36])
freq(age_rasters[[2]]) # there are X forest cells whose age is 1 (= new forests)
freq(reclass_cumul_trans[[1]]) # there should also be X forest cells in category "reforestation" (new forests)

### STEP 3: Reclass age
reclass_age <- function(r) {
  app(r, fun = function(x) {
    x[x == 0] <- 0
    x[x %in% 1:5] <- 5
    x[x %in% 6:15] <- 15
    x[x %in% 16:25] <- 25
    x[x > 25] <- 99
    x
  })
}

# Apply to all rasters
rasters_forest_age_cat <- lapply(age_rasters, reclass_age)

# Check
plot(rasters_forest_age_cat[[1]], main = years[1])
plot(rasters_forest_age_cat[[36]], main = years[36])


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
r_step4 <- crop(years_forest_change$year_rasters[[1]], zoom_ext)

### STEP 5 – Forest age category
r_step5 <- crop(rasters_forest_age_cat[[index_demo]], zoom_ext)

### PLOT

png(here("outputs","plot","01i_tm_lulc_demo.png"), 
    width = 3600, height = 2400, res = 300)

par(mfrow = c(2, 3), mar = c(2, 2, 2, 2))

## STEP 0 – Raw imported
plot(
  r_step0,
  main = paste0("Step 0 – Raw imported (", year_demo, ")"),
  col = c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#d4271e")
)
points(x_center, y_center, pch = 16, col = "red")

## STEP 2 – Cumulative transitions
vals2 <- unique(values(r_step2))
n_classes <- length(vals2[!is.na(vals2)])
plot(
  r_step2,
  main = "Step 2 – Cumulative trajectory code",
  col = brewer.pal(max(3, min(n_classes, 8)), "Pastel1")
)
points(x_center, y_center, pch = 16, col = "red")

## STEP 3 – Re/deforestation classification
plot(
  r_step3,
  main = "Step 3 – Re/deforestation classification",
  col = c(
    "#32a65e",
    "#ad975a",
    "#519799",
    "#FFFFB2",
    "#d4271e",
    "chartreuse",
    "pink"
  )
)
points(x_center, y_center, pch = 16, col = "red")

## STEP 4 – Year of 1st change
vals4 <- unique(values(r_step4))
n_classes <- length(vals4[!is.na(vals4)])
plot(
  r_step4,
  main = "Step 4 – Year of 1st forest change",
  col = brewer.pal(max(3, min(n_classes, 8)), "Greys")
)
points(x_center, y_center, pch = 16, col = "red")

## STEP 5 – Forest age category
plot(
  r_step5,
  main = "Step 5 – Forest age category",
  col = c(
    "grey90",   # 0 = non-forest
    "#c7e9c0",  # 5
    "#74c476",  # 15
    "#238b45",  # 25
    "#00441b"   # 99
  )
)
points(x_center, y_center, pch = 16, col = "red")

par(mfrow = c(1,1))
dev.off()


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

for (i in seq_along(years_forest_change$year_rasters)) {
  
  output_path <- file.path(output_dir, sprintf("raster_year_lulc_change_%02d.tif", i)) # nicely padded index
  message("  - Writing change-event raster #", i)
  
  terra::writeRaster(
    years_forest_change$year_rasters[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT2U", gdal = c("COMPRESS=LZW"))
  )
}

# Export age_rasters
# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_forest_age")

message("Exporting forest age rasters...")

for (i in seq_along(age_rasters)) {
  yr <- years[i]
  
  output_path <- file.path(output_dir, sprintf("raster_forest_age_%d.tif", yr))
  
  message("  - Writing forest age raster for year ", yr)
  
  terra::writeRaster(
    age_rasters[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(
      datatype = "INT2U",                # unsigned integer (0–65535)
      gdal = c("COMPRESS=LZW")
    )
  )
}

# Export rasters_forest_age_cat
# Define output folder
output_dir = here("outputs", "data", "MapBiomas", "Rasters_forest_age_cat")

message("Exporting forest age rasters (age categories)...")

for (i in seq_along(rasters_forest_age_cat)) {
  yr <- years[i]
  
  output_path <- file.path(output_dir, sprintf("raster_forest_age_cat_%d.tif", yr))
  
  message("  - Writing forest age raster (w/ categories) for year ", yr)
  
  terra::writeRaster(
    rasters_forest_age_cat[[i]],
    filename = output_path,
    overwrite = TRUE,
    wopt = list(
      datatype = "INT2U",                # unsigned integer (0–65535)
      gdal = c("COMPRESS=LZW")
    )
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