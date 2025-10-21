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
# Codes:
# 1 = intact forest
# 2 = deforested
# 3 = reforested
# 10 = matrix (non-forest)
# NA = true missing data (no raster information)

# # Starting point (first year)
# traj = rasters[[1]]
# 
# # Assign 10 to all non-forest (matrix) cells, keep NA where raster has no data
# traj[!is.na(traj) & traj != 1] = 10
# plot(traj, main = paste0("Initial (", years[1], ")"))
# 
# # Initialize trajectory list
# trajectories = list()
# trajectories[[1]] = traj
# 
# # Store previous raster
# prev_r = rasters[[1]]
# 
# # Loop over subsequent years
# for (i in 2:length(rasters)) {
#   curr_r = rasters[[i]]
#   
#   # Identify transitions
#   intact   = (prev_r == 1 & curr_r == 1)
#   deforest = (prev_r == 1 & curr_r != 1 & !is.na(curr_r))
#   reforest = (prev_r != 1 & curr_r == 1 & !is.na(curr_r))
#   
#   # Update cumulative trajectory
#   # 1. Reforestation (forest gained) — applies even to deforested or matrix pixels
#   traj[reforest] = 3
#   
#   # 2. Deforestation (forest lost) — applies even to reforested pixels
#   traj[deforest] = 2
#   
#   # 3. Intact forest stays intact if always forest
#   traj[intact & traj == 1] = 1
#   
#   # 4. Non-forest (never forest or currently matrix) = 10
#   traj[!is.na(curr_r) & curr_r != 1 & !deforest & !reforest & traj %in% c(1, 10)] = 10
#   
#   # 5. Preserve true NA (no data)
#   traj[is.na(curr_r)] = NA
#   
#   # Store this year's cumulative trajectory
#   trajectories[[i]] = traj
#   
#   # Update previous raster
#   prev_r = curr_r
# }
# 
# names(trajectories) = years
# 
# # --- Plot examples ---
# cols = c("darkgreen", "red", "blue", "grey")
# plot(trajectories[[2]], col = cols, legend = TRUE, main = paste0("Forest trajectory ", years[2]))
# plot(trajectories[[3]], col = cols, legend = TRUE, main = paste0("Forest trajectory ", years[3]))
# plot(trajectories[[4]], col = cols, legend = TRUE, main = paste0("Forest trajectory ", years[4]))
# plot(trajectories[[length(trajectories)]], col = cols, legend = TRUE, main = paste0("Forest trajectory ", years[length(years)]))



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

# selected_years = c(1989, 2000, 2012, 2023)
# selected_idx = match(selected_years, years)
# 
# cat_labels = c(
#   "0" = "matrix",
#   "1" = "intact forest",
#   "2" = "deforested",
#   "3" = "reforested"
# )
# 
# df_list = list()
# 
# for (i in seq_along(selected_idx)) {
#   
#   if (i == 1) {
#     # BASELINE (same year)
#     r_from = trajectories[[selected_idx[i]]]
#     r_from_filled = classify(r_from, rcl = matrix(c(NA, NA, 0), ncol = 3, byrow = TRUE))
#     
#     vals_from = values(r_from_filled, mat = FALSE)
#     tbl_from = table(factor(vals_from, levels = 0:3))
#     
#     df_baseline = expand.grid(from_cat = 0:3, to_cat = 0:3) %>%
#       dplyr::mutate(from = selected_years[i],
#                     to = selected_years[i],
#                     n = ifelse(from_cat == to_cat, as.numeric(tbl_from)[match(from_cat, 0:3)], 0))
#     
#     df_baseline$from_cat = cat_labels[as.character(df_baseline$from_cat)]
#     df_baseline$to_cat   = cat_labels[as.character(df_baseline$to_cat)]
#     
#     df_list[[i]] = df_baseline
#     
#   } else {
#     # TRANSITION BETWEEN YEARS
#     from_year = selected_years[i - 1]
#     to_year   = selected_years[i]
#     
#     r_from = trajectories[[selected_idx[i - 1]]]
#     r_to   = trajectories[[selected_idx[i]]]
#     
#     # Replace NAs by 0 (matrix)
#     r_from_filled = classify(r_from, rcl = matrix(c(NA, NA, 0), ncol = 3, byrow = TRUE))
#     r_to_filled   = classify(r_to,   rcl = matrix(c(NA, NA, 0), ncol = 3, byrow = TRUE))
#     
#     # Crosstab between consecutive rasters
#     tmat = terra::crosstab(c(r_from_filled, r_to_filled), useNA = FALSE)
#     tmat_df = as.data.frame(tmat)
#     colnames(tmat_df) = c("from_cat", "to_cat", "Freq")
#     
#     # Convert to character to ensure join compatibility
#     tmat_df = tmat_df %>%
#       dplyr::mutate(across(c(from_cat, to_cat), as.character))
#     
#     # Create all possible combinations (0–3 × 0–3)
#     all_combos = expand.grid(from_cat = as.character(0:3), to_cat = as.character(0:3))
#     
#     # Join and fill missing with zeros
#     tmat_full = all_combos %>%
#       dplyr::left_join(tmat_df, by = c("from_cat", "to_cat")) %>%
#       dplyr::mutate(Freq = ifelse(is.na(Freq), 0, Freq))
#     
#     # Add metadata and labels
#     df = tmat_full %>%
#       dplyr::mutate(
#         from = from_year,
#         to = to_year,
#         from_cat = cat_labels[from_cat],
#         to_cat   = cat_labels[to_cat],
#         n = Freq
#       ) %>%
#       dplyr::select(from, to, from_cat, to_cat, n)
#     
#     df_list[[i]] = df
#   }
# }
# 
# # Combine all into one table
# tm_1989_2023 = bind_rows(df_list)
# 
# # Compute area
# pixel_area_ha = prod(res(rasters_merged[[1]])) / 10000
# total_area_ha = ncell(rasters_merged[[1]]) * pixel_area_ha
# 
# tm_1989_2023 = tm_1989_2023 %>%
#   dplyr::mutate(
#     area_ha = n * pixel_area_ha,
#     area_perc = area_ha * 100 / total_area_ha
#   ) %>% 
#   dplyr::filter(n!=0)
# 
# ### Export datasets ------
# base_path = here("outputs", "data", "tm")
# write.csv(tm_1989_2023, file = file.path(base_path, "transition_matrix_bbox_1989_2023.csv"), row.names=FALSE)

### Plot transition matrix ----

## With PantaRhei
# # Reclass land uses
# map_class = function(x) {
#   dplyr::case_when(
#     x == 1 ~ "Forest",
#     x %in% c(3, 5) ~ "Other",
#     TRUE ~ NA_character_
#   )
# }
# 
# tm_clean = transition_matrix %>%
#   dplyr::mutate(
#     y1989 = map_class(year_1989),
#     y2001 = map_class(year_2001),
#     y2013 = map_class(year_2013),
#     y2023 = map_class(year_2023)
#   ) %>%
#   dplyr::filter(!is.na(y1989) & !is.na(y2001) & !is.na(y2013) & !is.na(y2023))
# 
# # Build all trajectory combinations
# tm_clean = tm_clean %>%
#   dplyr::mutate(traj2001 = paste(y1989, y2001, sep="-"),
#                 traj2013 = paste(y1989, y2001, y2013, sep="-"))
# 
# # Flows between key stages
# flows = list(
#   # 1989 -> 2001
#   tm_clean %>%
#     dplyr::count(y1989, y2001) %>%
#     dplyr::mutate(
#       from = paste0(y1989, "_1989"),
#       to = paste0(y1989, "-", y2001, "_2001"),
#       substance = ifelse(y1989 == "Forest" & y2001 == "Other", "Deforestation",
#                          ifelse(y1989 == "Other" & y2001 == "Forest", "Reforestation",
#                                 ifelse(y1989 == y2001 & y1989 == "Forest", "Forest_stay", "Other_stay"))),
#       quantity = n * 0.09
#     ),
#   
#   # 2001 -> 2013
#   tm_clean %>%
#     dplyr::count(traj2001, y2013) %>%
#     dplyr::mutate(
#       from = paste0(traj2001, "_2001"),
#       to = paste0(traj2001, "-", y2013, "_2013"),
#       substance = ifelse(y2013 == "Forest" & grepl("Other$", traj2001), "Reforestation",
#                          ifelse(y2013 == "Other" & grepl("Forest$", traj2001), "Deforestation",
#                                 ifelse(y2013 == "Forest", "Forest_stay", "Other_stay"))),
#       quantity = n * 0.09
#     ),
#   
#   # 2013 -> 2023
#   tm_clean %>%
#     dplyr::count(traj2013, y2023) %>%
#     dplyr::mutate(
#       from = paste0(traj2013, "_2013"),
#       to = paste0(y2023, "_2023"),
#       substance = ifelse(y2023 == "Forest" & grepl("Other$", traj2013), "Reforestation",
#                          ifelse(y2023 == "Other" & grepl("Forest$", traj2013), "Deforestation",
#                                 ifelse(y2023 == "Forest", "Forest_stay", "Other_stay"))),
#       quantity = n * 0.09
#     )
# )
# 
# flows_final = dplyr::bind_rows(flows) %>%
#   dplyr::select(from, to, substance, quantity)
# 
# # Define nodes
# nodes = tibble::tribble(
#   ~ID, ~label, ~label_pos, ~label_align, ~x, ~y, ~dir,
#   # 1989
#   "Forest_1989", "Forest 1989", "left", "", -6, 1.5, "right",
#   "Other_1989", "Other 1989", "left", "", -6, -1.5, "right",
#   # 2001
#   "Forest-Forest_2001", "Forest→Forest 2001", "below", "left", -3.5, 1.5, "right",
#   "Forest-Other_2001", "Forest→Other 2001", "below", "left", -3.5, 0.5, "right",
#   "Other-Other_2001", "Other→Other 2001", "below", "left", -3.5, -1.5, "right",
#   "Other-Forest_2001", "Other→Forest 2001", "below", "left", -3.5, -0.5, "right",
#   # 2013
#   "Forest-Forest-Forest_2013", "FFF 2013", "below", "left", -1, 1.5, "right",
#   "Forest-Forest-Other_2013", "FFO 2013", "below", "left", -1, 0.5, "right",
#   "Other-Other-Other_2013", "OOO 2013", "below", "left", -1, -1.5, "right",
#   "Other-Other-Forest_2013", "OOF 2013", "below", "left", -1, -0.5, "right",
#   "Other-Forest-Other_2013", "OFO 2013", "below", "left", -1, 0, "right",
#   "Forest-Other-Forest_2013", "FOF 2013", "below", "left", -1, 1, "right",
#   # 2023
#   "Forest_2023", "Forest 2023", "right", "", 2, 1.5, "right",
#   "Other_2023", "Other 2023", "right", "", 2, -1.5, "right"
# )
# 
# # Palette
# palette = tibble::tribble(
#   ~substance, ~color,
#   "Forest_stay", "#32a65e",
#   "Other_stay", "#7B68EE",
#   "Reforestation", "#61FA95",
#   "Deforestation", "#D95F02"
# )
# 
# # Style and plot
# ns = list(
#   type="arrow",
#   gp=grid::gpar(fill="#00008B", col="white", lwd=2),
#   length=0.7,
#   label_gp=grid::gpar(col="#00008B", fontsize=9),
#   mag_pos="label",
#   mag_fmt="%.0f ha",
#   mag_gp=grid::gpar(fontsize=9, fontface="bold", col="#00008B")
# )
# 
# title_txt = "Forest and Other trajectories (1989–2023)"
# attr(title_txt, "gp") = grid::gpar(fontsize=16, fontface="bold", col="#00008B")
# 
# PantaRhei::sankey(nodes, flows_final, palette,
#                   node_style = ns,
#                   max_width = 0.1,
#                   rmin = 0.5,
#                   legend = TRUE,
#                   page_margin = c(0.15, 0.05, 0.1, 0.1),
#                   title = title_txt
# )
# 
# # PDF export
# pdf("sankey_forest_trajectories_1989_2023.pdf", width = 13, height = 7)
# PantaRhei::sankey(
#   nodes, flows_final, palette,
#   node_style = ns,
#   max_width = 0.1,
#   rmin = 0.5,
#   legend = TRUE,
#   page_margin = c(0.15, 0.05, 0.1, 0.1),
#   title = title_txt
# )
# dev.off()

