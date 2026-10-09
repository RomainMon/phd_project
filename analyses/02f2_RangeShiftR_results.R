#------------------------------------------------#
# Author: Romain Monassier
# Objective: Analyzing RangeShifter results
#------------------------------------------------#

### Load packages ------
library(here)
library(purrr)
library(ggplot2)
library(readxl)
library(tidyterra)
library(patchwork)
library(viridis)

### Test configuration (path and parameters tested) ----
# Here, name explicitely the folder with the files

# List with components stored
test_config = list(
  test_name = "test_resist_18", # Name of the folder
  
  # Parameters tested with sensitivity analysis (i.e., those varying during simulations)
  parameters = c(
    "IndsHaCell",
    "DensDep"
  )
)

dirpath = here::here(
  "data",
  "rangeshifter",
  "tests",
  test_config$test_name,
  "/"
)

### Import data -----
#### Landscape -----
landsc = terra::rast(file.path(dirpath, "Inputs", "raster_reclass_2005.txt"))
terra::plot(landsc)
terra::res(landsc)
terra::unique(landsc)

#### Patches ----
## Cut patches
patch = terra::rast(file.path(dirpath, "Inputs", "patches_2005.txt"))
terra::res(patch)
# We can have a glimpse at how many cells the different patches contain:
table(terra::values(patch))
# Plot the patches in different colours
npatch = max(terra::values(patch), na.rm = TRUE)
# Create colors: gray for background + random colors for patches
cols = c("gray", rainbow(npatch))
terra::plot(
  patch,
  col = cols,
  type = "classes",
  legend = FALSE)

## Uncut patches
patch_uncut = sf::st_read(here::here("outputs", "data", 
                                     "patches_rshifter", "patches_rshifter_2005.gpkg")) 
plot(patch_uncut)

#### Species distribution -----
patch_w_glt  = terra::rast(file.path(dirpath, "Inputs", "patches_w_glt_2005.txt"))
terra::res(patch_w_glt)
terra::plot(patch_w_glt, col=c("gray","darkgreen"))
terra::unique(patch_w_glt)

#### Population data -----
real_census_data = read_excel(here::here("data", "glt", "JDietz", "GLT_POP_2005_2023.xlsx"), na="NA")
real_census_data %>%
  dplyr::summarise(dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

#### Correspondence table ----
# Correspondence between raster patches ids (used in RangeShifter) and vector patches
patch_corres_id = readr::read_csv(here::here("data", 
                                             "rangeshifter", 
                                             "tests", 
                                             test_config$test_name,
                                             "Inputs", 
                                             "patch_corres_id_2005.csv"),
                                  col_types = readr::cols(uncut_patch_id = readr::col_integer())) # PATCH INTEGER ID HERE

#### Species distribution ----
glt_census = sf::st_read(here::here("data", "glt", "JDietz", "glt_distrib_2013_2018_2022.shp"))
plot(glt_census)

#### Species long-term monitoring -----
regions = sf::st_read(here::here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
plot(regions)

#### Dispersal flux ------
glt_disp = sf::st_read(here::here("outputs", "data", "dispersal", "disp_lines.gpkg")) 
plot(sf::st_geometry(glt_disp))

#### Parameters file ----
# Load an Excel sheet with the parameters to test
metadata = read_excel(here::here("data", 
                                 "rangeshifter", 
                                 "tests", 
                                 test_config$test_name,
                                 "test_parameters.xlsx"),
                      sheet="test1")

# Transform TRUE/FALSE values into logical
metadata$StraightenPath = ifelse(metadata$StraightenPath == "TRUE", TRUE, FALSE)

# Parameters tested
params_tested = test_config$parameters

### Results -----
# RangeShifter can produce eight different types of outputs, or nine for patch-based models.
# All the output files will be named with a standard name reporting the simulation number (here assumed to be 0) and the type of output.
# all the set parameters will be automatically written to a text file Sim0_Parameters.txt.

## Range: 
# - Total number of individuals (NInds) 
# - Total number of individuals in each stage (NInd_stageX); these columns will be present only in case of stage-structured models; 
# - Total number of juveniles born (NJuvs); only in case of stage-structured models
# - Total number of cells (NOccupCells) or total number of patches (NOccupPatches) occupied  by a population capable of breeding: for a stage-structured population, individuals of the breeding stage(s) must be present, and for a sexual model, both sexes must be present 
# - Ratio between occupied and suitable cells or patches (OccupSuit) 
# - Species’ range, in term of maximum and minimum coordinates (min_X, max_X, min_Y,  max_Y) of cells / patches occupied by breeding populations (as above)

## Occupancy files: This output reports the cell / patch probability of occupancy by a breeding population. This is only permissible for multiple replicates.
# - Occupancy.txt: This file contains a list of all the cells in the landscape (x and y  coordinates) or of all the patches (PatchID). The remaining columns give the occupancy probability of the cell / patch at defined time steps. The occupancy probability is obtained by dividing the number of times (replicates) that the cell / patch has been occupied in a given year by the total number of replicates.
# - Occupancy_stats.txt: Summary occupancy statistics, i.e. the mean ratio between  occupied and suitable cells (Mean_OccupSuit) and its standard error (Std_error) at the set time interval.

## Population files: statistics regarding each population present in the landscape at a given time interval.
# - Species number (Species); not yet used, always zero 
# - Number of individuals in the population (NInd). In the case of a stage-structured population, the number of individuals in each stage (NInd_stageX). If the reproduction is sexual, these columns will be replaced by the number of females (Nfemales_stageX) and of males (Nmales_stageX) in each stage. In the case of sexual model without stage structure, two columns will indicate the number of females (Nfemales) and of males (Nmales) in the population. 
# - In the case of a stage-structured population, the number of juveniles born (NJuvs). If the  reproduction is sexual, these columns will be replaced by the number of females juveniles (NJuvFemales) and males (NJuvMales).

## Individuals

## Genetics

## Traits

## Connectivity matrix: available for a patch-based model only. It presents counts of the number of individuals successfully dispersing from each patch to each other patch for each year
# - ID number of natal patch (StartPatch)
# - ID number of settlement patch (EndPatch)
# - Number of individuals dispersing from StartPatch to EndPatch (NInds)
# The rows having an entry of -999 are summary rows, showing the total number of successful emigrants from a patch (if EndPatch = -999) and the total number of successful immigrants into a patch (if StartPatch = -999).

## Heatmaps
# When the transfer model is SMS, an additional optional output is a series of maps showing how many times each matrix cell (i.e. cells in the landscape which are not suitable for breeding) has been visited by a dispersing individual across the whole time period of the model. 
# These heat maps may be useful, for example, for identifying corridors which are heavily used during the dispersal phase.

## Log files
# When running in batch mode, an additional output file BatchNN_RS_log.csv (where NN is the batch number) will be created automatically. In it is listed the time taken (in seconds) to run each simulation in the batch.

#### Population size -----

##### Pop files -----
# stack all files
pop_files = list.files(
  here::here("data",
             "rangeshifter",
             "tests",
             test_config$test_name,
             "Outputs"),
  pattern = "_Pop\\.txt$", # Population files
  full.names = TRUE
)

# Read and stack all pop files
pop_all = purrr::map_dfr(pop_files, function(f){
  
  # Simulation id
  sim_id = stringr::str_extract(
    basename(f),
    "(?<=Sim)\\d+(?=_Land)"
  ) %>%  as.numeric()
  
  # Read pop file
  read.table(
    f,
    header = TRUE,
    sep = "\t"
  ) %>%
    dplyr::mutate(Id_simul = sim_id)
  
})

# Take a look at the dataset
dplyr::glimpse(pop_all)

### WARNING: pop_all records pop size in patches WHENEVER one patch becomes occupied, not before
#i.e. Patch A: Year 1–7 → no row
# VS Year 8 → individuals
## We complete the dataset
pop_all_complete = pop_all %>% 
  dplyr::select(-c(RepSeason, Ncells, Species)) %>% # Remove unnecessary columns
  tidyr::complete(
    PatchID,
    Id_simul,
    Rep,
    Year,
    fill = list(
      NInd = 0,
      NInd_stage1 = 0,
      NInd_stage2 = 0,
      NJuvs = 0
    )
  )
dplyr::glimpse(pop_all_complete)

# ## Quick check: Patch 138 before and after completing the dataset
# # Original data
# check_138_original = pop_all %>% 
#   dplyr::filter(PatchID == 138) %>% 
#   dplyr::summarise(
#     n_simulations = dplyr::n_distinct(Id_simul),
#     n_reps = dplyr::n_distinct(Rep),
#     n_years = dplyr::n_distinct(Year),
#     n_rows = dplyr::n(),
#     min_year = min(Year),
#     max_year = max(Year)
#   )
# 
# # Completed data
# check_138_complete = pop_all_complete %>% 
#   dplyr::filter(PatchID == 138) %>% 
#   dplyr::summarise(
#     n_simulations = dplyr::n_distinct(Id_simul),
#     n_reps = dplyr::n_distinct(Rep),
#     n_years = dplyr::n_distinct(Year),
#     n_rows = dplyr::n(),
#     min_year = min(Year),
#     max_year = max(Year)
#   )
# 
# check_138_original
# check_138_complete
# 
# # Visual inspection
# pop_all_complete %>% 
#   dplyr::filter(PatchID == 138) %>% 
#   dplyr::arrange(Id_simul, Rep, Year)

### Join tested parameter
pop_all_complete = pop_all_complete %>% 
  dplyr::left_join(metadata, by = "Id_simul")

##### Pop dynamics -----
# ATTENTION: 
# IF female-only model: the population only includes females -> multiply by 2 to estimate the total population
# IF sex model, the population includes both males and females

# Population size by simulation
pop_total = pop_all_complete %>%
  dplyr::group_by(Id_simul, 
                  dplyr::across(dplyr::all_of(params_tested)),
                  Rep, 
                  Year) %>%
  dplyr::summarise(NInd = sum(NInd)*2, # N Adults (Stage 1 + Stage 2)
                   NJuvs = sum(NJuvs)*2, # N Juveniles
                   NTot = (NInd+NJuvs)*2, # Total population
                   .groups = "drop") %>% 
  dplyr::filter(Year != max(Year, na.rm = TRUE)) # Remove the last Year

# Mean population size over time (by tested parameter)
pop_time = pop_total %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(params_tested)),
                  Year) %>%
  dplyr::summarise(MeanN_Ind = mean(NInd), 
                   MeanN_Juvs = mean(NJuvs),
                   MeanN_Tot = mean(NTot),
                   .groups = "drop")

# Plot evolution of population size
ggplot(
  pop_time,
  aes(
    x = Year,
    y = MeanN_Tot, # Choose the proper count
    colour = factor(.data[[params_tested[1]]])
  )
) +
  geom_line(linewidth = 1) +
  # Faceting
  facet_grid(
    cols = vars(.data[[params_tested[2]]]),
    # rows = vars(.data[[params_tested[3]]]),
    scales = "free_y") +
  # Vertical reference years
  geom_vline(
    xintercept = c(0, 8, 17),
    linetype = "dashed",
    colour = "black"
  ) +
  annotate("point", x = 0,  y = 1600, size = 3) +
  annotate("point", x = 8, y = 3706, size = 3) +
  annotate("point", x = 17, y = 4869, size = 3) +
  annotate("text", x = 0,  y = 1600, label = "2005", vjust = -1) +
  annotate("text", x = 8, y = 3706, label = "2013", vjust = -1) +
  annotate("text", x = 17, y = 4869, label = "2022", vjust = -1) +
  theme_bw() +
  labs(y = "Mean population size (total)")

## Export plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "evol_pop.png"),
    width = 3000, height = 1500, res = 300, type="cairo")
# Plot
ggplot(
  pop_time,
  aes(
    x = Year,
    y = MeanN_Tot,
    colour = factor(.data[[params_tested[1]]])
  )
) +
  geom_line(linewidth = 1) +
  # Faceting
  facet_grid(
    cols = vars(.data[[params_tested[2]]]),
    # rows = vars(.data[[params_tested[3]]]),
    scales = "free_y") +
  # Vertical reference years
  geom_vline(
    xintercept = c(0, 8, 17),
    linetype = "dashed",
    colour = "black"
  ) +
  annotate("point", x = 0,  y = 1600, size = 3) +
  annotate("point", x = 8, y = 3706, size = 3) +
  annotate("point", x = 17, y = 4869, size = 3) +
  annotate("text", x = 0,  y = 1600, label = "2005", vjust = -1) +
  annotate("text", x = 8, y = 3706, label = "2013", vjust = -1) +
  annotate("text", x = 17, y = 4869, label = "2022", vjust = -1) +
  theme_bw() +
  labs(y = "Mean population size (total)") # UPDATE according to indicator

dev.off()

##### Comparison with real data -----

### Identify good parameters by comparing with real pop
pop2005 = 1600 # Adjust
pop2013 = 3706 # Adjust
pop2022 = 4869 # Adjust

## RMSE
# Note: RMSE is frequently used to assess differences between predicted and real values
# Calculation: square root of the mean of the squares of the deviations

## Based on 2005, 2013 and 2022 populations sizes
fit_score = pop_time %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(params_tested))) %>%
  dplyr::summarise(
    N0 = MeanN_Tot[Year == 0],
    N8 = MeanN_Tot[Year == 8],
    N17 = MeanN_Tot[Year == 17],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    RMSE = sqrt(
      ((N0 - pop2005)^2 +
         (N8 - pop2013)^2 +
         (N17 - pop2022)^2) / 3
    ))
# Rank the best options
fit_score %>%
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1:10)

## Heatmap plot
ggplot(
  fit_score, # Fit scores
  aes(
    x = .data[[params_tested[1]]],
    y = .data[[params_tested[2]]],
    fill = RMSE # Indicator to map
  )
) +
  geom_tile() +
  # facet_wrap(vars(.data[[params_tested[3]]])) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1
  ) +
  theme_bw()

## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         test_config$test_name,
         "plot",
         "heatmap_popsize.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
# Plot
ggplot(
  fit_score, # Fit scores
  aes(
    x = .data[[params_tested[1]]],
    y = .data[[params_tested[2]]],
    fill = RMSE # Indicator to map
  )
) +
  geom_tile() +
  # facet_wrap(vars(.data[[params_tested[3]]])) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1
  ) +
  theme_bw()

dev.off()

#### Individuals ----
##### Inds files -----
# stack all files
inds_files = list.files(
  here::here("data",
             "rangeshifter",
             "tests",
             test_config$test_name,
             "Outputs"),
  pattern = "_Inds\\.txt$", # Connectivity files
  full.names = TRUE
)

# Read and stack all pop files
inds_all = purrr::map_dfr(inds_files, function(f){
  
  # Simulation id
  sim_id = stringr::str_extract(
    basename(f),
    "(?<=Sim)\\d+(?=_Land)"
  ) %>%  as.numeric()
  
  # Read pop file
  read.table(
    f,
    header = TRUE,
    sep = "\t"
  ) %>%
    dplyr::mutate(Id_simul = sim_id)
  
})

# Take a look at the dataset
dplyr::glimpse(inds_all)

# Unique values in the Status column
unique(inds_all$Status)
# Status (Bocedi et al. 2026, User Manual)
# 0 Initial status in natal patch / philopatric recruit 
# 1 Disperser 
# 2 Disperser awaiting settlement in possible suitable patch 
# 3 Waiting between dispersal events 
# 4 Completed settlement 
# 5 Completed settlement in a suitable neighbouring cell/patch 
# 6 Died during transfer by failing to find a suitable patch (includes exceeding the  maximum number of steps or crossing an absorbing boundary) 
# 7 Died during transfer by constant, step-dependent, habitat-dependent or  distance-dependent mortality 
# 8 Failed to survive annual (demographic) mortality 
# 9 Exceeded maximum age 
# 10 Translocated individual

## Take a look at an individual
# Randomly select one Rep x Id_simul combination
selected_sim = inds_all %>%
  dplyr::distinct(Rep, Id_simul) %>%
  dplyr::slice_sample(n = 1)
# Randomly select one individual within that combination
selected_ind = inds_all %>%
  dplyr::semi_join(
    selected_sim,
    by = c("Rep", "Id_simul")
  ) %>%
  dplyr::distinct(IndID, Rep, Id_simul) %>%
  dplyr::slice_sample(n = 1)
# Print the complete history of this individual
inds_all %>%
  dplyr::semi_join(
    selected_ind,
    by = c("IndID", "Rep", "Id_simul")
  ) %>%
  dplyr::arrange(Year)


### Join tested parameter
inds_all = inds_all %>% 
  dplyr::left_join(metadata, by = "Id_simul")

##### Individual status by patch -----
# To complete
# Identify births, deaths, immigrations, emigrations

##### Select dispersal events -----

# Select dispersal events
# Dispersal events are individuals with Status 1/2 (dispersing), 4 (settled), 6 or 7 (died in transfer)
# Nsteps: Number of steps taken
# NOTE: In movement models, individuals take single-cell steps basing their decisions on three parameters: their perceptual range (PR), the method used to evaluate the landscape within their perceptual range and their directional persistence (DP), which corresponds to their tendency to follow a correlated random walk.
dispersers_all = inds_all %>% 
  dplyr::filter(
    Nsteps > 0 # Each time an individual disperses, the number of step is recorded
  ) %>% 
  dplyr::mutate(
    DispersalEventID = dplyr::row_number()
  )

##### Dispersal characteristics -----
# We distinguish between successful intra- and inter-patch dispersal and died during transfer

# NOTE: We use UNCUT patches as our definition of patches here
# We join UNCUT patches information
dispersers_all = dispersers_all %>% 
  dplyr::left_join(patch_corres_id, by=c("Natal_patch"="cut_patch_id")) %>% 
  dplyr::rename(Orig_patch_name = uncut_patch_name,
                Orig_patch_id = uncut_patch_id) %>% 
  dplyr::left_join(patch_corres_id, by=c("PatchID"="cut_patch_id")) %>% 
  dplyr::rename(Dest_patch_name = uncut_patch_name,
                Dest_patch_id = uncut_patch_id) %>%
  dplyr::select(-c(Orig_patch_id, Dest_patch_id))

# When there is no patch correspondence (NA), it means individuals are in the Matrix (PatchID = 0 in the RS files)
# Therefore, we mutate "Matrix" wherever we find a 0
dispersers_all = dispersers_all %>% 
  dplyr::mutate(Dest_patch_name = ifelse(is.na(Dest_patch_name), "Matrix", Dest_patch_name))

# Mutate a variable for intra- and inter-patch dispersal
dispersers_all = dispersers_all %>% 
  dplyr::mutate(Disp_type = dplyr::case_when(
    Orig_patch_name == Dest_patch_name ~ "Intra-patch dispersal",
    Orig_patch_name != Dest_patch_name & Dest_patch_name != "Matrix" ~ "Inter-patch dispersal",
    Orig_patch_name != Dest_patch_name & Dest_patch_name == "Matrix" ~ "Died in matrix"
  ))

# Quick check
dispersers_all %>% 
  dplyr::select(Rep, Year, IndID, Id_simul, Status, Orig_patch_name, Dest_patch_name, Disp_type)

# Intersections between Status and Dispersal type
dispersers_all %>% 
  dplyr::count(Status, Disp_type)

###### Dispersal success ----
disp_success = dispersers_all %>% 
  dplyr::group_by(
    Id_simul,
    dplyr::across(dplyr::all_of(params_tested)),
    Rep
  ) %>% 
  dplyr::summarise(
    n_dispersal = dplyr::n(), # number of dispersal events
    Interpatch_disp = sum(Disp_type == "Inter-patch dispersal"), # number of successful inter-patch dispersal events
    Intrapatch_disp = sum(Disp_type == "Intra-patch dispersal"), # number of intra-patch dispersal events
    Disp_failure = sum(Disp_type == "Died in matrix"), # number of failed dispersal events
    prop_intra = Intrapatch_disp / n_dispersal, # % of successful intra-patch disp (VS the total number of disp events)
    prop_inter = Interpatch_disp / n_dispersal, # % of successful inter-patch disp (VS the total number of disp events)
    prop_fail = Disp_failure / n_dispersal,  # % of failed disp (VS the total number of disp events)
    .groups = "drop"
  ) %>% 
  dplyr::group_by(
    Id_simul,
    dplyr::across(dplyr::all_of(params_tested))
  ) %>% 
  # We compute mean proportion across the replicates (by simulation id)
  dplyr::summarise(
    mean_prop_intra = round(mean(prop_intra, na.rm = TRUE),2),
    mean_prop_inter = round(mean(prop_inter, na.rm = TRUE),2),
    mean_prop_fail = round(mean(prop_fail, na.rm=TRUE),2),
    .groups = "drop"
  )

### Barplot
disp_plot = disp_success %>% 
  dplyr::select(
    Id_simul,
    dplyr::all_of(params_tested),
    mean_prop_intra,
    mean_prop_inter,
    mean_prop_fail
  ) %>% 
  tidyr::pivot_longer(
    cols = c(mean_prop_intra, mean_prop_inter, mean_prop_fail),
    names_to = "Disp_type",
    values_to = "Proportion"
  ) %>% 
  dplyr::mutate(
    Disp_type = dplyr::recode(
      Disp_type,
      mean_prop_intra = "Intra-patch dispersal",
      mean_prop_inter = "Inter-patch dispersal",
      mean_prop_fail = "Died in the matrix"
    ),
    Label = paste0(round(Proportion * 100, 1), "%")
  )

## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         test_config$test_name,
         "plot",
         "barplot_disp_success.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
# Plot
ggplot2::ggplot(
  disp_plot,
  ggplot2::aes(
    x = factor(Id_simul),
    y = Proportion,
    fill = Disp_type
  )
) +
  ggplot2::geom_col() +
  ggplot2::geom_text(
    ggplot2::aes(label = Label),
    position = ggplot2::position_stack(vjust = 0.5),
    colour = "white",
    fontface = "bold",
    size = 3
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent
  ) +
  ggplot2::labs(
    x = "Simulation (Id_simul)",
    y = "Proportion of dispersal events",
    fill = "Dispersal type"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )
dev.off()

###### Intra- and inter-patch dispersal ----
# Here, we focus on SUCCESSFUL dispersal events
disp_proportions = dispersers_all %>% 
  dplyr::filter(Disp_type != "Died in matrix") %>% # We remove the dispersal failures (individuals dying)
  dplyr::group_by(
    Id_simul,
    dplyr::across(dplyr::all_of(params_tested)),
    Rep
  ) %>% 
  dplyr::summarise(
    n_dispersal = dplyr::n(), # number of successful dispersal events
    n_intra = sum(Disp_type == "Intra-patch dispersal", na.rm = TRUE), # number of successful intra-patch dispersal events
    n_inter = sum(Disp_type == "Inter-patch dispersal", na.rm = TRUE), # number of successful inter-patch dispersal events
    prop_intra = n_intra / n_dispersal,
    prop_inter = n_inter / n_dispersal,
    .groups = "drop"
  ) %>% 
  dplyr::group_by(
    Id_simul,
    dplyr::across(dplyr::all_of(params_tested))
  ) %>% 
  # We compute mean proportion across the replicates
  dplyr::summarise(
    mean_prop_intra = round(mean(prop_intra, na.rm = TRUE),2),
    mean_prop_inter = round(mean(prop_inter, na.rm = TRUE),2),
    .groups = "drop"
  )

## Compare with real dispersal data
# Expected distribution from Ponchon et al. (2026)
expected_prop = c(
  "Intra-patch dispersal" = 3.6 / (3.6 + 0.4),
  "Inter-patch dispersal" = 0.4 / (3.6 + 0.4)
)

# RMSE
disp_proportions = disp_proportions %>% 
  dplyr::mutate(
    diff_disp_intra = round(abs(
      mean_prop_intra - expected_prop["Intra-patch dispersal"]
    ),2)
  )
# Rank the best options
disp_proportions %>%
  dplyr::arrange(diff_disp_intra)
# Best option
disp_proportions %>%
  dplyr::arrange(diff_disp_intra) %>%
  dplyr::slice(1) # Select with slice(X) the row to keep

### Heatmap
## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         test_config$test_name,
         "plot",
         "heatmap_dispintra.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
# Plot
ggplot(
  disp_proportions, # Fit scores
  aes(
    x = .data[[params_tested[1]]],
    y = .data[[params_tested[2]]],
    fill = diff_disp_intra # Indicator to map
  )
) +
  geom_tile() +
  # facet_wrap(vars(.data[[params_tested[3]]])) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1
  ) +
  theme_bw()

dev.off()

### Barplot
disp_plot = disp_proportions %>% 
  dplyr::select(
    Id_simul,
    dplyr::all_of(params_tested),
    mean_prop_intra,
    mean_prop_inter
  ) %>% 
  tidyr::pivot_longer(
    cols = c(mean_prop_intra, mean_prop_inter),
    names_to = "Disp_type",
    values_to = "Proportion"
  ) %>% 
  dplyr::mutate(
    Disp_type = dplyr::recode(
      Disp_type,
      mean_prop_intra = "Intra-patch dispersal",
      mean_prop_inter = "Inter-patch dispersal"
    ),
    Label = paste0(round(Proportion * 100, 1), "%")
  )

# Add observed/reference distribution
expected_plot = data.frame(
  Id_simul = "Observed",
  Disp_type = names(expected_prop),
  Proportion = as.numeric(expected_prop)
) %>% 
  dplyr::mutate(
    Label = paste0(round(Proportion * 100, 1), "%")
  )

# Combine simulations + observed
disp_plot$Id_simul = as.character(disp_plot$Id_simul)
disp_plot = dplyr::bind_rows(
  disp_plot,
  expected_plot
)

## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         test_config$test_name,
         "plot",
         "barplot_disp_events.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
# Plot
ggplot2::ggplot(
  disp_plot,
  ggplot2::aes(
    x = factor(Id_simul),
    y = Proportion,
    fill = Disp_type
  )
) +
  ggplot2::geom_col() +
  ggplot2::geom_text(
    ggplot2::aes(label = Label),
    position = ggplot2::position_stack(vjust = 0.5),
    colour = "white",
    fontface = "bold",
    size = 3
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent
  ) +
  ggplot2::labs(
    x = "Simulation (Id_simul)",
    y = "Proportion of dispersal events",
    fill = "Dispersal type"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )
dev.off()

###### Dispersal distance -----
disp_dist = dispersers_all %>% 
  dplyr::filter(
    Disp_type != "Died in matrix" # Remove failed dispersal events
  )

# Mean dispersal distance across simulations
mean_dist = disp_dist %>% 
  dplyr::group_by(
    Id_simul,
    Disp_type
  ) %>% 
  dplyr::summarise(
    mean_dist = mean(DistMoved, na.rm = TRUE),
    .groups = "drop"
  )
# In wide format
mean_dist_wide = mean_dist %>% 
  tidyr::pivot_wider(values_from = mean_dist,
                     names_from = Disp_type) %>% 
  dplyr::rename(mean_disp_dist_inter = "Inter-patch dispersal",
                mean_disp_dist_intra = "Intra-patch dispersal")
## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         test_config$test_name,
         "plot",
         "hist_disp_dist.png"),
    width = 2000, height = 2000, res = 300, type="cairo")
# Plot
ggplot2::ggplot(
  disp_dist,
  ggplot2::aes(x = DistMoved)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    bins = 30,
    fill = "grey70",
    colour = "white"
  ) +
  ggplot2::geom_density(
    linewidth = 0.8
  ) +
  ggplot2::geom_vline(
    data = mean_dist,
    ggplot2::aes(xintercept = mean_dist),
    linetype = "dotted",
    linewidth = 0.8
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(Id_simul),
    cols = ggplot2::vars(Disp_type)
  ) +
  ggplot2::labs(
    x = "Dispersal distance (m)",
    y = "Density"
  ) +
  ggplot2::theme_bw()
dev.off()

#### Fit the best score -----
# Join dispersal flux
fit_score_pop_disp = fit_score %>%
  dplyr::left_join(disp_proportions) %>% 
  dplyr::left_join(mean_dist_wide)
# Best option
best_fit = fit_score_pop_disp %>%
  dplyr::arrange(RMSE, diff_disp_intra) %>% # Choose the indicator variables
  dplyr::slice(1) # Select with slice(X) the row to keep
best_fit
# Extract best values
best_values = best_fit %>%
  dplyr::select(dplyr::all_of(params_tested))
# Id_simul associated with the best combination of parameters
best_sim = best_fit %>% 
  dplyr::pull(Id_simul) %>% 
  unique()

#### Range -----
# We check patch occupancy using the best values
### Number of simulated patches occupied
# Using the Range file

##### Range files -------
# stack all files
range_files = list.files(
  here::here("data",
             "rangeshifter",
             "tests",
             test_config$test_name,
             "Outputs"),
  pattern = "_Range\\.txt$", # Population files
  full.names = TRUE
)

# Read and stack all pop files
range_all = purrr::map_dfr(range_files, function(f){
  
  # Simulation id
  sim_id = stringr::str_extract(
    basename(f),
    "(?<=Sim)\\d+(?=_Land)"
  ) %>%  as.numeric()
  
  # Read pop file
  read.table(
    f,
    header = TRUE,
    sep = "\t"
  ) %>%
    dplyr::mutate(Id_simul = sim_id)
  
})

# Take a look at the dataset
dplyr::glimpse(range_all)

### Join tested parameter
range_all = dplyr::left_join(
  range_all,
  metadata,
  by = "Id_simul"
)

### Parameters tested
params_tested = test_config$parameters

##### Range dynamics -----
# Mean occupancy over time (by tested parameter)
range_time = range_all %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(params_tested)),
                  Year) %>%
  dplyr::summarise(MeanNOccupPatches = mean(NOccupPatches),
                   .groups = "drop")

# Plot evolution of range
ggplot(
  range_time,
  aes(
    x = Year,
    y = MeanNOccupPatches, # Choose the proper parameter
    colour = factor(.data[[params_tested[1]]])
  )
) +
  geom_line(linewidth = 1) +
  # Faceting
  facet_grid(
    cols = vars(.data[[params_tested[2]]]),
    # rows = vars(.data[[params_tested[3]]]),
    scales = "free_y") +
  theme_bw() +
  labs(y = "Mean number of occupied patches")

## Export plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "evol_occ_patches.png"),
    width = 3000, height = 1500, res = 300, type="cairo")
# Plot
ggplot(
  range_time,
  aes(
    x = Year,
    y = MeanNOccupPatches, # Choose the proper parameter
    colour = factor(.data[[params_tested[1]]])
  )
) +
  geom_line(linewidth = 1) +
  # Faceting
  facet_grid(
    cols = vars(.data[[params_tested[2]]]),
    # rows = vars(.data[[params_tested[3]]]),
    scales = "free_y") +
  theme_bw() +
  labs(y = "Mean number of occupied patches")

dev.off()

# Number of occupied patches
patch_occ_2100 = range_time %>% 
  dplyr::filter(Year == 90)

#### Patch abundance and connectivity -----
##### Vector patches -----
patch_v = terra::as.polygons(patch) # Vectorize RangeShifter raster patches
patch_sf = sf::st_as_sf(patch_v) %>% 
  dplyr::rename(PatchID = id) %>% 
  dplyr::filter(PatchID != 0)  # Remove matrix

##### List of patches used for simulations -----
# names of the patches used for simulations (until "test_patch_cut_1")
# i.e., patches irrespective of forest elevation
# patch_list = c("Aldeia_I_1",
#                "Aldeia_I_2",
#                "Sta_Helena",
#                "Imbau_I_2",
#                "Afetiva",
#                "Nova_Esperanca_2",
#                "Pirineus_111",
#                "Poco_das_Antas",
#                "Rio_Vermelho",
#                "Uniao_N_2")

# names of the patches used for simulations
# i.e., patches of forests <500 m
patch_list = c("Vendaval",
               "Rio_Vermelho",
               "Boa_Esperanca_II",
               "Imbau_I_2",
               "Sta_Helena_II",
               "Sta_Helena_I",
               "Sta_Helena",
               "Aldeia_I_1",
               "Aldeia_I_2",
               "Poco_das_Antas",
               "Uniao_N_2",
               "Nova_Esperanca_2",
               "Afetiva",
               "Pirineus_114")

##### Population by patch -----
# ATTENTION: 
# IF female-only model: the population only includes females -> multiply by 2 to estimate the total population
# IF sex model, the population includes both males and females

pop_patch = pop_all_complete %>%
  # We focus on the best parameters
  dplyr::semi_join(best_values,
                   by = params_tested) %>%
  dplyr::filter(Year != max(Year, na.rm = TRUE), # Remove the last year
                PatchID != 0) %>% # Remove the matrix 
  dplyr::group_by(PatchID,
                  Year) %>%
  # Mean population size by patch/year across replicates
  dplyr::summarise(MeanNInd = round(mean(NInd)*2,0), # N Adults (Stage 1 + Stage 2)
                   MeanNJuvs = round(mean(NJuvs)*2,0), # N Juveniles
                   MeanNTot = round(mean(NInd+NJuvs)*2,0), # Total population
                   .groups = "drop")

# Complete missing patch/year combinations
# i.e., some patches have NAs because in the original Pop files, they are discarded if they never have populations
pop_patch = pop_patch %>% 
  tidyr::complete(
    PatchID = patch_sf$PatchID,
    Year,
    fill = list(
      MeanNInd = 0,
      MeanNJuvs = 0,
      MeanNTot = 0
    )
  )

# Join REAL patch names
pop_patch = pop_patch %>% 
  dplyr::inner_join(patch_corres_id, # Table with correspondence between the RangeShifter patches and the REAL patches
                    by=c("PatchID" = "cut_patch_id")) %>% # Update the variable in the correspondence table to make the correspondence
  dplyr::rename(patch_id = uncut_patch_name) # patch_id is the REAL patch id
dplyr::glimpse(pop_patch)

# # Check -> all patches should have the same number of years (i.e., years of simulations)
# pop_patch %>% 
#   dplyr::group_by(PatchID) %>% 
#   dplyr::summarise(n_rep = dplyr::n_distinct(Year))

##### Patch occupancy -----
# Join population size to spatial patches
patch_sf_pop = patch_sf %>% 
  dplyr::left_join(
    pop_patch,
    by = "PatchID"
  )

### Proportions of patches occupied
# We compare the number of patches occupied in simulations with the number of patches occupied in the census dataset
# Comparison with the 2013 census
sim_2013 = patch_sf_pop %>%
  # Select the year of interest
  dplyr::filter(Year == 8) %>%
  # Mutate the occupancy state
  dplyr::mutate(
    sim_occupied = dplyr::case_when(MeanNTot == 0 ~ "Unoccupied",
                                    MeanNTot > 0 ~ "Occupied")
  ) %>%
  dplyr::select(
    PatchID, # RangeShifter patch id
    Year,
    sim_occupied,
    geometry
  )

# Census data
census_2013 = glt_census %>%
  # Select year of interest
  dplyr::filter(!is.na(Detect2013)) %>%
  dplyr::select(
    ID2, # Transect id
    Detect2013, # Detection data
    geometry
  )

# Spatial join
comparison_2013 = sf::st_intersection(
  census_2013,
  sim_2013
  ) %>%
  # Compute the intersection area between the transect and the patch
  dplyr::mutate(
    overlap_area = as.numeric(sf::st_area(geometry)) # Intersection area
  ) %>%
  dplyr::group_by(ID2) %>%
  # Some census data overlap several patches (and vice-versa)
  # HENCE: we keep the largest intersection between the patch and the census polygon
  dplyr::slice_max(
    order_by = overlap_area,
    n = 1,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup() %>%
  sf::st_drop_geometry()

# We compute the proportion of patches occupied in 2013 and also occupied in simulations
# i.e. true positive: observed P, simulated occupied
# AND the proportion of patches unoccupied in 2013 and also unoccupied in simulations
# i.e. true negative: observed N, simulated unoccupied
comparison_2013 = comparison_2013 %>% 
  dplyr::mutate(
    match_pres = dplyr::case_when(
      Detect2013 == "P" & sim_occupied == "Occupied"   ~ "MATCH",
      Detect2013 == "P" & sim_occupied == "Unoccupied" ~ "UNMATCH"
    ),
    match_abs = dplyr::case_when(
      Detect2013 == "N" & sim_occupied == "Unoccupied" ~ "MATCH",
      Detect2013 == "N" & sim_occupied == "Occupied"   ~ "UNMATCH"
    )
  )

# Proportion
prop_match_2013 = comparison_2013 %>%
  dplyr::summarise(
    n_occ = sum(Detect2013 == "P"), # Number of occurrences
    n_match_pres = sum(match_pres == "MATCH", na.rm = TRUE), # Number of true positives
    n_unmatch_pres = sum(match_pres == "UNMATCH", na.rm = TRUE), # Number of false negatives
    prop_match_pres = round(n_match_pres / n_occ, 2), # % true positive
    n_abs = sum(Detect2013 == "N"), # Number of absences
    n_match_abs = sum(match_abs == "MATCH", na.rm = TRUE), # Number of true negatives
    n_unmatch_abs = sum(match_abs == "UNMATCH", na.rm = TRUE), # Number of false positives
    prop_match_abs = round(n_match_abs / n_abs, 2) # % true negatives
  ) %>% 
  dplyr::rename_with(~ paste0(., '_2013'))

## Comparison with 2022
# Comparison with the 2013 census
sim_2022 = patch_sf_pop %>%
  # Select the year of interest
  dplyr::filter(Year == 17) %>%
  dplyr::mutate(
    sim_occupied = dplyr::case_when(MeanNTot == 0 ~ "Unoccupied",
                                    MeanNTot > 0 ~ "Occupied")
  ) %>%
  dplyr::select(
    PatchID,
    Year,
    sim_occupied,
    geometry
  )

# Census data
census_2022 = glt_census %>%
  dplyr::filter(!is.na(Detect2022)) %>%
  dplyr::select(
    ID2,
    Detect2022,
    geometry
  )

# Spatial join
comparison_2022 = sf::st_intersection(
  census_2022,
  sim_2022) %>%
  dplyr::mutate(
    overlap_area = as.numeric(sf::st_area(geometry)) # Intersection area
  ) %>%
  dplyr::group_by(ID2) %>%
  # Keep the patch with the largest overlap with the census polygon
  dplyr::slice_max(
    order_by = overlap_area,
    n = 1,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup() %>%
  sf::st_drop_geometry()

# We compute the proportion of patches occupied in 2022 but not occupied in simulations (false negative)
# AND the proportion of patches unoccupied in 2022 but occupied in simulations (false positive)
comparison_2022 = comparison_2022 %>% 
  dplyr::mutate(
    match_pres = dplyr::case_when(
      Detect2022 == "P" & sim_occupied == "Occupied"   ~ "MATCH",
      Detect2022 == "P" & sim_occupied == "Unoccupied" ~ "UNMATCH"
    ),
    match_abs = dplyr::case_when(
      Detect2022 == "N" & sim_occupied == "Unoccupied" ~ "MATCH",
      Detect2022 == "N" & sim_occupied == "Occupied"   ~ "UNMATCH"
    )
  )

# Proportion
prop_match_2022 = comparison_2022 %>%
  dplyr::summarise(
    n_occ = sum(Detect2022 == "P"),
    n_match_pres = sum(match_pres == "MATCH", na.rm = TRUE),
    n_unmatch_pres = sum(match_pres == "UNMATCH", na.rm = TRUE),
    prop_match_pres = round(n_match_pres / n_occ, 2),
    n_abs = sum(Detect2022 == "N"),
    n_match_abs = sum(match_abs == "MATCH", na.rm = TRUE),
    n_unmatch_abs = sum(match_abs == "UNMATCH", na.rm = TRUE),
    prop_match_abs = round(n_match_abs / n_abs, 2)
  ) %>% 
  dplyr::rename_with(~ paste0(., '_2022'))


### Map of occupancy at selected years
# Years of interest
years_to_plot = c(0, 8, 17, 95)

# Filter patches with years
patch_sf_occupancy = patch_sf_pop %>% 
  dplyr::filter(Year %in% years_to_plot) %>% 
  dplyr::mutate(
    Occupancy = dplyr::if_else(
      MeanNTot > 0,
      "Occupied",
      "Unoccupied"
    )
  )

### Create maps
# Creating a background for the raster
coltb = data.frame(value=1:6, 
                   col=c(
                     "#FFFFB2",
                     "#32a65e",
                     "chartreuse",
                     "darkgreen",
                     "#0000FF",
                     "#d4271e"
                   ))
# Convert to factor
landsc_factor = terra::as.factor(landsc)
# Assign the color palette
terra::coltab(landsc_factor) = coltb

## 2005
# Patches
patches_0 = patch_sf_pop %>% 
  dplyr::filter(Year == 0)
# Map
map2005 = ggplot() +
  tidyterra::geom_spatraster(
    data = landsc_factor,
    use_coltab = TRUE,
    show.legend = FALSE
  ) +
  # Occupied patches
  geom_sf(
    data = patches_0 %>% 
      dplyr::filter(MeanNTot > 0),
    fill = "#FF7B00",
    colour = NA
  ) +
  # Regions
  geom_sf(
    data = regions,
    shape = 23,
    size = 1,
    fill = "#E7FF00",
    colour = "black",
    stroke = 0.1
  ) +
  ggtitle("2005") +
  theme_void()

## 2013
# Patches
patches_8 = patch_sf_pop %>% 
  dplyr::filter(Year == 8)
# Title
title_2013 = paste0(
  "2013\n",
  "% true positive = ",
  round(prop_match_2013$prop_match_pres_2013 * 100, 1),
  "% | true negative = ",
  round(prop_match_2013$prop_match_abs_2013 * 100, 1),
  "%"
)
# Map
map2013 = ggplot() +
  tidyterra::geom_spatraster(
    data = landsc_factor,
    use_coltab = TRUE,
    show.legend = FALSE
  ) +
  # Occupied patches
  geom_sf(
    data = patches_8 %>% 
      dplyr::filter(MeanNTot > 0),
    fill = "#FF7B00",
    colour = NA
  ) +
  geom_sf(
    data = glt_census %>% 
      dplyr::filter(Detect2013 == "N"),
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  geom_sf(
    data = glt_census %>% 
      dplyr::filter(Detect2013 == "P"),
    fill = NA,
    colour = "#0CD6E8",
    linewidth = 0.5
  ) +
  # Regions
  geom_sf(
    data = regions,
    shape = 23,
    size = 1,
    fill = "#E7FF00",
    colour = "black",
    stroke = 0.1
  ) +
  ggtitle(title_2013) +
  theme_void()

## 2022
# Patches
patches_17 = patch_sf_pop %>% 
  dplyr::filter(Year == 17)
# Title
title_2022 = paste0(
  "2022\n",
  "% true positive = ",
  round(prop_match_2022$prop_match_pres_2022 * 100, 1),
  "% | true negative = ",
  round(prop_match_2022$prop_match_abs_2022 * 100, 1),
  "%"
)
# Map
map2022 = ggplot() +
  tidyterra::geom_spatraster(
    data = landsc_factor,
    use_coltab = TRUE,
    show.legend = FALSE
  ) +
  # Occupied patches
  geom_sf(
    data = patches_17 %>% 
      dplyr::filter(MeanNTot > 0),
    fill = "#FF7B00",
    colour = NA
  ) +
  geom_sf(
    data = glt_census %>% 
      dplyr::filter(Detect2022 == "N"),
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  geom_sf(
    data = glt_census %>% 
      dplyr::filter(Detect2022 == "P"),
    fill = NA,
    colour = "#0CD6E8",
    linewidth = 0.5
  ) +
  # Regions
  geom_sf(
    data = regions,
    shape = 23,
    size = 1,
    fill = "#E7FF00",
    colour = "black",
    stroke = 0.1
  ) +
  ggtitle(title_2022) +
  theme_void()

## 2100
# Patches
patches_95 = patch_sf_pop %>% 
  dplyr::filter(Year == 95)
# Map
map2100 = ggplot() +
  tidyterra::geom_spatraster(
    data = landsc_factor,
    use_coltab = TRUE,
    show.legend = FALSE
  ) +
  # Occupied patches
  geom_sf(
    data = patches_95 %>% 
      dplyr::filter(MeanNTot > 0),
    fill = "#FF7B00",
    colour = NA
  ) +
  # Regions
  geom_sf(
    data = regions,
    shape = 23,
    size = 1,
    fill = "#E7FF00",
    colour = "black",
    stroke = 0.1
  ) +
  ggtitle("2100") +
  theme_void()

### Combine the four maps
maps_combined = map2005 + map2013 + map2022 + map2100 +
  patchwork::plot_layout(ncol = 2, nrow = 2, widths = c(1, 1))

## Reduce space taken by titles
maps_combined = maps_combined &
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 2)
    ),
    plot.margin = margin(2, 2, 2, 2)
  )

## Export plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "map_occ_patches.png"),
    width = 2400, height = 1800, res = 300, type="cairo")
maps_combined
dev.off()


##### Patch abundance ------
# Filter patches based on the list of patches used for simulations
pop_patch_select = pop_patch %>% 
  # Remove patches that do not belong to the list
  dplyr::filter(patch_id %in% patch_list)

# Keep years of interest
pop_patch_select = pop_patch_select %>%
  dplyr::filter(
    Year %in% c(0, 8, 13, 17) # Years of interest
  )

# Remove suffixes from patches ids and create a variable "FragName" resembling UMMP name
pop_patch_select = pop_patch_select %>%
  dplyr::mutate(
    FragName = stringr::str_remove(
      patch_id, # patch name
      "_\\d+$"
    )
  )
head(pop_patch_select) # See difference between patch_id and FragName

# Aggregate patches sharing the same fragment name
# Example: since Aldeia_I_1 and Aldeia_I_2 correspond to the same monitored UMMP, we sum their abundances
pop_patch_select_agg = pop_patch_select %>%
  dplyr::group_by(FragName, Year) %>%
  dplyr::summarise(
    SimN_Ad = sum(MeanNInd),
    SimN_Juv = sum(MeanNJuvs),
    SimN_Tot = sum(MeanNTot),
    .groups = "drop"
  )

# Reshape the real census data
real_census_long = real_census_data %>%
  tidyr::pivot_longer(
    starts_with("n_glt"),
    names_to = "Survey",
    values_to = "RealN"
  ) %>%
  dplyr::mutate(
    Year = dplyr::case_when(
      Survey == "n_glt_2005" ~ 0,
      Survey == "n_glt_2013" ~ 8,
      Survey == "n_glt_2018" ~ 13,
      Survey == "n_glt_2022" ~ 17
    )
  ) %>% 
  dplyr::filter(!is.na(FragName),
                Survey != "n_glt_2018") # Remove the 2018 census (yellow fever)

# Combine real and simulated pop sizes
comparison = pop_patch_select_agg %>%
  # Join using the FragName (i.e., UMMP name)
  dplyr::left_join(
    real_census_long,
    by = c(
      "FragName" = "FragName",
      "Year"
    )
  ) %>% 
  dplyr::filter(!is.na(RealN)) %>% # Remove where pop size was not assessed in some patches
  dplyr::group_by(FragName, 
                  Year) %>% 
  # Compute RMSE (difference between simulated and real)
  dplyr::mutate(
    RMSE = sqrt(mean((SimN_Tot - RealN)^2))
  ) %>% 
  dplyr::ungroup()

# Correlation between simulated and real data
cor(comparison$SimN_Tot, comparison$RealN)

# Smallest differences
comparison %>% 
  dplyr::select(FragName, Year, SimN_Tot, RealN, RMSE) %>% 
  dplyr::arrange(RMSE)

# Largest differences
comparison %>% 
  dplyr::select(FragName, Year, SimN_Tot, RealN, RMSE) %>% 
  dplyr::arrange(dplyr::desc(RMSE))

# Mean difference across years
comparison %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean(RMSE)) %>% 
  dplyr::arrange(Year)

# Mean difference across fragments
comparison %>% 
  dplyr::group_by(FragName) %>% 
  dplyr::summarise(mean = mean(RMSE)) %>% 
  dplyr::arrange(mean)


# Plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "corr_patch_simvsreal.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
ggplot(
  comparison,
  aes(x = RealN, y = SimN_Tot)
) +
  geom_point(size = 1) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2
  ) +
  facet_wrap(~Year) +
  theme_bw() +
  labs(
    x = "Observed population",
    y = "Simulated population"
  )
dev.off()

# Comparison (in long format)
comparison_long = comparison %>%
  dplyr::select(
    UMMPs,
    Year,
    RealN,
    SimN_Tot
  ) %>%
  tidyr::pivot_longer(
    c(RealN, SimN_Tot),
    names_to = "Source",
    values_to = "N"
  ) %>% 
  dplyr::mutate(Source = dplyr::case_when(
    Source == "RealN" ~ "Observed pop. size",
    Source == "SimN_Tot" ~ "Simulated pop. size"
  ))
# Plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "corr_patch_simvsreal2.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
ggplot(
  comparison_long,
  aes(
    Year,
    N,
    colour = Source
  )) +
  geom_line() +
  geom_point() +
  facet_wrap(
    ~UMMPs,
    scales = "free_y"
  ) +
  theme_bw()
dev.off()

##### Source/sink dynamics -----
# We work at the scale of real patches
# We identify patch pop dynamics (positive, negative, stable)
# We add dispersal flux (balance between emigration and immigration)

###### Patch dynamics --------
# Summarize pop size across uncut patches
pop_patch_uncut = pop_patch %>%
  dplyr::group_by(
    patch_id, # uncut patch id
    Year
  ) %>% 
  dplyr::summarise(
    MeanN_Ad = sum(MeanNInd),
    MeanN_Juv = sum(MeanNJuvs),
    MeanN_Tot = sum(MeanNTot),
    .groups = "drop"
  )

# Compute growth rate and lambda
pop_patch_uncut = pop_patch_uncut %>% 
  dplyr::group_by(patch_id) %>% 
  dplyr::arrange(Year, .by_group = TRUE) %>% 
  dplyr::mutate(
    lag_value = dplyr::lag(MeanN_Tot),
  
    # Finite rate of increase
    lambda = dplyr::if_else(
      lag_value > 0,
      MeanN_Tot / lag_value,
      NA_real_),
    
    # Dynamics
    dyn = dplyr::case_when(
      lambda > 1 ~ "+",
      lambda < 1 ~ "-",
      lambda == 1 ~ "="
    )
    ) %>% 
  dplyr::ungroup()

# Remove year 0
pop_patch_uncut = pop_patch_uncut %>% 
  dplyr::filter(Year != 0)

###### Patch dispersal --------
# Filter dispersal flux
# We keep the dispersal flux of the best simulation only
disp_subset = dispersers_all %>% 
  dplyr::filter(
    Id_simul == best_sim
  )

# Emigration: successful and failed emigrations leaving each patch (focus on the origin)
emig = disp_subset %>% 
  dplyr::filter(
    Disp_type %in% c(
      "Inter-patch dispersal",
      "Died in matrix"
    )
  ) %>% 
  dplyr::group_by(
    Orig_patch_name,
    Year,
    Rep
  ) %>% 
  dplyr::summarise(
    # Successful emigrations
    n_emig = dplyr::n_distinct(
      IndID[Disp_type == "Inter-patch dispersal"]
    ),
    
    # Failed emigrations
    n_emig_failed = dplyr::n_distinct(
      IndID[Disp_type == "Died in matrix"]
    ),
    
    # Mean distance of successful emigrants in this replicate
    mean_dist_emig = mean(
      DistMoved[Disp_type == "Inter-patch dispersal"],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>% 
  dplyr::rename(
    patch_id = Orig_patch_name
  ) %>% 
  dplyr::group_by(
    patch_id,
    Year
  ) %>% 
  dplyr::summarise(
    # Average across replicates
    mean_n_emig = round(mean(n_emig), 1),
    mean_n_emig_failed = round(mean(n_emig_failed), 1),
    mean_dist_emig = mean(mean_dist_emig, na.rm = TRUE),
    .groups = "drop"
  )

# Immigrants: focus on the destination
immig = disp_subset %>% 
  dplyr::filter(
    Disp_type == "Inter-patch dispersal"
    ) %>% 
  dplyr::group_by(
    Dest_patch_name,
    Year,
    Rep
  ) %>% 
  dplyr::summarise(
    
    # Number of successful immigrants
    n_immig = dplyr::n_distinct(IndID),
    .groups = "drop",
    
    # Mean dispersal distance of immigrants
    mean_dist_immig = mean(
      DistMoved,
      na.rm = TRUE
    ),
    
    
  ) %>% 
  dplyr::rename(
    patch_id = Dest_patch_name
  ) %>% 
  dplyr::group_by(
    patch_id,
    Year
  ) %>% 
  # Average across replicates
  dplyr::summarise(
    mean_n_immig = round(mean(n_immig),1),
    mean_dist_immig = mean(mean_dist_immig),
    .groups = "drop"
  )

# Bind
emig_immig = emig %>% 
  dplyr::full_join(immig, by=c("patch_id","Year"))

# Complete missing years
emig_immig = emig_immig %>% 
  tidyr::complete(
    patch_id,
    Year,
    fill = list(
      mean_n_immig = 0,
      mean_n_emig = 0,
      mean_n_emig_failed = 0
    )
  )

# Mean across patches
emig_immig %>% 
  dplyr::select(-Year) %>% 
  dplyr::summarise(
    dplyr::across(
      dplyr::where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    )
  )

###### Categorize as source/sink ------
# Merge pop and disp data
pop_disp_patch_uncut = pop_patch_uncut %>% 
  dplyr::left_join(emig_immig, by=c("patch_id","Year"))

# Categorize patches as sources and sinks
# Sources: patches with positive growth rates (lambda>1) and emigrants>immigrants
# Sinks: patches with negative growth rates (lambda<1) and immigrants>emigrants
pop_disp_patch_uncut = pop_disp_patch_uncut %>% 
  dplyr::mutate(
    source_sink = dplyr::case_when(
      dyn == "+" & mean_n_emig>mean_n_immig ~ "Viable source",
      dyn == "=" & mean_n_emig>mean_n_immig ~ "Viable source",
      dyn == "-" & mean_n_emig>mean_n_immig ~ "Unviable source",
      dyn == "+" & mean_n_immig>mean_n_emig ~ "Viable sink",
      dyn == "=" & mean_n_immig>mean_n_emig ~ "Viable sink",
      dyn == "-" & mean_n_immig>mean_n_emig ~ "Unviable sink",
      dyn == "=" & mean_n_immig==mean_n_emig ~ "Equilibrium",
      dyn == "+" & mean_n_emig==mean_n_immig ~ "Other viable patches",
      dyn == "-" & mean_n_emig==mean_n_immig ~ "Other unviable patches",
      MeanN_Tot > 0 & lag_value == 0 ~ "Recolonized",
      MeanN_Tot == 0 ~ "Unoccupied",
      TRUE ~ "Other"
    )
    )
pop_disp_patch_uncut %>% 
  dplyr::count(source_sink)

###### Patch dynamics ------
# Filter using the list of patches
patch_dyn_select = pop_disp_patch_uncut %>% 
  dplyr::filter(patch_id %in% patch_list)
  
# In long format
patch_dyn_select_long = patch_dyn_select %>% 
  dplyr::select(
    patch_id,
    Year,
    MeanN_Tot,
    mean_n_emig,
    mean_n_immig
  ) %>% 
  tidyr::pivot_longer(
    cols = c(
      MeanN_Tot,
      mean_n_emig,
      mean_n_immig
    ),
    names_to = "Variable",
    values_to = "Value"
  ) %>% 
  dplyr::mutate(
    Variable = dplyr::recode(
      Variable,
      MeanN_Tot = "Population size",
      mean_n_emig = "Emigrants",
      mean_n_immig = "Immigrants"
    )
  )
# Plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "patch_dyn.png"),
    width = 3000, height = 2000, res = 300, type="cairo")
# Plot
ggplot2::ggplot(
  patch_dyn_select_long,
  ggplot2::aes(
    x = Year,
    y = Value,
    colour = Variable,
    group = Variable
  )
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 0.5) +
  ggplot2::facet_wrap(
    ~ patch_id,
    scales = "free_y"
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Number of individuals",
    colour = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    strip.text = ggplot2::element_text(face = "bold")
  )
dev.off()

###### Patch status ------
# Proportion of patches in each source/sink category per year
source_sink_prop = pop_disp_patch_uncut %>% 
  dplyr::filter(!is.na(source_sink),
                source_sink != "Unoccupied",
                source_sink != "Equilibrium",
                source_sink != "Recolonized") %>% 
  dplyr::group_by(Year, source_sink) %>% 
  dplyr::summarise(
    n_patches = dplyr::n_distinct(patch_id),
    .groups = "drop"
  ) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::mutate(
    prop_patches = n_patches / sum(n_patches)
  ) %>% 
  dplyr::ungroup()

# Plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "source_sink_dyn.png"),
    width = 2000, height = 1200, res = 300, type="cairo")
ggplot2::ggplot(
  source_sink_prop,
  ggplot2::aes(
    x = Year,
    y = prop_patches,
    fill = source_sink
  )
) +
  ggplot2::geom_col(width = 0.8) +
  ggplot2::scale_fill_manual(
    values = c(
      "Viable source" = "#00F5DC",
      "Unviable source" = "#00453E",
      "Viable sink" = "#FF2273",
      "Unviable sink" = "#71002A",
      "Other viable patches" = "#CFFFFE",
      "Other unviable patches" = "#FFCFE1"
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format()
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Proportion of patches",
    fill = "Patch category"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom"
  )
dev.off()

#### Heatmaps ----
# Because movement is stochastic, the number of visits per cell and the colonisation of empty patches are different for each replicate. 
# In order to take into account this variance, we average over all replicates and plot the resulting heatmap
# create a raster stack with all replicates as layers

# Load heatmaps
heatmap_files = paste0(
  dirpath,
  "Output_Maps/Batch1_Sim",
  best_sim,
  "_Land1_Rep",
  0:19,
  "_Visits.txt"
)

heatmaps_stack = terra::rast(heatmap_files)

# average over all layers
heatmaps_mean = terra::mean(heatmaps_stack)

# quick plot
# Keep only patch cells with values > 0
patches_positive = terra::ifel(patch > 0, 1, NA)

# Plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "heatmap.png"),
    width = 2000, height = 1200, res = 300, type="cairo")
# Base heatmap
terra::plot(
  heatmaps_mean,
  col = magma(9)
)
# Overlay patches in transparent grey
terra::plot(
  patches_positive,
  col = adjustcolor("white", alpha.f = 0.20),
  add = TRUE,
  legend = FALSE
)
dev.off()


#### Connectivity ----
##### Connect files -----
### Stack ALL files
# connec_files = list.files(
#   here::here("data",
#              "rangeshifter",
#              "tests",
#              test_config$test_name,
#              "Outputs"),
#   pattern = "_Connect\\.txt$", # Connectivity files
#   full.names = TRUE
# )
# 
# # Read and stack all pop files
# connec_all = purrr::map_dfr(connec_files, function(f){
#   
#   # Simulation id
#   sim_id = stringr::str_extract(
#     basename(f),
#     "(?<=Sim)\\d+(?=_Land)"
#   ) %>%  as.numeric()
#   
#   # Read pop file
#   read.table(
#     f,
#     header = TRUE,
#     sep = "\t"
#   ) %>%
#     dplyr::mutate(Id_simul = sim_id)
#   
# })
# 
# ### Join tested parameter
# connec_all = connec_all %>% 
#   dplyr::left_join(metadata, by = "Id_simul")

### Stack ONLY files corresponding to the best simulation
connec_files = list.files(
  here::here(
    "data",
    "rangeshifter",
    "tests",
    test_config$test_name,
    "Outputs"
  ),
  pattern = paste0(
    "_Sim", best_sim, "_Land.*_Connect\\.txt$"
  ),
  full.names = TRUE
)

# Read and stack connectivity files
connec_all = purrr::map_dfr(connec_files, function(f) {
  
  # Simulation id
  sim_id = stringr::str_extract(
    basename(f),
    "(?<=Sim)\\d+(?=_Land)"
  ) %>% 
    as.numeric()
  
  # Read connectivity file
  read.table(
    f,
    header = TRUE,
    sep = "\t"
  ) %>%
    dplyr::mutate(
      Id_simul = sim_id
    )
})


# Take a look at the dataset
dplyr::glimpse(connec_all)

##### Among cut patches ------
## Plot the lines at the scale of cut patches

## Select the dataset
data.disp.cut = connec_all %>% 
  dplyr::filter(
    StartPatch != "-999", # Remove the total number of successful immigrants into a patch
    EndPatch != "-999" # Remove the total number of successful emigrants into a patch
  )

## Assign each year to a period
data.disp.cut = data.disp.cut %>%
  dplyr::mutate(
    Period = dplyr::case_when(
      Year >= 0  & Year <= 8  ~ "Year 0-8",
      Year > 8  & Year <= 17 ~ "Year 8-17",
      Year > 17 & Year < 96  ~ "Year 17-95",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(Period))

## Aggregate dispersal across years within each period
data.disp.cut = data.disp.cut %>%
  dplyr::group_by(
    # Id_simul,
    # dplyr::across(dplyr::all_of(params_tested)),
    Period,
    StartPatch,
    EndPatch
  ) %>%
  # Sums of dispersing individuals between pairs of patches
  dplyr::summarise(
    Ninds = sum(Ninds, na.rm = TRUE),
    .groups = "drop"
  )

## WARNING: duplicated pairs of patches
## e.g. movement from 1 -> 2 AND 2 -> 1
data.disp.cut = data.disp.cut %>%
  dplyr::filter(StartPatch != EndPatch) %>% ## Remove intra-patch dispersal
  dplyr::mutate(
    Patch1 = pmin(StartPatch, EndPatch),
    Patch2 = pmax(StartPatch, EndPatch),
    Pair = paste0(Patch1, "_", Patch2)
  ) %>%
  dplyr::group_by(
    # Id_simul,
    Period,
    Patch1,
    Patch2,
    Pair
  ) %>%
  dplyr::summarise(
    Ninds = sum(Ninds, na.rm = TRUE),
    .groups = "drop"
  )

### Join coordinates

# Centroids of CUT patches
patch_pts = patch_sf %>%
  sf::st_centroid() %>%
  dplyr::select(PatchID) %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[, 1],
    y = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry()


# Coordinates of origin
data.disp.cut = data.disp.cut %>% 
  dplyr::left_join(
    patch_pts %>%
      dplyr::rename(
        Patch1 = PatchID,
        Long_from = x,
        Lat_from = y
      ),
    by = "Patch1"
  ) %>%
  
  # Coordinates of destination
  dplyr::left_join(
    patch_pts %>%
      dplyr::rename(
        Patch2 = PatchID,
        Long_to = x,
        Lat_to = y
      ),
    by = "Patch2"
  )


### Create origins and destination objects

data.disp.cut.from = data.disp.cut %>%
  dplyr::select(-c(Long_to, Lat_to)) %>%
  dplyr::rename(
    Long = Long_from,
    Lat = Lat_from
  )

data.disp.cut.to = data.disp.cut %>%
  dplyr::select(-c(Long_from, Lat_from)) %>%
  dplyr::rename(
    Long = Long_to,
    Lat = Lat_to
  )

### Create LINESTRING geometries

disp.lines.cut = rbind(
  data.disp.cut.from,
  data.disp.cut.to
)

disp.lines.cut = disp.lines.cut %>% 
  sf::st_as_sf(
    coords = c("Long", "Lat"),
    na.fail = FALSE,
    crs = sf::st_crs(patch_sf)
  ) %>%
  dplyr::group_by(
    # Id_simul,
    Pair,
    Period
  ) %>%
  dplyr::summarise(
    Ninds = dplyr::first(Ninds),
    .groups = "drop"
  ) %>%
  sf::st_cast("LINESTRING")


## MAPS
# 2005-2013
map_disp_cut_2005_2013 = ggplot2::ggplot() +
  
  # Landscape
  tidyterra::geom_spatraster(
    data = landsc_factor,
    use_coltab = TRUE,
    show.legend = FALSE
  ) +
  
  # # Patches
  # ggplot2::geom_sf(
  #   data = patch_sf,
  #   ggplot2::aes(fill = factor(PatchID)),
  #   colour = "black",
  #   linewidth = 0.01,
  #   alpha = 0.5,
  #   show.legend = FALSE
  # ) +
  # 
  # Simulated connectivity
  ggplot2::geom_sf(
    data = disp.lines.cut %>% 
      dplyr::filter(
        # Id_simul == best_sim,
        Period == "Year 0-8"
      ),
    ggplot2::aes(linewidth = Ninds),
    colour = "deeppink",
    alpha = 0.8
  ) +
  
  # # Observed dispersal
  # ggplot2::geom_sf(
  #   data = glt_disp,
  #   colour = "orange",
  #   linewidth = 0.2,
  #   alpha = 0.9
  # ) +
  
  ggplot2::scale_linewidth_continuous(
    trans = "log1p",
    range = c(0.01, 0.5),
    name = "Number of dispersers"
  ) +
  
  ggplot2::ggtitle("Dispersal flux (2005-2013)") +
  
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none"
  )


# 2013-2022
map_disp_cut_2013_2022 = ggplot2::ggplot() +
  
  # Landscape
  tidyterra::geom_spatraster(
    data = landsc_factor,
    use_coltab = TRUE,
    show.legend = FALSE
  ) +
  # 
  # # Patches
  # ggplot2::geom_sf(
  #   data = patch_sf,
  #   ggplot2::aes(fill = factor(PatchID)),
  #   colour = "black",
  #   linewidth = 0.01,
  #   alpha = 0.5,
  #   show.legend = FALSE
  # ) +
  
  ggplot2::geom_sf(
    data = disp.lines.cut %>% 
      dplyr::filter(
        # Id_simul == best_sim,
        Period == "Year 8-17"
      ),
    ggplot2::aes(linewidth = Ninds),
    colour = "deeppink",
    alpha = 0.8
  ) +
  
  # ggplot2::geom_sf(
  #   data = glt_disp,
  #   colour = "orange",
  #   linewidth = 0.2,
  #   alpha = 0.9
  # ) +
  
  ggplot2::scale_linewidth_continuous(
    trans = "log1p",
    range = c(0.01, 0.5),
    name = "Number of dispersers"
  ) +
  
  ggplot2::ggtitle("Dispersal flux (2013-2022)") +
  
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none"
  )


# 2022-2100
map_disp_cut_2022_2100 = ggplot2::ggplot() +
  
  # Landscape
  tidyterra::geom_spatraster(
    data = landsc_factor,
    use_coltab = TRUE,
    show.legend = FALSE
  ) +
  
  # # Patches
  # ggplot2::geom_sf(
  #   data = patch_sf,
  #   ggplot2::aes(fill = factor(PatchID)),
  #   colour = "black",
  #   linewidth = 0.01,
  #   alpha = 0.5,
  #   show.legend = FALSE
  # ) +
  
  ggplot2::geom_sf(
    data = disp.lines.cut %>% 
      dplyr::filter(
        # Id_simul == best_sim,
        Period == "Year 17-95"
      ),
    ggplot2::aes(linewidth = Ninds),
    colour = "deeppink",
    alpha = 0.8
  ) +
  
  # ggplot2::geom_sf(
  #   data = glt_disp,
  #   colour = "orange",
  #   linewidth = 0.2,
  #   alpha = 0.9
  # ) +
  # 
  ggplot2::scale_linewidth_continuous(
    trans = "log1p",
    range = c(0.01, 0.5),
    name = "Number of dispersers"
  ) +
  
  ggplot2::ggtitle("Dispersal flux (2022-2100)") +
  
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none"
  )

### Combine the maps
maps_combined_disp = map_disp_cut_2005_2013 + map_disp_cut_2013_2022 + map_disp_cut_2022_2100 +
  patchwork::plot_layout(ncol = 2, nrow = 2, widths = c(1, 1))

## Export plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "map_disp_cut_patches.png"),
    width = 2400, height = 1800, res = 300, type="cairo")
maps_combined_disp
dev.off()

##### Among uncut patches ------
## Plot the lines at the scale of "real" patches (e.g., UMMPs)

# Select the dataset
data.disp.ummps = connec_all %>% 
  dplyr::filter(StartPatch != "-999", # Remove the total number of successful immigrants into a patch
                EndPatch != "-999")  %>% # Remove the total number of successful emigrants into a patch 
  dplyr::left_join(patch_corres_id, 
                   by=c("StartPatch" = "cut_patch_id")) %>% 
  dplyr::rename(StartPatchName = uncut_patch_name) %>% 
  dplyr::select(-uncut_patch_id) %>% 
  dplyr::left_join(patch_corres_id, 
                   by=c("EndPatch" = "cut_patch_id")) %>% 
  dplyr::rename(EndPatchName = uncut_patch_name) %>% 
  dplyr::select(-uncut_patch_id)

## Assign each year to a period
data.disp.ummps = data.disp.ummps %>%
  dplyr::mutate(
    Period = dplyr::case_when(
      Year >= 0  & Year <= 8  ~ "Year 0-8",
      Year > 8  & Year <= 17 ~ "Year 8-17",
      Year > 17 & Year < 96 ~ "Year 17-95",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(Period))

## Aggregate dispersal across years within each period
data.disp.ummps = data.disp.ummps %>%
  dplyr::group_by(
    Id_simul,
    # dplyr::across(dplyr::all_of(params_tested)),
    Period, # Or Year instrad
    StartPatchName,
    EndPatchName
  ) %>%
  dplyr::summarise(
    Ninds = sum(Ninds, na.rm = TRUE),
    .groups = "drop"
  )

# WARNING: duplicated pairs of patches (e.g., movement from 1 to 2 AND from 2 to 1)
# -> we aggregate first
data.disp.ummps = data.disp.ummps %>% 
  
  # Remove intra-patch dispersal
  dplyr::filter(StartPatchName != EndPatchName) %>% 
  
  # Create an undirected patch pair
  dplyr::mutate(
    Patch1 = pmin(StartPatchName, EndPatchName),
    Patch2 = pmax(StartPatchName, EndPatchName),
    Pair = paste0(Patch1, "_", Patch2)
  ) %>% 
  
  # Aggregate both directions
  # e.g. 1 -> 2 + 2 -> 1
  dplyr::group_by(
    # Id_simul,
    Pair,
    Period
  ) %>%
  dplyr::summarise(
    Ninds = sum(Ninds),
    # Keep other columns
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::first(.x)
    ),
    .groups = "drop"
  ) %>% 
  
  dplyr::select(-c(StartPatchName, EndPatchName))

### Join coordinates
# Centroids of patches (UNCUT)
patch_uncut_pts = patch_uncut %>%
  sf::st_centroid() %>%
  dplyr::select(patch_id) %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[, 1],
    y = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry()

# Join coordinates
data.disp.ummps = data.disp.ummps %>% 
  # coordinates of origin
  dplyr::left_join(
    patch_uncut_pts %>%
      dplyr::rename(
        Patch1 = patch_id,
        Long_from = x,
        Lat_from = y
      ),
    by = "Patch1"
  ) %>%
  
  # coordinates of destination
  dplyr::left_join(
    patch_uncut_pts %>%
      dplyr::rename(
        Patch2 = patch_id,
        Long_to = x,
        Lat_to = y
      ),
    by = "Patch2"
  )

# Create origins and destination objects
data.disp.ummps.from = data.disp.ummps %>%
  dplyr::select(-c(Long_to, Lat_to)) %>%
  dplyr::rename(
    Long = Long_from,
    Lat = Lat_from
  )
data.disp.ummps.to = data.disp.ummps %>%
  dplyr::select(-c(Long_from, Lat_from)) %>%
  dplyr::rename(
    Long = Long_to,
    Lat = Lat_to
  )

### To lines
# Create LINESTRING geometries
disp.lines.ummps = rbind(data.disp.ummps.from, data.disp.ummps.to) # We duplicate rows to create dispersal lines
disp.lines.ummps = disp.lines.ummps %>% 
  sf::st_as_sf(
    coords = c("Long", "Lat"),
    na.fail = FALSE,
    crs = sf::st_crs(patch_uncut)
  ) %>%
  dplyr::group_by(
    # Id_simul,
    Pair,
    Period
  ) %>%
  dplyr::summarise(
    Ninds = dplyr::first(Ninds),
    .groups = "drop"
  ) %>%
  sf::st_cast("LINESTRING")

# Plot the lines
# 2005-2013
map_disp_uncut_2005_2013 = ggplot2::ggplot() +
  
  # Patches
  ggplot2::geom_sf(
    data = patch_uncut,
    ggplot2::aes(fill = factor(patch_id)),
    colour = "black",
    linewidth = 0.01,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Simulated connectivity
  ggplot2::geom_sf(
    data = disp.lines.ummps %>% 
      dplyr::filter(
        # Id_simul == best_sim,
        Period == "Year 0-8") ,
    ggplot2::aes(linewidth = Ninds),
    colour = "deeppink",
    alpha = 0.8
  ) +
  
  # Observed dispersal
  ggplot2::geom_sf(
    data = glt_disp,
    colour = "orange",
    linewidth = 0.2,
    alpha = 0.9
  ) +
  
  # One panel per simulation
  # ggplot2::facet_wrap(~Id_simul) +
  
  # Width of simulated lines
  ggplot2::scale_linewidth_continuous(
    trans = "log1p",
    range = c(0.05, 1),
    name = "Number of dispersers"
  ) +
  
  ggtitle("Dispersal flux (2005-2013)") +
  
  ggplot2::theme_void() +
  
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "none"
  )

# 2013-2022
map_disp_uncut_2013_2022 = ggplot2::ggplot() +
  
  # Patches
  ggplot2::geom_sf(
    data = patch_uncut,
    ggplot2::aes(fill = factor(patch_id)),
    colour = "black",
    linewidth = 0.01,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Simulated connectivity
  ggplot2::geom_sf(
    data = disp.lines.ummps %>% 
      dplyr::filter(
        # Id_simul == best_sim,
        Period == "Year 8-17") ,
    ggplot2::aes(linewidth = Ninds),
    colour = "deeppink",
    alpha = 0.8
  ) +
  
  # Observed dispersal
  ggplot2::geom_sf(
    data = glt_disp,
    colour = "orange",
    linewidth = 0.2,
    alpha = 0.9
  ) +
  
  # One panel per simulation
  # ggplot2::facet_wrap(~Id_simul) +
  
  # Width of simulated lines
  ggplot2::scale_linewidth_continuous(
    trans = "log1p",
    range = c(0.05, 1),
    name = "Number of dispersers"
  ) +
  
  ggtitle("Dispersal flux (2013-2022)") +
  
  ggplot2::theme_void() +
  
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "none"
  )

# 2022-2100
map_disp_uncut_2022_2100 = ggplot2::ggplot() +
  
  # Patches
  ggplot2::geom_sf(
    data = patch_uncut,
    ggplot2::aes(fill = factor(patch_id)),
    colour = "black",
    linewidth = 0.01,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Simulated connectivity
  ggplot2::geom_sf(
    data = disp.lines.ummps %>% 
      dplyr::filter(
        # Id_simul == best_sim,
        Period == "Year 17-95") ,
    ggplot2::aes(linewidth = Ninds),
    colour = "deeppink",
    alpha = 0.8
  ) +
  
  # Observed dispersal
  ggplot2::geom_sf(
    data = glt_disp,
    colour = "orange",
    linewidth = 0.2,
    alpha = 0.9
  ) +
  
  # One panel per simulation
  # ggplot2::facet_wrap(~Id_simul) +
  
  # Width of simulated lines
  ggplot2::scale_linewidth_continuous(
    trans = "log1p",
    range = c(0.05, 1),
    name = "Number of dispersers"
  ) +
  
  ggtitle("Dispersal flux (2022-2100)") +
  
  ggplot2::theme_void() +
  
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "none"
  )

### Combine the maps
maps_combined_disp = map_disp_uncut_2005_2013 + map_disp_uncut_2013_2022 + map_disp_uncut_2022_2100 +
  patchwork::plot_layout(ncol = 2, nrow = 2, widths = c(1, 1))

## Export plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "map_disp_uncut_patches.png"),
    width = 2400, height = 1800, res = 300, type="cairo")
maps_combined_disp
dev.off()


### Append results -----
# Load an Excel sheet with the parameters to test
summary = read_excel(here::here("data", 
                                "rangeshifter", 
                                "tests", 
                                "Summary_tests.xlsx"))

#### Information to add manually ----

test_aim = "Tests IndsHaCell AND PR based on THE REAL POP SIZE (females*2)"
test_remarks = "IndsHaCell between 0.02 and 0.03, PR ~ 8"

#### Test name -----
test_name = basename(normalizePath(dirpath))

#### Dispersal ----
dispersal = "Y" # With "Y" or "N" depending on whether dispersal is included in simulations

#### Hab -----
hab = "Agriculture;Forest;Corridors & stepping stones;Wetlands & Forests>500m;Water;Built-up"

#### Tested parameters -----
# Put all the parameters that are listed in the metadata file
tested_parameters = c(
  "DensDep",
  "IndsHaCell",
  "Ad_survival",
  "Juv_survival",
  "Emig_prob",
  "PR",
  "PR_meth",
  "MS",
  "DP",
  "Step_mortality",
  "Max_nb_steps",
  "Goal_type",
  "Goal_bias",
  "StraightenPath",
  "Nhab",
  "Cost1",
  "Cost2",
  "Cost3",
  "Cost4",
  "Cost5",
  "Cost6"
)

# Make sure the format in the summary table is character
summary = summary %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(tested_parameters),
      as.character
    ))

# Put tested parameters into format
# If several values, paste with ; / if not tested, NA
format_tested_values = function(x) {
  
  x = unique(x[!is.na(x)])
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  paste(x, collapse = ";")
}

# Run
tested_values = sapply(
  metadata[tested_parameters],
  format_tested_values
)

# Put the combination into format
# Example: DensDep=0.08; IndsHaCell=0.095; Ad_survival=0.92
optimal_param = paste(
  names(best_fit)[names(best_fit) %in% tested_parameters],
  best_fit[1, names(best_fit) %in% tested_parameters],
  sep = "=",
  collapse = "; "
)

#### Pop size -----
# Extract pop sizes for the combination of parameters
optimal_pop = pop_time %>%
  dplyr::semi_join(
    best_values,
    by = params_tested)
pop_sizes = optimal_pop %>%
  dplyr::filter(Year %in% c(0, 8, 17, 95)) %>%
  dplyr::select(Year, MeanN_Tot) # Select the proper indicator
pop_2005 = pop_sizes$MeanN_Tot[pop_sizes$Year == 0]
pop_2013 = pop_sizes$MeanN_Tot[pop_sizes$Year == 8]
pop_2022 = pop_sizes$MeanN_Tot[pop_sizes$Year == 17]
pop_2100 = pop_sizes$MeanN_Tot[pop_sizes$Year == 95]

#### RMSE -----
rmse_pop_size = best_fit$RMSE

#### Stack all info ----

new_summary = dplyr::tibble(
  
  'Test' = test_name,
  'Aim' = test_aim,
  
  'DensDep' =
    format_tested_values(metadata$DensDep),
  'IndsHaCell' =
    format_tested_values(metadata$IndsHaCell),
  'Ad_survival' =
    format_tested_values(metadata$Ad_survival),
  'Juv_survival' =
    format_tested_values(metadata$Juv_survival),
  'Emig_prob' =
    format_tested_values(metadata$Emig_prob),
  'PR' =
    format_tested_values(metadata$PR),
  'PR_meth' =
    format_tested_values(metadata$PR_meth),
  'MS' =
    format_tested_values(metadata$MS),
  'DP' =
    format_tested_values(metadata$DP),
  'Step_mortality' =
    format_tested_values(metadata$Step_mortality),
  'Max_nb_steps' =
    format_tested_values(metadata$Max_nb_steps),
  'Goal_type' =
    format_tested_values(metadata$Goal_type),
  'Goal_bias' =
    format_tested_values(metadata$Goal_bias),
  'StraightenPath' =
    format_tested_values(metadata$StraightenPath),
  
  'Dispersal Y/N' = dispersal,
  
  'Nhab' = format_tested_values(metadata$Nhab),
  'Hab' = hab,
  'Cost1' =
    format_tested_values(metadata$Cost1),
  'Cost2' =
    format_tested_values(metadata$Cost2),
  'Cost3' =
    format_tested_values(metadata$Cost3),
  'Cost4' =
    format_tested_values(metadata$Cost4),
  'Cost5' =
    format_tested_values(metadata$Cost5),
  'Cost6' =
    format_tested_values(metadata$Cost6),
  
  'Optimal_param' = optimal_param,
  
  'Pop_size_2005' = pop_2005,
  'Pop_size_2013' = pop_2013,
  'Pop_size_2022' = pop_2022,
  'Pop_size_2100' = pop_2100,
  
  'RMSE_pop_size' = rmse_pop_size,
  
  'Patch_occ_2100' = patch_occ_2100 %>% 
    dplyr::semi_join(best_values, by = params_tested) %>% 
    dplyr::select(MeanNOccupPatches) %>% 
    dplyr::pull(1),
  'Occ_true_pos_2013%' = prop_match_2013$prop_match_pres_2013,
  'Occ_true_pos_2022%' = prop_match_2022$prop_match_pres_2022,
  'Occ_true_neg_2013%' = prop_match_2013$prop_match_abs_2013,
  'Occ_true_neg_2022%' = prop_match_2022$prop_match_abs_2022,
  'Prop_disp_intrapatch' = best_fit$mean_prop_intra,
  'Prop_disp_interpatch' = best_fit$mean_prop_inter,
  'Disp_dist_intrapatch' = best_fit$mean_disp_dist_intra,
  'Disp_dist_interpatch' = best_fit$mean_disp_dist_inter,
  
  'Remarks' = test_remarks
)

#### Append to existing summary ----

# Add to previous summary
summary = dplyr::bind_rows(
  summary,
  new_summary
)

#### Save --------
writexl::write_xlsx(
  summary,
  here::here(
    "data",
    "rangeshifter",
    "tests",
    "Summary_tests.xlsx"
  )
)
