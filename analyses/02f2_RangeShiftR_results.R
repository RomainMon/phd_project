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
  test_name = "test_resist_26", # Name of the folder
  
  # Parameters tested with sensitivity analysis (i.e., those varying during simulations)
  parameters = c(
    "IndsHaCell"
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
landsc = terra::rast(file.path(dirpath, "Inputs", "raster_reclass_2023.txt"))
terra::plot(landsc, col=c("#F1F13B","#00A305","#EE11EE","#004702","blue","red"))
terra::res(landsc)
terra::unique(landsc)

#### Patches ----
## Cut patches
patch = terra::rast(file.path(dirpath, "Inputs", "patches_2023.txt"))
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
                                     "patches_rshifter", 
                                     "patches_rshifter_2023.gpkg")) 
plot(patch_uncut)

#### Species distribution -----
patch_w_glt  = terra::rast(file.path(dirpath, "Inputs", "patches_w_glt_2023.txt"))
terra::res(patch_w_glt)
terra::plot(patch_w_glt, col=c("gray","darkgreen"))
# Overlay uncut patch boundaries
plot(
  sf::st_geometry(patch_uncut),
  add = TRUE,
  border = "black",
  lwd = 0.3
)
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
                                             "patch_corres_id_2023.csv"),
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
# IMPORTANT: Data are collected BEFORE reproduction at each reproductive season
# - Species number (Species); not yet used, always zero 
# - Number of individuals in the population (NInd). In the case of a stage-structured population, the number of individuals in each stage (NInd_stageX). If the reproduction is sexual, these columns will be replaced by the number of females (Nfemales_stageX) and of males (Nmales_stageX) in each stage. In the case of sexual model without stage structure, two columns will indicate the number of females (Nfemales) and of males (Nmales) in the population. 
# - In the case of a stage-structured population, the number of juveniles born (NJuvs). If the  reproduction is sexual, these columns will be replaced by the number of females juveniles (NJuvFemales) and males (NJuvMales).

## Individuals
# Contains information regarding each individual at a given time step.
# Data are recorded after settlement and before aging (in the case of overlapping generations), in order that dispersal data for individuals which die may be reported.
# IMPORTANT: Individuals data for year T therefore correspond to Population and Traits summary data for year T+1 (and individuals which die in year T must be excluded for the data to match at population level).

# 1. Replicate number (Rep) 
# 2. Year 
# 3. Reproductive season within the year (RepSeason) 
# 4. Species number (currently must be 0) 
# 5. Individual ID (IndID), a unique integer number that identifies the individual 
# 6. The individual’s Status
# 7. Natal cell (Natal_X and Natal_Y) and current cell (X and Y) coordinates or natal and current  patch IDs (Natal_patch and PatchID); if the individual has crossed an absorbing landscape or ‘no-data’ boundary, the current cell/patch will be missing (set to -1) 
# 8. Sex, in case of sexual model (0 = female, 1 = male) 
# 9. Age in years, in case of overlapping generations 
# 10. Stage, in case of stage structure 
# 11. Emigration traits when there is inter-individual variability, 
# 12. Dispersal kernel traits, if the transfer is modelled with dispersal kernels that vary between individuals
# 13. If the transfer is modelled with inter-individually variable CRW, columns in point 11 are replaced by the individual step length (StepLength) and walk correlation (Rho) 
# 14. If there is a density-dependent settlement rule for a movement model, and there is interindividual variability, three traits determining settlement probability (S0, alphaS and betaS) 
# 15. Distance moved in meters (linear distance from the centre of the starting cell to the centre  of the arrival cell - DistMoved); if the individual has crossed an absorbing landscape or ‘nodata’ boundary, the distance moved will be missing (set to -1) 
# 16. Number of steps taken (Nsteps) for movement models


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
# i.e. Patch A: Year 1–7 → no row VS Year 8 → individuals

## We complete the dataset
pop_all_complete = pop_all %>% 
  dplyr::select(-c(RepSeason, 
                   Ncells, 
                   Species)) %>% # Remove unnecessary columns
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

# CHECK
dplyr::glimpse(pop_all_complete) # We should see NInd = 0 now (before first occurrence)

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

# Population size by simulation
pop_total = pop_all_complete %>%
  dplyr::group_by(Id_simul, 
                  dplyr::across(dplyr::all_of(params_tested)),
                  Rep, 
                  Year) %>%
  # WARNING: 
  # IF female-only model: the population only includes females -> multiply to estimate the total population
  # Sex ratio in wild populations: between 53% and 66% (Dietz and Baker 1993)
  # Sex ratio in zoos: ~56%
  # Female proportion: 100-56=44
  # Multiplier: 1/0.44 = 2.3
  # IF sex model, the population includes both males and females
  dplyr::summarise(NInd = sum(NInd), # N Adults (Stage 1 + Stage 2) BEFORE REPRODUCTION
                   NJuvs = sum(NJuvs), # N births
                   NTot = (NInd+NJuvs)*2.3, # Total population AFTER REPRODUCTION
                   .groups = "drop") %>% 
  dplyr::filter(Year != max(Year, na.rm = TRUE)) # Remove the last Year

# Mean population size over time (by tested parameter)
pop_time = pop_total %>%
  dplyr::group_by(
    Id_simul,
    dplyr::across(dplyr::all_of(params_tested)),
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
  # facet_grid(
  #   cols = vars(.data[[params_tested[2]]]),
  #   rows = vars(.data[[params_tested[3]]]),
  #   scales = "free_y") +
  
  ### IF SIMULATIONS START IN 2005
  ## Vertical reference years
  # geom_vline(
  #   xintercept = c(0, 8, 17),
  #   linetype = "dashed",
  #   colour = "black"
  # ) +
  
  ## Annotations at reference years (2005, 2013, 2022)
  # annotate("point", x = 0,  y = 1600, size = 3) +
  # annotate("point", x = 8, y = 3706, size = 3) +
  # annotate("point", x = 17, y = 4869, size = 3) +
  # annotate("text", x = 0,  y = 1600, label = "2005", vjust = -1) +
  # annotate("text", x = 8, y = 3706, label = "2013", vjust = -1) +
  # annotate("text", x = 17, y = 4869, label = "2022", vjust = -1) +
  
  ### IF SIMULATIONS START IN 2023
  annotate("point", x = 0, y = 4869, size = 3) +
  annotate("text", x = 0, y = 4869, label = "2022", vjust = -1) +
  
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
    y = MeanN_Tot, # Choose the proper count
    colour = factor(.data[[params_tested[1]]])
  )
) +
  geom_line(linewidth = 1) +
  # Faceting
  # facet_grid(
  #   cols = vars(.data[[params_tested[2]]]),
  #   rows = vars(.data[[params_tested[3]]]),
  #   scales = "free_y") +
  
  ### IF SIMULATIONS START IN 2005
  ## Vertical reference years
  # geom_vline(
  #   xintercept = c(0, 8, 17),
  #   linetype = "dashed",
  #   colour = "black"
  # ) +
  
  ## Annotations at reference years (2005, 2013, 2022)
  # annotate("point", x = 0,  y = 1600, size = 3) +
  # annotate("point", x = 8, y = 3706, size = 3) +
  # annotate("point", x = 17, y = 4869, size = 3) +
  # annotate("text", x = 0,  y = 1600, label = "2005", vjust = -1) +
  # annotate("text", x = 8, y = 3706, label = "2013", vjust = -1) +
  # annotate("text", x = 17, y = 4869, label = "2022", vjust = -1) +
  
  ### IF SIMULATIONS START IN 2023
  annotate("point", x = 0, y = 4869, size = 3) +
  annotate("text", x = 0, y = 4869, label = "2022", vjust = -1) +
  
  theme_bw() +
  labs(y = "Mean population size (total)")

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
# fit_score = pop_time %>%
#   dplyr::group_by(
#     Id_simul,
#     dplyr::across(dplyr::all_of(params_tested))) %>%
#   dplyr::summarise(
#     N0 = MeanN_Tot[Year == 0],
#     N8 = MeanN_Tot[Year == 8],
#     N17 = MeanN_Tot[Year == 17],
#     .groups = "drop"
#   ) %>%
#   dplyr::mutate(
#     RMSE = sqrt(
#       ((N0 - pop2005)^2 +
#          (N8 - pop2013)^2 +
#          (N17 - pop2022)^2) / 3
#     ))

## Based on 2022 population size
fit_score = pop_time %>%
  dplyr::group_by(
    Id_simul,
    dplyr::across(dplyr::all_of(params_tested))) %>%
  dplyr::summarise(
    N0 = MeanN_Tot[Year == 0],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    RMSE = sqrt((N0 - pop2022)^2))

# Rank the best options
fit_score %>%
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1:10)

## Heatmap plot
# When >2 two parameters are tested only
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

# Read and stack all ind files
inds_all = purrr::map_dfr(inds_files, function(f){
  
  # Simulation id
  sim_id = stringr::str_extract(
    basename(f),
    "(?<=Sim)\\d+(?=_Land)"
  ) %>%  as.numeric()
  
  # Read file
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

### Summary data
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

### PLOT
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
  dplyr::slice(4) # Select with slice(X) the row to keep

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
# We focus on SUCCESSFUL dispersal events
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
  # facet_grid(
  #   cols = vars(.data[[params_tested[2]]]),
  #   rows = vars(.data[[params_tested[3]]]),
  #   scales = "free_y") +
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
  # facet_grid(
  #   cols = vars(.data[[params_tested[2]]]),
  #   rows = vars(.data[[params_tested[3]]]),
  #   scales = "free_y") +
  theme_bw() +
  labs(y = "Mean number of occupied patches")

dev.off()

# Number of occupied patches
patch_occ_2100 = range_time %>% 
  dplyr::filter(Year == 70)

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

# names of the patches used for simulations (2005 map)
# i.e., patches of forests <500 m
# patch_list = c("Vendaval",
#                "Rio_Vermelho",
#                "Boa_Esperanca_II",
#                "Imbau_I_2",
#                "Sta_Helena_II",
#                "Sta_Helena_I",
#                "Sta_Helena",
#                "Aldeia_I_1",
#                "Aldeia_I_2",
#                "Poco_das_Antas",
#                "Uniao_N_2",
#                "Nova_Esperanca_2",
#                "Afetiva",
#                "Pirineus_114")

# names of the patches used for simulations (2023 map)
# i.e., patches of forests <500 m
# Check patches names in the patch file (names were give in patch_prep script)
patch_list = c("Vendaval",
               "Rio_Vermelho",
               "Boa_Esperanca_II",
               "Imbau_I_2",
               "Sta_Helena_II",
               "Sta_Helena_I",
               "Sta_Helena",
               "Aldeia_I_1",
               "Aldeia_I_2",
               "Aldeia_I_3",
               "Aldeia_I_4",
               "Poco_das_Antas",
               "Uniao_N_2",
               "Nova_Esperanca_2",
               "Afetiva",
               "Pirineus_121")

##### Population by patch -----
pop_patch = pop_all_complete %>%
  # We focus on the best parameters
  dplyr::semi_join(best_values,
                   by = params_tested) %>%
  dplyr::filter(Year != max(Year, na.rm = TRUE), # Remove the last year
                PatchID != 0) %>% # Remove the matrix 
  dplyr::group_by(PatchID,
                  Year) %>%
  # Mean population size by patch/year across replicates
  # WARNING: 
  # IF female-only model: the population only includes females -> multiply to estimate the total population
  # Sex ratio in wild populations: between 53% and 66% (Dietz and Baker 1993)
  # Sex ratio in zoos: ~56%
  # Female proportion: 100-56=44
  # Multiplier: 1/0.44 = 2.3
  # IF sex model, the population includes both males and females
  dplyr::summarise(MeanN_Ind = round(mean(NInd),1), 
                   MeanN_Juvs = round(mean(NJuvs),1),
                   MeanNTot = round((MeanN_Ind+MeanN_Juvs)*2.3,1), # Total population across replicates
                   .groups = "drop")

# Complete missing patch/year combinations
# i.e., some patches have NAs because in the original Pop files, they are discarded if they never have populations
pop_patch = pop_patch %>% 
  tidyr::complete(
    PatchID = patch_sf$PatchID,
    Year,
    fill = list(
      MeanN_Ind = 0,
      MeanN_Juvs = 0,
      MeanNTot = 0
    )
  )

# Mean pop size by patch (cut)
# i.e., indicator of group size
# Golden lion tamarins live in territorial social groups of 2 to 14 individuals 
# mean 5.4 to 6.5 individuals per group (Baker & Beck 2023)
# IMPORTANT: check that the pop number actually represents the whole population (not just FEMALES)
group_size = pop_patch %>% 
  dplyr::filter(MeanNTot != 0) %>% 
  dplyr::summarise(
    Mean_grp_size = round(mean(MeanNTot),1),
    Median_grp_size = median(MeanNTot),
    Min_grp_size = min(MeanNTot),
    Max_grp_size = max(MeanNTot)
  )
group_size

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

# # Comparison with the 2013 CENSUS
# sim_2013 = patch_sf_pop %>%
#   # Select the year of interest
#   dplyr::filter(Year == 8) %>%
#   # Mutate the occupancy state
#   dplyr::mutate(
#     sim_occupied = dplyr::case_when(MeanNTot == 0 ~ "Unoccupied",
#                                     MeanNTot > 0 ~ "Occupied")
#   ) %>%
#   dplyr::select(
#     PatchID, # RangeShifter patch id
#     Year,
#     sim_occupied,
#     geometry
#   )
# 
# # Census data
# census_2013 = glt_census %>%
#   # Select year of interest
#   dplyr::filter(!is.na(Detect2013)) %>%
#   dplyr::select(
#     ID2, # Transect id
#     Detect2013, # Detection data
#     geometry
#   )
# 
# # Spatial join
# comparison_2013 = sf::st_intersection(
#   census_2013,
#   sim_2013
#   ) %>%
#   # Compute the intersection area between the transect and the patch
#   dplyr::mutate(
#     overlap_area = as.numeric(sf::st_area(geometry)) # Intersection area
#   ) %>%
#   dplyr::group_by(ID2) %>%
#   # Some census data overlap several patches (and vice-versa)
#   # HENCE: we keep the largest intersection between the patch and the census polygon
#   dplyr::slice_max(
#     order_by = overlap_area,
#     n = 1,
#     with_ties = FALSE
#   ) %>%
#   dplyr::ungroup() %>%
#   sf::st_drop_geometry()
# 
# # We compute the proportion of patches occupied in 2013 and also occupied in simulations
# # i.e. true positive: observed P, simulated occupied
# # AND the proportion of patches unoccupied in 2013 and also unoccupied in simulations
# # i.e. true negative: observed N, simulated unoccupied
# comparison_2013 = comparison_2013 %>% 
#   dplyr::mutate(
#     match_pres = dplyr::case_when(
#       Detect2013 == "P" & sim_occupied == "Occupied"   ~ "MATCH",
#       Detect2013 == "P" & sim_occupied == "Unoccupied" ~ "UNMATCH"
#     ),
#     match_abs = dplyr::case_when(
#       Detect2013 == "N" & sim_occupied == "Unoccupied" ~ "MATCH",
#       Detect2013 == "N" & sim_occupied == "Occupied"   ~ "UNMATCH"
#     )
#   )
# 
# # Proportion
# prop_match_2013 = comparison_2013 %>%
#   dplyr::summarise(
#     n_occ = sum(Detect2013 == "P"), # Number of occurrences
#     n_match_pres = sum(match_pres == "MATCH", na.rm = TRUE), # Number of true positives
#     n_unmatch_pres = sum(match_pres == "UNMATCH", na.rm = TRUE), # Number of false negatives
#     prop_match_pres = round(n_match_pres / n_occ, 2), # % true positive
#     n_abs = sum(Detect2013 == "N"), # Number of absences
#     n_match_abs = sum(match_abs == "MATCH", na.rm = TRUE), # Number of true negatives
#     n_unmatch_abs = sum(match_abs == "UNMATCH", na.rm = TRUE), # Number of false positives
#     prop_match_abs = round(n_match_abs / n_abs, 2) # % true negatives
#   ) %>% 
#   dplyr::rename_with(~ paste0(., '_2013'))

## Comparison with the 2022 CENSUS
sim_2022 = patch_sf_pop %>%
  # Select the year of interest
  dplyr::filter(Year == 0) %>% # 0 or 17 depending on the starting year of the simulations
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

# IF 2005 is the starting point
# years_to_plot = c(0, 8, 17, 95)

# IF 2023 is the starting point
years_to_plot = c(0, 76)

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

###### 2005-2013-2022 map -----------
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

###### 2023-2100 map -----------
## 2023
# Patches
patches_0 = patch_sf_pop %>% 
  dplyr::filter(Year == 0)
# Title
title_2023 = paste0(
  "2023\n",
  "% true positive = ",
  round(prop_match_2022$prop_match_pres_2022 * 100, 1),
  "% | true negative = ",
  round(prop_match_2022$prop_match_abs_2022 * 100, 1),
  "%"
)
# Map
map2023 = ggplot() +
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
  ggtitle(title_2023) +
  theme_void()

## 2100
# Patches
patches_76 = patch_sf_pop %>% 
  dplyr::filter(Year == 76)
# Map
map2100 = ggplot() +
  tidyterra::geom_spatraster(
    data = landsc_factor,
    use_coltab = TRUE,
    show.legend = FALSE
  ) +
  # Occupied patches
  geom_sf(
    data = patches_76 %>% 
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

### Combine the two maps
maps_combined = map2023 + map2100 +
  patchwork::plot_layout(
    ncol = 2,
    nrow = 1,
    widths = c(1, 1)
  )

### Reduce space around titles and plots
maps_combined = maps_combined &
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(t = 0, b = 0)
    ),
    plot.margin = margin(0, 2, 0, 2)
  )

## Export plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "map_occ_patches.png"),
    width = 2400, height = 1000, res = 300, type="cairo")
maps_combined
dev.off()

##### Patch abundance ------
# Filter patches based on the list of patches used for simulations
pop_patch_select = pop_patch %>% 
  # Remove patches that do not belong to the list
  dplyr::filter(patch_id %in% patch_list)

## Keep years of interest
# IF calibrated on 2005-2013-2022 data
# pop_patch_select = pop_patch_select %>%
#   dplyr::filter(
#     Year %in% c(0, 8, 13, 17) # Years of interest
#   )

# IF calibrated on 2022 data
pop_patch_select = pop_patch_select %>%
  dplyr::filter(
    Year %in% c(0) # Years of interest
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

### Aggregate patches sharing the same fragment name
# Example: since Aldeia_I_1 and Aldeia_I_2 correspond to the same monitored UMMP, we sum their abundances
pop_patch_select_agg = pop_patch_select %>%
  dplyr::group_by(FragName, Year) %>%
  dplyr::summarise(
    SimN_Tot = sum(MeanNTot),
    .groups = "drop"
  )

## Reshape the real census data
# IF calibrated on 2005-2013-2022 data
# real_census_long = real_census_data %>%
#   tidyr::pivot_longer(
#     starts_with("n_glt"),
#     names_to = "Survey",
#     values_to = "RealN"
#   ) %>%
#   dplyr::mutate(
#     Year = dplyr::case_when(
#       Survey == "n_glt_2005" ~ 0,
#       Survey == "n_glt_2013" ~ 8,
#       Survey == "n_glt_2018" ~ 13,
#       Survey == "n_glt_2022" ~ 17
#     )
#   ) %>% 
#   dplyr::filter(!is.na(FragName),
#                 Survey != "n_glt_2018") # Remove the 2018 census (yellow fever)

# IF calibrated on 2022 data
real_census_long = real_census_data %>%
  tidyr::pivot_longer(
    starts_with("n_glt"),
    names_to = "Survey",
    values_to = "RealN"
  ) %>%
  dplyr::mutate(
    Year = dplyr::case_when(
      Survey == "n_glt_2022" ~ 0
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


### Plot (biplot)
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

### Patch abundance at several dates
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
# We compute the contribution from focal subpopulation r to the metapopulation (metric C; Runge et al. 2006)
# This enables distinguishing between sources and patches
# Natal patch is not necessarily the patch an individual emigrates from. An individual may have dispersed several times during its lifetime
# Population size in a patch is given by: N = Survivals + Births + Immigrants - Emigrants - Deaths(local) - Deaths(emigrants)

# We need:
# Survivals: individuals with status 0 or 4, previous patch = current patch, WITHOUT duplicated individuals (dying in the course of the year)
# Immigrants: individuals with status 4 with current patch =! previous patch
# Emigrants: individuals with status 4 with previous patch =! current patch
# Births: individuals with stage 0 and not present the year before (except for year 0)
# Deaths (local): individuals with status 8 or 9 with previous patch = current patch
# Deaths (in transfer): individuals with status 6 or 7 with previous patch =! current patch and current patch = 0 (matrix)

# WARNING: Inds and POP files record the population at different points in the simulation year.
# Inds file for year T: records individual-level demographic events during the simulation year, including recruits, individuals surviving, and individuals that die. It can therefore contain individuals that contribute to reproduction but die later in the same year.
# POP year T+1 reflects the resulting population after survival and aging, together with the new juvenile cohort reported for that reproductive cycle.
# Example: (Ind file) the population of patch A in year T has 2 local recruits + 1 subadult + 5 adults = 8
# (Pop file) the population of patch A in year T+1 has 2 of stage 1 (2 local recruits aged) + 6 of stages 2 (1 subadult aged + 5 adults) = 8 individuals
# (Pop file) it also includes 6 juveniles born immediately after reproduction which only appear in the ind file the year T+1
# THEREFORE: the individual file may have DUPLICATED inds because they are recorded AFTER survival AND AFTER aging
# WHAT CORRESPONDS IS: Births+Survivals+Immigrants-Deaths-Emigrants (Ind File, Year T) = NInd (Pop File, Year T+1)
# WHAT CORRESPONDS ALSO IS: Births+Survivals+Immigrants-Deaths-Emigrants (Ind File, Year T) + Newborns (Juveniles born (staying or leaving), Year T+1) = NInd + NJuvs (Pop File, Year T+1)

###### Cut scale (territory) ------
### Filter the dataset
inds_filter = inds_all %>%
  # Filter using the best simulations
  dplyr::semi_join(
    best_values,
    by = params_tested) %>% 
  dplyr::mutate(
    Year = as.integer(Year)
  ) %>%
  dplyr::select(
    Rep, Year, IndID,
    Status, Natal_patch, PatchID, Age, Stage,
    DistMoved, Nsteps
  )

# Add the uncut patch ID for the current location
inds_filter = inds_filter %>%
  dplyr::left_join(
    patch_corres_id,
    by = c("PatchID" = "cut_patch_id")
  )

### CHECKS
## Status column
unique(inds_filter$Status)

## Example per status
example_ids = inds_filter %>%
  dplyr::filter(Rep == 1) %>%
  dplyr::group_by(Status) %>%
  dplyr::summarise(IndID = dplyr::first(IndID), .groups = "drop") %>%
  dplyr::distinct(IndID)

examples_history = inds_filter %>%
  dplyr::filter(Rep == 1, IndID %in% example_ids$IndID) %>%
  dplyr::arrange(IndID, Year) %>%
  dplyr::select(
    Rep, IndID, Year, Status, PatchID, uncut_patch_id,
    Natal_patch, Age, Stage, DistMoved, Nsteps
  )

print(examples_history)

## Number of patches per individual
inds_filter %>%
  dplyr::group_by(Rep, IndID) %>%
  dplyr::summarise(
    n_distinct_patches = dplyr::n_distinct(PatchID, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(n_distinct_patches))

## Duplicates
# Check duplicated individuals
dup = inds_filter %>%
  dplyr::count(
    Rep, IndID, Year,
    name = "n_records"
  ) %>%
  dplyr::filter(n_records > 1)
# There are duplicated individuals

# Details about duplicated individuals
dup_details = inds_filter %>%
  dplyr::semi_join(
    dup,
    by = c("Rep", "IndID", "Year")
  ) %>%
  dplyr::select(
    Rep, IndID, Year,
    Status, PatchID, Age, Stage
  ) %>% 
  dplyr::group_by(Rep, IndID, Year) %>%
  dplyr::summarise(
    n_records = dplyr::n(),
    Diff_status = paste(sort(unique(Status)), collapse = ", "),
    .groups = "drop"
  )

# Status of duplicates
unique(dup_details$Diff_status)
# -> Duplicated individuals are individuals dying of age (reach max. age) (Status 9)

## Mutate the previous patch
# Compute the patch occupied in the previous year
previous_patches = inds_filter %>%
  dplyr::transmute(
    Rep,
    IndID,
    Year = Year + 1L,
    previous_patch = PatchID
  )

# Join the previous patch
inds_history = inds_filter %>%
  dplyr::left_join(
    previous_patches,
    by = c("Rep", "IndID", "Year")
  )

## Individuals dying of age
deaths_aging = inds_history %>%
  dplyr::filter(Status == 9) %>%
  dplyr::distinct(Rep, Year, IndID)

## Individuals born and dispersing in the same year
same_year_dispersers = inds_history %>%
  dplyr::filter(
    Status == 4,
    is.na(previous_patch),
    !is.na(Natal_patch),
    PatchID != Natal_patch
  ) %>%
  dplyr::distinct(Rep, Year, IndID, .keep_all = TRUE)

## Helper: count juveniles and adults in wide format
# WARNING: use this helper where it makes sense
# For instance, due to Juv survival = 1 in the pop matrix, no juvenile will die from one year to another in a local patch -> does not make sense to apply it to local survivals
count_by_stage = function(data, prefix) {
  data %>%
    dplyr::group_by(Rep, Year, PatchID) %>%
    dplyr::summarise(
      !!paste0(prefix, "_juv") :=
        dplyr::n_distinct(IndID[Stage == 0], na.rm = TRUE),
      !!paste0(prefix, "_adult") :=
        dplyr::n_distinct(IndID[Stage %in% c(1, 2)], na.rm = TRUE),
      .groups = "drop"
    )
}

## Local survivors
# Survivals: individuals surviving in the same patch from one year to another
survivors = inds_history %>%
  dplyr::filter(
    Status %in% c(0, 4),
    previous_patch == PatchID
  ) %>%
  # Excluding individuals dying from aging
  dplyr::anti_join(
    deaths_aging,
    by = c("Rep", "Year", "IndID")
  ) %>%
  dplyr::group_by(Rep, Year, PatchID) %>%
  dplyr::summarise(
    n_surv = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Local births
# Births: individuals appearing over the course of the simulations in a patch
births = inds_history %>%
  dplyr::filter(Stage == 0, PatchID != 0) %>%
  # Excluding individuals born and dispersing the same year
  dplyr::anti_join(
    same_year_dispersers %>% dplyr::distinct(Rep, Year, IndID),
    by = c("Rep", "Year", "IndID")
  ) %>% 
  # Identify first appearance of each individual
  dplyr::group_by(Rep, IndID) %>%
  dplyr::filter(Year == min(Year, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Rep, Year, PatchID) %>%
  dplyr::summarise(
    n_births = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Identify movement events
movement_events = inds_history %>%
  dplyr::filter(DistMoved > 0) %>%
  dplyr::mutate(
    origin_patch = dplyr::coalesce(previous_patch, Natal_patch)
  ) %>%
  dplyr::filter(
    !is.na(origin_patch),
    !is.na(PatchID),
    PatchID != 0,
    origin_patch != PatchID
  ) %>%
  dplyr::distinct(
    Rep, Year, IndID, origin_patch, PatchID, Stage
  )

## Immigrants
# Immigrants: individuals arriving in a patch B from patch A
immigration_ids = movement_events %>%
  dplyr::distinct(Rep, Year, IndID, PatchID, Stage)
immigrants = count_by_stage(immigration_ids, "n_imm")

## Immigrants dying in the destination patch in the same year
immigrants_dying_ids = movement_events %>%
  dplyr::inner_join(
    inds_history %>%
      dplyr::filter(Status %in% c(8, 9)) %>%
      dplyr::select(Rep, Year, IndID, PatchID, Stage),
    by = c("Rep", "Year", "IndID", "PatchID"),
    suffix = c("_movement", "_death")
  ) %>%
  dplyr::transmute(
    Rep,
    Year,
    IndID,
    PatchID,
    Stage = Stage_death
  ) %>%
  dplyr::distinct(Rep, Year, IndID, PatchID, Stage)
immigrants_dying = count_by_stage(
  immigrants_dying_ids,
  "n_imm_dying"
)

## Emigrants
# Emigrants: individuals arriving in a patch A from patch B
emigration_ids = movement_events %>%
  dplyr::transmute(
    Rep,
    Year,
    IndID,
    PatchID = origin_patch,
    Stage
  ) %>%
  dplyr::distinct(Rep, Year, IndID, PatchID, Stage)
emigrants = count_by_stage(emigration_ids, "n_emi")

## Deaths during transfer
deaths_transfer_ids = inds_history %>%
  dplyr::filter(
    Status %in% c(6, 7),
    PatchID == 0
  ) %>%
  dplyr::mutate(
    PatchID = dplyr::coalesce(previous_patch, Natal_patch)
  ) %>%
  dplyr::filter(!is.na(PatchID), PatchID != 0) %>%
  dplyr::distinct(Rep, Year, IndID, PatchID, Stage)
deaths_transfer = count_by_stage(
  deaths_transfer_ids,
  "n_deaths_transfer"
)

## Local deaths
# Local deaths: individuals dying in the patch
deaths_local = inds_history %>% 
  # Keep individuals with status 8 or 9 (local deaths)
  dplyr::filter(
    Status %in% c(8, 9)
  ) %>%
  # Excluding immigrants who die in the same year
  dplyr::anti_join(
    immigrants_dying_ids %>%
      dplyr::select(Rep, Year, PatchID, IndID),
    by = c("Rep", "Year", "PatchID", "IndID")
  ) %>%
  # Total number
  dplyr::group_by(Rep, Year, PatchID) %>%
  dplyr::summarise(
    n_deaths_local= dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Merge all demographic tables
patch_demography = list(
  survivors,
  births,
  deaths_local,
  deaths_transfer,
  immigrants,
  immigrants_dying,
  emigrants
) %>%
  purrr::reduce(
    dplyr::full_join,
    by = c("Rep", "Year", "PatchID")
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("n_"),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  dplyr::filter(PatchID != 0)

# Count pop size
patch_demography = patch_demography %>%
  dplyr::mutate(
    N = n_surv + n_births + n_imm_juv - n_imm_dying_juv + n_imm_adult - n_imm_dying_adult
  )


## Check the correspondence between inds files and population files
# Population files
pop_patch_cut = pop_all_complete %>%
  # Keep the best simulation
  dplyr::semi_join(
    best_values,
    by = params_tested) %>%
  # Remove the matrix
  dplyr::filter(
    PatchID != 0
    ) %>% 
  dplyr::group_by(Rep, Year, PatchID) %>%
  dplyr::summarise(
    NInd = sum(NInd, na.rm = TRUE),
    NJuvs = sum(NJuvs, na.rm = TRUE),
    NTot = NInd + NJuvs,
    .groups = "drop"
  )

# Join population sizes and compare both relationships
patch_demography_check = patch_demography %>%
  dplyr::mutate(
    Year_pop = Year + 1L
  ) %>%
  # Population output for year T+1
  dplyr::left_join(
    pop_patch_cut %>%
      dplyr::rename(Year_pop = Year),
    by = c("Rep", "Year_pop", "PatchID")
  ) %>%
  # Births recorded in year T+1
  dplyr::left_join(
    births %>%
      dplyr::transmute(
        Rep,
        Year_pop = Year,
        PatchID,
        n_births_next = n_births
      ),
    by = c("Rep", "Year_pop", "PatchID")
  ) %>%
  # Juveniles emigrating during year T+1, counted in their origin patch
  dplyr::left_join(
    patch_demography %>%
      dplyr::transmute(
        Rep,
        Year_pop = Year,
        PatchID,
        n_emi_juv_next = n_emi_juv
      ),
    by = c("Rep", "Year_pop", "PatchID")
  ) %>%
  # Juveniles dying during transfer in year T+1, attributed to origin patch
  dplyr::left_join(
    patch_demography %>%
      dplyr::transmute(
        Rep,
        Year_pop = Year,
        PatchID,
        n_deaths_transfer_juv_next = n_deaths_transfer_juv
      ),
    by = c("Rep", "Year_pop", "PatchID")
  ) %>%
  dplyr::mutate(
    n_births_next = tidyr::replace_na(n_births_next, 0),
    n_emi_juv_next = tidyr::replace_na(n_emi_juv_next, 0),
    n_deaths_transfer_juv_next =
      tidyr::replace_na(n_deaths_transfer_juv_next, 0),
    
    # Comparison 1: year T demographic balance vs NInd in year T+1
    diff_NInd = N - NInd,
    
    # Comparison 2: add births and emigrating juvenile in year T+1
    N_next = N +
      n_births_next +
      n_emi_juv_next +
      n_deaths_transfer_juv_next,
    
    # Compare with total population in POP year T+1
    diff_NTot = N_next - NTot,
    
    correspondence_NInd = dplyr::if_else(
      diff_NInd == 0, "Match", "Mismatch"
    ),
    
    correspondence_NTot = dplyr::if_else(
      diff_NTot == 0, "Match", "Mismatch"
    )
  ) %>%
  dplyr::filter(Year != 0)

# Summarize
patch_demography_check %>%
  dplyr::summarise(
    n_patches = dplyr::n(),
    
    matches_NInd = sum(correspondence_NInd == "Match"),
    mismatches_NInd = sum(correspondence_NInd == "Mismatch"),
    
    matches_NTot = sum(correspondence_NTot == "Match"),
    mismatches_NTot = sum(correspondence_NTot == "Mismatch")
  )
# -> The pop size counted from individual files and NInd from pop files match
# -> The pop size counted from individual files and NTot from pop files match
# THIS MEANS THAT: total population size (adults + juveniles) in pop file in T+1 equals: 
# the number of individuals in the patch + the juveniles born the next year (staying and emigrating)

### Mean across replicates
patch_demography_mean = patch_demography %>%
  dplyr::group_by(Year, PatchID) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::starts_with("n_"),
      ~ round(mean(.x, na.rm = TRUE),1)
    ),
    N = round(mean(N, na.rm = TRUE),1),
    .groups = "drop"
  )

###### Uncut scale (real patch) ------
# Movements between cut patches within the same uncut patch can be counted as survivors at the uncut scale.
# We only need to count movements that cross an uncut-patch boundary:
# - individuals entering an uncut patch from another uncut patch.
# - individuals leaving an uncut patch for another uncut patch.
# - Intra-uncut movements are not counted separately as immigration or emigration.

### Filter the dataset
inds_filter_uncut = inds_all %>%
  # Keep best simulation
  dplyr::semi_join(
    best_values,
    by = params_tested
  ) %>%
  dplyr::mutate(
    Year = as.integer(Year)
  ) %>%
  dplyr::select(
    Rep, Year, IndID,
    Status, Natal_patch, PatchID, Age, Stage,
    DistMoved, Nsteps
  ) %>%
  # Add the uncut patch corresponding to the current cut patch
  dplyr::left_join(
    patch_corres_id,
    by = c("PatchID" = "cut_patch_id")
  )

## Previous cut patch and previous uncut patch
previous_patches_uncut = inds_filter_uncut %>%
  dplyr::transmute(
    Rep,
    IndID,
    Year = Year + 1L,
    previous_patch = PatchID,
    previous_uncut_patch_id = uncut_patch_id
  )
inds_history_uncut = inds_filter_uncut %>%
  dplyr::left_join(
    previous_patches_uncut,
    by = c("Rep", "IndID", "Year")
  )

## Individuals dying of age
deaths_aging_uncut = inds_history_uncut %>%
  # Status 9 means it reached max age
  dplyr::filter(Status == 9) %>%
  dplyr::distinct(Rep, Year, IndID)

## Individuals born and dispersing in the same year
same_year_dispersers_uncut = inds_history_uncut %>%
  # Individuals successfully settling in a patch but with different Natal and Patch IDs
  dplyr::filter(
    Status == 4,
    is.na(previous_patch),
    !is.na(Natal_patch),
    PatchID != Natal_patch
  ) %>%
  dplyr::distinct(Rep, Year, IndID, .keep_all = TRUE) %>%
  dplyr::mutate(
    natal_patch = Natal_patch
  ) %>%
  dplyr::left_join(
    patch_corres_id %>%
      dplyr::transmute(
        natal_patch = cut_patch_id,
        natal_uncut_patch_id = uncut_patch_id
      ),
    by = "natal_patch"
  )


## Local survivors
# survivors are individuals who remain in the same uncut patch, regardless of which cut patch they occupy
survivors_uncut = inds_history_uncut %>%
  # Survivors = individuals born here (0) or settled (4)
  # With the same previous patch
  dplyr::filter(
    Status %in% c(0, 4),
    !is.na(previous_uncut_patch_id),
    previous_uncut_patch_id == uncut_patch_id,
    !is.na(uncut_patch_id)
  ) %>%
  # Remove those dying of age the same year
  dplyr::anti_join(
    deaths_aging_uncut,
    by = c("Rep", "Year", "IndID")
  ) %>%
  dplyr::group_by(Rep, Year, uncut_patch_id) %>%
  dplyr::summarise(
    n_surv = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Births
births_uncut = inds_history_uncut %>%
  # Juveniles (stage 0)
  dplyr::filter(
    Stage == 0,
    PatchID != 0,
    !is.na(uncut_patch_id)
  ) %>%
  # Count individuals who dispersed after being born
  dplyr::left_join(
    same_year_dispersers_uncut %>%
      dplyr::select(
        Rep, Year, IndID, natal_uncut_patch_id
      ),
    by = c("Rep", "Year", "IndID")
  ) %>%
  dplyr::filter(
    is.na(natal_uncut_patch_id) |
      natal_uncut_patch_id == uncut_patch_id
  ) %>%
  dplyr::group_by(Rep, IndID) %>%
  # First occurrence of the individual
  dplyr::filter(Year == min(Year, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Rep, Year, uncut_patch_id) %>%
  dplyr::summarise(
    n_births = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Movements between different uncut patches only
movement_events_uncut = inds_history_uncut %>%
  # DistMoved indicates a dispersal event
  dplyr::filter(DistMoved > 0) %>%
  dplyr::mutate(
    origin_patch = dplyr::coalesce(previous_patch, Natal_patch)
  ) %>%
  dplyr::left_join(
    patch_corres_id %>%
      dplyr::rename(
        origin_patch = cut_patch_id,
        origin_uncut_patch_id = uncut_patch_id
      ),
    by = "origin_patch"
  ) %>%
  dplyr::filter(
    !is.na(origin_uncut_patch_id),
    !is.na(uncut_patch_id),
    !is.na(origin_patch),
    !is.na(PatchID),
    PatchID != 0,
    origin_uncut_patch_id != uncut_patch_id
  ) %>%
  dplyr::distinct(
    Rep, Year, IndID,
    origin_uncut_patch_id, uncut_patch_id,
    .keep_all = TRUE
  )

## Immigrants: entering an uncut patch from another uncut patch
immigrants_uncut = movement_events_uncut %>%
  dplyr::group_by(Rep, Year, uncut_patch_id) %>%
  dplyr::summarise(
    n_imm_inter_uncut = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Immigrants dying in their destination uncut patch
immigrants_dying_ids_uncut = movement_events_uncut %>%
  dplyr::inner_join(
    inds_history_uncut %>%
      dplyr::filter(
        # Dead individuals
        Status %in% c(8, 9),
        !is.na(uncut_patch_id)
      ) %>%
      dplyr::select(
        Rep, Year, IndID, uncut_patch_id
      ) %>%
      dplyr::distinct(),
    by = c("Rep", "Year", "IndID", "uncut_patch_id")
  ) %>%
  dplyr::distinct(
    Rep, Year, IndID, uncut_patch_id
  )

immigrants_dying_uncut = immigrants_dying_ids_uncut %>%
  dplyr::group_by(Rep, Year, uncut_patch_id) %>%
  dplyr::summarise(
    n_imm_dying = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Emigrants: leaving an uncut patch for another uncut patch
emigrants_uncut = movement_events_uncut %>%
  dplyr::group_by(
    Rep, Year,
    uncut_patch_id = origin_uncut_patch_id
  ) %>%
  dplyr::summarise(
    n_emi_inter_uncut = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Deaths during transfer, assigned to origin uncut patch
deaths_transfer_ids_uncut = inds_history_uncut %>%
  dplyr::filter(
    # Death during transfer in the matrix (patch 0)
    Status %in% c(6, 7),
    PatchID == 0
  ) %>%
  dplyr::mutate(
    origin_patch = dplyr::coalesce(previous_patch, Natal_patch)
  ) %>%
  dplyr::left_join(
    patch_corres_id %>%
      dplyr::rename(
        origin_patch = cut_patch_id,
        uncut_patch_origin = uncut_patch_id
      ),
    by = "origin_patch"
  ) %>%
  dplyr::filter(!is.na(uncut_patch_origin)) %>%
  dplyr::transmute(
    Rep, Year, IndID,
    uncut_patch_id = uncut_patch_origin
  ) %>%
  dplyr::distinct(Rep, Year, IndID, uncut_patch_id)

deaths_transfer_uncut = deaths_transfer_ids_uncut %>%
  dplyr::group_by(Rep, Year, uncut_patch_id) %>%
  dplyr::summarise(
    n_deaths_transfer = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Local deaths, excluding immigrants dying in the destination
deaths_local_uncut = inds_history_uncut %>%
  dplyr::filter(
    # Death of age
    Status %in% c(8, 9),
    !is.na(uncut_patch_id)
  ) %>%
  # Excluding immigrants dying in the destination
  dplyr::anti_join(
    immigrants_dying_ids_uncut %>%
      dplyr::select(Rep, Year, IndID, uncut_patch_id),
    by = c("Rep", "Year", "IndID", "uncut_patch_id")
  ) %>%
  dplyr::group_by(Rep, Year, uncut_patch_id) %>%
  dplyr::summarise(
    n_deaths_local = dplyr::n_distinct(IndID),
    .groups = "drop"
  )

## Build the table
patch_demography_uncut = list(
  survivors_uncut,
  births_uncut,
  deaths_local_uncut,
  deaths_transfer_uncut,
  immigrants_uncut,
  immigrants_dying_uncut,
  emigrants_uncut
) %>%
  purrr::reduce(
    dplyr::full_join,
    by = c("Rep", "Year", "uncut_patch_id")
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("n_"),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  dplyr::filter(
    !is.na(uncut_patch_id),
    Year != 0
  ) %>%
  dplyr::mutate(
    N = n_surv +
      n_births +
      n_imm_inter_uncut -
      n_imm_dying
  )

## Check 
# Pop files
pop_patch_uncut = pop_all_complete %>%
  # Keep the best simulation
  dplyr::semi_join(
    best_values,
    by = params_tested) %>%
  # Remove the matrix
  dplyr::filter(
    PatchID != 0
  ) %>% 
  dplyr::left_join(
    patch_corres_id,
    by = c("PatchID" = "cut_patch_id")
  ) %>%
  dplyr::group_by(Rep, Year, uncut_patch_id) %>%
  dplyr::summarise(
    NInd = sum(NInd, na.rm = TRUE),
    NJuvs = sum(NJuvs, na.rm = TRUE),
    NTot = NInd + NJuvs,
    .groups = "drop"
  )

# Join population sizes and compare both relationships at uncut-patch level
patch_demography_check_uncut = patch_demography_uncut %>%
  dplyr::mutate(
    Year_pop = Year + 1L
  ) %>%
  # Population output for year T+1
  dplyr::left_join(
    pop_patch_uncut %>%
      dplyr::rename(Year_pop = Year),
    by = c("Rep", "Year_pop", "uncut_patch_id")
  ) %>%
  dplyr::mutate(
    diff_NInd = N - NInd,
    correspondence_NInd = dplyr::if_else(
      diff_NInd == 0, "Match", "Mismatch"
    )
  ) %>%
  dplyr::filter(Year != 0)

# Summarize
patch_demography_check_uncut %>%
  dplyr::summarise(
    n_patches = dplyr::n(),
    matches_NInd = sum(correspondence_NInd == "Match", na.rm = TRUE),
    mismatches_NInd = sum(correspondence_NInd == "Mismatch", na.rm = TRUE)
  )

### Mean values across replicates
dplyr::group_vars(patch_demography_uncut) # Check
patch_demography_uncut_mean = patch_demography_uncut %>% 
  dplyr::group_by(Year, uncut_patch_id) %>% 
  dplyr::summarise(
    dplyr::across(
      dplyr::starts_with("n_"),
      ~ round(mean(.x, na.rm = TRUE), 0)
    ),
    N = round(mean(N, na.rm = TRUE), 0),
    .groups = "drop"
  )

###### Basic source/sink -----
# Following Pulliam (1988)
# Compute lambda
patch_source_sink = patch_demography_uncut_mean %>%
  dplyr::arrange(uncut_patch_id, Year) %>%
  dplyr::group_by(uncut_patch_id) %>%
  dplyr::mutate(
    lambda = dplyr::if_else(
      N > 0 & !is.na(dplyr::lead(N)),
      dplyr::lead(N) / N,
      NA_real_
    ),
    k = lambda - 1
  ) %>%
  dplyr::ungroup()

# Basic annual source/sink classification
patch_source_sink = patch_source_sink %>% 
  dplyr::group_by(uncut_patch_id) %>%
  dplyr::mutate(
    annual_status = dplyr::case_when(
      (n_births > n_deaths_local &
         n_emi_inter_uncut > n_imm_inter_uncut) |
        lambda > 1 ~ "Source",
      
      (n_births < n_deaths_local &
         n_imm_inter_uncut < n_emi_inter_uncut) |
        lambda < 1 ~ "Sink",
      
      TRUE ~ "Balanced"
    )
  ) %>%
  dplyr::ungroup()
table(patch_source_sink$annual_status)

# Overall classification per patch across years
patch_source_sink = patch_source_sink %>%
  dplyr::group_by(uncut_patch_id) %>%
  dplyr::summarise(
    classification = dplyr::case_when(
      all(annual_status == "Source") ~ "Unconditional source",
      all(annual_status == "Sink") ~ "Unconditional sink",
      
      any(annual_status == "Source") &
        any(annual_status == "Sink") &
        sum(annual_status == "Source") > sum(annual_status == "Sink") ~
        "Conditional source",
      
      any(annual_status == "Source") &
        any(annual_status == "Sink") &
        sum(annual_status == "Sink") > sum(annual_status == "Source") ~
        "Conditional sink",
      
      TRUE ~ "Balanced"
    ),
    .groups = "drop"
  )
table(patch_source_sink$classification)

### MAP
patch_map = patch_uncut %>%
  dplyr::left_join(
    patch_corres_id %>%
      dplyr::distinct(uncut_patch_name, uncut_patch_id) %>%
      dplyr::left_join(
        patch_source_sink,
        by = "uncut_patch_id"
      ) %>%
      dplyr::select(uncut_patch_name, classification) %>%
      dplyr::distinct(uncut_patch_name, .keep_all = TRUE),
    by = c("patch_id" = "uncut_patch_name")
  )

# Plot source/sink status
ggplot2::ggplot(patch_map) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = classification),
    colour = "white",
    linewidth = 0.2
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Unconditional source" = "#238443",
      "Conditional source"   = "#78C679",
      "Unconditional sink"   = "#CB181D",
      "Conditional sink"     = "#FB6A4A",
      "Unstable (source-sink alternance)" = "#984EA3",
      "Balanced" = "yellow"
    ),
    na.value = "grey80"
  ) +
  ggplot2::labs(
    fill = "Patch status",
    title = "Source–sink classification by patch"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank()
  )

###### C metric -----
# See Runge et al. 2006, The Role of Local Populations within a Landscape Context: Defining and Classifying Sources and Sinks, The American Naturalist
# See Newby et al. 2013, Human-caused mortality influences spatial population dynamics: Pumas in landscapes with varying mortality risks, Biological Conservation


#### Heatmaps ----
# Because movement is stochastic, the number of visits per cell and the colonisation of empty patches are different for each replicate. 
# In order to take into account this variance, we average over all replicates and plot the resulting heatmap
# create a raster stack with all replicates as layers

# Load heatmaps
heatmap_files = paste0(
  dirpath,
  "Output_Maps/Batch1_Sim",
  best_sim, # Heatmaps of the best simulation only
  "_Land1_Rep",
  0:19,
  "_Visits.txt"
)

heatmaps_stack = terra::rast(heatmap_files)

# average over all layers
heatmaps_mean = terra::mean(heatmaps_stack)

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

### IF calibrated with 2005-2013-2022 data
## Assign each year to a period
# data.disp.cut = data.disp.cut %>%
#   dplyr::mutate(
#     Period = dplyr::case_when(
#       Year >= 0  & Year <= 8  ~ "Year 0-8",
#       Year > 8  & Year <= 17 ~ "Year 8-17",
#       Year > 17 & Year < 96  ~ "Year 17-95",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   dplyr::filter(!is.na(Period))

### IF calibrated with 2022 data
## Assign each year to a period
data.disp.cut = data.disp.cut %>%
  dplyr::mutate(
    Period = dplyr::case_when(
      Year >= 0  & Year <= 76  ~ "Year 0-76",
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
###### 2005-2013-2022-2100 maps -------
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

###### 2023-2100 maps -------
# 2022-2100
map_disp_cut_2023_2100 = ggplot2::ggplot() +
  
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
        Period == "Year 0-76"
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
  
  ggplot2::ggtitle("Dispersal flux (2023-2100)") +
  
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none"
  )

## Export plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "map_disp_cut_patches.png"),
    width = 1500, height = 1000, res = 300, type="cairo")
map_disp_cut_2023_2100
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

### IF calibrated with 2005-2013-2022 data
## Assign each year to a period
# data.disp.ummps = data.disp.ummps %>%
#   dplyr::mutate(
#     Period = dplyr::case_when(
#       Year >= 0  & Year <= 8  ~ "Year 0-8",
#       Year > 8  & Year <= 17 ~ "Year 8-17",
#       Year > 17 & Year < 96 ~ "Year 17-95",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   dplyr::filter(!is.na(Period))

### IF calibrated with 2022 data
## Assign each year to a period
data.disp.ummps = data.disp.ummps %>%
  dplyr::mutate(
    Period = dplyr::case_when(
      Year >= 0  & Year <= 76  ~ "Year 0-76",
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

### MAPS

###### 2005-2013-2022-2100 maps -----
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
  
  # # Observed dispersal
  # ggplot2::geom_sf(
  #   data = glt_disp,
  #   colour = "orange",
  #   linewidth = 0.2,
  #   alpha = 0.9
  # ) +
  
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
  
  # # Observed dispersal
  # ggplot2::geom_sf(
  #   data = glt_disp,
  #   colour = "orange",
  #   linewidth = 0.2,
  #   alpha = 0.9
  # ) +
  
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
  
  # # Observed dispersal
  # ggplot2::geom_sf(
  #   data = glt_disp,
  #   colour = "orange",
  #   linewidth = 0.2,
  #   alpha = 0.9
  # ) +
  
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

###### 2023-2100 maps -----
map_disp_uncut_2023_2100 = ggplot2::ggplot() +
  
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
        Period == "Year 0-76") ,
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
  
  # One panel per simulation
  # ggplot2::facet_wrap(~Id_simul) +
  
  # Width of simulated lines
  ggplot2::scale_linewidth_continuous(
    trans = "log1p",
    range = c(0.05, 1),
    name = "Number of dispersers"
  ) +
  
  ggtitle("Dispersal flux (2023-2100)") +
  
  ggplot2::theme_void() +
  
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "none"
  )

## Export plot
png(here::here("data",
               "rangeshifter",
               "tests",
               test_config$test_name,
               "plot",
               "map_disp_uncut_patches.png"),
    width = 1500, height = 1000, res = 300, type="cairo")
map_disp_uncut_2023_2100
dev.off()


### Append results -----
# Load an Excel sheet with the parameters to test
summary = read_excel(here::here("data", 
                                "rangeshifter", 
                                "tests", 
                                "Summary_tests.xlsx"))

#### Information to add manually ----

test_aim = "Tests IndsHaCell using the 2023 map and 70 HA TERRITORIES with NEW DISTRIB. MAP (added territories to Pirineus)"
test_remarks = "IndsHaCell = 0.11"

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

### 2005-2013-2022-2100 pop sizes
# Extract pop sizes for the combination of parameters
# optimal_pop = pop_time %>%
#   dplyr::semi_join(
#     best_values,
#     by = params_tested)
# pop_sizes = optimal_pop %>%
#   dplyr::filter(Year %in% c(0, 8, 17, 95)) %>%
#   dplyr::select(Year, MeanN_Tot) # Select the proper indicator
# pop_2005 = pop_sizes$MeanN_Tot[pop_sizes$Year == 0]
# pop_2013 = pop_sizes$MeanN_Tot[pop_sizes$Year == 8]
# pop_2022 = pop_sizes$MeanN_Tot[pop_sizes$Year == 17]
# pop_2100 = pop_sizes$MeanN_Tot[pop_sizes$Year == 95]

### 2023-2100 pop sizes
# Extract pop sizes for the combination of parameters
optimal_pop = pop_time %>%
  dplyr::semi_join(
    best_values,
    by = params_tested)
pop_sizes = optimal_pop %>%
  dplyr::filter(Year %in% c(0, 76)) %>%
  dplyr::select(Year, MeanN_Tot) # Select the proper indicator
pop_2022 = pop_sizes$MeanN_Tot[pop_sizes$Year == 0]
pop_2100 = pop_sizes$MeanN_Tot[pop_sizes$Year == 76]

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
  
  # 'Pop_size_2005' = pop_2005,
  # 'Pop_size_2013' = pop_2013,
  'Pop_size_2022' = pop_2022,
  'Pop_size_2100' = pop_2100,
  
  'RMSE_pop_size' = rmse_pop_size,
  
  'Patch_occ_2100' = patch_occ_2100 %>% 
    dplyr::semi_join(best_values, by = params_tested) %>% 
    dplyr::select(MeanNOccupPatches) %>% 
    dplyr::pull(1),
  # 'Occ_true_pos_2013%' = prop_match_2013$prop_match_pres_2013,
  'Occ_true_pos_2022%' = prop_match_2022$prop_match_pres_2022,
  # 'Occ_true_neg_2013%' = prop_match_2013$prop_match_abs_2013,
  'Occ_true_neg_2022%' = prop_match_2022$prop_match_abs_2022,
  
  'Mean_grp_size' = group_size$Mean_grp_size,
  'Median_grp_size' = group_size$Median_grp_size,
  'Min_grp_size' = group_size$Min_grp_size,
  'Max_grp_size' = group_size$Max_grp_size,
  
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
