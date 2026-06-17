#------------------------------------------------#
# Author: Romain Monassier
# Objective: Running RangeShiftR simulations
#------------------------------------------------#

### BEFORE RUNNING EACH SIMULATION, UPDATE DIRPATH

### Load packages ------
library(RangeShiftR)
library(here)
library(purrr)
library(ggplot2)
library(readxl)

### Install RangeShiftR -----
# pak::pak("RangeShifter/RangeShiftR-pkg/RangeShiftR@main") # REQUIRES RTOOLS (4.4 here)

### Basics --------
# Within the standard workflow of RangeShiftR, a simulation is defined by:
# 1) a so-called parameter master object that contains the simulation modules, which represent the model structure, as well as the (numeric) values of all necessary simulation parameters, and
# 2) the path to the working directory on the disc where the simulation inputs and outputs are stored.

# The standard workflow of RangeShiftR is to load input maps from ASCII raster files
# AND to write all simulation output into text files. 

# Therefore, the specified working directory needs to have a certain folder structure: 
# It should contain 3 sub-folders named ‘Inputs’, ‘Outputs’ and ‘Output_Maps’.

## RS is made of a number of parameter modules that each define different aspects of the RangeShifter simulation. Specifically, there are:
# 1) Simulation
# 2) Landscape
# 3) Demography
# 4) Dispersal
# 5) Genetics
# 6) Management
# 7) Initialisation

## Results = 8 or 9 (patch-based) outputs
# output files are named with a standard name reporting the simulation number and the type of output
# 1) Parameters
# 2) Range with replicate number (Rep), year, reproductive season within the year, number of inds (NInds) and in each stage (NInd_stageX), number of juveniles (NJuvs), number of cells/patches (NOccupX) occupied by a population capable of breeding, ratio between occupied and suitable cells/patches (OccupSuit), species range (min_X, etc.)
# 3) Occupancy (proba. of occupancy of cells/patches) with two files "Occupancy" (list of cells/patches qui occupancy probability) + "Occupancy_Stats" (mean ratio between occupied ans suitable cells and its standard error)
# 4) Populations with replicate number, year, reproductive season, location (cell/patch ID), number of individuals in the population (NInd), number of individuals in each stage (alternatively, Nfemales and Nmales), number of juveniles
# 5) Individuals (i.e., information about each individual) with individual id, status, natal cell and current cell, sex, age, stage, emogration traits, dispersal traits, distance moved in m, n steps taken for movement models
# 6) Genetics (full genome of individuals) which can be extremely large
# 7) Traits
# 8) Connectivity matrix (for patch-based model only): number of individuals successfully dispersing from each patch to another (StartPatch -> EndPatch ; NInds)
# 9) Heatmaps (if SMS)

### Check the functions -----
# ?ImportedLandscape
# ?ArtificialLandscape
# ?StageStructure
# ?Emigration
# ?Transfer
# ?DispersalKernel
# ?Initialise

### Director path ----

dirpath = "data/rangeshifter/tests/test_densdep_preYF_5/" # UPDATE HERE !!!
## Create the RS folder structure, if it doesn’t yet exist
# dir.create(file.path(dirpath, "Inputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Outputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Output_Maps"), showWarnings = TRUE)

### Check the landscape -----
## Import a landscape
landsc = terra::rast(file.path(dirpath, "Inputs", "raster_reclass_binary_2005.txt"))
terra::plot(landsc, col=c("gray","darkgreen","orange"))
terra::res(landsc)
terra::unique(landsc)

## Patches
patch = terra::rast(file.path(dirpath, "Inputs", "patches_2005.txt"))
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

## Species distribution
patch_w_glt  = terra::rast(file.path(dirpath, "Inputs", "patches_w_glt_2005.txt"))
terra::plot(patch_w_glt, col=c("gray","darkgreen"))
terra::unique(patch_w_glt)


### Determine 1/b -------
# To explore the potential effect of b on equilibrium population sizes before running the simulation, use getLocalisedEquilPop(). 
# This allows a better understanding of reasonable values of 1/b for the different land types. 
# getLocalisedEquilPop() runs a quick simulation of a closed and localised population (i.e. without dispersal and in a single idealised patch) for a given vector of potential 1/b values (argument DensDep_values) and based on our defined Demography() module.
# The getLocalisedEquilPop() function uses absolute values of individuals in the local population (since there is not spatial extent).
# WHILE what we need are relative values of 1/b as individuals per hectare.
# We choose to simulate a hypothetical patch of our landscape, and want to determine the value of 1/b that is needed to observe e.g. 100 individuals in 1 ha (e.g., our empirical observation of how many individuals were maximally observed in woodland patches)
# Thus, we aim to find the 1/b parameter that would yield a maximum local population abundance of 100 individuals. 
# We can now assess the localised equilibrium population size for different values of 1/b and see how the density dependence plays out.

# Pop. matrix
mat = matrix(c(0, 0, 2,
               1, 0, 0,
               0, 0.56, 0.92),
             nrow=3, byrow=T)
# Stage structure
stg = StageStructure(Stages = 3,
                     TransMatrix = mat,
                     MaxAge = 20,
                     RepSeasons = 1,
                     SurvSched = 1, # Between reproductive events
                     FecDensDep = T,
                     DevDensDep = F,
                     SurvDensDep = F)
# Demography module
demo = Demography(StageStruct = stg,
                  ReproductionType = 0)

# Maximum individuals observed in a forest patch (see: Ruiz-Miranda et al. 2019)
?getLocalisedEquilPop
par(mfrow=c(1,1))
eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.095, 0.1, 0.2, 0.3)) #  absolute values of individuals
# Select the value that reaches the desired threshold
colSums(eq_pop)


### SENSITIVITY ANALYSIS ----
# Run simulations for several values for given parameters

#### Validation dataset ----
real_data = read_excel(here("data", "glt", "JDietz", "GLT_POP_2005_2023.xlsx"), na="NA")
real_data %>%
  dplyr::summarise(dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

#### Correspondence table ----
patch_corres_id = readr::read_csv(here("data", 
                                     "rangeshifter", 
                                     "tests", 
                                     "test_densdep_preYF_5", # UPDATE HERE
                                     "Inputs", 
                                     "patch_corres_id_2005.csv"),
                                col_types = readr::cols(unique_id = readr::col_integer()))

#### Parameters file ----
metadata = read_excel(here("data", 
                           "rangeshifter", 
                           "tests", 
                           "test_densdep_preYF_5",  # UPDATE HERE !!!
                           "test_parameters.xlsx"),
                      sheet="test1")

#### Loop -----
for(i in 1:nrow(metadata)) {
  
  densdep = metadata$DensDep[i]
  indshacell = metadata$IndsHaCell[i]
  ad_survival = metadata$Ad_survival[i]
  juv_survival = metadata$Juv_survival[i]
  id_simulation = metadata$Id_simul[i]
  
  # Print progression
  cat("\n========================================\n")
  cat(sprintf("Simulation %d/%d (ID: %d)\n",
              i, nrow(metadata), id_simulation))
  cat(sprintf("  DensDep = %.3f | Juv suvival = %.3f | Adult suvival = %.3f | IndsHaCell = %.3f\n",
              densdep, juv_survival, ad_survival, indshacell))
  cat("========================================\n\n")
  
  ##### 1) Simulation -----
  # This module is used to set general simulation parameters (e.g. simulation ID, number of replicates, and number of years to simulate) and to control output types (plus some more specific settings).
  sim = Simulation(Simulation = id_simulation, # Update simulation id
                   Replicates = 20, # Number of replicates
                   Years = 95, # Number of years
                   OutIntPop = 1, # Whether to export population files 
                   OutIntOcc = 1, # Whether to export occupancy files
                   OutIntRange = 0) # Whether to export range files
  
  ##### 2) Landscape -----
  # K_or_DensDep: determines the demographic density dependence of the modelled species and is given in units of the number of individuals per hectare (defaults to 
  # If HabPercent=FALSE: a vector of length Nhabitats, specifying the respective K_or_DensDep for every habitat code.
  # SpDistFile: Filename of the species initial distribution map which shall be imported (*.txt). Default is NULL.
  real_land = ImportedLandscape(
    LandscapeFile = "raster_reclass_binary_2005.txt",
    PatchFile = "patches_2005.txt",
    Resolution = 28.35578,
    Nhabitats = 3,
    K_or_DensDep = c(0, densdep, 0),
    SpDistFile = "patches_w_glt_2005.txt",
    SpDistResolution = 28.35578
  )
  
  ##### 3) Demography ----
  # To make a stage-structured model, we have to additionally create a stage-structure sub-module within the Demography module. 
  # We can use ‘+’ to add the StageStructure sub-module.
  mat = matrix(c(0, 0, 2,
                 juv_survival, 0, 0,
                 0, 0.56, ad_survival),
               nrow=3, byrow=T)
  
  # Stage structure
  # FecDensDep, DevDensDep, SurvDensDep = density-dependence on fecundity, development, survival
  stg = StageStructure(Stages = 3,
                       TransMatrix = mat,
                       MaxAge = 20,
                       RepSeasons = 1,
                       SurvSched = 1, # Between reproductive events
                       FecDensDep = T,
                       DevDensDep = F,
                       SurvDensDep = F)
  
  # Female-only models assume that males are not limiting, and that the population dynamics are driven only by females. 
  # It also means that sexes are not modelled explicitly and it is not possible to account for behaviours like mate-finding in the settlement decisions; females will settle in suitable habitat patches and then will automatically be able to attempt reproduction.
  demo = Demography(StageStruct = stg,
                    ReproductionType = 0)
  
  ##### 4) Dispersal -----
  ## Emigration
  emig = Emigration(EmigProb = 0,
                    StageDep = F,
                    DensDep = F)
  
  ## Transfer
  transfer = DispersalKernel(Distances = matrix(c(100),nrow=1),
                             StageDep = F)
  
  ## Settlement
  # Settle = 0 means 'die when unsuitable' for DispersalKernel and 'always settle when suitable' for Movement process
  settle = Settlement(StageDep = F,
                      Settle = 0,
                      FindMate = F,
                      DensDep = F)
  
  ## Dispersal
  disp = Dispersal(Emigration = emig,
                   Transfer = transfer,
                   Settlement = settle)
  
  ##### 5) Genetics ----
  # The genetics module controls the heritability and evolution of traits and is needed if inter-individual variability is enabled (IndVar = TRUE) e.g. for at least one dispersal trait
  
  ##### 6) Initialise -----
  # calculate proportion of all stages excluding the new-born juvenile (stage 0) population, 
  # which can't be initialised:
  eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = densdep, plot=F)
  prop_stgs = eq_pop[-1]/sum(eq_pop[-1])
  prop_stgs = round(prop_stgs,2)
  
  ## Initialise
  init = Initialise(InitType = 1,  # from loaded species distribution map
                    SpType = 0,   # All suitable cells within all distribution presence cells
                    InitDens = 2, # Set the number of individuals per cell/hectare to initialise in IndsHaCell
                    IndsHaCell = indshacell, # Initial density in inds/ha
                    PropStages = c(0, prop_stgs), # For StageStructured models only: Proportion of individuals initialised in each stage. Requires a vector of length equal to the number of stages
                    InitAge = 2)
  
  ##### 7) Parameter master -----
  s = RSsim(
    simul = sim,
    land = real_land,
    demog = demo,
    dispersal = disp,
    init = init
  )
  
  # SIMULATION
  RunRS(s, dirpath)
  #id_simulation = id_simulation +1
}

### Results -----

#### Population -----

##### Files -------
# stack all files
pop_files = list.files(
  here("data",
       "rangeshifter",
       "tests",
       "test_densdep_preYF_5", # UPDATE HERE
       "Outputs"),
  pattern = "_Pop\\.txt$",
  full.names = TRUE
)

pop_all = purrr::map_dfr(pop_files, function(f){
  
  sim_id = stringr::str_extract(
    basename(f),
    "(?<=Sim)\\d+(?=_Land)"
  ) %>%  as.numeric()
  
  read.table(
    f,
    header = TRUE,
    sep = "\t"
  ) %>%
    dplyr::mutate(Id_simul = sim_id)
  
})

dplyr::glimpse(pop_all)

### Join tested parameter
pop_all = dplyr::left_join(
  pop_all,
  metadata,
  by = "Id_simul"
)

##### Line plot ------
### Plot
pop_total = pop_all %>%
  dplyr::group_by(Id_simul, DensDep, IndsHaCell, Juv_survival, Ad_survival, Rep, Year) %>%
  dplyr::summarise(NInd = sum(NInd), .groups = "drop")
pop_time = pop_total %>%
  dplyr::group_by(DensDep, IndsHaCell, Juv_survival, Ad_survival, Year) %>%
  dplyr::summarise(MeanN = mean(NInd),.groups = "drop")
ggplot(pop_time, aes(Year, MeanN, colour = factor(DensDep))) +
  geom_line(linewidth = 1) +
  facet_grid(
    rows = vars(Ad_survival), # Parameter that varies
    cols = vars(Juv_survival), # Other parameter that varies
    scales = "free_y") +
  theme_bw() +
  labs(colour = "DensDep",
       y = "Mean population size")

## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         "test_densdep_preYF_5", # UPDATE HERE
         "plot",
         "evol_pop.png"), # UPDATE HERE
    width = 4000, height = 2000, res = 300, type="cairo")
ggplot(pop_time, aes(Year, MeanN, colour = factor(DensDep))) +
  geom_line(linewidth = 1) +
  facet_grid(
    rows = vars(Ad_survival), # Parameter that varies
    cols = vars(Juv_survival), # Other parameter that varies
    scales = "free_y") +
  theme_bw() +
  labs(colour = "DensDep",
       y = "Mean population size")
dev.off()

##### Comparison with long-term data -----
### Identify good parameters by comparing with real pop
## Identify parameters that provide the same initial and final pop. size
initial_popsize = 1600 # Adjust
final_popsize = 4869 # Adjust
initial_pop = pop_time %>%
  dplyr::filter(Year == min(pop_time$Year)) %>%
  dplyr::filter(abs(MeanN - initial_popsize) < 30)  # Adjust tolerance
final_pop = pop_time %>%
  dplyr::filter(Year == 18) %>% # 2023
  dplyr::filter(abs(MeanN - final_popsize) < 100)  # Adjust tolerance

# Get the parameter combinations for initial and final populations
initial_params = initial_pop %>%
  dplyr::select(DensDep, IndsHaCell, Ad_survival, Juv_survival) %>%
  dplyr::distinct()
final_params = final_pop %>%
  dplyr::select(DensDep, IndsHaCell, Ad_survival, Juv_survival) %>%
  dplyr::distinct()
# Find the intersection
dplyr::inner_join(initial_params, final_params)

## Heatmap plot
fit_score = pop_time %>%
  dplyr::group_by(Ad_survival, Juv_survival) %>%
  dplyr::summarise(
    InitialN = MeanN[Year == min(Year)],
    FinalN = MeanN[Year == 18],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Error_initial = abs(InitialN - initial_popsize),
    Error_final = abs(FinalN - final_popsize),
    # total distance from observed values
    Total_error = Error_initial + Error_final
  )
# Plot
ggplot(fit_score, aes(x = Ad_survival, y = Juv_survival, fill = Total_error)) +
  geom_tile() +
  # facet_wrap(~Ad_survival) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1
  ) +
  theme_bw()

## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         "test_densdep_preYF_5", # UPDATE HERE
         "plot",
         "heatmap.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
ggplot(fit_score, aes(x = Ad_survival, y = Juv_survival, fill = Total_error)) +
  geom_tile() +
  # facet_wrap(~Ad_survival) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1
  ) +
  theme_bw()
dev.off()

## RMSE
fit_score = pop_time %>%
  dplyr::group_by(
    DensDep,
    IndsHaCell,
    Ad_survival,
    Juv_survival
  ) %>%
  dplyr::summarise(
    N0 = MeanN[Year == 0],
    N18 = MeanN[Year == 18],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    RMSE = sqrt(((N0 - initial_popsize)^2 + (N18 - final_popsize)^2) / 2))
# heatmap RMSE
ggplot(fit_score, aes(x = DensDep,
                      y = IndsHaCell,
                      fill = RMSE)) +
  geom_tile() +
  facet_grid(Juv_survival ~ Ad_survival) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1) +
  theme_bw()
# Rank the best options
fit_score %>%
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1:5)


##### Patch abundance -----
pop_patch = pop_all %>%
  dplyr::group_by(PatchID, DensDep, IndsHaCell, Juv_survival, Ad_survival, Year) %>%
  dplyr::summarise(MeanN = mean(NInd),.groups = "drop")
# Join patch name
pop_patch = pop_patch %>% 
  dplyr::left_join(patch_corres_id, by=c("PatchID" = "unique_id")) %>% 
  dplyr::rename(patch_name = patch_id) %>% 
  dplyr::filter(!is.na(patch_name))

# Plot
pop_patch %>%
  ggplot(
    aes(
      x = Year,
      y = MeanN,
      group = patch_name,
      colour = patch_name
    )
  ) +
  geom_line(linewidth = 0.8) +
  facet_grid(
    rows = vars(Ad_survival),
    cols = vars(Juv_survival)
  ) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Mean patch abundance",
    colour = "Patch"
  )

### Compare with patch abundance over time
# Select parameters and years
sim_sel = pop_patch %>%
  dplyr::filter(
    DensDep == 0.088,
    IndsHaCell == 0.065,
    Ad_survival == 0.89,
    Juv_survival == 1,
    Year %in% c(0, 8, 13, 18) # Years of interest
  )

# Remove suffixes from patches ids
sim_sel = sim_sel %>%
  dplyr::mutate(
    FragName = stringr::str_remove(
      patch_name,
      "_\\d+$"
    )
  )

# Aggregate patches sharing the same fragment name
# Example: since Aldeia_I_1 and Aldeia_I_2 correspond to the same monitored UMMP, we sum their abundances
sim_frag = sim_sel %>%
  group_by(FragName, Year) %>%
  summarise(
    SimN = sum(MeanN),
    .groups = "drop"
  )

# Reshape the real census data
real_long = real_data %>%
  tidyr::pivot_longer(
    starts_with("n_glt"),
    names_to = "Survey",
    values_to = "RealN"
  ) %>%
  dplyr::mutate(
    Year = dplyr::case_when(
      Survey == "n_glt_2005" ~ 0,
      Survey == "n_glt_2014" ~ 8,
      Survey == "n_glt_2018" ~ 13,
      Survey == "n_glt_2023" ~ 18
    )
  ) %>% 
  dplyr::filter(!is.na(FragName))

# Join
sort(unique(sim_frag$FragName))
sort(unique(real_long$FragName))
comparison = sim_frag %>%
  dplyr::left_join(
    real_long,
    by = c(
      "FragName" = "FragName",
      "Year"
    )
  ) %>% 
  dplyr::filter(!is.na(RealN)) %>% 
  dplyr::filter(Survey != "n_glt_2018") # Remove YF data

# Correlation
cor(comparison$SimN, comparison$RealN)

# Plot
png(here("data",
         "rangeshifter",
         "tests",
         "test_densdep_preYF_5", # UPDATE HERE
         "plot",
         "corr_patch_simvsreal.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
ggplot(
  comparison,
  aes(x = RealN, y = SimN)
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
    FragName,
    Year,
    RealN,
    SimN
  ) %>%
  tidyr::pivot_longer(
    c(RealN, SimN),
    names_to = "Source",
    values_to = "N"
  )
# Plot
png(here("data",
         "rangeshifter",
         "tests",
         "test_densdep_preYF_5", # UPDATE HERE
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
    ~FragName,
    scales = "free_y"
  ) +
  theme_bw()
dev.off()
