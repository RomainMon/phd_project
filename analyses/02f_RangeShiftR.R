#------------------------------------------------#
# Author: Romain Monassier
# Objective: Running RangeShiftR simulations
#------------------------------------------------#

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
?ImportedLandscape
?ArtificialLandscape
?StageStructure
?Emigration
?Transfer
?DispersalKernel
?Initialise

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

# Maximum individuals observed in a forest patch (see: Ruiz-Miranda et al. 2019)
?getLocalisedEquilPop
par(mfrow=c(1,1))
eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.095, 0.1, 0.2, 0.3)) #  absolute values of individuals
# Select the value that reaches the desired threshold
colSums(eq_pop)


### SENSITIVITY ANALYSIS ----
# Run simulations for several values for given parameters

dirpath = "data/rangeshifter/test_densdep/" # UPDATE HERE
## Create the RS folder structure, if it doesn’t yet exist
# dir.create(file.path(dirpath, "Inputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Outputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Output_Maps"), showWarnings = TRUE)

#### Validation dataset ----
real_data = read_excel(here("data", "glt", "JDietz", "GLT_POP_2005_2023.xlsx"), na="NA")
real_data %>%
  dplyr::summarise(dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))


#### Parameters file ----
metadata = read_excel(here("data", "rangeshifter", 
                           "test_densdep", "test_parameters.xlsx"),
                      sheet="test3")

#### Loop -----
for(i in 1:nrow(metadata)) {
  
  densdep = metadata$DensDep[i]
  indshacell = metadata$IndsHaCell[i]
  id_simulation = metadata$Id_simul[i]
  
  # Print progression
  cat("\n========================================\n")
  cat(sprintf("Simulation %d/%d (ID: %d)\n",
              i, nrow(metadata), id_simulation))
  cat(sprintf("  DensDep = %.3f | IndsHaCell = %.3f\n",
              densdep, indshacell))
  cat("========================================\n\n")
  
  ##### 1) Simulation -----
  # This module is used to set general simulation parameters (e.g. simulation ID, number of replicates, and number of years to simulate) and to control output types (plus some more specific settings).
  sim = Simulation(Simulation = id_simulation, # Update simulation id
                   Replicates = 10, 
                   Years = 18,
                   OutIntPop = 1,
                   OutIntOcc = 0,
                   OutIntRange = 0)
  
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
                 0.9, 0, 0,
                 0, 0.56, 0.85),
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
# stack all files
pop_files = list.files(
  here("data","rangeshifter","test_densdep","Outputs"),
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

### Plot
pop_total = pop_all %>%
  dplyr::group_by(Id_simul, DensDep, IndsHaCell, Rep, Year) %>%
  dplyr::summarise(NInd = sum(NInd), .groups = "drop") %>% 
  dplyr::filter(!is.na(DensDep))
pop_time = pop_total %>%
  dplyr::group_by(DensDep, IndsHaCell, Year) %>%
  dplyr::summarise(MeanN = mean(NInd),.groups = "drop")
ggplot(pop_time, aes(Year, MeanN, colour = factor(DensDep))) +
  geom_line(linewidth = 1) +
  facet_wrap(~ IndsHaCell,scales="free") +
  theme_bw() +
  labs(colour = "DensDep",
       y = "Mean population size")

# Export plot
png(here("data",
         "rangeshifter",
         "test_densdep",
         "plot",
         "test_densdep_3.png"), 
    width = 2500, height = 2000, res = 300, type="cairo")
ggplot(pop_time, aes(Year, MeanN, colour = factor(DensDep))) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ IndsHaCell,scales="free") +
  theme_bw() +
  labs(colour = "DensDep",
       y = "Mean population size")
dev.off()

### Identify good parameters
# Identify parameters that provide the same initial and final pop. size
initial_popsize = 1600 # Adjust
final_popsize = 4869 # Adjust
initial_pop = pop_time %>%
  dplyr::filter(Year == min(pop_time$Year)) %>%
  dplyr::filter(abs(MeanN - initial_popsize) < 50)  # Adjust tolerance
final_pop = pop_time %>%
  dplyr::filter(Year == max(pop_time$Year)) %>%
  dplyr::filter(abs(MeanN - final_popsize) < 300)  # Adjust tolerance

# Get the parameter combinations for initial and final populations
initial_params = initial_pop %>%
  dplyr::select(DensDep, IndsHaCell) %>%
  dplyr::distinct()
final_params = final_pop %>%
  dplyr::select(DensDep, IndsHaCell) %>%
  dplyr::distinct()
# Find the intersection
dplyr::inner_join(initial_params, final_params)
