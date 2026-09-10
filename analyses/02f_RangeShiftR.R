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
# ?ImportedLandscape
# ?ArtificialLandscape
# ?StageStructure
# ?Emigration
# ?Transfer
# ?DispersalKernel
# ?Initialise

### Test configuration (path and parameters tested) ----
# Here, name explicitely the folder with the files

# List with components stored
test_config = list(
  test_name = "test_resist_5", # Name of the folder
  
  # Parameters tested with sensitivity analysis (i.e., those varying during simulations)
  parameters = c(
    "IndsHaCell",
    "Emig_prob",
    "PR"
  )
)

dirpath = here(
  "data",
  "rangeshifter",
  "tests",
  test_config$test_name,
  "/"
)

## Create the RS folder structure, if it doesn’t yet exist
# dir.create(file.path(dirpath, "Inputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Outputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Output_Maps"), showWarnings = TRUE)

### Check the landscape -----
## Import a landscape
landsc = terra::rast(file.path(dirpath, "Inputs", "raster_reclass_2005.txt"))
terra::plot(landsc)
terra::res(landsc)
terra::unique(landsc)

## Patches
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

## Species distribution
patch_w_glt  = terra::rast(file.path(dirpath, "Inputs", "patches_w_glt_2005.txt"))
terra::res(patch_w_glt)
terra::plot(patch_w_glt, col=c("gray","darkgreen"))
terra::unique(patch_w_glt)


### Determine 1/b -------
# To explore the potential effect of b on equilibrium population sizes before running the simulation, use getLocalisedEquilPop(). 
# This allows a better understanding of reasonable values of 1/b for the different land types. 
# getLocalisedEquilPop() runs a quick simulation of a closed and localised population (i.e. without dispersal and in a single idealised patch) for a given vector of potential 1/b values (argument DensDep_values) and based on our defined Demography() module.
# The getLocalisedEquilPop() function uses absolute values of individuals in the local population (since there is not spatial extent).
# WHILE what we need are relative values of 1/b as individuals per hectare.
# We choose to simulate a hypothetical patch of our landscape, and want to determine the value of 1/b that is needed to observe e.g. 100 individuals in 1 ha (e.g., our empirical observation of how many individuals were maximally observed in patches)
# Thus, we aim to find the 1/b parameter that would yield a maximum local population abundance of 100 individuals. 
# We can now assess the localised equilibrium population size for different values of 1/b and see how the density dependence plays out.

# Pop. matrix
mat = matrix(c(0, 0, 2,
               1, 0, 0,
               0, 0.56, 0.89),
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
# ?getLocalisedEquilPop
par(mfrow=c(1,1))
eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.095, 0.1, 0.2, 0.3)) #  absolute values of individuals
# Select the value that reaches the desired threshold
colSums(eq_pop)
# The mean GLT density per patch is 0.086 ind/ha (Ruiz-Miranda et al. 2019)
# NOTE: 8,6 individus / 100 ha (1 km²) = 0,086 individu/ha.
# This value corresponds to the 4th or 5th column (0.08 or 0.09)

# calculate proportion of all stages excluding the new-born juvenile (stage 0) population, 
# which can't be initialised:
eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = 0.089, plot=F) # Put DensDep_values
prop_stgs = eq_pop[-1]/sum(eq_pop[-1])
round(prop_stgs,2)

### SENSITIVITY ANALYSIS ----
# Run simulations for several values for given parameters

#### Validation datasets ----
# Census data
real_census_data = read_excel(here("data", "glt", "JDietz", "GLT_POP_2005_2023.xlsx"), na="NA")
real_census_data %>%
  dplyr::summarise(dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

# Occupied patches
# Corresponds to ids of patches (patch_id) occupied in 2013 by GLTs according to the census (spatial intersection with a census polygon "P" in 2013) and Regions files (spatial intersection)
# See: 02g_patches_occupied
occ_patches2013 = read.csv(here("outputs","data","patches_rshifter","patches_occupied_2013.csv"), sep="")

#### Correspondence table ----
# Correspondence between raster patches ids (used in RangeShifter) and vector patches
patch_corres_id = readr::read_csv(here("data", 
                                       "rangeshifter", 
                                       "tests", 
                                       test_config$test_name,
                                       "Inputs", 
                                       "patch_corres_id_2005.csv"),
                                col_types = readr::cols(uncut_patch_id = readr::col_integer())) # PATCH INTEGER ID HERE

#### Parameters file ----
# Load an Excel sheet with the parameters to test
metadata = read_excel(here("data", 
                           "rangeshifter", 
                           "tests", 
                           test_config$test_name,
                           "test_parameters.xlsx"),
                      sheet="test1")


#### Loop (WITHOUT DISPERSAL) -----

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
                   OutIntOcc = 0, # Whether to export occupancy files
                   OutIntRange = 0, # Whether to export range files
                   OutIntInd = 0, # Whether to export individual files
                   OutIntConn = 0, # Whether to export connectivity files (n individuals from patch i to patch j)
                   SMSHeatMap = FALSE, # Produce SMS heat map raster as output?
                   ReturnPopDataFrame = TRUE, # Return population data to R as data frame (most suitable for patch based models)?
                   CreatePopFile = TRUE # Create population output file? Defaults to TRUE.
  ) 
  
  ##### 2) Landscape -----
  # DynamicLandYears: For a dynamic landscape, DynamicLandYears lists the years in which the corresponding habitat maps in LandscapeFile and - if applicable - their respective patch and/or costs maps (in PatchFile,CostsFile) are loaded and used in the simulation
  # demogScaleLayersFile: List of vectors with file names of additional landscape layers which can be used to locally scale certain demographic rates and thus allow them to vary spatially. The list must contain equally sized vectors providing file names, one vector for each element in DynamicLandYears, which are interpreted as stacked layers. Can only be used in combination with habitat quality maps, i.e. when HabPercent=TRUE. It must contain percentage values ranging from 0 to 100
  real_land = ImportedLandscape(
    LandscapeFile = "raster_reclass_2005.txt",
    PatchFile = "patches_2005.txt",
    Resolution = 28.35578,
    Nhabitats = 2, # Number of land covers. UPDATE DEPENDING ON THE LANDSCAPE
    K_or_DensDep = c(0, densdep), # Density dependence of the modeled species and is given in units of the nb of individuals/ha (for each land cover). If combined with a StageStructured model, K_or_DensDep will be used as the strength of demographic density dependence b-1. If combined with a non-structured model, K_or_DensDep will be interpreted as limiting carrying capacity K
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
  # NB: we can import matrix of layer indices for the three demographic rates (fecundity/development/survival) if they are spatially varying with the parameters FecLayer, DevLayer, SurvLayer
  # FecStageWtsMatrix, DevStageWtsMatrix, SurvStageWtsMatrix: Stage-dependent weights in density dependence of fecundity / development / survival.
  # PostDestructn: In a dynamic landscape, determine if all individuals of a population should die (FALSE, default) or disperse (TRUE) if its patch gets destroyed.
  stg = StageStructure(Stages = 3, # Nb of life stages
                       TransMatrix = mat,
                       MaxAge = 20, # Maximum age
                       RepSeasons = 1, # Nb of reproduction events per year
                       RepInterval = 0, # Nb of reproductive seasons which must be missed following a reproduction attempt, before another reproduction attempt may occur
                       PRep = 1, # Probability of reproducing in subsequent reproductive seasons
                       SurvSched = 1, #Scheduling of Survival. When should survival and development occur? 0 = At reproduction, 1 = Between reproductive events (default), 2 = Annually (only for RepSeasons>1)
                       FecDensDep = T, # Density-dependence on fecundity?
                       DevDensDep = F, # Density-dependence on development?
                       SurvDensDep = F # Density-dependence on survival?
  ) 
  
  # Female-only models assume that males are not limiting, and that the population dynamics are driven only by females. 
  # It also means that sexes are not modelled explicitly and it is not possible to account for behaviours like mate-finding in the settlement decisions; females will settle in suitable habitat patches and then will automatically be able to attempt reproduction.
  # IF ReproductionType=2, specify arguments PropMales and Harem
  demo = Demography(StageStruct = stg, # corresponding parameter object generated by StageStructure, which holds all demographic parameters
                    ReproductionType = 0 # 0 = asexual / only female model (default); 1 = simple sexual model; 2 = sexual model with explicit mating system
  ) 
  
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
  init = Initialise(InitType = 1,  # InitType = 0: Free initialisation according to habitat map (default) (set FreeType), InitType = 1: From loaded species distribution map (set SpType), InitType = 2: From initial individuals list file
                    SpType = 0, # SpType = 0: All suitable cells within all distribution presence cells (default), SpType = 1: All suitable cells within some randomly chosen presence cells; set number of cells to initialise in NrCells.
                    InitDens = 2, # Number of individuals to be seeded in each cell/patch. InitDens = 0: At K_or_DensDep, InitDens = 1: At half K_or_DensDep (default), InitDens = 2: Set the number of individuals per cell/hectare to initialise in IndsHaCell.
                    IndsHaCell = indshacell, # Initial density in inds/ha
                    PropStages = c(0, prop_stgs), # For StageStructured models only: Proportion of individuals initialised in each stage. Requires a vector of length equal to the number of stages
                    InitAge = 2 # Initial age distribution within each stage. InitAge = 0: Minimum age for the respective stage. InitAge = 1 : Age randomly sampled between the minimum and the maximum age for the respective stage. InitAge = 2: According to a quasi-equilibrium distribution
  )
  
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

# IF THE SIMULATION DOES NOT RUN
# traceback()


#### Loop (WITH DISPERSAL) -----

for(i in 1:nrow(metadata)) {
  
  densdep = metadata$DensDep[i]
  indshacell = metadata$IndsHaCell[i]
  ad_survival = metadata$Ad_survival[i]
  juv_survival = metadata$Juv_survival[i]
  
  emig_prob = metadata$Emig_prob[i]
  pr = metadata$PR[i]
  ms = metadata$MS[i]
  dp = metadata$DP[i]
  step_mortality = metadata$Step_mortality[i]
  
  nhab = metadata$Nhab[i]
  costs = c(metadata$Cost1[i],
            metadata$Cost2[i],
            metadata$Cost3[i],
            metadata$Cost4[i],
            metadata$Cost5[i],
            metadata$Cost6[i])
  
  id_simulation = metadata$Id_simul[i]
  
  # Print progression
  cat("\n========================================\n")
  cat(sprintf("Simulation %d/%d (ID: %d)\n",
              i, nrow(metadata), id_simulation))
  cat(sprintf("  DensDep = %.3f | Juv suvival = %.3f | Adult suvival = %.3f | IndsHaCell = %.3f\n",
              densdep, juv_survival, ad_survival, indshacell))
  cat(sprintf("  EmigProb = %.3f | Perceptual range = %.3f | Memory size = %.3f | Directional persistence = %.3f | Nhab = %.3f | Step mortality = %.3f\n",
              emig_prob, pr, ms, dp, nhab, step_mortality))
  cat(sprintf("  Costs = %.3f\n",
              costs))
  cat("========================================\n\n")
  
  ##### 1) Simulation -----
  # This module is used to set general simulation parameters (e.g. simulation ID, number of replicates, and number of years to simulate) and to control output types (plus some more specific settings).
  sim = Simulation(Simulation = id_simulation, # Update simulation id
                   Replicates = 20, # Number of replicates
                   Years = 95, # Number of years
                   OutIntPop = 1, # Whether to export population files 
                   OutIntOcc = 1, # Whether to export occupancy files
                   OutIntRange = 1, # Whether to export range files
                   OutIntInd = 0, # Whether to export individual files
                   OutIntConn = 0, # Whether to export connectivity files (n individuals from patch i to patch j)
                   SMSHeatMap = TRUE, # Produce SMS heat map raster as output?
                   ReturnPopDataFrame = TRUE, # Return population data to R as data frame (most suitable for patch based models)?
                   CreatePopFile = TRUE # Create population output file? Defaults to TRUE.
                   ) 
  
  ##### 2) Landscape -----
  # DynamicLandYears: For a dynamic landscape, DynamicLandYears lists the years in which the corresponding habitat maps in LandscapeFile and - if applicable - their respective patch and/or costs maps (in PatchFile,CostsFile) are loaded and used in the simulation
  # demogScaleLayersFile: List of vectors with file names of additional landscape layers which can be used to locally scale certain demographic rates and thus allow them to vary spatially. The list must contain equally sized vectors providing file names, one vector for each element in DynamicLandYears, which are interpreted as stacked layers. Can only be used in combination with habitat quality maps, i.e. when HabPercent=TRUE. It must contain percentage values ranging from 0 to 100
  real_land = ImportedLandscape(
    LandscapeFile = "raster_reclass_2005.txt",
    PatchFile = "patches_2005.txt",
    Resolution = 28.35578,
    Nhabitats = nhab, # Number of land covers
    K_or_DensDep = c(0, densdep, 0, 0, 0, 0), # Density dependence of the modeled species and is given in units of the nb of individuals/ha (for each land cover). If combined with a StageStructured model, K_or_DensDep will be used as the strength of demographic density dependence b-1. If combined with a non-structured model, K_or_DensDep will be interpreted as limiting carrying capacity K
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
  # NB: we can import matrix of layer indices for the three demographic rates (fecundity/development/survival) if they are spatially varying with the parameters FecLayer, DevLayer, SurvLayer
  # FecStageWtsMatrix, DevStageWtsMatrix, SurvStageWtsMatrix: Stage-dependent weights in density dependence of fecundity / development / survival.
  # PostDestructn: In a dynamic landscape, determine if all individuals of a population should die (FALSE, default) or disperse (TRUE) if its patch gets destroyed.
  stg = StageStructure(Stages = 3, # Nb of life stages
                       TransMatrix = mat,
                       MaxAge = 20, # Maximum age
                       RepSeasons = 1, # Nb of reproduction events per year
                       RepInterval = 0, # Nb of reproductive seasons which must be missed following a reproduction attempt, before another reproduction attempt may occur
                       PRep = 1, # Probability of reproducing in subsequent reproductive seasons
                       SurvSched = 1, #Scheduling of Survival. When should survival and development occur? 0 = At reproduction, 1 = Between reproductive events (default), 2 = Annually (only for RepSeasons>1)
                       FecDensDep = T, # Density-dependence on fecundity?
                       DevDensDep = F, # Density-dependence on development?
                       SurvDensDep = F # Density-dependence on survival?
                       ) 
  
  # Female-only models assume that males are not limiting, and that the population dynamics are driven only by females. 
  # It also means that sexes are not modelled explicitly and it is not possible to account for behaviours like mate-finding in the settlement decisions; females will settle in suitable habitat patches and then will automatically be able to attempt reproduction.
  # IF ReproductionType=2, specify arguments PropMales and Harem
  demo = Demography(StageStruct = stg, # corresponding parameter object generated by StageStructure, which holds all demographic parameters
                    ReproductionType = 0 # 0 = asexual / only female model (default); 1 = simple sexual model; 2 = sexual model with explicit mating system
                    ) 
  
  ##### 4) Dispersal -----
  ## Emigration
  emig = Emigration(EmigProb = emig_prob, # Matrix containing all parameters (#columns) to determine emigration probabilities for each stage/sex (#rows). Its structure depends on the other parameters, see the Details. If the emigration probability is constant (i.e. DensDep, IndVar, StageDep, SexDep = FALSE), EmigProb can take a single numeric. Defaults to 0
                    SexDep = F, # Sex-dependent emigration probability?
                    StageDep = F, # Stage-dependent emigration probability?
                    DensDep = F, # Density-dependent emigration probability?
                    IndVar = F # Individual variability in emigration traits?
                    ) 
  
  ## Transfer (movement of an individual departing from its natal patch towards a potential new patch)
  # This movement can be modelled by one of three alternative processes:
  # - Dispersal kernel: use DispersalKernel
  # - Stochastic movement simulator (SMS): use SMS
  # - Correlated random walk (CRW): use CorrRW
  # IMPORTANT: the dispersal resistance of each land type is set by the argument Costs
  transfer = SMS(PR = pr, # Perceptual range in nb of cells (must be integer)
                 PRMethod = 1, # Method to evaluate the effective cost of a particular step from the landscape within the perceptual range: 1 = Arithmetic mean, 2 = Harmonic mean, 3 = Weighted arithmetic mean
                 MemSize = ms, # Memory size (nb of previous steps over which to calculate current direction to apply directional persistence). Default = 1, max = 14
                 DP = dp, # Directional persistence: tendency to follow a CRW. Must be >= 1 (default to 1)
                 GoalType = 0, # Goal bias type (i.e., a tendency to move towards a particular destination). 0 = None, 2 = Dispersal bias (i.e., moving away from the natal location)
                 IndVar = F, # Individual variability in SMS traits?
                 Costs = costs, # Landscape resistance to movement (for each land cover)
                 StepMort = step_mortality, # Per-step mortality probability. Constant or habitat-specific
                 StraightenPath = T # Straigten path after decision not to settle in a patch?
  )
  
  ## Settlement (or immigration)
  settle = Settlement(StageDep = F, # Stage-dependent settlement requirements?
                      SexDep = F, # Sex-dependent settlement requirements?
                      Settle = 0, # CODES (dor DispersalKernel) or PROBA (for Movement processes if DensDep = TRUE) for all stages, sexes. Default = 0 (i.e. 'die when unsuitable' for DispersalKernel and 'always settle when suitable' for Movement process)
                      FindMate = F, # Mating requirements to settle? FALSE if female-only model
                      DensDep = F, # For movement processes only: Density-dep settlement probability?
                      IndVar = F, # For movement processes only: Individual variability in settlement probability traits?
                      MinSteps = 0, # For movement processes only: min number of steps
                      MaxSteps = pr*2, # For movement processes only: max number of steps
                      MaxStepsYear = 0 # For movement processes and stage-structured population only: max nb of steps per year IF >1 reproductive season. IF 0:  every individual completes the dispersal phase in one year, i.e. between two successive reproduction phases.
  )
  
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
  init = Initialise(InitType = 1,  # InitType = 0: Free initialisation according to habitat map (default) (set FreeType), InitType = 1: From loaded species distribution map (set SpType), InitType = 2: From initial individuals list file
                    SpType = 0, # SpType = 0: All suitable cells within all distribution presence cells (default), SpType = 1: All suitable cells within some randomly chosen presence cells; set number of cells to initialise in NrCells.
                    InitDens = 2, # Number of individuals to be seeded in each cell/patch. InitDens = 0: At K_or_DensDep, InitDens = 1: At half K_or_DensDep (default), InitDens = 2: Set the number of individuals per cell/hectare to initialise in IndsHaCell.
                    IndsHaCell = indshacell, # Initial density in inds/ha
                    PropStages = c(0, prop_stgs), # For StageStructured models only: Proportion of individuals initialised in each stage. Requires a vector of length equal to the number of stages
                    InitAge = 2 # Initial age distribution within each stage. InitAge = 0: Minimum age for the respective stage. InitAge = 1 : Age randomly sampled between the minimum and the maximum age for the respective stage. InitAge = 2: According to a quasi-equilibrium distribution
                    )
  
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

# IF THE SIMULATION DOES NOT RUN
# traceback()

### Results -----

#### Population -----

##### Files -------
# stack all files
pop_files = list.files(
  here("data",
       "rangeshifter",
       "tests",
       test_config$test_name,
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

### Parameters tested
params_tested = test_config$parameters

##### Line plot ------
### Plot
# Population size by siumation
pop_total = pop_all %>%
  dplyr::group_by(Id_simul, 
                  dplyr::across(dplyr::all_of(params_tested)),
                  Rep, 
                  Year) %>%
  dplyr::summarise(NInd = sum(NInd), .groups = "drop")
# Mean population size across tested parameters
pop_time = pop_total %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(params_tested)),
                  Year) %>%
  dplyr::summarise(MeanN = mean(NInd),.groups = "drop")

# Plot evolution of population size
ggplot(
  pop_time,
  aes(
    x = Year,
    y = MeanN,
    colour = factor(.data[[params_tested[1]]])
  )
) +
  geom_line(linewidth = 1) +
  # Faceting
  facet_grid(
    cols = vars(.data[[params_tested[2]]]),
    rows = vars(.data[[params_tested[3]]]),
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
  labs(y = "Mean population size")

## Export plot
png(here("data",
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
    y = MeanN,
    colour = factor(.data[[params_tested[1]]])
  )
) +
  geom_line(linewidth = 1) +
  # Faceting
  facet_grid(
    cols = vars(.data[[params_tested[2]]]),
    rows = vars(.data[[params_tested[3]]]),
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
  labs(y = "Mean population size")

dev.off()

##### Comparison with long-term data -----
### Identify good parameters by comparing with real pop
## Identify parameters that provide the same initial and final pop. size
pop2005 = 1600 # Adjust
pop2013 = 3706 # Adjust
pop2022 = 4869 # Adjust
initial_pop = pop_time %>%
  dplyr::filter(Year == min(pop_time$Year)) %>% # 2005
  dplyr::filter(abs(MeanN - pop2005) < 200)  # Adjust tolerance
pop_date1 = pop_time %>%
  dplyr::filter(Year == 8) %>% # 2013
  dplyr::filter(abs(MeanN - pop2013) < 200)  # Adjust tolerance
pop_date2 = pop_time %>%
  dplyr::filter(Year == 17) %>% # 2022
  dplyr::filter(abs(MeanN - pop2022) < 500)  # Adjust tolerance

# Get the parameter combinations for 2005 and 2013
initial_params = initial_pop %>%
  dplyr::select(dplyr::all_of(params_tested)) %>%
  dplyr::distinct()
params_date1 = pop_date1 %>%
  dplyr::select(dplyr::all_of(params_tested)) %>%
  dplyr::distinct()
params_date2 = pop_date2 %>%
  dplyr::select(dplyr::all_of(params_tested)) %>%
  dplyr::distinct()
# Find the intersection
# Intersection between two dates
dplyr::inner_join(initial_params, params_date1) 
# Between three dates
initial_params %>%
  dplyr::inner_join(params_date1, by = params_tested) %>%
  dplyr::inner_join(params_date2, by = params_tested)

## Heatmap plot
fit_score = pop_time %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(params_tested))) %>%
  dplyr::summarise(
    InitialN = MeanN[Year == min(Year)],
    FinalN = MeanN[Year == 8],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Error_initial = abs(InitialN - pop2005),
    Error_final = abs(FinalN - pop2013),
    # total distance from observed values
    Total_error = Error_initial + Error_final
  )
# Plot
ggplot(
  fit_score,
  aes(
    x = .data[[params_tested[1]]],
    y = .data[[params_tested[2]]],
    fill = Total_error
  )
) +
  geom_tile() +
  facet_wrap(vars(.data[[params_tested[3]]])) +
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
         "heatmap.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
# Plot
ggplot(
  fit_score,
  aes(
    x = .data[[params_tested[1]]],
    y = .data[[params_tested[2]]],
    fill = Total_error
  )
) +
  geom_tile() +
  facet_wrap(vars(.data[[params_tested[3]]])) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1
  ) +
  theme_bw()

dev.off()

## RMSE
# Note: RMSE is frequently used to assess differences between predicted and real values
# Calculation: square root of the mean of the squares of the deviations

## Based on 2005 and 2013 populations sizes
fit_score_t1t2 = pop_time %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(params_tested))) %>%
  dplyr::summarise(
    N0 = MeanN[Year == 0],
    N8 = MeanN[Year == 8],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    RMSE = sqrt(
      ((N0 - pop2005)^2 +
         (N8 - pop2013)^2) / 2
    )
  )
# Rank the best options
fit_score_t1t2 %>%
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1:10)
# Best option
fit_score_t1t2 %>%
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1)

## Based on 2005, 2013 and 2022 populations sizes
fit_score_t1t2t3 = pop_time %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(params_tested))) %>%
  dplyr::summarise(
    N0 = MeanN[Year == 0],
    N8 = MeanN[Year == 8],
    N17 = MeanN[Year == 17],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    RMSE = sqrt(
      ((N0 - pop2005)^2 +
         (N8 - pop2013)^2 +
         (N17 - pop2022)^2) / 3
    ))
# Rank the best options
fit_score_t1t2t3 %>%
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1:10)
# Best option
fit_score_t1t2t3 %>%
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1)



### Best parameters
# Using RMSE
best_fit = fit_score_t1t2 %>% # Choose between 2005-2013 fit or 2005-2013-2022 fit
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1)
best_values = best_fit %>%
  dplyr::select(dplyr::all_of(params_tested))

# Manual selection
# best_fit = fit_score_t1t2t3 %>%
#   dplyr::filter(PR == 10,
#                 DP == 5)
# best_values = best_fit %>% 
#   dplyr::select(PR, DP)


#### Patch occupancy -----
## Number of simulated patches occupied in 2013
occ_patches_sim_2013 = pop_all %>%
  dplyr::semi_join(
    best_values, # using the optimal parameter combination
    by = params_tested
  ) %>%
  dplyr::filter(
    NInd > 0 # Remove patches unoccupied
  ) %>%
  dplyr::distinct(
    Rep,
    PatchID
  ) %>%
  dplyr::filter(
    PatchID != 0 # Remove the matrix
  )

# Group by real patch name 
occ_patches_sim_2013 = occ_patches_sim_2013 %>% 
  dplyr::left_join(patch_corres_id, by=c("PatchID"="cut_patch_id")) %>% # We join the vector patches names (from the correspondence table between RangeShifter raster patches and vector patches)
  dplyr::rename(patch_id = uncut_patch_name) %>% 
  dplyr::select(Rep, patch_id) %>% # Keep one id
  dplyr::distinct() # Remove duplicated lines (i.e., patches belonging to the same higher-level patch)

# Number of replicates
n_reps = dplyr::n_distinct(occ_patches_sim_2013$Rep)
  
# Keep patches occupied in >= 95% of replicates
occ_patches_sim_2013 = occ_patches_sim_2013 %>%
  dplyr::group_by(patch_id) %>% # Patch id column
  dplyr::summarise(
    n_reps_occupied = dplyr::n_distinct(Rep),
    prop_replicates = n_reps_occupied / n_reps,
    .groups = "drop"
    ) %>%
  dplyr::filter(prop_replicates >= 0.95) %>%
  dplyr::arrange(patch_id)

# N patches occupied
n_occ_patches_sim = occ_patches_sim_2013 %>% 
  dplyr::summarise(dplyr::n_distinct(patch_id)) %>% 
  as.integer()
n_occ_patches_sim

### Proportion of observed 2013 occupied patches that are missing from the simulation
# WARNING: here, we compare occupied patches with patches known to be occupied in 213
# We base our comparison on patch_id (i.e., patch name)
# It only works if patches are comparable (they share the same names)

# Simulated patches occurring in >= 95% of replicates
sim_patches = occ_patches_sim_2013 %>%
  dplyr::mutate(Simulated = TRUE)

# Validation patches
real_patches = occ_patches2013 %>%
  dplyr::select(patch_id) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Validation = TRUE)

# Compare visually the two lists
patch_comparison = sim_patches %>%
  dplyr::full_join(
    real_patches,
    by = "patch_id"
  ) %>%
  dplyr::mutate(
    Simulated = dplyr::coalesce(Simulated, FALSE),
    Validation = dplyr::coalesce(Validation, FALSE),
    Common = Simulated & Validation
  )
patch_comparison %>% dplyr::filter(Common == FALSE)

# Patches occupied in 2013 but not according to simulations
missing_patches_2013 = real_patches %>%
  dplyr::anti_join(
    sim_patches,
    by = "patch_id"
  )

# Percentage of missing patches
percent_missing_patches_2013 = nrow(missing_patches_2013) / nrow(real_patches)
percent_missing_patches_2013 # Print the result
# Note: these are the patches occupied in 2013 only


#### Patch abundance -----
pop_patch = pop_all %>%
  dplyr::group_by(PatchID, 
                  dplyr::across(dplyr::all_of(params_tested)),
                  Year) %>% # Add parameters here (depending on how many are tested)
  dplyr::summarise(MeanN = mean(NInd),.groups = "drop")

## Join patch name
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

# names of the patches used for simulations (starting from "test_patch_cut_1")
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

pop_patch = pop_patch %>% 
  dplyr::left_join(patch_corres_id, by=c("PatchID" = "cut_patch_id")) %>% # Update patch id variable here (integer id)
  dplyr::rename(patch_id = uncut_patch_name) %>% 
  dplyr::filter(!is.na(patch_id)) %>% # Patch name variable here
  dplyr::filter(patch_id %in% patch_list) # Patch name variable here

### Compare with patch abundance over time
sim_sel = pop_patch %>%
  dplyr::semi_join(
    best_values,
    by = params_tested
  ) %>% 
  dplyr::filter(
    Year %in% c(0, 8, 13, 17) # Years of interest
  )

# Remove suffixes from patches ids and create a variable "FragName" resembling UMMP name
sim_sel = sim_sel %>%
  dplyr::mutate(
    FragName = stringr::str_remove(
      patch_id, # patch name
      "_\\d+$"
    )
  )

# Aggregate patches sharing the same fragment name
# Example: since Aldeia_I_1 and Aldeia_I_2 correspond to the same monitored UMMP, we sum their abundances
sim_frag = sim_sel %>%
  dplyr::group_by(FragName, Year) %>%
  dplyr::summarise(
    SimN = sum(MeanN),
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
  dplyr::filter(!is.na(FragName))

# Join
comparison = sim_frag %>%
  dplyr::left_join(
    real_census_long,
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
         test_config$test_name,
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
    ~FragName,
    scales = "free_y"
  ) +
  theme_bw()
dev.off()

# Patch abundance summary
patch_summary = comparison %>%
  dplyr::mutate(Year = dplyr::case_when(Year == 0 ~ 2005,
                                        Year == 8 ~ 2013,
                                        Year == 17 ~ 2022)) %>% 
  dplyr::select(
    FragName,
    Year,
    SimN
    ) %>%
  tidyr::pivot_wider(
    names_from = c(FragName, Year),
    values_from = c(SimN),
    names_glue = "{.value}_{FragName}_{Year}"
  ) %>%
  dplyr::rename_with(
    ~ stringr::str_replace(.x, "^SimN_", "Pop_")
  )


#### RangeShiftR plots -----
# Require the different components of the simulations
densdep = 0.088
indshacell = 0.08
ad_survival = 0.89
juv_survival = 1
emig_prob = 0.035
pr = 10
ms = 10
dp = 5
step_mortality = 0.001

##### Simulation -----
sim = Simulation(Simulation = 1, # Update simulation id
                 Replicates = 20, # Number of replicates
                 Years = 95, # Number of years
                 OutIntPop = 1, # Whether to export population files 
                 OutIntOcc = 0, # Whether to export occupancy files
                 OutIntRange = 0, # Whether to export range files
                 OutIntInd = 0, # Whether to export individual files
                 OutIntConn = 0, # Whether to export connectivity files (n individuals from patch i to patch j)
                 SMSHeatMap = FALSE, # Produce SMS heat map raster as output?
                 ReturnPopDataFrame = TRUE, # Return population data to R as data frame (most suitable for patch based models)?
                 CreatePopFile = TRUE # Create population output file? Defaults to TRUE.
) 

##### Landscape -----
real_land = ImportedLandscape(
  LandscapeFile = "raster_reclass_2005.txt",
  PatchFile = "patches_2005.txt",
  Resolution = 28.35578,
  Nhabitats = 6, # Number of land covers
  K_or_DensDep = c(0, densdep, 0, 0, 0, 0), # Density dependence of the modeled species and is given in units of the nb of individuals/ha (for each land cover). If combined with a StageStructured model, K_or_DensDep will be used as the strength of demographic density dependence b-1. If combined with a non-structured model, K_or_DensDep will be interpreted as limiting carrying capacity K
  SpDistFile = "patches_w_glt_2005.txt",
  SpDistResolution = 28.35578
)

##### Demography
mat = matrix(c(0, 0, 2,
               juv_survival, 0, 0,
               0, 0.56, ad_survival),
             nrow=3, byrow=T)

# Stage structure
stg = StageStructure(Stages = 3, # Nb of life stages
                     TransMatrix = mat,
                     MaxAge = 20, # Maximum age
                     RepSeasons = 1, # Nb of reproduction events per year
                     RepInterval = 0, # Nb of reproductive seasons which must be missed following a reproduction attempt, before another reproduction attempt may occur
                     PRep = 1, # Probability of reproducing in subsequent reproductive seasons
                     SurvSched = 1, #Scheduling of Survival. When should survival and development occur? 0 = At reproduction, 1 = Between reproductive events (default), 2 = Annually (only for RepSeasons>1)
                     FecDensDep = T, # Density-dependence on fecundity?
                     DevDensDep = F, # Density-dependence on development?
                     SurvDensDep = F # Density-dependence on survival?
) 

demo = Demography(StageStruct = stg, # corresponding parameter object generated by StageStructure, which holds all demographic parameters
                  ReproductionType = 0 # 0 = asexual / only female model (default); 1 = simple sexual model; 2 = sexual model with explicit mating system
) 

##### Dispersal -----
## Emigration
emig = Emigration(EmigProb = emig_prob, # Matrix containing all parameters (#columns) to determine emigration probabilities for each stage/sex (#rows). Its structure depends on the other parameters, see the Details. If the emigration probability is constant (i.e. DensDep, IndVar, StageDep, SexDep = FALSE), EmigProb can take a single numeric. Defaults to 0
                  SexDep = F, # Sex-dependent emigration probability?
                  StageDep = F, # Stage-dependent emigration probability?
                  DensDep = F, # Density-dependent emigration probability?
                  IndVar = F # Individual variability in emigration traits?
) 

## Transfer (movement of an individual departing from its natal patch towards a potential new patch)
transfer = SMS(PR = pr, # Perceptual range in nb of cells (must be integer)
               PRMethod = 1, # Method to evaluate the effective cost of a particular step from the landscape within the perceptual range: 1 = Arithmetic mean, 2 = Harmonic mean, 3 = Weighted arithmetic mean
               MemSize = ms, # Memory size (nb of previous steps over which to calculate current direction to apply directional persistence). Default = 1, max = 14
               DP = dp, # Directional persistence: tendency to follow a CRW. Must be >= 1 (default to 1)
               GoalType = 0, # Goal bias type (i.e., a tendency to move towards a particular destination). 0 = None, 2 = Dispersal bias (i.e., moving away from the natal location)
               IndVar = F, # Individual variability in SMS traits?
               Costs = c(50,1,5,20,100,100), # Landscape resistance to movement (for each land cover)
               StepMort = step_mortality, # Per-step mortality probability. Constant or habitat-specific
               StraightenPath = T # Straigten path after decision not to settle in a patch?
)

## Settlement (or immigration)
settle = Settlement(StageDep = F, # Stage-dependent settlement requirements?
                    SexDep = F, # Sex-dependent settlement requirements?
                    Settle = 0, # CODES (dor DispersalKernel) or PROBA (for Movement processes if DensDep = TRUE) for all stages, sexes. Default = 0 (i.e. 'die when unsuitable' for DispersalKernel and 'always settle when suitable' for Movement process)
                    FindMate = F, # Mating requirements to settle? FALSE if female-only model
                    DensDep = F, # For movement processes only: Density-dep settlement probability?
                    IndVar = F, # For movement processes only: Individual variability in settlement probability traits?
                    MinSteps = 0, # For movement processes only: min number of steps
                    MaxSteps = pr*2, # For movement processes only: max number of steps
                    MaxStepsYear = 0 # For movement processes and stage-structured population only: max nb of steps per year IF >1 reproductive season. IF 0:  every individual completes the dispersal phase in one year, i.e. between two successive reproduction phases.
)

## Dispersal
disp = Dispersal(Emigration = emig,
                 Transfer = transfer,
                 Settlement = settle)

##### Initialise -----
eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = densdep, plot=F)
prop_stgs = eq_pop[-1]/sum(eq_pop[-1])
prop_stgs = round(prop_stgs,2)

## Initialise
init = Initialise(InitType = 1,  # InitType = 0: Free initialisation according to habitat map (default) (set FreeType), InitType = 1: From loaded species distribution map (set SpType), InitType = 2: From initial individuals list file
                  SpType = 0, # SpType = 0: All suitable cells within all distribution presence cells (default), SpType = 1: All suitable cells within some randomly chosen presence cells; set number of cells to initialise in NrCells.
                  InitDens = 2, # Number of individuals to be seeded in each cell/patch. InitDens = 0: At K_or_DensDep, InitDens = 1: At half K_or_DensDep (default), InitDens = 2: Set the number of individuals per cell/hectare to initialise in IndsHaCell.
                  IndsHaCell = indshacell, # Initial density in inds/ha
                  PropStages = c(0, prop_stgs), # For StageStructured models only: Proportion of individuals initialised in each stage. Requires a vector of length equal to the number of stages
                  InitAge = 2 # Initial age distribution within each stage. InitAge = 0: Minimum age for the respective stage. InitAge = 1 : Age randomly sampled between the minimum and the maximum age for the respective stage. InitAge = 2: According to a quasi-equilibrium distribution
)

##### Parameter master -----
s = RSsim(
  simul = sim,
  land = real_land,
  demog = demo,
  dispersal = disp,
  init = init
)

#### Plots ------
plotAbundance(s, dirpath) # Pop abundance
plotOccupancy(s, dirpath) # occupied patches

# Occupancy
# We plot the mean occupancy probability for each patch in year 100
# + the mean time to colonisation
terra::plot(landsc, col=c("#f0ffb3","darkgreen","green","#04170a","blue","red"))
# Store underlying landscape map display for later:
bg = function(main=NULL){
  terra::plot(landsc, axes=F, legend=F, col=c("#f0ffb3","darkgreen","green","#04170a","blue","red"),
              main=ifelse(is.null(main),"",main))
}
# as well as the extent of the landscape:
e = terra::ext(landsc)

# calculates the mean occupancy probability of given years as well as the time to colonisation for all replicates
col_stats_a = ColonisationStats(s, dirpath, years = 95, maps = T) # LONG
# mean occupancy probability in year 100
head(col_stats_a$occ_prob)
# time to colonisation
head(col_stats_a$col_time)

# If enabled, the function ColonisationStats() returns a raster stack with the mean occupancy probabilities of the given years as well as a raster with the mean time to colonisation over all replicates. 
# map occupancy probability
mycol_occprob = colorRampPalette(c('#c54e3d','#f3f7e8','#5b7a4f'))
terra::plot(col_stats_a$map_occ_prob, axes=F, range=c(0,1), col=mycol_occprob(10), type="continuous")
# map occupancy probability on landscape background
bg() 
terra::plot(col_stats_a$map_occ_prob, axes=F, range=c(0,1), col=mycol_occprob(10), type="continuous", plg=list(x="bottom", ext=c(e$xmin+400, e$xmax-400, e$ymin-150, e$ymin-50)), add =T)

# map colonisation time
mycol_coltime = colorRampPalette(c('#c54e3d','#e1a6a1','#f3f7e8','#b1c8a7','#5b7a4f'))
terra::plot(col_stats_a$map_col_time, axes=F, breaks=c(-9,seq(-9,100,length=11)), col=c('grey',mycol_coltime(20)), type="continuous")
# map colonisation time on landscape background
bg() 
terra::plot(col_stats_a$map_col_time, axes=F, breaks=c(-9,seq(-9,100,length=11)), col=c('grey',mycol_coltime(20)), type="continuous", plg=list(x="bottom", ext=c(e$xmin+400, e$xmax-400, e$ymin-150, e$ymin-50)), add =T)

# Heatmap
# the dispersal heatmap can be used to assess those parts of the landscape matrix that are frequently used for dispersal, whether it was successful or not
# Let’s first look at one replicate only and plot it:
library(viridis)
terra::plot(terra::rast(paste0(dirpath,"Output_Maps/Batch1_Sim1_Land1_Rep0_Visits.txt")),
     col=magma(9), axes=F)

# Because movement is stochastic, the number of visits per cell and the colonisation of empty patches are different for each replicate. 
# In order to take into account this variance, we average over all replicates and plot the resulting heatmap
# create a raster stack with all replicates as layers
heatmaps_stack <- terra::rast()
for(rep in 0:(s@simul@Replicates-1)){
  heatmaps_stack <- c(heatmaps_stack, 
                      terra::rast(paste0(dirpath, "Output_Maps/Batch", 
                                         s@control@batchnum, "_Sim", 
                                         s@simul@Simulation, "_Land", 
                                         s@land@LandNum,"_Rep",rep ,"_Visits.txt")))
}

# average over all layers
heatmaps_mean = terra::mean(heatmaps_stack)

# We create a non-linear color scale, in which the color changes more rapidly for small numbers of visits than higher ones:
# create exponential color scale
res <- 20
exp <- 3
lim <- max(terra::values(heatmaps_mean), na.rm = T)
my.at <- seq(1,lim^(1/exp),length.out=res)^exp

bg()
terra::plot(heatmaps_mean, 
     col=hcl.colors(res, "Inferno", rev = T, alpha=.8),
     breaks = my.at, axes=F, type="continuous", add=T, legend=F)


### Append results -----
# Load an Excel sheet with the parameters to test
summary = read_excel(here("data", 
                           "rangeshifter", 
                           "tests", 
                           "Summary_tests.xlsx"))

#### Information to add manually ----

test_aim = "The Pirineus patch was cut with the watershed; testing dispersal parameters with this new delimitation"
test_remarks = "Without the whole Pirineus patch, the pop size is below the 2005-2013 levels"

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
  "MS",
  "DP",
  "Step_mortality",
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
  dplyr::select(Year, MeanN)
pop_2005 = pop_sizes$MeanN[pop_sizes$Year == 0]
pop_2013 = pop_sizes$MeanN[pop_sizes$Year == 8]
pop_2022 = pop_sizes$MeanN[pop_sizes$Year == 17]
pop_2100 = pop_sizes$MeanN[pop_sizes$Year == 95]

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
  'MS' =
    format_tested_values(metadata$MS),
  'DP' =
    format_tested_values(metadata$DP),
  'Step_mortality' =
    format_tested_values(metadata$Step_mortality),
  
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

  'Patch_occ_2013' = n_occ_patches_sim,
  'Miss_patches_2013_%' = percent_missing_patches_2013,
  
  'Remarks' = test_remarks
)

#### Append to existing summary ----

# New information
new_summary = dplyr::bind_cols(
  new_summary,
  patch_summary
)

# Add to previous summary
summary = dplyr::bind_rows(
  summary,
  new_summary
)

#### Save --------
writexl::write_xlsx(
  summary,
  here(
    "data",
    "rangeshifter",
    "tests",
    "Summary_tests.xlsx"
  )
)
