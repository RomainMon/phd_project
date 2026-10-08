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
library(tidyterra)
library(patchwork)
library(viridis)


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
  test_name = "test_resist_17", # Name of the folder
  
  # Parameters tested with sensitivity analysis (i.e., those varying during simulations)
  parameters = c(
    "IndsHaCell",
    "PR"
  )
)

dirpath = here::here(
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
patch_uncut = sf::st_read(here::here("outputs", "data", "patches_rshifter", 
                                     "patches_rshifter_2005.gpkg")) 
plot(patch_uncut)

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

### Pop. matrix 
# Juv. survival = 0.56 (Ponchon et al. 2026)
# Ad. survival = 0.89 (Ponchon et al. 2026)
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
eq_pop = getLocalisedEquilPop(demog = demo, 
                              DensDep_values = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.095, 0.1, 0.2, 0.3)) #  absolute values of individuals
# Select the value that reaches the desired threshold
colSums(eq_pop)
# The mean GLT density per patch (in 2014) is 0.086 ind/ha (Ruiz-Miranda et al. 2019)
# GLT density ranges 1.7 inds/km² (0.017 inds/ha) (Gavioes) to 19.0 inds/km² (0.19 inds/ha) (Rio Vermelho, Imbau I) (Ruiz-Miranda et al. 2019)
# NOTE: 8.6 inds / 100 ha (1 km²) = 0.086 inds/ha.
# This value corresponds to the 4th or 5th column, which means that 1/b = 0.08 or 1/b = 0.09)

# calculate proportion of all stages excluding the new-born juvenile (stage 0) population, 
# which can't be initialised:
eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = 0.088, plot=F) # Put DensDep_values
prop_stgs = eq_pop[-1]/sum(eq_pop[-1])
round(prop_stgs,2)


### Parameters file ----
# Load an Excel sheet with the parameters to test
metadata = read_excel(here::here("data", 
                           "rangeshifter", 
                           "tests", 
                           test_config$test_name,
                           "test_parameters.xlsx"),
                      sheet="test1")

# Transform TRUE/FALSE values into logical
metadata$StraightenPath = ifelse(metadata$StraightenPath == "TRUE", TRUE, FALSE)


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
  pr_meth = metadata$PR_meth[i]
  ms = metadata$MS[i]
  dp = metadata$DP[i]
  step_mortality = metadata$Step_mortality[i]
  max_nb_steps = metadata$Max_nb_steps[i]
  goal_type = metadata$Goal_type[i]
  goal_bias = if (goal_type != 0) {
    metadata$Goal_bias[i]
  } else {
    NULL
  }
  straightenpath = metadata$StraightenPath[i]
  
  nhab = metadata$Nhab[i]
  costs = unlist(metadata[i, c("Cost1", "Cost2", "Cost3", "Cost4", "Cost5", "Cost6")])
  
  id_simulation = metadata$Id_simul[i]
  
  # Print progression
  cat("\n========================================\n")
  
  cat(sprintf(
    "Simulation %d/%d (ID: %d)\n",
    i, nrow(metadata), id_simulation
  ))
  
  cat(sprintf(
    "  DensDep = %.3f | Juv survival = %.3f | Adult survival = %.3f | IndsHaCell = %.3f\n",
    densdep,
    juv_survival,
    ad_survival,
    indshacell
  ))
  
  cat(sprintf(
    "  EmigProb = %.3f | Perceptual range = %.3f | PR method = %.3f | Memory size = %.3f | Directional persistence = %.3f | Nhab = %.3f | Step mortality = %.3f | Goal type = %.3f | Straighten Path = %.3f\n",
    emig_prob,
    pr,
    pr_meth,
    ms,
    dp,
    nhab,
    step_mortality,
    goal_type,
    straightenpath
  ))
  
  cat(sprintf(
    "  Costs = %s\n",
    paste(sprintf("%.3f", costs), collapse = " | ")
  ))
  
  cat("========================================\n\n")
  
  ##### 1) Simulation -----
  # This module is used to set general simulation parameters (e.g. simulation ID, number of replicates, and number of years to simulate) and to control output types (plus some more specific settings).
  sim = Simulation(Simulation = id_simulation, # Update simulation id
                   Replicates = 20, # Number of replicates
                   Years = 96, # Number of years
                   OutIntPop = 1, # Whether to export population files 
                   OutIntOcc = 0, # Whether to export occupancy files (every X year)
                   OutIntRange = 10, # Whether to export range files (every X year)
                   OutIntInd = 1, # Whether to export individual files
                   OutIntConn = 1, # Whether to export connectivity files (n individuals from patch i to patch j)
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
  if (goal_type != 0) {
  
    transfer = SMS(PR = pr, # Perceptual range in nb of cells (must be integer)
                   PRMethod = pr_meth, # Method to evaluate the effective cost of a particular step from the landscape within the perceptual range: 1 = Arithmetic mean, 2 = Harmonic mean, 3 = Weighted arithmetic mean
                   MemSize = ms, # Memory size (nb of previous steps over which to calculate current direction to apply directional persistence). Default = 1, max = 14
                   DP = dp, # Directional persistence: tendency to follow a CRW. Must be >= 1 (default to 1)
                   GoalType = 2, # Goal bias type (i.e., a tendency to move towards a particular destination). 0 = None, 2 = Dispersal bias (i.e., moving away from the natal location)
                   GoalBias = goal_bias, # Must be ≥ 1.0
                   AlphaDB = 1, # decay rate (slope) of the dispersal bias
                   BetaDB = 100000, # inflection point (in terms of number of steps taken) of the dispersal bias
                   # -> If it is desired that there is negligible decay in dispersal bias (i.e. the individual maintains a tendency to move away from its natal location indefinitely), then retain the default values (1.0 and 100000)
                   IndVar = F, # Individual variability in SMS traits?
                   Costs = costs, # Landscape resistance to movement (for each land cover)
                   StepMort = step_mortality, # Per-step mortality probability. Constant or habitat-specific
                   StraightenPath = straightenpath # Straigten path after decision not to settle in a patch?
    )
  
  } else {
    
    transfer = SMS(PR = pr, # Perceptual range in nb of cells (must be integer)
                   PRMethod = pr_meth, # Method to evaluate the effective cost of a particular step from the landscape within the perceptual range: 1 = Arithmetic mean, 2 = Harmonic mean, 3 = Weighted arithmetic mean
                   MemSize = ms, # Memory size (nb of previous steps over which to calculate current direction to apply directional persistence). Default = 1, max = 14
                   DP = dp, # Directional persistence: tendency to follow a CRW. Must be >= 1 (default to 1)
                   GoalType = 0, # Goal bias type (i.e., a tendency to move towards a particular destination). 0 = None, 2 = Dispersal bias (i.e., moving away from the natal location)
                   IndVar = F, # Individual variability in SMS traits?
                   Costs = costs, # Landscape resistance to movement (for each land cover)
                   StepMort = step_mortality, # Per-step mortality probability. Constant or habitat-specific
                   StraightenPath = T # Straigten path after decision not to settle in a patch?
    )
  }
  
  ## Settlement (or immigration)
  settle = Settlement(StageDep = F, # Stage-dependent settlement requirements?
                      SexDep = F, # Sex-dependent settlement requirements?
                      Settle = 0, # CODES (dor DispersalKernel) or PROBA (for Movement processes if DensDep = TRUE) for all stages, sexes. Default = 0 (i.e. 'die when unsuitable' for DispersalKernel and 'always settle when suitable' for Movement process)
                      FindMate = F, # Mating requirements to settle? FALSE if female-only model
                      DensDep = F, # For movement processes only: Density-dep settlement probability?
                      IndVar = F, # For movement processes only: Individual variability in settlement probability traits?
                      MinSteps = 0, # For movement processes only: min number of steps
                      MaxSteps = pr*max_nb_steps, # For movement processes only: max number of steps
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