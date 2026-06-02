#------------------------------------------------#
# Author: Romain Monassier
# Objective: Running RangeShiftR simulations
#------------------------------------------------#

### Load packages ------
library(RangeShiftR)
library(here)

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

#### Master object -----
s = RSsim() # Master object

#### Path ----
dirpath = here("data","rangeshifter")
# Create the RS folder structure, if it doesn’t yet exist
# dir.create(file.path(dirpath, "Inputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Outputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Output_Maps"), showWarnings = TRUE)


### Parameters ----
s

# It contains of a number of parameter modules that each define different aspects of the RangeShifter simulation. Specifically, there are:
# 1) Simulation
# 2) Landscape
# 3) Demography
# 4) Dispersal
# 5) Genetics
# 6) Management
# 7) Initialisation

#### 1. Simulations ------
# This module is used to set general simulation parameters (e.g. simulation ID, number of replicates, and number of years to simulate) and to control output types (plus some more specific settings).
# ?Simulation
sim = Simulation(Simulation = 0, 
                 Replicates = 20, 
                 Years = 9,
                 OutIntPop = 1,
                 OutIntOcc = 1,
                 OutIntRange = 1)

#### 2. Landscape -----
# To create an artificial landscape
# land = ArtificialLandscape()

##### Import a landscape -----
landsc = terra::rast(file.path(dirpath, "Inputs", "raster_reclass_binary_2005.txt"))
terra::plot(landsc, col=c("gray","darkgreen","orange"))
terra::res(landsc)
terra::unique(landsc)

##### Patches --------
patch = terra::rast(file.path(dirpath, "Inputs", "patches_2005.txt"))
# We can have a glimpse at how many cells the different patches contain:
table(terra::values(patch))
# Plot the patches in different colours
npatch = max(terra::values(patch), na.rm = TRUE)
# Create colors: white for background + random colors for patches
cols = c("gray", rainbow(npatch))
terra::plot(
  patch,
  col = cols,
  type = "classes",
  legend = FALSE)

##### Species distribution -----
patch_w_glt  = terra::rast(file.path(dirpath, "Inputs", "patches_w_glt_2005.txt"))
terra::plot(patch_w_glt, col=c("gray","darkgreen"))
terra::unique(patch_w_glt)

##### Define the landscape -----
?ImportedLandscape
land = ImportedLandscape(LandscapeFile = "raster_reclass_binary_2005.txt",
                         PatchFile = "patches_2005.txt", 
                         Resolution = 28.35578,
                         Nhabitats = 3,
                         K_or_DensDep = c(0, 0.075, 0),
                         SpDistFile = "patches_w_glt_2005.txt",
                         SpDistResolution = 28.35578)
land
# K_or_DensDep: determines the demographic density dependence of the modelled species and is given in units of the number of individuals per hectare (defaults to 
# If HabPercent=FALSE: a vector of length Nhabitats, specifying the respective K_or_DensDep for every habitat code.
# SpDistFile: Filename of the species initial distribution map which shall be imported (*.txt). Default is NULL.


#### 3. Demography ------
# To make a stage-structured model, we have to additionally create a stage-structure sub-module within the Demography module. 
# We can use ‘+’ to add the StageStructure sub-module.


##### Female-only model ----
# Female-only models assume that males are not limiting, and that the population dynamics are driven only by females. 
# It also means that sexes are not modelled explicitly and it is not possible to account for behaviours like mate-finding in the settlement decisions; females will settle in suitable habitat patches and then will automatically be able to attempt reproduction.

# Change demography settings
?StageStructure
mat = matrix(c(0, 0, 2,
         0.9, 0, 0,
         0, 0.56, 0.85),
       nrow=3, byrow=T)
stg = StageStructure(Stages = 3,
                     TransMatrix = mat,
                     MaxAge = 20,
                     RepSeasons = 1,
                     SurvSched = 1, # Between reproductive events
                     FecDensDep = T)
plotProbs(stg) # plot the rates from the transition matrix

# Female-only model
demo = Demography(StageStruct = stg,
                     ReproductionType = 0)


#### 4. Dispersal -------

##### Emigration -------
?Emigration
emig = Emigration(EmigProb = 0,
                  StageDep = F,
                  DensDep = F)

##### Transfer -------
?Transfer
?DispersalKernel
transfer = DispersalKernel(Distances = matrix(c(100),nrow=1),
                           StageDep = F)

##### Settlement -------
?Settlement
# Settle = 0 means 'die when unsuitable' for DispersalKernel and 'always settle when suitable' for Movement process
settle = Settlement(StageDep = F,
                    Settle = 0,
                    FindMate = F,
                    DensDep = F)

##### Dispersal -----
?Dispersal
disp = Dispersal(Emigration = emig,
          Transfer = transfer,
          Settlement = settle)


#### 5. Genetics -------
# The genetics module controls the heritability and evolution of traits and is needed if inter-individual variability is enabled (IndVar = TRUE) e.g. for at least one dispersal trait

#### 6. Initialisation ----
?Initialise
init = Initialise(InitType = 1,  # from loaded species distribution map
                   SpType = 0,   # All suitable cells within all distribution presence cells
                   InitDens = 2, # Set the number of individuals per cell/hectare to initialise in IndsHaCell
                   IndsHaCell = 0.086, # Number of individuals per ha
                   PropStages = c(0,0.5,0.5), # For StageStructured models only: Proportion of individuals initialised in each stage. Requires a vector of length equal to the number of stages
                   InitAge = 0)

#### 7. Parameter master -----
?RSsim
s = RSsim(simul = sim, 
          land = land, 
          demog = demo, 
          dispersal = disp, 
          init = init)

# Check for parameter conflicts
validateRSparams(s)

### Run simulations --------
dirpath = "data/rangeshifter/"
RunRS(s, dirpath)

### Plot results -----
range_df = readRange(s, dirpath)

# ...with replicates:
par(mfrow=c(1,2))
plotAbundance(range_df)
plotOccupancy(range_df)

# ...with standard deviation:
par(mfrow=c(1,2))
plotAbundance(range_df, sd=T, replicates = F)
plotOccupancy(range_df, sd=T, replicates = F)

# read population output file into a dataframe
pop_df = readPop(s, dirpath)

# Make data frame with number of individuals per output year - for only one repetition (Rep==0):
pop_wide_rep0 = reshape(subset(pop_df,Rep==0)[,c('Year','PatchID','NInd')], 
                        timevar='Year', v.names=c('NInd'), idvar=c('PatchID'), direction='wide')
head(pop_wide_rep0)
