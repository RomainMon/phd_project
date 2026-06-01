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
sim = Simulation(Simulation = 2,
                 Years = 50,
                 Replicates = 2,
                 OutIntPop = 50)

#### 2. Landscape -----
# To create an artificial landscape
# land = ArtificialLandscape()

##### Import a landscape -----
landsc = terra::rast(file.path(dirpath, "Inputs", "raster_reclass_binary_2005.txt"))

landsc.f = as.factor(landsc)
# add the land cover classes to the raster attribute table
rat = levels(landsc.f)[[1]][-2]
rat[["landcover"]] <- c("sea","matrix","habitat")
levels(landsc.f) = rat
plot(landsc.f, axes = F, col=c("white","lightgrey","darkgreen"))

##### Patches --------
patch <- terra::rast(file.path(dirpath, "Inputs", "patches_2005.txt"))
# We can have a glimpse at how many cells the different patches contain:
table(values(patch))
# Plot the patches in different colours
plot(patch)

##### Define the landscape -----
?ImportedLandscape
land = ImportedLandscape(LandscapeFile = "raster_reclass_binary_2005.txt",
                          PatchFile = "patches_2005.txt", 
                          Resolution = 30,
                          Nhabitats = 3,
                          K_or_DensDep = c(0, 0.075, 0))
land
# K_or_DensDep: determines the demographic density dependence of the modelled species and is given in units of the number of individuals per hectare (defaults to 
# If HabPercent=FALSE: a vector of length Nhabitats, specifying the respective K_or_DensDep for every habitat code.
# SpDistFile: Filename of the species initial distribution map which shall be imported (*.txt). Default is NULL.


#### 3. Demography ------
# To make a stage-structured model, we have to additionally create a stage-structure sub-module within the Demography module. 
# Here, we have already defined a Demography object and can use ‘+’ to add the StageStructure sub-module.
stg = StageStructure(Stages = 3,
                     TransMatrix = matrix(c(0,1,0,5.7,.5,.4,3.4,0,.9),nrow = 3),
                     FecDensDep = T,
                     SurvDensDep = T)

demo = Demography(StageStruct = stg, ReproductionType = 1, PropMales = 0.45)
plotProbs(stg) # plot the rates from the transition matrix


#### 4. Dispersal -------
disp =  Dispersal(Emigration = Emigration(EmigProb = 0.2), 
                  Transfer   = DispersalKernel(Distances = 50),
                  Settlement = Settlement() )
# Plot a dispersal kernel with a stage-dependent mean transfer distance:
plotProbs(DispersalKernel(Distances = matrix(c(0,1,2,70,50,30),nrow = 3), StageDep = T))

# Update Settlement conditions
disp =  disp + Settlement(SexDep = T, 
                          Settle = matrix(c(0,1,1,0), nrow = 2),
                          FindMate = matrix(c(1,0,F,F), nrow = 2))


#### 5. Genetics -------
# The genetics module controls the heritability and evolution of traits and is needed if inter-individual variability is enabled (IndVar = TRUE) e.g. for at least one dispersal trait

#### 6. Initialisation ----


#### 7. Parameter master -----
s = RSsim(simul = sim, 
           land = land, 
           demog = demo, 
           dispersal = disp2, 
           init = init)

# Check for parameter conflicts
validateRSparams(s)

### Run simulations --------
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