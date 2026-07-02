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

dirpath = "data/rangeshifter/tests/test_disp_5/" # UPDATE HERE !!!
## Create the RS folder structure, if it doesn’t yet exist
# dir.create(file.path(dirpath, "Inputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Outputs"), showWarnings = TRUE)
# dir.create(file.path(dirpath, "Output_Maps"), showWarnings = TRUE)

### Check the landscape -----
## Import a landscape
landsc = terra::rast(file.path(dirpath, "Inputs", "raster_reclass_binary_2005.txt"))
terra::plot(landsc, col=c("gray","darkgreen"))
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
?getLocalisedEquilPop
par(mfrow=c(1,1))
eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.095, 0.1, 0.2, 0.3)) #  absolute values of individuals
# Select the value that reaches the desired threshold
colSums(eq_pop)

# calculate proportion of all stages excluding the new-born juvenile (stage 0) population, 
# which can't be initialised:
eq_pop = getLocalisedEquilPop(demog = demo, DensDep_values = 0.089, plot=F)
prop_stgs = eq_pop[-1]/sum(eq_pop[-1])
round(prop_stgs,2)

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
                                       "test_disp_5", # UPDATE HERE
                                       "Inputs", 
                                       "patch_corres_id_2005.csv"),
                                col_types = readr::cols(unique_id = readr::col_integer()))

#### Parameters file ----
metadata = read_excel(here("data", 
                           "rangeshifter", 
                           "tests", 
                           "test_disp_5",  # UPDATE HERE !!!
                           "test_parameters.xlsx"),
                      sheet="test1")

#### Loop -----
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
  
  id_simulation = metadata$Id_simul[i]
  
  # Print progression
  cat("\n========================================\n")
  cat(sprintf("Simulation %d/%d (ID: %d)\n",
              i, nrow(metadata), id_simulation))
  cat(sprintf("  DensDep = %.3f | Juv suvival = %.3f | Adult suvival = %.3f | IndsHaCell = %.3f\n",
              densdep, juv_survival, ad_survival, indshacell))
  cat(sprintf("  EmigProb = %.3f | Perceptual range = %.3f | Memory size = %.3f | Directional persistence = %.3f | Step mortality = %.3f\n",
              emig_prob, pr, ms, dp, step_mortality))
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
    LandscapeFile = "raster_reclass_binary_2005.txt",
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
  transfer = SMS(PR = pr, # Perceptual range in nb of cells (must be integer)
                 PRMethod = 1, # Method to evaluate the effective cost of a particular step from the landscape within the perceptual range: 1 = Arithmetic mean, 2 = Harmonic mean, 3 = Weighted arithmetic mean
                 MemSize = ms, # Memory size (nb of previous steps over which to calculate current direction to apply directional persistence). Default = 1, max = 14
                 DP = dp, # Directional persistence: tendency to follow a CRW. Must be >= 1 (default to 1)
                 GoalType = 0, # Goal bias type (i.e., a tendency to move towards a particular destination). 0 = None, 2 = Dispersal bias (i.e., moving away from the natal location)
                 IndVar = F, # Individual variability in SMS traits?
                 Costs = c(50,1), # Landscape resistance to movement (for each land cover)
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
       "test_disp_5", # UPDATE HERE
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
# UPDATE HERE
param1 <- "Step_mortality"
param2 <- "PR"
param3 <- "DP"

##### Line plot ------
### Plot
pop_total = pop_all %>%
  dplyr::group_by(Id_simul, !!sym(param1), !!sym(param2), !!sym(param3), Rep, Year) %>%
  dplyr::summarise(NInd = sum(NInd), .groups = "drop")
pop_time = pop_total %>%
  dplyr::group_by(!!sym(param1), !!sym(param2), !!sym(param3), Year) %>%
  dplyr::summarise(MeanN = mean(NInd),.groups = "drop")
ggplot(pop_time, aes(Year, MeanN, colour = factor(!!sym(param1)))) + # Parameter that varies as colour
  geom_line(linewidth = 1) +
  # Faceting
  facet_grid(
    rows = vars(!!sym(param2)), # Parameter that varies
    cols = vars(!!sym(param3)), # Other parameter that varies
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
  annotate("text", x = 8, y = 3706, label = "2014", vjust = -1) +
  annotate("text", x = 17, y = 4869, label = "2022", vjust = -1) +
  theme_bw() +
  labs(y = "Mean population size")

## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         "test_disp_5", # UPDATE HERE
         "plot",
         "evol_pop.png"), # UPDATE HERE
    width = 3000, height = 3000, res = 300, type="cairo")
ggplot(pop_time, aes(Year, MeanN, colour = factor(!!sym(param1)))) + # Parameter that varies as colour
  geom_line(linewidth = 1) +
  # Faceting
  facet_grid(
    rows = vars(!!sym(param2)), # Parameter that varies
    cols = vars(!!sym(param3)), # Other parameter that varies
    scales = "free_y") +
  # Vertical reference years
  geom_vline(
    xintercept = c(0, 9, 17),
    linetype = "dashed",
    colour = "black"
  ) +
  annotate("point", x = 0,  y = 1600, size = 3) +
  annotate("point", x = 8, y = 3706, size = 3) +
  annotate("point", x = 17, y = 4869, size = 3) +
  annotate("text", x = 0,  y = 1600, label = "2005", vjust = -1) +
  annotate("text", x = 8, y = 3706, label = "2014", vjust = -1) +
  annotate("text", x = 17, y = 4869, label = "2022", vjust = -1) +
  theme_bw() +
  labs(y = "Mean population size")

dev.off()

##### Comparison with long-term data -----
### Identify good parameters by comparing with real pop
## Identify parameters that provide the same initial and final pop. size
initial_popsize = 1600 # Adjust
final_popsize = 3706 # Adjust
initial_pop = pop_time %>%
  dplyr::filter(Year == min(pop_time$Year)) %>%
  dplyr::filter(abs(MeanN - initial_popsize) < 20)  # Adjust tolerance
final_pop = pop_time %>%
  dplyr::filter(Year == 8) %>% # 2014
  dplyr::filter(abs(MeanN - final_popsize) < 200)  # Adjust tolerance

# Get the parameter combinations for initial and final populations
initial_params = initial_pop %>%
  dplyr::select(!!sym(param1),
                !!sym(param2),
                !!sym(param3)) %>%
  dplyr::distinct()
final_params = final_pop %>%
  dplyr::select(!!sym(param1),
                !!sym(param2),
                !!sym(param3)) %>%
  dplyr::distinct()
# Find the intersection
dplyr::inner_join(initial_params, final_params)

## Heatmap plot
fit_score = pop_time %>%
  dplyr::group_by(!!sym(param1),
                  !!sym(param2),
                  !!sym(param3)) %>%
  dplyr::summarise(
    InitialN = MeanN[Year == min(Year)],
    FinalN = MeanN[Year == 8],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Error_initial = abs(InitialN - initial_popsize),
    Error_final = abs(FinalN - final_popsize),
    # total distance from observed values
    Total_error = Error_initial + Error_final
  )
# Plot
ggplot(fit_score, aes(x = !!sym(param2), y = !!sym(param3), fill = Total_error)) +
  geom_tile() +
  facet_wrap(~.data[[param1]]) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1
  ) +
  theme_bw()

## Export plot
png(here("data",
         "rangeshifter",
         "tests",
         "test_disp_5", # UPDATE HERE
         "plot",
         "heatmap.png"),
    width = 2000, height = 1000, res = 300, type="cairo")
ggplot(fit_score, aes(x = !!sym(param2), y = !!sym(param3), fill = Total_error)) +
  geom_tile() +
  facet_wrap(~.data[[param1]]) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1
  ) +
  theme_bw()
dev.off()

## RMSE
fit_score = pop_time %>%
  dplyr::group_by(
    !!sym(param1),
    !!sym(param2),
    !!sym(param3)
  ) %>%
  dplyr::summarise(
    N0 = MeanN[Year == 0],
    N8 = MeanN[Year == 8],
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    RMSE = sqrt(((N0 - initial_popsize)^2 + (N8 - final_popsize)^2) / 2))
# heatmap RMSE
ggplot(fit_score, aes(x = !!sym(param2),
                      y = !!sym(param3),
                      fill = RMSE)) +
  geom_tile() +
  facet_grid(~.data[[param1]]) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1) +
  theme_bw()
# Rank the best options
fit_score %>%
  dplyr::arrange(RMSE) %>%
  dplyr::slice(1:10)


##### Patch abundance -----
pop_patch = pop_all %>%
  dplyr::group_by(PatchID, !!sym(param1), !!sym(param2), !!sym(param3), Year) %>%
  dplyr::summarise(MeanN = mean(NInd),.groups = "drop")
# Join patch name
patch_list = c("Aldeia_I_1",
               "Aldeia_I_2",
               "Sta_Helena",
               "Imbau_I_2",
               "Afetiva",
               "Nova_Esperanca_2",
               "Pirineus_111",
               "Poco_das_Antas",
               "Rio_Vermelho",
               "Uniao_N_2")
pop_patch = pop_patch %>% 
  dplyr::left_join(patch_corres_id, by=c("PatchID" = "unique_id")) %>% 
  dplyr::rename(patch_name = patch_id) %>% 
  dplyr::filter(!is.na(patch_name)) %>% 
  dplyr::filter(patch_name %in% patch_list)

### Compare with patch abundance over time
# Select parameters and years
sim_sel = pop_patch %>%
  dplyr::filter(
    Step_mortality == 0.01,
    DP == 5,
    PR == 2,
    Year %in% c(0, 8, 13, 17) # Years of interest
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
  dplyr::group_by(FragName, Year) %>%
  dplyr::summarise(
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
      Survey == "n_glt_2023" ~ 17
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
         "test_disp_5", # UPDATE HERE
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
         "test_disp_5", # UPDATE HERE
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
