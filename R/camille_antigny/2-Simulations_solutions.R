############################################################################################

# CAMILLE - Janvier 2025
# Simulation 30 paysages
  # Landscape : Matrice doit être 1, pas de 0
# Mortality per step = 0.005 et 0.02; 
# PR Method = 1 et 2
# Initialisation viabilité (partout) &  réintro (1 seul patch)
############################################################################################

library(RangeShiftR)
library(terra)
library(raster)
library(RColorBrewer)
library(stringr)
library(rstudioapi)
library(viridis)

library(dplyr)
library(plyr)
library(tidyverse)
library(igraph)
library(shape)
library(sf)

name_file = getSourceEditorContext()$path

name_file = str_replace(name_file, "/2-Simulations_solutions.R","/")
setwd(name_file)

dirpath <- "Overview/"

Res = 30

metadata = read.csv("Metadata_solutions.csv", sep= ",", stringsAsFactors = FALSE)
metadata = metadata %>% filter(!is.na(Id_simulation))

for (i in 1:nrow(metadata)) {
  
  # Extraction des paramètres pour chaque simulation
  id_simulation = metadata$Id_simulation[i]
  landscape = metadata$Landscape[i]
  step_mort_matrix = metadata$Mortalite_par_pas[i]
  PRMethod = metadata$PR_Method[i]
  initialisation = metadata$Initialisation[i]
  
  # Affichage de la progression des simu
  cat("\n========================================\n")
  cat(sprintf("Simulation %d/%d (ID: %d)\n", i, nrow(metadata), id_simulation))
  cat(sprintf("  Paysage: %s | Mortalité: %.3f | PR Method: %s\n", 
              landscape, step_mort_matrix, PRMethod))
  cat("========================================\n\n")

        # -- SIMULATION --#
          
        sim <- Simulation(Simulation = id_simulation,
                          Years = 100,
                          Absorbing = TRUE,
                          Replicates = 10,
                          Gradient = 0,
                          LocalExt = FALSE, 
                          OutIntRange = 10,
                          OutIntOcc = 10, 
                          OutIntPop = 10, 
                          OutIntInd = 10,
                          #OutIntGenetic = 10,
                          #OutIntTraitCell = 10,
                          #OutIntTraitRow = 10,
                          OutIntConn = 10,
                          #OutIntPaths = 10,
                          SMSHeatMap = TRUE)
          
        
        # -- LANDSCAPE --#
        
        # carrying capacities and landscape parameter object
        c_matrix = 0
        c_habitat = 0.2
        #c_hospitable_matrix = 0
        #c_roads = 0
        carrycap <- c(c_matrix,c_habitat)
        
        land <- ImportedLandscape(LandscapeFile = paste0("landscape_",landscape,".txt"), 
                                  Resolution = Res,
                                  HabPercent=FALSE,
                                  Nhabitats = 2, 
                                  PatchFile = paste0("cut_patches_",landscape,".txt"),
                                  K_or_DensDep = carrycap,
                                  SpDistFile = paste0("patch_central_",landscape,".txt"), #Si cas réintroduction via patch central
                                  SpDistResolution = Res) #Si cas réintroduction via patch central
        
        
        # -- DEMOGRAPHY --#
        
        trans_mat <- matrix(c(0, 0.35, 2, 0.8), nrow = 2)
        
        stg <- StageStructure(Stages = 2,
                              TransMatrix = trans_mat,
                              MaxAge = 8,
                              MinAge = c(0,0),
                              RepSeasons = 1, 
                              RepInterval = 0, 
                              PRep = 1, 
                              SurvSched = 1, 
                              FecDensDep = FALSE, 
                              DevDensDep = FALSE, 
                              SurvDensDep = FALSE,
                              #DevDensCoeff = 1,
                              #SurvDensCoeff = 1,
                              PostDestructn = FALSE)
        
        demo <- Demography(StageStruct = stg,
                           ReproductionType = 0)
        
        demo <- demo + stg
        
        plotProbs(stg)
        
        # -- DISPERSAL --#
        
        emig_prob <- matrix(c(0,1,0.8,0,10,0,0.5,0), nrow = 2) 
        
        emig <- Emigration(EmigProb = emig_prob,
                           SexDep = FALSE, 
                           StageDep = TRUE, 
                           DensDep = TRUE, 
                           IndVar = FALSE) 
        plotProbs(emig)
        
        
        #SMS
        costs_dep_matrix = 50
        costs_dep_habitat = 1
        #costs_dep_hospitable_matrix = 1
        #costs_dep_roads = 100
        costs_dep = c(costs_dep_matrix,costs_dep_habitat)
        
        #step_mort = c(0.005,0)
        step_mort_matrix = step_mort_matrix
        step_mort_habitat = 0
        #step_mort_hospitable_matrix = 0
        #step_mort_roads = 0.1
        step_mort = c(step_mort_matrix,step_mort_habitat)
        #step_mort = step_mort
        
        trans<- SMS(PR = 6,
                    PRMethod = PRMethod, 
                    MemSize = 14, 
                    DP = 1.5, 
                    GoalType = 2,
                    GoalBias = 1.3,
                    AlphaDB = 0.01,
                    BetaDB = 1000,
                    IndVar = FALSE,
                    Costs = costs_dep, 
                    StepMort = step_mort, 
                    StraightenPath = TRUE)
        plotProbs(trans)
        
        set_prob <- matrix(c(0,1,0.5,0.5,-5,-5,0.5,0.5), nrow = 2)
        set <- Settlement(Settle = set_prob,
                          SexDep = FALSE,
                          StageDep = TRUE,
                          DensDep = TRUE,
                          IndVar = FALSE,
                          FindMate = FALSE,
                          MaxSteps = 300)
        plotProbs(set) 
        
        disp <-  Dispersal(Emigration = emig, 
                           Transfer   = trans,
                           Settlement = set)
        
        
        # -- INITIALIZATION --#
        #init <- Initialise(InitType = 1,
        #InitDens = 2, 
        #IndsHaCell = 0.2, 
        #PropStages = c(0,1),
        #InitAge = 1)
        #init
        
        #OU
        init <- Initialise(InitType = initialisation,    # cas réintro (= 1) / cas viabilité (= 0)
                           FreeType = 1,   # cas viabilité (=1)
                           #NrCells = 15,
                           SpType = 0,      # cas réintro (=0)
                           InitDens = 1,
                           #IndsHaCell =0.2,
                           PropStages = c(0,1),
                           InitAge = 1)
        init
        
        #InitType : 
        # 0 : Free initialisation according to habitat map
        # 1 : From loaded species distribution map
        # 2 : From initial individuals list file
        
        # FreeType : 
        # 0 : Random; provide number of cells/patches to initialise in \code{NrCells}
        # 1 : All suitable cells/patches
        
        # SpType 
        # 0 : All suitable cells within all distribution presence cells (default)
        # 1 : All suitable cells within some randomly chosen presence cells; set number of cells to initialise in NrCells
        
        # InitDens : 
        # 0 : At K_or_DensDep
        # 1 : At half K_or_DensDep (default)
        # 2 : Set the number of individuals per cell/hectare to initialise IndsHaCell
        
        # -- PARAMETER MASTER --#
        s <- RSsim() + land + demo + disp + sim + init
        validateRSparams(s)
        
        
        # -- SIMULATION --#
        RunRS(s, dirpath)
}

        #id_simulation = id_simulation +1
