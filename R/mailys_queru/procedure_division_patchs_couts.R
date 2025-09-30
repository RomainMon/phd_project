############################################################################################

# Mailys - juin 2023
"
Procdure qui permet de créer les patchs de taille correcte
"

############################################################################################

##### 0) Importation des libraires

# Load Packages
library(raster)
library(terra)
library(maptools)
library(fields)
library(sp)
library(rgdal)
library(landscapemetrics)
library(rgeos) 
library(gridExtra)
library(dplyr)
library(sf)
library(fasterize)



##############################################
## PARTIE I : CREATTION DES PATCHS ##
##############################################


##### 1) Définition du répertoire de travail et ouverture des fichiers

setwd("D:/STAGE MODELISATION/COMPARAISON PETITS GRANDS PATCHS/CREATION DES PAYSAGES")

id_paysage = "C"

## Import the working data/raster (tree density from Copernicus)
for_den = rast("D:/STAGE MODELISATION/Carto/Automatisation_traitement_carto_R/Inputs/forest_density100.tif")


## Import data/shp (study area)
area = vect("D:/STAGE MODELISATION/Carto/Automatisation_traitement_carto_R/Inputs/Zone_etude_C.shp")
area = ext(area)


## Import data/shp (roads) and subset the study area
roads = vect("D:/STAGE MODELISATION/Carto/Automatisation_traitement_carto_R/Inputs/routes.shp")
roads_crop = crop(x=roads, y=ext(area))


## Subset the study area
for_den_crop = crop(x=for_den, y=ext(area), mask=TRUE)
plot(for_den_crop, main = "ETAPE 1 : Occupation du sol dans le paysage C")


##Save the new file
filename = paste("forest_density",id_paysage, sep="_")
writeRaster(x = for_den_crop, filename = paste(filename,"tif",sep="."),overwrite=TRUE)


##### 2) Définition des métadonnées des fichiers 

resolution = res(for_den_crop)[1]
nb_cell_paysage = ncell(for_den_crop)
nb_lignes = nb_colonnes = sqrt(nb_cell_paysage)
cell_size = resolution * resolution
aire_tot = (nb_cell_paysage*resolution)^2



##### 3) Définition de la couche "habitat"

#PARAMETRE : 
densite_limite_habitat = 30

#On ne conserve que les cellules pour lesquelles la densité est supérieure à 30%
habitat_30 = for_den_crop>densite_limite_habitat
legende <- c("Matrice", "Habitat")
couleurs <- c("#E2DEDE", "#0E9813")
plot(habitat_30, 
     type = "classes", 
     levels = legende,
     col = couleurs, 
     plg = list(cex = 0.7),
     main = "ETAPE 3 : discretisation de l'habitat (>30% d'arbres)"
)

##Save the new file
filename = paste("habitat_30",id_paysage, sep="_")
writeRaster(x = habitat_30, filename = paste(filename,"tif",sep="."),overwrite=TRUE)



##### 4) Dilatation - Erosion

# Importation de la fonction
source("D:/STAGE MODELISATION/Carto/Automatisation_traitement_carto_R/Fonctions/fct_dilatation_erosion.R")

# Définition des paramètres : 
"ne pas changer le working directory
fichier : fichier d'habitat créé précédement, format raster
seuil : taille du buffer"
working_directory = getwd()
file = "habitat_30_C.tif"
seuil = 100

# Utilisation de la fonction
dilatation_erosion_paysage = dilatation_erosion(working_directory,file,seuil)

plot(dilatation_erosion_paysage, 
     type = "classes", 
     main = "Paysage C après dilatation - erosion",
     levels = legende,
     col = couleurs, 
     plg = list(cex = 0.7)
)



##### 5) Exclusion des routes avant définition des patchs
dilatation_erosion_paysage_sans_routes = mask(dilatation_erosion_paysage,roads, inverse=TRUE)
plot(dilatation_erosion_paysage_sans_routes, 
     type = "classes", 
     main = "Paysage C après exclusion des routes",
     levels = c("Matrice", "Habitat","Routes"),
     col = c("#E2DEDE", "#0E9813","#FFFFFF"), 
     plg = list(cex = 0.7)
)




##### 6) Définition des patchs - region group

if (minmax(dilatation_erosion_paysage_sans_routes, compute=FALSE)[1]==1) { 
  #Si la valeur la plus petite est 1, on repasse en 0
  dilatation_erosion_paysage_sans_routes = dilatation_erosion_paysage_sans_routes-1
}
patchs = patches(dilatation_erosion_paysage_sans_routes,directions = 8, zeroAsNA=TRUE, allowGaps=FALSE)

# Affichage des patchs
plot(patchs)
#freq_patchs = freq(patchs)
#View(freq_patchs)

##### FICHIER PATCHS TAILLE INITIALE : 
filename = paste("patchs_initiaux",id_paysage, sep="_")
writeRaster(x = patchs, filename = paste(filename,"tif",sep="."),overwrite=TRUE)



#Récupérer la surface de chaque patch
taille_patchs <- zonal(cellSize(x=patchs, unit="m"), patchs, sum, as.raster=TRUE)
seuil_mini = 22500 #m²
seuil_maxi = 250000 #m² = 25ha

## Ne garder que les patchs dont la surface est inférieure à la surface minimum, pour le fichier de couts
patchs_inf_seuil_mini <- ifel(taille_patchs > seuil_mini, NA, patchs)
plot(patchs_inf_seuil_mini)

#On remplace toutes les valeurs non nulles (=habitat) par 1
patchs_inf_seuil_mini[patchs_inf_seuil_mini > 0] <- 1

##Save the new file
filename = paste("patchs_inf",seuil_mini, sep="_")
filename = paste(filename,id_paysage, sep="_")
writeRaster(x = patchs_inf_seuil_mini, filename = paste(filename,"tif",sep="."),overwrite=TRUE)


## Création d'un raster avec les patchs dont la surface est comprise entre min et max
patchs_superieur_seuil_mini <- ifel(taille_patchs < seuil_mini, NA, patchs)
patchs_taille_ok <- ifel(taille_patchs > seuil_maxi, NA, patchs_superieur_seuil_mini)
plot(patchs_taille_ok)

##Save the new file
filename = paste("patchs_entre",seuil_mini, sep="_")
filename = paste(filename,"et", sep="_")
filename = paste(filename,seuil_maxi, sep="_")
filename = paste(filename,id_paysage, sep="_")
writeRaster(x = patchs_taille_ok, filename = paste(filename,"tif",sep="."),overwrite=TRUE)

id_max = minmax(patchs_taille_ok, compute=TRUE)[2]


## Création d'un raster avec les patchs dont la surface est supérieure au max --> tessalation
patchs_superieur_seuil_maxi <- ifel(taille_patchs < seuil_maxi, NA, patchs)
plot(patchs_superieur_seuil_maxi,type="classes")

##Save the new file
filename = paste("patchs_sup",seuil_maxi, sep="_")
filename = paste(filename,id_paysage, sep="_")
writeRaster(x = patchs_superieur_seuil_maxi, filename = paste(filename,"tif",sep="."),overwrite=TRUE)


#Affichage de l'histogramme de la taille de chacun des patchs : 
taille_patches_m2 = freq(patchs)[3]*cell_size
hist(taille_patches_m2$count)



##### 7) Tessalation sur les patchs trop grands - tirage aléatoire

# Importation de la fonction
source("D:/STAGE MODELISATION/Carto/Automatisation_traitement_carto_R/Fonctions/fct_tessalation.R")

# Définition des paramètres
"r : fichier de patchs à découper, créé précédement, format raster
n = nb de points à tirer aléatoirement. Vérifier avec la variable 'val' décrite après si la
répartition des tailles de patchs est correcte. Si trop de grands patchs, augmenter la valeur
de n ; si trop de patchs de petite taille, diminuer la valeur de n
J'ai balayé une gamme de valeur pour déterminer le n optimal, mais c'est dans le cas de 
mon paysage, je ne sais pas trop à quel point ça diffère"
r=patchs_superieur_seuil_maxi
n = 1100

# Tirage des points aléatoires
points_aleatoire = tirage_pts_aleatoires(r,n)

#Tesselation
x = points_aleatoire[,1]
y = points_aleatoire[,2]
tesselation_aleatoire <- deldir(x, y)


#Définition des nouveaux patchs
new_patchs_aleatoire = decoupage_patchs(tesselation_aleatoire,id_max)
plot(new_patchs_aleatoire)

#Vérification que la taille des patchs correspond bien à ce qui était attendu
val = verif_decoupage(new_patchs_aleatoire)



##### 8) Regroupement des patchs de taille OK et des patchs découpés précédement
mosaic_raster_aleatoire <- mosaic(new_patchs_aleatoire, patchs_taille_ok)
plot(mosaic_raster_aleatoire)

##Save the new file
filename = paste("patchs_decoupes",id_paysage, sep="_")
writeRaster(x = mosaic_raster_aleatoire, filename = paste(filename,"tif",sep="."),overwrite=TRUE)



#############################################
## PARTIE II : DEFINITION DES COUTS DE DEP ##
#############################################

par(mfrow=c(1,1))
##### 1) Ouverture des fichiers

## Importation de l'habitat

habitat = rast("D:/STAGE MODELISATION/COMPARAISON PETITS GRANDS PATCHS/CREATION DES PAYSAGES/patchs_initiaux_C.tif")
habitat[habitat > 0] <- 2
plot(habitat, col = "green")


## Importation des patchs de passage

step_stone = rast("D:/STAGE MODELISATION/COMPARAISON PETITS GRANDS PATCHS/CREATION DES PAYSAGES/patchs_inf_22500_C.tif")
step_stone[step_stone > 0] <- 2
plot(step_stone, col = "green")

## Combinaison de l'habitat et des patchs de passage

habitat_step_stone <- mosaic(habitat, step_stone)
plot(habitat_step_stone, col = "#1A5B10")



## Création du raster des routes
roads[,1] = 1:nrow(data.frame(roads))
names(roads)[1]="id"

roads_raster <- rasterize(roads, habitat*0, field = "id")
roads_raster[roads_raster > 0] <- 4
plot(roads_raster,col="#9A1313")


## Définition des zones de matrice hospitalière
sol = rast("D:/STAGE MODELISATION/Carto/Automatisation_traitement_carto_R/PROCEDURE_APPLIQUEE/forest_density_C.tif")

densite_matrice_hosp = 10


#On ne conserve que les cellules pour lesquelles la densité est supérieure à 10%
matrice_hosp = sol>densite_matrice_hosp 
legende <- c("", "3")
couleurs <- c("white", "#83DE75")

matrice_hosp[matrice_hosp > 0] <- 3
plot(matrice_hosp)


##### 2) Combinaison des différentes couches qui forment le paysage : habitat, route, aide
paysage <- mosaic(matrice_hosp, habitat_step_stone,fun="min")
paysage_complet <- mosaic(paysage, roads_raster,fun="max")
paysage_complet[paysage_complet == 0] <- 1

plot(paysage_complet, 
     type = "classes", 
     levels = c("Matrice", "Habitat","Matrice hospitalière","Route"),
     col = c("#E2DEDE", "#1A5B10","#83DE75","#9A1313"), 
     plg = list(cex = 0.7),
     main = "Paysage C classé"
)

##Save the new file
filename = paste("paysage_complet",id_paysage, sep="_")
writeRaster(x = paysage_complet, filename = paste(filename,"tif",sep="."),overwrite=TRUE)




##### 3) Création du fichier de cout

# On duplique la matrice du paysage pour pouvoir l'utiliser
couts = matrix(paysage_complet,nrow=nrow(paysage_complet))

# On définie les couts que l'on souhaite attribuer à chaque classe
cout_habitat = 1
cout_matrice_hosp = 1
cout_route = 100
cout_matrice = 10


# On attribue les couts (précédment déterminés) à chaque case de la matrice
couts[couts == 1] <- cout_matrice
couts[couts == 2] <- cout_habitat
couts[couts == 3] <- cout_matrice_hosp
couts[couts == 4] <- cout_route
#View(couts)


# On convertit la matrice en raster en prenant la trasnposée!), et on l'enregistre
couts = t(couts)
raster_couts = rast(couts)
plot(raster_couts)


##Save the new file
filename = paste("couts",id_paysage, sep="_")
writeRaster(x = raster_couts, filename = paste(filename,"tif",sep="."),overwrite=TRUE)





#######################################################
## PARTIE III : CREATION DU FICHIER D'INITIALISATION ##
#######################################################


##### 1) Importation des fichiers 

## Import de la carte des patchs 
patchs = rast("D:/STAGE MODELISATION/COMPARAISON PETITS GRANDS PATCHS/CREATION DES PAYSAGES/patchs_initiaux_C.tif")
plot(patchs)
#View(freq(patchs))


## Import de la carte des présences
presence = vect("D:/STAGE MODELISATION/Carto/Automatisation_traitement_carto_R/Inputs/pts_presence_C.shp")
plot(presence, add=TRUE)


##### 2) Ne garder que les points qui sont dans les patchs d'habitat 

patch_presence = mask(patchs,presence)
#View(freq(patch_presence))
patch_presence[patch_presence > 0 ]  <- 1

plot(patch_presence,col="black")
nb_patchs = nrow(data.frame(patch_presence)) #Savoir combien de patchs ont au moins un indiv à t=0

##Save the new file
filename = paste("patchs_presence",id_paysage, sep="_")
writeRaster(x = patch_presence, filename = paste(filename,"tif",sep="."),overwrite=TRUE)




##############################################
## PARTIE IIV : CREATION DES FICHIERS TEXTS ##
##############################################


##### 1) Fichier d'habitat

#Métadonnées fichier texte
Ncols<- ncol(for_den_crop)
Nrows<-  nrow(for_den_crop)
xcorner<- xmin(for_den_crop)
ycorner<- ymin(for_den_crop)
reso<-  resolution
NA_values<- -9999

texte = paste(id_paysage,"txt", sep=".")
tif = paste(id_paysage,"tif", sep=".")

file_name<-paste("habitat",texte, sep="_")
f <- paste("habitat_30",tif, sep="_")
r <- raster(f)
r[is.na(r)] <- -9999
cat( "ncols ",Ncols, "\n","nrows ",Nrows,"\n","xllcorner ",xcorner, "\n",
     "yllcorner ",ycorner,"\n", "cellsize ", reso, "\n", "NODATA_value " ,NA_values ,"\n",
     file=file_name,sep="")

matrix.proj<- matrix(r,ncol=Nrows,nrow=Ncols,byrow=F)
write(matrix.proj, ncolumns=Ncols,file=file_name,append=T)


##### 2) Fichiers de patchs

#Patchs initiaux
file_name<-paste("patchs_initiaux",texte, sep="_")
f <- paste("patchs_initiaux",tif, sep="_")
r <- raster(f)
r[is.na(r)] <- 0
cat( "ncols ",Ncols, "\n","nrows ",Nrows,"\n","xllcorner ",xcorner, "\n",
     "yllcorner ",ycorner,"\n", "cellsize ", reso, "\n", "NODATA_value " ,NA_values ,"\n",
     file=file_name,sep="")

matrix.proj<- matrix(r,ncol=Nrows,nrow=Ncols,byrow=F)
write(matrix.proj, ncolumns=Ncols,file=file_name,append=T)


#Patchs découpés
file_name<-paste("patchs_decoupes",texte, sep="_")
f <- paste("patchs_decoupes",tif, sep="_")
r <- raster(f)
r[is.na(r)] <- 0
cat( "ncols ",Ncols, "\n","nrows ",Nrows,"\n","xllcorner ",xcorner, "\n",
     "yllcorner ",ycorner,"\n", "cellsize ", reso, "\n", "NODATA_value " ,NA_values ,"\n",
     file=file_name,sep="")

matrix.proj<- matrix(r,ncol=Nrows,nrow=Ncols,byrow=F)
write(matrix.proj, ncolumns=Ncols,file=file_name,append=T)


##### 3) Paysage 

#Métadonnées fichier texte
Ncols<- ncol(paysage_complet)
Nrows<-  nrow(paysage_complet)
xcorner<- xmin(paysage_complet)
ycorner<- ymin(paysage_complet)
reso<-  res(paysage_complet)[1]
NA_values<- -9999

file_name<-paste("paysage",texte, sep="_")
f <- paste("paysage_complet",tif, sep="_")
r <- raster(f)
r[is.na(r)] <- -9999
cat( "ncols ",Ncols, "\n","nrows ",Nrows,"\n","xllcorner ",xcorner, "\n",
     "yllcorner ",ycorner,"\n", "cellsize ", reso, "\n", "NODATA_value " ,NA_values ,"\n",
     file=file_name,sep="")

matrix.proj<- matrix(r,ncol=Nrows,nrow=Ncols,byrow=F)
write(matrix.proj, ncolumns=Ncols,file=file_name,append=T)


##### 4) Couts de déplacement

file_name<-paste("couts",texte, sep="_")
f <- paste("couts",tif, sep="_")
r <- raster(f)
r[is.na(r)] <- -9999
cat( "ncols ",Ncols, "\n","nrows ",Nrows,"\n","xllcorner ",xcorner, "\n",
     "yllcorner ",ycorner,"\n", "cellsize ", reso, "\n", "NODATA_value " ,NA_values ,"\n",
     file=file_name,sep="")

matrix.proj<- matrix(r,ncol=Nrows,nrow=Ncols,byrow=F)
write(matrix.proj, ncolumns=Ncols,file=file_name,append=T)


##### 5) Patchs de présence

Ncols<- ncol(patch_presence)
Nrows<-  nrow(patch_presence)
xcorner<- xmin(patch_presence)
ycorner<- ymin(patch_presence)
reso<-  res(patch_presence)[1]
NA_values<- -9999


file_name<-paste("presence",texte, sep="_")
f <- paste("patchs_presence",tif, sep="_")
r <- raster(f)
r[is.na(r)] <- -9999
cat( "ncols ",Ncols, "\n","nrows ",Nrows,"\n","xllcorner ",xcorner, "\n",
     "yllcorner ",ycorner,"\n", "cellsize ", reso, "\n", "NODATA_value " ,NA_values ,"\n",
     file=file_name,sep="")

matrix.proj<- matrix(r,ncol=Nrows,nrow=Ncols,byrow=F)
write(matrix.proj, ncolumns=Ncols,file=file_name,append=T)

#Evaluation du temps de lancement du script
endTime <- Sys.time()

# prints recorded time
print(endTime - startTime)