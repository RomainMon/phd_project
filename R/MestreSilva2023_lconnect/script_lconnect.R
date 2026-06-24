#load package from CRAN
install.packages("lconnect")

#Load the package
library(lconnect)

#Setting the working directory
getwd()
setwd("C:\\Users\\FMest\\Desktop")

#Load the landscape data
vec_path <- system.file("extdata/vec_projected.shp", package = "lconnect")
land <- upload_land(vec_path, habitat = 1, max_dist = 500)

#It is an object of class "lconnect"
class(land)

#The user can plot it
plot(land, main="Landscape clusters")

#Compute connectivity metrics
metrics <- con_metric(land, metric = c( "NC", "LNK", "SLC", "MSC", "CCP", "LCP", "CPL", "ECS", "AWF", "IIC"))
metrics <- as.data.frame(metrics)

#Computing patch importance based on IIC  
land1 <- patch_imp(land, metric="IIC")
  
#Generates an object of class
class(land1)
  
#Which the user can plot
plot(land1, main="Patch Importance - IIC")

#The patch importance output is based upon the “sf” class
class(land$landscape)
class(land1$landscape)

#As such, to save these outputs as shapefiles, the user can 
#use the functions of the sf package

library(sf)

st_write(land$landscape,  "land.shp")
st_write(land1$landscape, "land1.shp")
