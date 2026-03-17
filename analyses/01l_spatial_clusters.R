#------------------------------------------------#
# Author: Romain Monassier
# Objective: Delimiting spatial clusters
#------------------------------------------------#

# Source: https://www.paulamoraga.com/book-spatial/spatial-autocorrelation.html

### Load packages
library(dplyr)
library(spdep)
library(sf)
library(here)
library(ggplot2)
library(terra)
library(tmap)

### Load datasets -------------
raster_tm_2024 = terra::rast(here("outputs", "data", "MapBiomas", "Rasters_cumulative_tm", "raster_reclass_cumul_tm_2024.tif"))
data_car = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.rds"))
plot(raster_tm_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e", "chartreuse", "pink"))

### Prepare dataset -------------
defor = raster_tm_2024 == 8
refor = raster_tm_2024 == 7

### Deforestation ----
#### Compute local proportions -----
# Grid
bbox = st_as_sfc(st_bbox(raster_tm_2024))
grid = st_make_grid(bbox, cellsize = 5000, square = TRUE) %>% 
  st_as_sf()
st_crs(grid) = st_crs(raster_tm_2024)
plot(defor)
plot(grid, add=TRUE)

# Compute proportions in the grid
extract_defor = terra::extract(defor, vect(grid), fun = mean, na.rm = TRUE)
grid$prop_defor = extract_defor[,2]
plot(grid)

# Remove NAs
grid = grid %>% 
  dplyr::filter(!is.na(prop_defor))

#### Neighbor matrix  -------------
nb_defor = poly2nb(grid, queen = TRUE) # Queen contiguity
# Plot the grid and neighbors
plot.nb(nb_defor, coords = sf::st_geometry(grid))

#### Weigh matrix  -------------
# Computes spatial weighs for neighbours list
# Argument style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
# B is the basic binary coding
# W is row standardised (sums over all links to n)
# C is globally standardised (sums over all links to n)
# U is equal to C divided by the number of neighbours (sums over all links to unity)
# S is the variance-stabilizing coding scheme
lw_defor = nb2listw(nb_defor, style = "W", zero.policy = TRUE)

#### Compute Moran's I -----
# The localmoran() function of the spdep package can be used to compute the Local Moran’s I for a given dataset. The arguments of localmoran() include a numeric vector with the values of the variable, a list with the neighbor weights, and the name of an alternative hypothesis that can be set equal to greater (default), less or two.sided
# Ii: Local Moran’s I statistic for each area
# Var.Ii: Variance Local Moran’s I
# E.Ii: Expectation Local Moran’s I
# Z.Ii: z-score
#  Pr(z > E(Ii)), Pr(z < E(Ii)) or Pr(z != E(Ii)): p-value for an alternative hypothesis greater, less or two.sided, respectively.

# alternative = "greater" which corresponds to testing H0: no or negative spatial autocorrelation vs. H1: positive spatial autocorrelation
# alternative = "two.sided" corresponds to H0: no spatial autocorrelation vs. H1: positive or negative spatial autocorrelation
# In this two-sided test, z-score values lower than –1.96 indicate negative spatial autocorrelation, and z-score values greater than 1.96 indicate positive spatial autocorrelation
lmoran = spdep::localmoran(grid$prop_defor, lw_defor, alternative="two.sided")
head(lmoran)

# Extract values
grid$lmI = lmoran[, "Ii"] # local Moran's I
grid$lmZ = lmoran[, "Z.Ii"] # z-scores
grid$lmp = lmoran[, "Pr(z != E(Ii))"] # p-values

# Local Moran’s I allows us to identify clusters of the following types:
# High-High: areas of high values with neighbors of high values,
# High-Low: areas of high values with neighbors of low values,
# Low-High: areas of low values with neighbors of high values,
# Low-Low: areas of low values with neighbors of low values.

# Then, we identify the clusters of each type by using the information provided by the Moran’s I scatterplot obtained with the moran.plot() function
mp = moran.plot(as.vector(scale(grid$prop_defor)), lw_defor)
head(mp)
# Specifically, we identify the cluster types by using the quadrants of the scaled values (mp$x) and their spatially lagged values (mp$wx), and the p-values obtained with the local Moran’s I for each of the areas
# The classification of the clusters is as follows. Areas with significant local Moran’s I are classified as high-high if both the value and its corresponding spatially lagged value are positive, low-low if both the value and its spatially lagged value are negative, high-low if the the value is positive and the spatially lagged value negative, and low-high is the value is negative and the spatially lagged value positive.
# We create the variable quadrant denoting the type of cluster for each of the areas using the quadrant corresponding to its value and its spatially lagged value, and the p-value. Specifically, areas with quadrant equal to 1, 2, 3, and 4 correspond to clusters of type high-high, low-low, high-low, and low-high, respectively. Areas with quadrant equal to 5 are non-significant.
grid$quadrant = NA
# high-high
grid[(mp$x >= 0 & mp$wx >= 0) & (grid$lmp <= 0.05), "quadrant"]= 1
# low-low
grid[(mp$x <= 0 & mp$wx <= 0) & (grid$lmp <= 0.05), "quadrant"]= 2
# high-low
grid[(mp$x >= 0 & mp$wx <= 0) & (grid$lmp <= 0.05), "quadrant"]= 3
# low-high
grid[(mp$x <= 0 & mp$wx >= 0) & (grid$lmp <= 0.05), "quadrant"]= 4
# non-significant
grid[(grid$lmp > 0.05), "quadrant"] = 5

## Plot
unique(grid$quadrant)
# Make quadrant a factor with labels
grid$quadrant = factor(grid$quadrant,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("High-High", "Low-Low", "High-Low", "Low-High", "Non-significant"))

# Colors
cols = c("red", "blue", "lightpink", "skyblue2", "white")

# Plot
ggplot(grid) +
  geom_sf(aes(fill = quadrant),
          color = "black",
          size = 0.2,
          alpha = 0.8) +
  scale_fill_manual(values = cols,
                    name = NULL) +
  labs(title = "Clusters") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

### Map
hotspots = grid %>% dplyr::filter(quadrant == "High-High")
hotspots = sf::st_crop(hotspots, st_bbox(raster_tm_2024))
# Raster to dataframe
defor_df = as.data.frame(raster_tm_2024, xy = TRUE)
# Convert to factor with labels
defor_df$value = factor(defor_df$`2024`,
                         levels = 1:8,
                         labels = c("Forest",
                                    "Non-forest formation",
                                    "Wetland",
                                    "Agriculture",
                                    "Water",
                                    "Built-up",
                                    "Reforested",
                                    "Deforested"))
cols = c("#32a65e", "#ad975a", "#519799", "#FFFFB2",
          "#0000FF", "#d4271e", "chartreuse", "pink")

# Plot
ggplot() +
  # Raster background
  geom_raster(data = defor_df,
              aes(x = x, y = y, fill = value)) +
  scale_fill_manual(name = "Land use",
                    values = cols) +
  # Hotspots overlay (only High-High)
  geom_sf(data = hotspots,
          fill = "red",
          color = "black",
          alpha = 0.5,
          size = 0.2) +
  labs(title = "a) Deforestation hotspots") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )


### Reforestation ----
#### Compute local proportions -----
# Grid
bbox = st_as_sfc(st_bbox(raster_tm_2024))
grid = st_make_grid(bbox, cellsize = 5000, square = TRUE) %>% 
  st_as_sf()
st_crs(grid) = st_crs(raster_tm_2024)
plot(refor)
plot(grid, add=TRUE)

# Compute proportions in the grid
extract_refor = terra::extract(refor, vect(grid), fun = mean, na.rm = TRUE)
grid$prop_refor = extract_refor[,2]
plot(grid)

# Remove NAs
grid = grid %>% 
  dplyr::filter(!is.na(prop_refor))

#### Neighbor matrix  -------------
nb_refor = poly2nb(grid, queen = TRUE) # Queen contiguity
# Plot the grid and neighbors
plot.nb(nb_refor, coords = sf::st_geometry(grid))

#### Weigh matrix  -------------
# Computes spatial weighs for neighbours list
# Argument style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
# B is the basic binary coding
# W is row standardised (sums over all links to n)
# C is globally standardised (sums over all links to n)
# U is equal to C divided by the number of neighbours (sums over all links to unity)
# S is the variance-stabilizing coding scheme
lw_refor = nb2listw(nb_refor, style = "W", zero.policy = TRUE)

#### Compute Moran's I -----
# The localmoran() function of the spdep package can be used to compute the Local Moran’s I for a given dataset. The arguments of localmoran() include a numeric vector with the values of the variable, a list with the neighbor weights, and the name of an alternative hypothesis that can be set equal to greater (default), less or two.sided
# Ii: Local Moran’s I statistic for each area
# Var.Ii: Variance Local Moran’s I
# E.Ii: Expectation Local Moran’s I
# Z.Ii: z-score
#  Pr(z > E(Ii)), Pr(z < E(Ii)) or Pr(z != E(Ii)): p-value for an alternative hypothesis greater, less or two.sided, respectively.

# alternative = "greater" which corresponds to testing H0: no or negative spatial autocorrelation vs. H1: positive spatial autocorrelation
# alternative = "two.sided" corresponds to H0: no spatial autocorrelation vs. H1: positive or negative spatial autocorrelation
# In this two-sided test, z-score values lower than –1.96 indicate negative spatial autocorrelation, and z-score values greater than 1.96 indicate positive spatial autocorrelation
lmoran = spdep::localmoran(grid$prop_refor, lw_refor, alternative="two.sided")
head(lmoran)

# Extract values
grid$lmI = lmoran[, "Ii"] # local Moran's I
grid$lmZ = lmoran[, "Z.Ii"] # z-scores
grid$lmp = lmoran[, "Pr(z != E(Ii))"] # p-values

# Local Moran’s I allows us to identify clusters of the following types:
# High-High: areas of high values with neighbors of high values,
# High-Low: areas of high values with neighbors of low values,
# Low-High: areas of low values with neighbors of high values,
# Low-Low: areas of low values with neighbors of low values.

# Then, we identify the clusters of each type by using the information provided by the Moran’s I scatterplot obtained with the moran.plot() function
mp = moran.plot(as.vector(scale(grid$prop_refor)), lw_refor)
head(mp)
# Specifically, we identify the cluster types by using the quadrants of the scaled values (mp$x) and their spatially lagged values (mp$wx), and the p-values obtained with the local Moran’s I for each of the areas
# The classification of the clusters is as follows. Areas with significant local Moran’s I are classified as high-high if both the value and its corresponding spatially lagged value are positive, low-low if both the value and its spatially lagged value are negative, high-low if the the value is positive and the spatially lagged value negative, and low-high is the value is negative and the spatially lagged value positive.
# We create the variable quadrant denoting the type of cluster for each of the areas using the quadrant corresponding to its value and its spatially lagged value, and the p-value. Specifically, areas with quadrant equal to 1, 2, 3, and 4 correspond to clusters of type high-high, low-low, high-low, and low-high, respectively. Areas with quadrant equal to 5 are non-significant.
grid$quadrant = NA
# high-high
grid[(mp$x >= 0 & mp$wx >= 0) & (grid$lmp <= 0.05), "quadrant"]= 1
# low-low
grid[(mp$x <= 0 & mp$wx <= 0) & (grid$lmp <= 0.05), "quadrant"]= 2
# high-low
grid[(mp$x >= 0 & mp$wx <= 0) & (grid$lmp <= 0.05), "quadrant"]= 3
# low-high
grid[(mp$x <= 0 & mp$wx >= 0) & (grid$lmp <= 0.05), "quadrant"]= 4
# non-significant
grid[(grid$lmp > 0.05), "quadrant"] = 5

## Plot
unique(grid$quadrant)
# Make quadrant a factor with labels
grid$quadrant = factor(grid$quadrant,
                       levels = c(1, 2, 4, 5),
                       labels = c("High-High", "Low-Low", "Low-High", "Non-significant"))

# Colors
cols = c("red", "blue", "skyblue2", "white")

# Plot
ggplot(grid) +
  geom_sf(aes(fill = quadrant),
          color = "black",
          size = 0.2,
          alpha = 0.8) +
  scale_fill_manual(values = cols,
                    name = NULL) +
  labs(title = "Clusters") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

### Map
hotspots = grid %>% dplyr::filter(quadrant == "High-High")
hotspots = sf::st_crop(hotspots, st_bbox(raster_tm_2024))
# Raster to dataframe
refor_df = as.data.frame(raster_tm_2024, xy = TRUE)
# Convert to factor with labels
refor_df$value = factor(refor_df$`2024`,
                        levels = 1:8,
                        labels = c("Forest",
                                   "Non-forest formation",
                                   "Wetland",
                                   "Agriculture",
                                   "Water",
                                   "Built-up",
                                   "Reforested",
                                   "Deforested"))
cols = c("#32a65e", "#ad975a", "#519799", "#FFFFB2",
         "#0000FF", "#d4271e", "chartreuse", "pink")

# Plot
ggplot() +
  # Raster background
  geom_raster(data = refor_df,
              aes(x = x, y = y, fill = value)) +
  scale_fill_manual(name = "Land use",
                    values = cols) +
  # Hotspots overlay (only High-High)
  geom_sf(data = hotspots,
          fill = "red",
          color = "black",
          alpha = 0.5,
          size = 0.2) +
  labs(title = "b) Reforestation hotspots") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )
