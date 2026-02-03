#------------------------------------------------#
# Author: Romain Monassier
# Objective: LULC modeling
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(ggplot2)
library(car)

### Load datasets
data_defor_pixel <- readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_pixel.rds"))
data_refor_pixel <- readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_refor_pixel.rds"))
data_car <- readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.rds"))


### 1. Deforestation (pixel-scale) -------------

#### Summary ----------

## Deforestation events

# Data structure
str(data_defor_pixel)

# Number of NAs
data_defor_pixel %>% 
  dplyr::summarise(across(everything(), ~ sum(is.na(.))))

# Filter NAs
data_defor_pixel = data_defor_pixel %>% 
  tidyr::drop_na(slope_pct)

# Number of distinct cells
data_defor_pixel %>% dplyr::summarise(n=dplyr::n_distinct(cell_id))

# Number of events per category
data_defor_pixel %>% 
  dplyr::group_by(change_type) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))

# Number of events per year
data_defor_pixel %>% 
  dplyr::group_by(change_year) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n)) %>% 
  dplyr::arrange(n) %>% 
  print(n=34)

# Number of events per legal status
data_defor_pixel %>% 
  dplyr::group_by(change_type, legal_status) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))

# Number of events in or outside APA
data_defor_pixel %>% 
  dplyr::group_by(change_type, in_APA) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))

# Mean distance
data_defor_pixel %>% 
  dplyr::group_by(change_type) %>% 
  dplyr::summarise(across(starts_with("dist"), list(mean = mean)))

# Mean slope
data_defor_pixel %>% 
  tidyr::drop_na(slope_pct) %>% 
  dplyr::group_by(change_type) %>% 
  dplyr::summarise(mean_slope=mean(slope_pct))

# Mean land use
data_defor_pixel %>% 
  dplyr::group_by(change_type) %>% 
  dplyr::summarise(across(starts_with("area"), list(mean = mean)))

#### VIF ---------


#### Extract 20% of data -------
# Split the dataset by change_type
deforest_df <- data_defor_pixel %>% dplyr::filter(change_type == "deforest")
control_df <- data_defor_pixel %>% dplyr::filter(change_type == "control")

# Compute sample sizes
n_deforest <- nrow(deforest_df)
n_control <- nrow(control_df)

sample_def <- sample_frac(deforest_df, 0.10)   # 10% deforest
sample_ctl <- sample_frac(control_df, 0.10)     # 10% control

# Bind the sampled data (20% of full dataset)
data_20pct <- dplyr::bind_rows(sample_def, sample_ctl)

# Create the remaining 80% dataset
data_80pct <- dplyr::anti_join(data_defor_pixel, data_20pct, by = colnames(data_defor_pixel))

## Check
# Number of events per category
data_20pct %>% 
  dplyr::group_by(change_type) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))
data_80pct %>% 
  dplyr::group_by(change_type) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))

#### GLMM -------
