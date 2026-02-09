#------------------------------------------------#
# Author: Romain Monassier
# Objective: LULC modeling
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(ggplot2)
library(car)
library(HH)

### Load datasets
data_defor_pixel = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_pixel.rds"))
data_refor_pixel = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_refor_pixel.rds"))
data_car = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.rds"))


### Pixel-scale analysis -------------

#### Summary ----------

# Data structure
str(data_defor_pixel)
str(data_refor_pixel)

##### NAs --------
# Number of NAs
na = data_defor_pixel %>% 
  dplyr::summarise(across(everything(), ~ sum(is.na(.))))
na = data_refor_pixel %>% 
  dplyr::summarise(across(everything(), ~ sum(is.na(.))))

# Filter NAs
data_defor_subset = data_defor_pixel %>% 
  tidyr::drop_na(slope_pct)
data_refor_subset = data_refor_pixel %>% 
  tidyr::drop_na(slope_pct)

##### Number of data ------
# Number of events
data_defor_subset %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n()) %>% dplyr::mutate(prop = n*100/sum(n))
data_refor_subset %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n()) %>% dplyr::mutate(prop = n*100/sum(n))
cat("Number of pixels in the deforestation dataset:", length(data_defor_subset$cell_id), "\n")
cat("Number of pixels in the reforestation dataset:", length(data_refor_subset$cell_id), "\n")

# Number of unique cells
message("Number of unique cells (deforestation):")
data_defor_subset %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n_distinct(cell_id))

message("Number of unique cells (reforestation):")
data_refor_subset %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n_distinct(cell_id))

# Number of events per year
res_defor = data_defor_subset %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>% 
  dplyr::mutate(prop = n * 100 / sum(n)) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(cumprop = cumsum(prop)) %>% 
  print(n=35)

cat(
  "\nDeforestation — 3 years with the most events:\n",
  paste(
    paste0(
      res_defor$year[1:3], 
      ": ", 
      res_defor$n[1:3], 
      " events (", 
      round(res_defor$prop[1:3], 1), 
      "%, cumulative = ",
      round(res_defor$cumprop[1:3], 1),
      "%)"
    ),
    collapse = "\n"
  ),
  "\n"
)

res_refor = data_refor_subset %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>% 
  dplyr::mutate(prop = n * 100 / sum(n)) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(cumprop = cumsum(prop)) %>% 
  print(n=35)
cat(
  "\nReforestation — 3 years with the most events:\n",
  paste(
    paste0(
      res_refor$year[1:3], 
      ": ", 
      res_refor$n[1:3], 
      " events (", 
      round(res_refor$prop[1:3], 1), 
      "%, cumulative = ",
      round(res_refor$cumprop[1:3], 1),
      "%)"
    ),
    collapse = "\n"
  ),
  "\n"
)

##### Qualitative variables --------

###### Legal status -----
table(data_defor_subset$type, data_defor_subset$legal_status)
table(data_refor_subset$type, data_refor_subset$legal_status)

prop.table(table(data_defor_subset$type, data_defor_subset$legal_status), margin = 1) # By type
prop.table(table(data_refor_subset$type, data_refor_subset$legal_status), margin = 1) # By type
prop.table(table(data_defor_subset$type, data_defor_subset$legal_status), margin = 2) # By legal status
prop.table(table(data_refor_subset$type, data_refor_subset$legal_status), margin = 2) # By legal status

###### BR 101 -----
table(data_defor_subset$type, data_defor_subset$ns_br101)
table(data_refor_subset$type, data_refor_subset$ns_br101)

prop.table(table(data_defor_subset$type, data_defor_subset$ns_br101), margin = 1)
prop.table(table(data_refor_subset$type, data_refor_subset$ns_br101), margin = 1)
prop.table(table(data_defor_subset$type, data_defor_subset$ns_br101), margin = 2)
prop.table(table(data_refor_subset$type, data_refor_subset$ns_br101), margin = 2)

###### APA MLD -----
table(data_defor_subset$type, data_defor_subset$in_apa)
table(data_refor_subset$type, data_refor_subset$in_apa)

###### CAR -----
table(data_defor_subset$type, data_defor_subset$in_car)
table(data_refor_subset$type, data_refor_subset$in_car)

###### Public reserves -----
table(data_defor_subset$type, data_defor_subset$in_pub_res)
table(data_refor_subset$type, data_refor_subset$in_pub_res)

###### RPPNs -----
table(data_defor_subset$type, data_defor_subset$in_rppn)
table(data_refor_subset$type, data_refor_subset$in_rppn)
# Beware: few pixels intersect RPPNs

###### Legal Reserves -----
table(data_defor_subset$type, data_defor_subset$in_rl)
table(data_refor_subset$type, data_refor_subset$in_rl)

#### Quantitative variables ------
##### Distances -----
data_defor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("dist"), list(mean = mean)))
data_refor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("dist"), list(mean = mean)))

hist(data_defor_subset$dist_river_m)
hist(data_defor_subset$dist_urban_m)
hist(data_defor_subset$dist_road_m)
hist(data_defor_subset$dist_edge_m)

hist(data_refor_subset$dist_river_m)
hist(data_refor_subset$dist_urban_m)
hist(data_refor_subset$dist_road_m)
hist(data_refor_subset$dist_edge_m)

# Log transformation
data_defor_subset = data_defor_subset %>% 
  dplyr::mutate(dist_river_log = log10(dist_river_m+1),
                dist_road_log = log10(dist_road_m+1),
                dist_urban_log = log10(dist_urban_m+1),
                dist_edge_log = log10(dist_edge_m+1))

data_refor_subset = data_refor_subset %>% 
  dplyr::mutate(dist_river_log = log10(dist_river_m+1),
                dist_road_log = log10(dist_road_m+1),
                dist_urban_log = log10(dist_urban_m+1),
                dist_edge_log = log10(dist_edge_m+1))

hist(data_defor_subset$dist_river_log)
hist(data_defor_subset$dist_urban_log)
hist(data_defor_subset$dist_road_log)
hist(data_defor_subset$dist_edge_log)

hist(data_refor_subset$dist_river_log)
hist(data_refor_subset$dist_urban_log)
hist(data_refor_subset$dist_road_log)
hist(data_refor_subset$dist_edge_log)

##### Slope -------
data_defor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean_slope=mean(slope_pct))
data_refor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean_slope=mean(slope_pct))
hist(data_defor_subset$slope_pct)
hist(data_refor_subset$slope_pct)

# Log transformation
data_defor_subset = data_defor_subset %>%
  dplyr::mutate(slope_pct_log = log10(slope_pct))
data_refor_subset = data_refor_subset %>%
  dplyr::mutate(slope_pct_log = log10(slope_pct))
hist(data_defor_subset$slope_pct_log)
hist(data_refor_subset$slope_pct_log)

##### Land use -----
data_defor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("area"), list(mean = mean)))
data_refor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("area"), list(mean = mean)))

hist(data_defor_subset$area_m2_class_1)
hist(data_defor_subset$area_m2_class_4)
hist(data_defor_subset$area_m2_class_6)
hist(log10(data_defor_subset$area_m2_class_6))

hist(data_refor_subset$area_m2_class_1)
hist(data_refor_subset$area_m2_class_4)
hist(data_refor_subset$area_m2_class_6)
hist(log10(data_refor_subset$area_m2_class_6))

# Log transformation
data_defor_subset = data_defor_subset %>% 
  dplyr::mutate(area_class_6_log = log10(area_m2_class_6+1))
data_refor_subset = data_refor_subset %>% 
  dplyr::mutate(area_class_6_log = log10(area_m2_class_6+1))

##### Forest age -------
data_defor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(forest_age))
hist(data_defor_subset$forest_age)

##### Precipitations -------
data_defor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(prec_sum))
data_refor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(prec_sum))
hist(data_defor_subset$prec_sum)
hist(data_refor_subset$prec_sum)


##### Tmin -------
data_defor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(tmin_mean))
data_refor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(tmin_mean))
hist(data_defor_subset$tmin_mean)
hist(data_refor_subset$tmin_mean)

# Log transformation
data_defor_subset = data_defor_subset %>% 
  dplyr::mutate(tmin_mean_log = log10(tmin_mean))
data_refor_subset = data_refor_subset %>% 
  dplyr::mutate(tmin_mean_log = log10(tmin_mean))
hist(data_defor_subset$tmin_mean_log)
hist(data_refor_subset$tmin_mean_log)

##### Tmax -------
data_defor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(tmax_mean))
data_refor_subset %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(tmax_mean))
hist(data_defor_subset$tmax_mean)
hist(data_refor_subset$tmax_mean)

# Log transformation
data_defor_subset = data_defor_subset %>% 
  dplyr::mutate(tmax_mean_log = log10(tmax_mean))
data_refor_subset = data_refor_subset %>% 
  dplyr::mutate(tmax_mean_log = log10(tmax_mean))
hist(data_defor_subset$tmax_mean_log)
hist(data_refor_subset$tmax_mean_log)

#### Test correlations among variables ---------
##### Pearson =====

X = data_defor_subset %>% 
  dplyr::select(in_car, in_pub_res, in_rppn, in_rl, in_apa,
        area_m2_class_1, prop_class_1, area_m2_class_4, prop_class_4, area_class_6_log, prop_class_6,
        dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
        slope_pct_log, prec_sum, tmin_mean_log, tmax_mean_log,
        forest_age)
cor_mat = cor(X, use = "pairwise.complete.obs", method = "pearson")
cor_mat

# Identify strong correlations
high_corr = cor_mat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(-var1,names_to = "var2",values_to = "r") %>%
  dplyr::filter(var1 < var2, abs(r) > 0.6) %>%
  dplyr::arrange(desc(abs(r)))

high_corr
# Beware: the area of forest and of agriculture are strongly correlated

##### VIF ---------
X_vif = data_defor_subset %>% 
  dplyr::select(type, in_car, in_pub_res, in_rppn, in_rl, in_apa,
                area_m2_class_1, prop_class_1, area_m2_class_4, prop_class_4, area_class_6_log, prop_class_6,
                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                slope_pct_log, prec_sum, tmin_mean_log, tmax_mean_log,
                forest_age) %>% 
  as.data.frame()
vif.result = vif(X_vif, y.name="type")

# VIF(i) = 1/(1-Ri²) (where Ri² is the R² gotten from the regression of predictor i regarding other predictors)
# If Ri² is close to 1, it means that the variable i is well explained by the linear combination of other variables ; hence the variable i is redundant in the model 
# The more Ri² is close to 1, the more VIF increases (+Inf)
# Thus, the higher the VIF, the stronger the collinearity of i is with other variables (ie the information of i is already contained by others)
# Check VIF > 2.5
vif.result[vif.result > 2.5]
# The results confirm strong correlations between the amount of forest and of agriculture
# There is also a strong correlation between tmin and tmax

##### Variable selection ----
# We select variables based on their correlations with the response variable
...


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
