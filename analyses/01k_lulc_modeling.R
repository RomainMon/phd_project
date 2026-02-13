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
library(gtsummary)
library(corrr)
library(lme4)
library(lattice)
library(VSURF)
library(Boruta)

library(parallel)
detectCores() # nombre de coeurs physiques

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
sum(is.na(data_defor_pixel))
sum(is.na(data_refor_pixel))
na = data_car %>% 
  dplyr::summarise(across(everything(), ~ sum(is.na(.))))

##### Number of data ------
# Number of events
data_defor_pixel %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n()) %>% dplyr::mutate(prop = n*100/sum(n))
data_refor_pixel %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n()) %>% dplyr::mutate(prop = n*100/sum(n))
cat("Number of pixels in the deforestation dataset:", length(data_defor_pixel$cell_id), "\n")
cat("Number of pixels in the reforestation dataset:", length(data_refor_pixel$cell_id), "\n")

# Number of unique cells
message("Number of unique cells (deforestation):")
data_defor_pixel %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n_distinct(cell_id))

message("Number of unique cells (reforestation):")
data_refor_pixel %>% dplyr::group_by(type) %>% dplyr::summarise(n=dplyr::n_distinct(cell_id))

# Number of events per year
res_defor = data_defor_pixel %>% 
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

res_refor = data_refor_pixel %>% 
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
table(data_defor_pixel$type, data_defor_pixel$legal_status)
table(data_refor_pixel$type, data_refor_pixel$legal_status)

prop.table(table(data_defor_pixel$type, data_defor_pixel$legal_status), margin = 1) # By type
prop.table(table(data_refor_pixel$type, data_refor_pixel$legal_status), margin = 1) # By type
prop.table(table(data_defor_pixel$type, data_defor_pixel$legal_status), margin = 2) # By legal status
prop.table(table(data_refor_pixel$type, data_refor_pixel$legal_status), margin = 2) # By legal status

data_defor_pixel %>% 
  gtsummary::tbl_cross(row = legal_status, col = type,
                       percent = "column",
                       margin = "row"
                       )
data_refor_pixel %>% 
  gtsummary::tbl_cross(row = legal_status, col = type,
                       percent = "column",
                       margin = "row")

###### BR 101 -----
table(data_defor_pixel$type, data_defor_pixel$ns_br101)
table(data_refor_pixel$type, data_refor_pixel$ns_br101)

prop.table(table(data_defor_pixel$type, data_defor_pixel$ns_br101), margin = 1)
prop.table(table(data_refor_pixel$type, data_refor_pixel$ns_br101), margin = 1)
prop.table(table(data_defor_pixel$type, data_defor_pixel$ns_br101), margin = 2)
prop.table(table(data_refor_pixel$type, data_refor_pixel$ns_br101), margin = 2)

data_defor_pixel %>% 
  gtsummary::tbl_cross(row = ns_br101, col = type,
                       percent = "column",
                       margin = "row")
data_refor_pixel %>% 
  gtsummary::tbl_cross(row = ns_br101, col = type,
                       percent = "column",
                       margin = "row")


###### APA MLD -----
table(data_defor_pixel$type, data_defor_pixel$in_apa)
table(data_refor_pixel$type, data_refor_pixel$in_apa)

data_defor_pixel %>% 
  gtsummary::tbl_cross(row = in_apa, col = type,
                       percent = "column",
                       margin = "row")
data_refor_pixel %>% 
  gtsummary::tbl_cross(row = in_apa, col = type,
                       percent = "column",
                       margin = "row")

###### CAR -----
table(data_defor_pixel$type, data_defor_pixel$in_car)
table(data_refor_pixel$type, data_refor_pixel$in_car)

data_defor_pixel %>% 
  gtsummary::tbl_cross(row = in_car, col = type,
                       percent = "column",
                       margin = "row")
data_refor_pixel %>% 
  gtsummary::tbl_cross(row = in_car, col = type,
                       percent = "column",
                       margin = "row")

###### Public reserves -----
table(data_defor_pixel$type, data_defor_pixel$in_pub_res)
table(data_refor_pixel$type, data_refor_pixel$in_pub_res)

data_defor_pixel %>% 
  gtsummary::tbl_cross(row = in_pub_res, col = type,
                       percent = "column",
                       margin = "row")
data_refor_pixel %>% 
  gtsummary::tbl_cross(row = in_pub_res, col = type,
                       percent = "column",
                       margin = "row")

###### RPPNs -----
table(data_defor_pixel$type, data_defor_pixel$in_rppn)
table(data_refor_pixel$type, data_refor_pixel$in_rppn)

data_defor_pixel %>% 
  gtsummary::tbl_cross(row = in_rppn, col = type,
                       percent = "column",
                       margin = "row")
data_refor_pixel %>% 
  gtsummary::tbl_cross(row = in_rppn, col = type,
                       percent = "column",
                       margin = "row")

# Beware: few pixels intersect RPPNs

###### Legal Reserves -----
table(data_defor_pixel$type, data_defor_pixel$in_rl)
table(data_refor_pixel$type, data_refor_pixel$in_rl)

data_defor_pixel %>% 
  gtsummary::tbl_cross(row = in_rl, col = type,
                       percent = "column",
                       margin = "row")
data_refor_pixel %>% 
  gtsummary::tbl_cross(row = in_rl, col = type,
                       percent = "column",
                       margin = "row")

#### Quantitative variables ------
##### Cleveland dotplot ------
# This allows the detection of outliers
## Deforestation dataset
# Z = cbind(data_defor_pixel$dist_river_m, data_defor_pixel$dist_urban_m, data_defor_pixel$dist_road_m, data_defor_pixel$dist_edge_m,
#         data_defor_pixel$area_m2_r100_class_1, data_defor_pixel$area_m2_r100_class_4, data_defor_pixel$area_m2_r100_class_6,
#         data_defor_pixel$prec_sum, data_defor_pixel$tmin_mean, data_defor_pixel$tmax_mean, data_defor_pixel$slope_pct,
#         data_defor_pixel$forest_age)
# 
# colnames(Z) = c("dist_river", "dist_urban", "dist_road", "dist_edge",
#                  "area_forest", "area_agri", "area_urban",
#                  "prec", "tmin", "tmax", "slope",
#                  "forest_age")
# 
# dotplot(as.matrix(Z), groups = FALSE,
#         strip = strip.custom(bg = 'white',
#                              par.strip.text = list(cex = 0.8)),
#         scales = list(x = list(relation = "free"),
#                       y = list(relation = "free"),
#                       draw = FALSE),
#         col = 1, cex  = 0.5, pch = 16,
#         xlab = "Value of the variable",
#         ylab = "Order of the data from text file")

##### Distances -----
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("dist"), list(mean = mean)))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("dist"), list(mean = mean)))

data_defor_pixel %>% 
  tbl_summary(by = type, include = c(dist_river_m, dist_urban_m, dist_road_m, dist_edge_m),
              statistic = list(all_continuous() ~ "{mean} ({sd})"))
data_refor_pixel %>% 
  tbl_summary(by = type, include = c(dist_river_m, dist_urban_m, dist_road_m, dist_edge_m),
              statistic = list(all_continuous() ~ "{mean} ({sd})"))
  
hist(data_defor_pixel$dist_river_m)
hist(data_defor_pixel$dist_urban_m)
hist(data_defor_pixel$dist_road_m)
hist(data_defor_pixel$dist_edge_m)

hist(data_refor_pixel$dist_river_m)
hist(data_refor_pixel$dist_urban_m)
hist(data_refor_pixel$dist_road_m)
hist(data_refor_pixel$dist_edge_m)

# Log transformation
data_defor_pixel = data_defor_pixel %>% 
  dplyr::mutate(dist_river_log = log10(dist_river_m+1),
                dist_road_log = log10(dist_road_m+1),
                dist_urban_log = log10(dist_urban_m+1),
                dist_edge_log = log10(dist_edge_m+1))

data_refor_pixel = data_refor_pixel %>% 
  dplyr::mutate(dist_river_log = log10(dist_river_m+1),
                dist_road_log = log10(dist_road_m+1),
                dist_urban_log = log10(dist_urban_m+1),
                dist_edge_log = log10(dist_edge_m+1))

hist(data_defor_pixel$dist_river_log)
hist(data_defor_pixel$dist_urban_log)
hist(data_defor_pixel$dist_road_log)
hist(data_defor_pixel$dist_edge_log)

hist(data_refor_pixel$dist_river_log)
hist(data_refor_pixel$dist_urban_log)
hist(data_refor_pixel$dist_road_log)
hist(data_refor_pixel$dist_edge_log)

##### Slope -------
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean_slope=mean(slope_pct))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean_slope=mean(slope_pct))
hist(data_defor_pixel$slope_pct)
hist(data_refor_pixel$slope_pct)

# Log transformation
data_defor_pixel = data_defor_pixel %>%
  dplyr::mutate(slope_pct_log = log10(slope_pct))
data_refor_pixel = data_refor_pixel %>%
  dplyr::mutate(slope_pct_log = log10(slope_pct))
hist(data_defor_pixel$slope_pct_log)
hist(data_refor_pixel$slope_pct_log)

##### Land use -----
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("area"), list(mean = mean)))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("area"), list(mean = mean)))

###### Best radius ------
# We select the radius at which variables are most correlated to type

## Deforestation dataset
data_defor_pixel %>% 
  dplyr::select(c(type, dplyr::starts_with("area"))) %>% 
  corrr::correlate() %>% 
  focus(type) 
# The most correlated variables are within 100 m
data_defor_pixel %>% 
  dplyr::select(c(type, dplyr::starts_with("prop"))) %>% 
  corrr::correlate() %>% 
  focus(type) 
# Same result with prop !

# Summary
data_defor_pixel %>% 
  tbl_summary(by = type, include = c(area_m2_r100_class_1, area_m2_r100_class_4, area_m2_r100_class_6),
              statistic = list(all_continuous() ~ "{mean} ({sd})"))
data_refor_pixel %>% 
  tbl_summary(by = type, include = c(area_m2_r100_class_1, area_m2_r100_class_4, area_m2_r100_class_6),
              statistic = list(all_continuous() ~ "{mean} ({sd})"))

## Reforestation dataset
data_refor_pixel %>% 
  dplyr::select(c(type, dplyr::starts_with("area"))) %>% 
  corrr::correlate() %>% 
  focus(type) 
# The most correlated variables are within 100 m
data_refor_pixel %>% 
  dplyr::select(c(type, dplyr::starts_with("prop"))) %>% 
  corrr::correlate() %>% 
  focus(type) 
# Same result with prop !

hist(data_defor_pixel$area_m2_r100_class_1)
hist(data_defor_pixel$area_m2_r100_class_4)
hist(data_defor_pixel$area_m2_r100_class_6)
hist(data_defor_pixel$prop_r100_class_1)
hist(data_defor_pixel$prop_r100_class_4)
hist(data_defor_pixel$prop_r100_class_6)

hist(data_refor_pixel$area_m2_r100_class_1)
hist(data_refor_pixel$area_m2_r100_class_4)
hist(data_refor_pixel$area_m2_r100_class_6)
hist(data_refor_pixel$prop_r100_class_1)
hist(data_refor_pixel$prop_r100_class_4)
hist(data_refor_pixel$prop_r100_class_6)

# Log transformation
data_defor_pixel = data_defor_pixel %>% 
  dplyr::mutate(area_m2_r100_class_1_log = log10(area_m2_r100_class_1+1),
                area_m2_r100_class_4_log = log10(area_m2_r100_class_4+1),
                area_m2_r100_class_6_log = log10(area_m2_r100_class_6+1))
data_refor_pixel = data_refor_pixel %>% 
  dplyr::mutate(area_m2_r100_class_1_log = log10(area_m2_r100_class_1+1),
                area_m2_r100_class_4_log = log10(area_m2_r100_class_4+1),
                area_m2_r100_class_6_log = log10(area_m2_r100_class_6+1))

hist(data_defor_pixel$area_m2_r100_class_1_log)
hist(data_defor_pixel$area_m2_r100_class_4_log)
hist(data_defor_pixel$area_m2_r100_class_6_log)

hist(data_refor_pixel$area_m2_r100_class_1_log)
hist(data_refor_pixel$area_m2_r100_class_4_log)
hist(data_refor_pixel$area_m2_r100_class_6_log)

##### Forest age -------
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(forest_age),
                   sd=sd(forest_age))
hist(data_defor_pixel$forest_age)

##### Precipitations -------
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(prec_sum))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(prec_sum))
hist(data_defor_pixel$prec_sum)
hist(data_refor_pixel$prec_sum)


##### Tmin -------
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(tmin_mean))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(tmin_mean))
hist(data_defor_pixel$tmin_mean)
hist(data_refor_pixel$tmin_mean)

##### Tmax -------
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(tmax_mean))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(tmax_mean))
hist(data_defor_pixel$tmax_mean)
hist(data_refor_pixel$tmax_mean)

#### Test correlations among variables ---------

##### Deforestation dataset -----
###### Pearson =====

X = data_defor_pixel %>% 
  dplyr::select(in_car, in_pub_res, in_rppn, in_rl, in_apa,
        area_m2_r100_class_1, area_m2_r100_class_4, area_m2_r100_class_6,
        prop_r100_class_1, prop_r100_class_4, prop_r100_class_6,
        dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
        slope_pct_log, prec_sum, tmin_mean, tmax_mean,
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
# Beware: (i) the area of forest and of agriculture are strongly correlated
# And (ii) tmin and tmax

###### VIF ---------
X_vif = data_defor_pixel %>% 
  dplyr::select(type, 
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                area_m2_r100_class_1, area_m2_r100_class_4, area_m2_r100_class_6,
                prop_r100_class_1, prop_r100_class_4, prop_r100_class_6,
                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                slope_pct_log, prec_sum, tmin_mean, tmax_mean,
                forest_age) %>% 
  as.data.frame()
vif.result = vif(X_vif, y.name="type")

# VIF(i) = 1/(1-Ri²) (where Ri² is the R² gotten from the regression of predictor i regarding other predictors)
# If Ri² is close to 1, it means that the variable i is well explained by the linear combination of other variables ; hence the variable i is redundant in the model 
# The more Ri² is close to 1, the more VIF increases (+Inf)
# Thus, the higher the VIF, the stronger the collinearity of i is with other variables (ie the information of i is already contained by others)
# Check VIF > 2.5
vif.result[vif.result > 2.5]
# The results confirm strong correlations between: the amount of forest and of agriculture AND between tmin and tmax

###### Variable selection ----
# We select variables based on their correlations with the response variable
data_defor_pixel %>% 
  dplyr::select(c(type, area_m2_r100_class_1, area_m2_r100_class_4, area_m2_r100_class_6,
                  prop_r100_class_1, prop_r100_class_4, prop_r100_class_6,
                  tmin_mean, tmax_mean)) %>% 
  corrr::correlate() %>% 
  focus(type) 
# Proportions are more strongly correlated to the response variable than the amounts
# We retain the proportion of agriculture
# Tmin is more strongly correlated than tmax

###### Re-run VIF ------
X_vif = data_defor_pixel %>% 
  dplyr::select(type, 
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_r100_class_4, prop_r100_class_6,
                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                slope_pct_log, prec_sum, tmin_mean,
                forest_age) %>% 
  as.data.frame()
vif.result = vif(X_vif, y.name="type")
vif.result[vif.result > 2.5] # All good !

##### Reforestation dataset -----
###### Pearson =====
X = data_refor_pixel %>% 
  dplyr::select(in_car, in_pub_res, in_rppn, in_rl, in_apa,
                area_m2_r100_class_1, area_m2_r100_class_4, area_m2_r100_class_6,
                prop_r100_class_1, prop_r100_class_4, prop_r100_class_6,
                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                slope_pct_log, prec_sum, tmin_mean, tmax_mean)
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
# (i) the area of forest and of agriculture are strongly correlated
# (ii) tmin and tmax are strongly correlated
# (iii) the amount of forest habitat is strongly correlated to distance to edge

###### VIF ---------
X_vif = data_refor_pixel %>% 
  dplyr::select(type, 
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                area_m2_r100_class_1, area_m2_r100_class_4, area_m2_r100_class_6,
                prop_r100_class_1, prop_r100_class_4, prop_r100_class_6,
                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                slope_pct_log, prec_sum, tmin_mean, tmax_mean) %>% 
  as.data.frame()
vif.result = vif(X_vif, y.name="type")

# Check VIF > 2.5
vif.result[vif.result > 2.5]
# The results confirm strong correlations between: the amount of forest and of agriculture, between tmin and tmax, between distance to edges and forest/agriculture area

###### Variable selection ----
# We select variables based on their correlations with the response variable
data_refor_pixel %>% 
  dplyr::select(c(type, area_m2_r100_class_1, area_m2_r100_class_4, area_m2_r100_class_6,
                  prop_r100_class_1, prop_r100_class_4, prop_r100_class_6,
                  tmin_mean, tmax_mean,
                  dist_edge_log)) %>% 
  corrr::correlate() %>% 
  focus(type) 
# Proportions are more strongly correlated to the response variable than the amounts
# We retain the proportion of agriculture
# Tmin is more strongly correlated than tmax

###### Re-run VIF ------
X_vif = data_refor_pixel %>% 
  dplyr::select(type, 
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_r100_class_4, prop_r100_class_6,
                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                slope_pct_log, prec_sum, tmin_mean) %>% 
  as.data.frame()
vif.result = vif(X_vif, y.name="type")
vif.result[vif.result > 2.5] # Be careful with distance to edges


#### Extract 20% of data -------

##### Deforestation dataset -----
# Split the dataset by change_type
defor_only = data_defor_pixel %>% dplyr::filter(type == 8)
control_only = data_defor_pixel %>% dplyr::filter(type == 1)

# Sample
spl_defor = dplyr::sample_frac(defor_only, 0.20)
spl_control = dplyr::sample_frac(control_only, 0.20)

# Bind the sampled data (20% of full dataset)
data_defor_20pct = dplyr::bind_rows(spl_defor, spl_control)

# Create the remaining 80% dataset
data_defor_80pct = dplyr::anti_join(data_defor_pixel, data_defor_20pct, by = colnames(data_defor_pixel))

## Check
# Number of rows
count(data_defor_20pct) + count(data_defor_80pct)
count(data_defor_pixel) == count(data_defor_20pct) + count(data_defor_80pct)
count(data_defor_pixel) * 0.20

# Number of events per category
data_defor_20pct %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))
data_defor_80pct %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))

##### Reforestation dataset -----
# Split the dataset by change_type
refor_only = data_refor_pixel %>% dplyr::filter(type == 7)
control_only = data_refor_pixel %>% dplyr::filter(type == 4)

# Sample
spl_refor = dplyr::sample_frac(refor_only, 0.20)
spl_control = dplyr::sample_frac(control_only, 0.20)

# Bind the sampled data (20% of full dataset)
data_refor_20pct = dplyr::bind_rows(spl_refor, spl_control)

# Create the remaining 80% dataset
data_refor_80pct = dplyr::anti_join(data_refor_pixel, data_refor_20pct, by = colnames(data_refor_pixel))

## Check
# Number of rows
count(data_refor_20pct) + count(data_refor_80pct)
count(data_refor_pixel) == count(data_refor_20pct) + count(data_refor_80pct)
count(data_refor_pixel) * 0.20

# Number of events per category
data_refor_20pct %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))
data_refor_80pct %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))


#### Prepare datasets for GLMM --------
# We must transform categorical variables to factors
data_defor_80pct = data_defor_80pct %>% 
  dplyr::mutate(in_apa = factor(in_apa),
                in_car = factor(in_car),
                in_pub_res = factor(in_pub_res),
                in_rppn = factor(in_rppn),
                in_rl = factor(in_rl),
                legal_status = factor(legal_status, levels=c("unknown","private","private_within_reserve","public_reserve","rl","rppn")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id))
data_defor_20pct = data_defor_20pct %>% 
  dplyr::mutate(in_apa = factor(in_apa),
                in_car = factor(in_car),
                in_pub_res = factor(in_pub_res),
                in_rppn = factor(in_rppn),
                in_rl = factor(in_rl),
                legal_status = factor(legal_status, levels=c("unknown","private","private_within_reserve","public_reserve","rl","rppn")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id))
data_refor_80pct = data_refor_80pct %>% 
  dplyr::mutate(in_apa = factor(in_apa),
                in_car = factor(in_car),
                in_pub_res = factor(in_pub_res),
                in_rppn = factor(in_rppn),
                in_rl = factor(in_rl),
                legal_status = factor(legal_status,levels=c("unknown","private","private_within_reserve","public_reserve","rl","rppn")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id))
data_refor_20pct = data_refor_20pct %>% 
  dplyr::mutate(in_apa = factor(in_apa),
                in_car = factor(in_car),
                in_pub_res = factor(in_pub_res),
                in_rppn = factor(in_rppn),
                in_rl = factor(in_rl),
                legal_status = factor(legal_status,levels=c("unknown","private","private_within_reserve","public_reserve","rl","rppn")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id))

# The response variable must be 0 (control) VS 1 (deforestation/reforestation)
data_defor_80pct = data_defor_80pct %>% 
  dplyr::mutate(type = dplyr::case_when(type == 1 ~ 0,
                                        type == 8 ~ 1,
                                        TRUE ~ NA))

data_defor_20pct = data_defor_20pct %>% 
  dplyr::mutate(type = dplyr::case_when(type == 1 ~ 0,
                                        type == 8 ~ 1,
                                        TRUE ~ NA))

data_refor_80pct = data_refor_80pct %>% 
  dplyr::mutate(type = dplyr::case_when(type == 4 ~ 0,
                                        type == 7 ~ 1,
                                        TRUE ~ NA))

data_refor_20pct = data_refor_20pct %>% 
  dplyr::mutate(type = dplyr::case_when(type == 4 ~ 0,
                                        type == 7 ~ 1,
                                        TRUE ~ NA))

# Transform the response variable to factor
data_defor_80pct = data_defor_80pct %>%
  mutate(type = factor(type, levels = c(0, 1)))

data_defor_20pct = data_defor_20pct %>%
  mutate(type = factor(type, levels = c(0, 1)))

data_refor_80pct = data_refor_80pct %>%
  mutate(type = factor(type, levels = c(0, 1)))

data_refor_20pct = data_refor_20pct %>%
  mutate(type = factor(type, levels = c(0, 1)))

prop.table(table(data_defor_80pct$type))
prop.table(table(data_defor_20pct$type))
prop.table(table(data_refor_80pct$type))
prop.table(table(data_refor_80pct$type))

# We standardize to compare effect sizes with the function scale()
# Deforestation dataset
data_defor_80pct = data_defor_80pct %>% 
  dplyr::mutate(dplyr::across(c(prop_r100_class_4, prop_r100_class_6,
                                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                                slope_pct_log, prec_sum, tmin_mean, forest_age), 
                              ~ as.vector(scale(.x))))
data_defor_20pct = data_defor_20pct %>% 
  dplyr::mutate(dplyr::across(c(prop_r100_class_4, prop_r100_class_6,
                                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                                slope_pct_log, prec_sum, tmin_mean, forest_age), 
                              ~ as.vector(scale(.x))))
# Reforestation dataset
data_refor_80pct = data_refor_80pct %>% 
  dplyr::mutate(dplyr::across(c(prop_r100_class_4, prop_r100_class_6,
                                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                                slope_pct_log, prec_sum, tmin_mean), 
                              ~ as.vector(scale(.x))))
data_refor_20pct = data_refor_20pct %>% 
  dplyr::mutate(dplyr::across(c(prop_r100_class_4, prop_r100_class_6,
                                dist_river_log, dist_urban_log, dist_road_log, dist_edge_log,
                                slope_pct_log, prec_sum, tmin_mean), 
                              ~ as.vector(scale(.x))))

#### Random Forest --------

##### With VSURF ---------
###### Example ---------
# The following code is from Genuer et al. 2015, The R Journal

data("toys")
set.seed(3101318)

### Wrapping function
toys.vsurf <- VSURF(x = toys$x, y = toys$y, mtry = 100)
names(toys.vsurf)
summary(toys.vsurf)

#### In details...
### Step 1: Preliminary elimination and ranking
## Variable ranking

plot(toys.vsurf)
par(mfrow=c(1,1))
plot(toys.vsurf, step = "thres", imp.mean = FALSE, ylim = c(0, 2e-4)) # Zoom of the top right graph
# The result of variable ranking is drawn on the top left graph. True variables are significantly more important than the noisy ones.
# Starting from this order, the plot of the corresponding standard deviations of VI is used to estimate a threshold value for VI. This threshold (figured by the dotted horizontal red line on the top right graph of Figure 1) is set to the minimum prediction value given by a CART model fitting this curve (see the green piece-wise constant function on the same graph). Then only the variables with an averaged VI exceeding this level (i.e. above the horizontal red line in the top left graph) are retained.

## Variable elimination
# The computation of the 50 forests, the ranking and elimination steps are obtained with the VSURF_thres function
toys.thres <- VSURF_thres(toys$x, toys$y, mtry = 100)
# The main outputs are: varselect.thres which contains the indices of variables selected by this step, imp.mean.dec and imp.sd.dec which hold the VI mean and standard deviation (the order according to decreasing VI mean can be found in imp.mean.dec.ind).
toys.thres$varselect.thres

### Step 2: Variable selection
## Variable selection procedure for interpretation
toys.interp <- VSURF_interp(toys$x, toys$y, vars = toys.thres$varselect.thres) # varselect.interp: the variables selected by this step, and err.interp: OOB error rates of RF nested models
toys.interp$varselect.interp # in the bottom left graph, we see that the error decreases quickly. It reaches its (almost) minimum when the first four true variables are included in the model (see the vertical red line) and then it remains nearly constant

## Variable selection procedure for prediction
toys.pred <- VSURF_pred(toys$x, toys$y, err.interp = toys.interp$err.interp, varselect.interp = toys.interp$varselect.interp)
# main outputs of the VSURF_pred function are the variables selected by this final step, varselect.pred, and the OOB error rates of RF models, err.pred.
toys.pred$varselect.pred # For the toys data, the final model for prediction purpose involves only variables V3, V6, V5


### Other example on a data frame (ozone data set)
data("Ozone", package = "mlbench")
vozone <- VSURF(V4 ~ ., data = Ozone, na.action = na.omit) # V4 is the dependent variable
summary(vozone)
plot(vozone, step = "thres", imp.sd = FALSE, var.names = TRUE) # variable importance associated with each of the explanatory variables.
# three very sensible groups of variables can be discerned ranging from the most to the least important
number <- c(1:3, 5:13) # reorder the output variables of the procedure
# To know which index is which variable, refer to the Ozone dataset: variable index "1" is the first column (V1, the month)
number[vozone$varselect.thres] # the 3 variables of negative importance (variables 6, 3 and 2) are eliminated (not in the list)
number[vozone$varselect.interp] # the interpretation procedure leads to select the model with 5 variables, which contains all of the most important variables
number[vozone$varselect.pred] # the prediction step does not remove any additional variable.

###### On my dataset ---------

### Deforestation
subset = data_defor_80pct %>% 
  dplyr::select(c(type, legal_status, ns_br101,
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_r100_class_4, prop_r100_class_6,
                dist_river_log, dist_road_log, dist_urban_log, dist_edge_log,
                slope_pct_log, prec_sum, tmin_mean,
                forest_age))

subset = subset %>% dplyr::sample_frac(0.05)

rf = VSURF(type ~ ., data = subset, parallel = TRUE) # Very long ! See if we can optimize or try another option ?
summary(rf)
order = c(2:18) # reorder the output variables of the procedure (the index (e.g., 9) is the same index in the original dataframe) 
order[rf$varselect.thres] # the variables of negative importance are eliminated (not in the list)
order[rf$varselect.interp] # the interpretation procedure leads to select the model with X variables, which contains all of the most important variables
order[rf$varselect.pred] # the prediction step

### Reforestation
subset = data_refor_80pct %>% 
  dplyr::select(c(type, legal_status, ns_br101,
                  in_car, in_pub_res, in_rppn, in_rl, in_apa,
                  prop_r100_class_4, prop_r100_class_6,
                  dist_river_log, dist_road_log, dist_urban_log, dist_edge_log,
                  slope_pct_log, prec_sum, tmin_mean))

subset = subset %>% dplyr::sample_frac(0.05)

rf = VSURF(type ~ ., data = subset, parallel = TRUE) # Very long ! See if we can optimize or try another option ?
summary(rf)
order = c(2:17) # reorder the output variables of the procedure
order[rf$varselect.thres] # the variables of negative importance are eliminated (not in the list)
order[rf$varselect.interp] # the interpretation procedure leads to select the model with X variables, which contains all of the most important variables
order[rf$varselect.pred] # the prediction step


##### With Boruta ---------

### Deforestation
subset = data_defor_80pct %>% 
  dplyr::select(c(type, legal_status, ns_br101,
                  in_car, in_pub_res, in_rppn, in_rl, in_apa,
                  prop_r100_class_4, prop_r100_class_6,
                  dist_river_log, dist_road_log, dist_urban_log, dist_edge_log,
                  slope_pct_log, prec_sum, tmin_mean,
                  forest_age))

subset = subset %>% dplyr::sample_frac(0.05)

set.seed(1)
# Setting doTrace argument to 1 or 2 makes Boruta report the progress of the process; version 2 is a little more verbose, namely it shows attribute decisions as soon as they are cleared
Boruta.defor = Boruta(type ~ ., data = subset, doTrace = 2, ntree = 500)
Boruta.defor # Result
plot(Boruta.defor) # Z scores variability among attributes during the Boruta run
getConfirmedFormula(Boruta.defor) # formula object that defines a model based only on confirmed attributes
attStats(Boruta.defor) # creates a data frame containing each attribute’s Z score statistics and the fraction of random forest runs in which this attribute was more important than the most important shadow one

### Reforestation
subset = data_refor_80pct %>% 
  dplyr::select(c(type, legal_status, ns_br101,
                  in_car, in_pub_res, in_rppn, in_rl, in_apa,
                  prop_r100_class_4, prop_r100_class_6,
                  dist_river_log, dist_road_log, dist_urban_log, dist_edge_log,
                  slope_pct_log, prec_sum, tmin_mean))

subset = subset %>% dplyr::sample_frac(0.05)

set.seed(1)
# Setting doTrace argument to 1 or 2 makes Boruta report the progress of the process; version 2 is a little more verbose, namely it shows attribute decisions as soon as they are cleared
Boruta.defor = Boruta(type ~ ., data = subset, doTrace = 2, ntree = 500)
Boruta.defor # Result
plot(Boruta.defor) # Z scores variability among attributes during the Boruta run
getConfirmedFormula(Boruta.defor) # formula object that defines a model based only on confirmed attributes
attStats(Boruta.defor) # creates a data frame containing each attribute’s Z score statistics and the fraction of random forest runs in which this attribute was more important than the most important shadow one

#### Binomial GLMM -------

##### Deforestation ----
str(data_defor_80pct)

# We check whether the probability is close to 0.5 (we cannot model probability variance under 0.1 and above 0.9)
prop.table(table(data_defor_80pct$type)) # Exactly 0.5 due to sampling strategy

# Plots
par(mfrow=c(3,4))
boxplot(prop_r100_class_4~type, data=data_defor_80pct)
boxplot(prop_r100_class_6~type, data=data_defor_80pct)
boxplot(dist_river_log~type, data=data_defor_80pct)
boxplot(dist_urban_log~type, data=data_defor_80pct)
boxplot(dist_road_log~type, data=data_defor_80pct)
boxplot(dist_edge_log~type, data=data_defor_80pct)
boxplot(slope_pct~type, data=data_defor_80pct)
boxplot(prec_sum~type, data=data_defor_80pct)
boxplot(tmin_mean~type, data=data_defor_80pct)
boxplot(forest_age~type, data=data_defor_80pct)
par(mfrow=c(1,1))

##### GLMMs -------

## GLMM with binomial distribution
subset = data_refor_80pct %>% 
  dplyr::select(c(type, legal_status, ns_br101,
                  in_car, in_pub_res, in_rppn, in_rl, in_apa,
                  prop_r100_class_4, prop_r100_class_6,
                  dist_river_log, dist_road_log, dist_urban_log, dist_edge_log,
                  slope_pct_log, prec_sum, tmin_mean,
                  year))

subset = subset %>% dplyr::sample_frac(0.05)

mod1 = glmer(type ~ legal_status + ns_br101 + in_car + in_pub_res + in_rppn + in_rl + in_apa +
               prop_r100_class_4 + prop_r100_class_6 + 
               dist_river_log + dist_road_log + dist_urban_log + dist_edge_log + 
               slope_pct_log + prec_sum + tmin_mean + (1|year), family=binomial, data=subset,
             control = glmerControl(optimizer = "bobyqa"))
summary(mod1)
