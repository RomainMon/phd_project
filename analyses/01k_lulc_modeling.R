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
library(glmmTMB)
library(DHARMa)
library(performance)
library(spdep)
library(sp)
library(cv)
library(rsample)
library(pROC)
library(rsq)
library(MASS)
library(broom)
library(broom.mixed)
library(outliers)
library(BAMMtools)
library(patchwork)
library(spatialreg)
library(nlme)
library(MuMIn)
library(caret)

### Load datasets
data_defor_pixel = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", 
                                "data_defor_pixel_thinned.rds"))
data_refor_pixel = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", 
                                "data_refor_pixel_thinned.rds"))
data_car = readRDS(here("outputs", "data", "Mapbiomas", "LULCC_datasets", "data_defor_refor_car.rds"))
plot(sf::st_geometry(data_car))

### Pixel-scale analysis -------------

#### Summary ----------

# Data structure
str(data_defor_pixel)
str(data_refor_pixel)

##### NAs --------
# Number of NAs
sum(is.na(data_defor_pixel))
sum(is.na(data_refor_pixel))

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


#### Qualitative variables --------

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

###### APA MLD -----
table(data_defor_pixel$type, data_defor_pixel$in_apa)
table(data_refor_pixel$type, data_refor_pixel$in_apa)

#### Quantitative variables ------
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

##### Slope -------
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean_slope=mean(slope_pct))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean_slope=mean(slope_pct))
hist(data_defor_pixel$slope_pct)
hist(data_refor_pixel$slope_pct)

##### Elevation -------
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean_alt=mean(alt_m))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean_alt=mean(alt_m))
hist(data_defor_pixel$alt_m)
hist(data_refor_pixel$alt_m)

##### Land use -----
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("prop"), list(mean = mean)))
data_refor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(across(starts_with("prop"), list(mean = mean)))

###### Best radius ------
# We select the radius at which variables are most correlated to type

## Deforestation dataset
data_defor_pixel %>% 
  dplyr::select(c(type, dplyr::starts_with("prop"))) %>% 
  corrr::correlate() %>% 
  focus(type) 
# The most correlated variables are within 100 m

## Reforestation dataset
data_refor_pixel %>% 
  dplyr::select(c(type, dplyr::starts_with("prop"))) %>% 
  corrr::correlate() %>% 
  focus(type) 
# The most correlated variables are within 100 m

# Summary
data_defor_pixel %>% 
  tbl_summary(by = type, include = c(prop_forest_100m, prop_agri_100m, prop_urb_100m, prop_defor_100m),
              statistic = list(all_continuous() ~ "{mean} ({sd})"))
data_refor_pixel %>% 
  tbl_summary(by = type, include = c(prop_forest_100m, prop_agri_100m, prop_urb_100m, prop_refor_100m),
              statistic = list(all_continuous() ~ "{mean} ({sd})"))



hist(data_defor_pixel$prop_forest_100m)
hist(data_defor_pixel$prop_agri_100m)
hist(data_defor_pixel$prop_urb_100m)
hist(data_defor_pixel$prop_defor_100m)

hist(data_refor_pixel$prop_forest_100m)
hist(data_refor_pixel$prop_agri_100m)
hist(data_refor_pixel$prop_urb_100m)
hist(data_refor_pixel$prop_refor_100m)

##### Forest age -------
data_defor_pixel %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(mean=mean(forest_age),
                   sd=sd(forest_age))
hist(data_defor_pixel$forest_age)
plot(data_defor_pixel$year, data_defor_pixel$forest_age) # Warning here
# Forest age is correlated with year due to construction of forest age

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


#### Prepare datasets for GLMM --------

##### Spatial blocking --------

# Deforestation dataset
data_defor_pixel$block_x = floor(data_defor_pixel$x / 20000) # Distance in meters
data_defor_pixel$block_y = floor(data_defor_pixel$y / 20000) # Distance in meters
data_defor_pixel = data_defor_pixel %>%
  dplyr::mutate(sp_block = paste0(block_x, "_", block_y))
data_defor_pixel$sp_block = factor(data_defor_pixel$sp_block)
sample = data_defor_pixel %>% dplyr::sample_frac(0.1)
ggplot(sample, aes(x = x, y = y, color = sp_block)) +
  geom_point(size = 0.3) +
  theme_minimal() +
  guides(color = "none")

# Reforestation dataset
data_refor_pixel$block_x = floor(data_refor_pixel$x / 20000) # Distance in meters
data_refor_pixel$block_y = floor(data_refor_pixel$y / 20000) # Distance in meters
data_refor_pixel = data_refor_pixel %>%
  dplyr::mutate(sp_block = paste0(block_x, "_", block_y))
data_refor_pixel$sp_block = factor(data_refor_pixel$sp_block)
sample = data_refor_pixel %>% dplyr::sample_frac(0.1)
ggplot(sample, aes(x = x, y = y, color = sp_block)) +
  geom_point(size = 0.3) +
  theme_minimal() +
  guides(color = "none")

##### Scale drivers --------
# We standardize to compare effect sizes with the function scale()
# Deforestation dataset
data_defor_pixel = data_defor_pixel %>%
  dplyr::mutate(dplyr::across(c(prop_forest_100m, prop_agri_100m, prop_urb_100m, prop_defor_100m,
                                dist_river_m, dist_urban_m, dist_road_m, dist_edge_m,
                                alt_m, slope_pct, 
                                prec_sum, tmin_mean, tmax_mean, 
                                forest_age),
                              ~ as.vector(scale(.x)),
                              .names = "{.col}_sc"))
# Reforestation dataset
data_refor_pixel = data_refor_pixel %>%
  dplyr::mutate(dplyr::across(c(prop_forest_100m, prop_agri_100m, prop_urb_100m, prop_refor_100m,
                                dist_river_m, dist_urban_m, dist_road_m, dist_edge_m,
                                alt_m, slope_pct, 
                                prec_sum, tmin_mean, tmax_mean),
                              ~ as.vector(scale(.x)),
                              .names = "{.col}_sc"))

#### Correlation and VIF ---------

##### Deforestation dataset -----
###### Pearson =====

X = data_defor_pixel %>% 
  dplyr::select(in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_forest_100m_sc, prop_agri_100m_sc, prop_urb_100m_sc, prop_defor_100m_sc,
                dist_river_m_sc, dist_urban_m_sc, dist_road_m_sc, dist_edge_m_sc,
                slope_pct_sc, alt_m_sc,
                prec_sum_sc, tmin_mean_sc, tmax_mean_sc,
                forest_age_sc,
                year)
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
# Beware: (i) proportion of forest and of agriculture are strongly correlated
# And (ii) tmin and tmax
# And (iii) forest_age and year
# And (iv) proportion of agriculture and of deforested area
# And (v) proportion of forest and of deforested area

###### VIF ---------
X_vif = data_defor_pixel %>% 
  dplyr::select(type, 
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_forest_100m_sc, prop_agri_100m_sc, prop_urb_100m_sc, prop_defor_100m_sc,
                dist_river_m_sc, dist_urban_m_sc, dist_road_m_sc, dist_edge_m_sc,
                slope_pct_sc, alt_m_sc,
                prec_sum_sc, tmin_mean_sc, tmax_mean_sc,
                forest_age_sc,
                year) %>%
  as.data.frame()
vif.result = HH::vif(X_vif, y.name="type")

# VIF(i) = 1/(1-Ri²) (where Ri² is the R² gotten from the regression of predictor i regarding other predictors)
# If Ri² is close to 1, it means that the variable i is well explained by the linear combination of other variables ; hence the variable i is redundant in the model 
# The more Ri² is close to 1, the more VIF increases (+Inf)
# Thus, the higher the VIF, the stronger the collinearity of i is with other variables (ie the information of i is already contained by others)
# Check VIF > 2.5
vif.result[vif.result > 2.5]
# The results confirm strong correlations between: 
# - the amount of forest and of agriculture 
# - tmin and tmax
# - year and forest_age

###### Variable selection ----
# We select variables based on their correlations with the response variable
data_defor_pixel %>% 
  dplyr::select(c(type, 
                  prop_forest_100m_sc, prop_agri_100m_sc, prop_defor_100m_sc,
                  tmin_mean_sc, tmax_mean_sc)) %>% 
  corrr::correlate() %>% 
  focus(type) 
# We retain the proportion of forest area
# Tmin is more strongly correlated than tmax

###### Re-run VIF ------
X_vif = data_defor_pixel %>% 
  dplyr::select(type, 
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_forest_100m_sc,
                dist_river_m_sc, dist_urban_m_sc, dist_road_m_sc, dist_edge_m_sc,
                slope_pct_sc, alt_m_sc,
                prec_sum_sc, tmin_mean_sc,
                forest_age_sc,
                year) %>% 
  as.data.frame()
vif.result = HH::vif(X_vif, y.name="type")
vif.result[vif.result > 2.5]
# There is still a strong correlation between forest age and year

##### Reforestation dataset -----
###### Pearson =====
X = data_refor_pixel %>% 
  dplyr::select(in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_forest_100m_sc, prop_agri_100m_sc, prop_urb_100m_sc, prop_refor_100m_sc,
                dist_river_m, dist_urban_m, dist_road_m, dist_edge_m,
                slope_pct_sc, alt_m_sc,
                prec_sum_sc, tmin_mean_sc, tmax_mean_sc,
                year)
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
# (i) the proportion of forest and of agriculture are strongly correlated
# (ii) tmin and tmax are strongly correlated
# (iii) the proportion of forest habitat is strongly correlated to reforested area
# (iv) the proportion of reforested and area of agriculture are strongly correlated

###### VIF ---------
X_vif = data_refor_pixel %>% 
  dplyr::select(type, 
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_forest_100m_sc, prop_agri_100m_sc, prop_urb_100m_sc, prop_refor_100m_sc,
                dist_river_m_sc, dist_urban_m_sc, dist_road_m_sc, dist_edge_m_sc,
                slope_pct_sc, alt_m_sc,
                prec_sum_sc, tmin_mean_sc, tmax_mean_sc,
                year) %>% 
  as.data.frame()
vif.result = HH::vif(X_vif, y.name="type")

# Check VIF > 2.5
vif.result[vif.result > 2.5]
# The results confirm strong correlations between: 
# - the amount of forest and of agriculture
# - tmin and tmax

###### Variable selection ----
# We select variables based on their correlations with the response variable
data_refor_pixel %>% 
  dplyr::select(c(type, 
                  prop_forest_100m_sc, prop_agri_100m_sc, prop_refor_100m_sc,
                  tmin_mean_sc, tmax_mean_sc)) %>% 
  corrr::correlate() %>% 
  focus(type) 
# We retain the proportion of forest
# We keep tmin to remain consistent

###### Re-run VIF ------
X_vif = data_refor_pixel %>% 
  dplyr::select(type, 
                in_car, in_pub_res, in_rppn, in_rl, in_apa,
                prop_forest_100m_sc, prop_urb_100m_sc,
                dist_river_m_sc, dist_urban_m_sc, dist_road_m_sc, dist_edge_m_sc,
                slope_pct_sc, alt_m_sc, 
                prec_sum_sc, tmin_mean_sc) %>% 
  as.data.frame()
vif.result = HH::vif(X_vif, y.name="type")
vif.result[vif.result > 2.5] # All good

#### To factor --------

# We must transform categorical variables to factors
data_defor_pixel = data_defor_pixel %>% 
  dplyr::mutate(in_car = factor(in_car, levels = c(0,1), labels = c("Outside","Inside")),
                in_pub_res = factor(in_pub_res, levels = c(0,1), labels = c("Outside","Inside")),
                in_rppn = factor(in_rppn, levels = c(0,1), labels = c("Outside","Inside")),
                in_rl = factor(in_rl, levels = c(0,1), labels = c("Outside","Inside")),
                in_apa = factor(in_apa, levels = c(0,1), labels = c("Outside","Inside")),
                legal_status = factor(legal_status, levels=c("private","unknown","private_within_reserve","public_reserve","rl","rppn")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id))
data_refor_pixel = data_refor_pixel %>% 
  dplyr::mutate(in_car = factor(in_car, levels = c(0,1), labels = c("Outside","Inside")),
                in_pub_res = factor(in_pub_res, levels = c(0,1), labels = c("Outside","Inside")),
                in_rppn = factor(in_rppn, levels = c(0,1), labels = c("Outside","Inside")),
                in_rl = factor(in_rl, levels = c(0,1), labels = c("Outside","Inside")),
                in_apa = factor(in_apa, levels = c(0,1), labels = c("Outside","Inside")),
                legal_status = factor(legal_status,levels=c("private","unknown","private_within_reserve","public_reserve","rl","rppn")),
                ns_br101 = factor(ns_br101),
                year = factor(year),
                cell_id = factor(cell_id))

# The response variable must be 0 (control) VS 1 (deforestation/reforestation)
data_defor_pixel = data_defor_pixel %>% 
  dplyr::mutate(type = dplyr::case_when(type == 1 ~ 0,
                                        type == 8 ~ 1,
                                        TRUE ~ NA))

data_refor_pixel = data_refor_pixel %>% 
  dplyr::mutate(type = dplyr::case_when(type == 4 ~ 0,
                                        type == 7 ~ 1,
                                        TRUE ~ NA))

# Quick checks
prop.table(table(data_defor_pixel$type))
prop.table(table(data_refor_pixel$type))

#### Modeling -------

##### Deforestation (GLMM) ----
str(data_defor_pixel)

###### Quick checks -----
# We check whether the probability is close to 0.5 (we cannot model probability variance under 0.1 and above 0.9)
prop.table(table(data_defor_pixel$type))

# Plots
boxplot(prop_forest_100m_sc~type, data=data_defor_pixel)
boxplot(prop_urb_100m_sc~type, data=data_defor_pixel)
boxplot(dist_river_m_sc~type, data=data_defor_pixel)
boxplot(dist_urban_m_sc~type, data=data_defor_pixel)
boxplot(dist_road_m_sc~type, data=data_defor_pixel)
boxplot(dist_edge_m_sc~type, data=data_defor_pixel)
boxplot(slope_pct_sc~type, data=data_defor_pixel)
boxplot(prec_sum_sc~type, data=data_defor_pixel)
boxplot(tmin_mean_sc~type, data=data_defor_pixel)
boxplot(forest_age_sc~type, data=data_defor_pixel)

###### GLMMs -------

###### Sub-sample  ----
# We sub-sample the dataset for cross-validation
data_defor_pixel = data_defor_pixel %>%
  dplyr::mutate(group = interaction(type, year))
# Stratified sub-sample
split = rsample::initial_split(
  data_defor_pixel,
  prop = 0.8,  # 80% training model
  strata = "group"  # Stratification
)
# Extract training and testing datasets
train_data = training(split)
test_data = testing(split)
# Check equal repartition
prop.table(table(train_data$type))
prop.table(table(test_data$type))
train_data %>% 
  dplyr::group_by(year, type) %>% 
  dplyr::count()
test_data %>% 
  dplyr::group_by(year, type) %>% 
  dplyr::count()

## GLMM with binomial distribution
# We use the logit function to predict values between 0 and 1
# Binomial model
# IF crossed random effects (block and year) : (1|block)+(1|year), that means that every block appears multiple times in every year of the dataset
mod_glmm = glmmTMB(formula = 
                     type ~ 
                     in_car + in_pub_res + in_rppn + in_rl + in_apa + ns_br101 +
                     prop_forest_100m_sc + prop_urb_100m_sc + 
                     dist_river_m_sc + dist_road_m_sc + dist_urban_m_sc + dist_edge_m_sc + 
                     slope_pct_sc + prec_sum_sc + tmin_mean_sc + 
                     (1|year) + (1|sp_block),
                   REML=T, family=binomial, data=train_data)
summary(mod_glmm)


###### Validation -----------

### Check convergence
check_convergence(mod_glmm)

# 1) Examine plots of residuals versus fitted values for the entire model
# 2) Model residuals versus all explanatory variables to look for patterns
# 3) For GLMMs: residuals versus fitted values for each grouping level of a random intercept factor

#### Dharma method (from Hartig 2024)
# Rationale: misspecifications in GL(M)Ms cannot reliably be diagnosed with standard residual plots
# The "DHARMa" package uses a simulation-based approach to create readily interpretable scaled (quantile) residuals for fitted generalized linear (mixed) models
# The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression
# Also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial, temporal and phylogenetic autocorrelation.
# A residual of 0 means that all simulated values are larger than the observed value, and a residual of 0.5 means half of the simulated values are larger than the observed value.
# NB: If you have a lot of data points, residual diagnostics will nearly inevitably become significant, because having a perfectly fitting model is very unlikely.
# DHARMa only flags a difference between the observed and expected data - the user has to decide whether this difference is actually a problem for the analysis!

# Calculate the residuals, using the simulateResiduals() function (randomized quantile residuals)
simulationOutput = simulateResiduals(fittedModel = mod_glmm, plot = F)
# Access to residuals
plot(simulationOutput)
# Left panel: qq-plot to detect overall deviations from the expected distribution, by default with added tests for correct distribution (KS test), dispersion and outliers.
# Right panel: plotResiduals (right panel) produces a plot of the residuals against the predicted value (or alternatively, other variable). Simulation outliers (data points that are outside the range of simulated values) are highlighted as red stars.
# To provide a visual aid in detecting deviations from uniformity in y-direction, the plot function calculates an (optional default) quantile regression, which compares the empirical 0.25, 0.5 and 0.75 quantiles in y direction (red solid lines) with the theoretical 0.25, 0.5 and 0.75 quantiles (dashed black line), and provides a p-value for the deviation from the expected quantile
plotQQunif(simulationOutput) # left plot in plot.DHARMa()
plotResiduals(simulationOutput) # right plot in plot.DHARMa()

# GL(M)Ms often display over/underdispersion, which means that residual variance is larger/smaller than expected under the fitted model.
# If overdispersion is present, the main effect is that confidence intervals tend to be too narrow, and p-values to small, leading to inflated type I error. The opposite is true for underdispersion, i.e. the main issue of underdispersion is that you loose power.
# To check for over/underdispersion, plot the simulateResiduals() and check for deviation around the red line (and residuals around 0 and 1); see examples in DHARMa vignette
# DHARMa contains several overdispersion tests that compare the dispersion of simulated residuals to the observed residuals
testDispersion(simulationOutput)
# A significant ratio > 1 indicates overdispersion, a significant ratio < 1 underdispersion.
# Slight overdispersion 

# A second test that is typically run for LMs, but not for GL(M)Ms is to plot residuals against the predictors in the model (or potentially predictors that were not in the model) to detect possible misspecifications
# If you plot the residuals against predictors, space or time, the resulting plots should not only show no systematic dependency of those residuals on the covariates, but they should also again be flat for each fixed situation
plotResiduals(simulationOutput, train_data$in_car)
plotResiduals(simulationOutput, train_data$in_pub_res)
plotResiduals(simulationOutput, train_data$in_rppn)
plotResiduals(simulationOutput, train_data$in_rl)
plotResiduals(simulationOutput, train_data$in_apa)
plotResiduals(simulationOutput, train_data$ns_br101)
plotResiduals(simulationOutput, train_data$prop_forest_100m_sc)
plotResiduals(simulationOutput, train_data$prop_urb_100m_sc)
plotResiduals(simulationOutput, train_data$dist_river_m_sc)
plotResiduals(simulationOutput, train_data$dist_road_m_sc)
plotResiduals(simulationOutput, train_data$dist_urban_m_sc)
plotResiduals(simulationOutput, train_data$dist_edge_m_sc)
plotResiduals(simulationOutput, train_data$slope_pct_sc)
plotResiduals(simulationOutput, train_data$prec_sum_sc)
plotResiduals(simulationOutput, train_data$tmin_mean_sc)
plotResiduals(simulationOutput, train_data$year)


## Autocorrelation
# Spatial
res2 = recalculateResiduals(simulationOutput, group = train_data$year)
testSpatialAutocorrelation(res2, 
                           x = aggregate(train_data$x, list(train_data$year), mean)$x,
                           y = aggregate(train_data$y, list(train_data$year), mean)$x)
# Temporal
testTemporalAutocorrelation(res2, time = unique(train_data$year))


###### Spatial autocorrelation ----------
# If a distance between residuals can be defined (temporal, spatial, phylogenetic), we need to check if there is a distance-dependence in the residuals
# If autocorrelation is ignored, estimation of variance around predictors is biased = type I error risk (significant effects that are actually not significant)

# What is maximal distance between two points?
set.seed(123)
sample_indices = sample(1:nrow(train_data), 10000) # Sample
coords_sample = cbind(train_data$x[sample_indices], train_data$y[sample_indices])
distmat = as.matrix(dist(coords_sample))
dim(distmat)
max(distmat)
# Compute max distance to build correlograms (~1/2 to 2/3 total dist)
maxdist = 2/3*max(distmat)
maxdist

## Autocorrelation within the response variable
sample = train_data[sample_indices, ]

correlog.sp = data.frame(dist=seq(from=10000, to=maxdist, by=10000),
                         Morans.i=NA, Null.lcl=NA, Null.ucl=NA, Pvalue=NA)
head(correlog.sp)

# To spatial object
coords_sp_sample = SpatialPoints(coords_sample)

for (i in 1:nrow(correlog.sp)){
  ## Step 1: Neighbor definition
  # First and last values of distance class (for computing Moran's I)
  d.start = correlog.sp[i,"dist"]-10000 # the inferior value equals d-X
  d.end = correlog.sp[i,"dist"] # the superior value equals d
  # List of neighbors
  neigh = dnearneigh(x=coords_sp_sample, d1=d.start, d.end)
  ## Step 2: conversion into weights matrix
  wts = nb2listw(neighbours=neigh, style='W', zero.policy=T)
  ## Step 3: Compute Moran's I for this class of distance
  mor.i = moran.mc(x=sample$type, listw=wts, nsim=99, alternative="greater", zero.policy=T)
  
  # Integrate results into data frame
  correlog.sp[i, "dist"] = (d.end+d.start)/2  # Mean class distance
  correlog.sp[i, "Morans.i"] = mor.i$statistic # Moran I
  correlog.sp[i, "Null.lcl"] = quantile(mor.i$res, probs = 0.025,na.rm = TRUE)  # Confidence Interval (high envelop)
  correlog.sp[i, "Null.ucl"] = quantile(mor.i$res, probs = 0.975,na.rm = TRUE)  # Confidence Interval (low envelop)
  correlog.sp[i, "Pvalue"] = mor.i$p.value	# p-value (of Moran's I)
}

correlog.sp

# Plot the correlogram
plot(y=correlog.sp$Morans.i, x=correlog.sp$dist,
     xlab="Lag Distance(m)", ylab="Moran's I", ylim=c(-0.3,0.3))         
abline(h=0)                                                              
lines(correlog.sp$dist, correlog.sp$Null.lcl,col = "red")	               
lines(correlog.sp$dist, correlog.sp$Null.ucl,col = "red")
  

## Autocorrelation of GLMM residuals
res_mod1_sample = residuals(mod_glmm, type = "pearson")[sample_indices]
correlog.sp = data.frame(dist=seq(from=10000, to=maxdist, by=10000),
                          Morans.i=NA, Null.lcl=NA, Null.ucl=NA, Pvalue=NA)
head(correlog.sp)

# To spatial object
coords_sp_sample = SpatialPoints(coords_sample)

for (i in 1:nrow(correlog.sp)){
  ## Step 1: Neighbor definition
  # First and last values of distance class (for computing Moran's I)
  d.start = correlog.sp[i,"dist"]-10000 # the inferior value equals d-X
  d.end = correlog.sp[i,"dist"] # the superior value equals d
  # List of neighbors
  neigh = dnearneigh(x=coords_sp_sample, d1=d.start, d.end)
  ## Step 2: conversion into weights matrix
  wts = nb2listw(neighbours=neigh, style='W', zero.policy=T)
  ## Step 3: Compute Moran's I for this class of distance
  mor.i = moran.mc(x=res_mod1_sample, listw=wts, nsim=99, alternative="greater", zero.policy=T)
  
  # Integrate results into data frame
  correlog.sp[i, "dist"] = (d.end+d.start)/2  # Mean class distance
  correlog.sp[i, "Morans.i"] = mor.i$statistic # Moran I
  correlog.sp[i, "Null.lcl"] = quantile(mor.i$res, probs = 0.025,na.rm = TRUE)  # Confidence Interval (high envelop)
  correlog.sp[i, "Null.ucl"] = quantile(mor.i$res, probs = 0.975,na.rm = TRUE)  # Confidence Interval (low envelop)
  correlog.sp[i, "Pvalue"] = mor.i$p.value	# p-value (of Moran's I)
}

correlog.sp

# Plot the correlogram
plot(y=correlog.sp$Morans.i, x=correlog.sp$dist,
     xlab="Lag Distance(m)", ylab="Moran's I", ylim=c(-0.3,0.3))         
abline(h=0)                                                              
lines(correlog.sp$dist, correlog.sp$Null.lcl,col = "red")	               
lines(correlog.sp$dist, correlog.sp$Null.ucl,col = "red")

# There is spatial autocorrelation until 20000 m (model without spatial block as random effect)
# When accounting for the spatial structure (spatial blocks as random factors), the spatial autocorrelation is gone !

###### Simple cross-validation -----------
# ROC is used to measure the performance of models predicting the presence or absence of a phenomenon
# It is often summarised by the area under the curve (AUC) where one indicates a perfect fit and 0.5 indicates a purely random fit.

# Training
pred_train = predict(mod_glmm, type="response", re.form = NA) # we remove random effects
# By removing Random effects, we test whether predictors alone discriminate deforestation
roc_train = roc(train_data$type, pred_train)
auc_train = auc(roc_train)
cat("Train AUC:", auc_train, "\n")

# Testing
pred_test = predict(mod_glmm, newdata=test_data, type="response", re.form = NA)
roc_test = roc(test_data$type, pred_test)
auc_test = auc(roc_test)
cat("Test AUC:", auc_test, "\n")

plot(roc_train, col="blue", lwd=2, main="ROC Curves")
lines(roc_test, col="red", lwd=2)


###### K-fold cross-validation -----------
# Approach to estimating the predictive accuracy of regression models
# Data-division (e.g. 30/70) suffers from two problems: (1) Dividing the data decreases the sample size and thus increases sampling error; and (2), even more disconcertingly, particularly in smaller samples, the results can vary substantially based on the random division of the data
# In CV, the data are randomly divided as equally as possible into several, say k, parts, called “folds.” The statistical model is fit k times, leaving each fold out in turn. Each fitted model is then used to predict the response variable for the cases in the omitted fold.
# A CV criterion or “cost” measure, such as the mean-squared error (“MSE”) of prediction, is then computed using these predicted values
# In the extreme k=n, the number of cases in the data, thus omitting individual cases and refitting the model n times—a procedure termed “leave-one-out (LOO) cross-validation.”
# Estimated prediction error for k-fold CV with k=5 or 10 (commonly employed choices) is more biased than estimated prediction error for LOO CV

# Procedure: 1) Randomly divide a dataset into k groups, or “folds”, of roughly equal size.
# 2) Choose one of the folds to be the holdout set. Fit the model on the remaining k-1 folds. Calculate the test MSE on the observations in the fold that was held out.
# 3) Repeat this process k times, using a different set each time as the holdout set.
# 4) Calculate the overall test MSE to be the average of the k test MSE’s
summary(cv(mod_glmm,
           k = 5,
           clusterVariables = c("sp_block", "year")))
# The cv() methods returns the CV criterion ("CV crit"), the bias- adjusted CV criterion ("adj CV crit"), the criterion for the model applied to the full data ("full crit"), the confidence interval and level for the bias-adjusted CV criterion ("confint"), the number of folds ("k")


###### ICC ----------
# Intraclass Correlation Coefficient
# This function calculates the intraclass-correlation coefficient (ICC) - sometimes also called variance partition coefficient (VPC) or repeatability - for mixed effects models
# The ICC can be interpreted as "the proportion of the variance explained by the grouping structure in the population"
# This index goes from 0, if the grouping conveys no information, to 1, if all observations in a group are identical (Gelman and Hill, 2007, p. 258)
# The ICC can help determine whether a mixed model is even necessary: an ICC of zero (or very close to zero) means the observations within clusters are no more similar than observations from different clusters, and setting it as a random factor might not be necessary
# In simple cases, the ICC corresponds to the difference between the conditional R2 and the marginal R2
performance::icc(mod_glmm, by_group=TRUE)
# 0.01 (i.e., 1%) of variance explained by inter-annual differences
# 0.04 (i.e., 4%) of variance explained by spatial blocks
# This suggests that most variance was explained by predictors (fixed effects) rather than hierarchical grouping structure

###### Interpretation ----------
summary(mod_glmm)

###### R² ----------
# Marginal R2 = variance explained by only the fixed effects
# Conditional R2 = variance explained by both fixed and random effects i.e., the variance explained by the whole model
r2_nakagawa(mod_glmm)

###### Plot effects -------
# Tidy summarizes information about the components of a model
coef_defor = broom.mixed::tidy(mod_glmm, effects = "fixed", conf.int=TRUE) %>%
  dplyr::filter(term != "(Intercept)")
head(coef_defor)

# Forest plot of log-odds effect

x_labels = c(
"in_carInside" = "Inside private property",
"dist_river_m_sc" = "Distance to nearest river",
"dist_urban_m_sc" = "Distance to nearest town",
"dist_road_m_sc" = "Distance to nearest road",
"prec_sum_sc" = "Precipitations",
"tmin_mean_sc" = "Mean min. temperature",
"prop_urb_100m_sc" = "Proportion of built-up area",
"in_apaInside" = "Inside APA",
"slope_pct_sc" = "Slope",
"ns_br101South" = "South of BR-101",
"in_pub_resInside" = "Inside public reserve",
"in_rppnInside" = "Inside RPPN",
"in_rlInside" = "Inside Legal Reserve",
"dist_edge_m_sc" = "Distance to forest edge",
"prop_forest_100m_sc" = "Proportion of forest area"
)

# Rename variables
coef_defor = coef_defor %>%
  dplyr::mutate(term = dplyr::recode(term, !!!x_labels))
# Forest plot of log-odds effect
ggplot(coef_defor, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(x = "Standardized log-odds effect on deforestation probability",
       y = NULL)

# Compute odds ratios (OR)
coef_defor = coef_defor %>%
  dplyr::mutate(OR = exp(estimate),
                OR_low = exp(conf.low),
                OR_high = exp(conf.high))
# Forest plot
ggplot(coef_defor, aes(x = OR, y = reorder(term, OR))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), width = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(x = "Standardized odds ratio on deforestation probability",
       y = NULL)



##### Reforestation (GLMM) ----
str(data_refor_pixel)

###### Quick checks -----
# We check whether the probability is close to 0.5 (we cannot model probability variance under 0.1 and above 0.9)
prop.table(table(data_refor_pixel$type))

# Plots
boxplot(prop_forest_100m_sc~type, data=data_refor_pixel)
boxplot(prop_urb_100m_sc~type, data=data_refor_pixel)
boxplot(dist_river_m_sc~type, data=data_refor_pixel)
boxplot(dist_urban_m_sc~type, data=data_refor_pixel)
boxplot(dist_road_m_sc~type, data=data_refor_pixel)
boxplot(dist_edge_m_sc~type, data=data_refor_pixel)
boxplot(slope_pct_sc~type, data=data_refor_pixel)
boxplot(prec_sum_sc~type, data=data_refor_pixel)
boxplot(tmin_mean_sc~type, data=data_refor_pixel)

###### GLMMs -------

###### Sub-sample  ----
# We sub-sample the dataset for cross-validation
data_refor_pixel = data_refor_pixel %>%
  dplyr::mutate(group = interaction(type, year))
# Stratified sub-sample
split = rsample::initial_split(
  data_refor_pixel,
  prop = 0.8,  # 80% training model
  strata = "group"  # Stratification
)
# Extract training and testing datasets
train_data = training(split)
test_data = testing(split)
# Check equal repartition
prop.table(table(train_data$type))
prop.table(table(test_data$type))
train_data %>% 
  dplyr::group_by(year, type) %>% 
  dplyr::count()
test_data %>% 
  dplyr::group_by(year, type) %>% 
  dplyr::count()

## GLMM with binomial distribution
# We use the logit function to predict values between 0 and 1
# Binomial model
# IF crossed random effects (block and year) : (1|block)+(1|year), that means that every block appears multiple times in every year of the dataset
mod_glmm = glmmTMB(formula = 
                     type ~ 
                     in_car + in_pub_res + in_rppn + in_rl + in_apa + ns_br101 +
                     prop_forest_100m_sc + prop_urb_100m_sc + 
                     dist_river_m_sc + dist_road_m_sc + dist_urban_m_sc + dist_edge_m_sc + 
                     slope_pct_sc + prec_sum_sc + tmin_mean_sc + 
                     (1|year) + (1|sp_block), 
                   REML=T, family=binomial, data=train_data)


###### Validation -----------

### Check convergence
check_convergence(mod_glmm)

# 1) Examine plots of residuals versus fitted values for the entire model
# 2) Model residuals versus all explanatory variables to look for patterns
# 3) For GLMMs: residuals versus fitted values for each grouping level of a random intercept factor

#### Dharma method (from Hartig 2024)
# Rationale: misspecifications in GL(M)Ms cannot reliably be diagnosed with standard residual plots
# The "DHARMa" package uses a simulation-based approach to create readily interpretable scaled (quantile) residuals for fitted generalized linear (mixed) models
# The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression
# Also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial, temporal and phylogenetic autocorrelation.
# A residual of 0 means that all simulated values are larger than the observed value, and a residual of 0.5 means half of the simulated values are larger than the observed value.
# NB: If you have a lot of data points, residual diagnostics will nearly inevitably become significant, because having a perfectly fitting model is very unlikely.
# DHARMa only flags a difference between the observed and expected data - the user has to decide whether this difference is actually a problem for the analysis!

# Calculate the residuals, using the simulateResiduals() function (randomized quantile residuals)
simulationOutput = simulateResiduals(fittedModel = mod_glmm, plot = F)
# Access to residuals
plot(simulationOutput)
# Left panel: qq-plot to detect overall deviations from the expected distribution, by default with added tests for correct distribution (KS test), dispersion and outliers.
# Right panel: plotResiduals (right panel) produces a plot of the residuals against the predicted value (or alternatively, other variable). Simulation outliers (data points that are outside the range of simulated values) are highlighted as red stars.
# To provide a visual aid in detecting deviations from uniformity in y-direction, the plot function calculates an (optional default) quantile regression, which compares the empirical 0.25, 0.5 and 0.75 quantiles in y direction (red solid lines) with the theoretical 0.25, 0.5 and 0.75 quantiles (dashed black line), and provides a p-value for the deviation from the expected quantile
plotQQunif(simulationOutput) # left plot in plot.DHARMa()
plotResiduals(simulationOutput) # right plot in plot.DHARMa()

# GL(M)Ms often display over/underdispersion, which means that residual variance is larger/smaller than expected under the fitted model.
# If overdispersion is present, the main effect is that confidence intervals tend to be too narrow, and p-values to small, leading to inflated type I error. The opposite is true for underdispersion, i.e. the main issue of underdispersion is that you loose power.
# To check for over/underdispersion, plot the simulateResiduals() and check for deviation around the red line (and residuals around 0 and 1); see examples in DHARMa vignette
# DHARMa contains several overdispersion tests that compare the dispersion of simulated residuals to the observed residuals
testDispersion(simulationOutput)
# A significant ratio > 1 indicates overdispersion, a significant ratio < 1 underdispersion.

# A second test that is typically run for LMs, but not for GL(M)Ms is to plot residuals against the predictors in the model (or potentially predictors that were not in the model) to detect possible misspecifications
# If you plot the residuals against predictors, space or time, the resulting plots should not only show no systematic dependency of those residuals on the covariates, but they should also again be flat for each fixed situation
plotResiduals(simulationOutput, train_data$in_car)
plotResiduals(simulationOutput, train_data$in_pub_res)
plotResiduals(simulationOutput, train_data$in_rppn)
plotResiduals(simulationOutput, train_data$in_rl)
plotResiduals(simulationOutput, train_data$in_apa)
plotResiduals(simulationOutput, train_data$ns_br101)
plotResiduals(simulationOutput, train_data$prop_forest_100m_sc)
plotResiduals(simulationOutput, train_data$prop_urb_100m_sc)
plotResiduals(simulationOutput, train_data$dist_river_m_sc)
plotResiduals(simulationOutput, train_data$dist_road_m_sc)
plotResiduals(simulationOutput, train_data$dist_urban_m_sc)
plotResiduals(simulationOutput, train_data$dist_edge_m_sc)
plotResiduals(simulationOutput, train_data$slope_pct_sc)
plotResiduals(simulationOutput, train_data$prec_sum_sc)
plotResiduals(simulationOutput, train_data$tmin_mean_sc)
plotResiduals(simulationOutput, train_data$year)


## Autocorrelation
# Spatial
res2 = recalculateResiduals(simulationOutput, group = train_data$year)
testSpatialAutocorrelation(res2, 
                           x = aggregate(train_data$x, list(train_data$year), mean)$x,
                           y = aggregate(train_data$y, list(train_data$year), mean)$x)
# Temporal
testTemporalAutocorrelation(res2, time = unique(train_data$year))


###### Spatial autocorrelation ----------
# If a distance between residuals can be defined (temporal, spatial, phylogenetic), we need to check if there is a distance-dependence in the residuals
# If autocorrelation is ignored, estimation of variance around predictors is biased = type I error risk (significant effects that are actually not significant)

# What is maximal distance between two points?
set.seed(123)
sample_indices = sample(1:nrow(train_data), 10000) # Sample
coords_sample = cbind(data_refor_pixel$x[sample_indices], data_refor_pixel$y[sample_indices])
distmat = as.matrix(dist(coords_sample))
dim(distmat)
max(distmat)
# Compute max distance to build correlograms (~1/2 to 2/3 total dist)
maxdist = 2/3*max(distmat)
maxdist

## Autocorrelation within the response variable
sample = train_data[sample_indices, ]

correlog.sp = data.frame(dist=seq(from=10000, to=maxdist, by=10000),
                         Morans.i=NA, Null.lcl=NA, Null.ucl=NA, Pvalue=NA)
head(correlog.sp)

# To spatial object
coords_sp_sample = SpatialPoints(coords_sample)

for (i in 1:nrow(correlog.sp)){
  ## Step 1: Neighbor definition
  # First and last values of distance class (for computing Moran's I)
  d.start = correlog.sp[i,"dist"]-10000 # the inferior value equals d-X
  d.end = correlog.sp[i,"dist"] # the superior value equals d
  # List of neighbors
  neigh = dnearneigh(x=coords_sp_sample, d1=d.start, d.end)
  ## Step 2: conversion into weights matrix
  wts = nb2listw(neighbours=neigh, style='W', zero.policy=T)
  ## Step 3: Compute Moran's I for this class of distance
  mor.i = moran.mc(x=sample$type, listw=wts, nsim=99, alternative="greater", zero.policy=T)
  
  # Integrate results into data frame
  correlog.sp[i, "dist"] = (d.end+d.start)/2  # Mean class distance
  correlog.sp[i, "Morans.i"] = mor.i$statistic # Moran I
  correlog.sp[i, "Null.lcl"] = quantile(mor.i$res, probs = 0.025,na.rm = TRUE)  # Confidence Interval (high envelop)
  correlog.sp[i, "Null.ucl"] = quantile(mor.i$res, probs = 0.975,na.rm = TRUE)  # Confidence Interval (low envelop)
  correlog.sp[i, "Pvalue"] = mor.i$p.value	# p-value (of Moran's I)
}

correlog.sp

# Plot the correlogram
plot(y=correlog.sp$Morans.i, x=correlog.sp$dist,
     xlab="Lag Distance(m)", ylab="Moran's I", ylim=c(-0.3,0.3))         
abline(h=0)                                                              
lines(correlog.sp$dist, correlog.sp$Null.lcl,col = "red")	               
lines(correlog.sp$dist, correlog.sp$Null.ucl,col = "red")
# Autocorrelation until 20000 m


## Autocorrelation of GLMM residuals
res_mod1_sample = residuals(mod_glmm, type = "pearson")[sample_indices]
correlog.sp = data.frame(dist=seq(from=10000, to=maxdist, by=10000),
                         Morans.i=NA, Null.lcl=NA, Null.ucl=NA, Pvalue=NA)
head(correlog.sp)

# To spatial object
coords_sp_sample = SpatialPoints(coords_sample)

for (i in 1:nrow(correlog.sp)){
  ## Step 1: Neighbor definition
  # First and last values of distance class (for computing Moran's I)
  d.start = correlog.sp[i,"dist"]-10000 # the inferior value equals d-X
  d.end = correlog.sp[i,"dist"] # the superior value equals d
  # List of neighbors
  neigh = dnearneigh(x=coords_sp_sample, d1=d.start, d.end)
  ## Step 2: conversion into weights matrix
  wts = nb2listw(neighbours=neigh, style='W', zero.policy=T)
  ## Step 3: Compute Moran's I for this class of distance
  mor.i = moran.mc(x=res_mod1_sample, listw=wts, nsim=99, alternative="greater", zero.policy=T)
  
  # Integrate results into data frame
  correlog.sp[i, "dist"] = (d.end+d.start)/2  # Mean class distance
  correlog.sp[i, "Morans.i"] = mor.i$statistic # Moran I
  correlog.sp[i, "Null.lcl"] = quantile(mor.i$res, probs = 0.025,na.rm = TRUE)  # Confidence Interval (high envelop)
  correlog.sp[i, "Null.ucl"] = quantile(mor.i$res, probs = 0.975,na.rm = TRUE)  # Confidence Interval (low envelop)
  correlog.sp[i, "Pvalue"] = mor.i$p.value	# p-value (of Moran's I)
}

correlog.sp

# Plot the correlogram
plot(y=correlog.sp$Morans.i, x=correlog.sp$dist,
     xlab="Lag Distance(m)", ylab="Moran's I", ylim=c(-0.3,0.3))         
abline(h=0)                                                              
lines(correlog.sp$dist, correlog.sp$Null.lcl,col = "red")	               
lines(correlog.sp$dist, correlog.sp$Null.ucl,col = "red")

# There is spatial autocorrelation until 30000 m 
# Adding spatial blocks only partly corrects for spatial autocorrelation (see cor)

###### Simple cross-validation -----------
# ROC is used to measure the performance of models predicting the presence or absence of a phenomenon
# It is often summarised by the area under the curve (AUC) where one indicates a perfect fit and 0.5 indicates a purely random fit.

# Training
pred_train = predict(mod_glmm, type="response", re.form = NA) # we remove random effects
# By removing Random effects, we test whether predictors alone discriminate deforestation
roc_train = roc(train_data$type, pred_train)
auc_train = auc(roc_train)
cat("Train AUC:", auc_train, "\n")

# Testing
pred_test = predict(mod_glmm, newdata=test_data,
                    type="response",
                    re.form = NA)
roc_test = roc(test_data$type, pred_test)
auc_test = auc(roc_test)
cat("Test AUC:", auc_test, "\n")

plot(roc_train, col="blue", lwd=2, main="ROC Curves")
lines(roc_test, col="red", lwd=2)


###### K-fold cross-validation -----------
# Approach to estimating the predictive accuracy of regression models
# Data-division (e.g. 30/70) suffers from two problems: (1) Dividing the data decreases the sample size and thus increases sampling error; and (2), even more disconcertingly, particularly in smaller samples, the results can vary substantially based on the random division of the data
# In CV, the data are randomly divided as equally as possible into several, say k, parts, called “folds.” The statistical model is fit k times, leaving each fold out in turn. Each fitted model is then used to predict the response variable for the cases in the omitted fold.
# A CV criterion or “cost” measure, such as the mean-squared error (“MSE”) of prediction, is then computed using these predicted values
# In the extreme k=n, the number of cases in the data, thus omitting individual cases and refitting the model n times—a procedure termed “leave-one-out (LOO) cross-validation.”
# Estimated prediction error for k-fold CV with k=5 or 10 (commonly employed choices) is more biased than estimated prediction error for LOO CV

# Procedure: 1) Randomly divide a dataset into k groups, or “folds”, of roughly equal size.
# 2) Choose one of the folds to be the holdout set. Fit the model on the remaining k-1 folds. Calculate the test MSE on the observations in the fold that was held out.
# 3) Repeat this process k times, using a different set each time as the holdout set.
# 4) Calculate the overall test MSE to be the average of the k test MSE’s
summary(cv(mod_glmm,
           k = 5,
           clusterVariables = c("sp_block", "year")))
# The cv() methods returns the CV criterion ("CV crit"), the bias- adjusted CV criterion ("adj CV crit"), the criterion for the model applied to the full data ("full crit"), the confidence interval and level for the bias-adjusted CV criterion ("confint"), the number of folds ("k")


###### ICC ----------
# Intraclass Correlation Coefficient
# ICC is the proportion of variation that can be attributed to between-group variation (Nakagawa & Schielzeth 2010)
performance::icc(mod_glmm, by_group=TRUE)
# 0.006 (i.e., <1%) of variance explained by inter-annual differences
# 0.02 (i.e., 2%) of variance explained by spatial blocks
# This suggests that most variance was explained by predictors (fixed effects) rather than hierarchical grouping structure

###### Interpretation ----------
summary(mod_glmm)

###### R² ----------
# Marginal R2 = variance explained by only the fixed effects
# Conditional R2 = variance explained by both fixed and random effects i.e., the variance explained by the whole model
r2_nakagawa(mod_glmm)

###### Plot effects -------
# Tidy summarizes information about the components of a model
coef_refor = broom.mixed::tidy(mod_glmm, effects = "fixed", conf.int=TRUE) %>%
  dplyr::filter(term != "(Intercept)")
head(coef_refor)

# Forest plot of log-odds effect
x_labels = c(
  "in_carInside" = "Inside private property",
  "dist_river_m_sc" = "Distance to nearest river",
  "dist_urban_m_sc" = "Distance to nearest town",
  "dist_road_m_sc" = "Distance to nearest road",
  "prec_sum_sc" = "Precipitations",
  "tmin_mean_sc" = "Mean min. temperature",
  "prop_urb_100m_sc" = "Proportion of built-up area",
  "in_apaInside" = "Inside APA",
  "slope_pct_sc" = "Slope",
  "ns_br101South" = "South of BR-101",
  "in_pub_resInside" = "Inside public reserve",
  "in_rppnInside" = "Inside RPPN",
  "in_rlInside" = "Inside Legal Reserve",
  "dist_edge_m_sc" = "Distance to forest edge",
  "prop_forest_100m_sc" = "Proportion of forest area"
)

# Rename variables
coef_refor = coef_refor %>%
  dplyr::mutate(term = dplyr::recode(term, !!!x_labels))
# Forest plot of log-odds effect
ggplot(coef_refor, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(x = "Standardized log-odds effect on reforestation probability",
       y = NULL)

# Compute odds ratios (OR)
coef_refor = coef_refor %>%
  dplyr::mutate(OR = exp(estimate),
                OR_low = exp(conf.low),
                OR_high = exp(conf.high))
# Forest plot
ggplot(coef_refor, aes(x = OR, y = reorder(term, OR))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), width = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(x = "Standardized odds ratio on reforestation probability",
       y = NULL)

### Property-scale analysis -------------

# Data structure
str(data_car)

# other variables
# Less then 20% of forest in 2024
data_car = data_car %>% 
  dplyr::mutate(Less20For2024 = dplyr::case_when(prop_forest_2024 <= 0.20 ~ 1, TRUE ~ 0),
                Less20For1989 = dplyr::case_when(prop_forest_1989 <= 0.20 ~ 1, TRUE ~ 0))
data_car %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(c(prop_forest_2024, Less20For2024)) %>% 
  head()
# Proportion of properties with less then 20% of forest
data_car %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(Less20For1989) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))
data_car %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(Less20For2024) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/sum(n))

#### Duplicates ----
data_car = data_car %>%
  dplyr::distinct(centroid_x, centroid_y, .keep_all = TRUE)

#### NAs --------
# Number of NAs
sum(is.na(data_car))
na = data_car %>% 
  dplyr::summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  sf::st_drop_geometry()
data_car = na.omit(data_car)

#### Number of data ------
cat("Number of properties (without NAs):", length(data_car$car_id), "\n")

#### Number of 0 ----------
##### Count -------
data_car %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(area_deforest_ha == 0) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/5271)
data_car %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(area_reforest_ha == 0) %>% 
  dplyr::summarise(n=dplyr::n()) %>% 
  dplyr::mutate(prop = n*100/5271)

##### Remove 0 ------
# Deforested area
data_car_defor_without0 = data_car %>% 
  dplyr::filter(area_deforest_ha > 0) 
# Reforested area
data_car_refor_without0 = data_car %>% 
  dplyr::filter(area_reforest_ha > 0) 

#### Best radius --------
### Deforestation
data_car_defor_without0 %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(c(area_deforest_ha, dplyr::contains("buf"))) %>%
  corrr::correlate() %>%
  focus(area_deforest_ha) %>% 
  dplyr::arrange(desc(area_deforest_ha)) %>% 
  print(n=60)
# The most correlated variables are within 100 m

### Reforestation
data_car_refor_without0 %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(c(area_reforest_ha, dplyr::contains("buf"))) %>%
  corrr::correlate() %>%
  focus(area_reforest_ha) %>% 
  dplyr::arrange(desc(area_reforest_ha)) %>% 
  print(n=60)
# The most correlated variables are within 100 m

#### Random Forest --------

##### With VSURF ----------
###### Example ---------
# The following code is from Genuer et al. 2015, The R Journal

# ### Example on a data frame (ozone data set)
# data("Ozone", package = "mlbench")
# vozone <- VSURF(V4 ~ ., data = Ozone, na.action = na.omit) # V4 is the dependent variable
# summary(vozone)
# plot(vozone, step = "thres", imp.sd = FALSE, var.names = TRUE) # variable importance associated with each of the explanatory variables.
# # three very sensible groups of variables can be discerned ranging from the most to the least important
# number <- c(1:3, 5:13) # reorder the output variables of the procedure
# # To know which index is which variable, refer to the Ozone dataset: variable index "1" is the first column (V1, the month)
# number[vozone$varselect.thres] # the 3 variables of negative importance (variables 6, 3 and 2) are eliminated (not in the list)
# number[vozone$varselect.interp] # the interpretation procedure leads to select the model with 5 variables, which contains all of the most important variables
# number[vozone$varselect.pred] # the prediction step does not remove any additional variable.

###### On my dataset ---------

### Deforestation
subset = data_car_defor_without0 %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(c(area_deforest_ha,
                  car_area_ha,
                  area_forest_1989_ha, prop_forest_1989,
                  area_agri_1989_ha, prop_agri_1989,
                  area_urb_1989_ha, prop_urb_1989,
                  area_forest_buf100_1989_ha, area_forest_buf100_2024_ha, prop_forest_buf100_1989, prop_forest_buf100_2024, evol_forest_buf100_pct,
                  area_agri_buf100_1989_ha, area_agri_buf100_2024_ha, prop_agri_buf100_1989, prop_agri_buf100_2024, evol_agri_buf100_pct,
                  area_urb_buf100_1989_ha, area_urb_buf100_2024_ha, prop_urb_buf100_1989, prop_urb_buf100_2024, evol_urb_buf100_pct,
                  area_refor_buf100_2024_ha, prop_refor_buf100_2024,
                  area_defor_buf100_2024_ha, prop_defor_buf100_2024,
                  dist_to_urban_m, dist_to_road_m, dist_to_river_m, 
                  intersects_river, intersects_apa, intersects_pub_res, intersects_rppn, intersects_rl,
                  apa_cover_ha, pub_res_cover_ha, rppn_cover_ha, rl_cover_ha,
                  prop_apa_cover, prop_pub_res_cover, prop_rppn_cover, prop_rl_cover,
                  slope_mean, slope_sd, slope_cv,
                  alt_mean, alt_sd, alt_cv,
                  prec_2024_mean, prec_2024_sd, prec_2024_cv,
                  tmin_2024_mean, tmin_2024_sd, tmin_2024_cv,
                  tmax_2024_mean, tmax_2024_sd, tmax_2024_cv,
                  ns_br101,
                  Less20For1989, Less20For2024,
                  centroid_x, centroid_y))

rf = VSURF(area_deforest_ha ~ ., data = subset, parallel = TRUE) # Can be long
# Step 1: Preliminary elimination and ranking
# Step 2: Variable selection (for interpretation and for prediction)

summary(rf)
plot(rf, step = "thres") # variable importance associated with each of the explanatory variables.
order = c(2:63) # reorder the output variables of the procedure (the index (e.g., 9) is the same index in the original dataframe) 
order[rf$varselect.thres] # the variables of negative importance are eliminated (not in the list)
order[rf$varselect.interp] # the interpretation procedure leads to select the model with X variables, which contains all of the most important variables
order[rf$varselect.pred] # the prediction step
colnames(subset)[order[rf$varselect.thres]]
colnames(subset)[order[rf$varselect.interp]]
colnames(subset)[order[rf$varselect.pred]]

###### Subset based on RF --------
selected_vars = colnames(subset)[order[rf$varselect.interp]]
selected_vars = c("car_id","area_deforest_ha", "centroid_x", "centroid_y", selected_vars) # Add other columns
data_car_defor_rf = data_car_defor_without0 %>% 
  dplyr::select(dplyr::all_of(selected_vars))

##### With Boruta ---------

set.seed(1)
# Setting doTrace argument to 1 or 2 makes Boruta report the progress of the process; version 2 is a little more verbose, namely it shows attribute decisions as soon as they are cleared
Boruta.defor = Boruta(area_deforest_ha ~ ., data = subset, doTrace = 2, ntree = 500)
Boruta.defor # Result
plot(Boruta.defor) # Z scores variability among attributes during the Boruta run
getConfirmedFormula(Boruta.defor) # formula object that defines a model based only on confirmed attributes
attStats(Boruta.defor) # creates a data frame containing each attribute’s Z score statistics and the fraction of random forest runs in which this attribute was more important than the most important shadow one

#### Correlation and VIF ---------
###### Pearson =====

X = data_car_defor_rf %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-c(area_deforest_ha,car_id))
cor_mat = cor(X, use = "pairwise.complete.obs", method = "pearson")
cor_mat

# Identify strong correlations
high_corr = cor_mat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(-var1,names_to = "var2",values_to = "r") %>%
  dplyr::filter(var1 < var2, abs(r) > 0.6) %>%
  dplyr::arrange(desc(abs(r)))

print(high_corr, n=100)
# Beware:
# areas of forest and car area

###### VIF ---------
X_vif = data_car_defor_rf %>%
  sf::st_drop_geometry() %>% 
  as.data.frame()
vif.result = HH::vif(X_vif, y.name="area_deforest_ha")

# VIF(i) = 1/(1-Ri²) (where Ri² is the R² gotten from the regression of predictor i regarding other predictors)
# If Ri² is close to 1, it means that the variable i is well explained by the linear combination of other variables ; hence the variable i is redundant in the model 
# The more Ri² is close to 1, the more VIF increases (+Inf)
# Thus, the higher the VIF, the stronger the collinearity of i is with other variables (ie the information of i is already contained by others)
# Check VIF > 2.5
vif.result[vif.result > 2.5]
# The results confirm strong correlations between: 
# car area and forest area

###### Variable selection ----
# We select variables based on their correlations with the response variable
data_car_defor_rf %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(-car_id) %>% 
  corrr::correlate() %>% 
  focus(area_deforest_ha) %>% 
  dplyr::arrange(desc(area_deforest_ha))
# We retain:
# car_area_ha 

###### Re-run VIF ------
X_vif = data_car_defor_rf %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(area_deforest_ha, 
                car_area_ha,
                area_defor_buf100_2024_ha) %>% 
  as.data.frame()
vif.result = HH::vif(X_vif, y.name="area_deforest_ha")
vif.result[vif.result > 2.5]

###### Subset -------
data_car_defor_rf = data_car_defor_rf %>% 
  dplyr::select(c(car_id,
                  area_deforest_ha, 
                  car_area_ha,
                  area_defor_buf100_2024_ha,
                  centroid_x,
                  centroid_y))

#### Summary ----------
##### Outliers --------
# Cleveland plot allow the detection of outliers
dotplot(as.matrix(data_car_defor_rf %>% sf::st_drop_geometry()), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")


# Grubbs test
grubbs.test(data_car_defor_rf$car_area_ha)

##### Transform variables --------
# Count 0
data_car_defor_rf %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(where(is.numeric)) %>%
  dplyr::summarise(across(everything(), ~ sum(.x == 0, na.rm = TRUE)))
# Log-transform
data_car_defor_rf_clean = data_car_defor_rf %>% 
  dplyr::mutate(car_area_log = log(car_area_ha),
                area_deforest_log = log(area_deforest_ha),
                area_defor_buf100_2024_log = log(area_defor_buf100_2024_ha+ 0.0001))
# Dotplot
dotplot(as.matrix(data_car_defor_rf_clean %>% sf::st_drop_geometry()), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")


##### Distribution --------
# Loop to plot hists
numeric_cols = data_car_defor_rf_clean %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(where(is.numeric)) %>% 
  names()
plots = list()
for (col in numeric_cols) {
  p = ggplot(data_car_defor_rf_clean, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme_minimal()
  
  # Ajouter le graphique à la liste
  plots[[col]] = p
}
combined_plot = do.call(wrap_plots, c(plots, ncol = 3))
print(combined_plot)

##### Spatial blocking --------
# Deforestation dataset
data_car_defor_rf_clean$block_x = floor(data_car_defor_rf_clean$centroid_x / 10000) # Distance in meters
data_car_defor_rf_clean$block_y = floor(data_car_defor_rf_clean$centroid_y / 10000) # Distance in meters
data_car_defor_rf_clean = data_car_defor_rf_clean %>%
  dplyr::mutate(sp_block = paste0(block_x, "_", block_y))
data_car_defor_rf_clean$sp_block = factor(data_car_defor_rf_clean$sp_block)
ggplot(data_car_defor_rf_clean, aes(x = centroid_x, y = centroid_y, color = sp_block)) +
  geom_point(size = 0.3) +
  theme_minimal() +
  guides(color = "none")

##### Groups of property size -------
BAMMtools::getJenksBreaks(data_car_defor_rf_clean$car_area_ha, k = 4)
data_car_defor_rf_clean = data_car_defor_rf_clean %>% 
  dplyr::mutate(car_area_grp = dplyr::case_when(car_area_ha <= 10 ~ "<10",
                                                car_area_ha > 10 & car_area_ha <= 20 ~ "10-20",
                                                car_area_ha > 20 & car_area_ha <= 50 ~ "20-50",
                                                car_area_ha > 50 ~ ">50",
                                                TRUE ~ NA))
data_car_defor_rf_clean$car_area_grp = factor(data_car_defor_rf_clean$car_area_grp,
                                              levels=c("<10","10-20","20-50",">50"))
data_car_defor_rf_clean %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(car_area_grp) %>% 
  dplyr::count()

##### Y~X ----------
plot(area_deforest_log~car_area_log, data=data_car_defor_rf_clean)

plot(area_deforest_log~area_defor_buf100_2024_log, data=data_car_defor_rf_clean)

boxplot(area_deforest_log~car_area_grp, data=data_car_defor_rf_clean)

##### Coplots ----------
# See : Zuur & Ieno 2016, MEE
# coplot is an excellent graphical tool to visualize the potential presence of interactions.
M1 = lm(area_deforest_log ~ car_area_log*area_defor_buf100_2024_log, 
        data=data_car_defor_rf_clean)
summary(M1)
anova(M1)

# Make the coplot
# A bivariate linear regression line is added to each scatterplot
# if all lines are parallel, then there is probably no significant interaction
coplot(area_deforest_log ~ car_area_log | area_defor_buf100_2024_log, 
       data=data_car_defor_rf_clean,
       ylab = "car_area_ha",
       xlab = "area_defor_buf100_2024_ha",
       panel = function(x, y, ...) {
         tmp = lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

##### Remove very low values --------
# Homogeneity of variance was not validated due to the presence of outliers
data_car_defor_final = data_car_defor_rf_clean %>% 
  dplyr::filter(area_deforest_ha > 0.1) %>% 
  dplyr::filter(area_defor_buf100_2024_ha > 0.1)

#### Modeling -------

###### Sub-sample  ----
# Sub-sample
split = rsample::initial_split(
  data_car_defor_final,
  prop = 0.8 # 80% training model
)
# Extract training and testing datasets
train_data = training(split)
test_data = testing(split)

# Put id as rownames
rownames(train_data) = train_data$car_id
rownames(test_data) = test_data$car_id

##### GLMM -------
mod_glmm = glmmTMB(area_deforest_log ~ 
                     car_area_log + area_defor_buf100_2024_log +
                     car_area_log:area_defor_buf100_2024_log +
                     (1|sp_block),
                   data=train_data)
summary(mod_glmm)

###### Validation -----------

### Check convergence
check_convergence(mod_glmm)

# 1) Examine plots of residuals versus fitted values for the entire model (to check homogeneity of variance)
# Zuur & Ieno (210) : In all these graphs the residual variation should be similar. The solution to heterogeneity of variance is either a transformation of the response variable to stabilize the variance, or applying statistical techniques that do not require homogeneity
# 2) Model residuals versus all explanatory variables to look for patterns
# 3) For GLMMs: residuals versus fitted values for each grouping level of a random intercept factor

#### Dharma method (from Hartig 2024)
# Rationale: misspecifications in GL(M)Ms cannot reliably be diagnosed with standard residual plots
# The "DHARMa" package uses a simulation-based approach to create readily interpretable scaled (quantile) residuals for fitted generalized linear (mixed) models
# The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression
# Also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial, temporal and phylogenetic autocorrelation.
# A residual of 0 means that all simulated values are larger than the observed value, and a residual of 0.5 means half of the simulated values are larger than the observed value.
# NB: If you have a lot of data points, residual diagnostics will nearly inevitably become significant, because having a perfectly fitting model is very unlikely.
# DHARMa only flags a difference between the observed and expected data - the user has to decide whether this difference is actually a problem for the analysis!

# Calculate the residuals, using the simulateResiduals() function (randomized quantile residuals)
simulationOutput = simulateResiduals(fittedModel = mod_glmm, plot = F)
# Access to residuals
plot(simulationOutput)
# Left panel: qq-plot to detect overall deviations from the expected distribution, by default with added tests for correct distribution (KS test), dispersion and outliers.
# Right panel: plotResiduals (right panel) produces a plot of the residuals against the predicted value (or alternatively, other variable). Simulation outliers (data points that are outside the range of simulated values) are highlighted as red stars.
# To provide a visual aid in detecting deviations from uniformity in y-direction, the plot function calculates an (optional default) quantile regression, which compares the empirical 0.25, 0.5 and 0.75 quantiles in y direction (red solid lines) with the theoretical 0.25, 0.5 and 0.75 quantiles (dashed black line), and provides a p-value for the deviation from the expected quantile
plotQQunif(simulationOutput) # left plot in plot.DHARMa()
plotResiduals(simulationOutput) # right plot in plot.DHARMa()

# GL(M)Ms often display over/underdispersion, which means that residual variance is larger/smaller than expected under the fitted model.
# If overdispersion is present, the main effect is that confidence intervals tend to be too narrow, and p-values to small, leading to inflated type I error. The opposite is true for underdispersion, i.e. the main issue of underdispersion is that you loose power.
# To check for over/underdispersion, plot the simulateResiduals() and check for deviation around the red line (and residuals around 0 and 1); see examples in DHARMa vignette
# DHARMa contains several overdispersion tests that compare the dispersion of simulated residuals to the observed residuals
testDispersion(simulationOutput)
# A significant ratio > 1 indicates overdispersion, a significant ratio < 1 underdispersion.

# A second test that is typically run for LMs, but not for GL(M)Ms is to plot residuals against the predictors in the model (or potentially predictors that were not in the model) to detect possible misspecifications
# If you plot the residuals against predictors, space or time, the resulting plots should not only show no systematic dependency of those residuals on the covariates, but they should also again be flat for each fixed situation
plotResiduals(simulationOutput, train_data$car_area_ha)
## Autocorrelation
# Spatial
testSpatialAutocorrelation(simulationOutput,
                           x = train_data$centroid_x,
                           y = train_data$centroid_y)

##### SAR -------
# Spatial autoregressive models are models that account for spatial autocorrelation among observations (i.e., the response variable is not randomly distributed in space)
# Y = AX + W(Y) + B
# Sources: 
# https://bookdown.org/hhwagner1/LandGenCourse_book/WE_7.html#WE_7
# https://r-spatial.org/python/17-Econometrics.html
# Méthodes de régression spatiale : un grand bol d’R (authors: Philippe Apparicio Jérémy Gelb Jean Dubé Joan Carles Martori)

# We first need to define neighbors
# We define weights in three ways
# listw.gab: 1 = neighbour, 0 = not a neighbour.
# listw.d1: inverse distance weights: neighbour j with weight 1/dij
# listw.d2: inverse squared distance weights: neighbour j with weight 1/dij^2
# listw.d3: k neighbors

# Binary neighborhood
mat = train_data %>% sf::st_drop_geometry()
xy = data.matrix(mat[,c("centroid_x", "centroid_y")])
# ‘sym=TRUE’ means that if A is a neighbour of B, B is also a neighbour of A
nb.gab = spdep::graph2nb(spdep::gabrielneigh(xy), sym=TRUE)
listw.gab = spdep::nb2listw(nb.gab)
plot(st_geometry(train_data), border = "lightgray")
plot.nb(nb.gab, xy, add = TRUE)

# Inverse distances
dlist = spdep::nbdists(nb.gab, xy)
dlist = lapply(dlist, function(x) 1/x)
listw.d1 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)
dlist = lapply(dlist, function(x) 1/x^2)
listw.d2 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)

# Fixed number of neighbors
col.knn.nb = knn2nb(knearneigh(sf::st_centroid(train_data), k = 10)) # k is the nuùber of neighbors
listw.knn = nb2listw(col.knn.nb, style = "W") # to matrix of weights
plot(st_geometry(train_data), border = "lightgray")
plot.nb(col.knn.nb, xy, add = TRUE)

# Observe spatial autocorrelation
spdep::moran.test(train_data$area_deforest_log, listw.gab)

## SAR
# In trying to model spatial processes, one of the earliest spatial econometric representations is to model the spatial autocorrelation in the residual (spatial error model, SEM)
# Where Y = XB + u, with X is a covariate, B a vector of parameters, u is an spatially autocorrelated disturbance vector
# A model with a spatial process in the response only is termed a spatial lag model (SLM, often SAR - spatial autoregressive)
# SEM spatial error called with errorsarlm(..., Durbin=FALSE) function
# The method errorsarlm fits a simultaneous autoregressive model (‘sar’) to the error (‘error’) term of a ‘lm’ model
# SLM	spatial lag called with lagsarlm(..., Durbin=FALSE) function
# The estimating functions errorsarlm() and lagsarlm() take similar arguments, where the first two, formula and data are shared by most model estimating functions. The third argument is a listw spatial weights object, while na.action behaves as in other model estimating functions if the spatial weights can reasonably be subsetted to avoid observations with missing values. The weights argument may be used to provide weights indicating the known degree of per-observation variability in the variance term - this is not available for lagsarlm().

mod_sar1 = spatialreg::errorsarlm(area_deforest_log ~ 
                                  car_area_log + area_defor_buf100_2024_log +
                                  car_area_log:area_defor_buf100_2024_log, 
                                  listw=listw.gab,
                                 data=train_data)
mod_sar2 = spatialreg::errorsarlm(area_deforest_log ~ 
                                  car_area_log + area_defor_buf100_2024_log +
                                  car_area_log:area_defor_buf100_2024_log, 
                                  listw=listw.d1,
                                data=train_data)
mod_sar3 = spatialreg::errorsarlm(area_deforest_log ~ 
                                  car_area_log + area_defor_buf100_2024_log +
                                  car_area_log:area_defor_buf100_2024_log, 
                                  listw=listw.d2,
                                data=train_data)
mod_sar4 = spatialreg::errorsarlm(area_deforest_log ~ 
                                  car_area_log + area_defor_buf100_2024_log +
                                  car_area_log:area_defor_buf100_2024_log, 
                                  listw=listw.knn,
                                data=train_data)

# Comparison (AICc)
Models = list(mod_sar1=mod_sar1, 
              mod_sar2=mod_sar2,
              mod_sar3=mod_sar3,
              mod_sar4=mod_sar4)
data.frame(AICc = sapply(Models, MuMIn::AICc)) %>% 
  dplyr::mutate(delta = AICc - min(AICc)) %>%
  dplyr::arrange(delta)

# Interpretation
# The ‘coefficients’ section in the output may be interpreted in a similar way to a standard regression model
summary(mod_sar2, Nagelkerke = TRUE) # With the argument Nagelkerke = TRUE, we request a pseudo R-squared
# The `lambda’ section provides a p-value for the null hypothesis that λ=0 - that is, that there is a degree of spatial autocorrelation in the response variable. 
# If p<0.05, we reject the null hypothesis (i.e., there is spatial autocorrelation)

# There is a positive effect of the log-transformed property size on the log-transformed deforested area (p<0.05)
# There is a significant positive interaction between the log-transformed property size and the log-transformed deforested area around the property (p<0.05)
# Nagelkerke pseudo-R-squared: 0.50505 
# Spatial autocorrelation of residuals
moran.mc(resid(mod_sar2), listw.d1, nsim = 999)
# Residuals are no longer spatially autocorrelated!

###### Validation -------
# Residuals VS adjusted
residuals = residuals(mod_sar2)
fitted_values = fitted(mod_sar2)
ggplot(data.frame(fitted = fitted_values, resid = residuals), aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Standardized Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# Hist of residuals
hist(residuals(mod_sar2))
shapiro.test(residuals(mod_sar2))

# QQplot
# Extract residuals from the model
residuals = residuals(mod_sar2)
qqplot_data = data.frame(res = residuals)
ggplot(qqplot_data, aes(sample = res)) +
  stat_qq(distribution = qnorm) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "QQ Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Residuals vs. each explanatory variable
residuals = residuals(mod_sar2)
ggplot(train_data, aes(x = area_defor_buf100_2024_log, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()
ggplot(train_data, aes(x = car_area_log, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()

###### Cross-validation -----------

# Binary neighborhood
mat = data_car_defor_final %>% sf::st_drop_geometry()
xy = data.matrix(mat[,c("centroid_x", "centroid_y")])
nb.gab = spdep::graph2nb(spdep::gabrielneigh(xy), sym=TRUE)
plot(nb.gab, xy)
listw.gab = spdep::nb2listw(nb.gab)

# Inverse distances
dlist = spdep::nbdists(nb.gab, xy)
dlist = lapply(dlist, function(x) 1/x)
listw.d1 = spdep::nb2listw(nb.gab, style = "W", glist=dlist)

# Testing
# See https://r-spatial.github.io/spatialreg/reference/predict.sarlm.html
# In the out-of-sample prediction case (ie. if newdata is not NULL), if legacy.mixed=FALSE or if pred.type!="TS", it should include both in-sample and out-of-sample spatial units
pred_test = predict(mod_sar2, 
                     newdata = test_data,
                     listw = listw.d1,
                    pred.type = "TS")
pred_test_exp = exp(pred_test) # Back transformation

# RMSE and MAE
data.frame(
  RMSE = RMSE(pred_test_exp, test_data$area_deforest_log),
  MAE = MAE(pred_test_exp, test_data$area_deforest_log)
)
# RMSE is the square root of MSE, bringing the metric back to the same units as the target variable. It provides an easily interpretable measure of average error size
# RMSE values closer to 0 indicate better model performance. RMSE’s interpretation is straightforward since it’s in the same units as the target variable
# Read: RMSE is ±7.961294 ha
# MAE: MAE measures the average magnitude of errors in predictions, without considering their direction. It’s calculated as the average of absolute differences between predicted and actual values.
# Read: MAE is 3.047084 ha