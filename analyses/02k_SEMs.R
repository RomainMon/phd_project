#------------------------------------------------#
# Author: Romain Monassier
# Objective: Structural Equation Models (SEMs)
#------------------------------------------------#

### Packages -----
library(here)
library(readr)
library(piecewiseSEM)
library(dplyr)
library(HH)
library(semEff)
library(gsl)
library(ggplot2)

# GSL
# install.packages(
#   "gsl",
#   repos = c(
#     "https://robinhankin.r-universe.dev",
#     "https://cloud.r-project.org"
#   )
# )

### Dataset ----
data = readr::read_delim(
  here::here("outputs", "data", "glt", "repro_groups.csv"),
  delim=",")

### Data management ----
#### Filter -----
data = data %>% 
  dplyr::filter(n_repro_females > 0) %>% 
  dplyr::select(-season_assumed)

#### Variables -----
data = data %>% 
  # Mutate the mating system as a categorical variable
  dplyr::mutate(ms = dplyr::case_when(
    n_repro_females == 1 ~ "Monogamy",
    n_repro_females > 1 ~ "Polygamy",
    TRUE ~ NA
  ),
  # Sex-bias in the group
  bias = dplyr::case_when(n_males > n_females ~ "male-biased",
                          n_males < n_females ~ "female-biased",
                          n_males == n_females ~ "balanced")) %>% 
  # Mutate a binary MS variable with 0 = monogamy, 1 = polygamy
  dplyr::mutate(ms_binary = dplyr::case_when(
    n_repro_females == 1 ~ 0,
    n_repro_females > 1 ~ 1
  ))
poly = dplyr::filter(data, ms == "Polygamy")

### Data exploration -----
str(data)

#### 1. Where and when does polygamy occur? ------
dplyr::count(data, ms) # cases per mating system
dplyr::n_distinct(poly$Group) # distinct groups in polygamy
poly %>% dplyr::count(Group, name = "n_seasons") %>% dplyr::arrange(dplyr::desc(n_seasons))
poly %>% dplyr::count(Season_start) # years (start of the season)
poly %>% dplyr::count(UMMPs, name = "n_polygamy") %>% # polygamy by UMMPs, vs total seasons
  dplyr::left_join(dplyr::count(data, UMMPs, name = "n_total"), by = "UMMPs") %>%
  dplyr::mutate(prop_polygamy = round(n_polygamy / n_total, 2))

#### 2. Monogamy vs polygamy ------
vars = c("age_mating_dom", "GS_total", "n_adults", "sex_ratio", "adult_turnover",
         "n_immigrants", "n_emigrants", "n_floaters", "n_glt_patch", "n_groups_patch", "lambda",
         "enn_m", "area_km2", "dPC", "shape")

data %>%
  dplyr::select(ms, dplyr::all_of(vars)) %>%
  tidyr::pivot_longer(-ms, names_to = "variable", values_to = "value") %>%
  dplyr::filter(!is.na(value), ms %in% c("Monogamy", "Polygamy")) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    n_mono = sum(ms == "Monogamy"), 
    n_poly = sum(ms == "Polygamy"),
    mean_mono = round(mean(value[ms == "Monogamy"]), 2),
    mean_poly = round(mean(value[ms == "Polygamy"]), 2),
    sd_mono = round(sd(value[ms == "Monogamy"]), 2),
    sd_poly = round(sd(value[ms == "Polygamy"]), 2),
    # Wilcoxon rank-sum test (NA if a group has too few values)
    p_wilcox = tryCatch(round(wilcox.test(value ~ ms, exact = FALSE)$p.value, 4),
                        error = function(e) NA_real_),
    .groups = "drop") %>%
  dplyr::arrange(match(variable, vars)) %>% 
  dplyr::arrange(p_wilcox)

# Sex-biased groups (adults): counts and proportions
data %>% dplyr::filter(!is.na(bias)) %>%
  dplyr::count(ms, bias) %>%
  dplyr::group_by(ms) %>%
  dplyr::mutate(prop = round(n / sum(n), 2)) %>%
  dplyr::ungroup()

#### 3. Profile of the UMMPs ------
# Patch metrics vary by year: mean (and range) per UMMPs over the seasons of the dataset
data %>%
  dplyr::group_by(UMMPs) %>%
  dplyr::summarise(n_seasons = dplyr::n(),
                   n_groups = dplyr::n_distinct(Group),
                   dplyr::across(c(area_km2, enn_m, dPC, shape),
                                 list(mean = ~ round(mean(.x, na.rm = TRUE), 3),
                                      min = ~ round(min(.x, na.rm = TRUE), 3),
                                      max = ~ round(max(.x, na.rm = TRUE), 3))),
                   .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(area_km2_mean)) %>%
  print(n = Inf, width = Inf)

#### 4. Correlations and VIF ----
### Pearson
str(data)
X = data %>% 
  dplyr::select(n_repro_females,
                age_mating_dom, 
                GS_total, n_adults, n_immatures, n_males, n_females, sex_ratio,
                adult_turnover, 
                n_immigrants, n_immigrants_F, n_immigrants_M,
                n_emigrants, n_emigrants_F, n_emigrants_M,
                n_floaters, n_floaters_F, n_floaters_M,
                area_km2, shape, enn_m, dPC,
                n_glt_patch, n_groups_patch,
                lambda,
                n_days_supp_food)
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

### VIF
X_vif = data %>% 
  dplyr::select(n_repro_females,
                age_mating_dom, 
                GS_total, 
                sex_ratio,
                adult_turnover, 
                n_immigrants,
                n_emigrants, 
                n_floaters,
                area_km2, 
                enn_m,
                n_glt_patch,
                lambda,
                n_days_supp_food) %>%
  as.data.frame()
vif.result = HH::vif(X_vif, y.name="n_repro_females")

# VIF(i) = 1/(1-Ri²) (where Ri² is the R² gotten from the regression of predictor i regarding other predictors)
# If Ri² is close to 1, it means that the variable i is well explained by the linear combination of other variables ; hence the variable i is redundant in the model 
# The more Ri² is close to 1, the more VIF increases (+Inf)
# Thus, the higher the VIF, the stronger the collinearity of i is with other variables (ie the information of i is already contained by others)
# Check VIF > 2.5
vif.result[vif.result > 2.5]

### SEMs ----
# Documentation:
# Lefcheck (2021) https://jslefche.github.io/sem_book/index.html
# Lefcheck, piecewiseSEM: Piecewise structural equation modelling in r for ecology, evolution, and systematics. Methods in Ecology and Evolution 7, 573–579 (2016).

#### Isolation + Nb Floaters ----
# Data
subset = data %>% 
  dplyr::select(c(
    ms_binary,
    n_floaters,
    enn_m
  ))

# Distributions
hist(subset$enn_m)
hist(subset$n_floaters)

# Correlations
cor_mat = cor(subset, use = "pairwise.complete.obs", method = "pearson")
cor_mat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(-var1,names_to = "var2",values_to = "r") %>%
  dplyr::filter(var1 < var2) %>%
  dplyr::arrange(desc(abs(r)))

##### Path diagram -----
# Path diagram tested here:
# Patch isolation (X1) -> N Floaters (Y1) -> mating system (Y2)

##### SEM -----
# Classical SEMs are based on variance-covariance matrices with max. likelihood estimation (searches parameters to minimize differences)
# Local estimation (piecewise SEM): we fit a linear model for each response and then string together the inferences
# Hence: a separate variance-covariance matrix for each response
mod1 = psem(
  glm(n_floaters ~ enn_m, data = subset, family = poisson),
  glm(ms_binary ~ n_floaters, data = subset, family = binomial),
  data = subset
  )

##### Basis set -----
# the basis set is the min nb of independence claims derived from a path diagram 
# i.e., paths NOT TESTED
# e.g., x1 -> y1 -> y2 gives 1 independence claim (with variable between () being the conditioning variables)
# x1|y2 (y1)
basisSet(mod1)
# Here we have:
# ENN | MS (floaters)

##### Test of directed separation -----
# Evaluation of conditional independence claims to be used in determining the goodness-of-fit for piecewise SEMs
# The test of directed separation tests whether path coefficients are different from 0 and we are justified to exclude them
# We expect P>0.05, i.e., we were justified in excluding their relationships
dSep(mod1)

# p-values of the test of directed separation can be summed with Fisher's C
# If P>0.05: GOOD
fisherC(mod1)

##### Log-likelihood X² -----
# The larger function of max. likelihood, the larger the discrepancy between the model with ALL paths and observed model = poor fit
# It should be 0
# Chisq test: if P>0.05 = GOOD FIT
# assumption of the test = covariance between two variables is 0, i.e., they are unrelated
# IF they are: deviations, and the function of max. likelihood increases
# WARNING: the magnitude of P does not reflect increasing fit
LLchisq(mod1)

##### AIC ----
AIC_psem(mod1)

##### Summary -----
summary(mod1)

##### Model coefficients ----
# Scale-standardized coefficients
coefs(mod1)$Std.Estimate
# Range-standardized coefficients
coefs(mod1, standardize = "range")$Std.Estimate

# For binomial response models
# Coeffs are in units of logits (log odd ratios)
# The log of the proba. of observing an outcome (1, here: polygamy) relative to the proba of not observing it
# if B = -0.12, exp(-0.12) = 0.88 -> 88% reduction in the odds of observing 1
# Two approaches to compute model coefficients: latent theoretic and observation-empirical
coefs(mod1, standardize.type = "latent.linear")
coefs(mod1, standardize.type = "Menard.OE")

###### semEff -----
# To calculate effects, we first bootstrap and save the standardised direct effects:
system.time(
  mod.boot <- semEff::bootEff(mod1, R = 100, seed = 13, parallel = "no")
)

# Using the bootstrapped estimates we can now calculate direct, indirect, and total effects:
(mod1.sem.eff <- semEff::semEff(mod.boot))

# effects for the final response variable
summary(mod1.sem.eff, response = "ms.binary")
# The effect of the number of floaters is 0.05 [CIs: 0.029;0.090]
# mediator effects listed here are the sum of all the indirect paths from all predictors which involve each mediator, and can be useful as a rough indicator of the overall importance and net direction of effect of each

# To focus only on an exogenous variable, let’s recalculate:
summary(
  semEff::semEff(mod.boot, predictor = "n.floaters"),
  response = "ms.binary"
)
# All effects presented here are standardised unique effects (i.e. adjusted for multicollinearity, a.k.a. semipartial correlations), which is the only way to fully partition effects in the system. 
# These will usually be a bit smaller than the unadjusted standardised coefficients, since most variables are correlated to some degree
# If there are large differences between the two, consideration should be given to the impact (and relevance) of multicollinearity in the system.

##### Plot -----
plot(mod1, show="std", digits=3) # Standardized coeffs
plot(mod1, show="unstd") # Unstandardized coeffs
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod1),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)
