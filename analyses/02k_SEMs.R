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
library(DiagrammeR)
library(ggplot2)
library(ggeffects)
library(lme4)
library(MuMIn)
library(performance)
library(glmmTMB)
library(nlme)
library(glmm.hp)
library(lubridate)
# Sys.unsetenv("GITHUB_PAT")
# Sys.unsetenv("GITHUB_TOKEN")
# devtools::install_github("XiyuanEM-DrSheng/thresholdanalysisV1.0.1", force=TRUE)
library(thresholdanalysis)
library(glmertree)
library(segmented)
library(mgcv) # GAMs

# GSL
# install.packages(
#   "gsl",
#   repos = c(
#     "https://robinhankin.r-universe.dev",
#     "https://cloud.r-project.org"
#   )
# )

### Dataset ----
repro_groups = readr::read_delim(
  here::here("outputs", "data", "glt", "repro_groups.csv"),
  delim=",")

### Data management ----
#### Filter -----
data = repro_groups %>% 
  dplyr::filter(n_repro_females > 0) %>% 
  dplyr::select(-season_assumed)

#### Variables -----
data = data %>% 
  # Mutate supplementation as a categorical variable
  dplyr::mutate(supp_cat = dplyr::case_when(
    mean_days.year_supp < 5 ~ "<5",
    mean_days.year_supp >= 5 & mean_days.year_supp < 10 ~ "5-10",
    mean_days.year_supp >= 10 & mean_days.year_supp < 15 ~ "10-15",
    mean_days.year_supp >= 15 & mean_days.year_supp < 20 ~ "15-20",
    mean_days.year_supp >= 20 ~ ">20",
    TRUE ~ NA
  )) %>% 
  # Mutate floaters/pop
  dplyr::mutate(
    prop_floaters = n_floaters/n_glt_patch,
    prop_floaters_F = n_floaters/n_glt_patch,
    prop_floaters_M = n_floaters_M/n_glt_patch,
    n_floaters_M_perF = n_floaters_M/n_adult_F_patch
  ) %>% 
  # Mutate infants/female
  dplyr::mutate(
    n_inf_perF = n_infants/n_repro_females
  )
poly = dplyr::filter(data, ms == "Polygamy")

### Data exploration -----
str(data)

#### 1. Where and when does polygamy occur? ------
dplyr::count(data, ms) # cases per mating system
dplyr::count(data, supp_cat) # cases per supplementation cat

# Polygamy across seasons
poly %>% dplyr::count(Group, name = "n_seasons") %>% dplyr::arrange(dplyr::desc(n_seasons))

# Polygamy across years
poly %>% dplyr::count(Season_start) # years (start of the season)

# Polygamy across UMMPs
poly %>% dplyr::count(UMMPs, name = "n_polygamy")

# Observations (group x season) and groups with polygamy, all data
data %>% dplyr::summarise(n_obs = dplyr::n(),
                          n_obs_poly = sum(ms == "Polygamy", na.rm = TRUE),
                          n_groups = dplyr::n_distinct(Group),
                          n_groups_poly = dplyr::n_distinct(Group[ms == "Polygamy"]))

# Sex-biased groups (adults): counts and proportions
data %>% dplyr::filter(!is.na(group_bias)) %>%
  dplyr::count(ms, group_bias) %>%
  dplyr::group_by(ms) %>%
  dplyr::mutate(prop = round(n / sum(n), 2)) %>%
  dplyr::ungroup()

#### 2. Monogamy vs polygamy ------
# Statistical summary across predictors
vars = c("age_mating_dom",
         "GS_total", "n_adults", 
         "n_immigrants", "n_immigrants_M", "n_immigrants_F", "n_emigrants", 
         "adult_turnover", "male_turnover",
         "sex_ratio", 
         "n_floaters", "n_floaters_M", "n_floaters_F",
         "n_glt_patch", "n_adult_F_patch", "n_adult_M_patch", "sex_ratio_patch",
         "n_groups_patch", 
         "lambda",
         "enn_m", "area_km2", "dPC", "shape",
         "prop_floaters", "prop_floaters_M", "prop_floaters_F",
         "n_floaters_M_perF"
         )

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
  dplyr::arrange(p_wilcox) %>% 
  print(n=30)

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
                dens_floaters, dens_floaters_F, dens_floaters_M,
                area_km2, shape, enn_m, dPC,
                n_glt_patch, n_groups_patch,
                dens_glt, dens_groups,
                lambda,
                n_days_supp_food)
cor_mat = cor(X, use = "pairwise.complete.obs", method = "pearson")
cor_mat
# Correlation between densities and area
cor(data$dens_floaters_M, data$area_km2)

# Identify strong correlations
high_corr = cor_mat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(-var1,names_to = "var2",values_to = "r") %>%
  dplyr::filter(var1 < var2, 
                abs(r) > 0.5) %>%
  dplyr::arrange(desc(abs(r)))

high_corr

### VIF
X_vif = data %>% 
  dplyr::select(n_repro_females,
                age_mating_dom, 
                GS_total, 
                sex_ratio,
                adult_turnover, 
                male_turnover,
                n_immigrants_M,
                n_floaters_M,
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

#### 5. Plots through time -----
# Floaters and polygamy through time
subset = data %>% dplyr::mutate(mating_date = as.Date(mating_date))
dens_line = subset %>%
  dplyr::mutate(month = lubridate::floor_date(mating_date, "month")) %>%
  dplyr::group_by(UMMPs, month) %>%
  dplyr::summarise(dens = mean(dens_floaters_M, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(!is.na(dens))

ggplot() +
  geom_smooth(data = dens_line, aes(month, dens),
              method = "gam", formula = y ~ s(x, k = 4),
              se = FALSE, colour = "steelblue4", linewidth = 0.8) +
  geom_point(data = dplyr::filter(data, !is.na(dens_floaters_M), !is.na(ms)),
             aes(mating_date, dens_floaters_M, colour = ms, size = ms, shape = ms), alpha = 0.85) +
  scale_colour_manual(values = c(Monogamy = "grey55", Polygamy = "darkorange2")) +
  scale_size_manual(values = c(Monogamy = 1.8, Polygamy = 3.2)) +
  scale_shape_manual(values = c(Monogamy = 16, Polygamy = 17)) +
  facet_wrap(~ UMMPs, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "Mating reference date", y = "Male floater density (ind/km²)",
       colour = NULL, size = NULL, shape = NULL,
       title = "Male floater density and polygamy events") +
  theme_classic() +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        strip.text = element_text(face = "bold"), legend.position = "bottom")


# Age of dominant females and polygamy through time
age_line = subset %>%
  dplyr::mutate(month = lubridate::floor_date(mating_date, "month")) %>%
  dplyr::group_by(UMMPs, month) %>%
  dplyr::summarise(age = mean(age_mating_dom, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(!is.na(age))

ggplot() +
  geom_smooth(data = age_line, aes(month, age),
              method = "gam", formula = y ~ s(x, k = 4),
              se = FALSE, colour = "steelblue4", linewidth = 0.8) +
  geom_point(data = dplyr::filter(data, !is.na(age_mating_dom), !is.na(ms)),
             aes(mating_date, age_mating_dom, colour = ms, size = ms, shape = ms), alpha = 0.85) +
  scale_colour_manual(values = c(Monogamy = "grey55", Polygamy = "darkorange2")) +
  scale_size_manual(values = c(Monogamy = 1.8, Polygamy = 3.2)) +
  scale_shape_manual(values = c(Monogamy = 16, Polygamy = 17)) +
  facet_wrap(~ UMMPs, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "Mating reference date", y = "Age of dominant females (months)",
       colour = NULL, size = NULL, shape = NULL,
       title = "Age of dominant females and polygamy events") +
  theme_classic() +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        strip.text = element_text(face = "bold"), legend.position = "bottom")


#### 6. Age of dominant females: below / above N months ----
age_thr = 61.7
age_d = data %>%
  dplyr::filter(!is.na(age_mating_dom)) %>%
  dplyr::mutate(age_class = dplyr::if_else(age_mating_dom < age_thr, "< 62 months", ">= 62 months")) %>% 
  dplyr::select(Group, Season, UMMPs, age_mating_dom, age_class, ms)

# Observations and groups per class
age_d %>% dplyr::count(age_class, ms)
age_d %>% dplyr::group_by(age_class) %>%
  dplyr::summarise(n_obs = dplyr::n(), n_groups = dplyr::n_distinct(Group),
                   n_poly = sum(ms == "Polygamy"), prop_poly = round(n_poly / n_obs, 2),
                   n_groups_poly = dplyr::n_distinct(Group[ms == "Polygamy"]))

#### 7. Adult females in groups with ONE reproductive female ----
data %>% 
  dplyr::mutate(n_FEM_in_grp = dplyr::case_when(
    n_females > 1 ~ ">=2",
    TRUE ~ "1"
  )) %>% 
  dplyr::count(ms, n_FEM_in_grp)

# n_females = adult females (SA + AD) identified on the composition date
one_f = data %>% dplyr::filter(n_repro_females == 1)

one_f %>% dplyr::count(n_females) %>% dplyr::mutate(prop = round(n / sum(n), 2))

# Comparison with polygamous groups
data %>% dplyr::filter(!is.na(n_females)) %>% dplyr::group_by(ms) %>%
  dplyr::summarise(n = dplyr::n(), median_females = median(n_females),
                   prop_more_than_2 = round(mean(n_females > 2), 2))

ggplot(dplyr::filter(data, !is.na(n_females)), aes(n_females, fill = ms)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(Monogamy = "grey55", Polygamy = "darkorange2")) +
  labs(x = "Adult females in the group", y = "Observations", fill = NULL) + theme_classic()

#### 8. Food supplementation ----
data %>% dplyr::group_by(UMMPs) %>%
  dplyr::summarise(n = sum(!is.na(mean_days.year_supp)),
                   mean = round(mean(mean_days.year_supp, na.rm = TRUE), 1),
                   sd = round(sd(mean_days.year_supp, na.rm = TRUE), 1),
                   min = min(mean_days.year_supp, na.rm = TRUE),
                   max = max(mean_days.year_supp, na.rm = TRUE))

ggplot(dplyr::filter(data, !is.na(mean_days.year_supp)),
       aes(Season_start, mean_days.year_supp, group = Group, colour = Group)) +
  geom_line(alpha = 0.6) +
  geom_point(aes(shape = ms, size = ms)) +
  scale_shape_manual(values = c(Monogamy = 16, Polygamy = 17)) +
  scale_size_manual(values = c(Monogamy = 1.6, Polygamy = 3)) +
  facet_wrap(~ UMMPs, ncol = 2) +
  labs(x = "Season start", y = "Mean days of supplementation per year", shape = NULL, size = NULL) +
  theme_classic() + theme(strip.background = element_rect(fill = "grey90", colour = NA))

#### 9. Data exploration ----
# Following Zuur et al. (2010)
vars_x = c("dens_floaters_M", "n_floaters_M", "age_mating_dom")
cols_ms = c(Monogamy = "grey55", Polygamy = "darkorange2")

##### 9a. Outliers: Cleveland dotplot + boxplot ----
long = data %>% dplyr::mutate(order = dplyr::row_number()) %>%
  dplyr::select(order, ms, dplyr::all_of(vars_x)) %>%
  tidyr::pivot_longer(dplyr::all_of(vars_x), names_to = "variable", values_to = "value") %>%
  dplyr::filter(!is.na(value))

# CLEVELAND PLOT: identify outliers
ggplot(long, aes(value, order, colour = ms)) +
  geom_point(size = 1.6, alpha = 0.7) + scale_colour_manual(values = cols_ms) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Value of the variable", y = "Order of the data", colour = NULL) + theme_classic()

# BOXPLOT: identify outliers
ggplot(long, aes(x = "", y = value)) +
  geom_boxplot(outlier.colour = "red3", width = 0.5) +
  facet_wrap(~ variable, scales = "free_y") + labs(x = NULL, y = "Value") + theme_classic()

# Which rows are the largest densities?
data %>% dplyr::arrange(dplyr::desc(dens_floaters_M)) %>%
  dplyr::select(Group, UMMPs, Season, dens_floaters_M, n_floaters_M, area_km2, ms) %>% head(8)

##### 9b. Zeros ----
data %>%
  tidyr::pivot_longer(c(n_floaters_M, dens_floaters_M), names_to = "variable", values_to = "value") %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(n = sum(!is.na(value)), n_zero = sum(value == 0, na.rm = TRUE),
                   pct_zero = round(100 * n_zero / n, 1))

# Where are the zeros?
data %>% dplyr::group_by(UMMPs) %>%
  dplyr::summarise(n = sum(!is.na(n_floaters_M)), n_zero = sum(n_floaters_M == 0, na.rm = TRUE),
                   pct_zero = round(100 * n_zero / n, 1),
                   mean_effort = round(mean(effort_ummp, na.rm = TRUE), 1))
data %>% dplyr::group_by(ms) %>%
  dplyr::summarise(pct_zero = round(100 * mean(n_floaters_M == 0, na.rm = TRUE), 1))

# Frequency of values and zeros
ggplot(data, aes(n_floaters_M)) + geom_bar() + labs(x = "Male floaters (count)", y = "Frequency") + theme_classic()
ggplot(data, aes(dens_floaters_M)) + geom_histogram(bins = 30) +
  labs(x = "Male floater density (ind/km²)", y = "Frequency") + theme_classic()

# According to sampling effort
ggplot(data, aes(effort_ummp, n_floaters_M)) + geom_jitter(width = 0.3, height = 0.1, alpha = 0.6) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  labs(x = "UMMP sampling effort (dates per year)", y = "Male floaters") + theme_classic()
# If zeros happen at low sampling effort, they may be false zeros

##### 9c. Relationships between POLYGAMY and the explanatory variables ----
vars_rel = c("age_mating_dom", "dens_floaters_M", "n_floaters_M", 
             "dPC", "n_glt_patch", "n_adult_M_patch")
data %>% dplyr::select(ms_binary, dplyr::all_of(vars_rel)) %>%
  tidyr::pivot_longer(-ms_binary, names_to = "variable", values_to = "value") %>%
  dplyr::filter(!is.na(value), !is.na(ms_binary)) %>%
  ggplot(aes(value, ms_binary)) +
  geom_jitter(height = 0.04, width = 0, alpha = 0.5) +
  # linear logistic
  geom_smooth(method = "glm", method.args = list(family = binomial),
              colour = "darkorange2", fill = "darkorange2", alpha = 0.15) + 
  # flexible (GAM)
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), method.args = list(family = binomial),
              colour = "steelblue4", se = FALSE, linetype = "dashed") +  
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = NULL, y = "P(polygamy)") + theme_classic()

##### 9d. Interactions ----
int = data %>% dplyr::filter(!is.na(dens_glt), !is.na(dens_floaters_M)) %>%
  dplyr::mutate(
    dens_class = factor(dplyr::case_when(
      dens_floaters_M == 0 ~ "0",
      dens_floaters_M <= median(dens_floaters_M[dens_floaters_M > 0]) ~ "low (> 0)",
      TRUE ~ "high"), levels = c("0", "low (> 0)", "high")))

int %>% dplyr::count(dens_class, ms)   # events per panel: very few cells will have enough

# Effect of age at each density class
ggplot(int, aes(dens_glt, ms_binary)) +
  geom_jitter(height = 0.04, width = 0.5, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = binomial), colour = "darkorange2") +
  facet_wrap(~ dens_class) + 
  labs(y = "P(polygamy)") + theme_classic()

# Formal check
m_add = glm(ms_binary ~ age_mating_dom + dens_floaters_M, data = int, family = binomial)
m_int = glm(ms_binary ~ age_mating_dom * dens_floaters_M, data = int, family = binomial)
anova(m_add, m_int, test = "LRT"); AIC(m_add, m_int)


### Relationships between MS and sampling effort ----
subset = data %>% 
  dplyr::select(ms_binary, effort_group, effort_ummp) %>% 
  na.omit()
mod_spl_grp = glm(ms_binary ~ effort_group, data = subset, family = poisson)
mod_spl_ummp = glm(ms_binary ~ effort_ummp, data = subset, family = poisson)
summary(mod_spl_grp)
summary(mod_spl_ummp)

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
  )) %>% 
  dplyr::mutate(enn_log = log(enn_m+1),
                enn_scale = scale(enn_m))

# Distributions
hist(subset$enn_m)
hist(subset$enn_log)
hist(subset$enn_scale)
hist(subset$n_floaters)
glmm.hp::detect_glm_family(subset$n_floaters)
glmm.hp::detect_glm_family(subset$ms_binary)

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
  glm(n_floaters ~ enn_scale, data = subset, family = poisson),
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
# USE "Estimate" to know how big is the effect in real units
# USE Std.Estimate from coefs() to know which path is stronger
# USE semEff to know indirect and total effect of X on MS, with CIs

# Unstandardized effects
coefs(mod1, standardize = "none") # Std. Effects are removed

## Standardization
# Several methods for standardization (scale, range, 2 methods for binomial: latent linear, OE)
# The standardization options only change Std.Estimate

# Scale-standardized coefficients ("Std.Estimate")
coefs(mod1, standardize = "scale")
# Range-standardized coefficients
# response change as a fraction of its range when x spans its observed range. Not comparable with scale-standardized values.
coefs(mod1, standardize = "range")

# For binomial response models
# Two approaches to compute model STANDARDIZED coefficients: latent theoretic and observation-empirical
coefs(mod1, standardize.type = "latent.linear")
coefs(mod1, standardize.type = "Menard.OE")

# Binomial path (n_floaters -> ms_binary), logit scale:
# Estimate = change in log-odds of polygamy per +1 floater.
# b = 0.2352 -> OR = exp(0.2352) = 1.27 -> each extra floater multiplies the odds of polygamy by 1.27 (+26.5%). 
# OR > 1: shift toward polygamy; OR < 1: toward monogamy.
# Odds are not probabilities: the change in P(polygamy) depends on the baseline

# Poisson path (enn_m -> n_floaters), log scale:
# exp(Estimate) = rate ratio = multiplicative change in expected floaters per +1 m.
# b = 0.0015 -> RR = 1.0015 per metre; per 100 m: exp(0.15) = 1.16 (+16%)

# Odds ratios with 95% CI (profile CIs)
broom::tidy(mod1[[2]], exponentiate = TRUE, conf.int = TRUE) # Select the model to plot
# Predicted probability of polygamy along floaters
plot(ggeffects::ggpredict(mod1[[2]], terms = "n_floaters [all]"))

###### semEff -----
# semEff takes the standardized direct effects and bootstraps them. It then combines them into indirect effects (the product along the path) and total effects (direct plus indirect)
# R = 100 is for testing only; use >= 1000 for the final CIs.
# semEff works on STANDARDIZED direct effects with its OWN standardization:
# its values (0.051) are NOT comparable with piecewiseSEM's Std.Estimate (0.45).
# - DIRECT:  standardized direct effect
# - INDIRECT: product of direct effects along the path
# - TOTAL: direct + indirect
# - Bias: bootstrap bias (near 0 = good). '*' = CI excludes 0.
# - MEDIATORS: sum of the indirect effects going through that mediator.
# Units: SD of the response per SD of the predictor, NOT log-odds and NOT OR.
# Mixed log/logit links make the indirect effect approximate: read direction and
# relative importance, not an exact magnitude.

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

# Simple plot
plot(mod1, show="std", digits=3) # Standardized coeffs
plot(mod1, show="unstd") # Unstandardized coeffs
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod1),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

# Advanced plot
ct = coefs(mod1, standardize.type = "latent.linear")
r2 = rsquared(mod1)

p_txt = function(p) ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))
stars = function(p) as.character(cut(p, c(-Inf, .001, .01, .05, Inf),
                                     labels = c("***", "**", "*", "ns")))

# Pretty node names
lab = c(enn_scale = "Patch isolation\n(ENN)",
        n_floaters = "N floaters",
        ms_binary = "Polygamy\n(vs monogamy)")

# Nodes (R² shown on responses)
nodes = data.frame(id = 1:3, name = names(lab), stringsAsFactors = FALSE)
nodes$label = lab[nodes$name]
for (i in seq_len(nrow(r2))) {
  nodes$label[nodes$name == r2$Response[i]] =
    paste0(lab[r2$Response[i]], "\nR² = ", sprintf("%.2f", r2$R.squared[i]))
}

ndf = create_node_df(n = 3, label = nodes$label, shape = "box", style = "filled",
                     fillcolor = "grey92", color = "black", fontsize = 12,
                     width = 1.6, height = 0.8)

# Edges: width ~ |std estimate|, colour = sign, dashed if n.s.
edf = create_edge_df(
  from = nodes$id[match(ct$Predictor, nodes$name)],
  to   = nodes$id[match(ct$Response,  nodes$name)],
  label = sprintf("β std = %.2f %s\nb = %.2f ± %.2f\n%s",
                  ct$Std.Estimate, 
                  stars(ct$P.Value),
                  ct$Estimate, 
                  ct$Std.Error, 
                  p_txt(ct$P.Value)),
  penwidth = 1 + 6 * abs(ct$Std.Estimate),
  color = ifelse(ct$Estimate > 0, "#2a9d8f", "#c1121f"),
  style = ifelse(ct$P.Value < 0.05, "solid", "dashed"),
  fontsize = 10
)

create_graph(nodes_df = ndf, edges_df = edf) %>%
  add_global_graph_attrs("rankdir", "LR", "graph") %>%
  render_graph()


#### Density Floaters ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                n_floaters,
                n_floaters_M,
                n_floaters_F,
                dens_floaters,
                dens_floaters_M,
                dens_floaters_F,
                prop_floaters,
                prop_floaters_M,
                prop_floaters_F,
                n_floaters_M_perF,
                enn_m) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(enn_scale = scale(enn_m))

# Response variable
glmm.hp::detect_glm_family(subset$n_floaters_M)
glmm.hp::detect_glm_family(subset$dens_floaters)
glmm.hp::detect_glm_family(subset$dens_floaters_M)
glmm.hp::detect_glm_family(subset$prop_floaters_M)
glmm.hp::detect_glm_family(subset$n_floaters_M_perF)


##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> Density Floaters (Y1) -> mating system (Y2)

##### Test nb. of floaters VS density
cands = c("n_floaters", "n_floaters_M", "n_floaters_F",
          "dens_floaters", "dens_floaters_M", "dens_floaters_F",
          "prop_floaters", "prop_floaters_M", "prop_floaters_F",
          "n_floaters_M_perF")

d = subset %>%
  dplyr::select(ms_binary, dplyr::all_of(cands)) %>%
  na.omit()
nrow(d); table(d$ms_binary) # number of polygamy events limits what we can fit

# z-scored copies so that effect sizes are comparable across candidates
d[paste0(cands, "_z")] = lapply(d[cands], function(x) as.numeric(scale(x)))

fit_one = function(v) glmmTMB(
  as.formula(paste("ms_binary ~", v)),
  data = d, family = binomial)

fits = setNames(lapply(cands, fit_one), cands)
fits_z = setNames(lapply(paste0(cands, "_z"), fit_one), cands)

# AICc, delta and Akaike weights
MuMIn::model.sel(fits, rank = "AICc")

# Spearman correlations
round(cor(d[cands], method = "spearman"), 2)
GGally::ggpairs(d[cands], progress = FALSE)

# Histograms
op = par(mfrow = c(3, 3)); for (v in cands) hist(d[[v]], main = v, xlab = ""); par(op)

##### SEM
mod2 = psem(
  glm(dens_floaters_M ~ enn_scale, data = subset, family = gaussian),
  glm(ms_binary ~ dens_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod2)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod2),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Spatial & temporal structure ----
# Data
subset = data %>%
  dplyr::select(ms_binary, 
                n_floaters_M,
                dens_floaters_M, 
                enn_m, 
                Group, 
                UMMPs, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::distinct(Group, Season_start,
                  .keep_all = TRUE) %>% # REMOVE DUPLICATES (for the autocorrelative structure)
  dplyr::mutate(Group = factor(Group),
                UMMPs = factor(UMMPs),
                enn_scale = scale(enn_m),
                enn_log = log(enn_m+1),
                Season_start = factor(Season_start))

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> N Floaters (Y1) -> mating system (Y2)
# With random effects: Group|UMMPs, Group, Year

##### Relevance of random effects
# Observations are not independent: they are grouped by groups, patches, and years
# Groups in the same UMMP in the same year share the same ENN, so the rows are pseudoreplicated
# NB: With 4 UMMPs, a (1 | UMMPs) term is a poor fix (the variance of 4 levels is poorly estimated, and a singular fit is likely)
# If each Group sits in one UMMP, (1 | Group) already captures most of the UMMP effect

### Test for random effects
y = "dens_floaters_M" 
g0 = gls(as.formula(paste(y, "~ enn_scale")), data = subset, method = "REML")
g1 = lme(as.formula(paste(y, "~ enn_scale")), random = ~ 1 | Group,
         data = subset, method = "REML")
g2 = lme(as.formula(paste(y, "~ enn_scale")), random = ~ 1 | UMMPs,
         data = subset, method = "REML")
# Autoregressive model (temporal structure)
g3 = lme(as.formula(paste(y, "~ enn_scale")), random = ~ 1 | Group,
         correlation = corCAR1(form = ~ Season_start | Group),
         data = subset, method = "REML")
g4 = lme(as.formula(paste(y, "~ enn_scale")), random = ~ 1 | Season_start,
         data = subset, method = "REML")
g5 = lme4::lmer(as.formula(paste(y, "~ enn_scale + (1 | Group) + (1 | Season_start)")),
                 data = subset, REML = TRUE)

# check the AICs
# If g0 is within 2 AICc of the others, the random effects bring nothing for this response
MuMIn::AICc(g0, g1, g2, g3, g4, g5) %>% dplyr::arrange(AICc)
summary(g3)$modelStruct$corStruct  # estimated temporal correlation (Phi)

anova(g0,g4) # investigate the relevance of the random effect
# null hypothesis here is that the ratio of the two likelihood models is not different from 1

# one or more random-effect variances were estimated at (or very near) zero, on the boundary of the possible values
VarCorr(g5) # check variance: Group variance is near 0 once season is included
VarCorr(g4) # check variance: Year variance is 0.03 and residual variance is 0.12
performance::icc(g4) # year variance explains ~18% of the unexplained variance of floater density that is shared by all groups in a given year
performance::icc(glmmTMB(enn_m ~ 1 + (1 | Season_start), data = subset)) # year captures 0 variation in ENN
performance::icc(glmmTMB(enn_m ~ 1 + (1 | UMMPs), data = subset)) # UMMPs capture ~99% of variation in ENN

# Overdispersion
glmm.hp::detect_glm_family(subset$dens_floaters_M)
g4 = glmmTMB(dens_floaters_M ~ enn_scale + (1 | Season_start), data = subset, family=gaussian)
performance::check_overdispersion(g4)

# Residuals
DHARMa::simulateResiduals(g4) %>%  plot()

### Test for random effects on MS
subset$year_ou = glmmTMB::numFactor(subset$Season_start)   # for the continuous-time term
b0 = glmmTMB(ms_binary ~ dens_floaters_M, data = subset, family = binomial, REML = TRUE)
b1 = update(b0, . ~ . + (1 | Group), REML = TRUE)
b2 = update(b0, . ~ . + (1 | UMMPs), REML = TRUE)
# Temporal correlation within group
b3 = glmmTMB(ms_binary ~ dens_floaters_M + (1 | Group) + ou(0 + year_ou | Group),
             data = subset, family = binomial, REML = TRUE)
b4 = update(b0, . ~ . + (1 | Season_start), REML = TRUE)
b5 = update(b0, . ~ . + (1 | Group) + (1 | Season_start), REML = TRUE)

mods_b = list(none = b0, 
              Group = b1, 
              UMMPs = b2, 
              Group_Time = b3,
              Year = b4, 
              Group_Year = b5)

# AICc, delta and weights
MuMIn::model.sel(mods_b, rank = "AICc")

# Likelihood-ratio tests for the random effects (conservative on the boundary)
anova(b0, b1)

# Fixed-effect stability across structures
sapply(mods_b, function(m) round(fixef(m)$cond["dens_floaters_M"], 3))
# Slopes are consistent

# Residuals
DHARMa::simulateResiduals(b0) %>%  plot()
DHARMa::simulateResiduals(b1) %>%  plot()

# Data
subset = data %>%
  dplyr::select(ms_binary, 
                n_floaters_M,
                dens_floaters_M, 
                enn_m, 
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                Season_start = factor(Season_start))

##### SEM
mod3 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ dens_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod3)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod3),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### Patch size ----
# Data
subset = data %>%
  dplyr::select(ms_binary, 
                n_floaters_M,
                dens_floaters_M, 
                enn_m, 
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                Season_start = factor(Season_start))

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> N Floaters (Y1) -> mating system (Y2)
# Patch area (X2)      ->

##### SEM
mod4 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary  ~ dens_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod4)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod4),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### GLT density ----
# Data
subset = data %>%
  dplyr::select(ms_binary, 
                dens_floaters_M, 
                n_glt_patch,
                dens_glt,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                Season_start = factor(Season_start))

# Response variable
glmm.hp::detect_glm_family(subset$n_glt_patch)
glmm.hp::detect_glm_family(subset$dens_glt)

##### Path diagram
# Path diagram tested here:
#                      -------------->
# Patch isolation (X1) -> GLTs (Y1) -> Floaters (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                      -------------->

##### Test nb. of GLTs VS density
cands = c("n_glt_patch", "dens_glt")

d = subset %>%
  dplyr::select(dens_floaters_M, 
                Season_start,
                dplyr::all_of(cands)) %>%
  na.omit()

# z-scored copies so that effect sizes are comparable across candidates
d[paste0(cands, "_z")] = lapply(d[cands], function(x) as.numeric(scale(x)))

fit_one = function(v) glmmTMB(
  as.formula(paste("dens_floaters_M ~", v, "+ (1 | Season_start)")),
  data = d, family = gaussian)

fits = setNames(lapply(cands, fit_one), cands)
fits_z = setNames(lapply(paste0(cands, "_z"), fit_one), cands)

# AICc, delta and Akaike weights
MuMIn::model.sel(fits, rank = "AICc")

# Spearman correlations
round(cor(d[cands], method = "spearman"), 2)
GGally::ggpairs(d[cands], progress = FALSE)

# Histograms
op = par(mfrow = c(1, 2)); for (v in cands) hist(d[[v]], main = v, xlab = ""); par(op)

##### SEM
mod5 = psem(
  glmmTMB(dens_glt ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(dens_floaters_M ~ dens_glt + enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ dens_floaters_M + dens_glt, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod5)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod5),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Group density ----
# Data
subset = data %>%
  dplyr::select(ms_binary, 
                dens_floaters_M, 
                n_glt_patch,
                dens_glt,
                n_groups_patch,
                dens_groups,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                Season_start = factor(Season_start))

# Response variable
glmm.hp::detect_glm_family(subset$n_groups_patch)
glmm.hp::detect_glm_family(subset$dens_groups)

##### Path diagram
# Path diagram tested here:
#                      -------------->
# Patch isolation (X1) -> Groups (Y1) -> Floaters (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                      -------------->

##### Test nb. of groups VS density
cands = c("n_groups_patch", "dens_groups")

d = subset %>%
  dplyr::select(dens_floaters_M, 
                Season_start,
                dplyr::all_of(cands)) %>%
  na.omit()

# z-scored copies so that effect sizes are comparable across candidates
d[paste0(cands, "_z")] = lapply(d[cands], function(x) as.numeric(scale(x)))

fit_one = function(v) glmmTMB(
  as.formula(paste("dens_floaters_M ~", v, "+ (1 | Season_start)")),
  data = d, family = gaussian)

fits = setNames(lapply(cands, fit_one), cands)
fits_z = setNames(lapply(paste0(cands, "_z"), fit_one), cands)

# AICc, delta and Akaike weights
MuMIn::model.sel(fits, rank = "AICc")

# Spearman correlations
round(cor(d[cands], method = "spearman"), 2)
GGally::ggpairs(d[cands], progress = FALSE)

# Histograms
op = par(mfrow = c(1, 2)); for (v in cands) hist(d[[v]], main = v, xlab = ""); par(op)

##### SEM
mod6 = psem(
  glmmTMB(n_groups_patch ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = poisson),
  glmmTMB(dens_floaters_M ~ n_groups_patch + enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ dens_floaters_M + n_groups_patch, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod6)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod6),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Lambda ----
# Data
subset = data %>%
  dplyr::select(ms_binary, 
                dens_floaters_M, 
                lambda,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                Season_start = factor(Season_start))

# Response variable
glmm.hp::detect_glm_family(subset$lambda)

##### Path diagram
# Path diagram tested here:
#                      -------------->
# Patch isolation (X1) -> Lambda (Y1) -> Floaters (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                      -------------->

##### SEM
mod7 = psem(
  glmmTMB(lambda ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(dens_floaters_M ~ lambda + enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary  ~ dens_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod7)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod7),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Supplementation ----
# Data
subset = data %>%
  dplyr::select(ms_binary, 
                dens_floaters_M, 
                lambda,
                mean_days.month_supp,
                mean_days.year_supp,
                supp_cat,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                Season_start = factor(Season_start),
                supp_cat = factor(supp_cat))

# Number of events
table(subset$ms_binary)
table(subset$supp_cat)

# Response variable
glmm.hp::detect_glm_family(subset$mean_days.month_supp)
glmm.hp::detect_glm_family(subset$mean_days.year_supp)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> Floaters (Y1) -> mating system (Y2)
# Patch area (X2)      ->
# Supplementation (X3) -> 

##### Relevance of supplementation as a random effects
##### Test for random effects
y = "dens_floaters_M" 
g0 = gls(as.formula(paste(y, "~ enn_scale")), data = subset, method = "REML")
g1 = lme(as.formula(paste(y, "~ enn_scale")), random = ~ 1 | supp_cat,
         data = subset, method = "REML")
g2 = lme(as.formula(paste(y, "~ enn_scale")), random = ~ 1 | Season_start,
         data = subset, method = "REML")
g3 = lme4::lmer(as.formula(paste(y, "~ enn_scale + (1 | supp_cat) + (1 | Season_start)")),
                data = subset, REML = TRUE)

# check the AICs
# If g0 is within 2 AICc of the others, the random effects bring nothing for this response
MuMIn::AICc(g0, g1, g2, g3) %>% dplyr::arrange(AICc)

anova(g0,g1) # investigate the relevance of the random effect
# null hypothesis here is that the ratio of the two likelihood models is not different from 1

# one or more random-effect variances were estimated at (or very near) zero, on the boundary of the possible values
performance::icc(g1) # supplementation variance explains ~0.03% of the unexplained variance of floater density that is shared by all groups in a given year


##### Test for random effects on MS
b0 = glmmTMB(ms_binary ~ dens_floaters_M, data = subset, family = binomial, REML = TRUE)
b1 = update(b0, . ~ . + (1 | supp_cat), REML = TRUE)
b2 = update(b0, . ~ . + (1 | Season_start), REML = TRUE)
b3 = update(b0, . ~ . + (1 | supp_cat) + (1 | Season_start), REML = TRUE)

mods_b = list(none = b0, 
              Supp = b1, 
              Year = b2, 
              Supp_Year = b3)

# AICc, delta and weights
MuMIn::model.sel(mods_b, rank = "AICc")

# Likelihood-ratio tests for the random effects (conservative on the boundary)
anova(b0, b1)

# Fixed-effect stability across structures
sapply(mods_b, function(m) round(fixef(m)$cond["dens_floaters_M"], 3))
# Slopes are consistent

# Residuals
DHARMa::simulateResiduals(b0) %>%  plot()


##### Test supplementation variables (as covariates)
cands = c("mean_days.month_supp", "mean_days.year_supp")

d = subset %>%
  dplyr::select(lambda, 
                Season_start,
                dplyr::all_of(cands)) %>%
  na.omit()

# z-scored copies so that effect sizes are comparable across candidates
d[paste0(cands, "_z")] = lapply(d[cands], function(x) as.numeric(scale(x)))

fit_one = function(v) glmmTMB(
  as.formula(paste("lambda ~", v, "+ (1 | Season_start)")),
  data = d, family = gaussian)

fits = setNames(lapply(cands, fit_one), cands)
fits_z = setNames(lapply(paste0(cands, "_z"), fit_one), cands)

# AICc, delta and Akaike weights
MuMIn::model.sel(fits, rank = "AICc")

# Spearman correlations
round(cor(d[cands], method = "spearman"), 2)
GGally::ggpairs(d[cands], progress = FALSE)

# Histograms
op = par(mfrow = c(1, 2)); for (v in cands) hist(d[[v]], main = v, xlab = ""); par(op)

##### SEM
mod8 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + mean_days.year_supp + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ dens_floaters_M + mean_days.year_supp, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod8)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod8),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Lambda + supplementation ----
# Data
subset = data %>%
  dplyr::select(ms_binary, 
                dens_floaters_M, 
                lambda,
                mean_days.month_supp,
                mean_days.year_supp,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                Season_start = factor(Season_start))


##### Path diagram
# Path diagram tested here:
#                      -------------->
# Patch isolation (X1) -> Lambda (Y1) -> Floaters (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                      -------------->
# Supplementation (X3) -> 
#                      --------------->

##### SEM
mod9 = psem(
  glmmTMB(lambda ~ enn_scale + area_km2 + mean_days.year_supp + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(dens_floaters_M ~ lambda + enn_scale + area_km2 + mean_days.year_supp + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary  ~ dens_floaters_M + mean_days.year_supp, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod9)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod9),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Age of dominant females ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                n_floaters_M,
                dens_floaters_M, 
                age_mating_dom,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$age_mating_dom)
glmm.hp::detect_glm_family(subset$age_log)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> Floaters (Y1) -> mating system (Y2)
# Patch area (X2)      ->
#                 Age dom. females (X3) -> 

##### SEM
mod10 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ dens_floaters_M + age_log, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod10)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod10),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### Nb immigrants ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters_M, 
                age_mating_dom,
                n_immigrants,
                n_immigrants_F,
                n_immigrants_M,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$n_immigrants)
glmm.hp::detect_glm_family(subset$n_immigrants_M)
glmm.hp::detect_glm_family(subset$n_immigrants_F)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> N. immigrants (Y1) -> mating system (Y2)
# Patch area (X2)      ->
#                 Age dom. females (X3) -> 

##### Test nb. of immigrants
cands = c("n_immigrants", "n_immigrants_M", "n_immigrants_F")

d = subset %>%
  dplyr::select(ms_binary, dplyr::all_of(cands)) %>%
  na.omit()
nrow(d); table(d$ms_binary) # number of polygamy events limits what we can fit

# z-scored copies so that effect sizes are comparable across candidates
d[paste0(cands, "_z")] = lapply(d[cands], function(x) as.numeric(scale(x)))

fit_one = function(v) glmmTMB(
  as.formula(paste("ms_binary ~", v)),
  data = d, family = binomial)

fits = setNames(lapply(cands, fit_one), cands)
fits_z = setNames(lapply(paste0(cands, "_z"), fit_one), cands)

# AICc, delta and Akaike weights
MuMIn::model.sel(fits, rank = "AICc")
# All models are equivalent

# Spearman correlations
round(cor(d[cands], method = "spearman"), 2)
GGally::ggpairs(d[cands], progress = FALSE)

# Histograms
op = par(mfrow = c(1, 3)); for (v in cands) hist(d[[v]], main = v, xlab = ""); par(op)

##### SEM
mod11 = psem(
  glmmTMB(n_immigrants ~ enn_scale + area_km2 + age_log + (1 | Season_start), data = subset, family = poisson),
  glm(ms_binary ~ n_immigrants + age_log, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod11)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod11),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Nb immigrants + dens. floaters ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters_M, 
                age_mating_dom,
                n_immigrants,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

##### SEM
mod12 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(n_immigrants ~ dens_floaters_M + age_log + (1 | Season_start), data = subset, family = poisson),
  glm(ms_binary ~ dens_floaters_M + n_immigrants + age_log, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod12)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod12),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Turnover + dens. floaters ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters,
                dens_floaters_M,
                age_mating_dom,
                adult_turnover,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$adult_turnover)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> floaters (Y1) -> turnover (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                  Age dom. females (X3) -> 


##### SEM
mod13 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(adult_turnover ~ dens_floaters_M + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ adult_turnover + age_log, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod13)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod13),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### N. adults ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters,
                dens_floaters_M,
                age_mating_dom,
                adult_turnover,
                n_adults,
                n_males,
                n_females,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$n_adults)
glmm.hp::detect_glm_family(subset$n_males)
glmm.hp::detect_glm_family(subset$n_females)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> floaters (Y1) -> turnover (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                  Age dom. females (X3) -> 
#                                          N adults (X4) ->

##### SEM
mod14 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(adult_turnover ~ dens_floaters_M + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ adult_turnover + age_log + n_adults, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod14)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod14),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### N. males ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters,
                dens_floaters_M,
                age_mating_dom,
                adult_turnover,
                n_adults,
                n_males,
                n_females,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$n_adults)
glmm.hp::detect_glm_family(subset$n_males)
glmm.hp::detect_glm_family(subset$n_females)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> floaters (Y1) -> turnover (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                  Age dom. females (X3) -> 
#                                           N males (X4) ->

##### SEM
mod15 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(adult_turnover ~ dens_floaters_M + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ adult_turnover + age_log + n_males, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod15)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod15),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### N. females ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters,
                dens_floaters_M,
                age_mating_dom,
                adult_turnover,
                n_adults,
                n_males,
                n_females,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$n_adults)
glmm.hp::detect_glm_family(subset$n_males)
glmm.hp::detect_glm_family(subset$n_females)

##### Path diagram
# Path diagram tested here:
#                      -------------->
# Patch isolation (X1) -> floaters (Y1) -> turnover (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                  Age dom. females (X3) -> 
#                                         N females (X4) ->

##### SEM
mod16 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(adult_turnover ~ dens_floaters_M + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ adult_turnover + age_log + n_females, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod16)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod16),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### Group size ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters,
                dens_floaters_M,
                age_mating_dom,
                adult_turnover,
                n_adults,
                n_males,
                n_females,
                GS_total,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$n_adults)
glmm.hp::detect_glm_family(subset$n_males)
glmm.hp::detect_glm_family(subset$n_females)
glmm.hp::detect_glm_family(subset$GS_total)


##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> floaters (Y1) -> turnover (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                  Age dom. females (X3) -> 
#                                                GS (X4) ->

##### SEM
mod17 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(adult_turnover ~ dens_floaters_M + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ adult_turnover + age_log + GS_total, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod17)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod17),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### Turnover + GS ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters,
                dens_floaters_M,
                age_mating_dom,
                adult_turnover,
                n_adults,
                n_males,
                n_females,
                GS_total,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$n_adults)
glmm.hp::detect_glm_family(subset$n_males)
glmm.hp::detect_glm_family(subset$n_females)
glmm.hp::detect_glm_family(subset$GS_total)

##### Path diagram
# Path diagram tested here:
#                         N adults (X4) ->
# Patch isolation (X1) -> floaters (Y1) -> turnover (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                  Age dom. females (X3) -> 

##### SEM
mod18 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(adult_turnover ~ dens_floaters_M + n_adults + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ adult_turnover + age_log, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod18)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod18),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Turnover + dens. floaters + age ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters,
                dens_floaters_M,
                age_mating_dom,
                adult_turnover,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$dens_floaters_M)
glmm.hp::detect_glm_family(subset$adult_turnover)

##### Path diagram
# Path diagram tested here:
#                                       ------------------>
# Patch isolation (X1) -> floaters (Y1) -> turnover (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                  Age dom. females (X3) -> 

##### SEM
mod19 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(adult_turnover ~ dens_floaters_M + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ adult_turnover + age_log + dens_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod19)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod19),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### Male turnover ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters_M,
                age_mating_dom,
                male_turnover,
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$dens_floaters_M)
glmm.hp::detect_glm_family(subset$male_turnover)

##### Path diagram
# Path diagram tested here:
#                                       ------------------>
# Patch isolation (X1) -> floaters (Y1) -> male turnover (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                       Age dom. females (X3) -> 

##### SEM
mod20 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(male_turnover ~ dens_floaters_M + age_log + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ male_turnover + age_log + dens_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod20)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod20),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### dPC ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters_M,
                age_mating_dom,
                enn_m,
                dPC,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                dPC_scale = scale(dPC),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$dPC)
glmm.hp::detect_glm_family(subset$dPC_scale)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> floaters (Y1) -> mating system (Y2)
# Patch area (X2)      ->
#                 Age dom. females (X3) -> 

##### SEM
mod21 = psem(
  glmmTMB(dens_floaters_M ~ dPC + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ age_log + dens_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod21)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod21),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Floaters + age ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters_M,
                age_mating_dom,
                enn_m,
                dPC,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> floaters (Y1) -> mating system (Y2)
#                 Age dom. females (X3) -> 

##### SEM
mod22 = psem(
  glm(ms_binary ~ age_log + dens_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod22)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod22),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Age only ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

##### Path diagram
# Path diagram tested here:
# Age dom. females (X1) -> mating system (Y2)

##### SEM
mod23 = psem(
  glm(ms_binary ~ age_log, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod23)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod23),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### Age + sex-ratio ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                sex_ratio,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$sex_ratio)

##### Path diagram
# Path diagram tested here:
# Age dom. females (X1) -> mating system (Y2)
#        Sex ratio (X2) ->

##### SEM
mod24 = psem(
  glm(ms_binary ~ age_log + sex_ratio, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod24)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod24),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### Age + sex-ratio + floaters ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                sex_ratio,
                dPC,
                area_km2,
                dens_floaters_M,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$sex_ratio)

##### Path diagram
# Path diagram tested here:
#                                    Age dom. females (X1) -> 
#        Isolation (X2) -> Floaters (Y1) -> Sex ratio (Y2) -> Mating system (Y3)
#             Area (X3) ->

##### SEM
mod25 = psem(
  glmmTMB(dens_floaters_M ~ dPC + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glm(sex_ratio ~ dens_floaters_M, data = subset, family = gaussian),
  glm(ms_binary ~ age_log + sex_ratio, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod25)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod25),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Floaters + age + Group RE ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                dPC,
                area_km2,
                dens_floaters_M,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

##### Path diagram
# Path diagram tested here:
#                  Age dom. females (X1) -> 
#        Isolation (X2) -> Floaters (Y1) -> Mating system (Y2)
#             Area (X3) ->

##### SEM
mod26 = psem(
  glmmTMB(dens_floaters_M ~ dPC + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(ms_binary ~ age_log + dens_floaters_M + (1 | Group), data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod26)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod26),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Floaters + age + supplementation ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                dPC,
                area_km2,
                dens_floaters_M,
                mean_days.year_supp,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

##### Path diagram
# Path diagram tested here:
#                  Age dom. females (X1) -> 
#        Isolation (X2) -> Floaters (Y1) -> Mating system (Y2)
#             Area (X3) ->
#                   Supplementation (X4) -> 

##### SEM
mod27 = psem(
  glmmTMB(dens_floaters_M ~ dPC + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ age_log + dens_floaters_M + mean_days.year_supp, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod27)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod27),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Age + supplementation ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                mean_days.year_supp,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

##### Path diagram
# Path diagram tested here:
# Age dom. females (X1) -> 
#  Supplementation (X2) -> Mating system (Y1)

##### SEM
mod28 = psem(
  glm(ms_binary ~ age_log + mean_days.year_supp, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod28)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod28),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### Nb Floaters + age ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                dPC,
                area_km2,
                n_floaters_M,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Reponse variable
glmm.hp::detect_glm_family(subset$n_floaters_M)

##### Path diagram
# Path diagram tested here:
#                  Age dom. females (X1) -> 
#        Isolation (X2) -> Floaters (Y1) -> Mating system (Y2)
#             Area (X3) ->

##### SEM
mod29 = psem(
  glmmTMB(n_floaters_M ~ dPC + area_km2 + (1 | Season_start), data = subset, family = poisson),
  glm(ms_binary ~ age_log + n_floaters_M, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod29)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod29),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#### GAM ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                dPC,
                area_km2,
                dens_floaters_M,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_factor = factor(Season_start))

# SEM
mod30 = psem(
  glmmTMB(dens_floaters_M ~ dPC + area_km2 + (1 | Season_factor), data = subset, family = gaussian),
  mgcv::gam(ms_binary ~ s(age_log, k=4) + s(dens_floaters_M, k=4), data = subset, family = binomial, method = "REML"),
  data = subset
)
summary(mod30)

# Is the smooth needed? Compare outside psem
m_lin = glm(ms_binary ~ age_log + dens_floaters_M, data = subset, family = binomial)
m_gam = mgcv::gam(ms_binary ~ s(age_log, k = 4) + dens_floaters_M, data = subset, family = binomial, method = "REML")
AIC(m_lin, m_gam)
summary(m_gam)$s.table # edf close to 1 = the effect is linear
plot(m_gam, select = 1, shade = TRUE, seWithMean = TRUE)

# Age increases with time: separate age from calendar year
m_gam_t = mgcv::gam(ms_binary ~ s(age_log, k = 4) + s(Season_start, k = 4) + dens_floaters_M,
                    data = subset, family = binomial, method = "REML")
mgcv::concurvity(m_gam_t) # values near 1 = age and year cannot be separated


#### Zero inflation ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                age_mating_dom,
                dPC,
                area_km2,
                n_floaters_M,
                dens_floaters_M,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Is zero-inflation needed? Compare on the count
f_poisson = glmmTMB(n_floaters_M ~ dPC + area_km2 + (1 | Season_start),
               ziformula = ~ 0, data = subset, family = poisson)
f_nb = glmmTMB(n_floaters_M ~ dPC + area_km2 + (1 | Season_start),
                    ziformula = ~ 0, data = subset, family = nbinom2,
                    control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS")))
f_zinb = update(f_nb, ziformula = ~ 1)
MuMIn::AICc(f_poisson, f_nb, f_zinb)
performance::check_zeroinflation(f_nb)
performance::check_zeroinflation(f_poisson)

# COUNTS: zero-inflated negative binomial (conditional part = counts, zi part = excess zeros)
mod31 = psem(
  glmmTMB(n_floaters_M ~ dPC + area_km2 + (1 | Season_start),
          ziformula = ~ 1, data = subset, family = poisson),
  glm(ms_binary ~ age_log + n_floaters_M, data = subset, family = binomial),
  data = subset
)
summary(mod31)

# DENSITY: continuous with zeros -> Tweedie (a point mass at 0 + positive values)
mod32 = psem(
  glmmTMB(dens_floaters_M ~ dPC + area_km2 + (1 | Season_start),
          data = subset, family = tweedie(link = "log")),
  glm(ms_binary ~ age_log + dens_floaters_M, data = subset, family = binomial),
  data = subset
)
summary(mod32)

#### N adults males patch ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                dens_floaters_M,
                n_adult_F_patch,
                n_adult_M_patch,
                age_mating_dom,
                dPC,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$n_adult_M_patch)

##### Path diagram
# Path diagram tested here:
#                  Age dom. females (X1) -> 
#        Isolation (X2) -> N adults M (Y1) -> Mating system (Y2)
#             Area (X3) ->

##### SEM
mod33 = psem(
  glmmTMB(n_adult_M_patch ~ dPC + area_km2 + (1 | Season_start), data = subset, family = poisson),
  glm(ms_binary ~ age_log + n_adult_M_patch, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod33)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod33),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#### N males per female in the patch ----
# Data
subset = data %>%
  dplyr::select(ms_binary,
                n_floaters_M_perF,
                age_mating_dom,
                dPC,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                age_log = log(age_mating_dom),
                Season_start = factor(Season_start))

# Polygamy events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$n_floaters_M_perF)

##### Path diagram
# Path diagram tested here:
#                                        Age dom. females (X1) -> 
#        Isolation (X2) -> N adults floaters M per female (Y1) -> Mating system (Y2)
#             Area (X3) ->

##### SEM
mod34 = psem(
  glmmTMB(n_floaters_M_perF ~ dPC + area_km2 + age_log + (1 | Season_start), data = subset, family = gaussian),
  glm(ms_binary ~ age_log + n_floaters_M_perF, data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod34)

##### Plot
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(mod34),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


### Model comparison -----
mods = list(
  "isolation + n floaters" = mod1,
  "isolation + dens floaters M" = mod2,
  "isolation + dens floaters M + RE"= mod3,
  "isolation + area + dens floaters M + RE"= mod4,
  "isolation + area + dens floaters M + n GLT + RE"= mod5,
  "isolation + area + dens floaters M + n groups + RE"= mod6,
  "isolation + area + dens floaters M + lambda + RE"= mod7,
  "isolation + area + dens floaters M + supplementation + RE"= mod8,
  "isolation + area + dens floaters M + lambda + supplementation + RE"= mod9,
  "isolation + area + dens floaters M + age dom. females + RE"= mod10,
  "isolation + area + n immig males + age dom. females + RE" = mod11,
  "isolation + area + n immig males + dens floaters M + age dom. females + RE" = mod12,
  "isolation + area + turnover + dens floaters M + age dom. females + RE" = mod13,
  "isolation + area + turnover + dens floaters M + age dom. females + n adults + RE" = mod14,
  "isolation + area + turnover + dens floaters M + age dom. females + n males + RE" = mod15,
  "isolation + area + turnover + dens floaters M + age dom. females + n females + RE" = mod16,
  "isolation + area + turnover + dens floaters M + age dom. females + GS total + RE" = mod17,
  "isolation + area + turnover + dens floaters M + age dom. females + n adults (-> turnover) + RE" = mod18,
  "isolation + area + turnover + dens floaters M (-> MS) + age dom. females + RE" = mod19,
  "isolation + area + male turnover + dens floaters M (-> MS) + age dom. females + RE" = mod20,
  "isolation (dPC) + area + dens floaters M + age dom. females + RE" = mod21,
  # "dens floaters M + age dom. females" = mod22,
  # "age dom. females" = mod23,
  # "age dom. females + sex ratio" = mod24,
  "isolation + area + dens floaters M + age dom. females + sex ratio" = mod25,
  "isolation + area + dens floaters M + age dom. females + Group RE" = mod26,
  "isolation + area + dens floaters M + age dom. females + supplementation" = mod27,
  # "age dom. females + supplementation" = mod28,
  "isolation + area + n floaters M + age dom. females" = mod29,
  "isolation + area + dens floaters M + age dom. females - GAM" = mod30,
  "isolation + area + n floaters M + age dom. females - ZI" = mod31,
  "isolation + area + dens floaters M + age dom. females - ZI" = mod32,
  "isolation + area + n adult M patch + age dom. females" = mod33,
  "isolation + area + n floaters M per F + age dom. females" = mod34,
  "isolation + area + n floaters M per F + age dom. females + n infants" = mod35
  )

# Get statistics from SEMs
get_stats = function(m) {
  
  # AICc, K (parameters), n
  aic = piecewiseSEM::AIC_psem(m)
  
  # Tests of directed separation (one row per omitted path)
  ds = tryCatch(piecewiseSEM::dSep(m),     error = function(e) NULL)
  fc = tryCatch(piecewiseSEM::fisherC(m),  error = function(e) NULL)
  ll = tryCatch(piecewiseSEM::LLchisq(m),  error = function(e) NULL)
  r2 = tryCatch(piecewiseSEM::rsquared(m), error = function(e) NULL)
  
  # R² of the final response (mating system)
  r2_ms  = if (!is.null(r2)) r2[r2$Response == "ms_binary", ][1, ] else NULL
  r2_mar = if (is.null(r2_ms)) NA else if ("Marginal" %in% names(r2_ms)) r2_ms$Marginal else r2_ms$R.squared
  r2_con = if (is.null(r2_ms) || !"Conditional" %in% names(r2_ms)) NA else r2_ms$Conditional
  
  data.frame(
    K = aic$K, n = aic$n, AICc = aic$AICc,
    # Missing link = at least one omitted path with p < 0.05
    n_claims     = if (is.null(ds)) NA else nrow(ds),
    missing_link = if (is.null(ds) || nrow(ds) == 0) NA else any(ds$P.Value < 0.05, na.rm = TRUE),
    FisherC   = if (is.null(fc)) NA else fc$Fisher.C,
    FisherC_p = if (is.null(fc)) NA else fc$P.Value,
    Chisq     = if (is.null(ll)) NA else ll$Chisq,
    Chisq_p   = if (is.null(ll)) NA else ll$P.Value,
    R2_MS_marginal    = as.numeric(r2_mar),
    R2_MS_conditional = as.numeric(r2_con))
}

aic_tab = do.call(rbind, lapply(mods, get_stats))

aic_tab$delta  = aic_tab$AICc - min(aic_tab$AICc)
aic_tab$weight = exp(-aic_tab$delta / 2) / sum(exp(-aic_tab$delta / 2))
aic_tab = aic_tab[order(aic_tab$delta), ]
aic_tab$cum_weight = cumsum(aic_tab$weight)

# Print best models
aic_tab %>%
  tibble::rownames_to_column("model") %>%
  dplyr::mutate(missing_link = dplyr::case_when(is.na(missing_link) ~ "n/a",
                                                missing_link ~ "YES", TRUE ~ "no"),
                dplyr::across(where(is.numeric), ~ round(.x, 3))) %>%
  dplyr::select(model, K, n, AICc, delta, weight, cum_weight,
                missing_link, n_claims, FisherC, FisherC_p, Chisq, Chisq_p,
                R2_MS_marginal, R2_MS_conditional)


### Threshold analysis -----
# Complete cases only
d = data %>%
  dplyr::select(ms_binary, age_mating_dom, n_floaters_M, dens_floaters_M, 
                Group, Season_start) %>%
  na.omit() %>% as.data.frame()

#### thresholdanalysis() ----
# Song et al. (2026), Scientific Reports
# Works with GLMs
# Threshold of floater density, adjusted for the age of the dominant female
res_dens = logistic_threshold(ms_binary ~ dens_floaters_M + age_mating_dom,
                   data = d, var_name = "dens_floaters_M", rcs_nodes = 4)

# Threshold of age, adjusted for floater density
res_age = logistic_threshold(ms_binary ~ age_mating_dom + dens_floaters_M,
                   data = d, var_name = "age_mating_dom", rcs_nodes = 4)
thr_age = res_age$cutoff; thr_dens = res_dens$cutoff


#### glmertree() ------
# Works with GLMMs
tr = glmertree(ms_binary ~ 1 | (1 | Season_start) | age_mating_dom + dens_floaters_M,
               data = d, family = binomial)
plot(tr)


#### segmented() ------
# Works with GLMs
g = glm(ms_binary ~ age_mating_dom + dens_floaters_M, data = d, family = binomial)
davies.test(g, seg.Z = ~ age_mating_dom)
davies.test(g, seg.Z = ~ dens_floaters_M)


### Plots ----
m = glmmTMB(ms_binary ~ age_mating_dom + dens_floaters_M + (1 | Season_start), data = d, family = binomial)

# Odds ratios with 95% CI
broom.mixed::tidy(m, effects = "fixed", exponentiate = TRUE, conf.int = TRUE)

# X giving P(polygamy) = 50%, other variable at its mean
b = glmmTMB::fixef(m)$cond
x50_age  = -(b["(Intercept)"] + b["dens_floaters_M"] * mean(d$dens_floaters_M)) / b["age_mating_dom"]
x50_dens = -(b["(Intercept)"] + b["age_mating_dom"]  * mean(d$age_mating_dom))  / b["dens_floaters_M"]
x50_age; x50_dens

# Curves (the other variable is held at its mean by default)
pred_age  = ggpredict(m, terms = "age_mating_dom [all]")
pred_dens = ggpredict(m, terms = "dens_floaters_M [all]")

p_age = ggplot(pred_age, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill="orange") +
  geom_line(linewidth = 1) +
  geom_jitter(data = d, aes(age_mating_dom, ms_binary), height = 0.03, width = 0.5, alpha = 0.5, color="black") +
  labs(x = "Age of dominant female (months)", y = "P(polygamy)") +
  theme_classic()

p_dens = ggplot(pred_dens, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "orange") +
  geom_line(linewidth = 1) +
  geom_jitter(data = d, aes(dens_floaters_M, ms_binary), height = 0.03, width = 0.02, alpha = 0.5, color="black") +
  labs(x = "Male floater density (ind/km²)", y = "P(polygamy)") +
  theme_classic()

p_age; p_dens

# With threshold values
threshold_age  = 61.8 # months
threshold_dens = 0.49 # ind/km²

p_age_thr  = p_age  + geom_vline(xintercept = threshold_age,  linetype = "dashed", colour = "red3")
p_dens_thr = p_dens + geom_vline(xintercept = threshold_dens, linetype = "dashed", colour = "red3")

p_age_thr; p_dens_thr
