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
  )) %>% 
  dplyr::mutate(enn_log = log(enn_m+1),
                enn_scale = scale(enn_m))

# Distributions
hist(subset$enn_m)
hist(subset$enn_log)
hist(subset$enn_scale)
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


#### Isolation + Nb Floaters + Spatial & temporal structure ----
# Data
subset = data %>%
  dplyr::select(ms_binary, n_floaters, enn_m, Group, UMMPs, Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                UMMPs = factor(UMMPs),
                enn_scale = scale(enn_m),
                enn_log = log(enn_m+1),
                Season_start = as.numeric(Season_start))

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> N Floaters (Y1) -> mating system (Y2)
# With a random effect: Group|UMMPs

##### Choose the transformation and random structure on each submodel
# Same response -> AICc comparable across predictor transforms and random structures
f_log  = glmer(n_floaters ~ enn_log + (1 | Group), data = subset, family = poisson)
f_scale  = glmer(n_floaters ~ enn_scale + (1 | Group), data = subset, family = poisson)
f_group = glmer(n_floaters ~ enn_scale+ (1 | UMMPs/Group), data = subset, family = poisson)
f_group_time = glmer(n_floaters ~ enn_scale + (1 | Group) + (1 | Season_start), data = subset, family = poisson)
f_group_ummp = glmer(n_floaters ~ enn_scale + (1 | UMMPs/Group), data = subset, family = poisson)
f_group_ummp_time = glmer(n_floaters ~ enn_scale + (1 | UMMPs/Group) + (1 | Season_start), data = subset, family = poisson)
f_glm  = glm(n_floaters ~ enn_scale, data = subset, family = poisson)
MuMIn::AICc(f_log, f_scale, f_group, f_group_ummp, f_group_time, f_group_ummp_time, f_glm) %>% dplyr::arrange(AICc)

# Singular fit
isSingular(f_group_ummp)
# one or more random-effect variances were estimated at (or very near) zero, on the boundary of the possible values
VarCorr(f_group_ummp) # 0 variance on UMMPs
summary(f_group_ummp)$varcor
# Nested random effect (1 | UMMPs/Group) gave a singular fit: with only 4 UMMPs the
# between-UMMP variance is estimated at ~0, so the model is over-parameterized.

# Overdispersion
performance::check_overdispersion(f_group_time)

##### SEM
mod1_mixed = psem(
  glmmTMB(n_floaters ~ enn_scale + (1 | Group), data = subset, family = poisson),
  glmmTMB(ms_binary  ~ n_floaters + (1 | Group), data = subset, family = binomial),
  data = subset
)

##### Summary
summary(mod1_mixed)

##### Plot
# Advanced plot
ct = coefs(mod1_mixed, standardize.type = "latent.linear")
r2 = rsquared(mod1_mixed)   # Marginal = fixed effects only; Conditional = fixed + random

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
    paste0(lab[r2$Response[i]],
           "\nR²m = ", sprintf("%.2f", r2$Marginal[i]),
           " | R²c = ", sprintf("%.2f", r2$Conditional[i]))
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
