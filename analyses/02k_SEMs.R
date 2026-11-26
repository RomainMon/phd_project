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
library(tidychangepoint)

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
vars = c("age_mating_dom",
         "GS_total", "n_adults", 
         "n_immigrants", "n_immigrants_M", "n_immigrants_F", "n_emigrants", 
         "adult_turnover", "male_turnover",
         "sex_ratio", 
         "n_floaters", "n_floaters_M", "n_floaters_F",
         "n_glt_patch", "n_groups_patch", "lambda",
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
                dens_floaters, dens_floaters_F, dens_floaters_M,
                area_km2, shape, enn_m, dPC,
                n_glt_patch, n_groups_patch,
                dens_glt, dens_groups,
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

#### 5. Change-point analysis ----

# Check the number of observations per UMMPs and season
data %>%
  dplyr::mutate(
    mating_date_MMYY = lubridate::floor_date(
      mating_date, unit = "month"
    )
  ) %>%
  dplyr::group_by(UMMPs, mating_date_MMYY) %>%
  dplyr::summarise(
    n_distinct_floaters = dplyr::n_distinct(n_floaters_M, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(UMMPs, mating_date_MMYY) %>% 
  print(n=60)

# Check there aren't >1 measures of floaters per month-year
data %>%
  dplyr::mutate(
    mating_date_MMYY = lubridate::floor_date(mating_date, "month")
  ) %>%
  dplyr::group_by(UMMPs, mating_date_MMYY) %>%
  dplyr::summarise(
    n_records = n(),
    n_floaters_values = dplyr::n_distinct(n_floaters_M, na.rm = TRUE),
    n_density_values = dplyr::n_distinct(dens_floaters_M, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_floaters_values > 1 | n_density_values > 1)

# Build the temporal dataset
temporal_data = data %>%
  dplyr::mutate(
    mating_date_MMYY = lubridate::floor_date(mating_date, unit = "month")
  ) %>%
  dplyr::group_by(UMMPs, mating_date_MMYY) %>%
  dplyr::summarise(
    
    # Number of polygamous observations
    n_polygamy = sum(ms == "Polygamy", na.rm = TRUE),
    
    # Number of observations with known mating system
    n_observations = sum(!is.na(ms)),
    
    # Retain the floater count, without summing
    n_floaters_M = dplyr::first(n_floaters_M),
    
    # Retain the corresponding density
    dens_floaters_M = dplyr::first(dens_floaters_M),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(UMMPs, mating_date_MMYY)

# PLOT
# Scaling factor for the secondary axis
scale_factor <- max(temporal_data$n_polygamy, na.rm = TRUE) /
  max(temporal_data$n_floaters_M, na.rm = TRUE)

if (!is.finite(scale_factor) || scale_factor == 0) {
  scale_factor <- 1
}

p_temporal <- ggplot(
  temporal_data,
  aes(x = mating_date_MMYY)
) +
  
  # Polygamy occurrences
  geom_col(
    aes(y = n_polygamy),
    fill = "darkorange2",
    alpha = 0.65,
    width = 25
  ) +
  
  # Male floaters
  geom_line(
    aes(y = n_floaters_M * scale_factor),
    color = "steelblue4",
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  
  geom_point(
    aes(y = n_floaters_M * scale_factor),
    color = "steelblue4",
    size = 2,
    na.rm = TRUE
  ) +
  
  facet_wrap(
    ~ UMMPs,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_x_date(
    date_labels = "%b-%Y",
    date_breaks = "1 year",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  scale_y_continuous(
    name = "Number of polygamous observations",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Number of male floaters"
    )
  ) +
  
  labs(
    x = "Reference mating date",
    title = "Temporal dynamics of male floaters and polygamy",
    subtitle = "Polygamy occurrences (bars) and male floater abundance (line)"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.background = element_rect(
      fill = "grey90",
      color = NA
    ),
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(color = "darkorange3"),
    axis.title.y.right = element_text(color = "steelblue4")
  )

p_temporal

# Change-point analysis
# Floaters
# TO COMPLETE with tidychangepoint package



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
                enn_m) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(enn_scale = scale(enn_m))

# Response variable
glmm.hp::detect_glm_family(subset$n_floaters_M)
glmm.hp::detect_glm_family(subset$dens_floaters)
glmm.hp::detect_glm_family(subset$dens_floaters_M)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> Density Floaters (Y1) -> mating system (Y2)

##### Test nb. of floaters VS density
cands = c("n_floaters", "n_floaters_M", "n_floaters_F",
          "dens_floaters", "dens_floaters_M", "dens_floaters_F")

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
op = par(mfrow = c(2, 3)); for (v in cands) hist(d[[v]], main = v, xlab = ""); par(op)

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
g4 = glmmTMB(dens_floaters_M ~ enn_scale + (1 | Season_start), data = subset)
performance::check_overdispersion(g4)

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
                enn_m,
                area_km2,
                Group, 
                Season_start) %>%
  na.omit() %>%  # same rows in every submodel
  dplyr::mutate(Group = factor(Group),
                enn_scale = scale(enn_m),
                Season_start = factor(Season_start))

# Number of events
table(subset$ms_binary)

# Response variable
glmm.hp::detect_glm_family(subset$mean_days.month_supp)
glmm.hp::detect_glm_family(subset$mean_days.year_supp)

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> Floaters (Y1) -> mating system (Y2)
# Patch area (X2)      ->
# Supplementation (X3) -> 

##### Test supplementation variables
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
  glm(ms_binary ~ dens_floaters_M, data = subset, family = binomial),
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
  glm(ms_binary  ~ dens_floaters_M, data = subset, family = binomial),
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
  glmmTMB(n_immigrants_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = poisson),
  glm(ms_binary ~ n_immigrants_M + age_log, data = subset, family = binomial),
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

##### Path diagram
# Path diagram tested here:
# Patch isolation (X1) -> floaters (Y1) -> n. immigrants (Y2) -> mating system (Y3)
# Patch area (X2)      ->
#                                       Age dom. females (X3) -> 


##### SEM
mod12 = psem(
  glmmTMB(dens_floaters_M ~ enn_scale + area_km2 + (1 | Season_start), data = subset, family = gaussian),
  glmmTMB(n_immigrants_M ~ dens_floaters_M + enn_scale + (1 | Season_start), data = subset, family = poisson),
  glm(ms_binary ~ n_immigrants_M + age_log, data = subset, family = binomial),
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

#### Turnover + dens. floaters 2 ----
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

#### Floaters & age ----
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


#### Age & sex-ratio ----
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


#### Age, sex-ratio, floaters ----
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
  "dens floaters M + age dom. females" = mod22,
  "age dom. females" = mod23,
  "age dom.females + sex ratio" = mod24,
  "isolation + area + dens floaters M + age dom.females + sex ratio" = mod25
  )

aic_tab = do.call(rbind, lapply(mods, function(m) AIC_psem(m)))

aicc_col = grep("AICc", names(aic_tab), value = TRUE)[1] 
aic_tab$AICc = aic_tab[[aicc_col]]
aic_tab$delta = aic_tab$AICc - min(aic_tab$AICc)
aic_tab$weight = exp(-aic_tab$delta / 2) / sum(exp(-aic_tab$delta / 2))

aic_tab = aic_tab[order(aic_tab$delta), ]
aic_tab$cum_weight = cumsum(aic_tab$weight)
aic_tab[, c("K", "n", "AICc", "delta", "weight", "cum_weight")] %>% 
  round(3)