#------------------------------------------------#
# Author: Romain Monassier
# Objective: Data exploration
#------------------------------------------------#

### Packages -----
library(dplyr)
library(DataExplorer)

### Dataset ----
data = repro_final

# DataExplorer contains ~20 functions to explore a dataset

### Structure of the dataset ----
introduce(data)
plot_intro(data) 
plot_str(data)

### Missing data ----
plot_missing(data) 
profile_missing(data) %>% print(n=100)

### Distribution -----
plot_bar(data) # Qualitative data
plot_bar(data, by ="UMMPs") # By category
plot_histogram(data) # Quantitative data
plot_qq(data) 

### Relationships across variables -----
plot_correlation(data, cor_args = list("use" = "pairwise.complete.obs"))
# Qualitative data only
plot_correlation(data, cor_args = list("use" = "pairwise.complete.obs"), type="d") 
# Quantitative data only
plot_correlation(data, cor_args = list("use" = "pairwise.complete.obs"), type="c") 
# PCA
plot_prcomp(na.omit(data), nrow = 2L, ncol = 2L) 
# Relationship between quantiative and one qualitative variable
plot_boxplot(data, by="UMMPs") 
# Relationship between qualitative and one quantitative variable
plot_scatterplot(split_columns(data)$continuous, by = "n_repro_females") 
