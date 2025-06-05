#------------------------------------------------#
# Author: Romain Monassier
# Objective: Analyze IBGE data
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(readxl)
library(gtsummary)
library(here)

### Import data (Censo agropecuario) ------
base_path <- here("outputs", "data", "IBGE") 
tabela6753 = read_excel(file.path(base_path, "tabela6753_2017_clean.xlsx"), na = "NA")

### Statistical summary ------
# Replace NAs with 0
tabela6753_clean = tabela6753 %>%
  mutate(across(everything(), ~replace_na(., 0)))

# Turn specified columns into proportions
# Identify base columns and their targets
nb_base = "Nb_farm"
nb_cols = grep("^Nb_farm", colnames(tabela6753_clean), value = TRUE)
nb_targets = setdiff(nb_cols, nb_base)

area_base = "Area_farm"
area_cols = grep("^Area_farm", colnames(tabela6753_clean), value = TRUE)
area_targets = setdiff(area_cols, area_base)

# Create proportion columns
summary_table_prop = tabela6753_clean %>%
  dplyr::mutate(across(all_of(nb_targets), ~ ./get(nb_base))) %>%
  dplyr::mutate(across(all_of(area_targets), ~ ./get(area_base)))
