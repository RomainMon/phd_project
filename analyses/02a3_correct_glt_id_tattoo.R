#------------------------------------------------#
# Author: Romain Monassier
# Objective: Correct GLT identifier (GLT and Tattoo columns)
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(stringr)

### Import datasets -----
# Long-term data corrected by Aurore Ponchon and me (UMMP correction)
load(here::here("outputs", "data", "glt", "data_clean_long_final2.RData"))

### Parameters ------
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490")
use_tattoo_as_id = TRUE # TRUE: the column GLT receives the Tattoo (the scripts below keep working unchanged)

### Clean identifiers ------
data.clean.final = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(row_order = dplyr::row_number(), # to restore the original order
                # Real NA instead of "NA" or empty strings
                dplyr::across(c(GLT, Tattoo),
                              ~ dplyr::na_if(dplyr::na_if(stringr::str_squish(as.character(.x)), "NA"), "")),
                # Manual correction
                GLT = dplyr::if_else(GLT %in% "EM23", "E23", GLT),
                Tattoo_orig = Tattoo)

### Fill the missing Tattoo ------
# Rule: a NA Tattoo takes the Tattoo recorded for a GLT that gets a tattoo later on (previous record first, then next one). 
# BUT when the GLT is NEVER recorded with a Tattoo -> the tattoo gets "/GLT" (e.g., "/MS1")
# Unidentified individuals (bad) and records without GLT are left untouched
data.clean.final = data.clean.final %>%
  dplyr::arrange(GLT, DateObs, ObsOrder) %>%
  dplyr::group_by(GLT) %>%
  tidyr::fill(Tattoo, .direction = "downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Tattoo = dplyr::if_else(is.na(GLT) | GLT %in% bad, Tattoo_orig, Tattoo),
    Tattoo = dplyr::if_else(is.na(Tattoo) & !is.na(GLT) & !GLT %in% bad, paste0("/", GLT), Tattoo),
    Tattoo_source = dplyr::case_when(!is.na(Tattoo_orig) ~ "tattoo",
                                     startsWith(dplyr::coalesce(Tattoo, ""), "/") ~ "GLT_only",
                                     !is.na(Tattoo) ~ "filled_from_GLT",
                                     TRUE ~ NA_character_)) %>%
  dplyr::arrange(row_order) %>%
  dplyr::select(-row_order, -Tattoo_orig)

### Consistency checks ------
nrow(data.clean.final) # unchanged number of rows
dplyr::count(data.clean.final, Tattoo_source)

# GLT recorded with SEVERAL tattoos (the fill took the previous record: inspect them)
several_tattoos = data.clean.final %>%
  dplyr::filter(!is.na(GLT), !GLT %in% bad, Tattoo_source != "GLT_only") %>%
  dplyr::distinct(GLT, Tattoo) %>%
  dplyr::group_by(GLT) %>% dplyr::filter(dplyr::n() > 1) %>% 
  dplyr::ungroup()

# Tattoo shared by SEVERAL GLT names (e.g., ST40 / ST40.1: expected when the GLT name changes)
tattoo_shared = data.clean.final %>%
  dplyr::filter(!is.na(Tattoo), Tattoo_source != "GLT_only") %>%
  dplyr::distinct(Tattoo, GLT) %>%
  dplyr::group_by(Tattoo) %>% dplyr::filter(dplyr::n() > 1) %>% dplyr::ungroup()

# Individuals with a GLT but no Tattoo at all
no_tattoo = data.clean.final %>% 
  dplyr::filter(Tattoo_source == "GLT_only") %>% 
  dplyr::distinct(GLT, Tattoo)


### Export ------
save(data.clean.final, file = here::here("outputs", "data", "glt", "data_clean_long_final2.RData"))
