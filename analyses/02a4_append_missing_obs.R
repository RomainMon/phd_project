#------------------------------------------------#
# Author: Romain Monassier
# Objective: Missing rows in the long-term dataset
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(lubridate)

### Import datasets -----
# Long-term data corrected by Aurore Ponchon and me (UMMP correction)
load(here::here("outputs", "data", "glt", "data_clean_long_final3.RData"))

##############################################################
# RATIONALE: some infants/juveniles are recorded in a group after their recorded birth dates
# E.g., MA14 is born in 1998-09-27 but the first observation of the individual is 1999-04-22
# AIM: adding the individuals in the observations located between the recorded birth dates and the first observation in the group
##############################################################

### Parameters -----
infant_max_days = 60  # IN: age <= 60 days
juv_max_days = 300  # JU: 60 < age <= 300 days
min_gap_days = 1 # add rows only if the first observation is >= 1 day after birth
max_gap_days = 300 # ... and at most 300 days after birth
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490")

### 1. Prepare the data -----
dat = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(GLT = stringr::str_squish(GLT),
                DateObs_d = as.Date(DateObs),   # date version used for comparisons only
                BirthOK = as.Date(BirthOK))

### 2. First observation of each well-identified individual -----
# Birth date of the individual (earliest recorded value)
birth_tbl = dat %>%
  dplyr::filter(!is.na(BirthOK)) %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(BirthOK_ind = min(BirthOK), .groups = "drop")

first_obs = dat %>%
  # Only identified individuals, with a date
  dplyr::filter(!is.na(GLT), !GLT %in% bad, !is.na(DateObs_d)) %>%
  dplyr::group_by(GLT) %>%
  # First observation date over ALL records of the individual (even with Group = NA)
  dplyr::filter(DateObs_d == min(DateObs_d)) %>%
  # Groups recorded on that first date (an NA group counts as a value here)
  dplyr::mutate(n_groups_first = dplyr::n_distinct(Group)) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  # ONE group at the first observation, and not NA
  dplyr::filter(n_groups_first == 1, !is.na(Group)) %>%
  dplyr::left_join(birth_tbl, by = "GLT", relationship = "many-to-one") %>%
  dplyr::mutate(BirthOK = BirthOK_ind,
                first_date = DateObs_d,
                gap_days = as.numeric(first_date - BirthOK)) %>%
  # Seen AFTER its birth, but not too long after
  dplyr::filter(!is.na(BirthOK), gap_days >= min_gap_days, gap_days <= max_gap_days) %>%
  dplyr::select(-n_groups_first, -BirthOK_ind)

### 3. Dates when each group was observed -----
group_dates = dat %>%
  dplyr::filter(!is.na(Group)) %>%
  dplyr::arrange(Group, DateObs_d) %>%
  dplyr::distinct(Group, DateObs_d, .keep_all = TRUE) %>%
  dplyr::select(Group, DateObs_d, DateObs, DateObsMY, Year, Region, UMMPs, season, File)

### 4. Create the missing rows -----
new_rows = first_obs %>%
  # Individual information is kept, date-specific information comes from the group date
  dplyr::select(-dplyr::any_of(c("DateObs", "DateObs_d", "DateObsMY", "Year", "Region", "UMMPs",
                                 "season", "File", "ObsOrder", "IdadeOK", "gap_days"))) %>%
  dplyr::inner_join(group_dates,
                    by = dplyr::join_by(Group, BirthOK <= DateObs_d, first_date > DateObs_d),
                    relationship = "many-to-many") %>%
  # Never create a row on a date where the individual already has a record
  dplyr::anti_join(dplyr::distinct(dat, GLT, DateObs_d), by = c("GLT", "DateObs_d")) %>%
  dplyr::mutate(age_days = as.numeric(DateObs_d - BirthOK),
                IdadeOK = dplyr::case_when(age_days <= infant_max_days ~ "IN",
                                           age_days <= juv_max_days ~ "JU",
                                           TRUE ~ NA_character_),
                Disp = "0", Death = "0", Solo = "0",
                row_added = TRUE) %>%
  dplyr::filter(!is.na(IdadeOK))

### 5. Append to the dataset and recompute ObsOrder -----
data.clean.final = dat %>%
  dplyr::mutate(row_added = FALSE) %>%
  dplyr::bind_rows(dplyr::select(new_rows, dplyr::any_of(names(.)))) %>%
  # ObsOrder = rank of each record within the individual, by date
  dplyr::arrange(GLT, DateObs_d, Group) %>%
  dplyr::group_by(GLT) %>%
  dplyr::mutate(ObsOrder = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Group, DateObs_d, GLT) %>%
  dplyr::select(-DateObs_d)

### 6. Checks -----
nrow(new_rows)   # rows added
dplyr::n_distinct(new_rows$GLT)  # individuals completed
dplyr::count(new_rows, IdadeOK)

# Duplicates of identified individuals (GLT x date): after must equal before
dup_before = dat %>% dplyr::filter(!GLT %in% bad) %>%
  dplyr::count(GLT, DateObs) %>% dplyr::filter(n > 1)
dup_after = data.clean.final %>% dplyr::filter(!GLT %in% bad) %>%
  dplyr::count(GLT, DateObs) %>% dplyr::filter(n > 1)
nrow(dup_before); nrow(dup_after)  # same number = no duplicate created

# No added row for bad IDs, nor on a date already recorded for the individual
sum(new_rows$GLT %in% bad) # must be 0

### 7. Save -----
save(data.clean.final, file = here::here("outputs", "data", "glt", "data_clean_long_final4.RData"))
