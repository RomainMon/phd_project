#------------------------------------------------#
# Author: Romain Monassier
# Objective: Prepare long-term GLT data for subsequent ANALYSIS
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(xlsx)
library(terra)
library(sf)
library(lubridate)

### Import datasets -----

#### Long-term GLT data -----
# Long-term data corrected by Aurore Ponchon
load(here::here("data", "glt", "APonchon", "data_clean_long_final.RData"))

#### Mating system data -----
# Data regarding GLT behavior in fragmented patches
# From Andressa Sales Coelho thesis
# See: Conservação do mico-leão-dourado: Enfrentando os desafios de uma paisagem fragmentada. vol. 3 (Editora da Universidade Estadual do Norte Fluminense Darcy Ribeiro – UENF, Campos dos Goytacazes, RJ, 2008).
# Reproductive females and litters
reprod = xlsx::read.xlsx(
  here::here("data", "glt", "ACoelho", "data_clean", "dadosAndressa_V2.xlsx"),
  sheetName = "Sheet1"
)
# Dispersal
disp_andressa = xlsx::read.xlsx(
  here::here("data", "glt", "ACoelho", "data_clean", "dispersao_andressa_V2.xls"),
  sheetName = "Sheet1"
)
# Status when they were several reproductive females in the group
status = xlsx::read.xlsx(
  here::here("data", "glt", "ACoelho", "data_clean", "duasfemeas_andressa_V2.xls"),
  sheetName = "Sheet1"
)

#### Dispersal events -----
stays = group_flux = xlsx::read.xlsx(
  here::here("outputs", "data", "dispersal", "All_GLT_stays.xlsx"),
  sheetName = "Sheet1"
)
group_flux = xlsx::read.xlsx(
  here::here("outputs", "data", "dispersal", "Group_flux.xlsx"),
  sheetName = "Sheet1"
)

#### Group locations -----
# Group locations
regions = sf::st_read(here::here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions_csv = readr::read_csv2(
  here::here("data", "geo", "APonchon", "GLT", "RegionsName.csv"),
  locale = readr::locale(encoding = "UTF-8"),
  na = ""
)

#### Patches -----
# base_path = here::here("outputs", "data", "patches_rshifter") # REAL PATCHES
base_path = here::here("outputs", "data", "patchmetrics") # UMMP PATCHES
vect_files = list.files(base_path, pattern = "\\.gpkg$", full.names = TRUE)

# Extract years
years = stringr::str_extract(basename(vect_files), "(?<!\\d)\\d{4}(?!\\d)")
# Create a dataframe to link files and years
vector_df = data.frame(file = vect_files, year = as.numeric(years)) %>%
  dplyr::arrange(year)
# Load vectors in chronological order
patches = lapply(vector_df$file, sf::st_read)
years = vector_df$year
# Check
for (i in seq_along(patches)) {
  cat("Year", years[i], " → raster name:", basename(vector_df$file[i]), "\n")
}
names(patches) = vector_df$year # Name by year

##### Correspondence UUMP-Patch names -----
# We create a correspondence list between the names in the long-term dataset
# And the names of the UMMPs in the patch file
UMMP_crosswalk = data.frame(
  # UMMPs names in the long-term dataset
  UMMPs = c(
    "Uniao I",
    "Uniao II",
    "Poco das Antas",
    "Rio Vermelho",
    "Imbau II",
    "Aldeia I",
    "Aldeia II",
    "Imbau I",
    "Imbau III",
    "Pirineus",
    "Faz. Boa Esperanza",
    # UMMPs reassigned below (see "Correct UMMP")
    "4_Irmaos",
    "Recanto_Preservar",
    "Sao_Joao"
  ),
  # Equivalent in the patch file
  patch_id = c(
    "Uniao.I",
    "Uniao.II",
    "Poco.das.Antas",
    "Rio.Vermelho",
    "Imbau.II",
    "Aldeia.I",
    "Aldeia.II",
    "Imbau.I",
    "Imbau.III",
    "Pirineus",
    "Boa_Esperanca",
    "4_Irmaos",
    "Recanto_Preservar",
    "Sao_Joao"
  ),
  stringsAsFactors = FALSE
)

##### Unlist -----
# Unlist the patch metrics
patch_metrics = purrr::list_rbind(
  purrr::imap(
    patches,
    function(x, nm) {
      x %>%
        sf::st_drop_geometry() %>%
        dplyr::select(patch_id, area_ha, shape, enn_m, dPC) %>%
        dplyr::mutate(Year = as.integer(nm))
    }
  )
)

### Fill missing UMMPs of data.clean.final -----
# Accent-free, lower-case key, so "Rebio Uniao" and "Rebio União" match
norm_key = function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    stringr::str_replace_all("[._-]+", " ") %>%
    stringr::str_squish()
}

# Farm -> UMMPs lookup. UMMPs names are made accent-free, as in data.clean.final ("Uniao I")
farm_ummp = regions_csv %>%
  dplyr::filter(!is.na(Farm2), !is.na(UMMPs)) %>%
  dplyr::transmute(Region_key = norm_key(Farm2),
                   UMMPs_region = stringi::stri_trans_general(UMMPs, "Latin-ASCII")) %>%
  dplyr::distinct()

# UMMPs missing before
sum(is.na(data.clean.final$UMMPs))

data.clean.final = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(UMMPs = as.character(UMMPs),            # factor -> character
                Region_key = norm_key(Region)) %>%
  dplyr::left_join(farm_ummp, by = "Region_key", relationship = "many-to-one") %>%
  dplyr::mutate(UMMPs_filled = is.na(UMMPs) & !is.na(UMMPs_region),   # TRUE = UMMPs taken from regions
                UMMPs = dplyr::coalesce(UMMPs, UMMPs_region)) %>%
  dplyr::select(-Region_key, -UMMPs_region)

# Checks
sum(is.na(data.clean.final$UMMPs))  # UMMPs still missing

# Manual corrections
region_ummp_manual = dplyr::tribble(
  ~Region, ~UMMPs_manual,
  "Afetiva",  "Imbau II",
  "St. Professor", "Imbau I",
  "Rio Vermelho", "Rio Vermelho",
  "St. Joedi",  "Aldeia I",
  "Sitio Tertulio", "Aldeia I",
  "Sitio 15 de Marco", "Rio Vermelho"
) %>%
  dplyr::mutate(Region_key = norm_key(Region)) %>%
  dplyr::select(Region_key, UMMPs_manual)

data.clean.final = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(UMMPs = as.character(UMMPs),
                Region_key = norm_key(Region)) %>%
  dplyr::left_join(region_ummp_manual, by = "Region_key", relationship = "many-to-one") %>%
  dplyr::mutate(UMMPs_filled_manual = is.na(UMMPs) & !is.na(UMMPs_manual),  # TRUE = filled manually
                UMMPs = dplyr::coalesce(UMMPs, UMMPs_manual)) %>%
  dplyr::select(-Region_key, 
                -UMMPs_manual, 
                -UMMPs_filled,
                -UMMPs_filled_manual)

# Checks
sum(is.na(data.clean.final$UMMPs))  # UMMPs still missing

### Long-term dataset -----
#### Parameters ----------
# Problematic individuals
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490") # Last list of problematic individuals (see Aurore Zenodo)

#### Filter the long-term dataset -----
##### Filter out bad data -----
data.clean.filter = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    # !GLT %in% bad,
    # Disp!="1", # Dispersers
    Death !="1" # Dead individuals
    )

### Correct UMMP
# We reassign some groups to other UMMPs when relevant
# For instance, we consider 4 Irmaos as a distinct patch (not as part of Aldeia I)
# NB: this must be done BEFORE checking the correspondences
data.clean.filter = data.clean.filter %>%
  dplyr::mutate(
    UMMPs = dplyr::case_when(
      Region == "Sitio 4 Irmaos" ~ "4_Irmaos",
      Region == "Sitio Recanto Preservar" ~ "Recanto_Preservar",
      Region == "Faz. Sao Joao" ~ "Sao_Joao",
      TRUE ~ as.character(UMMPs)
    )
  )

##### Apply selection criteria ----
# Based on 02h_GLT_sampling effort.R

###### Criteria ----
# In which seasons should the observations occur?
criteria = "dry-dry"
# "dry-dry" for observations at the dry season only
# "dry-wet" for observations at both dry-wet seasons
# "annual" for any observation irrespective of the season

# How many min. observations are required?
min_obs_year = 0 # If the criteria is "annual" (should be 0 if dry-dry or dry-wet)
min_obs_dry = 1  # at least N distinct date in the dry season each year
min_obs_wet = 0 # at least N distinct date in the wet season each year (should be 0 if dry-dry)

# How many consecutive years are needed for a group to be retained?
threshold = 3

# Do we accept gaps in the monitoring?
# i.e., a gap is a year without any observation in a run
# for instance, 2002-2004-2005 with 2003 being a gap of 1 year
max_gap_years = 1

# TRUE = keep all observations of the selected group-years (all months)
# FALSE = keep only the observations that count for the criterion
keep_all_months = FALSE

# Years excluded because of Yellow Fever
excluded_years = 2017:2019

# If TRUE, excluded years do not break a run (2016 and 2020 are then consecutive)
bridge_excluded_years = FALSE

# Months defining the seasons
# Dry months (april, may, june) are typically months where groups are rather stable (no recruitment, less emigrations)
# Births occur from september to march
# Dispersal occurs from july to december mostly (see: dispersal data)
dry_months = 4:6
wet_months = c(12, 1:3)

stopifnot(
  length(criteria) == 1,
  criteria %in% c("annual", "dry-dry", "dry-wet"),
  min_obs_year >= 0, min_obs_dry >= 0, min_obs_wet >= 0,
  min_obs_year + min_obs_dry + min_obs_wet >= 1,
  threshold >= 1,
  max_gap_years >= 0
)
if (criteria == "dry-dry" && min_obs_wet > 0) {
  stop("criteria = 'dry-dry' only uses dry months: min_obs_wet must be 0")
}

# Months considered by the criterion
window_months = switch(
  criteria,
  "annual" = 1:12,
  "dry-dry" = dry_months,
  "dry-wet" = c(dry_months, wet_months)
)

###### Prepare the observations ----
data.prep = data.clean.filter %>%
  dplyr::filter(
    !is.na(Group), !is.na(UMMPs), !is.na(Year), !is.na(DateObs),
    # Remove YF years
    !Year %in% excluded_years
  ) %>%
  dplyr::mutate(
    Group = as.character(Group),
    UMMPs = as.character(UMMPs),
    Year = as.integer(as.character(Year)),
    DateObs = as.Date(DateObs),
    Month = lubridate::month(DateObs),
    # Annual cycle used to count consecutive years
    # dry-wet: Jan-Mar belong to the cycle of the previous year
    Cycle = if (criteria == "dry-wet") {
      as.integer(ifelse(Month <= 3, Year - 1L, Year))
    } else {
      Year
    },
    in_window = Month %in% window_months
  )

###### Qualifying years ----
# Count distinct observation dates per group and annual cycle
annual_counts = data.prep %>%
  # Keep only observations in the selected temporal window (e.g., dry months)
  dplyr::filter(in_window) %>%
  dplyr::distinct(UMMPs, Group, Cycle, Month, DateObs) %>%
  dplyr::group_by(UMMPs, Group, Cycle) %>%
  # Count the number of observations
  dplyr::summarise(
    n_observations = dplyr::n_distinct(DateObs),
    n_dry = dplyr::n_distinct(DateObs[Month %in% dry_months]),
    n_wet = dplyr::n_distinct(DateObs[Month %in% wet_months]),
    .groups = "drop"
  )

# Filter based on the min. nb of observations required
years = annual_counts %>%
  dplyr::filter(
    n_observations >= min_obs_year,
    n_dry >= min_obs_dry,
    n_wet >= min_obs_wet
  ) %>%
  dplyr::distinct(UMMPs, Group, Cycle) %>%
  dplyr::arrange(UMMPs, Group, Cycle)

###### Monitoring runs ----
# A new run starts when the gap between two qualifying years is larger than max_gap_years
# E.g., with max_gap_years=1, observations in 2000-2001-2003-2004 create two runs: 2000-2001 and 2003-2004
year_runs = years %>%
  dplyr::group_by(UMMPs, Group) %>%
  dplyr::arrange(Cycle, .by_group = TRUE) %>%
  dplyr::mutate(
    # Index used to detect gaps: optionally collapses the excluded years
    Cycle_index = if (bridge_excluded_years) {
      Cycle - vapply(Cycle, function(y) sum(excluded_years < y), integer(1))
    } else {
      Cycle
    },
    # Run id by group
    run_id = cumsum(
      dplyr::row_number() == 1 |
        (Cycle_index - dplyr::lag(Cycle_index)) > (max_gap_years + 1)
    )
  ) %>%
  dplyr::group_by(UMMPs, Group, run_id) %>%
  dplyr::summarise(
    # Number of qualifying years in the run
    n_consecutive_years = dplyr::n(),
    first_year = min(Cycle),
    last_year = max(Cycle),
    .groups = "drop"
  )

# Filter using the required number of years in a consecutive run
qualifying_runs = year_runs %>%
  dplyr::filter(n_consecutive_years >= threshold)

stopifnot("No run meets the threshold" = nrow(qualifying_runs) > 0)

# Qualifying group-years belonging to the retained runs
# (gap years inside a run are not included: they did not meet the criterion)
selected_group_years = qualifying_runs %>%
  dplyr::inner_join(
    years,
    by = dplyr::join_by(
      UMMPs, Group,
      between(y$Cycle, x$first_year, x$last_year)
    ),
    relationship = "one-to-many"
  ) %>%
  dplyr::mutate(
    run_uid = paste(UMMPs, Group, run_id, sep = " | ")
  ) %>%
  dplyr::select(
    UMMPs, Group, run_id, run_uid, Cycle,
    first_year, last_year, n_consecutive_years
  )

###### Filter the dataset ----
data.selected = data.prep

# Optionally keep only the months used by the criterion
if (!keep_all_months) {
  data.selected = data.selected %>%
    dplyr::filter(in_window)
}

# Keep observations belonging to the selected group-years
# (a group-year belongs to exactly one run, hence many-to-one)
data.selected = data.selected %>%
  dplyr::inner_join(
    selected_group_years,
    by = c("UMMPs", "Group", "Cycle"),
    relationship = "many-to-one"
  ) %>%
  dplyr::select(-in_window)

###### Checks ----
cat("\nCriterion:", criteria,
    "| min obs (year/dry/wet):", min_obs_year, "/", min_obs_dry, "/", min_obs_wet,
    "| threshold:", threshold, "years | max gap:", max_gap_years, "yr",
    "| keep all months:", keep_all_months, "\n")

data.selected %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_dates = dplyr::n_distinct(DateObs),
    n_groups = dplyr::n_distinct(UMMPs, Group),
    n_runs = dplyr::n_distinct(run_uid),
    n_individuals = dplyr::n_distinct(GLT),
    n_UMMPs = dplyr::n_distinct(UMMPs),
    year_min = min(Year),
    year_max = max(Year)
  )

#### Mutate variables-GLT -----
##### Composition -----
# Group-level variables (1 row = group x cycle x season)
season_data = data.selected %>%
  # Reassign the season based on the season categorization
  dplyr::mutate(
    season = dplyr::case_when(
      Month %in% dry_months ~ "Dry",
      Month %in% wet_months ~ "Wet",
      TRUE ~ NA_character_
    )
  ) %>%
  # Months outside both seasons (possible with keep_all_months = TRUE)
  dplyr::filter(!is.na(season))

### We could have several observations of the same group during one cycle
### Below, we compute the composition where the group size is at its maximum during the cycle
# Group size on each observation date
gs_date = season_data %>%
  dplyr::group_by(UMMPs, Group, run_uid, Cycle, season, DateObs) %>%
  dplyr::summarise(
    GS_date = dplyr::n_distinct(GLT),
    .groups = "drop"
  )

# Select the date with the maximum group size in each season
date_max = gs_date %>%
  dplyr::group_by(UMMPs, Group, run_uid, Cycle, season) %>%
  dplyr::mutate(
    # Number of observation dates in the season (sampling effort)
    n_dates = dplyr::n()
  ) %>%
  # Highest GS first; IF ties: select the most recent date
  dplyr::arrange(dplyr::desc(GS_date), dplyr::desc(DateObs), .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    GS = GS_date,
    date_max_GS = DateObs
  )

# Composition recorded on that date only
keys = c("UMMPs", "Group", "run_uid", "Cycle", "season")

composition = season_data %>%
  dplyr::inner_join(
    date_max %>% dplyr::select(dplyr::all_of(keys), date_max_GS),
    by = keys,
    relationship = "many-to-one"
  ) %>%
  # Only keep the observation where GS is max
  dplyr::filter(DateObs == date_max_GS) %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
  dplyr::summarise(
    # Number of males
    n_males = dplyr::n_distinct(
      GLT[SexOK %in% "M"], na.rm = TRUE
    ),
    # Number of females
    n_females = dplyr::n_distinct(
      GLT[SexOK %in% "F"], na.rm = TRUE
    ),
    # Number of immatures (infants and juveniles)
    n_immatures = dplyr::n_distinct(
      GLT[IdadeOK %in% c("IN", "JU")], na.rm = TRUE
    ),
    # Recruitment: number of newborns (infants)
    n_newborns = dplyr::n_distinct(
      GLT[IdadeOK %in% "IN"], na.rm = TRUE
    ),
    .groups = "drop"
  )

# Combine and compute the ratios
demography = date_max %>%
  dplyr::left_join(composition, by = keys, relationship = "one-to-one") %>%
  dplyr::mutate(
    # Sex ratio
    sex_ratio = dplyr::if_else(
      n_females > 0, n_males / n_females, NA_real_
    )
  )

##### Growth rate and turnover relative to the previous observation ----
# Value at year t = change between the previous survey (t-1, or earlier if gap) and t
# Same run, same season. NA for the first survey of each run

# Intervals between the previous and the current survey of the same group
intervals = demography %>%
  dplyr::select(
    UMMPs, Group, run_uid, season, Cycle,
    date_t = date_max_GS, 
    GS
  ) %>%
  # By season
  dplyr::group_by(UMMPs, Group, run_uid, season) %>%
  # Order chronologically
  dplyr::arrange(Cycle, .by_group = TRUE) %>%
  dplyr::mutate(
    Cycle_prev = dplyr::lag(Cycle), # Previous surveyed year
    date_prev = dplyr::lag(date_t), # Previous observation date
    GS_prev = dplyr::lag(GS)  # Previous GS
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Cycle_prev)) %>%
  dplyr::mutate(
    interval_id = dplyr::row_number(),
    n_years_prev = Cycle - Cycle_prev,  # Years between the two surveys
    mean_GS = (GS + GS_prev) / 2, # Mean GS over the two dates
    # Growth rate, annualised if the interval is longer than 1 year
    lambda = dplyr::if_else(
      GS_prev > 0, (GS / GS_prev)^(1 / n_years_prev), NA_real_
    )
  )

### Jaccard similarity of group composition
# Composition = individuals present on the snapshot date (date of maximum group size)
comp = season_data %>%
  dplyr::inner_join(
    demography %>%
      dplyr::select(UMMPs, Group, run_uid, Cycle, season, date_max_GS),
    by = c("UMMPs", "Group", "run_uid", "Cycle", "season"),
    relationship = "many-to-one"
  ) %>%
  dplyr::filter(DateObs == date_max_GS) %>%
  dplyr::group_by(UMMPs, Group, run_uid, Cycle, season) %>%
  # List of GLTs at both dates
  dplyr::summarise(
    members = list(unique(GLT)),
    .groups = "drop"
  )

keys = c("UMMPs", "Group", "run_uid", "season")

intervals = intervals %>%
  dplyr::left_join(comp, by = c(keys, "Cycle")) %>%
  dplyr::left_join(
    comp %>% dplyr::rename(Cycle_prev = Cycle, members_prev = members),
    by = c(keys, "Cycle_prev")
  ) %>%
  dplyr::mutate(
    # Individuals present at both surveys
    n_shared = purrr::map2_int(members_prev, members, ~ length(intersect(.x, .y))),
    # Individuals present at the previous survey but not at the current one
    n_lost = purrr::map2_int(members_prev, members, ~ length(setdiff(.x, .y))),
    # Individuals present at the current survey but not at the previous one
    n_gained = purrr::map2_int(members_prev, members, ~ length(setdiff(.y, .x))),
    jaccard = n_shared / (n_shared + n_lost + n_gained),
    # Turnover over the whole interval
    turnover_jaccard = 1 - jaccard
  )

#### Join to the dataset (by the CURRENT year) ----
demography = demography %>%
  dplyr::left_join(
    intervals %>%
      dplyr::select(
        UMMPs, Group, run_uid, season, Cycle,
        n_years_prev, mean_GS, lambda,
        n_shared, n_lost, n_gained, jaccard,
        turnover_jaccard
      ),
    by = c("UMMPs", "Group", "run_uid", "season", "Cycle"),
    relationship = "one-to-one"
  )

## Checks
demography %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_groups = dplyr::n_distinct(UMMPs, Group),
    n_runs = dplyr::n_distinct(run_uid),
    dplyr::across(
      c(GS, sex_ratio, n_newborns, lambda, jaccard),
      ~ mean(.x, na.rm = TRUE)
    )
  )

#### Join patch data to GLT dataset -----
# Join to the group-level data
demo_patch = demography %>%
  dplyr::mutate(
    # Calendar year of the sampling date (not the annual cycle)
    Year = lubridate::year(date_max_GS)
  ) %>%
  # UMMP -> patch_id
  dplyr::left_join(
    UMMP_crosswalk,
    by = "UMMPs"
  ) %>%
  # Patch metrics of the sampling year
  dplyr::left_join(
    patch_metrics,
    by = c("patch_id", "Year"),
    relationship = "many-to-one"
  )

### Checks
# Rows without patch_id
demo_patch %>%
  dplyr::filter(is.na(patch_id)) %>%
  dplyr::distinct(UMMPs)

# Rows with a patch_id but without metrics (no map for that year, or patch absent from it)
demo_patch %>%
  dplyr::filter(!is.na(patch_id), is.na(area_ha)) %>%
  dplyr::count(patch_id, Year)


### Mating system data -----
# From Andressa Coelho thesis

#### At the female level -----

##### Parameters ------
## Definition of a reproductive season (the resolution used)
# Reproductive season: from season_first_month (year t) to season_last_month (year t+1)
season_first_month = 7 # July (some reproductions are recorded in July)
season_last_month = 3 # March
# Events outside the reproductive season
status %>%
  dplyr::filter(!lubridate::month(Date) %in% c(season_first_month:12, 1:season_last_month)) %>%
  dplyr::count(Month = lubridate::month(Date))
# NB: the same first month defines the "population year" (July t to June t+1) used for patch counts, so that Season_start means the same thing in all the tables below

max_pref_before = 8 # months: a month AT or BEFORE the target is preferred up to this distance
max_pref_after = 2 # months: for April, a month AT or AFTER the target is preferred up to this distance
lambda_max = 6 # lambda above this value is considered extreme

##### Helpers ------
# Month index, to compute distances in months
month_idx = function(d) lubridate::year(d) * 12 + lubridate::month(d)

# Population year (July t - June t+1) of a date, identified by its starting year
pop_year = function(d) {
  dplyr::if_else(lubridate::month(d) >= season_first_month, 
                 lubridate::year(d), 
                 lubridate::year(d) - 1)
}

# For each reproduction record: select the group x month value closest to a target date
# - x: needs repro_id, Group, UMMPs and the target date column
# - table to look at: needs Group, UMMPs, ref_month (1st of the month) + the variables to bring
# - prefer = "before": a month AT or BEFORE the target is preferred, 
#   otherwise the closest month (i.e., later on); prefer = "after": the reverse
# - suffix is added to the brought variables; 'diag_' columns are kept for checks
closest_month = function(x, lookup, target_col, tag, prefer = "before", max_pref = 8, suffix = "") {
  sign = if (prefer == "before") 1 else -1
  x %>%
    dplyr::select(repro_id, Group, UMMPs, target = dplyr::all_of(target_col)) %>%
    dplyr::left_join(dplyr::rename(lookup, ref_UMMPs = UMMPs), by = "Group", relationship = "many-to-many") %>%
    dplyr::mutate(
      # > 0 = month on the preferred side of the target; 0 = same month
      dist_months = sign * (month_idx(target) - month_idx(ref_month)),
      priority = dplyr::case_when(dist_months >= 0 & dist_months <= max_pref ~ 1L,
                                  !is.na(dist_months) ~ 2L)
    ) %>%
    dplyr::group_by(repro_id) %>%
    # Priority, then closest month; at equal distance, same UMMPs first
    dplyr::arrange(priority, abs(dist_months), dplyr::desc(ref_UMMPs %in% UMMPs), .by_group = TRUE) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Group, -UMMPs, -target, -priority) %>%
    dplyr::rename_with(~ paste0(.x, suffix), -c(repro_id, ref_month, ref_UMMPs, dist_months)) %>%
    dplyr::rename_with(~ paste0("diag_", .x, "_", tag), c(ref_month, ref_UMMPs, dist_months))
}

##### By GLT/month -----
# One row = one female (GLT) x one reproductive event (month)
# Source of the litter/age/status: status data (GLT level)
# Source of the fragment/patch/supplementation: reprod data (Group level)

repro_month = status %>%
  dplyr::mutate(
    Age_months = as.numeric(Age_months),
    Age_years = round(Age_months / 12, 1),
    Year = lubridate::year(Date), # calendar year of the event
    Month = lubridate::month(Date),
    Month_name = month.abb[Month],
    # Season start = if the month of the record is above July (e.g., September) -> then it's the same year
    # if the month is below March -> then it's the year -1
    # e.g., the Season start for March 2000 is 1999
    # The season 1999 = July 1999-March 2000 (for instance) -> see variable "Season"
    Season_start = dplyr::case_when(Month >= season_first_month ~ Year,
                                    Month <= season_last_month ~ Year - 1),
    Season = dplyr::if_else(is.na(Season_start), NA_character_,
                            paste0("wet ", Season_start, "-", Season_start + 1)),
    # Litter characteristics
    litter_size = n_infants,
    litter_prop_males = dplyr::if_else(n_in_males + n_in_females > 0,
                                       n_in_males / (n_in_males + n_in_females), NA_real_),
    litter_surv_1y = dplyr::if_else(n_infants > 0, n_infants_surv_1y / n_infants, NA_real_)
  ) %>%
  # Add group x year information
  dplyr::left_join(
    reprod %>%
      dplyr::select(Group, Year, UMMPs, Fragment, IQH, IQHS,
                    n_days_supp_food, n_months_supp_food, mean_days.month_supp, mean_days.year_supp),
    by = c("Group", "Year"), relationship = "many-to-one"
  ) %>%
  dplyr::rename(Dominance = Status) %>%
  dplyr::arrange(GLT, Date) %>%
  dplyr::mutate(repro_id = dplyr::row_number()) %>% # one id per record, kept until the end
  dplyr::relocate(repro_id, GLT, Group, Date, Year, Month, Month_name, Season, Season_start,
                  Age_months, Age_years, Dominance,
                  litter_size, n_in_males, n_in_females, n_in_sex_unknown,
                  litter_prop_males, n_infants_surv_1y, litter_surv_1y, infant_surv_cat,
                  UMMPs, Fragment, IQH, IQHS) %>%
  dplyr::select(-n_infants)

##### Status of the female ------
# Here, we use the "stays" dataset (see script 02a_disp_events)
# The reproduction Date is the 1st of the month, while STAYS start on exact dates
# HENCE: a stay is matched from its START MONTH
# Each record takes: 
# 1) the stay containing the date, 
# 2) otherwise the closest stay of the SAME group,
# 3) otherwise the closest stay (any), e.g., when the long-term data start after the reproduction record
stays_join = stays %>%
  dplyr::mutate(start_month = lubridate::floor_date(start, "month")) %>%
  dplyr::select(GLT, 
                stay_Group = Group, 
                stay, status_in_grp, natal_group,
                start_in_grp = start, 
                start_month, 
                end_in_grp = end, 
                in_grp_days = duration_days)

repro_stay = repro_month %>%
  dplyr::select(repro_id, GLT, Group, Date) %>%
  dplyr::left_join(stays_join, by = "GLT", relationship = "many-to-many") %>%
  dplyr::mutate(
    group_match = Group == stay_Group,
    # Distance (days) between the date and the stay (0 = within the stay)
    dist_days = dplyr::case_when(Date < start_month ~ as.numeric(start_month - Date),
                                 Date > end_in_grp ~ as.numeric(Date - end_in_grp),
                                 !is.na(stay) ~ 0),
    priority = dplyr::case_when(dist_days == 0 ~ 1L,
                                group_match %in% TRUE ~ 2L,
                                !is.na(dist_days) ~ 3L)
  ) %>%
  dplyr::group_by(repro_id) %>%
  # Mutate the first value (by priority order)
  dplyr::arrange(priority, dplyr::desc(group_match %in% TRUE), dist_days, .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  # dplyr::mutate(diag_stay_match = dplyr::case_when(is.na(stay) ~ "no_stay",
  #                                                  priority == 1L ~ "within_stay",
  #                                                  priority == 2L ~ "closest_same_group",
  #                                                  TRUE ~ "closest_other")) %>%
  dplyr::select(repro_id, 
                status_in_grp, 
                natal_group, 
                start_in_grp, 
                end_in_grp, 
                in_grp_days)

repro_final = repro_month %>%
  dplyr::left_join(repro_stay, by = "repro_id", relationship = "one-to-one")

##### Immigrants and emigrants (group flux) ------
# HERE, we use the GROUP FLUX data (see script 02a) which were computed at any date

# Aggregate flux per group BY MONTH
group_flux_month = group_flux %>%
  dplyr::mutate(ref_month = lubridate::floor_date(DateObs, "month")) %>%
  dplyr::group_by(Group, UMMPs, ref_month) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("n_"), sum), .groups = "drop")

# Month of the reproduction if available, otherwise the closest month BEFORE (up to max_pref_before),
# otherwise the closest in time
repro_final = repro_final %>%
  dplyr::left_join(closest_month(repro_final, group_flux_month, "Date", "flux", "before", max_pref_before),
                   by = "repro_id", 
                   relationship = "one-to-one") %>% 
  dplyr::select(-c(diag_ref_UMMPs_flux,
                   diag_dist_months_flux)) %>% 
  dplyr::rename(ref_for_flux = diag_ref_month_flux)

##### Group size and composition ------
# HERE, we use the long-term monitoring dataset
# Compute per group x month:
# the date with the MAXIMUM group size (GS), the composition recorded on that date only, and the sampling effort (n_dates)
group_month = data.clean.final %>%
  dplyr::ungroup() %>%
  # Remove individuals without group
  # Remove Infants
  dplyr::filter(!is.na(Group), 
                !IdadeOK %in% c("IN","JU")) %>%
  dplyr::mutate(UMMPs = as.character(UMMPs)) %>%
  # Composition on each date 
  dplyr::group_by(UMMPs, Group, ref_month = lubridate::floor_date(DateObs, "month"), DateObs) %>%
  # unidentified records count in GS but not in the sexes
  dplyr::summarise(GS = dplyr::n_distinct(GLT[!GLT %in% bad]) + sum(GLT %in% bad),
                   n_males = dplyr::n_distinct(GLT[SexOK %in% "M" & !GLT %in% bad]),
                   n_females = dplyr::n_distinct(GLT[SexOK %in% "F" & !GLT %in% bad]),
                   .groups = "drop") %>%
  # Date with the maximum GS in the month
  dplyr::group_by(UMMPs, Group, ref_month) %>%
  dplyr::mutate(n_dates = dplyr::n()) %>%
  dplyr::arrange(dplyr::desc(GS), dplyr::desc(DateObs), .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  # Compute sex ratio
  dplyr::mutate(sex_ratio = round(dplyr::if_else(n_females > 0, n_males / n_females, NA_real_), 2)) %>%
  dplyr::select(-DateObs)

repro_final = repro_final %>%
  dplyr::mutate(target_before = Date %m-% months(1), # one month before the reproduction
                target_after = lubridate::make_date(Season_start + 1, 4, 1)) %>% # April after the season
  dplyr::left_join(closest_month(., group_month, "target_before", "before", "before", max_pref_before, "_before"),
                   by = "repro_id", relationship = "one-to-one") %>%
  dplyr::left_join(closest_month(., group_month, "target_after", "after", "after", max_pref_after, "_after"),
                   by = "repro_id", relationship = "one-to-one") %>%
  dplyr::select(-c(target_before, 
                   target_after,
                   diag_ref_UMMPs_before,
                   diag_ref_UMMPs_after,
                   diag_dist_months_before,
                   diag_dist_months_after)) %>% 
  dplyr::rename(ref_for_gs_before = diag_ref_month_before,
                ref_for_gs_after = diag_ref_month_after)

##### Patch population by UMMPs ------
rec = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(UMMPs = as.character(UMMPs),
                Season_start = pop_year(DateObs))

# Individuals, groups and monitoring effort in the patch
pop_patch = rec %>%
  dplyr::group_by(UMMPs, 
                  # POP SIZE AT THE BEGINNING OF THE MATING SEASON
                  Season_start) %>%
  dplyr::summarise(n_glt_patch = dplyr::n_distinct(GLT), # including bad individuals
                   n_groups_patch = dplyr::n_distinct(Group),
                   n_dates_patch = dplyr::n_distinct(DateObs), # monitoring effort
                   .groups = "drop")

# Population trend
# When the lambda is extreme, the season BEFORE it is considered undersampled:
# its counts are replaced by the mean of the previous and following seasons (pop_infer = TRUE)
pop_patch = pop_patch %>%
  dplyr::mutate(n_glt_patch = as.numeric(n_glt_patch),
                n_groups_patch = as.numeric(n_groups_patch)) %>%
  dplyr::arrange(UMMPs, Season_start) %>%
  dplyr::group_by(UMMPs) %>%
  dplyr::mutate(
    # Original counts, kept for reference
    n_glt_raw = n_glt_patch,
    n_groups_raw = n_groups_patch,
    # Consecutive seasons?
    has_prev = dplyr::lag(Season_start, default = -Inf) == Season_start - 1,
    has_next = dplyr::lead(Season_start, default = Inf) == Season_start + 1,
    # First lambda: N(season) / N(previous season)
    lambda_0 = dplyr::if_else(has_prev & dplyr::lag(n_glt_patch) > 0,
                              n_glt_patch / dplyr::lag(n_glt_patch), NA_real_),
    # Extreme lambda in the next season -> this season is inferred (needs both neighbours)
    pop_infer = has_prev & has_next & (dplyr::lead(lambda_0) > lambda_max) %in% TRUE,
    n_glt_patch = dplyr::if_else(pop_infer, round((dplyr::lead(n_glt_raw) + dplyr::lag(n_glt_raw)) / 2), n_glt_raw),
    n_groups_patch = dplyr::if_else(pop_infer, round((dplyr::lead(n_groups_raw) + dplyr::lag(n_groups_raw)) / 2), n_groups_raw),
    # Lambda recomputed on the corrected counts
    lambda = dplyr::if_else(has_prev & dplyr::lag(n_glt_patch) > 0,
                            n_glt_patch / dplyr::lag(n_glt_patch), NA_real_)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-has_prev, -has_next, -lambda_0)

##### Patch metrics and densities ------
# Patch metrics of the closest year, same year or earlier
# To force an exact year match instead: dplyr::join_by(patch_id, Season_start == patch_year)
pop_patch = pop_patch %>%
  dplyr::left_join(UMMP_crosswalk, by = "UMMPs") %>%
  dplyr::left_join(patch_metrics,
                   by = c("patch_id", "Season_start"="Year")) %>%
  dplyr::mutate(
    area_km2 = area_ha / 100,
    # Densities (individuals or groups per km2 of patch)
    dens_glt = round(n_glt_patch / area_km2, 2),
    dens_groups = round(n_groups_patch / area_km2, 2)
  )

repro_final = repro_final %>%
  dplyr::left_join(pop_patch, by = c("UMMPs", "Season_start"), 
                   relationship = "many-to-one")

##### Floaters present in the patch at the date of reproduction ------
# Floaters (total and by sex): individuals with a satellite state
floaters = stays %>%
  dplyr::group_by(GLT) %>%
  dplyr::arrange(stay, .by_group = TRUE) %>%
  tidyr::fill(UMMPs, .direction = "downup") %>%
  dplyr::ungroup() %>%
  dplyr::filter(stay_type == "satellite", !is.na(UMMPs)) %>%
  dplyr::transmute(sat_GLT = GLT, sat_SexOK = SexOK, sat_UMMPs = UMMPs,
                   # Reproduction dates are the 1st of the month: the period starts at the 1st of the start month
                   sat_start = lubridate::floor_date(start, "month"),
                   sat_end = end)

# Individuals whose satellite period includes the reproduction date, in the same patch
# The reproducing female herself is excluded 
floaters_repro = repro_final %>%
  dplyr::select(repro_id, GLT, UMMPs, Date) %>%
  dplyr::inner_join(floaters,
                    by = dplyr::join_by(UMMPs == sat_UMMPs, dplyr::between(Date, sat_start, sat_end)),
                    relationship = "many-to-many") %>%
  dplyr::filter(GLT != sat_GLT) %>%
  dplyr::group_by(repro_id) %>%
  dplyr::summarise(n_floaters = dplyr::n_distinct(sat_GLT),
                   n_floaters_F = dplyr::n_distinct(sat_GLT[sat_SexOK %in% "F"]),
                   n_floaters_M = dplyr::n_distinct(sat_GLT[sat_SexOK %in% "M"]),
                   n_floaters_unknown = dplyr::n_distinct(sat_GLT[is.na(sat_SexOK)]),
                   .groups = "drop")

repro_final = repro_final %>%
  dplyr::left_join(floaters_repro, by = "repro_id", relationship = "one-to-one") %>%
  dplyr::mutate(
    # No satellite period found = 0 floaters (only when the patch is known)
    dplyr::across(dplyr::starts_with("n_floaters"), ~ dplyr::if_else(is.na(UMMPs), NA_integer_, tidyr::replace_na(.x, 0L))),
    dens_floaters = round(n_floaters / area_km2, 2),
    dens_floaters_F = round(n_floaters_F / area_km2, 2),
    dens_floaters_M = round(n_floaters_M / area_km2, 2)
  )

##### Check ----
# Example
example = repro_final %>% 
  dplyr::filter(repro_id == 46)

#### At the group level -----
# Dataset = "reprod"

##### Parameters ------
lambda_max = 6 # lambda above this value is considered extreme
# Dry and wet months
dry_months = 4:6
wet_months = c(7:12, 1:3)
# Reproductive season: from season_first_month (year t) to season_last_month (year t+1)
season_first_month = 7 # July (some reproductions are recorded in July)
season_last_month = 3 # March
# Problematic individuals
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490") # Last list of problematic individuals (see Aurore Zenodo)

##### Helper ----
# Population year (July t - June t+1) of a date, identified by its starting year
pop_year = function(d) {
  dplyr::if_else(lubridate::month(d) >= season_first_month, 
                 lubridate::year(d), 
                 lubridate::year(d) - 1)
}

# Most frequent non-NA value 
# (used to summarise UMMPs over a stay, so that a few mis-aligned records do not decide the UMMPs of a whole stay)
mode_ok = function(x) {
  x = x[!is.na(x)]
  if (length(x) == 0) NA_character_ else names(which.max(table(x)))
}

##### Patch population by UMMPs ------
rec = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(UMMPs = as.character(UMMPs)) %>% 
  dplyr::filter(!is.na(UMMPs))

# Individuals, groups and monitoring effort in the patch
pop_patch = rec %>%
  dplyr::group_by(UMMPs,
                  Year) %>%
  dplyr::summarise(n_glt_patch = dplyr::n_distinct(GLT), # including bad individuals
                   n_groups_patch = dplyr::n_distinct(Group),
                   n_dates_patch = dplyr::n_distinct(DateObs), # monitoring effort
                   .groups = "drop")

# Population trend
# When the lambda is extreme, the year BEFORE it is considered undersampled:
# its counts are replaced by the mean of the previous and following years (pop_infer = TRUE)
pop_patch = pop_patch %>%
  dplyr::mutate(n_glt_patch = as.numeric(n_glt_patch),
                n_groups_patch = as.numeric(n_groups_patch)) %>%
  dplyr::arrange(UMMPs, Year) %>%
  dplyr::group_by(UMMPs) %>%
  dplyr::mutate(
    # Original counts, kept for reference
    n_glt_raw = n_glt_patch,
    n_groups_raw = n_groups_patch,
    # Consecutive years?
    has_prev = dplyr::lag(Year, default = -Inf) == Year - 1,
    has_next = dplyr::lead(Year, default = Inf) == Year + 1,
    # First lambda: N(Year) / N(previous Year)
    lambda_0 = dplyr::if_else(has_prev & dplyr::lag(n_glt_patch) > 0,
                              n_glt_patch / dplyr::lag(n_glt_patch), NA_real_),
    # Extreme lambda in the next Year -> this Year is inferred (needs both neighbours)
    pop_infer = has_prev & has_next & (dplyr::lead(lambda_0) > lambda_max) %in% TRUE,
    n_glt_patch = dplyr::if_else(pop_infer, round((dplyr::lead(n_glt_raw) + dplyr::lag(n_glt_raw)) / 2), n_glt_raw),
    n_groups_patch = dplyr::if_else(pop_infer, round((dplyr::lead(n_groups_raw) + dplyr::lag(n_groups_raw)) / 2), n_groups_raw),
    # Lambda recomputed on the corrected counts
    lambda = dplyr::if_else(has_prev & dplyr::lag(n_glt_patch) > 0,
                            n_glt_patch / dplyr::lag(n_glt_patch), NA_real_)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-has_prev, -has_next, -lambda_0)

##### Patch metrics and densities ------
pop_patch = pop_patch %>%
  dplyr::left_join(UMMP_crosswalk, by = "UMMPs") %>%
  dplyr::left_join(patch_metrics,
                   by = c("patch_id", "Year")) %>%
  dplyr::mutate(
    area_km2 = area_ha / 100,
    # Densities (individuals or groups per km2 of patch)
    dens_glt = round(n_glt_patch / area_km2, 2),
    dens_groups = round(n_groups_patch / area_km2, 2)
  )

# Join
reprod_patch = reprod %>%
  dplyr::left_join(pop_patch, by = c("UMMPs", "Year"), 
                   relationship = "many-to-one")

# Remove columns
reprod_patch = reprod_patch %>% 
  dplyr::select(-c(n_glt_raw,
                   n_groups_raw,
                   pop_infer))

##### Group composition -----
# HERE, we use the long-term monitoring dataset
# Compute per group x season: the mean group size (GS)
group_season = data.clean.final %>%
  dplyr::ungroup() %>%
  # Remove individuals without group, and infants/juveniles
  dplyr::filter(!is.na(Group),
                !IdadeOK %in% c("IN", "JU")) %>%
  dplyr::mutate(
    UMMPs = as.character(UMMPs),
    DateObs = as.Date(DateObs),
    Year_obs = lubridate::year(DateObs),
    Month = lubridate::month(DateObs),
    season_type = dplyr::case_when(Month %in% dry_months ~ "Dry",
                                   Month %in% wet_months ~ "Wet",
                                   TRUE ~ NA_character_),
    # First calendar year of the season: wet months before the first wet month (e.g., Jan-March before JULY) belong to the wet season that started the year before
    Season_start = dplyr::if_else(season_type %in% "Wet" & Month < season_first_month,
                                  Year_obs - 1L, Year_obs),
    # e.g., "wet 2009-2010", "dry 2010", "wet 2010-2011"
    season_id = dplyr::case_when(season_type == "Wet" ~ paste0("wet ", Season_start, "-", Season_start + 1),
                                 season_type == "Dry" ~ paste0("dry ", Season_start))
  ) %>%
  # Composition on each date
  dplyr::group_by(UMMPs, Group, season_id, Season_start, season_type, DateObs) %>%
  # unidentified records count in GS but not in the sexes
  dplyr::summarise(GS = dplyr::n_distinct(GLT[!GLT %in% bad]) + sum(GLT %in% bad),
                   n_males = dplyr::n_distinct(GLT[SexOK %in% "M" & !GLT %in% bad]),
                   n_females = dplyr::n_distinct(GLT[SexOK %in% "F" & !GLT %in% bad]),
                   .groups = "drop") %>%
  # One row per group x season
  dplyr::group_by(UMMPs, Group, season_id, Season_start, season_type) %>%
  dplyr::summarise(n_dates = dplyr::n(),
                   mean_GS = round(mean(GS), 2),
                   max_GS = max(GS),
                   mean_males = round(mean(n_males), 2),
                   mean_females = round(mean(n_females), 2),
                   sex_ratio = dplyr::if_else(mean_females > 0, round(mean_males / mean_females, 2), NA_real_),
                   .groups = "drop") %>%
  dplyr::arrange(Group, Season_start, season_type)

# Join to the dataset
# WARNING: several rows joined to each group because composition data were computed per season (two seasons/year)
reprod_compo = reprod_patch %>%
  dplyr::left_join(group_season, by = c("UMMPs", "Group", "Year" = "Season_start"),
                   relationship = "one-to-many")


##### Immigrants and emigrants (group flux) ------
# HERE, we use the GROUP FLUX data (see script 02a) which were computed at any date
# Aggregate flux per group BY SEASON
group_flux_season = group_flux %>%
  dplyr::mutate(
    DateObs = as.Date(DateObs),
    Year_obs = lubridate::year(DateObs),
    Month = lubridate::month(DateObs),
    season_type = dplyr::case_when(Month %in% dry_months ~ "Dry",
                                   Month %in% wet_months ~ "Wet",
                                   TRUE ~ NA_character_),
    # First calendar year of the season: wet months before the first wet month (e.g., Jan-March before JULY) belong to the wet season that started the year before
    Season_start = dplyr::if_else(season_type %in% "Wet" & Month < season_first_month,
                                  Year_obs - 1L, Year_obs),
    # e.g., "wet 2009-2010", "dry 2010", "wet 2010-2011"
    season_id = dplyr::case_when(season_type == "Wet" ~ paste0("wet ", Season_start, "-", Season_start + 1),
                                 season_type == "Dry" ~ paste0("dry ", Season_start))
  ) %>%
  dplyr::group_by(Group, UMMPs, season_type, Season_start, season_id) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("n_"), sum), .groups = "drop")

# Join to the dataset
# WARNING: several rows joined to each group because composition data were computed per season (two seasons/year)
reprod_disp = reprod_compo %>%
  dplyr::rename(n_immig_andressa = n_immigrants,
                n_emig_andressa = n_emigrants) %>% 
  dplyr::left_join(group_flux_season, by = c("UMMPs", "Group", "season_id", "season_type"))


##### Floaters ------
# Region and UMMPs recorded on each GLT x date (one row per GLT x date, no duplicates)
rec_loc = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(DateObs = as.Date(DateObs), UMMPs = as.character(UMMPs)) %>%
  dplyr::group_by(GLT, DateObs) %>%
  dplyr::summarise(rec_Region = mode_ok(Region), rec_UMMPs = mode_ok(UMMPs), .groups = "drop")

# Satellite periods (one row = one satellite stay)
floaters = stays %>%
  dplyr::filter(stay_type == "satellite") %>%
  dplyr::left_join(rec_loc, by = c("GLT", "start" = "DateObs"), relationship = "many-to-one") %>%
  dplyr::transmute(sat_GLT = GLT,
                   sat_SexOK = SexOK,
                   sat_Region = rec_Region,
                   sat_UMMPs = dplyr::coalesce(UMMPs, rec_UMMPs),
                   # reference dates are the 1st of a month: the period starts at the 1st of the start month
                   sat_start = lubridate::floor_date(start, "month"),
                   sat_end = end) %>%
  dplyr::filter(!is.na(sat_UMMPs))

# Reference date of each row: start of the wet season (July) or of the dry season (April)
# 'Year' is the start year of the season (Season_start)
reprod_disp = reprod_disp %>%
  dplyr::mutate(row_id = dplyr::row_number(),
                ref_date = dplyr::case_when(season_type == "Wet" ~ lubridate::make_date(Year, season_first_month, 1),
                                            season_type == "Dry" ~ lubridate::make_date(Year, 4, 1)),
                # WINDOW
                fl_start = ref_date %m-% months(3), # Floaters are counted from N months before the season to the beginning of the season
                fl_end = ref_date)

# Floaters of the patch whose satellite period overlaps the chosen window
floaters_gs = reprod_disp %>%
  dplyr::select(row_id, UMMPs, fl_start, fl_end) %>%
  dplyr::inner_join(floaters,
                    by = dplyr::join_by(UMMPs == sat_UMMPs, 
                                        fl_end >= sat_start, 
                                        fl_start <= sat_end),
                    relationship = "many-to-many") %>%
  dplyr::group_by(row_id) %>%
  dplyr::summarise(n_floaters = dplyr::n_distinct(sat_GLT),
                   n_floaters_F = dplyr::n_distinct(sat_GLT[sat_SexOK %in% "F"]),
                   n_floaters_M = dplyr::n_distinct(sat_GLT[sat_SexOK %in% "M"]),
                   n_floaters_unknown = dplyr::n_distinct(sat_GLT[is.na(sat_SexOK)]),
                   .groups = "drop")

# Join to data
reprod_floaters = reprod_disp %>%
  dplyr::left_join(floaters_gs, by = "row_id", relationship = "one-to-one") %>%
  dplyr::mutate(
    # No satellite period found = 0 floaters (only when the patch and the season are known)
    dplyr::across(dplyr::starts_with("n_floaters"),
                  ~ dplyr::if_else(is.na(UMMPs) | is.na(ref_date), NA_integer_, tidyr::replace_na(.x, 0L))),
    dens_floaters = round(n_floaters / area_km2, 2),
    dens_floaters_F = round(n_floaters_F / area_km2, 2),
    dens_floaters_M = round(n_floaters_M / area_km2, 2)
  ) %>%
  dplyr::select(-row_id, -fl_start, -fl_end)

### Patches ------
#### Biplot area ~ connectivity across time -------
# Years to plot
years_plot = c(1989, 2000, 2012, 2024)

biplot_data = patch_metrics %>%
  dplyr::filter(Year %in% years_plot) %>%
  dplyr::mutate(
    Year_f = factor(Year, levels = years_plot)
  ) %>%
  dplyr::arrange(patch_id, Year)

# Segments between consecutive selected years (one arrow per step)
biplot_segments = biplot_data %>%
  dplyr::group_by(patch_id) %>%
  dplyr::arrange(Year, .by_group = TRUE) %>%
  dplyr::mutate(
    dPC_end = dplyr::lead(dPC),
    area_end = dplyr::lead(area_ha)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(dPC_end))

# Label each patch at its most recent selected year
biplot_labels = biplot_data %>%
  dplyr::group_by(patch_id) %>%
  dplyr::slice_max(Year, n = 1) %>%
  dplyr::ungroup()

# Plot
patch_biplot = ggplot2::ggplot(
  biplot_data,
  ggplot2::aes(x = dPC, y = area_ha, colour = patch_id)
) +
  ggplot2::geom_segment(
    data = biplot_segments,
    ggplot2::aes(xend = dPC_end, yend = area_end),
    arrow = ggplot2::arrow(
      length = ggplot2::unit(0.2, "cm"),
      type = "closed"
    ),
    linewidth = 0.5,
    alpha = 0.8
  ) +
  ggplot2::geom_point(
    ggplot2::aes(shape = Year_f),
    size = 2.5
  ) +
  ggrepel::geom_text_repel(
    data = biplot_labels,
    ggplot2::aes(label = patch_id),
    size = 2.8,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  # ggplot2::scale_x_log10(
  #   labels = scales::label_number(drop0trailing = TRUE)
  # ) +
  ggplot2::scale_y_log10(
    labels = scales::label_number(drop0trailing = TRUE)
  ) +
  ggplot2::scale_colour_viridis_d(
    option = "turbo",
    guide = "none"
  ) +
  ggplot2::scale_shape_manual(
    values = c(16, 17, 15, 18),
    name = "Year"
  ) +
  ggplot2::labs(
    title = "Patch trajectories in the connectivity-size space",
    subtitle = paste0(
      "Arrows indicate chronological order (",
      paste(years_plot, collapse = " → "), ")"
    ),
    x = "Patch connectivity (dPC, log scale)",
    y = "Patch size (ha, log scale)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom"
  )

print(patch_biplot)

#### Changes between the first and last year for each patch -------
patch_change = biplot_data %>%
  dplyr::group_by(patch_id) %>%
  dplyr::arrange(Year, .by_group = TRUE) %>%
  dplyr::summarise(
    area_change = (dplyr::last(area_ha) - dplyr::first(area_ha)) /
      dplyr::first(area_ha) * 100,
    dPC_change = dplyr::if_else(
      dplyr::first(dPC) > 0,
      (dplyr::last(dPC) - dplyr::first(dPC)) / dplyr::first(dPC) * 100,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    strip_label = paste0(
      patch_id, "\n",
      "Area ", sprintf("%+.0f%%", area_change),
      " | dPC ", ifelse(is.na(dPC_change), "NA", sprintf("%+.0f%%", dPC_change))
    )
  )

biplot_data_f = biplot_data %>%
  dplyr::left_join(patch_change %>% dplyr::select(patch_id, strip_label), by = "patch_id")

biplot_segments_f = biplot_segments %>%
  dplyr::left_join(patch_change %>% dplyr::select(patch_id, strip_label), by = "patch_id")

### Plot: one panel per patch
patch_facets = ggplot2::ggplot(
  biplot_data_f,
  ggplot2::aes(x = dPC, y = area_ha, colour = patch_id)
) +
  ggplot2::geom_segment(
    data = biplot_segments_f,
    ggplot2::aes(xend = dPC_end, yend = area_end),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
    linewidth = 0.6
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_text(
    ggplot2::aes(label = Year),
    size = 2.5,
    vjust = -0.9,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(strip_label),
    scales = "free",
    ncol = 4
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.25)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.25)) +
  ggplot2::scale_colour_viridis_d(option = "turbo", guide = "none") +
  ggplot2::labs(
    title = "Patch trajectories (1989 → 2000 → 2012 → 2024)",
    subtitle = "Free axes in each panel: arrows show the direction of change over time",
    x = "Patch connectivity (dPC)",
    y = "Patch size (ha)"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    strip.text = ggplot2::element_text(size = 7, face = "bold"),
    axis.text = ggplot2::element_text(size = 6),
    panel.spacing = ggplot2::unit(0.8, "lines")
  )

print(patch_facets)

#### Relative changes -------
patch_change_plot = ggplot2::ggplot(
  patch_change,
  ggplot2::aes(x = dPC_change, y = area_change, colour = patch_id)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_point(size = 3) +
  ggrepel::geom_text_repel(
    ggplot2::aes(label = patch_id),
    size = 3,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  ggplot2::scale_colour_viridis_d(option = "turbo", guide = "none") +
  ggplot2::labs(
    title = "Change in patch connectivity and size, 1989–2024",
    x = "Change in connectivity, dPC (%)",
    y = "Change in patch size (%)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(patch_change_plot)