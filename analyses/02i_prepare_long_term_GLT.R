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
# Long-term data corrected by Aurore Ponchon and me (UMMP correction)
load(here::here("outputs", "data", "glt", "data_clean_long_final2.RData"))

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

##### Correspondence UMMP-Patch names -----
# We create a correspondence list between the names in the long-term dataset
unique(data.clean.final$UMMPs)
# And the names of the UMMPs in the patch file
unique(patches[[36]]$patch_id)
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
    # OUTSIDE UMMPs
    "Faz. Boa Esperanza",
    "4_Irmaos"
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
    # PATCHES OUTSIDE UMMPs
    "Boa_Esperanca",
    "4_Irmaos"
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

### Long-term dataset -----
#### Parameters ----------
# Problematic individuals
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490") # Last list of problematic individuals (see Aurore Zenodo)

#### Filter the long-term dataset -----
##### Filter out bad data -----
data.clean.filter = data.clean.final %>%
  dplyr::filter(
    !is.na(Tattoo),
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
    n_individuals = dplyr::n_distinct(Tattoo),
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
    GS_date = dplyr::n_distinct(Tattoo),
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
      Tattoo[SexOK %in% "M"], na.rm = TRUE
    ),
    # Number of females
    n_females = dplyr::n_distinct(
      Tattoo[SexOK %in% "F"], na.rm = TRUE
    ),
    # Number of immatures (infants and juveniles)
    n_immatures = dplyr::n_distinct(
      Tattoo[IdadeOK %in% c("IN", "JU")], na.rm = TRUE
    ),
    # Recruitment: number of newborns (infants)
    n_newborns = dplyr::n_distinct(
      Tattoo[IdadeOK %in% "IN"], na.rm = TRUE
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
    members = list(unique(Tattoo)),
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
# Build one analytical row per Group x reproductive season, combining:
# (1) reproductive events and litter information,
# (2) group composition and patch population observations,
# (3) group immigration/emigration, floater density, and patch-level metrics

# From Andressa Coelho thesis
# Dataset = "reprod" (group x calendar YEAR) + "status" (female x dated reproductive event)

# UNIT OF THE FINAL TABLE: one row = one group x reproductive season.
#   e.g., Season_start 1999 = wet season 1999-2000 (July 1999 to March 2000)
#
# PROBLEM: 'reprod' is organised by calendar Year, and a Year can contain TWO wet seasons:
#   - early events (Jan-Mar of Year) belong to the wet season that started the year before (Year - 1)
#   - late events (Jul-Dec of Year) belong to the wet season starting in Year
#   e.g., AP 2000: ST6 reproduces in January 2000 (wet 1999-2000), and ST6 + another female reproduce in October-November 2000 (wet 2000-2001)
#
# HOW A GROUP-YEAR IS ASSIGNED TO SEASONS
#   1) Months of reproduction are collected from: (a) the dates of the status data, and (b) the two "months_" variables of reprod (e.g., "JAN/OUT")
#   2) Jan-Mar -> season Year - 1; Jul-Dec -> season Year; Apr-Jun -> no season
#   3) If NO month is known for a group-year -> it is assigned to the LATE wet season (i.e., July-December: Season = Year)
#   4) The same group x season can be fed by two reprod rows (late part of Year t and early part of Year t+1): they are merged in a single row
#
# LITTER (infants, sexes, survival)
#   - group-year with ONE season: litter taken from reprod (complete)
#   - group-year with TWO seasons: litter taken from the status data (dated events), NA if the season has no status event, because reprod cannot split the litters between seasons
#   - rows merged from two reprod rows: litters are summed
#   -> 'litter_source' tells where the values come from
#
# FEMALES
#   - ids come from reprod (master list). 
#     A "?" in status is given to the named female of reprod when she is the only female of the group-year without dated status and reprod has no "?"
#   - a female with dated events is attached to the season(s) of her events
#   - a female without dated event is attached to all the seasons of her group-year (status unknown)
#
# YEARLY VARIABLES OF REPROD (patch quality, supplementation, Andressa's counts) are taken from the
# reprod row Year = Season_start (otherwise Year = Season_start + 1) and are YEARLY values
#
# LONG-TERM DATA are matched to the mating reference date. 
# The same date window is used for group composition, patch population, group flux, and floater overlap.


#### Parameters ------
lambda_max = 6 # lambda above this value is flagged as extreme
dry_months = 4:6  # Apr-Jun = dry season
wet_months = c(7:12, 1:3)  # Jul-Mar = wet season
season_last_month = 3  # Jan-Mar belong to the wet season that started the year before
gestation_days = 132  # GLT gestation ~125-132 days -> mating = birth - 132 days
window_months = 4  # symmetric matching window: N calendar months before/after mating
assumed_birth_month = 10 # births with no date are assumed on 1 October (late wet season)
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490")
litter_vars = c("n_infants", "n_in_males", "n_in_females", "n_in_sex_unknown", "n_infants_surv_1y")

#### Helpers ------
mode_ok = function(x) { # most frequent non-NA value
  x = x[!is.na(x)]
  if (length(x) == 0) NA_character_ else names(which.max(table(x)))
}
min_date = function(x) if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)

# Season of a date, from its month: Apr-Jun = dry, Jul-Mar = wet
season_type_of = function(month) {
  dplyr::case_when(month %in% dry_months ~ "dry", 
                   month %in% wet_months ~ "wet")
}
# Start year of the season: Jan-Mar -> year - 1; Apr-Dec -> year
season_start_of = function(year, month) {
  dplyr::if_else(month <= season_last_month, 
                 as.numeric(year) - 1, as.numeric(year))
}

# Label: "wet 1999-2000" (Jul 1999 - Mar 2000) or "dry 1999" (Apr-Jun 1999)
season_label = function(type, start) {
  dplyr::if_else(type == "wet", paste0("wet ", start, "-", start + 1), paste0("dry ", start))
}

#### 0. Reproductive events (one row = one female giving birth) ------

##### STATUS: precisely dated reproductive events ------
# Each status row is treated as one female reproductive event. The recorded
# date is the birth date; mating is estimated by subtracting gestation_days.
status_ev = status %>%
  dplyr::mutate(GLT = stringr::str_squish(GLT),
                Date = as.Date(Date),
                Year = as.numeric(lubridate::year(Date)),
                season_type = season_type_of(lubridate::month(Date)),
                Season_start = season_start_of(Year, lubridate::month(Date)),
                dplyr::across(dplyr::all_of(litter_vars), as.numeric),
                birth_date = Date, # date of birth(s) (1st of the month)
                repro_date = Date - lubridate::days(gestation_days),  # approximate mating date
                source = "status",
                season_assumed = FALSE) %>%
  dplyr::select(Group, Year, season_type, Season_start, GLT, Status, source, season_assumed,
                birth_date, repro_date, dplyr::all_of(litter_vars))

##### Observations: cleaned once, used by all the steps below ------
obs = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(GLT = stringr::str_squish(GLT),
                DateObs = as.Date(DateObs),
                BirthOK = as.Date(BirthOK)) %>%
  dplyr::filter(!is.na(Group), !is.na(DateObs))

##### REPROD: group-years not covered by status ------
# What is left = years with one reproductive female (known or "?") + years with no reproduction
reprod_rest = reprod %>%
  dplyr::mutate(Year = as.numeric(Year),
                dplyr::across(c(dplyr::all_of(litter_vars), n_reprod_females), as.numeric)) %>%
  dplyr::anti_join(dplyr::distinct(status_ev, Group, Year), by = c("Group", "Year")) %>%
  dplyr::mutate(reprod_type = dplyr::case_when(
    n_infants %in% 0 | n_reprod_females %in% 0 ~ "none",    # no reproduction that year
    n_reprod_females %in% 1 ~ "single",                     # one reproductive female
    TRUE ~ NA))

##### Long-term data: every identified infant, with birth date, group, sex and season ------
births_ind = obs %>%
  dplyr::filter(!is.na(GLT), !GLT %in% bad) %>% # unidentified animals cannot be dated
  dplyr::arrange(GLT, DateObs) %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(
    first_IN_obs   = if (any(IdadeOK %in% c("IN","JU"))) min(DateObs[IdadeOK %in% c("IN","JU")]) else as.Date(NA),
    first_IN_group = if (any(IdadeOK %in% c("IN","JU"))) Group[IdadeOK %in% c("IN","JU")][1] else NA_character_,
    birth_ok       = if (all(is.na(BirthOK))) as.Date(NA) else min(BirthOK, na.rm = TRUE),
    Sex            = mode_ok(SexOK),
    .groups = "drop") %>%
  dplyr::filter(!is.na(first_IN_obs)) %>%  # only individuals seen as infants
  dplyr::transmute(GLT, Sex,
                   Group = first_IN_group,   # group where the infant was first seen
                   birth_date = dplyr::coalesce(birth_ok, first_IN_obs),
                   Year = as.numeric(lubridate::year(birth_date)),    # = Year of reprod
                   season_type  = season_type_of(lubridate::month(birth_date)),
                   Season_start = season_start_of(Year, lubridate::month(birth_date)))

# Candidate seasons: infants of each group x year, split by season
# A calendar year has up to 3 candidates: early wet (Jan-Mar), dry (Apr-Jun), late wet (Jul-Dec)
births_season = births_ind %>%
  dplyr::group_by(Group, Year, season_type, Season_start) %>%
  dplyr::summarise(n_data   = dplyr::n(),
                   n_data_m = sum(Sex %in% "M"),
                   n_data_f = sum(Sex %in% "F"),
                   id_infants  = paste(GLT, collapse = ", "),
                   first_birth = min(birth_date),
                   .groups = "drop")

# Was the female seen as an adult in the group during that season?
fem_seen = obs %>%
  dplyr::filter(SexOK %in% "F", !IdadeOK %in% c("IN", "JU")) %>%
  dplyr::transmute(Group, GLT,
                   season_type  = season_type_of(lubridate::month(DateObs)),
                   Season_start = season_start_of(lubridate::year(DateObs), lubridate::month(DateObs)),
                   female_seen = TRUE) %>%
  dplyr::distinct()


##### Season of the reprod events ------
# For each reprod group-year, compare the litter of reprod (n infants, males, females)
# with the infants born in each candidate season of the same group and year.
# Ranking: manual date > months listed in reprod > matching by count and sex

##### Manual birth dates
# Identified manually in the dataset
# Year = reprod year (the key of the reprod row); birth_my = month/year of the birth.
# The birth can fall in the previous calendar year (e.g., reprod 1999 -> birth 09/1998).
manual_births = tibble::tribble(
  ~Year, ~Group, ~GLT,   ~birth_my,
  1999,  "AP",   "ST6",  "09/1998",
  2000,  "EM",   "E15",  "10/1999",
  2002,  "AT",   "PT4",  "12/2001",
  2002,  "OL",   "OL1",  "05/2002",
  2003,  "FX",   "OL5",  "12/2003",
  2004,  "OL",   "OL1",  "11/2003",
  2005,  "SP",   "OL15", "12/2005") %>%
  dplyr::mutate(birth_date = lubridate::my(birth_my)) %>%   # 1st of the month
  dplyr::select(-birth_my)

reprod_single = reprod_rest %>%
  # Remove rows without reproductionn
  dplyr::filter(reprod_type != "none") %>%
  # the manual dates only apply to single-female rows
  dplyr::left_join(manual_births, by = c("Group", "Year", "id_reprod_females" = "GLT"),
                   relationship = "many-to-one") %>%
  dplyr::mutate(birth_manual = dplyr::if_else(reprod_type == "single", birth_date, as.Date(NA))) %>%
  dplyr::select(-birth_date)

##### Manual events
# Extract manual corrections
manual_ev = reprod_single %>%
  dplyr::filter(!is.na(birth_manual)) %>%
  dplyr::transmute(Group, Year,
                   season_type = season_type_of(lubridate::month(birth_manual)),
                   Season_start = season_start_of(lubridate::year(birth_manual), lubridate::month(birth_manual)),
                   GLT = id_reprod_females, Status = NA_character_,
                   birth_date = birth_manual,
                   repro_date = birth_manual - lubridate::days(gestation_days),
                   dplyr::across(dplyr::all_of(litter_vars)),
                   id_infants = NA_character_, 
                   season_assumed = FALSE, 
                   match_type = "manual")

##### Single females who reproduced twice
# For females reproducing twice a year, the months are given in reprod
# Tokens such as "JAN/OUT" = births in January and October of Year (1st of the month assumed).
# Months falling in the same season are collapsed into one event (the earliest month).
months_tbl = reprod_single %>%
  dplyr::filter(is.na(birth_manual), reprod_type == "single",
                !is.na(months_with_several_reprod_events)) %>%
  dplyr::select(Group, Year, 
                id_reprod_females, 
                n_infants_reprod = n_infants,
                months_with_several_reprod_events) %>%
  tidyr::separate_longer_delim(months_with_several_reprod_events, delim = "/") %>%
  # Month name from portuguese abbreviations
  dplyr::mutate(Month = unname(pt_months[stringr::str_to_upper(stringr::str_squish(months_with_several_reprod_events))])) %>%
  dplyr::filter(!is.na(Month)) %>% # unreadable tokens: the row goes to matching (C)
  dplyr::mutate(birth_date   = lubridate::make_date(Year, Month, 1),
                season_type  = season_type_of(Month),
                Season_start = season_start_of(Year, Month)) %>%
  dplyr::arrange(Group, Year, birth_date) %>%
  dplyr::distinct(Group, Year, id_reprod_females, season_type, Season_start, .keep_all = TRUE)

# The litter of each event cannot be split from reprod (yearly totals):
# it is taken from the infants born in that season in the long-term data
months_ev = months_tbl %>%
  dplyr::left_join(births_season, by = c("Group", "Year", "season_type", "Season_start"),
                   relationship = "many-to-one") %>%
  dplyr::group_by(Group, Year, id_reprod_females) %>%
  # do the infants of all the events add up to the total of reprod?
  dplyr::mutate(sum_ok = sum(n_data, na.rm = TRUE) == dplyr::first(n_infants_reprod)) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(Group, Year, season_type, Season_start,
                   GLT = id_reprod_females, Status = NA_character_,
                   birth_date,
                   repro_date = birth_date - lubridate::days(gestation_days),
                   n_infants = n_data, n_in_males = n_data_m, n_in_females = n_data_f,
                   n_in_sex_unknown = n_data - n_data_m - n_data_f,
                   n_infants_surv_1y = NA_real_,        # cannot be split between the two events
                   id_infants, season_assumed = FALSE,
                   match_type = dplyr::if_else(sum_ok, "months+count", "months_only"))

# MANUAL CORRECTIONS
# TO COMPLETE
months_ev = months_ev %>%
  dplyr::mutate(
    n_infants = dplyr::case_when(GLT == "MA9" & Group == "RV" & ...)
  )

# Remove NAs
months_ev = months_ev %>% 
  dplyr::filter(!is.na(n_infants))

##### All other events: season = candidate season that matches reprod
# Ranking: count matches > sexes match > female seen in the group that season > smallest gap
reprod_match = reprod_single %>%
  dplyr::filter(is.na(birth_manual)) %>%
  dplyr::anti_join(dplyr::distinct(months_tbl, Group, Year, id_reprod_females),
                   by = c("Group", "Year", "id_reprod_females")) %>%
  dplyr::select(-birth_manual)

match_ev = reprod_match %>%
  dplyr::left_join(births_season, by = c("Group", "Year"), relationship = "one-to-many") %>%
  dplyr::left_join(fem_seen, by = c("Group", "season_type", "Season_start", "id_reprod_females" = "GLT"),
                   relationship = "many-to-one") %>%
  dplyr::mutate(
    count_ok = dplyr::coalesce(n_data == n_infants, FALSE),
    sex_ok   = count_ok & dplyr::coalesce(is.na(n_in_males) | is.na(n_in_females) |
                                            (n_data_m == n_in_males & n_data_f == n_in_females), FALSE),
    seen_ok  = id_reprod_females %in% "?" | dplyr::coalesce(female_seen, FALSE),  # "?": group and year only
    key      = 4 * count_ok + 2 * sex_ok + seen_ok,
    gap      = abs(n_data - n_infants)) %>%
  dplyr::arrange(Group, Year, id_reprod_females, dplyr::desc(key), gap, first_birth) %>%
  dplyr::group_by(Group, Year, id_reprod_females) %>%
  dplyr::mutate(n_best = sum(key == dplyr::first(key) & dplyr::coalesce(gap == dplyr::first(gap), TRUE))) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    season_assumed = is.na(n_data),
    match_type = dplyr::case_when(season_assumed ~ "no_births_in_data",
                                  count_ok & n_best > 1 ~ "ambiguous",
                                  count_ok & sex_ok ~ "count+sex",
                                  count_ok ~ "count_only",
                                  TRUE ~ "closest"),
    season_type  = dplyr::if_else(season_assumed, "wet", season_type),
    Season_start = dplyr::if_else(season_assumed, Year, Season_start),
    birth_date = first_birth,
    repro_date = birth_date - lubridate::days(gestation_days)) %>%
  dplyr::transmute(Group, Year, season_type, Season_start, GLT = id_reprod_females,
                   Status = NA_character_, birth_date, repro_date,
                   dplyr::across(dplyr::all_of(litter_vars)),
                   id_infants, season_assumed, match_type)

reprod_events = dplyr::bind_rows(manual_ev, months_ev, match_ev)

##### Combine status and reprod ------
status_ev = status_ev %>%
  dplyr::mutate(id_infants = NA_character_, match_type = "status") %>%
  dplyr::select(Group, Year, season_type, Season_start, GLT, Status, birth_date, repro_date,
                dplyr::all_of(litter_vars), id_infants, season_assumed, match_type)

repro_events = dplyr::bind_rows(status_ev, reprod_events) %>%
  dplyr::mutate(Season = season_label(season_type, Season_start)) %>%
  dplyr::relocate(Year, Season_start, Season, season_type, Group, GLT)

#### 1. Group x season table (reproduction) ------
##### 1a. Summarise reproductive events by group x season ------
# A season can gather several events (2 females) and two reprod years (late t + early t+1)
events_gs = repro_events %>%
  dplyr::group_by(Group, season_type, Season_start) %>%
  dplyr::summarise(
    # reprod year that supplies the annual covariates (t for late events, t + 1 for Jan-Mar only)
    Year = if (any(Year == first(Season_start))) first(Season_start) else first(Season_start) + 1,
    n_repro_females  = dplyr::n_distinct(GLT),
    id_repro_females = paste(sort(unique(GLT)), collapse = ", "),
    n_dom = dplyr::n_distinct(GLT[Status %in% "ALF"]),
    n_sub = dplyr::n_distinct(GLT[!is.na(Status) & Status != "ALF"]),
    n_status_unknown = dplyr::n_distinct(GLT[is.na(Status)]),
    dplyr::across(dplyr::all_of(litter_vars), ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    id_infants = dplyr::na_if(paste(stats::na.omit(id_infants), collapse = ", "), ""),
    first_birth      = min_date(birth_date),
    repro_date_first = min_date(repro_date),            # earliest mating of the season
    season_assumed = all(season_assumed),
    match_type = paste(sort(unique(match_type)), collapse = " + "),
    .groups = "drop")

##### 1b. Add group-years with no reproduction ------
# These rows are assigned to the late wet season because no event date exists.
zero_gs_all = reprod_rest %>%
  dplyr::filter(reprod_type == "none") %>%
  dplyr::transmute(Group, Year, season_type = "wet", Season_start = Year,
                   n_repro_females = 0L,
                   dplyr::across(dplyr::all_of(litter_vars), ~ dplyr::coalesce(.x, 0)),
                   season_assumed = TRUE, match_type = "no_reproduction")

# If the same group x season already exists (e.g., Jan-Mar births of the next year), drop the zero row
zero_gs = dplyr::anti_join(zero_gs_all, events_gs, by = c("Group", "season_type", "Season_start"))

##### 1c. Annual covariates from the reprod dataset ------
# These variables remain annual; they are joined using the selected reprod Year.
reprod_yearly = reprod %>%
  dplyr::transmute(Group, Year = as.numeric(Year), UMMPs, Fragment, Patch_size, IQH, IQHS, Group_isolation,
                   n_days_supp_food, n_months_supp_food, mean_days.month_supp, mean_days.year_supp)

group_ummp = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Group), !is.na(UMMPs)) %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(UMMPs_group = mode_ok(as.character(UMMPs)), .groups = "drop")

##### 1d. Build the group-season table and define mating reference dates ------
# If mating cannot be dated, use the assumed 1 October birth date minus gestation.
gs = dplyr::bind_rows(events_gs, zero_gs) %>%
  dplyr::left_join(reprod_yearly, by = c("Group", "Year"), relationship = "many-to-one") %>%
  dplyr::left_join(group_ummp, by = "Group", relationship = "many-to-one") %>%
  dplyr::mutate(
    UMMPs  = dplyr::coalesce(as.character(UMMPs), UMMPs_group),
    Season = season_label(season_type, Season_start),
    # Reference date = mating of the season (earliest event). No date: assumed birth (1 Oct) - gestation
    ref_date = dplyr::coalesce(repro_date_first,
                               lubridate::make_date(Season_start, assumed_birth_month, 1) -
                                 lubridate::days(gestation_days)),
    # Symmetric calendar-month window around the estimated mating date.
    # %m+% / %m-% handle month-end dates safely (e.g. 31 May + 3 months).
    win_start = ref_date %m-% months(window_months),
    win_end  = ref_date %m+% months(window_months),
    Year_ref = lubridate::year(ref_date),
    # Litter characteristics
    litter_prop_males = dplyr::if_else(n_in_males + n_in_females > 0,
                                       round(n_in_males / (n_in_males + n_in_females), 2), NA_real_),
    litter_surv_1y = dplyr::if_else(n_infants > 0, round(n_infants_surv_1y / n_infants, 2), NA_real_),
    row_id = dplyr::row_number()) %>%
  dplyr::select(-UMMPs_group)

# Common reference windows used to match observational datasets of different
# temporal resolutions. Each row_id represents one group x reproductive season.
win = gs %>% dplyr::select(row_id, Group, UMMPs, ref_date, win_start, win_end)

#### 2. Group composition: nearest group observation to mating date ------
# First calculate group composition at each observed Group x DateObs.
# The original code averaged all dates in the window. Here, to accommodate
# datasets with different sampling resolutions, we select ONE date per
# reproductive-season row: the date closest to ref_date, within +/- 3 months.
group_date = obs %>%
  dplyr::mutate(
    immature = IdadeOK %in% c("IN", "JU"),
    unidentified = GLT %in% bad
  ) %>%
  dplyr::group_by(Group, DateObs) %>%
  dplyr::summarise(
    n_immatures = dplyr::n_distinct(Tattoo[immature & !unidentified], na.rm = TRUE) +
      sum(immature & unidentified, na.rm = TRUE),
    n_adults = dplyr::n_distinct(Tattoo[!immature & !unidentified], na.rm = TRUE) +
      sum(!immature & unidentified, na.rm = TRUE),
    n_males = dplyr::n_distinct(Tattoo[!immature & !unidentified & SexOK %in% "M"], na.rm = TRUE),
    n_females = dplyr::n_distinct(Tattoo[!immature & !unidentified & SexOK %in% "F"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    GS_total = n_adults + n_immatures,
    sex_ratio_date = dplyr::if_else(n_females > 0, n_males / n_females, NA_real_)
  )

# Find candidate dates in the window, then keep the closest one per row_id.
# Ties are resolved in favour of the earlier observation date for reproducibility.
comp_date_match = group_date %>%
  dplyr::inner_join(
    dplyr::select(win, row_id, Group, ref_date, win_start, win_end),
    by = dplyr::join_by(Group, dplyr::between(DateObs, win_start, win_end)),
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(diff_days = abs(as.integer(DateObs - ref_date))) %>%
  dplyr::arrange(row_id, diff_days, DateObs) %>%
  dplyr::group_by(row_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup()

# Keep the historical output column names for compatibility. Since only one
# date is selected, the "mean_" values now describe that nearest observation.
comp_gs = comp_date_match %>%
  dplyr::transmute(
    row_id,
    comp_obs_date = DateObs,
    comp_diff_days = diff_days,
    n_dates = 1L,
    mean_GS_adults = round(n_adults, 2),
    mean_GS_total = round(GS_total, 2),
    mean_immatures = round(n_immatures, 2),
    mean_males = round(n_males, 2),
    mean_females = round(n_females, 2),
    mean_sex_ratio = round(sex_ratio_date, 2)
  )

#### 3. Patch population: nearest patch observation to mating date ------
# One row per individual x patch x group x observation date.
# Individuals with missing Tattoo are excluded from individual counts below.
obs_patch = obs %>%
  dplyr::filter(!is.na(UMMPs), !is.na(DateObs)) %>%
  dplyr::transmute(
    UMMPs = as.character(UMMPs), Group, Tattoo, DateObs
  ) %>%
  dplyr::distinct()

win_patch = win %>%
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::select(row_id, UMMPs, ref_date, win_start, win_end)

# For each reproductive-season row, select the nearest date on which the patch
# was observed anywhere in the patch. Then count all individuals/groups recorded
# in that patch on that selected date. A missing match produces no count row.
patch_counts = function(w) {
  nearest_patch_date = obs_patch %>%
    dplyr::distinct(UMMPs, DateObs) %>%
    dplyr::inner_join(
      w,
      by = dplyr::join_by(UMMPs, dplyr::between(DateObs, win_start, win_end)),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(diff_days = abs(as.integer(DateObs - ref_date))) %>%
    dplyr::arrange(row_id, diff_days, DateObs) %>%
    dplyr::group_by(row_id) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(row_id, UMMPs, ref_date, DateObs, diff_days)
  
  obs_patch %>%
    dplyr::inner_join(nearest_patch_date, by = c("UMMPs", "DateObs")) %>%
    dplyr::group_by(row_id) %>%
    dplyr::summarise(
      n_glt = dplyr::n_distinct(Tattoo, na.rm = TRUE),
      n_groups = dplyr::n_distinct(Group, na.rm = TRUE),
      n_dates = 1L,
      patch_obs_date = dplyr::first(DateObs),
      patch_diff_days = dplyr::first(diff_days),
      .groups = "drop"
    )
}

# Population size at the closest available patch observation date.
pop_now = patch_counts(win_patch) %>%
  dplyr::rename(
    n_glt_patch = n_glt,
    n_groups_patch = n_groups,
    n_dates_patch = n_dates
  )

# Estimate annual population change by repeating the same nearest-date procedure
# around the reference mating date shifted exactly one calendar year backwards.
win_patch_prev = win_patch %>%
  dplyr::mutate(
    ref_date = ref_date %m-% lubridate::years(1),
    win_start = win_start %m-% lubridate::years(1),
    win_end = win_end %m-% lubridate::years(1)
  )

pop_prev = patch_counts(win_patch_prev) %>%
  dplyr::select(row_id, n_glt_prev = n_glt)

# Lambda is only calculated when a previous-year patch count is available and > 0.
patch_gs = pop_now %>%
  dplyr::left_join(pop_prev, by = "row_id", relationship = "one-to-one") %>%
  dplyr::mutate(
    lambda = dplyr::if_else(!is.na(n_glt_prev) & n_glt_prev > 0,
                            round(n_glt_patch / n_glt_prev, 2), NA_real_),
    lambda_extreme = lambda > lambda_max
  ) %>%
  dplyr::select(-n_glt_prev)

#### 4. Immigrants and emigrants (group flux) within the +/- 3-month window ------
# Flux records are event counts, so retain/sum every event falling in the window
# rather than selecting a single nearest record.
flux_gs = group_flux %>%
  dplyr::mutate(DateObs = as.Date(DateObs)) %>%
  dplyr::inner_join(dplyr::select(win, row_id, Group, win_start, win_end),
                    by = dplyr::join_by(Group, dplyr::between(DateObs, win_start, win_end)),
                    relationship = "many-to-many") %>%
  dplyr::group_by(row_id) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("n_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

#### 5. Floaters: satellite stays overlapping the +/- 3-month window ------
# Stays are intervals, so count distinct satellite individuals whose stay overlaps
# any part of the reference window.
rec_loc = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(DateObs = as.Date(DateObs), UMMPs = as.character(UMMPs)) %>%
  dplyr::group_by(Tattoo, DateObs) %>%
  dplyr::summarise(rec_Region = mode_ok(Region), rec_UMMPs = mode_ok(UMMPs), .groups = "drop")

floaters = stays %>%
  dplyr::filter(stay_type == "satellite") %>%
  dplyr::left_join(rec_loc, by = c("Tattoo", "start" = "DateObs"), relationship = "many-to-one") %>%
  dplyr::transmute(sat_GLT = Tattoo, sat_SexOK = SexOK,
                   sat_UMMPs = dplyr::coalesce(as.character(UMMPs), rec_UMMPs),
                   sat_start = lubridate::floor_date(start, "month"),   # reference dates are 1st of a month
                   sat_end = end) %>%
  dplyr::filter(!is.na(sat_UMMPs))

floaters_gs = win_patch %>%
  dplyr::inner_join(floaters,
                    by = dplyr::join_by(UMMPs == sat_UMMPs, win_end >= sat_start, win_start <= sat_end),
                    relationship = "many-to-many") %>%
  dplyr::group_by(row_id) %>%
  dplyr::summarise(n_floaters   = dplyr::n_distinct(sat_GLT),
                   n_floaters_F = dplyr::n_distinct(sat_GLT[sat_SexOK %in% "F"]),
                   n_floaters_M = dplyr::n_distinct(sat_GLT[sat_SexOK %in% "M"]),
                   n_floaters_unknown = dplyr::n_distinct(sat_GLT[is.na(sat_SexOK)]),
                   .groups = "drop")

#### 6. Join all derived datasets and calculate patch densities ------
# All joins below are keyed by row_id (one row per group-season), except the
# patch crosswalk and annual patch metrics.
gs = gs %>%
  dplyr::left_join(comp_gs,     by = "row_id", relationship = "one-to-one") %>%
  dplyr::left_join(patch_gs,    by = "row_id", relationship = "one-to-one") %>%
  dplyr::left_join(flux_gs,     by = "row_id", relationship = "one-to-one") %>%
  dplyr::left_join(floaters_gs, by = "row_id", relationship = "one-to-one") %>%
  # Patch metrics are yearly: matched on the year of mating (columns also in reprod get the suffix _pm)
  dplyr::left_join(UMMP_crosswalk, by = "UMMPs") %>%
  dplyr::left_join(patch_metrics, by = c("patch_id", "Year_ref" = "Year"), suffix = c("", "_pm")) %>%
  dplyr::mutate(
    area_km2 = area_ha / 100,
    dens_glt = round(n_glt_patch / area_km2, 2),
    dens_groups = round(n_groups_patch / area_km2, 2),
    # No satellite stay found = 0 floaters (only when the patch is known)
    dplyr::across(dplyr::starts_with("n_floaters"),
                  ~ dplyr::if_else(is.na(UMMPs), NA_integer_, tidyr::replace_na(.x, 0L))),
    dens_floaters   = round(n_floaters / area_km2, 2),
    dens_floaters_F = round(n_floaters_F / area_km2, 2),
    dens_floaters_M = round(n_floaters_M / area_km2, 2))

#### 7. Assemble and order the final analytical table ------
repro_final = gs %>%
  dplyr::mutate(mating_date = ref_date) %>%
  dplyr::select(
    dplyr::any_of(c("Group", "UMMPs", "Fragment", "Group_isolation")),
    dplyr::any_of(c("Season", "season_type", "Season_start", "season_assumed", "match_type", "mating_date")),
    # reproduction
    dplyr::any_of(c("n_infants", "n_in_males", "n_in_females", "n_in_sex_unknown", "n_infants_surv_1y",
                    "litter_prop_males", "litter_surv_1y", "first_birth", "id_infants")),
    dplyr::any_of(c("n_repro_females", "id_repro_females", "n_dom", "n_sub", "n_status_unknown")),
    # group composition and distance (days) between the observation and mating
    dplyr::any_of(c("mean_GS_adults", "mean_GS_total", "mean_immatures", "mean_males", "mean_females",
                    "mean_sex_ratio", "comp_diff_days", "patch_diff_days")),
    # dispersal and floaters
    dplyr::starts_with("n_immigrants"), dplyr::starts_with("n_emigrants"), dplyr::starts_with("n_founders"),
    dplyr::starts_with("n_floaters"), dplyr::starts_with("dens_floaters"),
    # patch
    dplyr::any_of(c("area_km2", "shape", "enn_m", "dPC", "IQH", "IQHS", "n_glt_patch", "n_groups_patch",
                    "dens_glt", "dens_groups", "lambda", "lambda_extreme")),
    dplyr::any_of(c("n_days_supp_food", "n_months_supp_food", "mean_days.month_supp", "mean_days.year_supp"))) %>%
  dplyr::arrange(Group, Season_start, season_type)


#### 8. Diagnostics and consistency checks ------
reprod %>% dplyr::count(Group, Year) %>% dplyr::filter(n > 1)  # reprod: 1 row per group x year?
dplyr::count(repro_events, match_type)   # ambiguous / closest / count_only = rows to inspect
dplyr::count(repro_final, season_type, season_assumed)
sum(is.na(repro_final$n_dates)) # group-composition rows with no observation within +/- N months
sum(is.na(repro_final$patch_obs_date)) # patch rows with no observation within +/- N months

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