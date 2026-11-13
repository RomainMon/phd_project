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
# From Andressa Coelho thesis
# Dataset = "reprod" (group x calendar YEAR) + "status" (female x dated reproductive event)

# UNIT OF THE FINAL TABLE: one row = one group x wet reproductive season (Season_start = year of July)
#   e.g., Season_start 1999 = wet season 1999-2000 (July 1999 to March 2000)
#
# PROBLEM: 'reprod' is organised by calendar Year, and a Year can contain TWO wet seasons:
#   - early events (Jan-Mar of Year) belong to the wet season that started the year before (Year - 1)
#   - late events (Jul-Dec of Year) belong to the wet season starting in Year
#   e.g., AP 2000: ST6 reproduces in January 2000 (wet 1999-2000), and ST6 + another female
#   reproduce in October-November 2000 (wet 2000-2001)
#
# HOW A GROUP-YEAR IS ASSIGNED TO SEASONS
#   1) Months of reproduction are collected from: (a) the dates of the status data, and (b) the two "months_" variables of reprod (e.g., "JAN/OUT")
#   2) Jan-Mar -> season Year - 1; Jul-Dec -> season Year; Apr-Jun -> no season (ignored, see checks)
#   3) If NO month is known for a group-year -> it is assigned to the LATE wet season (i.e., July-December: Season = Year)
#   4) The same group x season can be fed by two reprod rows (late part of Year t and early part of
#      Year t+1): they are merged in a single row
#
# LITTER (infants, sexes, survival)
#   - group-year with ONE season: litter taken from reprod (complete)
#   - group-year with TWO seasons: litter taken from the status data (dated events), NA if the season has no status event, because reprod cannot split the litters between seasons
#   - rows merged from two reprod rows: litters are summed
#   -> 'litter_source' tells where the values come from
#
# FEMALES
#   - ids come from reprod (master list). A "?" in status is given to the named female of reprod
#     when she is the only female of the group-year without dated status and reprod has no "?"
#   - a female with dated events is attached to the season(s) of her events
#   - a female without dated event is attached to all the seasons of her group-year (status unknown)
#
# YEARLY VARIABLES OF REPROD (patch quality, supplementation, Andressa's counts) are taken from the
# reprod row Year = Season_start (otherwise Year = Season_start + 1) and are YEARLY values
#
# LONG-TERM DATA (composition, flux, floaters, patch population) are computed for the WET season only
# (reproduction takes place in the wet season; Dry rows are no longer in the table)

#### Parameters ------
lambda_max = 6 # lambda above this value is considered extreme
# Dry and wet months
dry_months = 4:6
wet_months = c(7:12, 1:3)
# Reproductive season: from season_first_month (year t) to season_last_month (year t+1)
season_first_month = 7 # July (some reproductions are recorded in July)
season_last_month = 3 # March
floater_window_months = 3 # floaters are counted over the N months before the season
# Problematic individuals
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490")
# Litter variables (same names in reprod and status)
litter_vars = c("n_infants", "n_in_males", "n_in_females", "n_in_sex_unknown", "n_infants_surv_1y")
# Portuguese month abbreviations used in reprod
pt_months = c(JAN = 1, FEV = 2, MAR = 3, ABR = 4, MAI = 5, JUN = 6,
              JUL = 7, AGO = 8, SET = 9, OUT = 10, NOV = 11, DEZ = 12)

#### Helpers ----
# Most frequent non-NA value
mode_ok = function(x) {
  x = x[!is.na(x)]
  if (length(x) == 0) NA_character_ else names(which.max(table(x)))
}

# Reproductive season (start year) of an event: Jul-Dec -> same year; Jan-Mar -> year - 1; Apr-Jun -> NA
season_of = function(year, month) {
  year = as.numeric(year)
  dplyr::case_when(month >= season_first_month ~ year,
                   month <= season_last_month ~ year - 1)
}

#### 1. Reproductive events: months, seasons, females ------
##### 1a. Status data: one row = one dated reproductive event of a female ------
status_ev = status %>%
  dplyr::mutate(Tattoo = stringr::str_squish(Tattoo),
                Year = as.numeric(lubridate::year(Date)),
                Month = lubridate::month(Date),
                # Mutate the start (Year) of the season corresponding to the reproductive event
                Season_start = season_of(Year, Month),
                dominant = Status %in% "ALF")

##### 1b. Females listed in reprod: one row = one female x group x calendar year ------
fem_reprod = reprod %>%
  dplyr::filter(!is.na(id_reprod_females)) %>%
  # Separate females
  tidyr::separate_longer_delim(id_reprod_females, delim = ",") %>%
  dplyr::transmute(Group, Year, Tattoo = stringr::str_squish(id_reprod_females)) %>%
  dplyr::filter(Tattoo != "") %>%
  dplyr::distinct()

# A "?" is given to a named female of reprod when:
# she is the ONLY female of the group without dated status, reprod has no "?", and status has a "?"
q_map = fem_reprod %>%
  dplyr::anti_join(dplyr::distinct(status_ev, Group, Year, Tattoo), by = c("Group", "Year", "Tattoo")) %>%
  dplyr::add_count(Group, Year, name = "n_no_event") %>%
  dplyr::filter(n_no_event == 1) %>%
  dplyr::anti_join(dplyr::filter(fem_reprod, Tattoo == "?"), by = c("Group", "Year")) %>%
  dplyr::semi_join(dplyr::filter(status_ev, Tattoo == "?"), by = c("Group", "Year")) %>%
  dplyr::select(Group, Year, GLT_named = Tattoo)

# Update identity of reproductive females
status_ev = status_ev %>%
  dplyr::left_join(q_map, by = c("Group", "Year"), relationship = "many-to-one") %>%
  dplyr::mutate(GLT = dplyr::if_else(Tattoo == "?" & !is.na(GLT_named), GLT_named, Tattoo)) %>%
  dplyr::select(-GLT_named)

##### 1c. Months of reproduction known for each group-year ------
# From the "months" variables of reprod (e.g., "JAN/OUT" -> January and October)
months_reprod = reprod %>%
  dplyr::select(Group, Year, months_with_several_reprod_events, months_where_several_females_reprod) %>%
  tidyr::pivot_longer(c(months_with_several_reprod_events, months_where_several_females_reprod),
                      names_to = "var", values_to = "token") %>%
  dplyr::filter(!is.na(token)) %>%
  # Separate months
  tidyr::separate_longer_delim(token, delim = "/") %>%
  dplyr::mutate(token = stringr::str_to_upper(stringr::str_squish(token)),
                Month = unname(pt_months[token]))

# Union of both sources (status dates + reprod months), with the source of each month
months_known = dplyr::bind_rows(
  dplyr::transmute(status_ev, Group, Year, Month, source = "status"),
  months_reprod %>% dplyr::filter(!is.na(Month)) %>% dplyr::transmute(Group, Year, Month, source = "reprod")
) %>%
  dplyr::group_by(Group, Year, Month) %>%
  dplyr::summarise(source = paste(sort(unique(source)), collapse = "+"), .groups = "drop") %>%
  dplyr::mutate(Season_start = season_of(Year, Month))

##### 1d. Seasons of each group-year ------
gy_seasons = reprod %>%
  dplyr::distinct(Group, Year) %>%
  dplyr::left_join(months_known %>%
                     dplyr::filter(!is.na(Season_start)) %>%
                     dplyr::distinct(Group, Year, Season_start),
                   by = c("Group", "Year"), relationship = "one-to-many") %>%
  # No month known: the Year is assigned to the LATE wet season (July-December)
  dplyr::mutate(Season_start = dplyr::coalesce(Season_start, Year)) %>%
  dplyr::group_by(Group, Year) %>%
  dplyr::mutate(n_seasons_year = dplyr::n()) %>%
  dplyr::ungroup()

##### 1e. Litter by group x year x season ------
reprod_num = reprod %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(litter_vars), as.numeric))

# Litters of the dated events of status
status_litter = status_ev %>%
  dplyr::filter(!is.na(Season_start)) %>%
  dplyr::group_by(Group, Year, Season_start) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(litter_vars), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

litter_gys = dplyr::bind_rows(
  # One season in the year: litter of reprod (complete)
  gy_seasons %>%
    dplyr::filter(n_seasons_year == 1) %>%
    dplyr::left_join(dplyr::select(reprod_num, Group, Year, dplyr::all_of(litter_vars)),
                     by = c("Group", "Year"), relationship = "many-to-one") %>%
    dplyr::mutate(litter_source = "reprod"),
  # Two seasons in the year: litter of status (NA when the season has no dated event)
  gy_seasons %>%
    dplyr::filter(n_seasons_year > 1) %>%
    dplyr::left_join(status_litter, by = c("Group", "Year", "Season_start"), relationship = "many-to-one") %>%
    dplyr::mutate(litter_source = dplyr::if_else(is.na(n_infants), "none", "status"))
)

# One row per group x season (a season can be fed by the late part of year t and the early part of t+1)
litter_season = litter_gys %>%
  dplyr::group_by(Group, Season_start) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(litter_vars),
                                 ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
                   litter_source = paste(sort(unique(litter_source)), collapse = "+"), .groups = "drop")


##### 1f. Females by group x season ------
# Dominance of each female for each season (from her dated events)
fem_ev = status_ev %>%
  dplyr::filter(!is.na(Season_start)) %>%
  dplyr::group_by(Group, Year, Season_start, Tattoo) %>%
  dplyr::summarise(dominant = any(dominant), .groups = "drop")

fem_season = dplyr::bind_rows(
  # Female with dated event(s): attached to the season(s) of her events
  fem_reprod %>%
    dplyr::inner_join(fem_ev, by = c("Group", "Year", "Tattoo")),
  # Female without dated event: attached to all the seasons of her group-year, status unknown
  fem_reprod %>%
    dplyr::anti_join(fem_ev, by = c("Group", "Year", "Tattoo")) %>%
    dplyr::inner_join(dplyr::distinct(gy_seasons, Group, Year, Season_start),
                      by = c("Group", "Year"), relationship = "many-to-many") %>%
    dplyr::mutate(dominant = NA)
) %>%
  # A female can come from two reprod rows for the same season
  dplyr::group_by(Group, Season_start, Tattoo) %>%
  dplyr::summarise(dominant = if (all(is.na(dominant))) NA else any(dominant, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::arrange(Group, Season_start, Tattoo) %>%
  dplyr::group_by(Group, Season_start) %>%
  dplyr::summarise(id_repro_females = paste(Tattoo, collapse = ", "),
                   n_repro_females = dplyr::n(),
                   n_dom = sum(dominant %in% TRUE),
                   n_sub = sum(dominant %in% FALSE),
                   n_status_unknown = sum(is.na(dominant)), # females without status
                   status_repro = dplyr::case_when(n_dom > 0 & n_sub > 0 ~ "DOM-SUB",
                                                   n_dom > 0 ~ "DOM",
                                                   n_sub > 0 ~ "SUB",
                                                   TRUE ~ NA_character_),
                   status_complete = n_status_unknown == 0,
                   .groups = "drop")

##### 1g. Months of reproduction by group x season ------
months_season = months_known %>%
  dplyr::filter(!is.na(Season_start)) %>%
  # chronological order within the season (July ... December, January ... March)
  dplyr::mutate(m_order = dplyr::if_else(Month >= season_first_month, Month, Month + 12L)) %>%
  dplyr::arrange(Group, Season_start, m_order) %>%
  dplyr::group_by(Group, Season_start) %>%
  dplyr::summarise(months_repro = paste(month.abb[Month], collapse = ", "),
                   n_months_repro = dplyr::n_distinct(Month),
                   months_source = paste(sort(unique(source)), collapse = "+"),
                   .groups = "drop")

##### 1h. Yearly variables of reprod ------
reprod_yearly = reprod %>%
  dplyr::transmute(Group, Year, UMMPs, Fragment, Patch_size, IQH, IQHS, Group_isolation,
                   n_days_supp_food, n_months_supp_food, mean_days.month_supp, mean_days.year_supp,
                   # Values of Andressa, kept for comparison (YEARLY values)
                   reprod_mean_GS = Mean_group_size,
                   reprod_n_females = n_reprod_females,
                   reprod_id_females = id_reprod_females,
                   reprod_n_infants = n_infants,
                   reprod_n_immigrants = n_immigrants,
                   reprod_n_emigrants = n_emigrants)

# UMMPs of the group in the long-term data
group_ummp = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Group), !is.na(UMMPs)) %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(UMMPs_group = mode_ok(as.character(UMMPs)), .groups = "drop")

##### 1i. Base table: one row = one group x wet season ------
gs = litter_season %>%
  dplyr::left_join(fem_season, by = c("Group", "Season_start"), relationship = "one-to-one") %>%
  dplyr::left_join(months_season, by = c("Group", "Season_start"), relationship = "one-to-one") %>%
  # Yearly variables: reprod row Year = Season_start...
  dplyr::left_join(dplyr::rename(reprod_yearly, Season_start = Year),
                   by = c("Group", "Season_start"), relationship = "one-to-one")

# ...otherwise reprod row Year = Season_start + 1 (season fed only by the early events of the next year)
gs = dplyr::rows_patch(gs,
                       reprod_yearly %>% dplyr::mutate(Season_start = Year - 1) %>% dplyr::select(-Year),
                       by = c("Group", "Season_start"), unmatched = "ignore")

gs = gs %>%
  dplyr::left_join(group_ummp, by = "Group", relationship = "many-to-one") %>%
  dplyr::mutate(
    UMMPs = dplyr::coalesce(UMMPs, UMMPs_group),
    Season = paste0("wet ", Season_start, "-", Season_start + 1),
    # TRUE = no month known: season assigned by default (late wet season of the Year)
    season_assumed = is.na(months_repro),
    # Litter characteristics of the season
    n_infants_per_female = dplyr::if_else(n_repro_females > 0, round(n_infants / n_repro_females, 2), NA_real_),
    litter_prop_males = dplyr::if_else(n_in_males + n_in_females > 0,
                                       round(n_in_males / (n_in_males + n_in_females), 2), NA_real_),
    litter_surv_1y = dplyr::if_else(n_infants > 0, round(n_infants_surv_1y / n_infants, 2), NA_real_),
    row_id = dplyr::row_number()
  ) %>%
  dplyr::select(-UMMPs_group)

#### 2. Patch population by UMMPs x season ------
rec = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(UMMPs = as.character(UMMPs),
                DateObs = as.Date(DateObs),
                Year_obs = lubridate::year(DateObs),
                Month = lubridate::month(DateObs),
                season_type = dplyr::case_when(Month %in% dry_months ~ "Dry",
                                               Month %in% wet_months ~ "Wet"),
                # Jan-Mar belong to the wet season that started the year before
                Season_start = dplyr::if_else(season_type %in% "Wet" & Month < season_first_month,
                                              Year_obs - 1L, Year_obs),
                season_id = dplyr::case_when(season_type == "Wet" ~ paste0("wet ", Season_start, "-", Season_start + 1),
                                             season_type == "Dry" ~ paste0("dry ", Season_start))) %>%
  dplyr::filter(!is.na(UMMPs), !is.na(season_id))

# Individuals, groups and monitoring effort in the patch, per season
pop_patch = rec %>%
  dplyr::group_by(UMMPs, season_type, Season_start, season_id) %>%
  dplyr::summarise(n_glt_patch = dplyr::n_distinct(Tattoo), # including bad individuals
                   n_groups_patch = dplyr::n_distinct(Group),
                   n_dates_patch = dplyr::n_distinct(DateObs), # monitoring effort
                   .groups = "drop") %>%
  dplyr::mutate(n_glt_patch = as.numeric(n_glt_patch),
                n_groups_patch = as.numeric(n_groups_patch))

# Population trend: season t vs the SAME season type of the previous year (wet vs wet, dry vs dry)
# When the lambda is extreme, the season BEFORE it is considered undersampled:
# its counts are replaced by the mean of the previous and following seasons (pop_infer = TRUE)
pop_patch = pop_patch %>%
  dplyr::arrange(UMMPs, season_type, Season_start) %>%
  dplyr::group_by(UMMPs, season_type) %>%
  dplyr::mutate(
    n_glt_raw = n_glt_patch,
    n_groups_raw = n_groups_patch,
    has_prev = dplyr::lag(Season_start, default = -Inf) == Season_start - 1,
    has_next = dplyr::lead(Season_start, default = Inf) == Season_start + 1,
    lambda_0 = dplyr::if_else(has_prev & dplyr::lag(n_glt_patch) > 0,
                              n_glt_patch / dplyr::lag(n_glt_patch), NA_real_),
    pop_infer = has_prev & has_next & (dplyr::lead(lambda_0) > lambda_max) %in% TRUE,
    n_glt_patch = dplyr::if_else(pop_infer, round((dplyr::lead(n_glt_raw) + dplyr::lag(n_glt_raw)) / 2), n_glt_raw),
    n_groups_patch = dplyr::if_else(pop_infer, round((dplyr::lead(n_groups_raw) + dplyr::lag(n_groups_raw)) / 2), n_groups_raw),
    lambda = dplyr::if_else(has_prev & dplyr::lag(n_glt_patch) > 0,
                            n_glt_patch / dplyr::lag(n_glt_patch), NA_real_)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-has_prev, -has_next, -lambda_0)

# Patch metrics are yearly: matched on the start year of the season
pop_patch = pop_patch %>%
  dplyr::left_join(UMMP_crosswalk, by = "UMMPs") %>%
  dplyr::left_join(patch_metrics, by = c("patch_id", "Season_start" = "Year")) %>%
  dplyr::mutate(area_km2 = area_ha / 100,
                dens_glt = round(n_glt_patch / area_km2, 2),
                dens_groups = round(n_groups_patch / area_km2, 2)) %>%
  dplyr::select(-n_glt_raw, -n_groups_raw, -pop_infer, -patch_id)

#### 3. Group composition (long-term data) -----
# Compute per group x season: group size (adults, immatures, total) and socionomic sex ratio
# Immatures = IN + JU; adults = everything else
group_season = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Group)) %>%
  dplyr::mutate(
    UMMPs = as.character(UMMPs),
    DateObs = as.Date(DateObs),
    Year_obs = lubridate::year(DateObs),
    Month = lubridate::month(DateObs),
    season_type = dplyr::case_when(Month %in% dry_months ~ "Dry",
                                   Month %in% wet_months ~ "Wet",
                                   TRUE ~ NA_character_),
    Season_start = dplyr::if_else(season_type %in% "Wet" & Month < season_first_month,
                                  Year_obs - 1L, Year_obs),
    season_id = dplyr::case_when(season_type == "Wet" ~ paste0("wet ", Season_start, "-", Season_start + 1),
                                 season_type == "Dry" ~ paste0("dry ", Season_start)),
    immature = IdadeOK %in% c("IN", "JU"),
    unidentified = GLT %in% bad
  ) %>%
  dplyr::filter(!is.na(season_id)) %>%
  # Composition on each date (unidentified records count in the group sizes but not in the sexes)
  dplyr::group_by(UMMPs, Group, season_id, Season_start, season_type, DateObs) %>%
  dplyr::summarise(n_immatures = dplyr::n_distinct(Tattoo[immature & !unidentified]) + sum(immature & unidentified),
                   n_adults = dplyr::n_distinct(Tattoo[!immature & !unidentified]) + sum(!immature & unidentified),
                   n_males = dplyr::n_distinct(Tattoo[!immature & !unidentified & SexOK %in% "M"]),
                   n_females = dplyr::n_distinct(Tattoo[!immature & !unidentified & SexOK %in% "F"]),
                   .groups = "drop") %>%
  dplyr::mutate(GS_total = n_adults + n_immatures,
                sex_ratio_date = dplyr::if_else(n_females > 0, n_males / n_females, NA_real_)) %>%
  # One row per group x season
  dplyr::group_by(UMMPs, Group, season_id, Season_start, season_type) %>%
  dplyr::summarise(n_dates = dplyr::n(),
                   mean_GS_adults = round(mean(n_adults), 2),
                   mean_GS_total = round(mean(GS_total), 2),
                   mean_immatures = round(mean(n_immatures), 2),
                   mean_males = round(mean(n_males), 2),
                   mean_females = round(mean(n_females), 2),
                   mean_sex_ratio = dplyr::if_else(all(is.na(sex_ratio_date)), NA_real_,
                                                   round(mean(sex_ratio_date, na.rm = TRUE), 2)),
                   .groups = "drop") %>%
  dplyr::arrange(Group, Season_start, season_type)

#### 4. Immigrants and emigrants (group flux) ------
# HERE, we use the GROUP FLUX data (see script 02a), aggregated by season
group_flux_season = group_flux %>%
  dplyr::mutate(
    DateObs = as.Date(DateObs),
    Year_obs = lubridate::year(DateObs),
    Month = lubridate::month(DateObs),
    season_type = dplyr::case_when(Month %in% dry_months ~ "Dry",
                                   Month %in% wet_months ~ "Wet",
                                   TRUE ~ NA_character_),
    Season_start = dplyr::if_else(season_type %in% "Wet" & Month < season_first_month,
                                  Year_obs - 1L, Year_obs),
    season_id = dplyr::case_when(season_type == "Wet" ~ paste0("wet ", Season_start, "-", Season_start + 1),
                                 season_type == "Dry" ~ paste0("dry ", Season_start))
  ) %>%
  dplyr::filter(!is.na(season_id)) %>%
  dplyr::group_by(Group, UMMPs, season_type, Season_start, season_id) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("n_"), sum), .groups = "drop")

#### 5. Join the long-term data to the base table (WET seasons only) ------
gs = gs %>%
  # Patch population, metrics and densities
  dplyr::left_join(pop_patch %>%
                     dplyr::filter(season_type == "Wet") %>%
                     dplyr::select(-season_type, -season_id),
                   by = c("UMMPs", "Season_start"), relationship = "many-to-one") %>%
  # Group composition
  dplyr::left_join(group_season %>%
                     dplyr::filter(season_type == "Wet") %>%
                     dplyr::select(-season_type, -season_id),
                   by = c("UMMPs", "Group", "Season_start"), relationship = "many-to-one") %>%
  # Immigrants and emigrants
  dplyr::left_join(group_flux_season %>%
                     dplyr::filter(season_type == "Wet") %>%
                     dplyr::select(-season_type, -season_id),
                   by = c("UMMPs", "Group", "Season_start"), relationship = "many-to-one")

#### 6. Floaters ------
# Region and UMMPs recorded on each GLT x date (one row per GLT x date, no duplicates)
rec_loc = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(DateObs = as.Date(DateObs), UMMPs = as.character(UMMPs)) %>%
  dplyr::group_by(Tattoo, DateObs) %>%
  dplyr::summarise(rec_Region = mode_ok(Region), rec_UMMPs = mode_ok(UMMPs), .groups = "drop")

# Satellite periods (one row = one satellite stay)
floaters = stays %>%
  dplyr::filter(stay_type == "satellite") %>%
  dplyr::left_join(rec_loc, by = c("Tattoo", "start" = "DateObs"), relationship = "many-to-one") %>%
  dplyr::transmute(sat_GLT = Tattoo,
                   sat_SexOK = SexOK,
                   sat_Region = rec_Region,
                   sat_UMMPs = dplyr::coalesce(UMMPs, rec_UMMPs),
                   # reference dates are the 1st of a month: the period starts at the 1st of the start month
                   sat_start = lubridate::floor_date(start, "month"),
                   sat_end = end) %>%
  dplyr::filter(!is.na(sat_UMMPs))

# Reference date = start of the wet season (July); window = N months BEFORE it
gs = gs %>%
  dplyr::mutate(ref_date = lubridate::make_date(Season_start, season_first_month, 1),
                fl_start = ref_date %m-% months(floater_window_months),
                fl_end = ref_date)

# Floaters of the patch whose satellite period overlaps the window
floaters_gs = gs %>%
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::select(row_id, UMMPs, fl_start, fl_end) %>%
  dplyr::inner_join(floaters,
                    by = dplyr::join_by(UMMPs == sat_UMMPs, fl_end >= sat_start, fl_start <= sat_end),
                    relationship = "many-to-many") %>%
  dplyr::group_by(row_id) %>%
  dplyr::summarise(n_floaters = dplyr::n_distinct(sat_GLT),
                   n_floaters_F = dplyr::n_distinct(sat_GLT[sat_SexOK %in% "F"]),
                   n_floaters_M = dplyr::n_distinct(sat_GLT[sat_SexOK %in% "M"]),
                   n_floaters_unknown = dplyr::n_distinct(sat_GLT[is.na(sat_SexOK)]),
                   .groups = "drop")

gs = gs %>%
  dplyr::left_join(floaters_gs, by = "row_id", relationship = "one-to-one") %>%
  dplyr::mutate(
    # No satellite period found = 0 floaters (only when the patch is known)
    dplyr::across(dplyr::starts_with("n_floaters"),
                  ~ dplyr::if_else(is.na(UMMPs), NA_integer_, tidyr::replace_na(.x, 0L))),
    dens_floaters = round(n_floaters / area_km2, 2),
    dens_floaters_F = round(n_floaters_F / area_km2, 2),
    dens_floaters_M = round(n_floaters_M / area_km2, 2)
  ) %>%
  dplyr::select(-row_id, -ref_date, -fl_start, -fl_end)

#### 7. Final table: logical order of the variables ------
repro_final = gs %>%
  dplyr::select(
    # 1) Group
    dplyr::any_of(c("Group", "UMMPs", "Fragment", "Group_isolation")),
    # 2) Time
    dplyr::any_of(c("Season", "Season_start", "season_assumed", "months_repro", "n_months_repro", "months_source")),
    # 3) Reproduction (litter of the season)
    dplyr::any_of(c("n_infants", "n_infants_per_female", "n_in_males", "n_in_females", "n_in_sex_unknown",
                    "litter_prop_males", "n_infants_surv_1y", "litter_surv_1y", "litter_source")),
    # 4) Reproductive females
    dplyr::any_of(c("n_repro_females", "id_repro_females", "status_repro", "n_dom", "n_sub",
                    "n_status_unknown", "status_complete")),
    # 5) Group composition (wet season)
    dplyr::any_of(c("n_dates", "mean_GS_adults", "mean_GS_total", "mean_immatures",
                    "mean_males", "mean_females", "mean_sex_ratio")),
    # 6) Dispersal and floaters
    dplyr::starts_with("n_immigrants"), dplyr::starts_with("n_emigrants"), dplyr::starts_with("n_founders"),
    dplyr::starts_with("n_floaters"), dplyr::starts_with("dens_floaters"),
    # 7) Patch: metrics, population, densities, trend
    dplyr::any_of(c("patch_id", "area_ha", "area_km2", "shape", "enn_m", "dPC", "IQH", "IQHS",
                    "n_glt_patch", "n_groups_patch", "n_dates_patch", "dens_glt", "dens_groups", "lambda")),
    # 8) Supplementation
    dplyr::any_of(c("n_days_supp_food", "n_months_supp_food", "mean_days.month_supp", "mean_days.year_supp")),
    # 9) Values of reprod kept for comparison (YEARLY values)
    dplyr::starts_with("reprod_"), dplyr::any_of("Patch_size_ha"),
    dplyr::everything()
  ) %>%
  dplyr::arrange(Group, Season_start)

#### 8. Checks ------
# reprod: one row per group x year?
reprod %>% dplyr::count(Group, Year) %>% dplyr::filter(n > 1)

# How many group-years are split in two seasons, and how many seasons are assumed
dplyr::count(gy_seasons, n_seasons_year)
dplyr::count(repro_final, season_assumed)


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