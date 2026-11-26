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
load(here::here("outputs", "data", "glt", "data_clean_long_final4.RData"))

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
# Reference dates for checking the group composition of each group
# This was done manually by seeing which dates correspond to the approximated mating dates in each group
# Mating dates = birth dates - gestation length (~132 days)
ref_tbl = readr::read_delim(here::here("data", "glt", "ACoelho", "data_clean", "ref_dates.csv"),
                          delim = ";",
                          col_types = readr::cols(.default = "c")) %>%
  dplyr::transmute(Group, 
                   Season,
                   ref_date_list = lubridate::mdy(ref_date),   # only used for the check below
                   ref_compo   = as.Date(lubridate::parse_date_time(ref_compo,   c("dmy", "ymd"))),
                   ref_jaccard = as.Date(lubridate::parse_date_time(ref_jaccard, c("dmy", "ymd"))))

#### Dispersal events -----
stays = xlsx::read.xlsx(
  here::here("outputs", "data", "dispersal", "All_GLT_stays.xlsx"),
  sheetName = "Sheet1"
)
group_flux = xlsx::read.xlsx(
  here::here("outputs", "data", "dispersal", "Group_flux.xlsx"),
  sheetName = "Sheet1"
)
sat_periods = xlsx::read.xlsx(
  here::here("outputs", "data", "dispersal", "sat_periods.xlsx"),
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
#
# FEMALES
#   - ids come from reprod 
#
# YEARLY VARIABLES OF REPROD (patch quality, supplementation, Andressa's counts) are taken from the
# reprod row Year = Season_start (otherwise Year = Season_start + 1) and are YEARLY values
#
# LONG-TERM DATA are matched to a mating reference date (birth - gestation length)
# A table with reference dates for each group and season was created manually and is imported to assess group composition, immigration and emigration
# Floaters are counted as satellite individuals during a +/- N month window around the mating reference date

#### Parameters ------
dry_months = 4:6  # Apr-Jun = dry season
wet_months = c(7:12, 1:3)  # Jul-Mar = wet season
season_last_month = 3  # Jan-Mar belong to the wet season that started the year before
gestation_days = 132  # GLT gestation ~125-132 days -> mating = birth - 132 days
floater_window_months = 1  # floaters: satellite periods overlapping mating date +/- N months
assumed_birth_month = 10 # births with no date are assumed on 1 October (late wet season)
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490")
litter_vars = c("n_infants", "n_in_males", "n_in_females", "n_in_sex_unknown", "n_infants_surv_1y")
# Portuguese month abbreviations used in reprod
pt_months = c(JAN = 1, FEV = 2, MAR = 3, ABR = 4, MAI = 5, JUN = 6,
              JUL = 7, AGO = 8, SET = 9, OUT = 10, NOV = 11, DEZ = 12)

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

##### INFANT BIRTHS in the long-term data ------
# Observations from the long-term data
obs = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(GLT = stringr::str_squish(GLT),
                DateObs = as.Date(DateObs),
                BirthOK = as.Date(BirthOK)) %>%
  dplyr::filter(!is.na(Group), !is.na(DateObs))

# Here, we use the long term data to identify infants and their birth dates
births_ind = obs %>%
  dplyr::filter(!is.na(GLT), 
                !GLT %in% bad) %>% # unidentified animals cannot be dated
  dplyr::arrange(GLT, DateObs) %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(
    # First observation as infant or juvenile
    first_IN_obs = if (any(IdadeOK %in% c("IN","JU"))) min(DateObs[IdadeOK %in% c("IN","JU")]) else as.Date(NA),
    # Group where the infant was first seen
    first_IN_group = if (any(IdadeOK %in% c("IN","JU"))) Group[IdadeOK %in% c("IN","JU")][1] else NA_character_,
    birth_ok = if (all(is.na(BirthOK))) as.Date(NA) else min(BirthOK, na.rm = TRUE),
    Sex = mode_ok(SexOK),
    .groups = "drop") %>%
  dplyr::filter(!is.na(first_IN_obs)) %>%  # only individuals seen as infants
  dplyr::transmute(GLT, Sex,
                   Group = first_IN_group,   # group where the infant was first seen
                   birth_date = dplyr::coalesce(birth_ok, first_IN_obs),
                   Year = as.numeric(lubridate::year(birth_date)),
                   season_type  = season_type_of(lubridate::month(birth_date)),
                   Season_start = season_start_of(Year, lubridate::month(birth_date)))

# We regroup infants by GROUP and SEASON
# A calendar year has up to 3 candidates: early wet (Jan-Mar), dry (Apr-Jun), late wet (Jul-Dec)
births_season = births_ind %>%
  dplyr::group_by(Group, Year, season_type, Season_start) %>%
  dplyr::summarise(n_data  = dplyr::n(),
                   n_data_m = sum(Sex %in% "M"),
                   n_data_f = sum(Sex %in% "F"),
                   id_infants  = paste(GLT, collapse = ", "),
                   first_birth = min(birth_date),
                   .groups = "drop")

# List of females present in groups in seasons
fem_seen = obs %>%
  # Select adult females
  dplyr::filter(SexOK %in% "F", !IdadeOK %in% c("IN", "JU")) %>%
  dplyr::transmute(Group, 
                   GLT,
                   season_type  = season_type_of(lubridate::month(DateObs)),
                   Season_start = season_start_of(lubridate::year(DateObs), lubridate::month(DateObs)),
                   female_seen = TRUE) %>%
  dplyr::distinct()

# Every identified individual with its first record in a group
# tier 1 = BirthOK recorded
# tier 2 = no BirthOK, first recorded as immature (IN/JU)
# tier 3 = no BirthOK and no age class at first record, never adult within 300 days (likely an unrecorded infant)
ind_first = obs %>%
  dplyr::filter(!is.na(GLT), !GLT %in% bad) %>%
  dplyr::arrange(GLT, DateObs) %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(first_date = dplyr::first(DateObs),
                   first_group = dplyr::first(Group),
                   n_groups_first = dplyr::n_distinct(Group[DateObs == dplyr::first(DateObs)]),
                   first_age = dplyr::first(IdadeOK),
                   adult_early = any(IdadeOK %in% "AD" & DateObs <= dplyr::first(DateObs) + 300),
                   birth_ok = if (all(is.na(BirthOK))) as.Date(NA) else min(BirthOK, na.rm = TRUE),
                   Sex= mode_ok(SexOK),
                   .groups = "drop") %>%
  dplyr::filter(n_groups_first == 1) %>%  # ONE group at the first record
  dplyr::mutate(tier_rank = dplyr::case_when(!is.na(birth_ok) ~ 1L,
                                             first_age %in% c("IN", "JU") ~ 2L,
                                             is.na(first_age) & !adult_early ~ 3L),
                ref_date = dplyr::coalesce(birth_ok, first_date),
                Sex_c = dplyr::if_else(Sex %in% c("M", "F"), Sex, "U")) %>%
  dplyr::filter(!is.na(tier_rank)) # adults are out



##### STATUS: precisely dated reproductive events ------
# Each status row = one female reproductive event. 
# The recorded date is the birth date;
# Mating is estimated by subtracting gestation_days.

# Parameters of the search for the infants
birth_tol_months = 1  # tier 1: BirthOK within +/- N months of the status date
first_seen_window_months = 4 # tiers 2-3: infant first seen up to N months after the status date
max_first_gap_months = 12   # tier 1: first record at most N months after the status date

# Status events
status_ev0 = status %>%
  dplyr::mutate(GLT = stringr::str_squish(GLT),
                Date = as.Date(Date),
                Year = as.numeric(lubridate::year(Date)),
                season_type = season_type_of(lubridate::month(Date)),
                Season_start = season_start_of(Year, lubridate::month(Date)),
                dplyr::across(dplyr::all_of(litter_vars), as.numeric),
                birth_date = Date,
                repro_date = Date - lubridate::days(gestation_days),
                birth_month = lubridate::floor_date(Date, "month"),
                season_assumed = FALSE)

# Litter to find, per group x birth month (two females in the same month share the pool)
gm = status_ev0 %>%
  dplyr::group_by(Group, birth_month) %>%
  dplyr::summarise(n_events = dplyr::n(),
                   n_tot = sum(n_infants, na.rm = TRUE),
                   n_m = sum(n_in_males, na.rm = TRUE),
                   n_f = sum(n_in_females, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::filter(n_tot > 0)

# Candidates of each group x birth month
cand = gm %>%
  dplyr::inner_join(ind_first, by = c("Group" = "first_group"), relationship = "many-to-many") %>%
  dplyr::filter(
    dplyr::case_when(
      tier_rank == 1 ~ birth_ok >= birth_month %m-% months(birth_tol_months) &
        birth_ok <  birth_month %m+% months(birth_tol_months + 1) &
        first_date < birth_month %m+% months(max_first_gap_months),
      TRUE           ~ first_date >= birth_month %m-% months(1) &
        first_date <  birth_month %m+% months(first_seen_window_months + 1))) %>%
  dplyr::mutate(dist = abs(as.numeric(ref_date - birth_month))) %>%
  dplyr::arrange(Group, birth_month, tier_rank, dist, GLT)    # best tier first, then closest to the status date

# Selection: sexes of status first, then fill up to n_infants
sel1 = cand %>%
  dplyr::group_by(Group, birth_month, Sex_c) %>%
  dplyr::mutate(rk = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::filter((Sex_c == "M" & rk <= n_m) | (Sex_c == "F" & rk <= n_f))

n_sel1 = dplyr::count(sel1, Group, birth_month, name = "n_sel1")

sel2 = cand %>%
  dplyr::anti_join(sel1, by = c("Group", "birth_month", "GLT")) %>%
  dplyr::left_join(n_sel1, by = c("Group", "birth_month")) %>%
  dplyr::mutate(n_sel1 = dplyr::coalesce(n_sel1, 0L)) %>%
  dplyr::group_by(Group, birth_month) %>%
  dplyr::mutate(rk = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(rk <= n_tot - n_sel1)

ids_gm = dplyr::bind_rows(sel1, sel2) %>%
  dplyr::arrange(Group, birth_month, ref_date, GLT) %>%
  dplyr::group_by(Group, birth_month) %>%
  dplyr::summarise(id_infants = paste(GLT, collapse = ", "),
                   n_found = dplyr::n(),
                   n_found_m = sum(Sex_c == "M"),
                   n_found_f = sum(Sex_c == "F"),
                   worst_tier = max(tier_rank),
                   .groups = "drop")

### Manual corrections
manual_ids = tibble::tribble(
  ~Group, ~Date,        ~GLT,   ~id_manual,
  "AP", "2002-10-01", "ST6",  "AS15, AS16",
  "AP", "2002-10-01", "?",    "AS17",
  "AP", "2003-03-01", "ST6",  "AS18",
  "AP", "2003-10-01", "ST6",  "AS19, AS20",
  "AP", "2003-10-01", "?",    "AS21",
  "EM", "2003-11-01", "E15",  "E28, E29",
  "EM", "2003-11-01", "?",    "E30",
  "EM", "2004-10-01", "?",    "E31?",
  "EM", "2004-11-01", "E15",  "E32?, E33",
  "EM", "2006-11-01", "E15",  "E34, E35",
  "EM", "2006-11-01", "?",    "E36",
  "MA", "2002-11-01", "MA19", "MA27, MA28",
  "MA", "2002-11-01", "CM13", "MA25, MA26",
  "MA", "2003-10-01", "MA19", "MA31, MA32",
  "MA", "2003-10-01", "CM13", "MA29, MA30",
  "OL", "1999-10-01", "OL3",  "OL11, OL12",
  "OL", "1999-11-01", "OL1",  "OL13?, OL14",
  "RV", "2004-03-01", "RV3",  "?", 
  "RV", "2004-03-01", "MA9",  "?",
  "SP", "2006-10-01", "SP2",  "?",
  "SP", "2006-10-01", "OL15", "?",
  "ST", "1999-10-01", "CM11", "ST27, ST28",
  "ST", "1999-09-01", "ST13", "ST25?, ST26?",
  "ST", "2001-09-01", "?",    "ST33?",
  "ST", "2001-09-01", "CM11", "ST34, ST35",
  "ST", "2003-09-01", "CM11", "?",
  "ST", "2003-09-01", "?",    "?",
  "ST", "2004-09-01", "CM11", "?", 
  "ST", "2004-09-01", "?",    "?",
  "RV", "2003-11-01", "RV3",  "?",
  "MA", "2002-09-01", "?",    "?",
  "FX", "2002-10-01", "OL9",  "FX7, FX8, ?, ?",
  "FX", "2002-10-01", "OL5",  "FX7, FX8, ?, ?",
  "TR", "2001-11-01", "TR10", "?",
  "TR", "2001-11-01", "E6",   "?",
  "TR", "2002-10-01", "TR10", "?",
  "TR", "2002-10-01", "E6",   "?",
  "TR", "2003-10-01", "E6", "TR25, TR26",
  "TR", "2003-11-01", "?",   "TR27",
  "ST", "2006-07-01", "ST18", "?",
  "ST", "2006-07-01", "CM11", "?") %>%
  dplyr::mutate(Date = as.Date(Date))

# Back to the status events
status_ev = status_ev0 %>%
  dplyr::left_join(gm,  by = c("Group", "birth_month"), relationship = "many-to-one") %>%
  dplyr::left_join(ids_gm, by = c("Group", "birth_month"), relationship = "many-to-one") %>%
  # Manual corrections: the correction replaces the matched ids, otherwise the regular value is kept
  dplyr::left_join(manual_ids, by = c("Group", "birth_date" = "Date", "GLT"),
                   relationship = "many-to-one") %>%
  dplyr::mutate(
    id_infants = dplyr::case_when(
      !is.na(id_manual) ~ id_manual,
      TRUE ~ id_infants)) %>%
  dplyr::select(Group, Year, season_type, Season_start, GLT, Status, birth_date, repro_date,
                dplyr::all_of(litter_vars), id_infants, season_assumed)

##### REPROD: group-years not covered by status ------
# What is left = years with one reproductive female (known or "?") + years with no reproduction
reprod_rest = reprod %>%
  dplyr::mutate(Year = as.numeric(Year),
                dplyr::across(c(dplyr::all_of(litter_vars), n_reprod_females), as.numeric)) %>%
  # Remove two-reproductive female groups
  dplyr::anti_join(dplyr::distinct(status_ev, Group, Year), by = c("Group", "Year")) %>%
  
  # Mutate reproduction type depending on the number of reproductive females
  dplyr::mutate(reprod_type = dplyr::case_when(
    n_infants %in% 0 | n_reprod_females %in% 0 ~ "none",    # no reproduction that year
    n_reprod_females %in% 1 ~ "single",   # one reproductive female
    TRUE ~ NA))


##### REPROD: retrieve season ------
# The dates of the reproductive events are lacking in the reprod data (aggregated by year)
# We want to create a table with reproductive events aggregated by mating season (e.g., wet 1999-2000)
# For each row in reprod, compare the litter of reprod (n infants, males, females) with the infants born in each candidate season of the same group and year.
# The number of infants born in a group a certain year (in reprod) can be matched with the number of infants recorded in a group a certain year in the long term data -> to ultimately get their birth dates 

##### Manual birth dates
# First, we enter manual births dates of infants for specific rows
# The birth can fall in the previous calendar year (e.g., reprod 1999 -> birth 09/1998).
manual_births = tibble::tribble(
  ~Year, ~Group, ~GLT,   ~birth_my, ~id_infants,
  1999,  "AP",   "ST6",  "09/1998", "AS4, AS5",
  2000,  "EM",   "E15",  "10/1999", "E20, E21",
  2002,  "AT",   "PT4",  "12/2001", "AT8, AT9",
  2002,  "OL",   "OL1",  "05/2002", "OL19, OL20",
  2003,  "FX",   "OL5",  "12/2003", "FX9, FX10",
  2004,  "OL",   "OL1",  "11/2003", "OL22, OL23",
  2005,  "SP",   "OL15", "12/2005", "SP5"
  ) %>%
  dplyr::mutate(birth_date = lubridate::my(birth_my)) %>%  # 1st of the month
  dplyr::select(-birth_my)

reprod_single = reprod_rest %>%
  # Remove rows without reproductionn
  dplyr::filter(reprod_type != "none") %>%
  # join the manual dates (only apply to single-female rows)
  dplyr::left_join(manual_births, by = c("Group", "Year", "id_reprod_females" = "GLT"),
                   relationship = "many-to-one") %>%
  dplyr::mutate(birth_manual = dplyr::if_else(reprod_type == "single", birth_date, as.Date(NA))) %>%
  dplyr::select(-birth_date)

# Extract manual corrections
manual_ev = reprod_single %>%
  dplyr::filter(!is.na(birth_manual)) %>%
  dplyr::transmute(Group, Year,
                   season_type = season_type_of(lubridate::month(birth_manual)),
                   Season_start = season_start_of(lubridate::year(birth_manual), lubridate::month(birth_manual)),
                   GLT = id_reprod_females, 
                   Status = NA_character_,
                   birth_date = birth_manual,
                   # Compute the approximate mating date (i.e., birth-gestation)
                   repro_date = birth_manual - lubridate::days(gestation_days),
                   dplyr::across(dplyr::all_of(litter_vars)),
                   id_infants, 
                   season_assumed = FALSE, 
                   match_type = "manual")

##### Single females who reproduced twice
# For females reproducing twice a year, the months are given in reprod
# E.g., "JAN/OUT" = births in January and October of Year (1st of the month assumed).
# Months falling in the same season are collapsed into one event (the earliest month).
months_tbl = reprod_single %>%
  dplyr::filter(is.na(birth_manual), 
                # Keep rows corresponding to females reproducing twice a year
                reprod_type == "single",
                !is.na(months_with_several_reprod_events)) %>%
  dplyr::select(Group, 
                Year, 
                id_reprod_females, 
                n_infants_year = n_infants,
                months_with_several_reprod_events) %>%
  tidyr::separate_longer_delim(months_with_several_reprod_events, delim = "/") %>%
  # Month name from portuguese abbreviations
  dplyr::mutate(Month = unname(pt_months[stringr::str_to_upper(stringr::str_squish(months_with_several_reprod_events))])) %>%
  dplyr::mutate(birth_date = lubridate::make_date(Year, Month, 1), # Reconstruct birth date from Year/Month in the table
                season_type = season_type_of(Month),
                Season_start = season_start_of(Year, Month)) %>%
  dplyr::arrange(Group, Year, birth_date) %>%
  dplyr::distinct(Group, Year, id_reprod_females, season_type, Season_start, .keep_all = TRUE)

# The litter of each event cannot be split from reprod (yearly totals):
# it is taken from the infants born in that season in the long-term data
months_ev = months_tbl %>%
  # Join the infants born each season (by year)
  dplyr::left_join(births_season, by = c("Group", "Year", "season_type", "Season_start"),
                   relationship = "many-to-one") %>%
  dplyr::transmute(Group, 
                   Year, 
                   season_type, 
                   Season_start,
                   GLT = id_reprod_females, 
                   Status = NA_character_,
                   birth_date,
                   # Approximate mating date
                   repro_date = birth_date - lubridate::days(gestation_days),
                   n_infants = n_data, 
                   n_in_males = n_data_m, 
                   n_in_females = n_data_f,
                   n_in_sex_unknown = n_data - n_data_m - n_data_f,
                   n_infants_surv_1y = NA_real_, # cannot be split between the two events
                   id_infants, 
                   season_assumed = FALSE)

### MANUAL CORRECTIONS
# Correct NA rows
months_ev = months_ev %>%
  dplyr::mutate(
    n_infants = dplyr::case_when(
      GLT == "CM13" & Group == "MA" & Year == 2000 & Season_start == 2000 & is.na(n_infants) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 1999 & is.na(n_infants) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 2000 & is.na(n_infants) ~ 3,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2001 & is.na(n_infants) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2002 & is.na(n_infants) ~ 1,
      GLT == "MA9" & Group == "RV" & Year == 2005 & Season_start == 2005 & is.na(n_infants) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2006 & Season_start == 2005 & is.na(n_infants) ~ 2,
      TRUE ~ n_infants),
    n_in_males = dplyr::case_when(
      GLT == "CM13" & Group == "MA" & Year == 2000 & Season_start == 2000 & is.na(n_in_males) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 1999 & is.na(n_in_males) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 2000 & is.na(n_in_males) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2001 & is.na(n_in_males) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2002 & is.na(n_in_males) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2005 & Season_start == 2005 & is.na(n_in_males) ~ 1,
      GLT == "MA9" & Group == "RV" & Year == 2006 & Season_start == 2005 & is.na(n_in_males) ~ 2,
      TRUE ~ n_in_males),
    n_in_females = dplyr::case_when(
      GLT == "CM13" & Group == "MA" & Year == 2000 & Season_start == 2000 & is.na(n_in_females) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 1999 & is.na(n_in_females) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 2000 & is.na(n_in_females) ~ 1,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2001 & is.na(n_in_females) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2002 & is.na(n_in_females) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2005 & Season_start == 2005 & is.na(n_in_females) ~ 1,
      GLT == "MA9" & Group == "RV" & Year == 2006 & Season_start == 2005 & is.na(n_in_females) ~ 0,
      TRUE ~ n_in_females),
    n_in_sex_unknown = dplyr::case_when(
      GLT == "CM13" & Group == "MA" & Year == 2000 & Season_start == 2000 & is.na(n_in_sex_unknown) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 1999 & is.na(n_in_sex_unknown) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 2000 & is.na(n_in_sex_unknown) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2001 & is.na(n_in_sex_unknown) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2002 & is.na(n_in_sex_unknown) ~ 1,
      GLT == "MA9" & Group == "RV" & Year == 2005 & Season_start == 2005 & is.na(n_in_sex_unknown) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2006 & Season_start == 2005 & is.na(n_in_sex_unknown) ~ 0,
      TRUE ~ n_in_sex_unknown),
    n_infants_surv_1y = dplyr::case_when(
      GLT == "CM13" & Group == "MA" & Year == 2000 & Season_start == 2000 & is.na(n_infants_surv_1y) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 1999 & is.na(n_infants_surv_1y) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 2000 & is.na(n_infants_surv_1y) ~ 3,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2001 & is.na(n_infants_surv_1y) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2002 & is.na(n_infants_surv_1y) ~ 0,
      GLT == "MA9" & Group == "RV" & Year == 2005 & Season_start == 2005 & is.na(n_infants_surv_1y) ~ 2,
      GLT == "MA9" & Group == "RV" & Year == 2006 & Season_start == 2005 & is.na(n_infants_surv_1y) ~ 2,
      id_infants == "BE1, BE2" ~ 2,
      id_infants == "BE3" ~ 1,
      id_infants == "MA18, MA19" ~ 2,
      id_infants == "RV28, RV29" ~ 2,
      id_infants == "RV3, RV4" ~ 1,
      id_infants == "RV34, RV35" ~ 2,
      id_infants == "RV6, RV7" ~ 2,
      id_infants == "ST15, ST16" ~ 1,
      id_infants == "ST18" ~ 1,
      id_infants == "TR11" ~ 1,
      id_infants == "TR12, TR13" ~ 2,
      id_infants == "TR15, TR16" ~ 2,
      id_infants == "TR15, TR16" ~ 2,
      id_infants == "TR17, TR18" ~ 2,
      id_infants == "TR29, TR30" ~ 2,
      TRUE ~ n_infants_surv_1y),
    id_infants = dplyr::case_when(
      GLT == "CM13" & Group == "MA" & Year == 2000 & Season_start == 2000 & is.na(id_infants) ~ "MA20, MA21",
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 1999 & is.na(id_infants) ~ "RV6, RV7",
      GLT == "MA9" & Group == "RV" & Year == 2000 & Season_start == 2000 & is.na(id_infants) ~ "RV10, RV11, RV12",
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2001 & is.na(id_infants) ~ "RV14, RV15",
      GLT == "MA9" & Group == "RV" & Year == 2002 & Season_start == 2002 & is.na(id_infants) ~ "RV16",
      GLT == "MA9" & Group == "RV" & Year == 2005 & Season_start == 2005 & is.na(id_infants) ~ "RV30, RV31",
      GLT == "MA9" & Group == "RV" & Year == 2006 & Season_start == 2005 & is.na(id_infants) ~ "RV32, RV33",
      TRUE ~ id_infants)
  )

## MANUAL CORRECTIONS: Infants to add, events to remove, birth dates to change
months_ev = months_ev %>%
  
  # 1. Delete events
  dplyr::filter(
    !(Group == "RV" & Year == 2002 & season_type == "wet" & Season_start == 2002),  # RV16 moved to the Sept 2001 litter
    !(Group == "RV" & Year == 2000 & season_type == "wet" & Season_start == 1999),  # RV6, RV7 already counted in reprod 1999
    !(Group == "RV" & Year == 2000 & season_type == "dry" & Season_start == 2000),  # recreated below
    !(Group == "TR" & Year == 2004 & season_type == "wet" & Season_start == 2003)) %>%  # recreated below
  
  # 2. Create events that do not exist yet
  dplyr::bind_rows(
    tibble::tibble(
      Group = c("RV", "TR"),
      Year = c(2000, 2004),
      season_type = c("dry", "wet"),
      Season_start = c(2000, 2003),
      # Female of the group-year (same GLT as the other events of the same group-year)
      GLT = c(dplyr::first(months_ev$GLT[months_ev$Group == "RV" & months_ev$Year == 2000]),
              dplyr::first(months_ev$GLT[months_ev$Group == "TR" & months_ev$Year == 2004])),
      birth_date = as.Date(c("2000-05-01", "2003-10-01")),
      n_infants = c(2, 1),
      n_in_males = c(0, 0),
      n_in_females = c(0, 1),
      n_in_sex_unknown = c(2, 0),
      n_infants_surv_1y = c(0, 1),
      id_infants = c("RV8, RV9", "TR27"),
      season_assumed = FALSE)) %>%
  
  # 3. Modify existing events
  dplyr::mutate(
    n_infants = dplyr::case_when(
      Group == "AX2" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ 3, # + BE4
      Group == "RV" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ 4, # RV13, RV14, RV15, RV16
      TRUE ~ n_infants),
    n_in_males = dplyr::case_when(
      Group == "RV" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ 3, # RV13, RV14, RV15
      TRUE ~ n_in_males),
    n_in_females = dplyr::case_when(
      Group == "RV" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ 0,
      TRUE ~ n_in_females),
    n_in_sex_unknown = dplyr::case_when(
      Group == "AX2" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ 1, # BE4
      Group == "RV" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ 1, # RV16
      TRUE ~ n_in_sex_unknown),
    n_infants_surv_1y = dplyr::case_when(
      Group == "AX2" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ 3, # BE1, BE2, BE4 survive
      Group == "RV" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ 2, # RV14, RV15 survive; RV16 does not; RV13 not specified
      TRUE ~ n_infants_surv_1y),
    id_infants = dplyr::case_when(
      Group == "AX2" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ "BE1, BE2, BE4",
      Group == "RV" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ "RV13, RV14, RV15, RV16",
      TRUE ~ id_infants),
    birth_date = dplyr::case_when(
      Group == "RV" & Year == 2002 & season_type == "wet" & Season_start == 2001 ~ as.Date("2001-09-01"),
      TRUE ~ birth_date),
    # Mating date recalculated for all rows from the (possibly corrected) birth date
    repro_date = birth_date - lubridate::days(gestation_days),
    Status = NA_character_) %>%
  dplyr::arrange(Group, Year, birth_date)

### Validation: number of infants in REPROD vs the total in the long term data
months_ev %>% 
  dplyr::select(Group, Year, Season_start, n_infants) %>% 
  dplyr::left_join(
    months_tbl %>% 
      dplyr::distinct(Group, Year, n_infants_year),
    by = c("Group", "Year"),
    relationship = "many-to-one"
  ) %>%
  dplyr::group_by(Group, Year) %>% 
  dplyr::summarise(
    n_infants_sum = sum(n_infants, na.rm = TRUE),
    n_infants_year = dplyr::first(n_infants_year),
    n_missing = sum(is.na(n_infants)),
    sum_ok = dplyr::case_when(
      n_missing > 0 ~ "INCOMPLETE",
      is.na(n_infants_year) ~ NA_character_,
      n_infants_sum == n_infants_year ~ "OK",
      TRUE ~ "NOT OK"
    ),
    .groups = "drop"
  ) %>% 
  dplyr::arrange(Group, Year)


##### All other events
# For the other reproductive events, we implement the season according to 
# i) the number of infants
# ii) their sex
# iii) the presence of the female in the group

# Create the table with the remaining reprod events
reprod_match = reprod_single %>%
  dplyr::filter(is.na(birth_manual)) %>%
  dplyr::anti_join(dplyr::distinct(months_tbl, Group, Year, id_reprod_females),
                   by = c("Group", "Year", "id_reprod_females")) %>%
  dplyr::select(-birth_manual)

# Match
match_ev = reprod_match %>%
  dplyr::select(-id_infants) %>% 
  # Join infant births (long-term data)
  dplyr::left_join(births_season, by = c("Group", "Year"), relationship = "one-to-many") %>%
  # Join female presence
  dplyr::left_join(fem_seen, by = c("Group", "season_type", "Season_start", "id_reprod_females" = "GLT"),
                   relationship = "many-to-one") %>%
  # Rules
  dplyr::mutate(
    id_before = id_infants,
    n_infants_dead_1y = as.numeric(n_infants_dead_1y),
    # The number in the long term dataset should equal the number in reprod
    count_ok = dplyr::coalesce(n_data == n_infants, FALSE),
    # Number of infants, per sex
    sex_ok = count_ok & dplyr::coalesce(is.na(n_in_males) | is.na(n_in_females) |
                                            (n_data_m == n_in_males & n_data_f == n_in_females), FALSE),
    # Reproductive female present in the group
    # for females with "?": group and year only
    female_seen_ok  = id_reprod_females %in% "?" | dplyr::coalesce(female_seen, FALSE),
    # IF THE KEY = 7 -> all good (4 + 2 +1)
    key = 4 * count_ok + 2 * sex_ok + female_seen_ok,
    # Difference between the number of infants in the long-term dataset and the ones in reprod
    gap = abs(n_data - n_infants))

# keep one candidate season per female-year
match_ev = match_ev %>%
  dplyr::mutate(corrected = dplyr::coalesce(id_infants != id_before, FALSE)) %>%
  dplyr::arrange(Group, Year, id_reprod_females, dplyr::desc(corrected), dplyr::desc(key), gap, first_birth) %>%
  dplyr::group_by(Group, Year, id_reprod_females) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup()

# Corrections
match_ev = match_ev %>% 
  dplyr::mutate(
    id_infants = dplyr::case_when(
      Group == "AP" & Year == 1997 & Season_start == 1997 ~ "AS3, AS2 (?)",
      Group == "MA" & Year == 1998 & Season_start == 1998 ~ "MA14, MA15",
      Group == "MA" & Year == 1999 & Season_start == 1999 ~ "MA16, MA17",
      Group == "AF" & Year == 2006 & Season_start == 2006 ~ "AF7, AF8",
      Group == "SP" & Year == 2003 & Season_start == 2003 ~ "SP2",
      Group == "OL" & Year == 2007 & Season_start == 2007 ~ "OL30, OL31",
      # Rows with NAs
      Group == "AT" & Year == 2000 & is.na(Season_start) ~ "AT6, AT7",
      Group == "FX" & Year == 2000 & is.na(Season_start) ~ "FX1, FX2",
      Group == "RV" & Year == 2001 & is.na(Season_start) ~ "RV13",
      TRUE ~ id_infants
    ),
    first_birth = dplyr::case_when(
      Group == "AP" & Year == 1997 & Season_start == 1997 ~ as.Date("1997-09-01"),
      Group == "MA" & Year == 1998 & Season_start == 1998 ~ as.Date("1998-09-01"),
      Group == "MA" & Year == 1999 & Season_start == 1999 ~ as.Date("1999-09-01"),
      Group == "AF" & Year == 2006 & Season_start == 2006 ~ as.Date("2006-10-01"),
      Group == "SP" & Year == 2003 & Season_start == 2003 ~ as.Date("2003-11-01"),
      Group == "OL" & Year == 2007 & Season_start == 2007 ~ as.Date("2007-10-01"),
      # Rows with NAs
      Group == "AT" & Year == 2000 & is.na(Season_start) ~ as.Date("2000-10-01"),
      Group == "FX" & Year == 2000 & is.na(Season_start) ~ as.Date("2000-10-01"),
      Group == "RV" & Year == 2001 & is.na(Season_start) ~ as.Date("2001-09-01"),
      TRUE ~ first_birth
    ),
    season_type = dplyr::case_when(
      # Rows with NAs
      Group == "AT" & Year == 2000 & is.na(Season_start) ~ "wet",
      Group == "FX" & Year == 2000 & is.na(Season_start) ~ "wet",
      Group == "RV" & Year == 2001 & is.na(Season_start) ~ "wet",
      TRUE ~ season_type
    ),
    Season_start = dplyr::case_when(
      # Rows with NAs
      Group == "AT" & Year == 2000 & is.na(Season_start) ~ 2000,
      Group == "FX" & Year == 2000 & is.na(Season_start) ~ 2000,
      Group == "RV" & Year == 2001 & is.na(Season_start) ~ 2001,
      TRUE ~ Season_start
    ),
    # Group SP (wrong number of infants)
    n_infants = dplyr::case_when(
      Group == "SP" & Year == 2003 & Season_start == 2003 ~ 1,
      TRUE ~ n_infants
    ),
    n_in_males = dplyr::case_when(
      Group == "SP" & Year == 2003 & Season_start == 2003 ~ 0,
      TRUE ~ n_in_males
    ),
    n_in_females = dplyr::case_when(
      Group == "SP" & Year == 2003 & Season_start == 2003 ~ 1,
      TRUE ~ n_in_females
    ),
    n_in_sex_unknown = dplyr::case_when(
      Group == "SP" & Year == 2003 & Season_start == 2003 ~ 0,
      TRUE ~ n_in_sex_unknown
    ),
    n_infants_dead_1y = dplyr::case_when(
      Group == "SP" & Year == 2003 & Season_start == 2003 ~ 0,
      TRUE ~ n_infants_dead_1y
    ),
    n_infants_surv_1y = dplyr::case_when(
      Group == "SP" & Year == 2003 & Season_start == 2003 ~ 1,
      TRUE ~ n_infants_surv_1y
    )
  )

# Remove the rows to drop
match_ev = match_ev %>%
  dplyr::filter(!(Group == "AF" & id_infants == "AF7, AF8" & season_type %in% "dry"))

# Same format as the other tables
match_ev = match_ev %>%
  dplyr::mutate(
    season_assumed = is.na(Season_start),   # no infant found in the data: late wet season
    birth_date = first_birth,
    repro_date = birth_date - lubridate::days(gestation_days)) %>%
  dplyr::transmute(Group, Year, season_type, Season_start,
                   GLT = id_reprod_females, 
                   Status = NA_character_,
                   birth_date, 
                   repro_date,
                   dplyr::across(dplyr::all_of(litter_vars)),
                   id_infants, 
                   season_assumed
                   )

# COMBINE ALL
reprod_events = dplyr::bind_rows(
  dplyr::select(manual_ev, Group, Year, season_type, Season_start, GLT, Status, birth_date, repro_date,
                dplyr::all_of(litter_vars), id_infants, season_assumed),
  dplyr::select(months_ev, Group, Year, season_type, Season_start, GLT, Status, birth_date, repro_date,
                dplyr::all_of(litter_vars), id_infants, season_assumed),
  match_ev)

##### MERGE ------
## Combine status and reprod data
status_ev = status_ev %>%
  dplyr::select(Group, Year, season_type, Season_start, GLT, Status, birth_date, repro_date,
                dplyr::all_of(litter_vars), id_infants, season_assumed)

# Bind rows
repro_events = dplyr::bind_rows(status_ev, reprod_events) %>%
  dplyr::mutate(Season = season_label(season_type, Season_start)) %>%
  dplyr::relocate(Year, Season_start, Season, season_type, Group, GLT)

##### CORRECT FEMALE ID -----
##### Name the "?" females with the females named in reprod
# Named females of reprod, by group x year
named = reprod %>%
  dplyr::transmute(Group, Year = as.numeric(Year), id = id_reprod_females) %>%
  dplyr::filter(!is.na(id)) %>%
  tidyr::separate_longer_delim(id, delim = ",") %>%
  dplyr::transmute(Group, Year, GLT_named = stringr::str_squish(id)) %>%
  dplyr::filter(GLT_named != "", !GLT_named %in% bad) %>%
  dplyr::distinct()

# Event id
repro_events = repro_events %>% dplyr::mutate(ev_id = dplyr::row_number())

# Females named in 'reprod' and not in the final table
free = dplyr::anti_join(named, repro_events,
                        by = c("Group", "Year", "GLT_named" = "GLT"))

# Events with an unidentified female ("?")
q = repro_events %>%
  dplyr::filter(GLT == "?") %>%
  dplyr::select(ev_id, Group, Year, season_type, Season_start)

# Replace a "?" with a name given in the 'reprod' table
fill = q %>%
  dplyr::inner_join(free, by = c("Group", "Year"), relationship = "many-to-many") %>%
  dplyr::group_by(ev_id) %>% dplyr::filter(dplyr::n() == 1) %>% dplyr::ungroup() %>%
  dplyr::select(ev_id, GLT_fill = GLT_named)

# Replace
repro_events = repro_events %>%
  dplyr::left_join(fill, by = "ev_id", relationship = "one-to-one") %>%
  dplyr::mutate(GLT = dplyr::coalesce(GLT_fill, GLT)) %>%
  dplyr::select(-GLT_fill, -ev_id)

# Keep distinct reproductive events per female and season
repro_events = repro_events %>%
  dplyr::distinct(Season, Group, GLT, repro_date, .keep_all = TRUE) %>%
  # Remove one problematic ro
  dplyr::filter(!(Year == 1999 & Group %in% "AP" & GLT %in% "ST6"))

##### FEMALE STATUS IN THE GROUP ----
# Join the female status in the group
stays_grp = stays %>%
  dplyr::filter(stay_type == "group") %>%
  dplyr::transmute(Group, GLT = stringr::str_squish(GLT), 
                   stay_start = start, 
                   stay_end = end, 
                   status_in_grp)

repro_events = repro_events %>% dplyr::mutate(ev_id = dplyr::row_number())

grp_status = repro_events %>%
  dplyr::select(ev_id, Group, GLT, repro_date, repro_date) %>%
  dplyr::inner_join(stays_grp,
                    by = dplyr::join_by(Group, GLT, repro_date >= stay_start, repro_date <= stay_end),
                    relationship = "many-to-many") %>%
  dplyr::group_by(ev_id) %>%
  dplyr::summarise(status_in_grp = paste(sort(unique(status_in_grp)), collapse = "/"), .groups = "drop")

repro_events = repro_events %>%
  dplyr::left_join(grp_status, by = "ev_id", relationship = "one-to-one") %>%
  dplyr::select(-ev_id)

### CORRECT THE STATUS IN GRP
repro_events = repro_events %>% 
  dplyr::mutate(
    status_in_grp = dplyr::case_when(
      GLT == "JG3" & Group == "AX2" ~ "founder",
      GLT == "MA9" & Group == "RV"  ~ "unknown",
      GLT == "PT4" & Group == "AT" ~ "immigrant",
      is.na(status_in_grp)  ~ "unknown",
      TRUE ~ status_in_grp))


##### BIRTH DATE AND AGE AT MATING OF THE FEMALES -----
# Birth date of each individual
glt_birth = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(GLT = stringr::str_squish(GLT), BirthOK = as.Date(BirthOK)) %>%
  dplyr::filter(!is.na(GLT), !GLT %in% bad, !is.na(BirthOK)) %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(female_birth_date = min(BirthOK), .groups = "drop")

# Age at birth recorded in 'status' (age of the female at the litter's birth date)
status_age = status %>%
  dplyr::transmute(Group,
                   GLT = stringr::str_squish(GLT),
                   birth_date = as.Date(Date),
                   age_birth_status = as.numeric(Age_months)) %>%
  dplyr::filter(!GLT %in% bad, !is.na(age_birth_status)) %>%
  dplyr::group_by(Group, GLT, birth_date) %>%
  dplyr::summarise(age_birth_status = mean(age_birth_status), .groups = "drop")

repro_events = repro_events %>%
  dplyr::left_join(glt_birth, by = "GLT", relationship = "many-to-one") %>%
  dplyr::left_join(status_age, by = c("Group", "GLT", "birth_date"), relationship = "many-to-one") %>%
  dplyr::mutate(
    # age at mating from the BirthOK of the long-term data
    age_mating_months = round(as.numeric(repro_date - female_birth_date) / 30.4, 1),
    # age at mating from status = age at birth - gestation
    age_mating_status = round(age_birth_status - gestation_days / 30.4, 1),
    # difference between both estimates (positive = status gives an older female)
    age_diff = age_mating_status - age_mating_months)

# Comparison of the two estimates (events with both ages)
age_cmp = repro_events %>%
  dplyr::filter(!is.na(age_mating_months), !is.na(age_mating_status))
cor(age_cmp$age_mating_months, age_cmp$age_mating_status)  # Pearson
# Visual check (the line is y = x)
ggplot2::ggplot(age_cmp, ggplot2::aes(age_mating_months, age_mating_status)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
  ggplot2::geom_point(alpha = 0.6)

# COALESCE
# long-term data (BirthOK) first, then status (age at birth - gestation)
repro_events = repro_events %>%
  dplyr::mutate(age_mating_months = dplyr::coalesce(age_mating_months, age_mating_status))

# Birth date for females with a known age at mating but no BirthOK
# birth = mating date - age at mating (estimated once per female, median over her events)
birth_fill = repro_events %>%
  dplyr::filter(!GLT %in% bad, 
                is.na(female_birth_date),
                !is.na(age_mating_months), 
                !is.na(repro_date)) %>%
  dplyr::mutate(birth_est = as.numeric(repro_date) - age_mating_months * 30.4) %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(n_est = dplyr::n(),
                   spread_days = round(max(birth_est) - min(birth_est)),
                   birth_est = as.Date(round(median(birth_est)), origin = "1970-01-01"),
                   .groups = "drop")

# Join the estimated births and recompute
repro_events = repro_events %>%
  dplyr::left_join(dplyr::select(birth_fill, GLT, birth_est), 
                   by = "GLT", relationship = "many-to-one") %>%
  dplyr::mutate(
    female_birth_date = dplyr::coalesce(female_birth_date, birth_est),
    # Age at mating for the other events of these females (still NA)
    age_mating_months = dplyr::coalesce(
      age_mating_months,
      round(as.numeric(repro_date - female_birth_date) / 30.4, 1))) %>%
  # Remove the intermediate columns: only the birth date and the age at mating remain
  dplyr::select(-c(birth_est, age_birth_status, age_mating_status, age_diff))

# Checks
sum(is.na(repro_events$age_mating_months[!repro_events$GLT %in% bad])) # identified females still without age

##### COMPLETE STATUS -----
max_status_gap = 5 # a status is borrowed from another row of the same female only within N years
repro_events = repro_events %>%
  dplyr::mutate(row_id = dplyr::row_number())

# 1. Same female, same group: status of her closest row (in time) with a recorded status
known = repro_events %>%
  dplyr::filter(!is.na(Status), 
                !GLT %in% bad) %>%
  dplyr::distinct(Group, GLT, Season_start_k = Season_start, Status_k = Status)

fill_same = repro_events %>%
  dplyr::filter(is.na(Status), !GLT %in% bad) %>%
  dplyr::select(row_id, Group, GLT, Season_start) %>%
  dplyr::inner_join(known, by = c("Group", "GLT"), relationship = "many-to-many") %>%
  dplyr::mutate(gap = abs(Season_start - Season_start_k)) %>%
  dplyr::filter(gap <= max_status_gap) %>%
  dplyr::arrange(row_id, gap, Season_start_k) %>% # closest row, the earlier one in case of a tie
  dplyr::group_by(row_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(row_id, Status_fill = Status_k)

repro_events = repro_events %>%
  dplyr::left_join(fill_same, by = "row_id", relationship = "one-to-one") %>%
  dplyr::mutate(Status = dplyr::coalesce(Status, Status_fill)) %>%
  dplyr::select(-Status_fill)

# 2. Another female of the same group is ALF -> this female is SUB
alf_other = repro_events %>%
  dplyr::filter(Status == "ALF") %>%
  dplyr::distinct(Group, season_type, Season_start, GLT_alf = GLT)

sub_rows = repro_events %>%
  dplyr::filter(is.na(Status)) %>%
  dplyr::inner_join(alf_other, by = c("Group", "season_type", "Season_start"), relationship = "many-to-many") %>%
  dplyr::filter(GLT != GLT_alf) %>%
  dplyr::distinct(row_id) %>%
  dplyr::mutate(sub_fill = "SUB")

repro_events = repro_events %>%
  dplyr::left_join(sub_rows, by = "row_id", relationship = "one-to-one") %>%
  dplyr::mutate(Status = dplyr::coalesce(Status, sub_fill)) %>%
  dplyr::select(-sub_fill, -row_id)

dplyr::count(repro_events, Status)   # NA / NA = still unknown (e.g., "?" females)


##### CHECKS -----
##### CHECK 1: females reproducing twice (months_with_several_reprod_events) ------
# Expected: one event per female x distinct season of the listed months
# (JAN/DEZ 2002 -> wet 2001-2002 and wet 2002-2003; JUL/OUT 2004 -> same season = ONE event)
# Events of the final table, with the season label recomputed from the birth date is not needed:
# Season is already in repro_events
re = repro_events

# Females reproducing twice a year
exp_twice = reprod %>%
  dplyr::filter(!is.na(months_with_several_reprod_events)) %>%
  dplyr::transmute(Group, Year = as.numeric(Year), n_reprod_females, id_reprod_females,
                   token = months_with_several_reprod_events) %>%
  tidyr::separate_longer_delim(token, delim = "/") %>%
  dplyr::mutate(Month  = unname(pt_months[stringr::str_to_upper(stringr::str_squish(token))]),
                Season = season_label(season_type_of(Month), season_start_of(Year, Month))) %>%
  dplyr::distinct(Group, Year, n_reprod_females, id_reprod_females, Season) %>%
  tidyr::separate_longer_delim(id_reprod_females, delim = ",") %>%
  dplyr::mutate(GLT = stringr::str_squish(id_reprod_females)) %>%
  dplyr::filter(GLT != "", GLT != "?")                       # "?" cannot be traced to a female

# 1a. females reproducing in seasons missing in the final table
check_twice_missing = exp_twice %>%
  dplyr::anti_join(re, by = c("Group", "GLT", "Season")) %>%
  dplyr::select(Group, Year, GLT, Season, n_reprod_females)

# 1b. ... in number of seasons
check_twice_count = exp_twice %>%
  dplyr::group_by(Group, Year, GLT) %>%
  dplyr::summarise(n_exp = dplyr::n_distinct(Season), .groups = "drop") %>%
  dplyr::left_join(
    re %>% dplyr::filter(!is.na(birth_date)) %>%
      dplyr::inner_join(dplyr::distinct(exp_twice, Group, Year, GLT, Season),
                        by = c("Group", "GLT", "Season"), suffix = c("", "_exp")) %>%
      dplyr::group_by(Group, Year = Year_exp, GLT) %>%
      dplyr::summarise(n_found = dplyr::n_distinct(Season), .groups = "drop"),
    by = c("Group", "Year", "GLT"), relationship = "one-to-one") %>%
  dplyr::mutate(n_found = dplyr::coalesce(n_found, 0L)) %>%
  dplyr::filter(n_exp != n_found)

##### CHECK 2: several females reproducing in the same month (months_where_several_females_reprod) ------
# Expected: in that group and season, one row per reproductive female
# (MA 2004 SET/OUT: CM13 and MA19 -> two rows in wet 2004-2005)
exp_several = reprod %>%
  dplyr::filter(!is.na(months_where_several_females_reprod)) %>%
  dplyr::transmute(Group, Year = as.numeric(Year), n_reprod_females, id_reprod_females,
                   token = months_where_several_females_reprod) %>%
  tidyr::separate_longer_delim(token, delim = "/") %>%
  dplyr::mutate(Month  = unname(pt_months[stringr::str_to_upper(stringr::str_squish(token))]),
                Season = season_label(season_type_of(Month), season_start_of(Year, Month))) %>%
  dplyr::distinct(Group, Year, n_reprod_females, id_reprod_females, Season)

# 2a. fewer rows than reproductive females in that group x season
check_several_n = exp_several %>%
  dplyr::left_join(re %>% dplyr::count(Group, Season, name = "n_rows"),
                   by = c("Group", "Season"), relationship = "many-to-one") %>%
  dplyr::mutate(n_rows = dplyr::coalesce(n_rows, 0L)) %>%
  dplyr::filter(n_rows < n_reprod_females)

# 2b. named females of reprod without a row in that group x season
check_several_ids = exp_several %>%
  tidyr::separate_longer_delim(id_reprod_females, delim = ",") %>%
  dplyr::mutate(GLT = stringr::str_squish(id_reprod_females)) %>%
  dplyr::filter(GLT != "", GLT != "?") %>%
  dplyr::anti_join(re, by = c("Group", "GLT", "Season")) %>%
  dplyr::select(Group, Year, GLT, Season, n_reprod_females)

##### Summary
tibble::tibble(
  check = c("1a twice: female x season missing",
            "1b twice: number of seasons differs",
            "2a several females: fewer rows than females",
            "2b several females: named female missing"),
  n_rows = c(nrow(check_twice_missing), nrow(check_twice_count),
             nrow(check_several_n), nrow(check_several_ids)))
# 2 reproductive events involving RV3 are missing but it's normal -> this females DOES NOT reproduce but the other one in the group does several times (confusion in the reprod dataset)
# -> IGNORE 1a & 1b

##### CORRECTIONS -----
repro_events = repro_events %>%
  dplyr::mutate(season_type = dplyr::if_else(Group == "MA" & Season == "dry 1999", "wet", season_type),
                Season = season_label(season_type, Season_start))

#### 1. Group x season table (reproduction) ------
##### 1a. Summarise reproductive events by group x season ------
# A season can gather several events (2 females) and two reprod years (late t + early t+1)
events_gs = repro_events %>%
  dplyr::group_by(Group, season_type, Season_start, Season) %>%
  dplyr::summarise(
    # reprod year that supplies the annual covariates (t for late events, t + 1 for Jan-Mar only)
    Year = if (any(Year == first(Season_start))) first(Season_start) else first(Season_start) + 1,
    
    #  Summary
    n_repro_females  = dplyr::n_distinct(GLT),
    id_repro_females = paste(sort(unique(GLT)), collapse = ", "),
    n_dom = dplyr::n_distinct(GLT[Status %in% "ALF"]),
    n_sub = dplyr::n_distinct(GLT[!is.na(Status) & Status != "ALF"]),
    n_status_unknown = dplyr::n_distinct(GLT[is.na(Status)]),
    n_repro_natal = dplyr::n_distinct(GLT[status_in_grp %in% "natal"]),
    status_in_grp = dplyr::na_if(paste(sort(unique(stats::na.omit(status_in_grp))), collapse = "/"), ""),
    
    # Dominant: the ALF female(s); if there is no ALF and ONE female with unknown status, she is considered the dominant
    dom_assumed = n_dom == 0 & n_status_unknown == 1,
    age_mating_dom = {
      a = if (dom_assumed) age_mating_months[is.na(Status)] else age_mating_months[Status %in% "ALF"]
      if (all(is.na(a))) NA_real_ else round(mean(a, na.rm = TRUE), 1)},
    age_mating_sub = {
      a = age_mating_months[!is.na(Status) & Status != "ALF"]
      if (all(is.na(a))) NA_real_ else round(mean(a, na.rm = TRUE), 1)},
    # Age difference between the subordinate and the dominant at mating (positive = subordinate is older)
    age_diff_sub_dom = round(abs(age_mating_sub - age_mating_dom), 1),
      
    # Litter size
    dplyr::across(dplyr::all_of(litter_vars), ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    id_infants = dplyr::na_if(paste(stats::na.omit(id_infants), collapse = ", "), ""),
    first_birth = min_date(birth_date),
    repro_date_first = min_date(repro_date), # earliest mating of the season
    season_assumed = all(season_assumed),
    .groups = "drop") %>% 
  dplyr::select(-dom_assumed)

##### 1b. Add group-years with no reproduction ------
# In 'reprod', years without reproductive events are also recorded -> we join them back
# These rows are assigned to the late wet season because no event date exists
zero_gs_all = reprod_rest %>%
  dplyr::filter(reprod_type == "none") %>%
  dplyr::transmute(Group, Year, 
                   season_type = "wet", 
                   Season_start = Year,
                   n_repro_females = 0L,
                   dplyr::across(dplyr::all_of(litter_vars), ~ dplyr::coalesce(.x, 0)),
                   season_assumed = TRUE)

# If the same group x season already exists (e.g., Jan-Mar births of the next year), drop the zero row
zero_gs = dplyr::anti_join(zero_gs_all, 
                           events_gs, by = c("Group", "season_type", "Season_start"))

##### 1c. Annual covariates from the reprod dataset ------
# These variables remain annual; they are joined using the selected reprod Year.
reprod_yearly = reprod %>%
  dplyr::transmute(Group, 
                   Year = as.numeric(Year), 
                   UMMPs, 
                   Fragment, 
                   IQH, 
                   IQHS, 
                   Group_isolation,
                   n_days_supp_food, 
                   n_months_supp_food, 
                   mean_days.month_supp, 
                   mean_days.year_supp,
                   n_immigrants_reprod = n_immigrants,
                   n_immigrants_M_reprod = n_immig_males,
                   n_immigrants_F_reprod = n_immig_females)

##### 1d. Build the group-season table and define mating reference dates ------
# If mating cannot be dated, use the assumed 1 October birth date minus gestation.
gs = dplyr::bind_rows(events_gs, zero_gs) %>%
  dplyr::left_join(reprod_yearly, by = c("Group", "Year"), relationship = "many-to-one") %>%
  dplyr::mutate(
    Season = season_label(season_type, Season_start),
    # Reference date = mating of the season (earliest event). 
    # No date: assumed birth (1 Oct) - gestation
    ref_date = dplyr::coalesce(repro_date_first,
                               lubridate::make_date(Season_start, assumed_birth_month, 1) -
                                 lubridate::days(gestation_days)),

    # Litter characteristics
    litter_prop_males = dplyr::if_else(n_in_males + n_in_females > 0,
                                       round(n_in_males / (n_in_males + n_in_females), 2), NA_real_),
    litter_surv_1y = dplyr::if_else(n_infants > 0, round(n_infants_surv_1y / n_infants, 2), NA_real_),
    row_id = dplyr::row_number()) %>% 
  dplyr::relocate(row_id, 
                  Group, Year, Season, season_type, Season_start,
                  first_birth, repro_date_first,
                  ref_date)

# JOIN REFERENCE DATES FOR MEASURING GROUP COMPOSITION
gs = gs %>% 
  dplyr::left_join(ref_tbl, by = c("Group", "Season"), relationship = "many-to-one")

# Checks
dplyr::filter(gs, is.na(ref_compo)) %>% dplyr::select(Group, Season)   # rows without a reference date
# mating date differs from the list
gs %>% dplyr::filter(!is.na(ref_date_list), ref_date_list != as.Date(ref_date)) %>%
  dplyr::select(Group, Season, ref_date, ref_date_list)   

# REMOVE NAs
gs = gs %>% 
  dplyr::filter(!is.na(ref_compo))

#### 2. Group composition at the reference dates ------
# For each group x season, the composition is the one recorded on ref_compo (see ref_tbl).
# The reproductive females (? excluded) are added to the adults if they were not recorded that day.
# For the Jaccard index: we compare the adults at t with the “reference set” at t-1, which is the adults at t-1 plus the immatures at t-1 who are adults at t
# Immigrants and emigrants follow the same logic. An immigrant is an adult at t who was absent at t-1, as adult or immature. An emigrant is an adult at t-1 who is absent at t.

# Sex of each identified individual
sex_tbl = obs %>%
  dplyr::filter(!is.na(GLT), !GLT %in% bad) %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(Sex = mode_ok(SexOK), .groups = "drop")

# 2a. Observation dates of each group
obs_dates = obs %>% dplyr::distinct(Group, DateObs) %>% dplyr::mutate(found = TRUE)

# Identified individuals present on each date (one row = group x date x individual)
ids_date = obs %>%
  dplyr::mutate(immature = IdadeOK %in% c("IN", "JU"),
                unidentified = is.na(GLT) | GLT %in% bad | is.na(Tattoo),
                dead = Death %in% "1") %>%
  dplyr::filter(!unidentified, !dead) %>%
  dplyr::group_by(Group, DateObs, GLT) %>%
  dplyr::summarise(stage = dplyr::if_else(any(!immature), "adult", "immature"), .groups = "drop")

# Unidentified individuals: counted in the group size, but cannot be tracked throughout time
unid_date = obs %>%
  dplyr::mutate(immature = IdadeOK %in% c("IN", "JU"),
                unidentified = is.na(GLT) | GLT %in% bad | is.na(Tattoo),
                dead = Death %in% "1") %>%
  dplyr::group_by(Group, DateObs) %>%
  dplyr::summarise(n_adults_unid = sum(!immature & unidentified & !dead),
                   n_immatures_unid = sum(immature & unidentified & !dead),
                   .groups = "drop")

# 2b. Reference dates of each row, with the previous row of the group
rowmap = gs %>%
  dplyr::select(row_id, Group, Season, Season_start, season_type, ref_compo, ref_jaccard) %>%
  # Join the observation dates (for t)
  dplyr::left_join(dplyr::rename(obs_dates, compo_found = found),
                   by = c("Group", "ref_compo" = "DateObs"), relationship = "many-to-one") %>%
  # Join the observation dates (for t-1, i.e., jaccard)
  dplyr::left_join(dplyr::rename(obs_dates, jaccard_found = found),
                   by = c("Group", "ref_jaccard" = "DateObs"), relationship = "many-to-one") %>%
  dplyr::mutate(compo_found = dplyr::coalesce(compo_found, FALSE)) %>% 
  dplyr::arrange(Group, Season_start, season_type) %>%   # dry season (Apr-Jun) comes before wet
  dplyr::group_by(Group) %>%
  dplyr::mutate(prev_row_id = dplyr::lag(row_id),
                prev_row_found = dplyr::lag(compo_found)) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    # t-1 = date given in ref_jaccard if any, otherwise the snapshot of the previous row
    prev_source = dplyr::if_else(!is.na(ref_jaccard), "ref_jaccard", "previous row"),
    prev_found = dplyr::if_else(!is.na(ref_jaccard),
                                dplyr::coalesce(jaccard_found, FALSE),
                                dplyr::coalesce(prev_row_found, FALSE)),
    both_found = compo_found & prev_found)

# 2c. Snapshot at t: identified individuals present + reproductive females (as adults)
repro_add = repro_events %>%
  # Remove bad individuals
  dplyr::filter(!is.na(GLT), 
                !GLT %in% bad) %>%
  dplyr::distinct(Group, Season, GLT) %>%
  # Join by the ref. date for COMPOSITION
  dplyr::inner_join(dplyr::select(rowmap, row_id, Group, Season, compo_found),
                    by = c("Group", "Season"), relationship = "many-to-one") %>%
  dplyr::filter(compo_found) %>%
  dplyr::transmute(row_id, GLT, stage = "adult")

# Group members at t
now_members = rowmap %>%
  dplyr::filter(compo_found) %>%
  dplyr::select(row_id, Group, ref_compo) %>%
  # Corresspondence between date of obs and reference date
  dplyr::inner_join(ids_date, by = c("Group", "ref_compo" = "DateObs"), relationship = "one-to-many") %>%
  dplyr::select(row_id, GLT, stage) %>%
  # Add reproductive females
  dplyr::bind_rows(repro_add) %>%
  dplyr::group_by(row_id, GLT) %>%
  dplyr::summarise(stage = dplyr::if_else(any(stage == "adult"), "adult", "immature"), .groups = "drop")

# 2d. Snapshot at t-1 (attached to the row at t)
prev_exc = rowmap %>%  # exceptions: date of ref_jaccard
  dplyr::filter(!is.na(ref_jaccard), jaccard_found %in% TRUE) %>%
  dplyr::select(row_id, Group, ref_jaccard) %>%
  dplyr::inner_join(ids_date, by = c("Group", "ref_jaccard" = "DateObs"), relationship = "one-to-many") %>%
  dplyr::select(row_id, GLT, stage_prev = stage)

prev_row = rowmap %>% # general case: snapshot of the previous row
  dplyr::filter(is.na(ref_jaccard), !is.na(prev_row_id)) %>%
  dplyr::select(row_id, prev_row_id) %>%
  dplyr::inner_join(now_members, by = c("prev_row_id" = "row_id"), relationship = "one-to-many") %>%
  dplyr::select(row_id, GLT, stage_prev = stage)

prev_members = dplyr::bind_rows(prev_exc, prev_row)

# 2e. Compare t-1 and t, individual by individual
pair = now_members %>%
  dplyr::rename(stage_now = stage) %>%
  dplyr::full_join(prev_members, by = c("row_id", "GLT")) %>%
  dplyr::left_join(sex_tbl, by = "GLT", relationship = "many-to-one") %>%
  dplyr::mutate(
    is_adult_now = stage_now %in% "adult",
    # reference set at t-1 = adults at t-1 + immatures at t-1 that are adults at t
    in_ref_prev = stage_prev %in% "adult" | (stage_prev %in% "immature" & is_adult_now),
    # immigrant = adult at t, absent at t-1 (neither adult nor immature)
    is_immigrant = is_adult_now & is.na(stage_prev),
    # emigrant = adult at t-1, absent at t (neither adult nor immature)
    is_emigrant = stage_prev %in% "adult" & is.na(stage_now))

# Change between both dates
change = pair %>%
  dplyr::group_by(row_id) %>%
  dplyr::summarise(
    n_shared = sum(is_adult_now & in_ref_prev),
    n_union = sum(is_adult_now | in_ref_prev),
    n_immigrants = sum(is_immigrant),
    n_immigrants_F = sum(is_immigrant & Sex %in% "F"),
    n_immigrants_M = sum(is_immigrant & Sex %in% "M"),
    n_emigrants = sum(is_emigrant),
    n_emigrants_F = sum(is_emigrant & Sex %in% "F"),
    n_emigrants_M = sum(is_emigrant & Sex %in% "M"),
    immigrants_ids = paste(sort(GLT[is_immigrant]), collapse = ", "),
    emigrants_ids = paste(sort(GLT[is_emigrant]), collapse = ", "),
    # Adult males: reference set at t-1 = adult males at t-1 + immature males at t-1 now adult
    n_males_prev = sum(stage_prev %in% "adult" & Sex %in% "M"),
    n_males_shared = sum(is_adult_now & in_ref_prev & Sex %in% "M"),
    n_males_union  = sum((is_adult_now | in_ref_prev) & Sex %in% "M"),
    .groups = "drop")

# 2f. Composition at t
comp_n = now_members %>%
  dplyr::left_join(sex_tbl, by = "GLT", relationship = "many-to-one") %>%
  dplyr::group_by(row_id) %>%
  dplyr::summarise(
    n_adults_id = sum(stage == "adult"),
    n_immatures_id = sum(stage == "immature"),
    n_males = sum(stage == "adult" & Sex %in% "M"),
    n_females = sum(stage == "adult" & Sex %in% "F"),
    adults_present = paste(sort(GLT[stage == "adult"]), collapse = ", "),
    immatures_present = paste(sort(GLT[stage == "immature"]), collapse = ", "),
    .groups = "drop")

# 2g. Final composition table (one row per row_id of gs)
compo_gs = rowmap %>%
  dplyr::left_join(comp_n, by = "row_id", relationship = "one-to-one") %>%
  dplyr::left_join(unid_date, by = c("Group", "ref_compo" = "DateObs"), relationship = "many-to-one") %>%
  dplyr::left_join(change, by = "row_id", relationship = "one-to-one") %>%
  dplyr::mutate(
    # Composition: NA if the group was not observed on ref_compo, 0 if observed with nobody
    dplyr::across(c(n_adults_id, n_immatures_id, n_males, n_females, n_adults_unid, n_immatures_unid),
                  ~ dplyr::if_else(compo_found, dplyr::coalesce(.x, 0L), NA_integer_)),
    # Changes: NA if one of the two snapshots is missing
    dplyr::across(c(n_shared, n_union, n_immigrants, n_immigrants_F, n_immigrants_M,
                    n_emigrants, n_emigrants_F, n_emigrants_M,
                    n_males_prev, n_males_shared, n_males_union),
                  ~ dplyr::if_else(both_found, dplyr::coalesce(.x, 0L), NA_integer_)),
    dplyr::across(c(adults_present, immatures_present),
                  ~ dplyr::if_else(compo_found, dplyr::na_if(dplyr::coalesce(.x, ""), ""), NA_character_)),
    dplyr::across(c(immigrants_ids, emigrants_ids),
                  ~ dplyr::if_else(both_found, dplyr::na_if(dplyr::coalesce(.x, ""), ""), NA_character_)),
    n_adults = n_adults_id + n_adults_unid,
    n_immatures = n_immatures_id + n_immatures_unid,
    GS_total = n_adults + n_immatures,
    sex_ratio = dplyr::if_else(n_females > 0, round(n_males / n_females, 2), NA_real_),
    # Jaccard on adults: adults at t vs (adults at t-1 + immatures at t-1 who became adults)
    adult_jaccard = dplyr::if_else(n_union > 0, round(n_shared / n_union, 2), NA_real_),
    adult_turnover = 1 - adult_jaccard,
    n_males_replaced = pmin(n_emigrants_M, n_immigrants_M), # males lost AND replaced by immigrants
    male_replacement_prop = dplyr::if_else(n_males_prev > 0, round(n_males_replaced / n_males_prev, 2), NA_real_),
    male_turnover = dplyr::if_else(n_males_union > 0, round(1 - n_males_shared / n_males_union, 2), NA_real_)) %>%
  dplyr::select(row_id, compo_date = ref_compo, compo_found, prev_source,
                GS_total, n_adults, n_immatures, n_males, n_females, sex_ratio,
                adult_jaccard, adult_turnover,
                n_immigrants, n_immigrants_F, n_immigrants_M,
                n_emigrants, n_emigrants_F, n_emigrants_M,
                adults_present, immatures_present, immigrants_ids, emigrants_ids,
                n_males_replaced, male_replacement_prop, male_turnover)

# Checks
dplyr::count(compo_gs, compo_found) # FALSE = no observation of the group on ref_compo
dplyr::count(compo_gs, prev_source)
# Example EM, ref_compo 2001-09-01 (expected: 4 shared, 9 in the union, jaccard 0.44)
ex = gs %>% dplyr::filter(Group == "EM") %>% dplyr::select(row_id, Season) %>%
  dplyr::inner_join(compo_gs, by = "row_id") %>%
  dplyr::select(Season, compo_date, adults_present, adult_jaccard, adult_turnover, immigrants_ids, emigrants_ids)

# Remove not useful columns
compo_gs = compo_gs %>% dplyr::select(-c(compo_found, prev_source))

#### 3. Patch population: yearly counts per UMMP ------
# One value per UMMPs
# Counts = distinct identified individuals / groups recorded in the patch during that year.
obs_patch = obs %>%
  dplyr::filter(!is.na(UMMPs), !is.na(DateObs)) %>%
  dplyr::transmute(
    UMMPs = as.character(UMMPs), Group, GLT, DateObs,
    Season_start = season_start_of(lubridate::year(DateObs), lubridate::month(DateObs))) %>%
  dplyr::distinct()

patch_year = obs_patch %>%
  dplyr::group_by(UMMPs, Season_start) %>%
  dplyr::summarise(
    n_glt_patch = dplyr::n_distinct(GLT[!is.na(GLT) & !GLT %in% bad]),  # identified animals only
    n_groups_patch = dplyr::n_distinct(Group),
    n_obs_dates = dplyr::n_distinct(DateObs),      # monitoring effort, to check the counts
    .groups = "drop")

# Previous year (t-1) for lambda; only valid if the patch was monitored in t-1
patch_prev = patch_year %>%
  dplyr::transmute(UMMPs, 
                   Season_start = Season_start + 1, 
                   n_glt_prev = n_glt_patch)

patch_gs = patch_year %>%
  dplyr::left_join(patch_prev, by = c("UMMPs", "Season_start"), relationship = "one-to-one") %>%
  dplyr::mutate(
    lambda = dplyr::if_else(!is.na(n_glt_prev) & n_glt_prev > 0,
                            round(n_glt_patch / n_glt_prev, 2), NA_real_)) %>%
  dplyr::select(-n_glt_prev)


#### 4. Floaters: satellites at mating +/- 1 month ------
# Count distinct individuals in a satellite period (sat_periods, from the dispersal script)
# overlapping [ref_date - N months, ref_date + N months], within the same UMMP
# WARNING: floaters (short window) and patch population (yearly) are on different time scales,
# which matters when computing, e.g., floaters per patch individual

win_patch = gs %>%
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::transmute(row_id, 
                   UMMPs = as.character(UMMPs),
                   ref_date,
                   window_start = ref_date %m-% months(floater_window_months),
                   window_end = ref_date %m+% months(floater_window_months))

floaters_gs = win_patch %>%
  dplyr::inner_join(
    dplyr::mutate(sat_periods, f_start = lubridate::floor_date(f_start, "month")),
    by = dplyr::join_by(UMMPs, window_end >= f_start, window_start <= f_end),
    relationship = "many-to-many") %>%
  dplyr::group_by(row_id) %>%
  dplyr::summarise(n_floaters   = dplyr::n_distinct(GLT),
                   n_floaters_F = dplyr::n_distinct(GLT[SexOK %in% "F"]),
                   n_floaters_M = dplyr::n_distinct(GLT[SexOK %in% "M"]),
                   .groups = "drop")

#### 5. Join all derived datasets and calculate patch densities ------
# Most joins below are based on row_id (one row per group-season)
gs = gs %>%
  # GROUP COMPOSITION
  dplyr::left_join(compo_gs, by = "row_id", relationship = "one-to-one") %>%
  # PATCH POP SIZE (yearly, by patch and season start)
  dplyr::left_join(patch_gs, by = c("UMMPs", "Season_start"), relationship = "many-to-one") %>%
  # FLOATERS
  dplyr::left_join(floaters_gs, by = "row_id", relationship = "one-to-one") %>%
  
  # Patch metrics are yearly: matched on the year of mating (columns also in reprod get the suffix _pm)
  dplyr::left_join(UMMP_crosswalk, by = "UMMPs") %>%
  dplyr::left_join(patch_metrics, by = c("patch_id", "Season_start" = "Year"), suffix = c("", "_pm")) %>%
  dplyr::mutate(
    area_km2 = area_ha / 100,
    dens_glt = round(n_glt_patch / area_km2, 2),
    dens_groups = round(n_groups_patch / area_km2, 2),
    # No satellite stay found = 0 floaters (only when the patch is known)
    dplyr::across(dplyr::starts_with("n_floaters"),
                  ~ dplyr::if_else(is.na(UMMPs), NA_integer_, tidyr::replace_na(.x, 0L))),
    dens_floaters = round(n_floaters / area_km2, 2),
    dens_floaters_F = round(n_floaters_F / area_km2, 2),
    dens_floaters_M = round(n_floaters_M / area_km2, 2)) %>% 
  # Fill immigrants from reprod ONLY where the two-date method gave NA
  dplyr::mutate(
    fill_imm = is.na(n_immigrants) & !is.na(n_immigrants_reprod),
    n_immigrants = dplyr::if_else(fill_imm, as.integer(n_immigrants_reprod), n_immigrants),
    n_immigrants_M = dplyr::if_else(fill_imm, as.integer(n_immigrants_M_reprod), n_immigrants_M),
    n_immigrants_F = dplyr::if_else(fill_imm, as.integer(n_immigrants_F_reprod), n_immigrants_F)) %>%
  dplyr::select(-fill_imm)

#### 6. Assemble and order the final analytical table ------
repro_final = gs %>%
  dplyr::mutate(mating_date = ref_date) %>%
  dplyr::select(
    dplyr::any_of(c("Group", "UMMPs", "Fragment", "Group_isolation")),
    dplyr::any_of(c("Season", "season_type", "Season_start", "season_assumed", "mating_date")),
    # reproduction
    dplyr::any_of(c("n_infants", "n_in_males", "n_in_females", "n_in_sex_unknown", "n_infants_surv_1y",
                    "litter_prop_males", "litter_surv_1y", "first_birth", "id_infants")),
    dplyr::any_of(c("n_repro_females", "id_repro_females", "n_dom", "n_sub", "n_status_unknown",
                    "age_mating_dom", "age_mating_sub", "age_diff_sub_dom")),
    dplyr::any_of(c("n_repro_natal", "status_in_grp")),
    # composition, immigrants, emigrants, floaters
    dplyr::any_of(c("compo_date",
                    "GS_total", "n_adults", "n_immatures", "n_males", "n_females", "sex_ratio",
                    "adult_jaccard", "adult_turnover",
                    "n_males_replaced", "male_replacement_prop", "male_turnover")),
    dplyr::starts_with("n_immigrants"), dplyr::starts_with("n_emigrants"),
    dplyr::any_of(c("adults_present", "immatures_present", "immigrants_ids", "emigrants_ids")),
    dplyr::starts_with("n_floaters"), dplyr::starts_with("dens_floaters"),
    # patch
    dplyr::any_of(c("area_km2", "shape", "enn_m", "dPC", "IQH", "IQHS", 
                    "n_glt_patch", "n_groups_patch",
                    "dens_glt", "dens_groups", 
                    "lambda")),
    dplyr::any_of(c("n_days_supp_food", "n_months_supp_food", "mean_days.month_supp", "mean_days.year_supp"))) %>%
  dplyr::arrange(Group, Season_start, season_type)


#### 7. Export ------
# Female-level: one row = one reproductive event of a female
repro_females_export = repro_events %>%
  dplyr::select(Group, Season, season_type, Season_start, Year, GLT, Status, status_in_grp,
                female_birth_date, age_mating_months, birth_date, repro_date,
                dplyr::all_of(litter_vars), id_infants, season_assumed) %>%
  dplyr::arrange(Group, Season_start, season_type, GLT)

# Group-level: one row = one group x season (including seasons without reproduction)
repro_groups_export = repro_final

readr::write_csv(repro_females_export, here::here("outputs", "data", "glt", "repro_females.csv"))
readr::write_csv(repro_groups_export,  here::here("outputs", "data", "glt", "repro_groups.csv"))

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