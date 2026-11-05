#------------------------------------------------#
# Author: Romain Monassier
# Objective: Analysis long-term GLT data
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
mat_system = xlsx::read.xlsx(
  here::here("data", "glt", "ACoelho", "dadosAndressa_V2.xlsx"),
  sheetName = "Sheet1"
)

#### Group locations -----
# Group locations
regions = sf::st_read(here::here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions_csv = readr::read_csv2(
  here::here("data", "geo", "APonchon", "GLT", "RegionsName.csv"),
  locale = readr::locale(encoding = "ISO-8859-1"),
  na = ""
)

#### Successful dispersal events -----
disp = xlsx::read.xlsx(here::here("outputs", "data", "dispersal", "Successful_dispersal.xlsx"),
                       sheetName = "Sheet1")

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

### Long-term dataset -----
#### Parameters ----------
# Problematic individuals
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490") # Last list of problematic individuals (see Aurore Zenodo)

#### Filter the long-term dataset -----
##### Filter out bad data -----
data.clean.filter = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    Disp!="1", # Dispersers
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

##### Immigrations -----
# From the dispersal dataset where one row of disp = one dispersal event
# An immigrant = an event where the origin and destination Group differ.
disp_prep = disp %>%
  dplyr::filter(!is.na(GLT), !GLT %in% bad) %>%
  dplyr::mutate(
    DateObs = as.Date(DateObs),
    Month = lubridate::month(DateObs),
    Year_disp = lubridate::year(DateObs),
    # Same UMMP reassignment as in data.clean.final
    ToUMMP = dplyr::case_when(
      ToRegion == "Sitio 4 Irmaos" ~ "4_Irmaos",
      ToRegion == "Sitio Recanto Preservar" ~ "Recanto_Preservar",
      ToRegion == "Faz. Sao Joao" ~ "Sao_Joao",
      TRUE ~ as.character(ToUMMP)
    ),
    FromUMMP = dplyr::case_when(
      FromRegion == "Sitio 4 Irmaos" ~ "4_Irmaos",
      FromRegion == "Sitio Recanto Preservar" ~ "Recanto_Preservar",
      FromRegion == "Faz. Sao Joao" ~ "Sao_Joao",
      TRUE ~ as.character(FromUMMP)
    ),
    # Same annual cycle as for the monitoring data
    Cycle = if (criteria == "dry-wet") {
      as.integer(ifelse(Month <= 3, Year_disp - 1L, Year_disp))
    } else {
      as.integer(Year_disp)
    }
  )

# Number of arrivals per destination group and annual cycle
immigrants = disp_prep %>%
  dplyr::group_by(UMMPs = ToUMMP, 
                  Group = as.character(ToGroup), 
                  Cycle) %>%
  dplyr::summarise(
    n_immigrants = dplyr::n_distinct(disp_id), # Number of dispersal EVENTS (including prospection)
    .groups = "drop"
  )

# Join to demography
demography = demography %>%
  dplyr::left_join(
    immigrants,
    by = c("UMMPs", "Group", "Cycle")
  ) %>%
  dplyr::mutate(
    n_immigrants = dplyr::coalesce(n_immigrants, 0L)
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
      c(GS, sex_ratio, n_newborns, n_immigrants, lambda, jaccard),
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
# From Andressa Sales Coelho thesis

#### Join patch data -----
mat_system = mat_system %>%
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


### Patch data -----

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