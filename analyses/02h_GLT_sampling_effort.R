#------------------------------------------------#
# Author: Romain Monassier
# Objective: Sampling effort of long-term GLT data
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(xlsx)
library(terra)
library(sf)
library(lubridate)
library(patchwork)

### Import datasets -----

# GLT data
load(here::here("data", "glt", "APonchon", "data_clean_long_final.RData"))

# Group locations
regions = sf::st_read(here::here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions_csv = readr::read_csv2(
  here::here("data", "geo", "APonchon", "GLT", "RegionsName.csv"),
  locale = readr::locale(encoding = "ISO-8859-1"),
  na = ""
)

# Patches
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
# Patches in 2024
patches_sf = patches[[36]]

### Sampling effort -----
# Problematic individuals
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490") # Last list of problematic individuals (see Aurore Zenodo)

#### Number of observations -----
# 1. Distinct individuals
effort = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad
  ) %>%
  dplyr::summarise(
    n_individuals = dplyr::n_distinct(GLT)
  )

cat(
  "\nSampling effort: A total of",
  effort$n_individuals,
  "distinct individuals were recorded.\n"
)


# 2. Sex ratio
effort = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(SexOK)
  ) %>%
  dplyr::distinct(GLT, SexOK) %>%
  dplyr::count(SexOK, name = "n_individuals")

n_males = sum(
  effort$n_individuals[effort$SexOK == "M"],
  na.rm = TRUE
)

n_females = sum(
  effort$n_individuals[effort$SexOK == "F"],
  na.rm = TRUE
)

cat(
  "\nSex ratio: ", n_males, " males and ",
  n_females, " females were recorded"
)

# 3. Stage
effort = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(IdadeOK)
  ) %>%
  dplyr::distinct(GLT, IdadeOK) %>%
  dplyr::count(IdadeOK, name = "n_individuals") %>%
  dplyr::mutate(
    proportion = n_individuals / sum(n_individuals),
    percentage = round(100 * proportion, 1)
  )

cat("\nStage composition (excluding NAs):\n")

purrr::pwalk(
  effort,
  function(IdadeOK, n_individuals, proportion, percentage) {
    cat(
      "- Stage", IdadeOK, ":",
      n_individuals, "individuals (",
      percentage, "%).\n"
    )
  }
)

# 4. Distinct individuals per year
effort = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad
  ) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    n_individuals = dplyr::n_distinct(GLT),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Year)

cat("\nSampling effort: distinct individuals per year\n")

purrr::pwalk(
  effort,
  function(Year, n_individuals) {
    cat(
      "In", Year, ",",
      n_individuals,
      "distinct individuals were recorded.\n"
    )
  }
)

# 5. Total distinct groups
effort = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(Group)
  ) %>%
  dplyr::summarise(
    n_groups = dplyr::n_distinct(Group)
  )

cat(
  "\nSampling effort: A total of",
  effort$n_groups,
  "distinct groups were recorded.\n"
)

# 6. Distinct groups per year
effort = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(Group)
    ) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    n_groups = dplyr::n_distinct(Group),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Year)

cat("\nSampling effort: distinct groups per year\n")

purrr::pwalk(
  effort,
  function(Year, n_groups) {
    cat(
      "In", Year, ",",
      n_groups,
      "distinct groups were recorded.\n"
    )
  }
)

# 7. Distinct groups per UMMP
effort = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(Group),
    !is.na(UMMPs)
  ) %>%
  dplyr::mutate(UMMPs = as.character(UMMPs)) %>%
  dplyr::group_by(UMMPs) %>%
  dplyr::summarise(
    n_groups = dplyr::n_distinct(Group),
    .groups = "drop"
  ) %>%
  dplyr::arrange(UMMPs)

purrr::pwalk(
  effort,
  function(UMMPs, n_groups) {
    cat(
      "In", UMMPs, ",",
      n_groups,
      "distinct groups were recorded.\n"
    )
  }
)

#### Sampling effort in space/time -----
### Chronological order
# Count observations by month and season
data.clean.final %>%
  dplyr::filter(!is.na(DateObs), !is.na(season)) %>%
  dplyr::mutate(
    Month = lubridate::month(DateObs)) %>%
  dplyr::count(Month, season, name = "n_observations") %>%
  dplyr::arrange(Month, season)

# Reassign season
data.clean.final = data.clean.final %>%
  dplyr::mutate(
    DateObs = as.Date(DateObs),
    Month = lubridate::month(DateObs),
    season = dplyr::case_when(
      Month %in% 1:4   ~ "Wet 1",
      Month %in% 5:9   ~ "Dry",
      Month %in% 10:12 ~ "Wet 2",
      TRUE ~ NA_character_
    ),
    season = factor(
      season,
      levels = c("Wet 1", "Dry", "Wet 2")
    )
  )

# Define the chronological order of seasons
effort_plot = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT), # Remove NA GLTs
    !GLT %in% bad, # Remove problematic individuals
    !is.na(Group), # Remove NA groups
    !is.na(Year), # Remove NA years
    !is.na(UMMPs), # Remove NA UMMPs
    !is.na(DateObs), # Remove NA observation dates
  ) %>%
  dplyr::mutate(
    DateObs = as.Date(DateObs),
    Group = as.character(Group),
    UMMPs = as.character(UMMPs),
    season = factor(
      season,
      levels = c("Wet 1", "Dry", "Wet 2")
    )
  ) %>%
  # Count distinct monitoring dates per group, season, year, and UMMP
  # n_visits represents the number of distinct visits (dates)
  dplyr::group_by(UMMPs, Group, Year, season) %>%
  dplyr::summarise(
    n_visits = dplyr::n_distinct(DateObs),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    # 1/2/3 depending on the season
    season_index = match(
      as.character(season),
      c("Wet 1", "Dry", "Wet 2")
    ),
    # Chronological time index
    # For instance, if the observation is in Wet 1-1989
    # 3*(1989-1989)+1 -> 1 (beginning of the sampling)
    time_index = 3 * (Year - min(Year)) + season_index
  ) %>%
  dplyr::arrange(UMMPs, Group, time_index)


### Order groups by UMMP
group_levels = effort_plot %>%
  dplyr::distinct(UMMPs, Group) %>%
  dplyr::arrange(UMMPs, Group) %>%
  # Mutate a group label that unites Group with UMMP
  dplyr::mutate(
    group_label = paste(UMMPs, Group, sep = " - ")
  ) %>%
  dplyr::pull(group_label)
effort_plot = effort_plot %>%
  dplyr::mutate(
    group_label = paste(UMMPs, Group, sep = " - "),
    group_label = factor(
      group_label,
      levels = rev(unique(group_levels))
    )
  )

### Build separate season and year axis labels
year_season_labels = effort_plot %>%
  dplyr::distinct(time_index, Year, season) %>%
  dplyr::arrange(time_index) %>%
  dplyr::mutate(
    season_label = as.character(season),
    # Put the label on "Dry"
    year_label = dplyr::if_else(
      season_label == "Dry",
      as.character(Year),
      ""
    )
  )

### Plot sampling effort
ggplot2::ggplot(
  effort_plot,
  ggplot2::aes(
    x = time_index,
    y = group_label,
    colour = UMMPs,
    group = interaction(UMMPs, Group)
  )
) +
  ggplot2::geom_line(
    linewidth = 0.5,
    alpha = 0.65
  ) +
  ggplot2::geom_point(
    ggplot2::aes(size = n_visits),
    alpha = 0.85
  ) +
  ggplot2::scale_size_continuous(
    name = "Number of observations",
    range = c(1.5, 7),
    breaks = scales::pretty_breaks(n = 4)
  ) +
  ggplot2::scale_x_continuous(
    breaks = year_season_labels$time_index,
    labels = year_season_labels$season_label,
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_y_discrete(
    name = "Group (UMMP - Group)"
  ) +
  ggplot2::labs(
    title = "Sampling effort by group over time",
    subtitle = paste(
      "Wet 1 (Jan-Apr), Dry (May-Sep), Wet 2 (Oct-Dec);",
      "dot size represents the number of observations"
    ),
    x = NULL,
    colour = "UMMP"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 8
    ),
    axis.text.y = ggplot2::element_text(size = 8),
    axis.title.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      colour = "grey90",
      linewidth = 0.3
    ),
    legend.position = "right",
    plot.title = ggplot2::element_text(face = "bold")
  ) +
  ggplot2::geom_vline(
    xintercept = 3 * (
      sort(unique(effort_plot$Year)) -
        min(effort_plot$Year)
    ) + 0.5,
    colour = "grey80",
    linewidth = 0.4
  ) +
  ggplot2::geom_text(
    data = effort_plot %>%
      dplyr::distinct(Year) %>%
      dplyr::mutate(
        year_center = 3 * (Year - min(effort_plot$Year)) + 2
      ),
    ggplot2::aes(
      x = year_center,
      y = -0.8,
      label = Year
    ),
    inherit.aes = FALSE,
    size = 3.5,
    vjust = 1
  ) +
  ggplot2::coord_cartesian(
    clip = "off"
  ) +
  ggplot2::theme(
    plot.margin = ggplot2::margin(
      t = 10, r = 10, b = 30, l = 10
    )
  )

##### Export the plot ----
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_sampling_effort.png"),
    width = 6000, height = 6500, res = 300, type="cairo")
# THE PLOT
ggplot2::ggplot(
  effort_plot,
  ggplot2::aes(
    x = time_index,
    y = group_label,
    colour = UMMPs,
    group = interaction(UMMPs, Group)
  )
) +
  ggplot2::geom_line(
    linewidth = 0.5,
    alpha = 0.65
  ) +
  ggplot2::geom_point(
    ggplot2::aes(size = n_visits),
    alpha = 0.85
  ) +
  ggplot2::scale_size_continuous(
    name = "Number of observations",
    range = c(1.5, 7),
    breaks = scales::pretty_breaks(n = 4)
  ) +
  ggplot2::scale_x_continuous(
    breaks = year_season_labels$time_index,
    labels = year_season_labels$season_label,
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_y_discrete(
    name = "Group (UMMP - Group)"
  ) +
  ggplot2::labs(
    title = "Sampling effort by group over time",
    subtitle = paste(
      "Wet 1 (Jan-Apr), Dry (May-Sep), Wet 2 (Oct-Dec);",
      "dot size represents the number of observations"
    ),
    x = NULL,
    colour = "UMMP"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 8
    ),
    axis.text.y = ggplot2::element_text(size = 8),
    axis.title.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      colour = "grey90",
      linewidth = 0.3
    ),
    legend.position = "right",
    plot.title = ggplot2::element_text(face = "bold")
  ) +
  ggplot2::geom_vline(
    xintercept = 3 * (
      sort(unique(effort_plot$Year)) -
        min(effort_plot$Year)
    ) + 0.5,
    colour = "grey80",
    linewidth = 0.4
  ) +
  ggplot2::geom_text(
    data = effort_plot %>%
      dplyr::distinct(Year) %>%
      dplyr::mutate(
        year_center = 3 * (Year - min(effort_plot$Year)) + 2
      ),
    ggplot2::aes(
      x = year_center,
      y = -0.8,
      label = Year
    ),
    inherit.aes = FALSE,
    size = 3.5,
    vjust = 1
  ) +
  ggplot2::coord_cartesian(
    clip = "off"
  ) +
  ggplot2::theme(
    plot.margin = ggplot2::margin(
      t = 10, r = 10, b = 30, l = 10
    )
  )
dev.off()

#### Continuous sampling -----
# Identify groups monitored continuously and UMMPs meeting the threshold

# Choose the criterion:
# "annual_1" = at least 1 distinct observation date per year
# "annual_2" = at least 2 distinct observation dates per year
# "dry_dry"  = at least 1 observation date in the Dry season per year

criteria = "dry_dry"

# Minimum number of consecutive qualifying years
threshold = 3

# Years excluded because of Yellow Fever
excluded_years = 2017:2019

stopifnot(criteria %in% c("annual_1", "annual_2", "dry_dry"))
stopifnot(threshold >= 1)


##### 0. CROSSWALK BETWEEN UMMP AND PATCH NAMES #####

UMMP_crosswalk = data.frame(
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
    "Faz. Boa Esperanza"
  ),
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
    "Boa_Esperanca"
  ),
  stringsAsFactors = FALSE
)

# Check which observation names do not match the crosswalk
unmatched_UMMPs = data.clean.final %>%
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::distinct(UMMPs) %>%
  dplyr::mutate(UMMPs = as.character(UMMPs)) %>%
  dplyr::anti_join(UMMP_crosswalk, by = "UMMPs")

cat("\nUMMP names not matched by the crosswalk:\n")
print(unmatched_UMMPs)

# Check which patch IDs have no corresponding UMMP name in the crosswalk
unmatched_patches = patches_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(patch_id) %>%
  dplyr::anti_join(
    UMMP_crosswalk %>% dplyr::distinct(patch_id),
    by = "patch_id"
  )

cat("\nPatch IDs without a corresponding UMMP in the crosswalk:\n")
print(unmatched_patches)

# Correct UMMP
data.clean.final = data.clean.final %>%
  dplyr::mutate(
    UMMPs = dplyr::case_when(
      Region == "Sitio 4 Irmaos" ~ "4_Irmaos",
      Region == "Sitio Recanto Preservar" ~ "Recanto_Preservar",
      Region == "Faz. Sao Joao" ~ "Sao_Joao",
      TRUE ~ as.character(UMMPs)
    )
  )


##### 1. PREPARE MONITORING DATA #####

monitoring = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(Group),
    !is.na(UMMPs),
    !is.na(Year),
    !is.na(DateObs),
    !Year %in% excluded_years
  ) %>%
  dplyr::mutate(
    Group = as.character(Group),
    UMMPs = as.character(UMMPs),
    Year = as.integer(as.character(Year)),
    DateObs = as.Date(DateObs),
    season = as.character(season)
  ) %>%
  dplyr::distinct(UMMPs, Group, Year, DateObs, season) %>%
  dplyr::left_join(
    UMMP_crosswalk,
    by = "UMMPs"
  )

# Apply the Dry-season restriction when required
if (criteria == "dry_dry") {
  monitoring = monitoring %>%
    dplyr::filter(season == "Dry")
}

cat("\nCriterion:", criteria,
    "\nThreshold:", threshold, "consecutive years",
    "\nExcluded years:", paste(excluded_years, collapse = ", "),
    "\nNumber of monitoring records:", nrow(monitoring), "\n")

cat("\nMonitoring data preview:\n")
head(monitoring, n=10)


##### 2. IDENTIFY QUALIFYING YEARS #####

# Count distinct observation dates per group, UMMP and year
annual_counts = monitoring %>%
  dplyr::group_by(UMMPs, Group, Year) %>%
  dplyr::summarise(
    n_observations = dplyr::n_distinct(DateObs),
    .groups = "drop"
  )

# Apply the observation-count criterion
if (criteria == "annual_1") {
  years = annual_counts %>%
    dplyr::filter(n_observations >= 1)
}

if (criteria == "annual_2") {
  years = annual_counts %>%
    dplyr::filter(n_observations >= 2)
}

if (criteria == "dry_dry") {
  years = annual_counts %>%
    dplyr::filter(n_observations >= 1)
}

years = years %>%
  dplyr::distinct(UMMPs, Group, Year) %>%
  dplyr::arrange(UMMPs, Group, Year)


##### 3. IDENTIFY CONSECUTIVE MONITORING PERIODS #####

year_runs = years %>%
  dplyr::group_by(UMMPs, Group) %>%
  dplyr::arrange(Year, .by_group = TRUE) %>%
  # Missing years create new runs
  dplyr::mutate(
    run_id = Year - dplyr::row_number()
  ) %>%
  dplyr::group_by(UMMPs, Group, run_id) %>%
  dplyr::summarise(
    n_consecutive_years = dplyr::n(),
    first_year = min(Year),
    last_year = max(Year),
    .groups = "drop"
  )

cat("\nAll consecutive monitoring runs:\n")
print(year_runs %>% 
        dplyr::filter(n_consecutive_years>1) %>% 
        dplyr::arrange(dplyr::desc(n_consecutive_years)), 
      n = Inf)

# Longest run per group
longest = year_runs %>%
  dplyr::group_by(UMMPs, Group) %>%
  dplyr::summarise(
    max_consecutive_years = max(n_consecutive_years),
    .groups = "drop"
  )

cat("\nLongest consecutive monitoring period per group:\n")
print(longest, n = Inf)


##### 4. CUMULATIVE NUMBER OF DISTINCT GROUPS BY DURATION #####

# Count each group once, using its longest consecutive run
summary_monitoring = longest %>%
  dplyr::count(
    max_consecutive_years,
    name = "n_groups_exact"
  ) %>%
  dplyr::arrange(max_consecutive_years)

# Include durations with zero groups, if any
max_duration = max(summary_monitoring$max_consecutive_years)

summary_monitoring = summary_monitoring %>%
  tidyr::complete(
    max_consecutive_years = seq_len(max_duration),
    fill = list(n_groups_exact = 0L)
  ) %>%
  dplyr::arrange(max_consecutive_years) %>%
  dplyr::mutate(
    # Groups with at least this many consecutive years
    n_groups_at_least = rev(cumsum(rev(n_groups_exact)))
  )

cat("\nGroups by consecutive monitoring duration:\n")
print(summary_monitoring, n = Inf)


##### 5. RETAIN RUNS MEETING THE SELECTED THRESHOLD #####

qualifying_runs = year_runs %>%
  dplyr::filter(n_consecutive_years >= threshold)

cat("\nRuns meeting the threshold:\n")
print(qualifying_runs, n = Inf)

# Each group is counted only once per UMMP
qualifying_groups = qualifying_runs %>%
  dplyr::distinct(UMMPs, Group)

cat("\nDistinct qualifying UMMP-group combinations:\n")
print(qualifying_groups, n = Inf)


##### 6. COUNT QUALIFYING GROUPS PER PATCH #####

# All patch IDs, including patches with no observations
all_patches = patches_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(patch_id)

# Count groups by matching observation UMMPs to patch IDs
groups_per_patch = qualifying_groups %>%
  dplyr::left_join(
    UMMP_crosswalk,
    by = "UMMPs"
  ) %>%
  dplyr::filter(!is.na(patch_id)) %>%
  dplyr::count(
    patch_id,
    name = "n_groups"
  )

# Keep every patch, assigning zero to those without qualifying groups
groups_per_patch = all_patches %>%
  dplyr::left_join(
    groups_per_patch,
    by = "patch_id"
  ) %>%
  dplyr::mutate(
    n_groups = dplyr::coalesce(n_groups, 0L),
    included = n_groups > 0,
    status = ifelse(included, "Included", "Not included")
  ) %>%
  dplyr::arrange(dplyr::desc(n_groups), patch_id)

cat("\nNumber of qualifying groups per patch:\n")
print(groups_per_patch)

cat("\nNumber of patches with at least one qualifying group:",
    sum(groups_per_patch$included), "\n")

cat("Number of patches with zero qualifying groups:",
    sum(!groups_per_patch$included), "\n")


##### 7. PLOT 1: CUMULATIVE GROUPS BY CONSECUTIVE DURATION #####

plot_duration = ggplot2::ggplot(
  summary_monitoring,
  ggplot2::aes(
    x = max_consecutive_years,
    y = n_groups_at_least
  )
) +
  ggplot2::geom_col() +
  ggplot2::geom_text(
    ggplot2::aes(label = n_groups_at_least),
    vjust = -0.4,
    size = 3.5
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_len(max_duration)
  ) +
  ggplot2::labs(
    title = "Continuity of group monitoring",
    subtitle = paste0(
      "Criterion: ", criteria,
      " | Excluding 2017–2019"
    ),
    x = "Minimum number of consecutive years",
    y = "Cumulative number of distinct groups"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(plot_duration)


##### 8. PLOT 2: NUMBER OF QUALIFYING GROUPS PER UMMP #####

plot_groups_UMMP = ggplot2::ggplot(
  groups_per_patch,
  ggplot2::aes(
    x = stats::reorder(patch_id, n_groups),
    y = n_groups
  )
) +
  ggplot2::geom_col() +
  ggplot2::geom_text(
    ggplot2::aes(label = n_groups),
    hjust = -0.2,
    size = 3.5
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = ggplot2::expansion(mult = c(0, 0.15))
  ) +
  ggplot2::labs(
    title = "Qualifying groups per UMMP",
    subtitle = paste0(
      "Criterion: ", criteria,
      " | At least ", threshold,
      " consecutive years"
    ),
    x = "UMMP patch",
    y = "Number of qualifying groups"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(plot_groups_UMMP)


##### 9. MAP: INCLUDED AND NON-INCLUDED PATCHES #####

map_data = patches_sf %>%
  dplyr::left_join(
    groups_per_patch %>%
      dplyr::select(patch_id, n_groups, status),
    by = "patch_id"
  ) %>%
  dplyr::mutate(
    n_groups = dplyr::coalesce(n_groups, 0L),
    status = dplyr::coalesce(status, "Not included"),
    UMMP = patch_id
  )

# Create label points guaranteed to fall within each polygon
map_labels = sf::st_point_on_surface(map_data)

map_UMMPs = ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(
    ggplot2::aes(
      fill = status,
      colour = UMMP
    ),
    linewidth = 0.2
  ) +
  ggplot2::geom_sf_text(
    data = map_labels,
    ggplot2::aes(label = UMMP),
    colour = "black",
    size = 2,
    fontface = "bold"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Included" = "lightgreen",
      "Not included" = "white"
    ),
    breaks = c("Included", "Not included"),
    drop = FALSE
  ) +
  ggplot2::scale_colour_viridis_d(
    name = "UMMP",
    option = "turbo"
  ) +
  ggplot2::labs(
    title = "UMMPs meeting the monitoring threshold",
    subtitle = paste0(
      "Criterion: ", criteria,
      " | At least ", threshold,
      " consecutive years | Included patches shown in green"
    )
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none"
  )

print(map_UMMPs)


##### 10. PROGRESSIVE INCLUSION OF UMMPs #####

# Longest run achieved by any group within each UMMP
longest_UMMP = longest %>%
  dplyr::group_by(UMMPs) %>%
  dplyr::summarise(
    max_consecutive_years = max(max_consecutive_years),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    dplyr::desc(max_consecutive_years),
    UMMPs
  )

progressive_UMMP = longest_UMMP %>%
  dplyr::group_by(max_consecutive_years) %>%
  dplyr::summarise(
    n_UMMPs_added = dplyr::n_distinct(UMMPs),
    UMMPs_added = paste(
      sort(unique(UMMPs)),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(max_consecutive_years))

cat("\nProgressive inclusion of UMMPs by monitoring duration:\n")
print(progressive_UMMP, n = Inf)


##### 11. RETAIN TEMPORAL OBSERVATIONS #####

# Retain observations falling within each qualifying run.
# For annual criteria, all observation dates in qualifying years are shown.
# For dry_dry, only Dry-season observation dates are shown.

retained_data = qualifying_runs %>%
  dplyr::inner_join(
    monitoring,
    by = c("UMMPs", "Group")
  ) %>%
  dplyr::filter(
    Year >= first_year,
    Year <= last_year
  ) %>%
  dplyr::distinct(
    UMMPs, Group, run_id, Year, DateObs
  ) %>%
  dplyr::mutate(
    group_label = paste(UMMPs, Group, sep = " — "),
    run_label = paste(group_label, run_id, sep = " | ")
  ) %>%
  dplyr::arrange(UMMPs, Group, DateObs)

cat("\nNumber of retained observation dates:",
    nrow(retained_data), "\n")


##### 12. TEMPORAL PLOT: GROUPS AND OBSERVATION DATES #####

# Order groups by UMMP and group identifier
group_order = retained_data %>%
  dplyr::distinct(UMMPs, Group, group_label) %>%
  dplyr::arrange(UMMPs, Group) %>%
  dplyr::pull(group_label)

retained_data = retained_data %>%
  dplyr::mutate(
    group_label = factor(
      group_label,
      levels = group_order
    )
  )

temporal_plot = ggplot2::ggplot(
  retained_data,
  ggplot2::aes(
    x = DateObs,
    y = group_label,
    group = run_label
  )
) +
  ggplot2::geom_line(
    colour = "grey55",
    linewidth = 0.5
  ) +
  ggplot2::geom_point(
    size = 1
  ) +
  ggplot2::labs(
    title = "Temporal distribution of retained monitoring",
    subtitle = paste0(
      "Criterion: ", criteria,
      " | At least ", threshold,
      " consecutive years"
    ),
    x = "Observation date",
    y = "UMMP — Group"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 7)
  )

print(temporal_plot)

##### 13. SYNTHETIC FIGURE ------

### Prepare the existing plots for the combined figure
# Cumulative groups
plot_duration_combined = plot_duration +
  ggplot2::labs(
    title = "A. Continuity of group monitoring",
    subtitle = paste0("Criterion: ", criteria),
    x = "Minimum consecutive years",
    y = "Distinct groups meeting duration"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 11),
    plot.subtitle = ggplot2::element_text(size = 9)
  )

# Qualifying groups per UMMP
plot_groups_UMMP_combined = plot_groups_UMMP +
  ggplot2::labs(
    title = "B. Qualifying groups per UMMP",
    subtitle = paste0("Threshold: ", threshold, " consecutive years"),
    x = "UMMP",
    y = "Number of groups"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 11),
    plot.subtitle = ggplot2::element_text(size = 9)
  )

# Map
map_UMMPs_combined = map_UMMPs +
  ggplot2::labs(
    title = "C. Spatial distribution of qualifying UMMPs",
    subtitle = paste0(
      "Threshold: ", threshold, " consecutive years"
    )
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 11),
    plot.subtitle = ggplot2::element_text(size = 9),
    legend.position = "none"
  )

# Temporal distribution
temporal_plot_combined = temporal_plot +
  ggplot2::labs(
    title = "D. Temporal distribution of monitoring",
    subtitle = "Observation dates within qualifying runs",
    x = "Observation date",
    y = "UMMP — Group"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 11),
    plot.subtitle = ggplot2::element_text(size = 9),
    axis.text.y = ggplot2::element_text(size = 6)
  )


### Assemble the figure
synthetic_figure = (
  plot_duration_combined +
    plot_groups_UMMP_combined +
    map_UMMPs_combined +
    temporal_plot_combined
) +
  patchwork::plot_layout(
    design = "
    AB
    CD
    ",
    heights = c(1, 1.7),
    widths = c(1, 1),
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    title = "Continuous monitoring of groups across UMMPs",
    subtitle = paste0(
      criterion_text,
      " | Threshold: ", threshold, " consecutive years",
      " | Excluded years: ", paste(excluded_years, collapse = ", ")
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )
  )

print(synthetic_figure)


### Save the figure
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_sampling_effort_dry_dry.png"),
    width = 3500, height = 2600, res = 300, type="cairo")
# THE PLOT
synthetic_figure
dev.off()

