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
  dplyr::filter(!is.na(DateObs), 
                !is.na(season)) %>%
  dplyr::mutate(
    Month = lubridate::month(DateObs)) %>%
  dplyr::count(Month, season, name = "n_observations") %>%
  dplyr::arrange(Month, season)

##### Reassign season -----
data.clean.final = data.clean.final %>%
  dplyr::mutate(
    DateObs = as.Date(DateObs),
    Month = lubridate::month(DateObs),
    season = dplyr::case_when(
      Month %in% 1:3 ~ "Wet 1",
      Month %in% 4:8 ~ "Dry",
      Month %in% 9:12 ~ "Wet 2",
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

# Create group labels
effort_plot = effort_plot %>%
  dplyr::mutate(
    group_label = paste(UMMPs, Group, sep = " - ")
  )

# Define the order of group labels
group_levels = effort_plot %>%
  dplyr::distinct(UMMPs, Group, group_label) %>%
  dplyr::arrange(UMMPs, Group) %>%
  dplyr::pull(group_label)

# Convert group labels into an ordered factor
effort_plot = effort_plot %>%
  dplyr::mutate(
    group_label = factor(
      group_label,
      levels = rev(group_levels)
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

##### Export the sampling effort plot ----
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_sampling_effort.png"),
    width = 6000, height = 6500, res = 300, type="cairo")
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
      "Wet 1 (Jan-March), Dry (April-August), Wet 2 (September-Dec);",
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


### Natural group dynamic -----
# Dietz et al. (1994): most litters from september to march (with a few at the end of august)
# Successful immigrations (this data) suggest immigration occurs all year, but with peaks during the mating season (from july to december)
# We plot the dynamic of groups over the course of the year to see what is the best window to analyze group dynamics
group_data = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(Group),
    !is.na(UMMPs),
    !is.na(season),
    !is.na(DateObs),
    !Year %in% c(2017, 2018, 2019) # Remove YF outbreak
  ) %>%
  dplyr::mutate(
    DateObs = as.Date(DateObs),
    Year = lubridate::year(DateObs),
    Month = lubridate::month(DateObs)
    )

# Number of seasons represented in the dataset
n_seasons = dplyr::n_distinct(group_data$season)
n_months = dplyr::n_distinct(group_data$Month)

#### Monthly GS ------
### OPTION 1. Retain groups recorded in every season
groups_seasons = group_data %>%
  dplyr::distinct(
    Year, UMMPs, Region, Group, season, DateObs
  ) %>%
  dplyr::group_by(
    Year, UMMPs, Region, Group
  ) %>%
  dplyr::summarise(
    n_dates = dplyr::n_distinct(DateObs),
    n_seasons = dplyr::n_distinct(season),
    .groups = "drop"
  ) %>%
  # At least one observation in each of the three seasons
  dplyr::filter(n_seasons == 3)

### Keep observations belonging to those groups
data.groups = group_data %>%
  dplyr::inner_join(
    groups_seasons %>%
      dplyr::select(Year, UMMPs, Region, Group),
    by = c("Year", "UMMPs", "Region", "Group")
  )

### Monthly group size
group_size_monthly = data.groups %>%
  dplyr::group_by(Region, Group, Year, Month) %>%
  dplyr::summarise(
    Group_size = dplyr::n_distinct(GLT),
    .groups = "drop"
  ) %>% 
  # Remove floaters
  dplyr::filter(Group_size > 2)

# Mean group size (and variability)
group_size_summary = group_size_monthly %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(
    Mean_size = mean(Group_size),
    SD = stats::sd(Group_size),
    n = dplyr::n(),
    SE = SD / sqrt(n),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    Month = 1:12
  ) %>%
  dplyr::mutate(
    Month_label = factor(
      Month,
      levels = 1:12,
      labels = month.abb # Month abbreviation
    )
  )

# Add the season for coloring
group_size_summary = group_size_summary %>%
  dplyr::left_join(data.clean.final %>% dplyr::distinct(Month, season), by = "Month")

group_size_summary = group_size_summary %>% 
  dplyr::mutate(season = dplyr::case_when(
    season == "Wet 1" ~ "Wet",
    season == "Wet 2" ~ "Wet",
    TRUE ~ season
  ))

# PLOT
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_monthly_GS_1obsSeason.png"),
    width = 2300, height = 1600, res = 300, type="cairo")
ggplot2::ggplot(
  group_size_summary,
  ggplot2::aes(x = Month, y = Mean_size)
) +
  ggplot2::geom_rect(
    data = group_size_summary %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(
        xmin = min(Month) - 0.5,
        xmax = max(Month) + 0.5,
        .groups = "drop"
      ),
    ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = season
    ),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = Mean_size - SE,
      ymax = Mean_size + SE
    ),
    fill = "grey65",
    alpha = 0.5
  ) +
  ggplot2::geom_line(
    colour = "grey25",
    linewidth = 1
  ) +
  ggplot2::geom_point(
    ggplot2::aes(colour = season),
    size = 2.5
  ) +
  ggplot2::scale_fill_manual(
    values = c("Wet" = "#56B4E9", "Dry" = "#ff3f14")
  ) +
  ggplot2::scale_colour_manual(
    values = c("Wet" = "#0072B2", "Dry" = "#D55E00")
  ) +
  ggplot2::scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    limits = c(0.5, 12.5)
  ) +
  ggplot2::labs(
    title = "Dynamics of group size",
    subtitle = "Groups with at least 1 observation per season in a year",
    x = "Month",
    y = "Mean group size (distinct GLTs)",
    fill = "Season",
    colour = "Season",
    caption = "Points represent monthly means across group-year observations; shaded area represents ±1 SE."
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle = ggplot2::element_text(size = 10),
    plot.caption = ggplot2::element_text(size = 8),
    legend.position = "bottom"
  )
dev.off()

#### Raw GS -----
### OPTION 2. At least N observations per season/year across consecutive years
# Filter using the number of seasons (and of observations)
groups_seasons = group_data %>%
  dplyr::distinct(
    Year, UMMPs, Region, Group, season, DateObs
  ) %>%
  dplyr::group_by(
    Year, UMMPs, Region, Group
  ) %>%
  dplyr::summarise(
    n_dates = dplyr::n_distinct(DateObs),
    n_seasons = dplyr::n_distinct(season),
    .groups = "drop"
  ) %>%
  # At least one observation in each of the three seasons
  dplyr::filter(n_seasons == 3)

# Groups monitored consecutive years
groups_consecutive_years = groups_seasons %>%
  dplyr::arrange(UMMPs, Region, Group, Year) %>%
  dplyr::group_by(UMMPs, Region, Group) %>%
  dplyr::mutate(
    consecutive_run = cumsum(
      dplyr::row_number() == 1 |
        Year != dplyr::lag(Year) + 1
    )
  ) %>%
  dplyr::group_by(UMMPs, Region, Group, consecutive_run) %>%
  dplyr::mutate(
    n_consecutive_years = dplyr::n(),
    run_label = paste0(min(Year), "-", max(Year))
  ) %>%
  dplyr::filter(n_consecutive_years >= 3) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    Year, UMMPs, Region, Group,
    consecutive_run, run_label,
    n_dates, n_seasons, n_consecutive_years
  )

# Number of groups
groups_consecutive_years %>%
  dplyr::distinct(UMMPs, Region, Group) %>%
  nrow()

### Keep observations belonging to those groups
data.groups = group_data %>%
  dplyr::inner_join(
    groups_consecutive_years %>%
      dplyr::select(Year, UMMPs, Region, Group, consecutive_run, run_label),
    by = c("Year", "UMMPs", "Region", "Group")
  )

## Raw GS at specific dates
group_size = data.groups %>%
  dplyr::mutate(
    season_2 = dplyr::case_when(
      grepl("wet", season, ignore.case = TRUE) ~ "wet",
      grepl("dry", season, ignore.case = TRUE) ~ "dry",
      TRUE ~ NA_character_
    ),
    Month = lubridate::floor_date(as.Date(DateObs), unit = "month"),
    # 1 panneau = 1 groupe x 1 séquence d'années consécutives
    panel_id = paste0(UMMPs, " | ", Region, " | ", Group, " (", run_label, ")")
  ) %>%
  # Taille de groupe à chaque date d'observation
  dplyr::group_by(
    panel_id, UMMPs, Region, Group, consecutive_run, Month, DateObs, season_2
  ) %>%
  dplyr::summarise(
    GroupSize = dplyr::n_distinct(GLT),
    .groups = "drop"
  ) %>%
  # Moyenne mensuelle
  dplyr::group_by(
    panel_id, UMMPs, Region, Group, consecutive_run, Month
  ) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    season_2 = names(which.max(table(season_2))),
    GroupSize = mean(GroupSize),
    .groups = "drop"
  ) %>%
  # Ne garder que les séquences dont la taille de groupe varie
  dplyr::group_by(panel_id) %>%
  dplyr::filter(dplyr::n_distinct(GroupSize) > 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(panel_id, Month)

# Nombre de panneaux conservés
dplyr::n_distinct(group_size$panel_id)

# Plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_raw_GS_perGroup.png"),
    width = 6000, height = 3500, res = 300, type="cairo")
ggplot2::ggplot(
  group_size,
  ggplot2::aes(x = Month, y = GroupSize)
) +
  ggplot2::geom_smooth(
    ggplot2::aes(group = 1),
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    colour = "grey30",
    linewidth = 0.7
  ) +
  ggplot2::geom_point(
    ggplot2::aes(colour = season_2),
    size = 2
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(panel_id),
    scales = "free_x"
  ) +
  ggplot2::scale_colour_manual(
    values = c("wet" = "#1f78b4", "dry" = "#e08214"),
    name = "Season"
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    minor_breaks = NULL,
    expand = ggplot2::expansion(add = 15)
  ) +
  ggplot2::labs(
    x = "Month",
    y = "Mean group size (number of distinct GLT)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
    strip.text = ggplot2::element_text(size = 6),
    legend.position = "bottom"
  )
dev.off()

### Continuous sampling -----
# Identify what is the sampling effort depending on:
# 1) A definition of what is "continuous sampling" (e.g., 1 observation each year)
# 2) A threshold (e.g., at least 3 continuous observations)
# This is important for the analysis of group dynamics (growth rate, recruitment, etc.)

#### Parameters -----
# Choose the criterion:
# "annual_1" = at least 1 distinct observation date per year
# "annual_2" = at least 2 distinct observation dates per year
# "dry-dry"  = at least 1 observation date in the Dry season (April-May-June) per year
# "dry-wet"  = at least 1 observation date in the Dry season (April-May-June) AND at least 1 at the end of the reproductive season (December-January-February-March) within the same annual cycle.
# Annual cycle Y = Dry season of year Y + Dec of year Y + Jan-Mar of year Y+1
# (i.e., Jan-Mar observations are attributed to the cycle of the previous year)

criteria = "dry-dry"

# Minimum number of consecutive qualifying years
threshold = 3

# Years excluded because of Yellow Fever
excluded_years = 2017:2019

# If TRUE, excluded years do not break a run (e.g., 2016 and 2020 are treated as consecutive)
# If FALSE, a group monitored in 2016 and 2020 has two distinct runs
bridge_excluded_years = FALSE

# Months defining the seasons
dry_months = 4:6
wet_months = c(12, 1:3)

# Maximum number of missing years tolerated between two qualifying years within a run
# 0 = strictly consecutive years
# 1 = e.g., 2003, 2005, 2006 form a single run (even if 2004 is missing)
max_gap_years = 1

stopifnot(max_gap_years >= 0)

# Label reused in the plot subtitles
threshold_label = paste0(
  "At least ", threshold, " monitored years per run (max gap: ",
  max_gap_years, " yr)"
)

stopifnot(
  length(criteria) == 1,
  criteria %in% c("annual_1", "annual_2", "dry-dry", "dry-wet")
)
stopifnot(threshold >= 1)


#### Correspondence UUMP-Patch names -----
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
    # CHECK: patch_id must match the names used in the patch file
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

### Correct UMMP
# We reassign some groups to other UMMPs when relevant
# For instance, we consider 4 Irmaos as a distinct patch (not as part of Aldeia I)
# NB: this must be done BEFORE checking the correspondences
data.clean.final = data.clean.final %>%
  dplyr::mutate(
    UMMPs = dplyr::case_when(
      Region == "Sitio 4 Irmaos" ~ "4_Irmaos",
      Region == "Sitio Recanto Preservar" ~ "Recanto_Preservar",
      Region == "Faz. Sao Joao" ~ "Sao_Joao",
      TRUE ~ as.character(UMMPs)
    )
  )

# Any forgotten correspondence in the long-term dataset?
data.clean.final %>%
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::distinct(UMMPs) %>%
  dplyr::mutate(UMMPs = as.character(UMMPs)) %>%
  dplyr::anti_join(UMMP_crosswalk, by = "UMMPs")

# Any forgotten correspondence in the patch file?
patches_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(patch_id) %>%
  dplyr::anti_join(
    UMMP_crosswalk %>% dplyr::distinct(patch_id),
    by = "patch_id"
  )


#### Select monitoring data -----
# Individual-level table (kept to count distinct individuals later)
monitoring_ind = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(Group),
    !is.na(UMMPs),
    !is.na(Year),
    !is.na(DateObs),
    # Remove YF years
    !Year %in% excluded_years
  ) %>%
  dplyr::mutate(
    Group = as.character(Group),
    UMMPs = as.character(UMMPs),
    Year = as.integer(as.character(Year)),
    DateObs = as.Date(DateObs),
    season = as.character(season),
    Month = lubridate::month(DateObs),
    # Annual cycle used to count consecutive years
    # dry-wet: Jan-Mar belong to the cycle of the previous year
    Cycle = if (criteria == "dry-wet") {
      as.integer(ifelse(Month <= 3, Year - 1L, Year))
    } else {
      Year
    }
  )

# Apply restrictions pertaining to seasons (i.e., sampling during specific seasons)
if (criteria == "dry-dry") {
  monitoring_ind = monitoring_ind %>%
    dplyr::filter(Month %in% dry_months)
}

if (criteria == "dry-wet") {
  monitoring_ind = monitoring_ind %>%
    dplyr::filter(Month %in% c(dry_months, wet_months))
}

monitoring_ind = monitoring_ind %>%
  dplyr::distinct(
    UMMPs, Region, Group, GLT, Year, Cycle, Month, DateObs, season
  )

# Date-level table (1 row = 1 group x 1 observation date)
monitoring = monitoring_ind %>%
  dplyr::distinct(
    UMMPs, Region, Group, Year, Cycle, Month, DateObs, season
  ) %>%
  dplyr::left_join(
    UMMP_crosswalk,
    by = "UMMPs"
  )

cat("\nCriterion:", criteria,
    "\nThreshold:", threshold, "consecutive years",
    "\nExcluded years:", paste(excluded_years, collapse = ", "),
    "\nBridge excluded years:", bridge_excluded_years,
    "\nNumber of monitoring records:", nrow(monitoring), "\n")

cat("\nMonitoring data preview:\n")
head(monitoring, n = 10)


#### Qualifying years -----

# Count distinct observation dates per group, UMMP and annual cycle
annual_counts = monitoring %>%
  dplyr::group_by(UMMPs, Group, Cycle) %>%
  dplyr::summarise(
    n_observations = dplyr::n_distinct(DateObs),
    n_dry = dplyr::n_distinct(DateObs[Month %in% dry_months]),
    n_wet = dplyr::n_distinct(DateObs[Month %in% wet_months]),
    .groups = "drop"
  )

# Apply the observation-count criterion
years = annual_counts %>%
  # Number of required observations
  dplyr::mutate(
    qualifies = switch(
      criteria,
      "annual_1" = n_observations >= 1,
      "annual_2" = n_observations >= 2,
      "dry-dry"  = n_dry >= 1,
      "dry-wet"  = n_dry >= 1 & n_wet >= 1
    )
  ) %>%
  dplyr::filter(qualifies) %>%
  dplyr::distinct(UMMPs, Group, Cycle) %>%
  dplyr::arrange(UMMPs, Group, Cycle)


#### Consecutive monitoring periods -----
#### Monitoring runs (gaps tolerated up to max_gap_years) -----
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
    # A new run starts when the gap since the previous qualifying year
    # is larger than max_gap_years + 1 (e.g., 2003 -> 2005 = gap of 1 missing year)
    run_id = cumsum(
      dplyr::row_number() == 1 |
        (Cycle_index - dplyr::lag(Cycle_index)) > (max_gap_years + 1)
    )
  ) %>%
  dplyr::group_by(UMMPs, Group, run_id) %>%
  dplyr::summarise(
    # Number of qualifying years in the run
    # (column name kept so that the rest of the script works unchanged)
    n_consecutive_years = dplyr::n(),
    first_year = min(Cycle),
    last_year = max(Cycle),
    # Total length of the run, including the tolerated gaps
    span_years = max(Cycle_index) - min(Cycle_index) + 1,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    n_gap_years = span_years - n_consecutive_years
  )

cat("\nAll consecutive monitoring runs:\n")
print(year_runs %>%
        dplyr::filter(n_consecutive_years > 1) %>%
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


#### Cum. nb of distinct groups ----
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


#### Keep runs based on the threshold -----
qualifying_runs = year_runs %>%
  dplyr::filter(n_consecutive_years >= threshold)

stopifnot("No run meets the threshold" = nrow(qualifying_runs) > 0)

cat("\nRuns meeting the threshold:\n")
print(qualifying_runs, n = Inf)

# Each group is counted only once per UMMP
qualifying_groups = qualifying_runs %>%
  dplyr::distinct(UMMPs, Group)

cat("\nDistinct qualifying UMMP-group combinations:\n")
print(qualifying_groups, n = Inf)


#### Groups per patch -----

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


#### PLOT: Cum groups ----

plot_duration = ggplot2::ggplot(
  summary_monitoring %>%
    dplyr::mutate(
      # Bar corresponding to the retained threshold
      is_threshold = max_consecutive_years == threshold
    ),
  ggplot2::aes(
    x = max_consecutive_years,
    y = n_groups_at_least,
    fill = is_threshold
  )
) +
  ggplot2::geom_col() +
  ggplot2::geom_text(
    ggplot2::aes(label = n_groups_at_least),
    vjust = -0.4,
    size = 3.5
  ) +
  ggplot2::scale_fill_manual(
    values = c("FALSE" = "grey70", "TRUE" = "#d95f02"),
    # No legend: avoids adding an entry to the collected legend of the synthetic figure
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_len(max_duration)
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.1))
  ) +
  ggplot2::labs(
    title = "Continuity of group monitoring",
    subtitle = paste0(
      "Criterion: ", criteria,
      " | Excluding ", min(excluded_years), "–", max(excluded_years),
      " | Selected threshold: ", threshold, " years (orange)"
    ),
    x = "Minimum number of monitored years in a run",
    y = "Cumulative number of distinct groups"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(plot_duration)


#### PLOT: Nb. grp per UMMP ------

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
    subtitle = paste0("Criterion: ", criteria, " | ", threshold_label),
    x = "UMMP patch",
    y = "Number of qualifying groups"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(plot_groups_UMMP)


#### MAP: Included patches -----

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
    subtitle = paste0("Criterion: ", criteria, " | ", threshold_label)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none"
  )

print(map_UMMPs)


#### Progressive inclusion of UMMPs ------

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


#### Temporal observations ------

# Retain observations falling within each qualifying run.
# The previous version joined on (UMMPs, Group) only and then filtered the years,
# which created a many-to-many join (several runs x many dates per group).
# Here a range join links each observation to the single run containing its cycle:
# each run matches many observations, but each observation matches at most one run
# (runs of a group never overlap), hence relationship = "one-to-many".
# NB: inside join_by(), between() is used without namespace on purpose
retained_data = qualifying_runs %>%
  dplyr::inner_join(
    monitoring,
    by = dplyr::join_by(
      UMMPs, Group,
      between(y$Cycle, x$first_year, x$last_year)
    ),
    relationship = "one-to-many"
  ) %>%
  # Keep only observations from qualifying years (not from the gap years)
  dplyr::semi_join(years, by = c("UMMPs", "Group", "Cycle")) %>%
  dplyr::distinct(
    UMMPs, Group, run_id, Cycle, Year, DateObs
  ) %>%
  dplyr::mutate(
    group_label = paste(UMMPs, Group, sep = " — "),
    run_label = paste(group_label, run_id, sep = " | ")
  ) %>%
  dplyr::arrange(UMMPs, Group, DateObs)

# Same retained observations, at the individual level (for the summary panel)
retained_ind = qualifying_runs %>%
  dplyr::inner_join(
    monitoring_ind,
    by = dplyr::join_by(
      UMMPs, Group,
      between(y$Cycle, x$first_year, x$last_year)
    ),
    relationship = "one-to-many"
  ) %>%
  dplyr::semi_join(years, by = c("UMMPs", "Group", "Cycle"))

cat("\nNumber of retained observation dates:",
    dplyr::n_distinct(retained_data$DateObs), "\n")


#### Summary statistics of the retained data ------
info_stats = retained_ind %>%
  dplyr::summarise(
    n_dates = dplyr::n_distinct(DateObs),
    n_group_visits = dplyr::n_distinct(UMMPs, Group, DateObs),
    n_groups = dplyr::n_distinct(UMMPs, Group),
    n_individuals = dplyr::n_distinct(GLT),
    n_UMMPs = dplyr::n_distinct(UMMPs),
    year_min = min(Year),
    year_max = max(Year)
  )

cat("\nSummary of the retained data:\n")
print(info_stats)


#### PLOT: temporal observations ------
# Aggregate observation dates within the same year (annual cycle) and run
retained_year = retained_data %>%
  dplyr::group_by(
    UMMPs, Group, group_label, run_id, run_label, Cycle
  ) %>%
  dplyr::summarise(
    n_dates = dplyr::n_distinct(DateObs),
    .groups = "drop"
  )

# Order groups by UMMP and group identifier
group_order = retained_year %>%
  dplyr::distinct(UMMPs, Group, group_label) %>%
  dplyr::arrange(UMMPs, Group) %>%
  dplyr::pull(group_label)

retained_year = retained_year %>%
  dplyr::mutate(
    group_label = factor(
      group_label,
      levels = group_order
    )
  ) %>%
  dplyr::arrange(UMMPs, Group, Cycle)

temporal_plot = ggplot2::ggplot(
  retained_year,
  ggplot2::aes(
    x = Cycle,
    y = group_label,
    group = run_label
  )
) +
  ggplot2::geom_line(
    colour = "grey55",
    linewidth = 0.5
  ) +
  ggplot2::geom_point(
    ggplot2::aes(size = n_dates)
  ) +
  ggplot2::scale_size_continuous(
    name = "Observation dates per year",
    range = c(1, 3),
    # Integer breaks only (counts)
    breaks = function(x) unique(round(scales::breaks_pretty()(x))),
    guide = ggplot2::guide_legend(
      nrow = 1,
      title.position = "left",
      label.position = "right"
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 10)
  ) +
  ggplot2::labs(
    title = "Temporal distribution of retained monitoring",
    subtitle = paste0("Criterion: ", criteria, " | ", threshold_label),
    x = "Year",
    y = "UMMP — Group"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 7)
  )

print(temporal_plot)


#### PLOT: summary panel ------
info_df = data.frame(
  x = 1:6,
  value = c(
    format(info_stats$n_dates, big.mark = ","),
    format(info_stats$n_group_visits, big.mark = ","),
    format(info_stats$n_groups, big.mark = ","),
    format(info_stats$n_individuals, big.mark = ","),
    format(info_stats$n_UMMPs, big.mark = ","),
    paste0(info_stats$year_min, "–", info_stats$year_max)
  ),
  label = c(
    "Distinct observation dates",
    "Group visits\n(group x date)",
    "Distinct groups",
    "Distinct individuals",
    "Distinct UMMPs",
    "Period covered"
  ),
  stringsAsFactors = FALSE
)

info_panel = ggplot2::ggplot(
  info_df,
  ggplot2::aes(x = x)
) +
  ggplot2::geom_text(
    ggplot2::aes(y = 1.15, label = value),
    size = 7,
    fontface = "bold"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(y = 0.75, label = label),
    size = 3,
    lineheight = 0.9
  ) +
  ggplot2::scale_x_continuous(limits = c(0.5, 6.5)) +
  ggplot2::scale_y_continuous(limits = c(0.4, 1.5)) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "grey95", colour = NA)
  )


#### SYNTHETIC FIGURE ------

### Prepare the existing plots for the combined figure
# Cumulative groups
plot_duration_combined = plot_duration +
  ggplot2::labs(
    title = "A. Continuity of group monitoring",
    subtitle = paste0("Criterion: ", criteria, " | Threshold highlighted in orange"),
    x = "Minimum number of monitored years in a run",
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
    subtitle = threshold_label,
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
    subtitle = threshold_label
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
    x = "Year",
    y = "UMMP — Group"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 11),
    plot.subtitle = ggplot2::element_text(size = 9),
    axis.text.y = ggplot2::element_text(size = 6)
  )


### Assemble the figure
# Letters in the design follow the order in which plots are added:
# A = duration, B = groups per UMMP, C = map, D = temporal, E = summary panel
synthetic_figure = (
  plot_duration_combined +
    plot_groups_UMMP_combined +
    map_UMMPs_combined +
    temporal_plot_combined +
    info_panel
) +
  patchwork::plot_layout(
    design = "
    EE
    AB
    CD
    ",
    heights = c(0.35, 1, 1.7),
    widths = c(1, 1),
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    title = "Continuous monitoring of groups across UMMPs",
    subtitle = paste0(criteria, " | ", threshold_label,
                      " | Excluded years: ", paste(excluded_years, collapse = ", ")),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = ggplot2::element_text(size = 10),
      # Collected legend: one compact horizontal row at the bottom
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
    )
  )

print(synthetic_figure)


### Save the figure
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_sampling_effort_dry-wet_0gap.png"),
    width = 3000, height = 2800, res = 300, type="cairo")
print(synthetic_figure)
dev.off()
