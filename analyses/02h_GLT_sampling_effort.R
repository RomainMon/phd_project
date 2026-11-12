#------------------------------------------------#
# Author: Romain Monassier
# Objective: Sampling effort of long-term GLT data
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)
library(xlsx)
library(readxl)
library(terra)
library(sf)
library(lubridate)
library(patchwork)

### Import datasets -----

#### GLT DATA -----
# GLT data
load(here::here("data", "glt", "APonchon", "data_clean_long_final.RData"))

#### Group locations -----
# Group locations
regions = sf::st_read(here::here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions_csv = readr::read_csv2(
  here::here("data", "geo", "APonchon", "GLT", "RegionsName.csv"),
  locale = readr::locale(encoding = "UTF-8"),
  na = ""
)

#### Population data -----
real_census_data = read_excel(here::here("data", "glt", "JDietz", "GLT_POP_2005_2023.xlsx"), na="NA")
real_census_data %>%
  dplyr::summarise(dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

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
# Patches in 2024
patches_sf = patches[[36]]

### Reassign season -----
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

### Correspondence UUMP-Patch names -----
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

# Fill missing UMMPs of data.clean.final -----
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

# Correct UMMP
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

### Problematic individuals -----
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490") # Last list of problematic individuals (see Aurore Zenodo)

### Sampling effort -----

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

#### By group -----
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

# Build separate season and year axis labels
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

##### Export plot ----
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

#### By patch -----
##### Overall -----
# Patch size (patches_sf = most recent patch map)
patch_size = patches_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(patch_id, area_ha)

# Sampling effort per patch (1 visit = 1 group x 1 observation date)
effort_patch = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    !GLT %in% bad,
    !is.na(Group),
    !is.na(UMMPs),
    !is.na(DateObs)
  ) %>%
  dplyr::mutate(
    UMMPs = as.character(UMMPs),
    Group = as.character(Group),
    DateObs = as.Date(DateObs)
  ) %>%
  # Link UMMP names to patch names
  dplyr::left_join(UMMP_crosswalk, by = "UMMPs") %>%
  dplyr::filter(!is.na(patch_id)) %>%
  dplyr::distinct(patch_id, Group, DateObs) %>%
  dplyr::group_by(patch_id) %>%
  dplyr::summarise(
    n_visits = dplyr::n_distinct(DateObs),
    n_groups = dplyr::n_distinct(Group),
    n_years = dplyr::n_distinct(lubridate::year(DateObs)),
    .groups = "drop"
  )

# Keep every patch (0 if never monitored) and add effort relative to patch size
effort_patch = patch_size %>%
  dplyr::left_join(effort_patch, by = "patch_id") %>%
  dplyr::mutate(
    dplyr::across(c(n_visits, n_groups, n_years), ~ dplyr::coalesce(.x, 0L)),
    visits_per_group = dplyr::if_else(n_groups > 0, n_visits / n_groups, NA_real_),
    # Patches ordered by size (largest at the top of the plots)
    patch_id = stats::reorder(patch_id, area_ha)
  )

effort_patch %>%
  dplyr::arrange(dplyr::desc(area_ha))

# Common theme
theme_patch = ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 11),
    panel.grid.major.y = ggplot2::element_blank()
  )

# A. Patch size
p_area = ggplot2::ggplot(
  effort_patch,
  ggplot2::aes(x = area_ha, y = patch_id)
) +
  ggplot2::geom_col(fill = "grey60") +
  ggplot2::geom_text(
    ggplot2::aes(label = format(round(area_ha), big.mark = ",")),
    hjust = -0.15,
    size = 3
  ) +
  ggplot2::scale_x_log10(
    expand = ggplot2::expansion(mult = c(0, 0.3))
  ) +
  ggplot2::labs(
    title = "A. Patch size",
    x = "Area (ha, log scale)",
    y = "UMMP (patch), ordered by size"
  ) +
  theme_patch

# B. Number of distinct groups monitored
p_groups = ggplot2::ggplot(
  effort_patch,
  ggplot2::aes(x = n_groups, y = patch_id)
) +
  ggplot2::geom_col(fill = "#1b9e77") +
  ggplot2::geom_text(
    ggplot2::aes(label = n_groups),
    hjust = -0.3,
    size = 3
  ) +
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.2))
  ) +
  ggplot2::labs(
    title = "B. Distinct groups monitored",
    x = "Number of groups",
    y = NULL
  ) +
  theme_patch +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

# C. Number of visits
p_visits = ggplot2::ggplot(
  effort_patch,
  ggplot2::aes(x = n_visits, y = patch_id)
) +
  ggplot2::geom_col(fill = "#7570b3") +
  ggplot2::geom_text(
    ggplot2::aes(label = format(n_visits, big.mark = ",")),
    hjust = -0.2,
    size = 3
  ) +
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.25))
  ) +
  ggplot2::labs(
    title = "C. Visits",
    x = "Number of visits",
    y = NULL
  ) +
  theme_patch +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

patch_effort_plot = patchwork::wrap_plots(
  p_area, p_groups, p_visits,
  nrow = 1,
  widths = c(1.2, 1, 1)
) +
  patchwork::plot_annotation(
    title = "Sampling effort per UMMP, ordered by patch size",
    subtitle = paste0(
      "Patch size from the ", names(patches)[36], " patch map; ",
      "visits = distinct group x observation date"
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 9)
    )
  )

print(patch_effort_plot)

###### Export plot ----
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_sampling_effort_per_patch.png"),
    width = 3000, height = 1800, res = 300, type = "cairo")
print(patch_effort_plot)
dev.off()

##### By season/year ----
# Helper function to remove accents
norm_name = function(x) {
  tolower(gsub("[^A-Za-z0-9]", "", stringi::stri_trans_general(x, "Latin-ASCII")))
}

# Unique patches ids in the spatial patches
patch_ids = unique(as.character(patches_sf$patch_id))
patch_ids

# Match between all patches names found in several datasets
match_patch = function(ummp) {
  from_crosswalk = UMMP_crosswalk$patch_id[match(ummp, UMMP_crosswalk$UMMPs)]
  from_name = patch_ids[match(norm_name(ummp), norm_name(patch_ids))]
  dplyr::coalesce(from_crosswalk, from_name)
}

# UMMPs of the long-term data without any patch
ummp_patch = dplyr::tibble(
  UMMPs = unique(as.character(data.clean.final$UMMPs))
) %>%
  dplyr::filter(!is.na(UMMPs)) %>%
  # Correspondence with spatial patches
  dplyr::mutate(patch_id = match_patch(UMMPs))

### Visits table
visits_tbl = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT), 
    !GLT %in% bad,
    !is.na(Group), 
    !is.na(UMMPs), 
    !is.na(DateObs)
  ) %>%
  dplyr::mutate(
    UMMPs = as.character(UMMPs),
    Group = as.character(Group),
    DateObs = as.Date(DateObs),
    Year = lubridate::year(DateObs),
    season = as.character(season)
  ) %>%
  # Join patches ids
  dplyr::left_join(ummp_patch, by = "UMMPs") %>%
  dplyr::filter(!is.na(patch_id)) %>%
  dplyr::group_by(patch_id, Group, DateObs, Year, season) %>%
  dplyr::summarise(
    # Group size on that date (distinct GLT)
    GS = dplyr::n_distinct(GLT),
    .groups = "drop"
  )

### Effort per patch x year x season
season_levels = c("Wet 1", "Dry", "Wet 2")
patch_levels = levels(effort_patch$patch_id) # ordered by patch size
year_range = range(visits_tbl$Year)

# Number of visits
visits_grid = visits_tbl %>%
  dplyr::group_by(patch_id, Year, season) %>%
  dplyr::summarise(
    n_visits = dplyr::n_distinct(DateObs),
    n_groups = dplyr::n_distinct(Group),
    .groups = "drop"
  ) %>%
  # Complete the grid by adding 0 where there is no monitoring
  dplyr::right_join(
    tidyr::expand_grid(
      patch_id = patch_levels,
      Year = year_range[1]:year_range[2],
      season = season_levels
    ),
    by = c("patch_id", "Year", "season")
  ) %>%
  dplyr::mutate(
    n_visits = dplyr::coalesce(n_visits, 0L),
    n_groups = dplyr::coalesce(n_groups, 0L),
    patch_id = factor(patch_id, levels = patch_levels),
    # Chronological index: Wet 1 / Dry / Wet 2 within each year
    time_index = 3 * (Year - year_range[1]) + match(season, season_levels)
  )

# X axis: one label every 2 years, centred on the Dry season
year_seq = seq(year_range[1], year_range[2], by = 2)
year_breaks = 3 * (year_seq - year_range[1]) + 2

# One facet per year: the three seasons (W1 / D / W2) are on the axis, the year is below
tile_panel = function(df, title, legend_name) {
  df = df %>%
    dplyr::mutate(season = factor(season, levels = season_levels))
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = season, y = patch_id, fill = value)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.1) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(Year),
      switch = "x"
    ) +
    ggplot2::scale_fill_viridis_c(
      name = legend_name,
      trans = "log10",
      direction = -1,
      na.value = "grey93"      # grey = no monitoring
    ) +
    ggplot2::scale_x_discrete(
      labels = c("Wet 1" = "W1", "Dry" = "D", "Wet 2" = "W2")
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "UMMP (patch), ordered by size"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      panel.grid = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(0.15, "lines"),
      axis.text.x = ggplot2::element_text(size = 5, angle = 90, vjust = 0.5, hjust = 1),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 7, angle = 90, hjust = 1)
    )
}

p_tiles_visits = tile_panel(
  visits_grid %>% dplyr::mutate(value = dplyr::na_if(n_visits, 0L)),
  "A. Visits per season and year", "Visits"
)

p_tiles_groups = tile_panel(
  visits_grid %>% dplyr::mutate(value = dplyr::na_if(n_groups, 0L)),
  "B. Distinct groups monitored per season and year", "Groups"
)

patch_effort_years = (p_tiles_visits / p_tiles_groups) +
  patchwork::plot_annotation(
    title = "Sampling effort per UMMP through time",
    subtitle = paste0(
      "UMMPs ordered by patch size (largest at the top); ",
      "W1 = Wet 1 (Jan-Mar), D = Dry (Apr-Aug), W2 = Wet 2 (Sep-Dec); ",
      "grey = no monitoring"
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 9)
    )
  )

print(patch_effort_years)

###### Export plot ----
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_sampling_effort_per_patch_years.png"),
    width = 3500, height = 2400, res = 300, type = "cairo")
print(patch_effort_years)
dev.off()

##### Snapshots ----
# Compare the census data with the estimated population size at census dates (Dietz et al. 2024)
census_years = c(2005, 2013, 2018, 2022)   # years with a census
census_window = 1 # years before AND after each census (1 = e.g., 2012-2013-2014 for 2013)
mean_GS = 4.5 # mean group size used to convert groups into individuals

# Census: remove the "n_glt_" prefix and reshape to long format
census_patch = real_census_data %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("n_glt_"),
    names_to = "Year",
    names_prefix = "n_glt_",
    names_transform = list(Year = as.integer),
    values_to = "pop"
  ) %>%
  dplyr::filter(Year %in% census_years) %>%
  dplyr::mutate(patch_id = match_patch(UMMPs))

# Census UMMPs without patch (to add to UMMP_crosswalk if needed)
census_patch %>%
  dplyr::filter(is.na(patch_id)) %>%
  dplyr::distinct(UMMPs, FragName)

# Summarise pop
census_patch = census_patch %>%
  dplyr::filter(!is.na(patch_id)) %>%
  dplyr::group_by(patch_id, Year) %>%
  dplyr::summarise(
    pop = if (all(is.na(pop))) NA_real_ else sum(pop, na.rm = TRUE),
    .groups = "drop"
  )

# Monitoring pooled over the window around each census year
effort_census = dplyr::bind_rows(lapply(census_years, function(cy) {
  win = visits_tbl %>%
    dplyr::filter(abs(Year - cy) <= census_window # keep years of the window
                  )
  
  # Largest size reached by each group in the window
  per_group = win %>%
    dplyr::group_by(patch_id, Group) %>%
    dplyr::summarise(GS_max = max(GS), .groups = "drop")
  
  # Visits and number of years actually monitored in the window
  per_patch = win %>%
    dplyr::group_by(patch_id) %>%
    dplyr::summarise(
      n_visits = dplyr::n_distinct(DateObs),
      n_years = dplyr::n_distinct(Year),
      .groups = "drop"
    )
  
  per_group %>%
    dplyr::group_by(patch_id) %>%
    dplyr::summarise(
      n_groups = dplyr::n(),
      # Individuals actually counted (sum of the maximum size of each group)
      n_ind_obs = sum(GS_max),
      .groups = "drop"
    ) %>%
    dplyr::left_join(per_patch, by = "patch_id") %>%
    dplyr::mutate(Year = cy)
}))

period_labels = paste0(
  census_years, " (", census_years - census_window, "–",
  census_years + census_window, ")"
)

census_effort = tidyr::expand_grid(patch_id = patch_levels, Year = census_years) %>%
  dplyr::left_join(effort_census, by = c("patch_id", "Year")) %>%
  dplyr::left_join(census_patch, by = c("patch_id", "Year")) %>%
  dplyr::mutate(
    dplyr::across(c(n_groups, n_visits, n_years, n_ind_obs), ~ dplyr::coalesce(.x, 0L)),
    # Individuals expected in the monitored groups
    est_pop_monitored = n_groups * mean_GS,
    # Share of the census population covered by monitored groups
    coverage = dplyr::if_else(!is.na(pop) & pop > 0, est_pop_monitored / pop, NA_real_),
    patch_id = factor(patch_id, levels = patch_levels),
    # Legend: census year and monitoring window
    Year_f = factor(Year, levels = census_years, labels = period_labels)
  )

census_effort %>%
  dplyr::arrange(Year, dplyr::desc(pop)) %>%
  print(n = Inf)

# Remove 2018 (yellow fever) and prepare labels
census_effort = census_effort %>%
  dplyr::filter(Year != 2018) %>%
  dplyr::mutate(
    # Largest patch on the left
    patch_id = factor(patch_id, levels = rev(patch_levels)),
    facet_label = paste0(
      "Census ", Year, "  (monitoring ",
      Year - census_window, "–", Year + census_window, ")"
    )
  ) %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(facet_label = factor(facet_label, levels = unique(facet_label)))

# Long format: for each UMMP x census year, the census and the estimate side by side
census_long_plot = census_effort %>%
  dplyr::select(
    patch_id, facet_label,
    Census = pop,
    Monitored = n_ind_obs
  ) %>%
  tidyr::pivot_longer(
    cols = c(Census, Monitored),
    names_to = "type",
    values_to = "value"
  ) %>%
  dplyr::mutate(type = factor(type, levels = c("Census", "Monitored")))

# The ratio monitored GLTs to estimated GLTs in the patch, written above each pair of bars
coverage_labels = census_effort %>%
  dplyr::filter(!is.na(pop), pop > 0) %>%
  dplyr::transmute(
    patch_id, facet_label,
    y = pmax(pop, n_ind_obs),
    label = scales::percent(n_ind_obs / pop, accuracy = 1)
  )

census_figure = ggplot2::ggplot(
  census_long_plot,
  ggplot2::aes(x = patch_id, y = value, fill = type)
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.8, preserve = "single"),
    width = 0.75,
    na.rm = TRUE       # UMMP without census value: no bar (not a zero)
  ) +
  ggplot2::geom_text(
    data = coverage_labels,
    ggplot2::aes(x = patch_id, y = y, label = label),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 2.4
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(facet_label),
    ncol = 1,
    scales = "free_y"
  ) +
  ggplot2::scale_fill_manual(
    values = c("Census" = "grey35", "Monitored" = "#e08214"),
    name = NULL
  ) +
  # Square-root scale: starts at 0 and keeps small populations visible
  ggplot2::scale_y_sqrt(
    breaks = scales::breaks_extended(n = 5),
    labels = scales::label_number(big.mark = ","),
    expand = ggplot2::expansion(mult = c(0, 0.15))
  ) +
  ggplot2::labs(
    title = "Census population vs. population covered by monitoring",
    subtitle = paste0(
      "UMMPs ordered by patch size (largest on the left); \nPercentages = nb. GLTs monitored / census"
    ),
    x = "UMMP (patch), ordered by size",
    y = "Individuals (sqrt scale)"
  ) +
  theme_patch +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle = ggplot2::element_text(size = 9),
    strip.text = ggplot2::element_text(face = "bold", hjust = 0),
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major.y = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.position = "bottom"
  )

print(census_figure)

###### Export plot -----
png(here("outputs", "plot", "Axis2", "GLT_sampling_effort_patch_2005-2013-2022.png"),
    width = 2600, height = 1800, res = 300, type = "cairo")
print(census_figure)
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
# "annual" = at least N distinct observation date per year
# "dry-dry"  = at least N observation date in the Dry season (April-May-June) per year
# "dry-wet"  = at least N observation date in the Dry season (April-May-June) AND at least N at the end of the reproductive season (December-January-February-March) within the same annual cycle.
# Annual cycle Y = Dry season of year Y + Dec of year Y + Jan-Mar of year Y+1
# (i.e., Jan-Mar observations are attributed to the cycle of the previous year)

criteria = "dry-wet"

# Minimum number of DISTINCT observation dates required per year (annual cycle)
# min_obs_year = in the whole window (all months considered by `criteria`)
# min_obs_dry = in the dry months (dry_months)
# min_obs_wet = in the wet months (wet_months)
# Use 0 to ignore a condition. Examples (former criteria):
min_obs_year = 0
min_obs_dry = 1
min_obs_wet = 1

# Minimum number of consecutive years
# i.e., nb of continuous monitoring required (e.g., 3 years means that a group monitored in 2005-2007 is excluded)
threshold = 2

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
# NB: this limits each gap individually = there could be several gaps within a run (e.g., 2003-2005-2006-2008 with a gap of 1 year), as long as the gap is below the threshold retained
max_gap_years = 1

# Label reused in the plot subtitles
threshold_label = paste0(
  "At least ", threshold, " monitored years per run (max gap: ",
  max_gap_years, " yr)"
)

# Label describing the observation criterion (reused in the figure)
dry_label = paste(month.abb[dry_months], collapse = "-")
wet_label = paste(month.abb[wet_months], collapse = "-")
obs_label = paste(
  c(
    if (min_obs_year > 0) paste0("≥", min_obs_year, " obs. date(s) per year"),
    if (min_obs_dry > 0) paste0("≥", min_obs_dry, " in dry season (", dry_label, ")"),
    if (min_obs_wet > 0) paste0("≥", min_obs_wet, " in wet season (", wet_label, ")")
  ),
  collapse = " + "
)

stopifnot(
  length(criteria) == 1,
  criteria %in% c("annual", "dry-dry", "dry-wet"),
  min_obs_year >= 0, min_obs_dry >= 0, min_obs_wet >= 0,
  min_obs_year + min_obs_dry + min_obs_wet >= 1
)
if (criteria == "dry-dry" && min_obs_wet > 0) {
  stop("criteria = 'dry-dry' only keeps dry months: min_obs_wet must be 0")
}
stopifnot(threshold >= 1)


#### Select monitoring data -----
# Individual-level table (kept to count distinct individuals later)
monitoring_ind = data.clean.final %>%
  dplyr::filter(
    !is.na(GLT),
    # Remove bad individuals
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
  # Join UMMP name based on the correspondence with the patch file
  dplyr::left_join(
    UMMP_crosswalk,
    by = "UMMPs"
  )

### Summary of criteria
cat("\nCriterion:", criteria,
    "\nObservations required:", obs_label,
    "\nThreshold:", threshold, "qualifying years per run",
    "\nMax gap:", max_gap_years, "year(s)",
    "\nExcluded years:", paste(excluded_years, collapse = ", "),
    "\nBridge excluded years:", bridge_excluded_years,
    "\nNumber of observations:", nrow(monitoring), "\n")

cat("\nMonitoring data preview:\n")
head(monitoring, n = 10)


#### Qualifying years -----

# Count distinct observation dates per group, UMMP and annual cycle
annual_counts = monitoring %>%
  dplyr::group_by(UMMPs, Group, Cycle) %>%
  dplyr::summarise(
    n_observations = dplyr::n_distinct(DateObs), # Number of distinct observations
    n_dry = dplyr::n_distinct(DateObs[Month %in% dry_months]), # Number of distinct observations in dry months
    n_wet = dplyr::n_distinct(DateObs[Month %in% wet_months]), # Number of distinct observations in wet months
    .groups = "drop"
  )

# Apply the observation criteria (i.e., min nb. of observations)
years = annual_counts %>%
  dplyr::mutate(
    qualifies = n_observations >= min_obs_year &
      n_dry >= min_obs_dry &
      n_wet >= min_obs_wet
  ) %>%
  dplyr::filter(qualifies) %>%
  dplyr::distinct(UMMPs, Group, Cycle) %>%
  dplyr::arrange(UMMPs, Group, Cycle)


#### Consecutive monitoring runs (gaps tolerated up to max_gap_years) -----
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
    # A new run starts when the gap since the previous qualifying year is larger than max_gap_years + 1 (e.g., 2003 -> 2005 = gap of 1 missing year)
    # Example: group CLA monitored in Aldeia I is monitored in 1991-1993, 1996-1999, 2002-2007 (except 2006)
    # Given an accepted gap of 1 year (e.g., 1994, 1995, or 2006 are gaps because there was no monitoring these years), there will be 3 RUNS
    run_id = cumsum(
      dplyr::row_number() == 1 |
        (Cycle_index - dplyr::lag(Cycle_index)) > (max_gap_years + 1)
    )
  )

# Summarise the number of years per run
year_runs = year_runs %>% 
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
# Remove runs based on the min. nb of consecutive years required
qualifying_runs = year_runs %>%
  dplyr::filter(n_consecutive_years >= threshold)

stopifnot("No run meets the threshold" = nrow(qualifying_runs) > 0)

cat("\nRuns meeting the threshold:\n")
print(qualifying_runs, n = Inf)

# Each group is counted only once per UMMP
qualifying_groups = qualifying_runs %>%
  dplyr::distinct(UMMPs, Group)

#### Groups per patch -----
# All patch IDs, including patches with no observations
all_patches = patches_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(patch_id)

# Count groups by matching observation UMMPs to patch IDs
groups_per_patch = qualifying_groups %>%
  # Join to groups the UMMP name (from patches)
  dplyr::left_join(
    UMMP_crosswalk,
    by = "UMMPs"
  ) %>%
  # Remove those without UMMPs
  dplyr::filter(!is.na(patch_id)) %>%
  dplyr::count(
    patch_id,
    name = "n_groups"
  )
# Number of groups
groups_per_patch %>% dplyr::select(n_groups) %>% colSums()

# Keep every patch, assigning zero to those without qualifying groups
groups_per_patch = all_patches %>%
  # Join to patches the number of groups
  dplyr::left_join(
    groups_per_patch,
    by = "patch_id"
  ) %>%
  dplyr::mutate(
    # 0 if NO group
    n_groups = dplyr::coalesce(n_groups, 0L),
    # Mutate a variable where the patch is included in the dataset if it has >0 group
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
# Plot the cumulative number of groups included in the dataset (based on the threshold)
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
# Plot the number of groups per UMMP (when using the dataset filtered by the min. nb of consecutive years)
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
# Map the patches included in the analysis (when using the dataset filtered by the min. nb of consecutive years)
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

# Longest run (i.e., continuous monitoring of a group) within each UMMP
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

# By period of time, count the number of UMMPs monitored
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

# Check what UMMPs are included and excluded based on the number of consecutive years selected
cat("\nProgressive inclusion of UMMPs by monitoring duration:\n")
print(progressive_UMMP, n = Inf)


#### Temporal observations ------

# Retain observations falling within each qualifying run.
# Here a join links each observation to the single run containing its cycle:
# each run matches many observations, but each observation matches at most one run
# (runs of a group never overlap), hence relationship = "one-to-many".
retained_data = qualifying_runs %>%
  # Cross the data with the long-ter monitoring data
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
# Check the number of groups
retained_data %>% dplyr::summarise(n_group = dplyr::n_distinct(Group))

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
    # Number of distinct observations
    n_dates = dplyr::n_distinct(DateObs),
    # Number of distinct groups 
    n_groups = dplyr::n_distinct(UMMPs, Group),
    # Number of distinct individuals
    n_individuals = dplyr::n_distinct(GLT),
    # Number of distinct UMMPs
    n_UMMPs = dplyr::n_distinct(UMMPs),
    # Range
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

# Groups IDs (ordonned)
group_order = retained_year %>%
  dplyr::distinct(UMMPs, Group, group_label) %>%
  dplyr::arrange(UMMPs, Group) %>%
  dplyr::pull(group_label)

# Group level to factor
retained_year = retained_year %>%
  dplyr::mutate(
    group_label = factor(
      group_label,
      levels = group_order
    )
  ) %>%
  dplyr::arrange(UMMPs, Group, Cycle)

# Plot the group monitoring along time
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
  x = 1:5,
  value = c(
    format(info_stats$n_dates, big.mark = ","),
    format(info_stats$n_groups, big.mark = ","),
    format(info_stats$n_individuals, big.mark = ","),
    format(info_stats$n_UMMPs, big.mark = ","),
    paste0(info_stats$year_min, "–", info_stats$year_max)
  ),
  label = c(
    "Distinct observation dates",
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
    subtitle = paste0(
      "Criterion: ", criteria, " | Observations required: ", obs_label, "\n",
      threshold_label, " | Excluded years: ", paste(excluded_years, collapse = ", ")
    ),
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


###### Save the figure ------
png(here("outputs",
         "plot",
         "Axis2",
         paste0("GLT_sampling_effort_", criteria,
                "_y", min_obs_year, "_d", min_obs_dry, "_w", min_obs_wet,
                "_gap", max_gap_years, ".png")),
    width = 3000, height = 2800, res = 300, type = "cairo")
print(synthetic_figure)
dev.off()
