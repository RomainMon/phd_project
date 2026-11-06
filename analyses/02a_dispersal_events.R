#------------------------------------------------#
# Author: Romain Monassier
# Objective: Extract SUCCESSFUL dispersal events from long-term datasets, associate them to patch delimitations and export them as spatial lines
#------------------------------------------------#

# This work is based on the script "1-Formatting data.qmd" (section 4) from Aurore Ponchon repository

### Load packages ------
library(dplyr)
library(here)
library(cli)
library(xlsx)
library(terra)
library(readxl)
library(sf)
source(here::here("R","APonchon","progress_bar.R"))

### Import datasets -----

# GLT data
load(here::here("data", "glt", "APonchon", "data_clean_long_final.RData"))

# Population data
real_census_data = read_excel(here::here("data", "glt", "JDietz", "GLT_POP_2005_2023.xlsx"), na="NA")
real_census_data %>%
  dplyr::summarise(dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

# Group locations
regions = sf::st_read(here::here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions_csv = readr::read_csv2(
  here::here("data", "geo", "APonchon", "GLT", "RegionsName.csv"),
  locale = readr::locale(encoding = "ISO-8859-1"),
  na = ""
)

# LULC
raster_lulc_2024 = terra::rast(here::here("outputs", "data", "MapBiomas", "Rasters_reclass", "raster_reclass_2024.tif"))

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

# Plot
plot(raster_lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(patches[[36]]), col=NA, add=TRUE)
plot(sf::st_geometry(regions), col="magenta", add=TRUE)

### Parameters ------
# Bad GLTs (e.g., unidentified individuals)
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6","MP489","MP490") # Last list of problematic individuals (see Aurore Zenodo)

# Dispersal variable
unique(data.clean.final$Disp)

# Correspondence list between the names in the long-term dataset
# And the names of the UMMPs in the census file
unique(real_census_data$FragName)
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
    "Pirineus"
  ),
  # Equivalent in the patch file
  UMMPs_census = c(
    "União I",
    "União II",
    "Poço das Antas",
    "Rio Vermelho",
    "Imbaú III",
    "Aldeia I",
    "Aldeia II",
    "Imbaú I",
    "Imbaú III",
    "Pirineus"
  ),
  stringsAsFactors = FALSE
)

### HOW DISPERSAL IS CODED?
ex = data.clean.final %>%
  dplyr::filter(Disp %in% 1, !GLT %in% bad) %>%
  dplyr::distinct(GLT) %>%
  dplyr::slice_head(n = 3) %>%
  dplyr::pull(GLT)

data.clean.final %>%
  dplyr::filter(GLT %in% ex) %>%
  dplyr::arrange(GLT, DateObs) %>%
  dplyr::select(GLT, Group, Region, DateObs, Disp, Death, ObsOrder)
# Disp 1 means an individual is observed alone or dispersing in another fragment
# WARNING: It does not mean that the individual has left their group THAT DAY
# WARNING: DISP was computed from ONE YEAR TO ANOTHER by Aurore (Ponchon et al. 2026)

# Individuals without groups
data.clean.final %>%
  dplyr::count(group_missing = is.na(Group), Disp, Solo)

### Individual histories ----

### OBSERVATIONS -> STATUS OF EACH OBSERVATION

# Filter the data
obs_raw = data.clean.final %>%
  dplyr::mutate(
    DateObs = lubridate::as_date(DateObs),  # calendar date (DateObs is a POSIXct)
    UMMPs = as.character(UMMPs)
  ) %>%
  # Remove bad individuals
  dplyr::filter(!is.na(GLT), !GLT %in% bad, !is.na(DateObs))

# Build an observation table:
# 1 row = 1 individual (GLT) on 1 date, with its status on that date:
#   "group" in a group (and the same group as the last group where it was seen)
#   "satellite" observed without a group (Group is NA)
#   "immigration"  in a group DIFFERENT from the last group where it was seen
#   "dead" observed dead (the history stops there)
#   "disappearance": absent from a monitored group after its last sighting

# For individuals observed several times a day:
# n_groups_day = number of distinct groups that day
# n_sat_day = number of satellite records that day
# day_conflict = NA for ordinary days; otherwise "group + satellite same day" or "two groups same day"
# Rules applied to individuals with >1 observations the SAME DAY, in ObsOrder:
# same place several times -> one row (the first one, by ObsOrder)
# different places -> all rows are kept and flagged

obs = obs_raw %>%
  # Mutate the status: dead, in a group, sallite if outside a group, or translocated
  dplyr::mutate(
    status = dplyr::case_when(
      Death %in% "1" ~ "dead",
      !is.na(Group) ~ "group",
      TRUE ~ "satellite"
    ),
    # GLT key = union of group and UMMP (e.g., AF | Imbau II)
    g_key = dplyr::if_else(status == "group", paste(UMMPs, Group, sep = " | "), NA_character_),
    # If the individual is in a group, the place is the GROUP, if not it's the STATUS (e.g., dead)
    place = dplyr::if_else(status == "group", g_key, status),
    transloc = Disp %in% "2" # translocated individuals
  ) %>%
  
  # Arrange chronologically
  dplyr::arrange(GLT, DateObs, ObsOrder) %>%
  dplyr::group_by(GLT, DateObs) %>%
  dplyr::mutate(transloc = any(transloc)) %>%
  # Same place several times on the same date = one record (first by ObsOrder)
  # This happens with multiple observations of an individual in the same group the same day
  dplyr::distinct(place, .keep_all = TRUE) %>%
  # Falg individuals with several observations per date
  dplyr::mutate(
    # Observations in the same group at the same date
    n_groups_day = sum(status == "group"),
    # Satellite observations at the same day
    n_sat_day = sum(status == "satellite"),
    conflict = dplyr::case_when(
      # When the individual is observed in >1 groups the same day
      n_groups_day >= 2 ~ "two groups same day",
      # When the individual is observed in a group and satellite at the same time
      n_groups_day == 1 & n_sat_day >= 1 ~ "group + satellite same day",
      TRUE ~ NA_character_
    ),
    # If a satellite is also recorded in a group -> "contact"
    status = dplyr::if_else(status == "satellite" & n_groups_day >= 1, "contact", status),
    place = dplyr::if_else(status == "contact", "contact", place)
  ) %>%
  dplyr::ungroup() %>%
  
  # Arrange chronologically
  dplyr::arrange(GLT, DateObs, ObsOrder) %>%
  dplyr::group_by(GLT) %>%
  # Nothing after the first record of death and 0 before death
  dplyr::filter(cumsum(dplyr::lag(status == "dead", default = FALSE)) == 0) %>%
  dplyr::mutate(
    # Last group in which the individual was seen BEFORE this record
    prev_g_key = dplyr::lag(g_key)
  ) %>%
  # Fill with the same value
  tidyr::fill(prev_g_key, .direction = "down") %>%
  dplyr::mutate(
    # A group different from the previous one = immigration
    # (going out and back to the SAME group stays "group")
    status = dplyr::if_else(
      status == "group" & !is.na(prev_g_key) & g_key != prev_g_key,
      "immigration", status
    ),
    place = dplyr::if_else(status %in% c("group", "immigration"), g_key, place)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(GLT, Tattoo, SexOK, BirthOK, IdadeOK, UMMPs, Group, Region,
                DateObs, ObsOrder, status, place, g_key, transloc, File,
                n_groups_day, n_sat_day, conflict)


### Individuals with several places on the same date
# 1 row = 1 individual observed twice a day with a conflict 
doublons = obs %>%
  dplyr::filter(!is.na(conflict)) %>%
  dplyr::group_by(GLT, DateObs, conflict) %>%
  dplyr::summarise(
    groups = paste(unique(na.omit(g_key)), collapse = " & "), # group(s) seen that day
    n_records = dplyr::n(),
    .groups = "drop"
  )
# Diagnostic
doublons %>% dplyr::count(conflict)

# Days when an individual has several places on the same date (used to flag events)
conflict_days = obs %>%
  dplyr::filter(!is.na(conflict)) %>%
  dplyr::distinct(GLT, DateObs, conflict)

# Fixed characteristics of each individual (kept out of the stays table)
individuals = obs %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(
    dplyr::across(c(Tattoo, SexOK, BirthOK), ~ .x[!is.na(.x)][1]),
    .groups = "drop"
  )

#### Disappearances ----
# Individual whose LAST sighting is in a group that was surveyed afterwards, without it 
# i.e., the group was monitored, the individual was not seen in it
# NB: a disappearance is an absence (death OR emigration), not an observation
# This does not work with a satellite animal (we need a revisit to assess the individual has disappeared)

# Dates where groups were surveyed
group_dates = obs_raw %>%
  dplyr::filter(!is.na(Group)) %>%
  dplyr::distinct(g_key = paste(UMMPs, Group, sep = " | "), survey_date = DateObs)

# Last observations of each individual (in a group)
last_obs = obs %>%
  # Contact days are not used to decide where an individual was last seen
  dplyr::filter(status != "contact") %>%
  dplyr::group_by(GLT) %>%
  dplyr::arrange(DateObs, ObsOrder, .by_group = TRUE) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(status %in% c("group", "immigration")) # to keep individuals in groups

# Disappearance tables
disappearance = last_obs %>%
  dplyr::select(GLT, g_key, 
                last_seen = DateObs) %>% # Rename DateObs -> last observation
  # JOIN ALL posterior recordings of the group
  dplyr::inner_join(
    group_dates,
    by = dplyr::join_by(g_key, last_seen < survey_date),
    relationship = "many-to-many"
  ) %>%
  dplyr::group_by(GLT, g_key, last_seen) %>%
  # Compute the first observation of the group without the GLT (date_missing) and the number of posterior surveys without the individual
  dplyr::summarise(
    date_missing = min(survey_date), # first survey of the group without the individual
    n_surveys_missed = dplyr::n(), # how many later surveys without it
    .groups = "drop"
  )

# 1 row = 1 individual missing from a monitored group after its last sighting
disappear_events = disappearance %>%
  # Join GLT informations
  dplyr::left_join(
    last_obs %>% dplyr::select(GLT, UMMPs, Group, IdadeOK), # IdadeOK = stage at the last sighting
    by = "GLT"
  ) %>%
  dplyr::left_join(individuals, by = "GLT") %>%
  dplyr::mutate(
    days_missing = as.numeric(difftime(date_missing, last_seen, units = "days")),
    Year = lubridate::year(date_missing)
  ) %>%
  dplyr::select(GLT, Tattoo, SexOK, BirthOK, IdadeOK, UMMPs, Group,
                last_seen, date_missing, n_surveys_missed, days_missing, Year)


#### Individual history ----
# 1 row = 1 observation of 1 individual with its status: group / immigration / satellite / dead
# A CONTACT (alone AND in a group the same day) is considered a satellite observation
# i.e., the individual is floating and approaching groups. It is kept as "satellite" with contact = TRUE
# On a given date, satellite rows come first, then group rows.

# Compute the contact column (TRUE if contact, FALSE otherwise)
# Replace contact in status with satellite
history = obs %>%
  dplyr::mutate(
    contact = status == "contact",
    status = dplyr::if_else(contact, "satellite", status),
    place = dplyr::if_else(status == "satellite", "satellite", place)
  ) %>%
  dplyr::arrange(GLT, DateObs, !contact, ObsOrder)
history %>% dplyr::count(status, contact)


#### Stays ----
# HOW TO READ THE STAYS TABLE
# 1 row = 1 uninterrupted stay of 1 individual, i.e., it remained in the same place either as a group member (natal group or immigrant), a floater, or dead
# IF IN A GROUP: UMMP, Group, Region, place
# TIMING: first_seen, last_seen, n_obs, dates and number of observations of the stay
# AGE: age_first, age_last, stage at the first and at the last observation of the stay
# transloc = TRUE if the individual was translocated during the stay (Disp = 2)
# cum_tl = number of translocated stays so far (increasing = translocation)
# If observed both alone and in a group on the same date, priority is given to satellite.
# n_contact = number of satellite observations that were also in a group the same day.

stays = history %>%
  dplyr::group_by(GLT, DateObs) %>%
  dplyr::mutate(has_contact = any(contact, na.rm = TRUE)) %>%
  # On contact days, keep satellite observations and remove group observations
  dplyr::filter(!(has_contact & status %in% c("group", "immigration"))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(GLT) %>%
  # New stay id each time the status or place changes
  dplyr::mutate(stay = dplyr::consecutive_id(status, place))

# Aggregate by period (stay)
stays = stays %>%
  dplyr::group_by(GLT, stay) %>%
  dplyr::summarise(
    # Status during the stay
    status = dplyr::first(status),
    # Location
    UMMP = dplyr::first(UMMPs), Group = dplyr::first(Group), Region = dplyr::first(Region),
    # Timing
    first_seen = min(DateObs), last_seen = max(DateObs), n_obs = dplyr::n(),
    # Number of times the GLT is approaching groups
    n_contact = sum(contact),
    # Age
    age_first = dplyr::first(IdadeOK), age_last = dplyr::last(IdadeOK),
    transloc = any(transloc),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(UMMP, Group, Region),
      ~ dplyr::if_else(status %in% c("group", "immigration"), .x, NA_character_)
    )
  ) %>%
  dplyr::group_by(GLT) %>%
  dplyr::mutate(cum_tl = cumsum(transloc)) %>%
  dplyr::ungroup()

stays %>% dplyr::count(status)
# Individuals recorded in two groups at the same time
stays %>% dplyr::group_by(GLT) %>% dplyr::count(first_seen) %>% dplyr::filter(n>1) %>% print(n=30)



## AURORE CODE -----
### SUCCESSFUL IMMIGRATION ------
# Here, we identify individuals located in at least two groups, i.e., we remove individuals leaving a group and not resighted later on
# WARNING: it does not mean it's dispersal: it can be prospection (we do not know whether it is followed with REPRODUCTION)

#### Filter the data -----

# Filter out "bad" individuals, dead individuals, and individuals not observed at a given time (disp == 1)
data.filter = data.clean.final %>% 
  # Disp = 1 means the individual was not observed at that time (i.e., he left the group)
  dplyr::filter(!GLT %in% bad & Disp!=1 & Death==0) %>% 
  dplyr::arrange(DateObs, GLT) %>% 
  dplyr::group_by(GLT) 

# Get the name of all individuals observed in more than one group
dispind = data.filter %>%
  dplyr::group_by(GLT) %>%
  dplyr::summarise(N = dplyr::n_distinct(Group)) %>% 
  dplyr::filter(N >1)

# Create a data frame with GLT, origin-destination
# Warning: the table may keep individuals who both PROSPECT (i.e., come back to native group) AND DISPERSE (i.e., reproduce and stay in another group)
data.disp = data.filter %>% 
  dplyr::filter(GLT %in% dispind$GLT) %>% # Select dispersing individuals
  dplyr::arrange(GLT,DateObs) %>% # Arrange by GLT and date of observation
  dplyr::mutate(FromGroup=dplyr::lag(Group), # Previous group
                ToGroup=Group, # Current group
                FromRegion=dplyr::lag(Region),
                ToRegion=Region,
                FromUMMP=dplyr::lag(UMMPs),
                ToUMMP=UMMPs) %>% 
  # Identify translocated individuals: individuals who go from 0 to 2
  dplyr::mutate(Transloc=ifelse(dplyr::lag(Disp==2) & 
                                  Disp==0 & 
                                  dplyr::lag(GLT)==GLT,
                                1,0)) %>% 
  dplyr::filter(ObsOrder > 1) %>% # remove first observation
  
  dplyr::mutate(Change=ifelse(ToGroup != FromGroup, 1, 0)) %>% 
  
  # Keep individuals who changed their group and dispersal events ONLY (remove translocated ones)
  dplyr::filter(Change > 0 & Transloc == 0) %>% 
  dplyr::select(Year,DateObs,GLT,Tattoo,SexOK,BirthOK,IdadeOK,
                FromGroup,ToGroup,FromRegion,ToRegion,FromUMMP,ToUMMP) %>% 
  dplyr::ungroup() %>%
  
  # Dispersal id
  dplyr::mutate(disp_id = paste0("disp", row_number()))

#### Summary statistics -----
##### 1. Number of distinct GLTs -----
data.disp %>%
  dplyr::distinct(GLT) %>%
  nrow()

##### 2. Monthly observations ----
data.disp %>%
  dplyr::mutate(Month = lubridate::month(DateObs)) %>% 
  dplyr::count(Month)

dispersal_months = data.disp %>%
  dplyr::mutate(
    Month = lubridate::month(DateObs)
  ) %>%
  dplyr::count(Month, name = "n_dispersal") %>%
  tidyr::complete(
    Month = 1:12,
    fill = list(n_dispersal = 0)
  ) %>%
  dplyr::mutate(
    Month = factor(
      Month,
      levels = 1:12,
      labels = month.abb # Month abbreviations
    )
  )

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_success_immig_byMonth.png"),
    width = 1500, height = 1200, res = 300, type="cairo")
ggplot2::ggplot(
  dispersal_months,
  ggplot2::aes(x = Month, y = n_dispersal)
) +
  ggplot2::geom_col(
    fill = "#e0923d",
    colour = "black",
    width = 0.5
  ) +
  ggplot2::labs(
    title = "Timing of immigration into a group",
    x = "Month",
    y = "Number of observations"
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks()
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 12
    ),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

##### 3. Annual observations -----
# time range
paste(
  min(data.disp$Year),
  "to",
  max(data.disp$Year)
)

# annual observations
data.disp %>%
  dplyr::count(Year, name = "Observations") %>%
  dplyr::arrange(Year) %>% 
  print(n=31)

dispersal_year = data.disp %>%
  dplyr::count(Year, name = "Observations") %>%
  dplyr::arrange(Year)

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_success_immig_byYear.png"),
    width = 1800, height = 1200, res = 300, type="cairo")
ggplot2::ggplot(
  dispersal_year,
  ggplot2::aes(x = factor(Year), y = Observations)
) +
  ggplot2::geom_col(
    fill = "#e0923d",
    colour = "black",
    width = 0.5
  ) +
  ggplot2::labs(
    title = "Annual observations of successful immigration into a group",
    x = "Year",
    y = "Number of observations"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       size=12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

##### 4. Sex ratio ------
data.disp %>%
  dplyr::count(SexOK) %>%
  dplyr::mutate(Proportion = n / sum(n))

dispersal_sex = data.disp %>%
  dplyr::filter(!is.na(SexOK)) %>%
  dplyr::count(SexOK, name = "Frequency") %>%
  dplyr::mutate(
    Proportion = Frequency / sum(Frequency),
    Percentage = Proportion * 100
  )

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_success_immig_bySex.png"),
    width = 1200, height = 1000, res = 300, type="cairo")
ggplot2::ggplot(
  dispersal_sex,
  ggplot2::aes(x = SexOK, y = Percentage)
) +
  ggplot2::geom_col(
    fill = "#e0923d",
    colour = "black",
    width = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(Percentage, 1), "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  ggplot2::labs(
    title = "Sex ratio of immigrating individuals",
    x = "Sex",
    y = "Percentage of observations"
  ) +
  ggplot2::theme_classic() +
  ggplot2::expand_limits(y = max(dispersal_sex$Percentage) * 1.12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       size = 12),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

##### 5. By stage ----
data.disp %>%
  dplyr::count(IdadeOK, name = "Frequency") %>%
  dplyr::mutate(Proportion = Frequency / sum(Frequency)) %>% 
  dplyr::arrange(dplyr::desc(Frequency))

dispersal_age = data.disp %>%
  dplyr::filter(!is.na(IdadeOK)) %>%
  dplyr::mutate(IdadeOK = dplyr::case_when(
    IdadeOK == "AD" ~ "Adult",
    IdadeOK == "SA" ~ "Subadult",
    IdadeOK == "JU" ~ "Juvenile",
    IdadeOK == "IN" ~ "Infant"
  )) %>% 
  dplyr::count(IdadeOK, name = "Frequency") %>%
  dplyr::mutate(
    Proportion = Frequency / sum(Frequency),
    Percentage = Proportion * 100
  ) %>%
  dplyr::arrange(dplyr::desc(Frequency))

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_success_immig_byStage.png"),
    width = 1600, height = 1000, res = 300, type="cairo")
ggplot2::ggplot(
  dispersal_age,
  ggplot2::aes(
    x = reorder(IdadeOK, Frequency),
    y = Frequency
  )
) +
  ggplot2::geom_col(
    fill = "#e0923d",
    colour = "black",
    width = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = Frequency),
    hjust = -0.2,
    size = 3.5
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Age distribution of immigrating individuals",
    x = "Age category",
    y = "Number of observations"
  ) +
  ggplot2::theme_classic() +
  ggplot2::expand_limits(y = max(dispersal_age$Frequency) * 1.12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       size = 12),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

##### 6. Number of groups GLTs departed from or immigrated to -----
data.disp %>%
  dplyr::distinct(FromGroup) %>%
  nrow()
data.disp %>%
  dplyr::distinct(ToGroup) %>%
  nrow()

##### 7. To-From associations (Groups, Regions, UMMPs) -----
# Groups
data.disp %>%
  dplyr::count(FromGroup, ToGroup) %>% 
  dplyr::arrange(dplyr::desc(n))

transitions_group = data.disp %>%
  dplyr::filter(!is.na(FromGroup), !is.na(ToGroup)) %>%
  dplyr::count(FromGroup, ToGroup, name = "Frequency") %>%
  dplyr::mutate(
    Association = paste(FromGroup, "→", ToGroup)
  ) %>%
  dplyr::slice_max(
    order_by = Frequency,
    # Number of top associations to keep
    n = 40,
    with_ties = FALSE
  )

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_success_immig_byGroup.png"),
    width = 1800, height = 1800, res = 300, type="cairo")
ggplot2::ggplot(
  transitions_group,
  ggplot2::aes(
    x = reorder(Association, Frequency),
    y = Frequency
  )
) +
  ggplot2::geom_col(fill = "#e0923d", 
                    colour = "black",
                    width = 0.75) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Most frequent group transitions (n=40)",
    x = "Origin → destination group",
    y = "Number of dispersal observations"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       size = 12),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

# Region
data.disp %>%
  dplyr::count(FromRegion, ToRegion)  %>% 
  dplyr::arrange(dplyr::desc(n))

transitions_region = data.disp %>%
  dplyr::filter(!is.na(FromRegion), !is.na(ToRegion)) %>%
  dplyr::count(FromRegion, ToRegion, name = "Frequency") %>%
  dplyr::mutate(
    Association = paste(FromRegion, "→", ToRegion)
  ) %>%
  dplyr::slice_max(
    order_by = Frequency,
    n = 15,
    with_ties = FALSE
  )

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_success_immig_byRegion.png"),
    width = 2000, height = 1600, res = 300, type="cairo")
ggplot2::ggplot(
  transitions_region,
  ggplot2::aes(
    x = reorder(Association, Frequency),
    y = Frequency
  )
) +
  ggplot2::geom_col(fill = "#e0923d", 
                    colour = "black",
                    width = 0.75) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Most frequent region immigrations (n=15)",
    x = "Origin → destination region",
    y = "Number of dispersal observations"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 12),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

# UMMP
data.disp %>%
  dplyr::count(FromUMMP, ToUMMP)  %>% 
  dplyr::arrange(dplyr::desc(n))

transitions_UMMP = data.disp %>%
  dplyr::filter(
    !is.na(FromUMMP),
    !is.na(ToUMMP)
  ) %>%
  dplyr::mutate(
    Dispersal_type = dplyr::if_else(
      FromUMMP == ToUMMP,
      "Intra-patch",
      "Inter-patch"
    )
  ) %>%
  dplyr::count(
    FromUMMP,
    ToUMMP,
    Dispersal_type,
    name = "Frequency"
  ) %>%
  dplyr::mutate(
    Association = paste(FromUMMP, "→", ToUMMP)
  )

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_success_immig_byUMMP.png"),
    width = 2200, height = 1800, res = 300, type="cairo")
ggplot2::ggplot(
  transitions_UMMP,
  ggplot2::aes(
    x = reorder(Association, Frequency),
    y = Frequency,
    fill = Dispersal_type
  )
) +
  ggplot2::geom_col(
    colour = "black",
    width = 0.75
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(
    values = c(
      "Intra-patch" = "#fcacef",
      "Inter-patch" = "#56B4E9"
    )
  ) +
  ggplot2::labs(
    title = "UMMP transitions",
    x = "Origin → destination UMMP",
    y = "Number of dispersal observations",
    fill = "Dispersal type"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       size = 12),
    axis.text = ggplot2::element_text(size = 10),
    legend.position = "bottom"
  )
dev.off()

#### Join spatial information ------
# Keep only dispersal between known locations
# Create a vector of all unique group names from FromGroup and ToGroup
valid_groups = data.disp %>%
  dplyr::select(FromGroup, ToGroup) %>%
  unlist() %>%
  unique()

# Filter to keep only rows where both FromGroup and ToGroup are known locations
data.disp.filter.grp = data.disp %>%
  dplyr::filter(
    FromGroup %in% regions_csv$Abreviation,
    ToGroup %in% regions_csv$Abreviation
  )

## Based on group location (regions file)
# Reformat regions
grp.loc = regions_csv %>% 
  dplyr::rename(Long=CENTROIDE_X_UTM_SAD69_23S,
                Lat=CENTROIDE_Y_UTM_SAD69_23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter_all(any_vars(!is.na(.)))

# Join group location
data.disp.from = data.disp.filter.grp %>% 
  dplyr::left_join(grp.loc %>% dplyr::select(c(Group,Long,Lat)), by=c("FromGroup"="Group"), multiple="first")
data.disp.to = data.disp.filter.grp %>% 
  dplyr::left_join(grp.loc %>% dplyr::select(c(Group,Long,Lat)), by=c("ToGroup"="Group"), multiple="first")

# Create origins and destination sf objects
data.disp.from.sf = data.disp.from %>% 
  sf::st_as_sf(coords=c("Long","Lat"), crs=sf::st_crs(regions))
data.disp.to.sf = data.disp.to %>% 
  sf::st_as_sf(coords=c("Long","Lat"), crs=sf::st_crs(regions))

# Join patch id
# This function joins patches id depending on the year
# Two methods: intersection and if still NA, take closest patch
join_patch_by_year = function(df, patches, patch_col = "patch_id") {
  
  dplyr::bind_rows(
    lapply(split(df, df$Year), function(d) {
      
      y = unique(d$Year)
      patches_y = patches[[as.character(y)]]
      
      patches_y = patches_y %>%
        dplyr::rename(patch_id = all_of(patch_col)) %>%
        dplyr::select(patch_id)
      
      # First: intersection join
      d_join = sf::st_join(d, patches_y, join = sf::st_intersects, left = TRUE)
      
      # Identify NAs
      na_idx = which(is.na(d_join$patch_id))
      
      if (length(na_idx) > 0) {
        
        # Compute nearest patch for NA rows
        nearest_idx = sf::st_nearest_feature(d_join[na_idx, ], patches_y)
        d_join$patch_id[na_idx] = patches_y$patch_id[nearest_idx]
      }
      
      return(d_join)
    })
  )
}

# Apply to origins
data.disp.from.sf = join_patch_by_year(
  data.disp.from.sf,
  patches,
  patch_col = "patch_id") %>%
  dplyr::rename(patch_from = patch_id)
# Apply to destinations
data.disp.to.sf = join_patch_by_year(
  data.disp.to.sf,
  patches,
  patch_col = "patch_id") %>%
  dplyr::rename(patch_to = patch_id)

# Flag NAs
sum(is.na(data.disp.from.sf$patch_from))
sum(is.na(data.disp.to.sf$patch_to))

##### To lines -----
# Create LINESTRING geometries
disp.lines = rbind(data.disp.from, data.disp.to) # We duplicate rows to create dispersal lines
disp.lines = disp.lines %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), na.fail = FALSE, crs=crs(regions)) %>% 
  dplyr::group_by(disp_id) %>% 
  dplyr::summarize() %>%
  dplyr::filter(sf::st_geometry_type(.) == "MULTIPOINT") %>%
  sf::st_cast("LINESTRING")

# Plot the lines
plot(raster_lulc_2024, col=c("#32a65e", "#ad975a", "#519799", "#FFFFB2", "#0000FF", "#d4271e"))
plot(sf::st_geometry(regions), col="magenta", add=TRUE)
plot(sf::st_geometry(disp.lines), col = "deeppink", lwd = 1.5, add=TRUE)

##### Join to lines -----
# Recombine
disp.final = disp.lines %>%
  dplyr::left_join(data.disp.from.sf %>% sf::st_drop_geometry() %>% dplyr::select(disp_id, patch_from), by="disp_id") %>%
  dplyr::left_join(data.disp.to.sf %>% sf::st_drop_geometry() %>% dplyr::select(disp_id, patch_to), by="disp_id")

# Flag inter- and intra-patch movement
# If the origin and destination are the same patches -> 1
disp.final = disp.final %>%
  dplyr::mutate(
    intra_patch = ifelse(patch_from == patch_to, 1, 0)
  )
# Intra-patch dispersal: 1
# Inter-patch dispersal: 0
table(disp.final$intra_patch)

# Join GLT information
disp.final.glt = disp.final %>% 
  dplyr::left_join(data.disp.filter.grp %>% 
                     dplyr::select(c(Year,DateObs,GLT,Tattoo,disp_id,SexOK,IdadeOK)), 
                   by=c("disp_id"))

# Statistics
disp.final.glt %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(patch_from, patch_to) %>% 
  dplyr::count() %>%
  dplyr::arrange(dplyr::desc(n)) %>% 
  print(n=80)

### ALL EMIGRATIONS EVENTS ----
# Here, we identify individuals leaving groups, irrespective of whether they were located after in another group (i.e., emigration events)

#### Emigration events from individual histories -----
hist = data.clean.final %>%
  # Remove bad individuals and dead individuals
  dplyr::filter(!is.na(GLT), !GLT %in% bad, Death %in% 0) %>%
  dplyr::mutate(
    DateObs = as.Date(DateObs),
    state = dplyr::case_when(
      Disp %in% 1 ~ "solitary", # seen outside any group
      Disp %in% 2 ~ "translocated",
      Disp %in% 0 & !is.na(Group) ~ "group",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::arrange(GLT, DateObs, ObsOrder) %>%
  dplyr::group_by(GLT) %>%
  dplyr::mutate(
    # Group information, only on rows where the individual is in a group
    g_date = dplyr::if_else(state == "group", DateObs, as.Date(NA)),
    g_group = dplyr::if_else(state == "group", as.character(Group), NA_character_),
    g_UMMP = dplyr::if_else(state == "group", as.character(UMMPs), NA_character_),
    g_region = dplyr::if_else(state == "group", as.character(Region), NA_character_),
    # Previous row
    prev_state = dplyr::lag(state),
    prev_date = dplyr::lag(DateObs),
    prev_group = dplyr::lag(g_group),
    prev_UMMP = dplyr::lag(g_UMMP),
    prev_region = dplyr::lag(g_region),
    # Last group seen before the row (filled down), next group after it (filled up)
    last_date = g_date, last_group = g_group, last_UMMP = g_UMMP, last_region = g_region,
    next_date = g_date, next_group = g_group, next_UMMP = g_UMMP, next_region = g_region
  ) %>%
  tidyr::fill(dplyr::starts_with("last_"), .direction = "down") %>%
  tidyr::fill(dplyr::starts_with("next_"), .direction = "up") %>%
  dplyr::ungroup()

# Group -> solitary (first solitary sighting after a group sighting)
ev_solitary = hist %>%
  dplyr::filter(state == "solitary", prev_state %in% "group") %>%
  dplyr::transmute(
    GLT, Tattoo, SexOK, BirthOK, IdadeOK,
    type = "to solitary",
    DateLastSeen = last_date, # last sighting in the group
    DateOut = DateObs, # first sighting outside the group
    FromGroup = last_group, FromUMMP = last_UMMP, FromRegion = last_region,
    ToGroup = next_group, ToUMMP = next_UMMP, ToRegion = next_region,
    DateNextGroup = next_date # next sighting in any group (NA = never)
  )

# Group -> another group without a solitary sighting in between
# (translocations are excluded: their previous state is "translocated")
ev_transfer = hist %>%
  dplyr::filter(
    state == "group", prev_state %in% "group",
    g_group != prev_group | g_UMMP != prev_UMMP
  ) %>%
  dplyr::transmute(
    GLT, Tattoo, SexOK, BirthOK, IdadeOK,
    type = "direct transfer",
    DateLastSeen = prev_date,
    DateOut = DateObs,
    FromGroup = prev_group, FromUMMP = prev_UMMP, FromRegion = prev_region,
    ToGroup = g_group, ToUMMP = g_UMMP, ToRegion = g_region,
    DateNextGroup = DateObs
  )

emigr_events = dplyr::bind_rows(ev_solitary, ev_transfer) %>%
  dplyr::arrange(GLT, DateOut) %>%
  dplyr::mutate(
    UMMPs = FromUMMP, # origin UMMP (used by the downstream code)
    Year = lubridate::year(DateOut),
    days_to_next_group = as.integer(DateNextGroup - DateOut),
    fate = dplyr::case_when(
      type == "direct transfer" ~ "joined another group",
      is.na(ToGroup) ~ "not seen in a group afterwards",
      ToGroup == FromGroup & ToUMMP == FromUMMP ~ "returned to origin group",
      TRUE ~ "joined another group"
    ),
    emig_id = paste0("emig", dplyr::row_number())
  )

##### Checks -----
emigr_events %>% dplyr::count(type, fate)

#### Summary statistics -----
##### 1. Number of distinct GLTs -----
emigr %>%
  dplyr::distinct(GLT) %>%
  nrow()

##### 2. Monthly observations ----
emigr %>%
  dplyr::mutate(Month = lubridate::month(DateObs)) %>% 
  dplyr::count(Month)

dispersal_months = emigr %>%
  dplyr::mutate(
    Month = lubridate::month(DateObs)
  ) %>%
  dplyr::count(Month, name = "n_dispersal") %>%
  tidyr::complete(
    Month = 1:12,
    fill = list(n_dispersal = 0)
  ) %>%
  dplyr::mutate(
    Month = factor(
      Month,
      levels = 1:12,
      labels = month.abb # Month abbreviations
    )
  )

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_emig_byMonth.png"),
    width = 1500, height = 1200, res = 300, type="cairo")
ggplot2::ggplot(
  dispersal_months,
  ggplot2::aes(x = Month, y = n_dispersal)
) +
  ggplot2::geom_col(
    fill = "#e0923d",
    colour = "black",
    width = 0.5
  ) +
  ggplot2::labs(
    title = "Timing of emigration",
    x = "Month",
    y = "Number of observations"
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks()
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 12
    ),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

##### 3. Annual observations -----
# time range
paste(
  min(emigr$Year),
  "to",
  max(emigr$Year)
)

# annual observations
emigr %>%
  dplyr::count(Year, name = "Observations") %>%
  dplyr::arrange(Year)

dispersal_year = emigr %>%
  dplyr::count(Year, name = "Observations") %>%
  dplyr::arrange(Year)

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_emig_byYear.png"),
    width = 1800, height = 1200, res = 300, type="cairo")
ggplot2::ggplot(
  dispersal_year,
  ggplot2::aes(x = factor(Year), y = Observations)
) +
  ggplot2::geom_col(
    fill = "#e0923d",
    colour = "black",
    width = 0.5
  ) +
  ggplot2::labs(
    title = "Annual observations of emigration events",
    x = "Year",
    y = "Number of observations"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       size=12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

##### 4. Sex ratio ------
emigr %>%
  dplyr::count(SexOK) %>%
  dplyr::mutate(Proportion = round(n / sum(n),3))

dispersal_sex = emigr %>%
  dplyr::filter(!is.na(SexOK)) %>%
  dplyr::count(SexOK, name = "Frequency") %>%
  dplyr::mutate(
    Proportion = Frequency / sum(Frequency),
    Percentage = Proportion * 100
  )

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_emig_bySex.png"),
    width = 1200, height = 1000, res = 300, type="cairo")
ggplot2::ggplot(
  dispersal_sex,
  ggplot2::aes(x = SexOK, y = Percentage)
) +
  ggplot2::geom_col(
    fill = "#e0923d",
    colour = "black",
    width = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(Percentage, 1), "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  ggplot2::labs(
    title = "Sex ratio of emigrating individuals",
    x = "Sex",
    y = "Percentage of observations"
  ) +
  ggplot2::theme_classic() +
  ggplot2::expand_limits(y = max(dispersal_sex$Percentage) * 1.12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       size = 12),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

##### 5. By stage ----
emigr %>%
  # dplyr::filter(!is.na(IdadeOK)) %>%
  dplyr::count(IdadeOK, name = "Frequency") %>%
  dplyr::mutate(Proportion = round(Frequency / sum(Frequency),2)) %>% 
  dplyr::arrange(dplyr::desc(Frequency))

dispersal_age = emigr %>%
  dplyr::filter(!is.na(IdadeOK)) %>%
  dplyr::mutate(IdadeOK = dplyr::case_when(
    IdadeOK == "AD" ~ "Adult",
    IdadeOK == "SA" ~ "Subadult",
    IdadeOK == "JU" ~ "Juvenile",
    IdadeOK == "IN" ~ "Infant"
  )) %>% 
  dplyr::count(IdadeOK, name = "Frequency") %>%
  dplyr::mutate(
    Proportion = Frequency / sum(Frequency),
    Percentage = Proportion * 100
  ) %>%
  dplyr::arrange(dplyr::desc(Frequency))

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_emig_byStage.png"),
    width = 1600, height = 1000, res = 300, type="cairo")
ggplot2::ggplot(
  dispersal_age,
  ggplot2::aes(
    x = reorder(IdadeOK, Frequency),
    y = Frequency
  )
) +
  ggplot2::geom_col(
    fill = "#e0923d",
    colour = "black",
    width = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = Frequency),
    hjust = -0.2,
    size = 3.5
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Age distribution of emigrating individuals",
    x = "Age category",
    y = "Number of observations"
  ) +
  ggplot2::theme_classic() +
  ggplot2::expand_limits(y = max(dispersal_age$Frequency) * 1.12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       size = 12),
    axis.text = ggplot2::element_text(size = 10)
  )
dev.off()

##### 5b. Emigration probability stage ----
# We join population information to assess the proportion of individuals emigrating
# We use census data (Holst et al. 2006, Ruiz-Miranda et al. 2019, Dietz et al. 2024)
# Census: remove the prefix and reshape to long format (1 row = UMMP x census year)
census_long = real_census_data %>%
  dplyr::rename_with(
    ~ sub("^n_glt_", "", .x),
    dplyr::starts_with("n_glt_")
  ) %>%
  tidyr::pivot_longer(
    cols = -c(UMMPs, FragName),
    names_to = "Year",
    names_transform = list(Year = as.integer),
    values_to = "pop"
  ) %>%
  dplyr::mutate(
    # Accent-free key to match UMMP names (e.g., "Imbaú I" vs "Imbau I")
    UMMP_key = stringi::stri_trans_general(UMMPs, "Latin-ASCII")
  ) %>%
  dplyr::select(UMMP_key, Year, pop)

# Emigrants per UMMP x census period x stage
emigr_prob = emigr %>%
  dplyr::filter(
    !is.na(UMMPs),
    Year %in% c(2004, 2005, 2006, 2007, 2012, 2013, 2014, 2015)
  ) %>%
  dplyr::mutate(
    # Group the years around each census
    Year = dplyr::case_when(
      Year %in% c(2004, 2005, 2006, 2007) ~ 2005L,
      Year %in% c(2012, 2013, 2014, 2015) ~ 2013L,
      TRUE ~ NA_integer_
    ),
    # Infants (IN) and juveniles (JU) are considered as the same stage
    IdadeOK = dplyr::case_when(
      IdadeOK %in% c("IN", "JU") ~ "IN-JU",
      TRUE ~ as.character(IdadeOK)
    )
  ) %>%
  dplyr::group_by(UMMPs, Year, IdadeOK) %>%
  dplyr::summarise(
    n_emig = dplyr::n(),
    .groups = "drop"
  ) %>%
  # Stages without any emigrant are zeros, not missing rows
  tidyr::complete(
    tidyr::nesting(UMMPs, Year), IdadeOK,
    fill = list(n_emig = 0L)
  ) %>%
  dplyr::mutate(
    UMMP_key = stringi::stri_trans_general(UMMPs, "Latin-ASCII")
  ) %>%
  dplyr::left_join(
    UMMP_crosswalk,
    by = "UMMPs"
  ) %>%
  dplyr::left_join(
    census_long,
    by = c("UMMP_key", "Year"),
    relationship = "many-to-one"
  ) %>%
  dplyr::mutate(
    # Emigration probability = emigrants / population
    emigr_prob = dplyr::if_else(!is.na(pop) & pop > 0, round(n_emig / pop,3), NA_real_)
  ) %>% 
  dplyr::filter(!is.na(emigr_prob),
                !is.na(IdadeOK)) %>% 
  dplyr::select(-UMMP_key)

# Mean emigration probability per stage (across census years and UMMPs)
emigr_prob_stage = emigr_prob %>%
  dplyr::filter(UMMPs %in% c("Aldeia I", "Rio Vermelho"),
                n_emig > 0) %>% 
  dplyr::group_by(IdadeOK) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),# UMMP x year combinations
    mean_emigr_prob = mean(emigr_prob),
    sd_emigr_prob = stats::sd(emigr_prob),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dplyr::across(c(mean_emigr_prob, sd_emigr_prob), ~ round(.x, 3))
  )


### Export ---------
#### Emigration (table) -----
write.xlsx(as.data.frame(emigr),
           file=here::here("outputs","data","dispersal","All_dispersal_events.xlsx"),
           sheetName = "Sheet1",
           col.names=T,
           row.names=F)


#### Immigration (table) -----
# Export the table BEFORE spatial filter (all immigration events)
write.xlsx(as.data.frame(data.disp),
           file=here::here("outputs","data","dispersal","Successful_dispersal.xlsx"),
           sheetName = "Sheet1",
           col.names=T,
           row.names=F)


#### Immigration (dispersal lines) -----
sf::st_write(
  disp.final.glt,
  dsn = here::here("outputs", "data", "dispersal", "disp_lines.gpkg"),
  layer = "disp_lines",
  delete_dsn = TRUE
)
