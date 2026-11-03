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

### SUCCESSFUL IMMIGRATION ------
# Here, we identify individuals located in at least two groups, i.e., we remove individuals leaving a group and not resighted later on
# WARNING: it does not mean it's dispersal: it can be prospection (we do not know whether it is followed with REPRODUCTION)

#### Filter the data -----

# Filter out "bad" individuals, dead individuals, and individuals not observed at a given time (disp == 1)
data.filter = data.clean.final %>% 
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
  dplyr::mutate(FromGroup=dplyr::lag(Group),
                ToGroup=Group,
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
  
  dplyr::mutate(Change=ifelse(ToGroup != FromGroup, 1,0)) %>% 
  
  # Keep dispersal individuals ONLY (remove translocated ones)
  dplyr::filter(Change > 0 & Transloc == 0) %>% 
  dplyr::select(Year,DateObs,GLT,Tattoo,SexOK,BirthOK,IdadeOK,
                FromGroup,ToGroup,FromRegion,ToRegion,FromUMMP,ToUMMP) %>% 
  dplyr::ungroup() %>%
  
  # Dispersal id
  dplyr::mutate(disp_id = paste0("disp", row_number()))

#### Spatial filter ------
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

#### Summary statistics -----
##### 1. Number of distinct GLTs -----
data.disp.filter.grp %>%
  dplyr::distinct(GLT) %>%
  nrow()

##### 2. Monthly observations ----
data.disp.filter.grp %>%
  dplyr::mutate(Month = lubridate::month(DateObs)) %>% 
  dplyr::count(Month)

dispersal_months = data.disp.filter.grp %>%
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
  min(data.disp.filter.grp$Year),
  "to",
  max(data.disp.filter.grp$Year)
)

# annual observations
data.disp.filter.grp %>%
  dplyr::count(Year, name = "Observations") %>%
  dplyr::arrange(Year) %>% 
  print(n=31)

dispersal_year = data.disp.filter.grp %>%
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
data.disp.filter.grp %>%
  dplyr::count(SexOK) %>%
  dplyr::mutate(Proportion = n / sum(n))

dispersal_sex = data.disp.filter.grp %>%
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
data.disp.filter.grp %>%
  dplyr::count(IdadeOK, name = "Frequency") %>%
  dplyr::mutate(Proportion = Frequency / sum(Frequency)) %>% 
  dplyr::arrange(dplyr::desc(Frequency))

dispersal_age = data.disp.filter.grp %>%
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
data.disp.filter.grp %>%
  dplyr::distinct(FromGroup) %>%
  nrow()
data.disp.filter.grp %>%
  dplyr::distinct(ToGroup) %>%
  nrow()

##### 7. To-From associations (Groups, Regions, UMMPs) -----
# Groups
data.disp.filter.grp %>%
  dplyr::count(FromGroup, ToGroup) %>% 
  dplyr::arrange(dplyr::desc(n))

transitions_group = data.disp.filter.grp %>%
  dplyr::filter(!is.na(FromGroup), !is.na(ToGroup)) %>%
  dplyr::count(FromGroup, ToGroup, name = "Frequency") %>%
  dplyr::mutate(
    Association = paste(FromGroup, "→", ToGroup)
  ) %>%
  dplyr::slice_max(
    order_by = Frequency,
    n = 30,
    with_ties = FALSE
  )

# Export plot
png(here("outputs",
         "plot",
         "Axis2",
         "GLT_success_immig_byGroup.png"),
    width = 1800, height = 1600, res = 300, type="cairo")
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
    title = "Most frequent group transitions (n=30)",
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
data.disp.filter.grp %>%
  dplyr::count(FromRegion, ToRegion)  %>% 
  dplyr::arrange(dplyr::desc(n))

transitions_region = data.disp.filter.grp %>%
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
data.disp.filter.grp %>%
  dplyr::count(FromUMMP, ToUMMP)  %>% 
  dplyr::arrange(dplyr::desc(n))

transitions_UMMP = data.disp.filter.grp %>%
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

#### Filter the data -----
#filter data and add id count over time
data.filter = data.clean.final %>% 
  dplyr::filter(!GLT %in% bad & Disp!=1 & Death==0) %>% 
  dplyr::arrange(DateObs, GLT) %>% 
  group_by(GLT) 

disp<-NULL

gp<-sort(unique(data.filter$Group))

for (i in 1:length(gp)){
  
  singlegp<-data.filter %>% 
    dplyr::filter(Group==gp[i])
  
  dateobs<-sort(unique(singlegp$DateObs))
  
  if(length(dateobs)>1){
    for (j in 2:length(dateobs)){
      
      daygpbef<-singlegp %>% 
        dplyr::filter(DateObs==dateobs[j-1])
      
      daygp<-singlegp %>% 
        dplyr::filter(DateObs==dateobs[j])
      
      miss<-anti_join(daygpbef,daygp,by="GLT")
      
      if(nrow(miss)>0){
        disp<-rbind(disp,miss)
        
      }
      setTxtProgressBar(progress_bar(length(gp)),i)  
    }
  }
}

# To data frame
emigr = disp %>% 
  # Remove reintroductions
  dplyr::filter(Year > 1994) %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::slice(max(ObsOrder)) %>% 
  as.data.frame(.)

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
write.xlsx(as.data.frame(data.disp.filter.grp),
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
