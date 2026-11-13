#------------------------------------------------#
# Author: Romain Monassier
# Objective: Correct UMMPs in the long-term dataset
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)

### Import dataset -----
# GLT data
load(here::here("data", "glt", "APonchon", "data_clean_long_final.RData"))

# Group locations
regions = sf::st_read(here::here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
path_regions = here::here("data", "geo", "APonchon", "GLT", "RegionsName.csv")
readr::guess_encoding(path_regions) # to identify the encoding and preserve accents
regions_csv = readr::read_csv2(
  path_regions,
  locale = readr::locale(encoding = "UTF-8"),
  na = ""
)

### Fill missing UMMPs of data.clean.final -----

#### BEFORE
sum(is.na(data.clean.final$UMMPs))   # missing before
unique(data.clean.final$UMMPs)
dplyr::count(data.clean.final, UMMPs, sort = TRUE)

#### HOMOGENIZE THE NAME
norm_key = function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    stringr::str_replace_all("[._-]+", " ") %>%
    stringr::str_squish()
}

#### "NA" (character) should be real missing values
data.clean.final = data.clean.final %>%
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(c(UMMPs, Region),
                              ~ dplyr::na_if(dplyr::na_if(stringr::str_squish(as.character(.x)), "NA"), "")))

#### Lookup from the regions file (Farm2 -> UMMPs)
lookup_file = regions_csv %>%
  dplyr::filter(!is.na(Farm2), !is.na(UMMPs)) %>%
  dplyr::transmute(Region_key = norm_key(Farm2),
                   UMMPs_lookup = stringi::stri_trans_general(UMMPs, "Latin-ASCII"),
                   source = "file") %>%
  dplyr::distinct() %>%
  dplyr::group_by(Region_key) %>%
  dplyr::filter(dplyr::n() == 1) %>%
  dplyr::ungroup()

#### Manual rules (take priority over the file)
lookup_manual = dplyr::tribble(
  ~Region,                    ~UMMPs_lookup,
  "Afetiva",                  "Imbau II",
  "St. Professor",            "Imbau I",
  "Rio Vermelho",             "Rio Vermelho",
  "St. Joedi",                "Aldeia I",
  "Sitio Tertulio",           "Aldeia I",
  "Sitio 15 de Marco",        "Rio Vermelho",
  "Sitio 4 Irmaos",           "4_Irmaos",
  "Faz. Boa Esperanca",       "Faz. Boa Esperanza",
  "Sitio Recanto Preservar",  "Poco das Antas",
  "Faz. Vendaval",            "Faz. Boa Esperanza",
  "Faz. Sao Joao",            "Aldeia II",
  "Faz. Sao Joao 2",          "Faz. Boa Esperanza"
) %>%
  dplyr::transmute(Region_key = norm_key(Region), UMMPs_lookup, source = "manual")

region_ummp = dplyr::bind_rows(lookup_manual, lookup_file) %>%
  dplyr::distinct(Region_key, .keep_all = TRUE)   # manual first

#### Fill only the missing UMMPs
data.clean.final = data.clean.final %>%
  dplyr::mutate(Region_key = norm_key(Region)) %>%
  dplyr::left_join(region_ummp, by = "Region_key", relationship = "many-to-one") %>%
  dplyr::mutate(UMMPs = dplyr::coalesce(UMMPs, UMMPs_lookup)) %>%
  dplyr::select(-Region_key, -UMMPs_lookup, -source)

#### Checks
sum(is.na(data.clean.final$UMMPs))   # still missing
unique(data.clean.final$UMMPs)
dplyr::count(data.clean.final, UMMPs, sort = TRUE)

### Export ----
path = here::here("outputs", "data", "glt", "data_clean_long_final2.RData")
save(data.clean.final, file = path)
