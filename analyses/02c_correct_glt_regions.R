#------------------------------------------------#
# Author: Romain Monassier
# Objective: Correct regions shapefile (group locations)
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)

### Import dataset ------
## Regions
regions = sf::st_read(here("data", "geo", "APonchon", "GLT", "RegionsName.shp"))
regions_csv = readr::read_csv2(
  here::here("data", "geo", "APonchon", "GLT", "RegionsName.csv"),
  locale = readr::locale(encoding = "ISO-8859-1"),
  na = ""
)

### Correct shapefile ------
regions = regions %>% 
  dplyr::mutate(Farm2 = dplyr::case_when(
    Farm == "Faz. Boa Esperanca" & Abreviatio == "OL2" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Boa Esperanca" & Abreviatio == "AF8" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Cabana nova Conquista" & Abreviatio == "JG" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Sao Joao" & Abreviatio == "IR" ~ "Faz. Sao Joao 2",
    Farm == "Faz. Iguape" & Abreviatio == "SK" ~ "Faz. Igarape",
    Farm == "Faz.Afetiva" | Farm == "Fazenda Afetiva" ~ "Faz. Afetiva",
    Farm == "Faz. Sao Francisco" & Abreviatio == "KS" ~ "Faz. Igarape",
    Farm == "Rebio Uniao" & Abreviatio == "LB" ~ "Rebio Uniao Sul",
    TRUE ~ Farm
  ))

### Correct CSV ------
regions_csv = regions_csv %>% 
  dplyr::mutate(Farm2 = dplyr::case_when(
    Farm == "Faz. Boa Esperanca" & Abreviation == "OL2" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Boa Esperanca" & Abreviation == "AF8" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Cabana nova Conquista" & Abreviation == "JG" ~ "Fazenda Nova Esperança",
    Farm == "Faz. Sao Joao" & Abreviation == "IR" ~ "Faz. Sao Joao 2",
    Farm == "Faz. Iguape" & Abreviation == "SK" ~ "Faz. Igarape",
    Farm == "Faz.Afetiva" | Farm == "Fazenda Afetiva" ~ "Faz. Afetiva",
    Farm == "Faz. Sao Francisco" & Abreviation == "KS" ~ "Faz. Igarape",
    Farm == "Rebio Uniao" & Abreviation == "LB" ~ "Rebio Uniao Sul",
    TRUE ~ Farm
  ))


### Export ----------
base_path = here("data", "geo", "APonchon", "GLT")

# Export shapefile
sf::st_write(
  regions,
  file.path(base_path, "RegionsName.shp"),
  append = FALSE,
  delete_dsn = TRUE,
  quiet = TRUE
)

# Export CSV
readr::write_excel_csv2(
  regions_csv,
  file.path(base_path, "RegionsName.csv")
)
