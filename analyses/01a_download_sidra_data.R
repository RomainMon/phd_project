#------------------------------------------------#
# Author: Romain Monassier
# Objective: Dowloading data from the SIDRA (IBGE) website
#------------------------------------------------#


### Load packages ------
library(sidrar)
library(dplyr)
library(tidyr)
library(writexl)

# Define the city codes as a vector
# These codes represent the cities of Araruama, Cabo Frio, Cachoeiras de Macacu, Casimiro de Abreu, Macaé, Nova Friburgo, Rio Bonito, Rio das Ostras, São Pedro da Aldeia, Saquarema, Silva Jardim
city_codes = c(3300209, 3300704, 3300803, 3301306, 3302403, 
               3303401, 3304300, 3304524, 3305208, 3305505, 3305604)

### Get data from SIDRA for the specified cities ------
# The function get_sidra downloads data, with specified arguments (x = tabela, variable = name of the variable included, see info_sidra, geo = spatial unit, geo.filter = filter by Cities, Regions, etc., classific =  table’s classification)

#### Tabela 6753 ------
# # Get information about SIDRA tables
# info_sidra(6753, wb = TRUE)
# Tabela 6753 - 1
# Number and area of agricultural units by legal status of the land
tabela_6753_1 = get_sidra(
  x = 6753, 
  variable = c(183, 184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("c221"),
  format = 2
)
# Reshape table
tabela_6753_1 = tabela_6753_1 %>%
  dplyr::select(Município, Ano, Variável, 'Condição legal das terras', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Condição legal das terras'),
    values_from = Valor,
    names_sep = " - "
  )
# Tabela 6753 - 2
# Number and area of agricultural units, by producer relationship with the lands
tabela_6753_2 = get_sidra(
  x = 6753, 
  variable = c(183, 184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("c218"),
  format = 2
)
# Reshape table
tabela_6753_2 = tabela_6753_2 %>%
  dplyr::select(Município, Ano, Variável, 'Condição do produtor em relação às terras', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Condição do produtor em relação às terras'),
    values_from = Valor,
    names_sep = " - "
  )
# Merge table
tabela_6753 = dplyr::full_join(tabela_6753_1, tabela_6753_2)

#### Tabela 6754 ------
# # Get information about SIDRA tables
# info_sidra(6754, wb = TRUE)
# Number and area of agricultural units, by type of production
tabela_6754 = get_sidra(
  x = 6754, 
  variable = c(183, 184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12517"),
  format = 2
)
# Reshape table
tabela_6754 = tabela_6754 %>%
  dplyr::select(Município, Ano, Variável, 'Grupos de atividade econômica', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Grupos de atividade econômica'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6755 ------
# # Get information about SIDRA tables
# info_sidra(6755, wb = TRUE)
# Tabela 6755 - 1
# Number of agricultural exploitations owned by the producer, by level of education
tabela_6755_1 = get_sidra(
  x = 6755, 
  variable = c(9998),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C800"),
  format = 2
)
# Reshape table
tabela_6755_1 = tabela_6755_1 %>%
  dplyr::select(Município, Ano, Variável, 'Escolaridade do produtor', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Escolaridade do produtor'),
    values_from = Valor,
    names_sep = " - "
  )
# Tabela 6755 - 2
# Number of agricultural exploitations owned by the producer, by age category
tabela_6755_2 = get_sidra(
  x = 6755, 
  variable = c(9998),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12771"),
  format = 2
)
# Reshape table
tabela_6755_2 = tabela_6755_2 %>%
  dplyr::select(Município, Ano, Variável, 'Classe de idade do produtor', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Classe de idade do produtor'),
    values_from = Valor,
    names_sep = " - "
  )
# Tabela 6755 - 3
# Number of agricultural exploitations owned by the producer, by genre
tabela_6755_3 = get_sidra(
  x = 6755, 
  variable = c(9998),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12564"),
  format = 2
)
# Reshape table
tabela_6755_3 = tabela_6755_3 %>%
  dplyr::select(Município, Ano, Variável, 'Sexo do produtor', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Sexo do produtor'),
    values_from = Valor,
    names_sep = " - "
  )
# Merge table
tabela_6755 = dplyr::full_join(tabela_6755_1, tabela_6755_2)
tabela_6755 = dplyr::full_join(tabela_6755, tabela_6755_3)

#### Tabela 6756 ------
# # Get information about SIDRA tables
# info_sidra(6756, wb = TRUE)
# Number of agricultural units owned by the producer, by type of help received
tabela_6756 = get_sidra(
  x = 6756, 
  variable = c(9998),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12567"),
  format = 2
)
# Reshape table
tabela_6756 = tabela_6756 %>%
  dplyr::select(Município, Ano, Variável, 'Origem da orientação técnica recebida', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Origem da orientação técnica recebida'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6769 ------
# # Get information about SIDRA tables
# info_sidra(6769, wb = TRUE)
# Number and area of agricultural units, by specific type of production
tabela_6769 = get_sidra(
  x = 6769, 
  variable = c(183,184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12440"),
  format = 2
)
# Reshape table
tabela_6769 = tabela_6769 %>%
  dplyr::select(Município, Ano, Variável, 'Grupos e classes de atividade', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Grupos e classes de atividade'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6770 ------
# # Get information about SIDRA tables
# info_sidra(6770, wb = TRUE)
# Number and area of agricultural units, by legal status of the producer
tabela_6770 = get_sidra(
  x = 6770, 
  variable = c(183,184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12559"),
  format = 2
)
# Reshape table
tabela_6770 = tabela_6770 %>%
  dplyr::select(Município, Ano, Variável, 'Condição legal do produtor', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Condição legal do produtor'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6772 ------
# # Get information about SIDRA tables
# info_sidra(6772, wb = TRUE)
# Number and area of agricultural units, by size of the production unit
tabela_6772 = get_sidra(
  x = 6772, 
  variable = c(183,184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C220"),
  format = 2
)
# Reshape table
tabela_6772 = tabela_6772 %>%
  dplyr::select(Município, Ano, Variável, 'Grupos de área total', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Grupos de área total'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6773 ------
# # Get information about SIDRA tables
# info_sidra(6773, wb = TRUE)
# Tabela 6773 - 1
# Number and area of agricultural units, by residential place of the producer
tabela_6773_1 = get_sidra(
  x = 6773, 
  variable = c(183,184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12553"),
  format = 2
)
# Reshape table
tabela_6773_1 = tabela_6773_1 %>%
  dplyr::select(Município, Ano, Variável, 'Residência da pessoa que dirige o estabelecimento', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Residência da pessoa que dirige o estabelecimento'),
    values_from = Valor,
    names_sep = " - "
  )
# Tabela 6773 - 2
# Number and area of agricultural units, by main purpose of the establishment's agricultural production
tabela_6773_2 = get_sidra(
  x = 6773, 
  variable = c(183,184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C834"),
  format = 2
)
# Reshape table
tabela_6773_2 = tabela_6773_2 %>%
  dplyr::select(Município, Ano, Variável, 'Finalidade principal da produção agropecuária do estabelecimento', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Finalidade principal da produção agropecuária do estabelecimento'),
    values_from = Valor,
    names_sep = " - "
  )
# Merge table
tabela_6773 = dplyr::full_join(tabela_6773_1, tabela_6773_2)

#### Tabela 6774 ------
# # Get information about SIDRA tables
# info_sidra(6774, wb = TRUE)
# Number of agricultural units, depending on how the land was obtained
tabela_6774 = get_sidra(
  x = 6774, 
  variable = c(10000,9997),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12774"),
  format = 2
)
# Reshape table
tabela_6774 = tabela_6774 %>%
  dplyr::select(Município, Ano, Variável, 'Forma de obtenção das terras', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Forma de obtenção das terras'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6836 ------
# # Get information about SIDRA tables
# info_sidra(6836, wb = TRUE)
# Tabela 6836 - 1
# Number and area of forestry units, number of trees, and cut area of forestry species, by forestry species
tabela_6836_1 = get_sidra(
  x = 6836, 
  variable = c(10067,10108,9321,1977),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C231"),
  format = 2
)
# Reshape table
tabela_6836_1 = tabela_6836_1 %>%
  dplyr::select(Município, Ano, Variável, 'Espécies da silvicultura', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Espécies da silvicultura'),
    values_from = Valor,
    names_sep = " - "
  )
# Tabela 6836 - 2
# Number and area of forestry units, number of trees, and cut area of forestry species, by legal status of the producer
tabela_6836_2 = get_sidra(
  x = 6836, 
  variable = c(10067,10108,9321,1977),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12559"),
  format = 2
)
# Reshape table
tabela_6836_2 = tabela_6836_2 %>%
  dplyr::select(Município, Ano, Variável, 'Condição legal do produtor', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Condição legal do produtor'),
    values_from = Valor,
    names_sep = " - "
  )
# Merge table
tabela_6836 = dplyr::full_join(tabela_6836_1, tabela_6836_2)

#### Tabela 6845 ------
# # Get information about SIDRA tables
# info_sidra(6845, wb = TRUE)
# Number of agricultural units, by type of production
tabela_6845 = get_sidra(
  x = 6845, 
  variable = c(183),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12568"),
  format = 2
)
# Reshape table
tabela_6845 = tabela_6845 %>%
  dplyr::select(Município, Ano, Variável, 'Tipo de prática agrícola', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Tipo de prática agrícola'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6879 ------
# # Get information about SIDRA tables
# info_sidra(6879, wb = TRUE)
# Number and area of agricultural units, by production economic value
tabela_6879 = get_sidra(
  x = 6879, 
  variable = c(183,184),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12894"),
  format = 2
)
# Reshape table
tabela_6879 = tabela_6879 %>%
  dplyr::select(Município, Ano, Variável, 'Classes de valor da produção', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Classes de valor da produção'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6887 ------
# # Get information about SIDRA tables
# info_sidra(6887, wb = TRUE)
# Number of agricultural units with employed personnel and number of staff employees, by type of staff
tabela_6887 = get_sidra(
  x = 6887, 
  variable = c(10099,185),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12578"),
  format = 2
)
# Reshape table
tabela_6887 = tabela_6887 %>%
  dplyr::select(Município, Ano, Variável, 'Tipo de pessoal ocupado', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Tipo de pessoal ocupado'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6907 ------
# # Get information about SIDRA tables
# info_sidra(6907, wb = TRUE)
# Number of agricultural units with livestock, number of heads of cattle, by type of cattle
tabela_6907 = get_sidra(
  x = 6907, 
  variable = c(10010,2209),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C12443"),
  format = 2
)
# Reshape table
tabela_6907 = tabela_6907 %>%
  dplyr::select(Município, Ano, Variável, 'Espécie da pecuária', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Espécie da pecuária'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6908 ------
# # Get information about SIDRA tables
# info_sidra(6908, wb = TRUE)
# Number of agricultural units with livestock, number of heads of cattle, by production size
tabela_6908 = get_sidra(
  x = 6908, 
  variable = c(10010,2209),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C220"),
  format = 2
)
# Reshape table
tabela_6908 = tabela_6908 %>%
  dplyr::select(Município, Ano, Variável, 'Grupos de área total', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Grupos de área total'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6946 ------
# # Get information about SIDRA tables
# info_sidra(6946, wb = TRUE)
# Number of forestry units by production size
tabela_6946 = get_sidra(
  x = 6946, 
  variable = c(10067),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C220"),
  format = 2
)
# Reshape table
tabela_6946 = tabela_6946 %>%
  dplyr::select(Município, Ano, Variável, 'Grupos de área total', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Grupos de área total'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6947 ------
# # Get information about SIDRA tables
# info_sidra(6947, wb = TRUE)
# Number of forestry units, forest production, production sold, production and selling value of forestry products, by type of forest product
tabela_6947 = get_sidra(
  x = 6947, 
  variable = c(10068,10069,10070,142,143),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C230"),
  format = 2
)
# Reshape table
tabela_6947 = tabela_6947 %>%
  dplyr::select(Município, Ano, Variável, 'Produtos da silvicultura', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Produtos da silvicultura'),
    values_from = Valor,
    names_sep = " - "
  )

#### Tabela 6955 ------
# # Get information about SIDRA tables
# info_sidra(6955, wb = TRUE)
# Number of permanent crops with and without 50 feet, by type of permanent crop
tabela_6955 = get_sidra(
  x = 6955, 
  variable = c(9504,10081),
  geo = "City", 
  geo.filter = list(City = city_codes),
  header = TRUE,
  classific = c("C227"),
  format = 2
)
# Reshape table
tabela_6955 = tabela_6955 %>%
  dplyr::select(Município, Ano, Variável, 'Produtos da lavoura permanente', Valor) %>%
  tidyr::pivot_wider(
    names_from = c(Variável, 'Produtos da lavoura permanente'),
    values_from = Valor,
    names_sep = " - "
  )

### Function ----
# # Get a given SIDRA table by specifying the table number, the variables, the classification variables, and reshape the resulting variable
# get_sidra_table <- function(table_number, variables, classific, reshape_col, city_codes) {
#   # Fetch SIDRA table data
#   tabela <- get_sidra(
#     x = table_number,
#     variable = variables,
#     geo = "City",
#     geo.filter = list(City = city_codes),
#     header = TRUE,
#     classific = c(classific),
#     format = 2
#   )
# 
#   # Reshape the table
#   tabela <- tabela %>%
#     dplyr::select(Município, Ano, Variável, all_of(reshape_col), Valor) %>%
#     tidyr::pivot_wider(
#       names_from = c(Variável, all_of(reshape_col)),
#       values_from = Valor,
#       names_sep = " - "
#     )
# 
#   return(tabela)
# }
# 
# # Example usage
# example <- get_sidra_table(
#   table_number = 6845,
#   variables = c(183),
#   classific = "C829",
#   reshape_col = "Tipologia",
#   city_codes = city_codes
# )

### Export data ----
writexl::write_xlsx(tabela_6753, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6753_2017.xlsx"))
writexl::write_xlsx(tabela_6754, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6754_2017.xlsx"))
writexl::write_xlsx(tabela_6755, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6755_2017.xlsx"))
writexl::write_xlsx(tabela_6756, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6756_2017.xlsx"))
writexl::write_xlsx(tabela_6769, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6769_2017.xlsx"))
writexl::write_xlsx(tabela_6770, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6770_2017.xlsx"))
writexl::write_xlsx(tabela_6772, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6772_2017.xlsx"))
writexl::write_xlsx(tabela_6773, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6773_2017.xlsx"))
writexl::write_xlsx(tabela_6774, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6774_2017.xlsx"))
writexl::write_xlsx(tabela_6836, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6836_2017.xlsx"))
writexl::write_xlsx(tabela_6845, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6845_2017.xlsx"))
writexl::write_xlsx(tabela_6879, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6879_2017.xlsx"))
writexl::write_xlsx(tabela_6887, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6887_2017.xlsx"))
writexl::write_xlsx(tabela_6907, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6907_2017.xlsx"))
writexl::write_xlsx(tabela_6908, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6908_2017.xlsx"))
writexl::write_xlsx(tabela_6946, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6946_2017.xlsx"))
writexl::write_xlsx(tabela_6947, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6947_2017.xlsx"))
writexl::write_xlsx(tabela_6955, here("data", "agriculture", "IBGE", "Censo_agropecuario", "tabela6955_2017.xlsx"))
