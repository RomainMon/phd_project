#------------------------------------------------#
# Author: Romain Monassier
# Objective: Analyze IBGE data
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(tidyr)
library(readxl)
library(here)
library(sidrar)
library(ggplot2)
library(purrr)

### Import data -------
# Censo agropecuario
base_path <- here("outputs", "data", "IBGE") 
tabela6753 = read_excel(file.path(base_path, "tabela6753_2017_clean.xlsx"), na = "NA")
tabela6754 = read_excel(file.path(base_path, "tabela6754_2017_clean.xlsx"), na = "NA")
tabela6755 = read_excel(file.path(base_path, "tabela6755_2017_clean.xlsx"), na = "NA")
tabela6756 = read_excel(file.path(base_path, "tabela6756_2017_clean.xlsx"), na = "NA")
tabela6769 = read_excel(file.path(base_path, "tabela6769_2017_clean.xlsx"), na = "NA")
tabela6770 = read_excel(file.path(base_path, "tabela6770_2017_clean.xlsx"), na = "NA")
tabela6772 = read_excel(file.path(base_path, "tabela6772_2017_clean.xlsx"), na = "NA")
tabela6773 = read_excel(file.path(base_path, "tabela6773_2017_clean.xlsx"), na = "NA")
tabela6774 = read_excel(file.path(base_path, "tabela6774_2017_clean.xlsx"), na = "NA")
tabela6836 = read_excel(file.path(base_path, "tabela6836_2017_clean.xlsx"), na = "NA")
tabela6845 = read_excel(file.path(base_path, "tabela6845_2017_clean.xlsx"), na = "NA")
tabela6879 = read_excel(file.path(base_path, "tabela6879_2017_clean.xlsx"), na = "NA")
tabela6887 = read_excel(file.path(base_path, "tabela6887_2017_clean.xlsx"), na = "NA")
tabela6907 = read_excel(file.path(base_path, "tabela6907_2017_clean.xlsx"), na = "NA")
tabela6908 = read_excel(file.path(base_path, "tabela6908_2017_clean.xlsx"), na = "NA")
tabela6946 = read_excel(file.path(base_path, "tabela6946_2017_clean.xlsx"), na = "NA")
tabela6947 = read_excel(file.path(base_path, "tabela6947_2017_clean.xlsx"), na = "NA")
tabela6955 = read_excel(file.path(base_path, "tabela6955_2017_clean.xlsx"), na = "NA")

# PAM
tabela5457_1 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5457_1_2023_clean.xlsx"))
tabela5457_2 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5457_2_2023_clean.xlsx"))
tabela5457_3 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5457_3_2023_clean.xlsx"))
tabela5457_4 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5457_4_2023_clean.xlsx"))
tabela5457_5 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5457_5_2023_clean.xlsx"))
tabela5457_6 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5457_6_2023_clean.xlsx"))
tabela5457_7 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5457_7_2023_clean.xlsx"))
tabela5457_8 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5457_8_2023_clean.xlsx"))

# PPM
tabela74_1 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela74_1_2023_clean.xlsx"))
tabela74_2 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela74_2_2023_clean.xlsx"))
tabela74_3 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela74_3_2023_clean.xlsx"))
tabela94   = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela94_2023_clean.xlsx"))
tabela95   = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela95_2023_clean.xlsx"))
tabela3939 = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela3939_2023_clean.xlsx"))

# Other socio-demographics
POP2024     = readxl::read_excel(here::here("outputs", "data", "IBGE", "POP2024_clean.xlsx"))
tabela5434  = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela5434_2024_clean.xlsx"))
tabela8175  = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela8175_2022_clean.xlsx"))
tabela8176  = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela8176_2022_clean.xlsx"))
tabela9514  = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela9514_2022_clean.xlsx"))
tabela9923  = readxl::read_excel(here::here("outputs", "data", "IBGE", "tabela9923_2022_clean.xlsx"))

### Filter ------
# The filter to apply
to_remove <- c("Saquarema (RJ)", "Saquarema")

df_names <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
dfs <- mget(df_names, envir = .GlobalEnv)

dfs_filtered <- map(dfs, ~ {
  if ("City_name" %in% names(.x)) {
    filter(.x, !(as.character(City_name) %in% to_remove))
  } else .x
})

list2env(dfs_filtered, envir = .GlobalEnv)

### Statistical summary ------
#### Censo agropecuario ----

##### tabela6753 ----
# Replace NAs with 0
tabela6753 = tabela6753 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6753, wb = TRUE)

###### Legal status of lands ----
## Plot counts
# Reshape (to long)
df_long = tabela6753 %>% 
  dplyr::select(c(City_name, Nb_farm_proprias:Nb_farm_ocupadas)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Legal status of lands") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Legal status of lands") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))


###### Producer relationship with lands ----
## Plot counts
# Reshape (to long)
df_long = tabela6753 %>% 
  dplyr::select(c(City_name, Nb_farm_proprietario:Nb_farm_prod_sem_area)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Producer relationship with lands") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Producer relationship with lands") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6754 ----
# Replace NAs with 0
tabela6754 = tabela6754 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6754, wb = TRUE)

###### Type of production on land (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6754 %>% 
  dplyr::select(c(City_name, Nb_farm_lav_tempo:Nb_farm_aquicultura)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of production on land (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of production on land (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Type of production on land (area) ----
## Plot counts
# Reshape (to long)
df_long = tabela6754 %>% 
  dplyr::select(c(City_name, Area_lav_temp:Area_aquicultura)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of production on land (area)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of production on land (area)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))


##### tabela6755 ----
# Replace NAs with 0
tabela6755 = tabela6755 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6755, wb = TRUE)

###### Producer education (reading and writing) ----
## Plot counts
# Reshape (to long)
df_long = tabela6755 %>% 
  dplyr::select(c(City_name, Nb_farm_dirig_productor_leem_escrevem, Nb_farm_dirig_productor_nao_leem_escrevem)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Producer education (reading and writing)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Producer education (reading and writing)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Producer education ----
## Plot counts
# Reshape (to long)
df_long = tabela6755 %>% 
  dplyr::select(c(City_name, Nb_farm_dirig_productor_nunca_escola:Nb_farm_dirig_productor_mestrado_doutorado)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Producer education") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Producer education") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Producer age ----
## Plot counts
# Reshape (to long)
df_long = tabela6755 %>% 
  dplyr::select(c(City_name, Nb_farm_dirig_productor_less_25:Nb_farm_dirig_productor_mais_75)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Producer age") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Producer age") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))


###### Producer genre ----
## Plot counts
# Reshape (to long)
df_long = tabela6755 %>% 
  dplyr::select(c(City_name, Nb_farm_dirig_productor_men:Nb_farm_dirig_productor_women)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Producer genre") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Producer genre") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))



##### tabela6756 ----
# Replace NAs with 0
tabela6756 = tabela6756 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6756, wb = TRUE)

###### Technical orientation received on-farm ----
## Plot counts
# Reshape (to long)
df_long = tabela6756 %>% 
  dplyr::select(c(City_name, Nb_farm_dirig_productor_recebe_orientacao_tecnica, Nb_farm_dirig_productor_nao_recebe_orientacao_tecnica)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Technical orientation received on-farm") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Technical orientation received on-farm") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Origin of technical orientation received on-farm ----
## Plot counts
# Reshape (to long)
df_long = tabela6756 %>% 
  dplyr::select(c(City_name, Nb_farm_dirig_productor_orientacao_governo:Nb_farm_dirig_productor_orientacao_outra)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Origin of technical orientation received on-farm") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Origin of technical orientation received on-farm") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6769 ----
# Replace NAs with 0
tabela6769 = tabela6769 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6769, wb = TRUE)

###### Type of agricultural production (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6769 %>% 
  dplyr::select(-c(Nb_farm_lav_tempo, Nb_farm_horticultura, Nb_farm_lav_perm, Nb_farm_sementes_certif, Nb_farm_pecuaria, Nb_farm_pesca_agua_doce, Nb_farm_aquicultura)) %>% 
  dplyr::select(c(City_name, Nb_farm_cereais:Nb_farm_aquicultura_agua_doce)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of agricultural production (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of agricultural production (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=30)

###### Type of agricultural production (area) ----
## Plot counts
# Reshape (to long)
df_long = tabela6769 %>% 
  dplyr::select(-c(Area_lav_temp, Area_horticultura, Area_lav_perm, Area_farm_sementes_certif, Area_pecuaria, Area_farm_pesca_agua_doce, Area_aquicultura)) %>% 
  dplyr::select(c(City_name, Area_farm_cereais:Area_farm_aquicultura_agua_doce)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of agricultural production (area)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of agricultural production (area)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=30)

##### tabela6770 ----
# Replace NAs with 0
tabela6770 = tabela6770 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6770, wb = TRUE)

###### Legal status of the producer ----
## Plot counts
# Reshape (to long)
df_long = tabela6770 %>% 
  dplyr::select(c(City_name, Nb_farm_produtor_indiv:Nb_farm_outra_condicao)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Legal status of the producer (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Legal status of the producer (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6772 ----
# Replace NAs with 0
tabela6772 = tabela6772 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6772, wb = TRUE)

###### Farm size (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6772 %>% 
  dplyr::select(c(City_name, Nb_farm_menos_10a:Nb_farm_prod_sem_area)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Farm size (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Farm size (farm number)") +
  theme_minimal()


## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))


##### tabela6773 ----
# Replace NAs with 0
tabela6773 = tabela6773 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6773, wb = TRUE)

###### Home of the owner (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6773 %>% 
  dplyr::select(c(City_name, Nb_farm_dirigente_reside_no_farm:Nb_farm_dirigente_reside_outro_local)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Home of the owner (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Home of the owner (farm number)") +
  theme_minimal()


## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))


###### Farm aim (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6773 %>% 
  dplyr::select(c(City_name, Nb_farm_aim_consumo_proprio:Nb_farm_aim_comercializacao)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Farm aim (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Farm aim (farm number)") +
  theme_minimal()


## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))


##### tabela6774 ----
# Replace NAs with 0
tabela6774 = tabela6774 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6774, wb = TRUE)

###### Land owners and renters (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6774 %>% 
  dplyr::select(c(City_name, Nb_farm_prod_proprio, Nb_farm_prod_sem_titulacao)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Land owners and renters (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Land owners and renters (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Type of land acquisition (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6774 %>% 
  dplyr::select(c(City_name, Nb_farm_prod_proprio_compra_particular:Nb_farm_prod_proprio_nao_sabe)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of land acquisition (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of land acquisition (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Type of land acquisition (without title) (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6774 %>% 
  dplyr::select(c(City_name, Nb_farm_prod_sem_titulacao_compra_particular:Nb_farm_prod_sem_titulacao_nao_sabe)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of land acquisition (without title) (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of land acquisition (without title) (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))


##### tabela6836----
# Replace NAs with 0
tabela6836 = tabela6836 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6836, wb = TRUE)

###### Type of forest production (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6836 %>% 
  dplyr::select(c(City_name, Nb_farm_silvi_acacia_mangium:Nb_farm_silvi_outra_especie)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of forest product (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of forest product (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Type of forest production (tree units) ----
## Plot counts
# Reshape (to long)
df_long = tabela6836 %>% 
  dplyr::select(c(City_name, Nb_pes_farm_acacia_mangium:Nb_pes_farm_outra_especie)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of forest product (tree units)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of forest product (tree units)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6845----
# Replace NAs with 0
tabela6845 = tabela6845 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6845, wb = TRUE)

###### Type of agricultural management (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6845 %>% 
  dplyr::select(c(City_name, Nb_farm_plantio_nivel:Nb_farm_nenhuma_pratica)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of agricultural management (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of agricultural management (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6879----
# Replace NAs with 0
tabela6879 = tabela6879 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
#info_sidra(6879, wb = TRUE)

###### Farm value (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6879 %>% 
  dplyr::select(c(City_name, Nb_farm_valor_0_5000:Nb_farm_sem_valor)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Farm value (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Farm value (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6887----
# Replace NAs with 0
tabela6887 = tabela6887 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6887, wb = TRUE)

###### Family relationship with the staff employed (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6887 %>% 
  dplyr::select(c(City_name, Nb_farm_pessoal_parentesco, Nb_farm_pessoal_sem_parentesco)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Family relationship with the staff employed (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Family relationship with the staff employed (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Characteristics of family members employed (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6887 %>% 
  dplyr::select(c(City_name, Nb_farm_pessoal_parentesco_homens_mais14:Nb_farm_pessoal_parentesco_mulheres_menos14)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Characteristics of family members employed (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Characteristics of family members employed (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Type of non-family staff employed (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6887 %>% 
  dplyr::select(c(City_name, Nb_farm_pessoal_sem_parentesco_permanentes, Nb_farm_pessoal_sem_parentesco_temporarios, Nb_farm_pessoal_sem_parentesco_parceiros)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of non-family staff employed (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of non-family staff employed (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6907----
# Replace NAs with 0
tabela6907 = tabela6907 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6907, wb = TRUE)

###### Type of livestock (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6907 %>% 
  dplyr::select(c(City_name, Nb_farm_bovinos:Nb_farm_coelhos)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of livestock (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of livestock (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

###### Type of livestock (number of heads) ----
## Plot counts
# Reshape (to long)
df_long = tabela6907 %>% 
  dplyr::select(c(City_name, Nb_heads_bovinos:Nb_heads_coelhos)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Type of livestock (number of heads)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Type of livestock (number of heads)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6908----
# Replace NAs with 0
tabela6908 = tabela6908 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6908, wb = TRUE)

###### Livestock farm size (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6908 %>% 
  dplyr::select(c(City_name, Nb_farm_pecuaria_menos_10a:Nb_farm_pecuaria_sem_area)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Livestock farm size (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Livestock farm size (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6946----
# Replace NAs with 0
tabela6946 = tabela6946 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6946, wb = TRUE)

###### Forest production size (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6946 %>% 
  dplyr::select(c(City_name, Nb_farm_silvi_0_10a:Nb_farm_silvi_sem_area)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Forest production size (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Forest production size (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6947----
# Replace NAs with 0
tabela6947 = tabela6947 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6947, wb = TRUE)

###### Types of forest product (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6947 %>% 
  dplyr::select(c(City_name, Nb_farm_silvi_arvore_pe:Nb_farm_silvi_mudas_outras)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Types of forest product (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Types of forest product (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela6955----
# Replace NAs with 0
tabela6955 = tabela6955 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(6955, wb = TRUE)

###### Types of permanent crops (> 50 feet) (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6955 %>% 
  dplyr::select(c(City_name, Nb_farm_mais50_pes_lav_perm_abacate:Nb_farm_mais50_pes_lav_perm_mudas_outro)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Types of permanent crops (> 50 feet) (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Types of permanent crops (> 50 feet) (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=70)

###### Types of permanent crops (< 50 feet) (farm number) ----
## Plot counts
# Reshape (to long)
df_long = tabela6955 %>% 
  dplyr::select(c(City_name, Nb_farm_menos50_pes_lav_perm_abacate:Nb_farm_menos50_pes_lav_perm_mudas_outro)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Types of permanent crops (< 50 feet) (farm number)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Types of permanent crops (< 50 feet) (farm number)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=70)

#### PAM ----

##### tabela5457_1 ----
# Replace NAs with 0
tabela5457_1 = tabela5457_1 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(5457, wb = TRUE)

###### Temporary and permanent crops (area planted) ----
## Plot counts
# Reshape (to long)
df_long = tabela5457_1 %>% 
  dplyr::select(c(City_name, Abacate_area_plant_ha:Uva_area_plant_ha)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Temporary and permanent crops (area planted, ha)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Temporary and permanent crops (area planted, ha)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=65)

##### tabela5457_3 ----
# Replace NAs with 0
tabela5457_3 = tabela5457_3 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(5457, wb = TRUE)

###### Temporary and permanent crops (area harvested) ----
## Plot counts
# Reshape (to long)
df_long = tabela5457_3 %>% 
  dplyr::select(c(City_name, Abacate_area_colh_ha:Uva_area_colh_ha)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Temporary and permanent crops (area harvested)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Temporary and permanent crops (area harvested)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=65)

##### tabela5457_5 ----
# Replace NAs with 0
tabela5457_5 = tabela5457_5 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(5457, wb = TRUE)

###### Temporary and permanent crops (quantity produced, tons) ----
## Plot counts
# Reshape (to long)
df_long = tabela5457_5 %>% 
  dplyr::select(c(City_name, Abacate_qt_prod_t:Uva_qt_prod_t)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Temporary and permanent crops (quantity produced, tons)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Temporary and permanent crops (quantity produced, tons)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=65)

##### tabela5457_6 ----
# Replace NAs with 0
tabela5457_6 = tabela5457_6 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(5457, wb = TRUE)

###### Temporary and permanent crops (average production yield, Kilograms per Hectare) ----
## Plot counts
# Reshape (to long)
df_long = tabela5457_6 %>% 
  dplyr::select(c(City_name, Abacate_rend:Uva_rend)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Temporary and permanent crops (average production yield, Kilograms per Hectare)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Temporary and permanent crops (average production yield, Kilograms per Hectare)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=65)

##### tabela5457_7 ----
# Replace NAs with 0
tabela5457_7 = tabela5457_7 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(5457, wb = TRUE)

###### Temporary and permanent crops (value of production, Thousand Reais) ----
## Plot counts
# Reshape (to long)
df_long = tabela5457_7 %>% 
  dplyr::select(c(City_name, Abacate_valor_reais:Uva_valor_reais)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Temporary and permanent crops (value of production, Thousand Reais)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Temporary and permanent crops (value of production, Thousand Reais)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=65)

#### PPM ----

##### tabela74_1 ----
# Replace NAs with 0
tabela74_1 = tabela74_1 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(74, wb = TRUE)

###### Production of animal origin ----
## Plot counts
# Reshape (to long)
df_long = tabela74_1 %>% 
  dplyr::select(c(City_name, Leite_qt:La_qt)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Production of animal origin") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Production of animal origin") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela74_2 ----
# Replace NAs with 0
tabela74_2 = tabela74_2 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(74, wb = TRUE)

###### Production of animal origin ----
## Plot counts
# Reshape (to long)
df_long = tabela74_2 %>% 
  dplyr::select(c(City_name, Leite_valor_reais:La_valor_reais)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Production of animal origin (value, reais)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Production of animal origin (value, reais)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela94----
# Replace NAs with 0
tabela94 = tabela94 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(94, wb = TRUE)

###### Milking cows ----
## Plot counts
# Reshape (to long)
df_long = tabela94 %>% 
  dplyr::select(c(City_name, Vacas_ordenhadas)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Milking cows") +
  theme_minimal()

##### tabela3939----
# Replace NAs with 0
tabela3939 = tabela3939 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(3939, wb = TRUE)

###### Herd numbers ----
## Plot counts
# Reshape (to long)
df_long = tabela3939 %>% 
  dplyr::select(c(City_name, Bovino:Codornas)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Herd numbers") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Herd numbers") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

#### Other socio-demographics ----

##### POP2024----
# Replace NAs with 0
POP2024 = POP2024 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Compute proportions
POP2024 %>% 
  dplyr::mutate(proportion = Pop2024*100/sum(Pop2024)) %>% 
  dplyr::arrange(desc(proportion))

##### tabela5434----
# Replace NAs with 0
tabela5434 = tabela5434 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(5434, wb = TRUE)

###### People aged 14 and over by activity group (thousand persons) ----
# Reshape (to long)
df_long = tabela5434 %>% 
  dplyr::filter(UF=="Rio de Janeiro") %>% 
  dplyr::select(c(UF, Pessoas_total, Agricultura_pecuaria:Atividades_mal_definidas)) %>% 
  tidyr::pivot_longer(cols = -UF, 
                      names_to = "Category", 
                      values_to = "N")

# Compute proportions
df_long %>% 
  dplyr::mutate(proportion = N*100/8326) %>% 
  dplyr::arrange(desc(proportion))

##### tabela8175 ----
# Replace NAs with 0
tabela8175 = tabela8175 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(8175, wb = TRUE)

###### Indigenous population ----
# Compute proportions
tabela8175 %>% 
  dplyr::mutate(proportion = Indigenas_total*100/sum(Indigenas_total)) %>% 
  dplyr::arrange(desc(proportion))

## Plot counts
# Reshape (to long)
df_long = tabela8175 %>% 
  dplyr::select(c(City_name, Indigenas_homens:Indigenas_mulheres)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Indigenous people (by gender)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Indigenous people (by gender)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela8176 ----
# Replace NAs with 0
tabela8176 = tabela8176 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(8176, wb = TRUE)

###### Quilombolas ----
# Compute proportions
tabela8176 %>% 
  dplyr::mutate(proportion = Quilombolas_total*100/sum(Quilombolas_total)) %>% 
  dplyr::arrange(desc(proportion))

## Plot counts
# Reshape (to long)
df_long = tabela8176 %>% 
  dplyr::select(c(City_name, Quilombolas_homens:Quilombolas_mulheres)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Quilombolas (by gender)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Quilombolas (by gender)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion))

##### tabela9514 ----
# Replace NAs with 0
tabela9514 = tabela9514 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(9514, wb = TRUE)

###### Number of inhabitants by age ----
## Plot counts
# Reshape (to long)
df_long = tabela9514 %>% 
  dplyr::select(c(City_name, Pessoas_0_4_anos:Pessoas_mais_100_anos)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Number of inhabitants by age category") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Number of inhabitants by age category") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=21)

##### tabela9923 ----
# Replace NAs with 0
tabela9923 = tabela9923 %>%
  dplyr::mutate(across(where(is.logical), ~ as.numeric(.))) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(., 0)))

# Info data (Sidra)
# info_sidra(9923, wb = TRUE)

###### Number of inhabitants by residence location (rural or urban) ----
## Plot counts
# Reshape (to long)
df_long = tabela9923 %>% 
  dplyr::select(c(City_name, Populacao_urbana:Populacao_rural)) %>% 
  tidyr::pivot_longer(cols = -City_name, 
                      names_to = "Category", 
                      values_to = "N")

# Plot
ggplot(df_long, aes(y = City_name, x = N, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(N, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            check_overlap = TRUE) +
  labs(x = "N", y = "City", title = "Number of inhabitants by residence location (rural or urban)") +
  theme_minimal()

## Plot proportions
# Reshape (to long)
df_long_prop = df_long %>%
  group_by(City_name) %>%
  mutate(Proportion = N / sum(N, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(df_long_prop, aes(y = City_name, x = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", check_overlap = TRUE) +
  labs(x = "Proportion", y = "City", title = "Number of inhabitants by residence location (rural or urban)") +
  theme_minimal()

## Compute means
df_long %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(N_cat = sum(N, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_proportion = N_cat / sum(N_cat) * 100
  ) %>% 
  dplyr::arrange(desc(mean_proportion)) %>% 
  print(n=21)