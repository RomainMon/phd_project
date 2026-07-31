#------------------------------------------------#
# Authors: Aurélie Vinot, Romain Monassier
# Objective: Running SNAs on questionnaire data (Q1) to stakeholders
# Mostly based on Aurélie's M2 internship (January-July 2026)
# Some analyses were added after the end of her internship
#------------------------------------------------#

# library 
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)
library(forcats)
library(igraph)
library(multinet)
library(sna)
library(gtools)
library(bipartite)
library(bipartiteD3)
library(htmltools)
library(htmlwidgets)
library(ggeffects)
library(lmerTest)
library(performance)
library(sjPlot)
library(DHARMa)
library(here)
library(AICcmodavg)

### 1.RESPONSE RATE -----
## Data
pourcentage <- read_excel(here("data","interviews","Q1","Dataset_Q1_clean.xlsx"),
                          sheet = "responses_rates")

## Clean data 
creer_graphique_article <- function(pourcentage, col_taux, titre) {
  
  # clean and sort data 
  data_clean <- pourcentage %>%
    dplyr::filter(!!sym(col_taux) > 0)
  
  categories_triee <- sort(unique(data_clean$Category))
  if ("Other" %in% categories_triee) {
    categories_triee <- c(categories_triee[categories_triee != "Other"], "Other")
  }
  
  # labels 
  data_clean <- data_clean %>%
    dplyr::mutate(Category = factor(Category, levels = categories_triee)) %>%
    dplyr::arrange(desc(Category)) %>% # Inversion pour correspondre à l'ordre d'empilement ggplot
    dplyr::mutate(
      prop = !!sym(col_taux),
      label = paste0(round(prop, 1), "%"),
      label = ifelse(prop < 2, "", paste0(round(prop, 1), "%")), # label if >= 2
      y_pos = cumsum(prop) - (prop / 2) # text in the middle
    )
  
  # add colors 
  nb_colors <- length(categories_triee)
  mes_couleurs <- palette.colors(n = nb_colors, palette = "Polychrome")
  
  # graph
  ggplot(data_clean, aes(x = 2, y = prop, fill = Category)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.5) + 
  coord_polar(theta = "y") +
  # rate
  geom_text(aes(y = y_pos, label = label), 
            color = "black", 
            size = 2.5, 
            fontface = "bold") +
  xlim(0.5, 2.5) + 
  theme_void() + 
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15, margin = margin(b = 10))
  ) +
  labs(title = titre, fill = "Stakeholders category") +
  scale_fill_manual(values = as.vector(mes_couleurs))
}


## Graph creation 
p1 <- creer_graphique_article(pourcentage, "Taux_tot_category", "Percentage of the respondents")
p2 <- creer_graphique_article(pourcentage, "Taux_pop", "Percentage of contacted actors")

# Extract colors of stakeholders in p1
cats_p1 <- levels(ggplot_build(p1)$plot$data$Category)
cols_p1 <- as.vector(palette.colors(n = length(cats_p1), palette = "Polychrome"))

palette_reference <- cols_p1
names(palette_reference) <- cats_p1 

# Add stakeholders in P2 but not in P1
toutes_cats_p2 <- unique(pourcentage$Category[pourcentage$Taux_pop > 0])
nouvelles_cats <- setdiff(toutes_cats_p2, cats_p1)

# Neew colors from polychrom but different from the existing categories 
if(length(nouvelles_cats) > 0) {
  extra_cols <- as.vector(palette.colors(n = length(toutes_cats_p2) + 5, palette = "Polychrome"))
  nouvelles_cols <- extra_cols[(length(cols_p1) + 1):(length(cols_p1) + length(nouvelles_cats))]
  names(nouvelles_cols) <- nouvelles_cats
  palette_reference <- c(palette_reference, nouvelles_cols) # new reference palette 
}

# Re-create the graphs 
creer_graphique_stable <- function(pourcentage, col_taux, titre, palette_fixe) {
  
  data_clean <- pourcentage %>% dplyr::filter(!!sym(col_taux) > 0)
  
  categories_presentes <- sort(unique(data_clean$Category))
  if ("Other" %in% categories_presentes) {
    categories_presentes <- c(categories_presentes[categories_presentes != "Other"], "Other")
  }
  
  data_clean <- data_clean %>%
    dplyr::mutate(Category = factor(Category, levels = categories_presentes)) %>%
    dplyr::arrange(desc(Category)) %>% 
    dplyr::mutate(
      prop = !!sym(col_taux),
      label = ifelse(prop < 2, "", paste0(round(prop, 1), "%")),
      y_pos = cumsum(prop) - (prop / 2)
    )
  
  ggplot(data_clean, aes(x = 2, y = prop, fill = Category)) +
    geom_bar(stat = "identity", color = "white", linewidth = 0.5) + 
    coord_polar(theta = "y", clip = "off") + 
    geom_text(
      aes(x = 2, y = y_pos, label = label),
      size = 4,
      fontface = "bold",
      color = "black"
    ) +
    xlim(0.5, 3.5) + 
    labs(fill = "Stakeholder category", title = titre) +
    theme_void() + 
    theme(
      legend.position = "right",
      plot.margin = margin(20, 20, 20, 20) 
    ) +
    scale_fill_manual(values = palette_fixe)
}

p1 <- creer_graphique_article(pourcentage, "Taux_tot_category", "Percentage of the respondents")
p2 <- creer_graphique_article(pourcentage, "Taux_pop", "Percentage of contacted actors")

print(p1)
print(p2)

# combined plots 
combined_plot <- p1 + theme(legend.position = "none") + (p2 + theme(legend.position = "none"))
combined_plot


### 2.DISTRIBUTION OF THE SURVEY POPULATION ----
## Data
data <- read_excel(here("data","interviews","Q1","Dataset_Q1_clean.xlsx"),
                   sheet = "demographic_data")


## Demographic graph 
data <- data %>%
  dplyr::mutate(group = cut(Age, 
                     breaks = c(0, 20, 30, 40, 50, 60, 70, 100),
                     labels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
                     right = FALSE))

ggplot(data, aes(x = group, fill = Gender)) +
  geom_bar(position = "dodge") +
   geom_text(
    stat = 'count', 
    aes(label = after_stat(count)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3.5
  ) +
  scale_fill_manual(values = c("Female" = "#e91", "Male" = "#219")) +
  labs(
    title = "Distribution of actors by age and gender",
    x = "Age group",
    y = "Number of actors"
  ) +
  theme_minimal()

## Years of involvement 
data <- data %>%
  dplyr::mutate(pro_group = cut(Pro_involvement , 
                         breaks = c(0,10,20, 30, 40, 51),
                         labels = c("<10","10-19", "20-29", "30-39", "40-49"),
                         right = FALSE))

ggplot(data, aes(x = pro_group)) +
  geom_bar(fill = "#346789", color = "white") +
  geom_text(stat='count', aes(label = after_stat(count)), vjust=-0.5) + 
  labs(
    title = "Distribution of actors by their years of professional experience in the GLTCP",
    x = "Years of professional experience in the GLTCP",
    y = "Number of actors"
  ) +
  theme_minimal()


### 3.MULTILAYER NETWORK -----
## data 
data_q8 <- read_excel(here("data","interviews","Q1","Dataset_Q1_clean.xlsx"), 
                      sheet = "matrix_collaboration")
data_q9 <- read_excel(here("data","interviews","Q1","Dataset_Q1_clean.xlsx"),
                      sheet = "matrix_information")
data_q10 <- read_excel(here("data","interviews","Q1","Dataset_Q1_clean.xlsx"),
                       sheet = "matrix_dependency")
#### 3.1 Matrices  ---------
##### Collaboration ------
## Summary
data_q8_long = data_q8 %>% 
  tidyr::pivot_longer(!Stakeholders_categories, names_to = "With", values_to = "Collab_type")
unique(data_q8_long$Collab_type)


###### Binary network ---------
## Asymmetric matrix
data_q8ma <- data_q8 %>%
  dplyr::mutate (across(
    .col = -1, ~ dplyr::case_when(
      . == "Currently collaborating" ~ 1, # If 2 stakeholders are currently collaborating: take 1
      str_detect(., "^No prior")       ~ 0, # If 2 stakeholders are no prior collaboration: take 0
      str_detect(., "^Collaborated in the past,") ~ 1, # If 2 stakeholders collaborated in the past: take 1
      TRUE                            ~ NA_real_
    )))

## Transform into a symmetric matrix between stakeholder categories  
data_q8m <- data_q8ma %>%
  dplyr::group_by(Stakeholders_categories) %>%
  dplyr::summarise(dplyr::across(everything(), ~ if(all(is.na(.))) NA_real_ else max(., na.rm = TRUE))) %>% 
  dplyr::ungroup()

# Rearrange the table, rows, and columns as shown opposite + replace NA with 0
data_q8m_final = data_q8m %>%
  dplyr::arrange(Stakeholders_categories) %>%
  dplyr::select(Stakeholders_categories, sort(setdiff(colnames(.), "Stakeholders_categories")))

data_q8m_clean <- data_q8m_final %>%
  dplyr::mutate(across(where(is.numeric), ~ replace_na(., 0))) # NA --> 0 

## Transform into clean matrix
matrice_q8 = data_q8m_clean %>%
  tibble::column_to_rownames("Stakeholders_categories") %>%
  as.matrix()

diag(matrice_q8) <- 0 # Diagonal of 0

matrice_q8_sym <- (matrice_q8 | t(matrice_q8)) # rendre la matrice symétrique, si 1 d'un côté, 1 de l'autre aussi 
matrice_q8_sym <- matrice_q8_sym * 1

## Network 
net_q8 <-
  igraph::graph_from_adjacency_matrix(as.matrix(matrice_q8_sym),
                                      mode = "max")
## Network metrics 
#degree
igraph::degree(net_q8) 
#betweeness
igraph::betweenness(net_q8)
#eigenvector
igraph::eigen_centrality(net_q8)
#centralization
igraph::centr_degree(net_q8) 
#density
igraph::edge_density(net_q8)
#Triades-transitivité 
igraph::triad_census(net_q8)

igraph::transitivity(net_q8, type = "global")
igraph::transitivity(net_q8, type = "local")

igraph::diameter(net_q8, directed = FALSE)
igraph::farthest_vertices(net_q8, directed = FALSE)
# Modularité 
plot(net_q8,vertex.size=5,vertex.label="")
coms<-cluster_fast_greedy(net_q8)
coms

###### Weighted network ---------
# We calculate wij = number of respondents reporting collaboration / number of respondents in the stakeholder category
# This would solve two issues:
# 1) it avoids creating a tie based on a single "yes" response;
# 2) it makes the collaboration layer weighted, making it more consistent with the other information and dependency layers. 
# The collaboration network would then represent the strength (or prevalence) of collaboration between stakeholder groups rather than simply the existence of a collaboration.

## Asymmetric matrix
# Here, we only consider the current collaborations (take the value 1)
data_q8a <- data_q8 %>%
  dplyr::mutate (across(
    .col = -1, ~ dplyr::case_when(
      . == "Currently collaborating" ~ 1,
      str_detect(., "^No prior") ~ 0,
      str_detect(., "^Collaborated in the past,") ~ 0,
      TRUE                            ~ NA_real_
    )))

# Compute wij
data_q8av <- data_q8a %>%
  dplyr::group_by(Stakeholders_categories) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      ~ round(sum(. == 1, na.rm = TRUE) / dplyr::n(),2)
    ),
    .groups = "drop"
  )

# Put rows and columns across from each other
data_q8arr <- data_q8av %>%
  arrange(Stakeholders_categories) %>%
  select(Stakeholders_categories, sort(setdiff(colnames(.), "Stakeholders_categories")))

matrice_q8av <- data_q8arr %>% 
  tibble::column_to_rownames("Stakeholders_categories") %>%
  as.matrix()

# Creation of the symmetric matrix (mean of each side)
temp_array <- array(c(matrice_q8av, t(matrice_q8av)), dim = c(nrow(matrice_q8av), ncol(matrice_q8av), 2)) # 3D version of the matrice 
mat_sym_q8 <- rowMeans(temp_array, dims = 2, na.rm = TRUE) # Calculating the average along the third dimension (depth)
mat_sym_q8[is.na(mat_sym_q8)] <- 0 # rowMeans with na.rm=TRUE returns 0 if both values were NA
rownames(mat_sym_q8) <- rownames(matrice_q8av)
colnames(mat_sym_q8) <- colnames(matrice_q8av)# Restoring column and row names 
diag(mat_sym_q8) <- 0

## Network 
net_q8av <- graph_from_adjacency_matrix(mat_sym_q8, 
                                        mode = "max", 
                                        weighted = TRUE, 
                                        diag = FALSE)

## Network metrics
# degree
igraph::degree(net_q8av)
igraph::strength(net_q8av)
# betweenessfo weighted networks  
poids_distance <- 1 / E(net_q8av)$weight
igraph::betweenness(net_q8av, directed = FALSE, weights = poids_distance)
# eigenvector 
igraph::eigen_centrality(net_q8av)
# Centralization
igraph::centr_degree(net_q8av) 
# density
igraph::edge_density(net_q8av)
#Triades-transitivité 
igraph::triad_census(net_q8av)

igraph::transitivity(net_q8av, type = "global")
igraph::transitivity(net_q8av, type = "local")
# diameter 
igraph::diameter(net_q8av, directed = FALSE)
igraph::farthest_vertices(net_q8av, directed = FALSE)  #get the ids of the nodes that form the ends of that longest shortest path
# Modularité 
plot(net_q8av,vertex.size=5,vertex.label="")
coms<-cluster_fast_greedy(net_q8av)
coms


##### Information ------
data_q9a <- data_q9 %>%
  mutate (across(
    .col = -1, ~ case_when(
      . == "Not at all" ~ 0,
      . == "Slightly" ~ 0.25,
      . == "Moderately" ~ 0.5,
      . == "Highly" ~ 0.75,
      . == "Fully" ~ 1,
    )))

# Mean by stakeholder categories/column 
data_q9av <- data_q9a %>%
  group_by(across(1)) %>% # group by first column 
  summarise(across(everything(), ~ round(mean(., na.rm = TRUE),2)), .groups = "drop") # mean for other columns 

# Put rows and columns across from each other
data_q9arr <- data_q9av %>%
  arrange(Stakeholders_categories) %>%
  select(Stakeholders_categories, sort(setdiff(colnames(.), "Stakeholders_categories")))

matrice_q9av <- data_q9arr %>% 
  tibble::column_to_rownames("Stakeholders_categories") %>%
  as.matrix()

# Creation of the symmetric matrix (mean of each side)
temp_array <- array(c(matrice_q9av, t(matrice_q9av)), dim = c(nrow(matrice_q9av), ncol(matrice_q9av), 2)) # 3D version of the matrice 
mat_sym_q9 <- rowMeans(temp_array, dims = 2, na.rm = TRUE) # Calculating the average along the third dimension (depth)
mat_sym_q9[is.na(mat_sym_q9)] <- 0 # rowMeans with na.rm=TRUE returns 0 if both values were NA
rownames(mat_sym_q9) <- rownames(matrice_q9av)
colnames(mat_sym_q9) <- colnames(matrice_q9av)# Restoring column and row names 
diag(mat_sym_q9) <- 0

## Network 
net_q9av <- graph_from_adjacency_matrix(mat_sym_q9, 
                                        mode = "max", 
                                        weighted = TRUE, 
                                        diag = FALSE)

## Network metrics
# degree
igraph::degree(net_q9av)
igraph::strength(net_q9av)
# betweenessfo weighted networks  
poids_distance <- 1 / E(net_q9av)$weight
igraph::betweenness(net_q9av, directed = FALSE, weights = poids_distance)
# eigenvector 
igraph::eigen_centrality(net_q9av)
# Centralization
igraph::centr_degree(net_q9av) 
# density
igraph::edge_density(net_q9av)
#Triades-transitivité 
igraph::triad_census(net_q9av)

igraph::transitivity(net_q9av, type = "global")
igraph::transitivity(net_q9av, type = "local")
# diameter 
igraph::diameter(net_q9av, directed = FALSE)
igraph::farthest_vertices(net_q9av, directed = FALSE)  #get the ids of the nodes that form the ends of that longest shortest path
# Modularité 
plot(net_q9av,vertex.size=5,vertex.label="")
coms<-cluster_fast_greedy(net_q9av)
coms

##### Dependency ------
data_q10a <- data_q10 %>%
  mutate (across(
    .col = -1, ~ case_when(
      . == "Not at all" ~ 0,
      . == "Slightly" ~ 0.25,
      . == "Moderately" ~ 0.5,
      . == "Highly" ~ 0.75,
      . == "Fully" ~ 1,
    )))

# Mean by stakeholder category/column 
data_q10av <- data_q10a %>%
  group_by(across(1)) %>% 
  summarise(across(everything(), ~ round(mean(., na.rm = TRUE),2)), .groups = "drop") 

# Put rows and columns across from each other 
data_q10arr <- data_q10av %>%
  arrange(Stakeholders_categories) %>%
  select(Stakeholders_categories, sort(setdiff(colnames(.), "Stakeholders_categories")))

matrice_q10av <- data_q10arr %>% 
  tibble::column_to_rownames("Stakeholders_categories") %>%
  as.matrix()

# Creation of the symmetric matrix (mean of each side)
temp_array <- array(c(matrice_q10av, t(matrice_q10av)), dim = c(nrow(matrice_q10av), ncol(matrice_q10av), 2)) 
mat_sym_q10 <- rowMeans(temp_array, dims = 2, na.rm = TRUE) 
mat_sym_q10[is.na(mat_sym_q10)] <- 0 
rownames(mat_sym_q10) <- rownames(matrice_q10av)
colnames(mat_sym_q10) <- colnames(matrice_q10av)
diag(mat_sym_q10) <- 0

## Network
net_q10av <- graph_from_adjacency_matrix(mat_sym_q10, 
                                         mode = "max", 
                                         weighted = TRUE, 
                                         diag = FALSE)

## Network metrics
# degree
igraph::degree(net_q10av)
igraph::strength(net_q10av)
# betweenessfo weighted networks  
poids_distance2 <- 1 / E(net_q10av)$weight
igraph::betweenness(net_q10av, directed = FALSE, weights = poids_distance2)
# eigenvector 
igraph::eigen_centrality(net_q10av)
# Centralization
igraph::centr_degree(net_q10av) 
# density
igraph::edge_density(net_q10av)
#Triades-transitivité 
igraph::triad_census(net_q10av)

igraph::transitivity(net_q10av, type = "global")
igraph::transitivity(net_q10av, type = "local")
# diameter 
igraph::diameter(net_q10av, directed = FALSE)
igraph::farthest_vertices(net_q10av, directed = FALSE)  #get the ids of the nodes that form the ends of that longest shortest path
# Modularité 
plot(net_q10av,vertex.size=5,vertex.label="")
coms<-cluster_fast_greedy(net_q10av)
coms

#### 3.2 Multilayer network ----
##### Create multilayer object ----
# Create igraph objects 
mnet2 <- ml_empty()

add_igraph_layer_ml(mnet2, net_q8av, "Collaboration")
add_igraph_layer_ml(mnet2, net_q9av, "Information")
add_igraph_layer_ml(mnet2, net_q10av, "Dependancy")

mnet2

##### Node metrics -----
## DEGREE
# Total degree per actor (all categories combined)
deg2 <- degree_ml(mnet2)
names(deg2) <- unlist(actors_ml(mnet2)) 
top_degrees2 <- (deg2[order(-deg2)]) 
print(top_degrees2)
# Degree per layer
(topdeg2 <- 
    data.frame(actor = names(top_degrees2),
               Collaboration = degree_ml(mnet2, actors = names(top_degrees2), layers = "Collaboration"),
               Information = degree_ml(mnet2, actors = names(top_degrees2), layers = "Information"),
               Dependancy = degree_ml(mnet2, actors = names(top_degrees2), layers = "Dependancy")))

# Deviation
(topdeg2$Deviation <- apply(topdeg2[, c("Collaboration", "Information", "Dependancy")], 1, sd))
# Standardized deviation
(topdeg2$Moyenne_Degre <- rowMeans(topdeg2[, c("Collaboration", "Information", "Dependancy")]))

(topdeg2$Dev_Standardisee <- ifelse(topdeg2$Moyenne_Degre > 0, 
                                    topdeg2$Deviation / topdeg2$Moyenne_Degre, 
                                    0))
print(topdeg2)


## DISTANCE
acteurs_reseau2 <- unlist(actors_ml(mnet2))
length(acteurs_reseau2)
liste_resultats2 <- list()
for (i in 1:length(acteurs_reseau2)) {
  nom_actuel2 <- acteurs_reseau2[i]
  liste_resultats2[[i]] <- distance_ml(mnet2, from = nom_actuel2)
}

toutes_les_distances2 <- do.call(rbind, liste_resultats2)

## MEAN DEGREE AND VERSATILITY
# Versatility is used to identify which stakeholders occupy consistently important positions across all three layers, rather than within a single network

## Calcul de l'écart type entre les trois couches 
liste_acteurs <- actors_ml(mnet2)[1]

# nettoyer liste et séparer acteurs 
acteurs <- gsub('c\\(|\\)|"|\'', '', liste_acteurs)

# On sépare les noms à chaque virgule et on enlève les espaces inutiles au début/fin
noms_acteurs <- trimws(unlist(strsplit(acteurs, ",")))

# Calcul des degrés des acteurs par couche
deg_collaboration <- degree_ml(mnet2, layers = "Collaboration")
deg_information   <- degree_ml(mnet2, layers = "Information")
deg_dependancy    <- degree_ml(mnet2, layers = "Dependancy")

# Assemblage dans un data.frame propre
df_degres <- data.frame(
  Collaboration = deg_collaboration,
  Information   = deg_information,
  Dependancy    = deg_dependancy
)

## Mean degree across layers
df_degres %>%
  pivot_longer(cols = c(Collaboration, Information, Dependancy), 
               names_to = "Couche", values_to = "Degree") %>%
  group_by(Couche) %>%
  summarise(
    Moyenne = mean(Degree),
    Ecart_Type = sd(Degree)
  ) %>%
  mutate(
    Resultat_Formatted = paste0(round(Moyenne, 2), " +/- ", round(Ecart_Type, 2))
  )

## Versatility computation
# Calculs
moyenne <- rowMeans(df_degres)
ecart_type <- apply(df_degres, 1, sd)

versatility_data <- data.frame(
  Stakeholder = noms_acteurs,
  Versatility_Score = moyenne / (1 + ecart_type)
)

View(versatility_data)


##### Layer comparison -----
## BASIC COMPARISON
#comparaison distribution degré
layer_comparison_ml(mnet2, method = "jeffrey.degree")
#même degré dans différentes couches - qq soit les voisins
layer_comparison_ml(mnet2, method="pearson.degree")
#mêmes liens entre mêmes paires de sommets
layer_comparison_ml(mnet2, method="jaccard.edges")

## EDGE OVERLAP
# to quantify how many relationships are shared across dimensions
matrice_overlap <- layer_comparison_ml(mnet2, method = "coverage.edges")
print(matrice_overlap)

##### Community detection -----
## COMMUNITY DETECTION
comu1 <- glouvain_ml(mnet2) #optimisation de la modularité
comu2 <- clique_percolation_ml(mnet2) #recherche de cliques adjacentes
comu3 <- abacus_ml(mnet2, min.actors = 3, min.layers = 3) #détection de motifs
comu4 <- infomap_ml(mnet2) # utlisation de marches aléatoires
#nb communautés et taille selon l'algo choisi
table(comu1$cid)
table(comu2$cid)
table(comu3$cid)
table(comu4$cid)
#indicateurs
modularity_ml(mnet2, comu1, gamma = 1, omega = 1)
modularity_ml(mnet2, comu2, gamma = 1, omega = 1)
modularity_ml(mnet2, comu3, gamma = 1, omega = 1)
modularity_ml(mnet2, comu4, gamma = 1, omega = 1)
#visualisation
plot(mnet2, vertex.labels.cex=.3, com=comu1)
plot(mnet2,vertex.labels.cex=.3, com=comu2)
plot(mnet2, vertex.labels.cex=.3, com=comu3)

#### 3.3 Cytoscape visualization ---------
# -> Requires two main tables representing intra-layer edges and nodes

###### Collaboration -----
# Node table with metric
node_table_q8 = as.data.frame(igraph::degree(net_q8av)) %>% 
  rownames_to_column(var = "Node") %>% 
  dplyr::rename(Degree_1 = "igraph::degree(net_q8av)") %>% 
  dplyr::mutate(Node = dplyr::case_when(Node == "RaPPN" ~ "RPPN",
                                        TRUE ~ Node))
# Edge table with edge weight
intra_layer_table_q8 = mat_sym_q8 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "source") %>% 
  tidyr::pivot_longer(!source, names_to = "target", values_to = "score_1") %>% 
  dplyr::mutate(source = dplyr::case_when(source == "RaPPN" ~ "RPPN",
                                          TRUE ~ source)) %>% 
  dplyr::mutate(target = dplyr::case_when(target == "RaPPN" ~ "RPPN",
                                          TRUE ~ target)) %>% 
  # Remove node i-node i edges
  subset(source != target) %>%
  # Remove 0
  dplyr::filter(score_1 > 0)

###### Information -----
# Node table with metric
node_table_q9 = as.data.frame(igraph::degree(net_q9av)) %>% 
  rownames_to_column(var = "Node") %>% 
  dplyr::rename(Degree_2 = "igraph::degree(net_q9av)") %>% 
  dplyr::mutate(Node = dplyr::case_when(Node == "RaPPN" ~ "RPPN",
                                        TRUE ~ Node))
# Edge table with edge weight
intra_layer_table_q9 = mat_sym_q9 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "source") %>% 
  tidyr::pivot_longer(!source, names_to = "target", values_to = "score_2") %>% 
  dplyr::mutate(source = dplyr::case_when(source == "RaPPN" ~ "RPPN",
                                          TRUE ~ source)) %>% 
  dplyr::mutate(target = dplyr::case_when(target == "RaPPN" ~ "RPPN",
                                          TRUE ~ target)) %>% 
  # Remove node i-node i edges
  subset(source != target) %>% 
  # Remove 0
  dplyr::filter(score_2 > 0)

###### Dependency -----
# Node table with metric
node_table_q10 = as.data.frame(igraph::degree(net_q10av)) %>% 
  rownames_to_column(var = "Node") %>% 
  dplyr::rename(Degree_3 = "igraph::degree(net_q10av)") %>% 
  dplyr::mutate(Node = dplyr::case_when(Node == "RaPPN" ~ "RPPN",
                                        TRUE ~ Node))
# Edge table with edge weight
intra_layer_table_q10 = mat_sym_q10 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "source") %>% 
  tidyr::pivot_longer(!source, names_to = "target", values_to = "score_3") %>% 
  dplyr::mutate(source = dplyr::case_when(source == "RaPPN" ~ "RPPN",
                                          TRUE ~ source)) %>% 
  dplyr::mutate(target = dplyr::case_when(target == "RaPPN" ~ "RPPN",
                                          TRUE ~ target)) %>% 
  # Remove node i-node i edges
  subset(source != target)  %>% 
  # Remove 0
  dplyr::filter(score_3 > 0)

###### Merge tables -----
# Node table
node_table = node_table_q8 %>% 
  dplyr::left_join(node_table_q9, by="Node") %>% 
  dplyr::left_join(node_table_q10, by="Node")

# Edge table
intra_layer_table = intra_layer_table_q8 %>% 
  dplyr::left_join(intra_layer_table_q9, by=c("source","target")) %>% 
  dplyr::left_join(intra_layer_table_q10, by=c("source","target")) %>% 
  # Remove duplicated pairs (source, target)
  dplyr::mutate(
    node1 = pmin(source, target),
    node2 = pmax(source, target)) %>% # Creates two columns that reorder the pairs
  dplyr::distinct(node1, node2, .keep_all = TRUE) %>% # Keep only one element by pair
  dplyr::select(-c(node1, node2))


#### 3.4 MuxViz vizualisation-----
## Création des reseau igraph
net_q8av <- graph_from_adjacency_matrix(mat_sym_q8, 
                                        mode = "max", 
                                        weighted = TRUE, 
                                        diag = FALSE)

net_q9av <- graph_from_adjacency_matrix(mat_sym_q9, 
                                        mode = "max", 
                                        weighted = TRUE, 
                                        diag = FALSE)

net_q10av <- graph_from_adjacency_matrix(mat_sym_q10, 
                                         mode = "max", 
                                         weighted = TRUE, 
                                         diag = FALSE)

## Reseau en 3D ##
file.edit("~/.Rprofile")
library(muxViz)
library(rgl)

# To make it work, I modified the code for the `plot_multiplex3D` function using this command: 
# trace(plot_multiplex3D, edit=TRUE)
# Substitutions : 
# rgl.clear par clear3d
# if (layer.labels == "auto" || length(layer.labels) != Layers) par if (any(layer.labels == "auto") || length(layer.labels) != Layers)
# if (!is.na(layer.labels) && !is.null(layer.labels)) par if (all(!is.na(layer.labels)) && !is.null(layer.labels)) x3
# if (show.aggregate && (all(!is.na(layer.labels)) && !is.null(layer.labels))) { layer.labels <- c(layer.labels, "Aggregate") par if (isTRUE(show.aggregate) && !is.null(layer.labels) && all(!is.na(layer.labels))) { layer.labels <- c(layer.labels, "Aggregate")}
# if (node.size.values == "auto") par if (is.character(node.size.values) && any(node.size.values == "auto"))
# V(g.list[[l]])$size <- node.size.values * node.size.scale par V(g.list[[l]])$size <- (if(is.list(node.size.values)) node.size.values[[l]] else node.size.values) * node.size.scale[l]
# Then, to make this a permanent change, I created a “patch_muxviz” script with the modified function code 
# And an R.profile using this command: file.edit("~/.Rprofile")
# These two new scripts allow you to directly use the modified version of the `plot_multiplex3D` function when calling the muxViz package.

### Import the new function
source(here::here("R", "aurelie_vinot", "patch_muxviz.R"))

g_list <- list(net_q8, net_q9av, net_q10av)

lay <- layoutMultiplex(g_list, layout="fr", ggplot.format=F, box=T)

node_degrees_list <- lapply(g_list, function(x) igraph::degree(x))

# PLOT

### ESSAI AMELIORATION MULTICOUCHE
plot_multiplex3D(g_list, 
                 layer.layout = lay, 
                 layer.colors = c("#E41A1C", "#377EB8", "#4DAF4A"),
                 layer.labels = c("Collaboration", "Information", "Dependency"),
                 layer.labels.cex = 1.5,
                 
                 # On garde l'espacement augmenté pour aérer le graphique
                 layer.shift.x = 0.8,       
                 layer.space = 3.5,         
                 
                 node.size.values = node_degrees_list, 
                 node.size.scale = 1.2,     
                 
                 show.nodeLabels = FALSE,   
                 show.aggregate = FALSE,
                 as.undirected = TRUE)


# AJUSTEMENT DES LIENS INTER-COUCHES 
shift_x <- 0.8  
layer_space <- 3.5 
nb_layers <- length(g_list)
nb_nodes <- nrow(lay) 

inter_layer_coords <- c()

for(l in 1:(nb_layers - 1)) {
  z_start <- (l-1) * layer_space
  x_off_start <- (l-1) * shift_x
  
  z_end <- l * layer_space
  x_off_end <- l * shift_x
  
  for(i in 1:nb_nodes) {
    pA <- c(lay[i, 1] + x_off_start, lay[i, 2], z_start)
    pB <- c(lay[i, 1] + x_off_end,   lay[i, 2], z_end)
    inter_layer_coords <- rbind(inter_layer_coords, pA, pB)
  }
}

# Dessin des segments plus fins et transparents
segments3d(inter_layer_coords, 
           col = "grey30",   
           alpha = 0.2,      # baisse de l'opacité
           lty = 3,          
           lwd = 0.8)

# AJOUT DES NOMS DES NOEUDS
noms_officiels <- c(
  "Assentamentos", "City_hall", "Federal_agencies", "Forestry_co", 
  "International_org", "Local_assos", "Local_landowners", 
  "Educational_inst", "NGO", 
  "Oil_energy_&_mining_co", "RPPN", "Road_co", 
  "Scientific_inst", "State_agencies", "Zoo"
)

numeros <- noms_officiels <- c(
  "n1", "n2", "n3", "n4", 
  "n5", "n6", "n7", "n8", "n9", "n10", "n11", "n12", "n13", 
  "n14", "n15"
)

#Ajout et placement du texte sur le graphique 3D
text3d(x = lay[, 1] + 1.0,  # On ajoute 2 * layer.shift.x (0.5 * 2)
       y = lay[, 2], 
       z = 4.1,                # On utilise 2 * layer.space (2 * 2)
       texts = numeros,
       col = "black", 
       cex = 1.2,            # 'Petit' (réduit de 1.2 à 0.7)
       adj = 0.5)


# Exportation 
library(htmlwidgets)

scene <- rglwidget() # Capture la scène actuelle
saveWidget(scene, "Multilayer_network_3D.html", selfcontained = TRUE)
shell.exec(getwd())

### 4.QAP TEST -----
liste_layers <- list(
  "Collaboration" = mat_sym_q8, 
  "Information" = mat_sym_q9,             
  "Dependancy" = mat_sym_q10
)

# collaboration-informations layers
collab_info2 <- qaptest(liste_layers, gcor, g1=1, g2=2,reps=1000)
collab_info2

plot(density(collab_info2$dist), 
     main = "Comparison of collaboration and information networks")
abline(v = collab_info2$testval, col = "red", lwd = 2)

summary(collab_info2)

# Collaboration-dependancy layers
collab_dep2 <- qaptest(liste_layers, gcor, g1=1, g2=3,reps=1000)
collab_dep2

plot(density(collab_dep2$dist), 
     main = "Comparison of collaboration and dependency networks")
abline(v = collab_dep2$testval, col = "red", lwd = 2)
summary(collab_dep2)

# Information-dependancy layers
info_dep2 <- qaptest(liste_layers, gcor, g1=2, g2=3,reps=1000)
info_dep2

plot(density(info_dep2$dist), 
     main = "Comparison of information and dependency networks")
abline(v = info_dep2$testval, col = "red", lwd = 2)
summary(info_dep2)

## Final visualization 
# Combine distribution data 
df_dist <- data.frame(
  `Collab_Info` = collab_info2$dist,
  `Collab_Dep`  = collab_dep2$dist,
  `Info_Dep`    = info_dep2$dist
) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "Comparison", values_to = "Correlation")

# Create a dataframe 
df_observed <- data.frame(
  Comparison = c("Collab_Info", "Collab_Dep", "Info_Dep"),
  Observed = c(collab_info2$testval, collab_dep2$testval, info_dep2$testval)
)

#  Define colors 
custom_colors <- c(
  "Collab_Info" = "#6C3483",  # Violet
  "Collab_Dep"  = "#F1C40F",  # Jaune
  "Info_Dep"    = "#117864"   # Vert Canard
)

# Labels 
legend_labels <- c(
  "Collab_Info" = "Collaboration vs Information",
  "Collab_Dep"  = "Collaboration vs Dependency",
  "Info_Dep"    = "Information vs Dependency"
)

# Ajust labels 
df_observed <- df_observed %>%
  mutate(
    v_offset = case_when(
      Comparison == "Collab_Dep"  ~ 4,  # below
      Comparison == "Collab_Info" ~ 2,  # middle
      Comparison == "Info_Dep"    ~ 3,  
      TRUE                        ~ 2
    )
  )

# PLOT
qap = ggplot(df_dist, aes(x = Correlation, fill = Comparison, color = Comparison)) +
  # density curves
  geom_density(alpha = 0.35, size = 0.8) +
  
  # Vertical lines
  geom_vline(data = df_observed, aes(xintercept = Observed, color = Comparison),
             linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
  
  # Text
  geom_text(data = df_observed, aes(x = Observed, y = Inf, 
                                    label = paste0("Obs: ", round(Observed, 2)), 
                                    color = Comparison, vjust = v_offset),
            hjust = 1.1, fontface = "bold", size = 3.5, show.legend = FALSE) +
  
  # Colors
  scale_fill_manual(values = custom_colors, labels = legend_labels) +
  scale_color_manual(values = custom_colors, labels = legend_labels) +
  
  # Labs
  labs(
    x = "Correlation Coefficient",
    y = "Density",
    fill = "Network Pair",
    color = "Network Pair"
  ) +
  
  theme_minimal (base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)), # Titre X plus grand
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)), # Titre Y plus grand
    axis.text.x = element_text(size = 10),                                         # Chiffres sur l'axe X
    axis.text.y = element_text(size = 10),

    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),                       
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Export
png(here("outputs","plot","03a_SNAs_QAP.png"), width = 3000, height = 1500, res = 300, type="cairo")
plot(qap)
dev.off()

### 5.BIPARTITE NETWORKS -----
# Data
data_strat <- read_excel(here("data","interviews","Q1","Dataset_Q1_clean.xlsx"),
                   sheet = "final_strategies")

#### 5.1 Bipartite actor-strategies -----
## Incidence matrix
# Dataframe
str1 <- data_strat %>%
  mutate (across(
    .col = -1, ~ case_when(
      . == "Not at all" ~ 0,
      . == "Slightly" ~ 0.25,
      . == "Moderately" ~ 0.5,
      . == "Highly" ~ 0.75,
      . == "Fully" ~ 1,
    )))
str1[is.na(str1)] <- 0

str_1 <- str1 %>% as.data.frame() # Convert to a dataframe to assign unique names to the rows
rownames(str_1) <- make.unique(str_1$Stakeholders_categories)

matrice_str_1 <- str_1 %>% # On supprime l'ancienne colonne et on transforme en matrice
  dplyr::select(-Stakeholders_categories) %>% 
  as.matrix()
View(matrice_str_1)

# réorganisation des lignes et colonnes de la matrice 
matrice_triee_1 <- matrice_str_1[mixedorder(rownames(matrice_str_1)), order(colnames(matrice_str_1))]
View(matrice_triee_1)

## Attribution de couleurs 
racine_bas <- sapply(strsplit(rownames(matrice_triee_1), "[.]"), `[`, 1) # on récupère les noms de la matrice pour avoir les attributs 
# Création du dictionnaire de couleurs 
(categories_uniques <- unique(racine_bas))
couleurs <- palette.colors(n = 11, palette = "Polychrome")
names(couleurs) <- categories_uniques

rownames(matrice_triee_1) <- sprintf("ID%02d", 1:42)  # Changement de noms des lignes 
colnames(matrice_triee_1) <- gsub("_", " ", colnames(matrice_triee_1))

## Couleurs bipartite 
# Tous les noms du graphique
noms_lignes <- rownames(matrice_triee_1)
noms_colonnes <- colnames(matrice_triee_1)
tous_les_noms <- c(noms_lignes, noms_colonnes)
# On initialise en gris clair pour les colonnes (sinon bipartite pas content)
couleurs_finales <- rep("#D3D3D3", length(tous_les_noms))
names(couleurs_finales) <- tous_les_noms
# On remplace les couleurs des lignes par les couleurs calculées
couleurs_finales[noms_lignes] <- couleurs[racine_bas]

## BipartiteD3
# Assign percentages to the strategies and rank them in descending order 
pourcentage1 <- c(" 5%", " 7%", " 6%", " 7%", " 8%", " 11%", " 8%", " 4%", " 8%", " 8%", " 6%", " 10%", " 8%", " 5%")
colnames(matrice_triee_1) <- paste0(colnames(matrice_triee_1), pourcentage1) # percentage to column 
p_numerique <- as.numeric(gsub(".* ([0-9]+)%", "\\1", colnames(matrice_triee_1))) # digit extraction 
ordre_decroissant <- order(p_numerique, decreasing = TRUE)
(matrice_ordonnee <- matrice_triee_1[, ordre_decroissant]) # matrix sorting

# PLOT
bp <- bipartite_D3(
  matrice_ordonnee,                
  colouroption = "manual",      
  NamedColourVector = couleurs_finales, 
  ColourBy = 1,               # FORCE la coloration par le côté GAUCHE
  MainFigSize = c(1000, 1500),
  IndivFigSize = c(300, 800),
  BoxLabPos = c(50, 50),
  Pad = 4,
  BarSize = 20,
  MinWidth = 5,
  PrimaryLab = 'Actors', 
  SecondaryLab = "Conservation strategies",
  SortSecondary = colnames(matrice_ordonnee),
  SiteNames = "Work into",
  IncludePerc = F)

# Create the legend in HTML: 
# References: Documentation on the htmltools package
names(couleurs) <- gsub("_", " ", names(couleurs)) 
# Iterate through the categories to create colored squares
# `lapply` transforms each category in the legend into a small piece of HTML code
legende_html <- tags$div(
  style = "padding: 10px; font-family: sans-serif; font-size: 12px; line-height: 1.5;",
  tags$h4("stakeholder category"),
  lapply(names(couleurs), function(cat) {
    tags$div(
      tags$span(style = paste0("display:inline-block; width:12px; height:12px; margin-right:5px; background-color:", couleurs[cat], ";")),
      cat
    )
  })
)

# Arrange everything in a “Flexbox” layout: CSS layout method: guide to the Flexbox module  https://css-tricks.com/snippets/css/a-guide-to-flexbox/
page_finale <- tags$div(
  style = "display: flex; align-items: flex-start;",
  tags$div(style = "flex: 0 0 200px; border-right: 1px solid #ddd;", legende_html), # Column legend
  tags$div(style = "flex: 1;", bp)                                            # Column graph
)

browsable(page_finale) 

##### Network metrics ------
## Nodes metric
# Here, we calculate various indices for network properties at the node level ('species' in bipartite package)
# where higher level nodes are in columns, lower level nodes in row

# Respondents
low1 <- specieslevel(matrice_triee_1, level = "lower")
(low_clean_1 <- low1[, c("degree", # Sum of links
                         "normalised.degree", # As degree, but scaled by the number of possible partners
                         "weighted.closeness", # Computes closeness (in one of its varieties), but based on weighted representation of the network
                         "weighted.betweenness", # Computes betweenness (proportion of shortest paths through this node), but based on weighted representation of the network
                         "nestedrank")]) # Quantifies generalism

# Strategies
high1 <- specieslevel(matrice_triee_1, level = "higher")
(high_clean_1 <- high1[, c("degree", 
                           "weighted.closeness", 
                           "nestedrank")])

## Network metrics 
# Calculation of the Most Important Overall Indices
(res_network_1 <- networklevel(matrice_triee_1, 
                               index = c("connectance", 
                                         "NODF", 
                                         "modularity", 
                                         "robustness")))

## Group-level metrics 
grouplevel(matrice_triee_1,
           index = c( "mean number of shared partners",
                      "togetherness",
                      "C score")) 


#### 5.2 Bipartite organization-strategies -----
# Calcul de la moyenne par organisation/colonne
str1_av <- str1 %>%
  group_by(across(1)) %>% # On groupe par la première colonne 
  summarise(across(everything(), ~ mean(., na.rm = TRUE)), .groups = "drop") # On calcule la moyenne pour toutes les autres colonnes
View(str1_av)

matt_str1_av <- str1_av %>% 
  tibble::column_to_rownames("Stakeholders_categories") %>%
  as.matrix()
View(matt_str1_av)

### Création du réseau bipartite ###
## Plotweb
colnames(matt_str1_av) <- gsub("_", " ", colnames(matt_str1_av))
rownames(matt_str1_av) <- gsub("_", " ", rownames(matt_str1_av))
windows(width=15, height=10)
bipartite::plotweb(matt_str1_av, 
                   srt = 90,              
                   text_size = 0.7,       
                   y_lim = c(-0.5, 1.5),  
                   spacing = 0.3,         
                   lower_color = couleurs,   
                   link_color = "lower",               
                   link_alpha = 0.5)

palette <- palette.colors(n = 14, palette = "Polychrome")
bipartite::plotweb(matt_str1_av, 
                   srt = 90,              
                   text_size = 0.7,       
                   y_lim = c(-0.5, 1.5),  
                   spacing = 0.3,         
                   higher_color = palette,   
                   link_color = "higher",               
                   link_alpha = 0.5)
## BipartiteD3
# Vecteur couleur 
lignes <- rownames(matt_str1_av)
colonnes <- colnames(matt_str1_av)
tot_noms <- c(lignes,colonnes)
# On initialise en gris clair pour les colonnes (sinon bipartite pas content)
col <- rep("#D3D3D3", length(tot_noms))
names(col) <- tot_noms
# On remplace les couleurs des lignes par les couleurs calculées
col[lignes] <- couleurs

# Classer colonne de la matrice par ordre alphabétique 
ordre_alphabetique <- order(colnames(matt_str1_av))
matt_triee <- matt_str1_av[, ordre_alphabetique]
View(matt_triee)
# Assigner les pourcentages aux stratégies et les classer dans l'ordre décroissant 
pourcentage2 <- c(" 4%", " 7%", " 5%", " 8%", " 6%", " 11%", " 7%", " 4%", " 7%", " 7%", " 7%", " 12%", " 9%", " 5%")
(colnames(matt_triee) <- paste0(colnames(matt_triee), pourcentage2)) # assigner les pourcentages aux colonnes
p_numerique <- as.numeric(gsub(".* ([0-9]+)%", "\\1", colnames(matt_triee))) # extraction des chiffres 
ordre_decroissant <- order(p_numerique, decreasing = TRUE)
(matrice_ordonnee2 <- matt_triee[, ordre_decroissant]) # trie de la matrice

# Afficher le graphique 
bp2 <- bipartite_D3(
  matrice_ordonnee2,                
  colouroption = "manual",      
  NamedColourVector = couleurs, 
  ColourBy = 1,               # FORCE la coloration par le côté GAUCHE
  MainFigSize = c(3000, 1500),
  IndivFigSize = c(300, 800),
  BoxLabPos = c(20, 50),
  Pad = 4,
  BarSize = 20,
  MinWidth = 5,
  PrimaryLab = 'Org', 
  SecondaryLab = "Conservation strategies",
  SortSecondary = colnames(matrice_ordonnee2),
  SiteNames = "Work into",
  IncludePerc = F)

# Assembler le tout dans une structure "Flexbox" : méthode de mise en page CSS : guide sur le module Flexbox https://css-tricks.com/snippets/css/a-guide-to-flexbox/
page_finale2 <- tags$div(
  style = "display: flex; align-items: flex-start;",
  tags$div(style = "flex: 0 0 200px; border-right: 1px solid #ddd;", legende_html), # Colonne légende
  tags$div(style = "flex: 1;", bp2)                                            # Colonne graphique
)

browsable(page_finale2)

#### Network metrics ------
## Nodes metric
(low2 <- specieslevel(matt_str1_av, level = "lower"))
(low_clean_2 <- low2[, c("degree", 
                         "weighted.closeness", 
                         "nestedrank")])


(high2 <- specieslevel(matt_str1_av, level = "higher"))
(high_clean_2 <- high2[, c("degree", 
                           "weighted.closeness", 
                           "nestedrank")])

## Network metrics 
# Calcul des indices globaux les plus importants
(res_network_2 <- networklevel(matt_str1_av, 
                               index = c("connectance", 
                                         "NODF", 
                                         "modularity", 
                                         "robustness")))

## Group-level metrics 
grouplevel(matt_str1_av,
           index = c( "mean number of shared partners",
                      "togetherness",
                      "C score"))


### 6.LINEAR MIXED EFFECT MODEL ----
## Data
data_reg <- read_excel(here("data","interviews","Q1","Dataset_Q1_clean.xlsx"),
                         sheet = "perceived_success")

# Table for regressions
reg <- data_reg %>%
  dplyr::mutate (across(
    .col = c(3:16, 21:34) , ~ dplyr::case_when(
      . == "Not at all" ~ 0,
      . == "Slightly" ~ 0.25,
      . == "Moderately" ~ 0.5,
      . == "Highly" ~ 0.75,
      . == "Fully" ~ 1,
      . == "I don't know" ~ NA,
    )))

## Pivot the table to get the format for the regressions
colonnes_fixes <- c("Stakeholders_categories", "years_in_project", 
                    "involvement_level", "Degree", "ID", "Nestedrank")  # List of columns to keep, other than the strategy columns

reg_final <- reg %>%
  # strategy columns in long format
  tidyr::pivot_longer(
    cols = -any_of(colonnes_fixes), 
    names_to = "temp_name", 
    values_to = "valeur"
  ) %>%
  # determine whether it is success or involvement
  dplyr::mutate(
    type_mesure = ifelse(str_detect(temp_name, fixed("(involvement)")), "involvement", "Perceived_success"),
    
    # clean strategy's name 
    strategy = str_remove(temp_name, fixed("(involvement)"))
  ) %>%
  # deleting the temporary column that still contains the suffixes
  dplyr::select(-temp_name) %>%
  # pivot to have one column success and one involvement
  tidyr::pivot_wider(
    names_from = type_mesure,
    values_from = valeur
  )

reg_final$strategy <- gsub("_", " ", reg_final$strategy)

# logic order and format change 
ordre <- c("Occasionally", "Moderately", "Significantly", "Exclusively")

reg_final <- reg_final %>%
  dplyr::mutate(across(c(involvement_level), 
                ~ factor(.x, levels = ordre, ordered = TRUE)), # convertir les caractères en facteur avec un ordre
         Stakeholders_categories = as.factor(Stakeholders_categories),
         strategy = as.factor(strategy),
         ID = as.factor(ID)
  )
str(reg_final)

# join bipartite metrics
metrics = low_clean_1 %>% 
  dplyr::select(1:2) %>% 
  rownames_to_column(var="ID")
reg_final = reg_final %>% 
  dplyr::left_join(metrics, by="ID")

## MODEL SELECTION
model1 <- lmer(Perceived_success ~ degree * involvement + years_in_project + (1|ID), REML=FALSE, data = reg_final)
model2 <- lmer(Perceived_success ~ normalised.degree * involvement + years_in_project + (1|ID), REML=FALSE, data = reg_final)
cand.set <- list(
  degree = model1,
  normalised_degree = model2
)
aictab(cand.set)

# Model interpretation
model1 <- lmer(Perceived_success ~ degree * involvement + years_in_project + (1|ID), REML=TRUE, data = reg_final)
performance::check_model(model1, check = c("normality", "qq", "homogeneity", "outliers"))
summary(model1, correlation = T)

# Model predictions 
pred1 <- ggpredict(model1, terms = c("involvement", "degree"))

plot(pred1) + 
  labs( 
    x = "Level of involvement",
    y = "Predicted perceived success",
    colour = "Bipartite node degree") +
  theme_minimal()


pred2 <- ggpredict(model1, terms = c("degree", "involvement"))

pred = plot(pred2) + 
  labs(
    x = "Bipartite node degree",
    y = "Predicted perceived success",
    colour = "Level of involvement",
    fill = "Level of involvement" 
  ) +

  scale_fill_manual(labels = c("Very low", 
                               "Low",
                               "Moderate",
                               "High",
                               "Very high"),
                    values = c("#01016f",
                               "#6497bf",
                               "#5a5a5a",
                               "#de8d3a",
                               "#d8031c")) +
  scale_colour_manual(labels = c("Very low", 
                               "Low",
                               "Moderate",
                               "High",
                               "Very high"),
                    values = c("#01016f",
                               "#6497bf",
                               "#5a5a5a",
                               "#de8d3a",
                               "#d8031c")) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        fill = NA,
        linewidth = 1.2
      )
    ),
    fill = "none" # to remove the filled boxes from the legend
  ) +
  
  theme_minimal (base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)), # Titre X plus grand
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)), # Titre Y plus grand
    axis.text.x = element_text(size = 10),                                         # Chiffres sur l'axe X
    axis.text.y = element_text(size = 10),
    
    title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),                 
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Export
png(here("outputs","plot","Axis3","03a_SNAs_LMM.png"), width = 2500, height = 1500, res = 300, type="cairo")
plot(pred)
dev.off()


###7.CORRELATIONS-------

#### Number of respondents ~ node degree -----

# 1. Node degree
degree_8 <- igraph::degree(net_q8av)
degree_9 <- igraph::degree(net_q9av)
degree_10 <- igraph::degree(net_q10av)

# 2. Number of respondents at each question
nb_respondents_8 <- data_q8 %>%
    dplyr::group_by(Stakeholders_categories) %>%
    dplyr::summarise(nb_respondents_8 = n())

nb_respondents_9 <- data_q9 %>%
    dplyr::group_by(Stakeholders_categories) %>%
    dplyr::summarise(nb_respondents_9 = n())

nb_respondents_10 <- data_q10 %>%
    dplyr::group_by(Stakeholders_categories) %>%
    dplyr::summarise(nb_respondents_10 = n())


# 3. Join tables
df_8 <- data.frame(
  Stakeholders_categories= names(degree_8),
  degree = as.numeric(degree_8)
)
View(df_8)

df_9 <- data.frame(
  Stakeholders_categories= names(degree_9),
  degree = as.numeric(degree_9)
)

df_10 <- data.frame(
  Stakeholders_categories= names(degree_10),
  degree = as.numeric(degree_10)
)

# On fusionne avec le nombre de répondants
df8_f <- left_join(df_8, nb_respondents_8, by = "Stakeholders_categories")
df9_f <- left_join(df_9, nb_respondents_9, by = "Stakeholders_categories")
df10_f <- left_join(df_10, nb_respondents_10, by = "Stakeholders_categories")

# corrélation 
r8 <- cor(df8_f$nb_respondents_8, df8_f$degree, method = "spearman", use = "complete.obs")
r9 <- cor(df9_f$nb_respondents_9, df9_f$degree, method = "spearman", use = "complete.obs")
r10 <- cor(df10_f$nb_respondents_10, df10_f$degree, method = "spearman", use = "complete.obs")

# Afficher le résultat
print(r8)
print(r9)
print(r10)

### WITHOUT ONG ###
nb_8 <- data_q8 %>%
    dplyr::group_by(Stakeholders_categories) %>%
    dplyr::filter(Stakeholders_categories != "NGO") %>%
    dplyr::summarise(nb_8 = n())

nb_9 <- data_q9 %>%
    dplyr::group_by(Stakeholders_categories) %>%
    dplyr::filter(Stakeholders_categories != "NGO") %>%
    dplyr::summarise(nb_9 = n())

nb_10 <- data_q10 %>%
    dplyr::group_by(Stakeholders_categories) %>%
    dplyr::filter(Stakeholders_categories != "NGO") %>%
    dplyr::summarise(nb_10 = n())

df8 <- data.frame(
  Stakeholders_categories= names(degree_8),
  degree = as.numeric(degree_8)
)

df9 <- data.frame(
  Stakeholders_categories= names(degree_9),
  degree = as.numeric(degree_9)
)

df10 <- data.frame(
  Stakeholders_categories= names(degree_10),
  degree = as.numeric(degree_10)
)

# On fusionne avec le nombre de répondants
df8_f <- left_join(df8, nb_8, by = "Stakeholders_categories")
df9_f <- left_join(df_9, nb_9, by = "Stakeholders_categories")
df10_f <- left_join(df_10, nb_10, by = "Stakeholders_categories")

# corrélation 
r8 <- cor(df8_f$nb_8, df8_f$degree, method = "spearman", use = "complete.obs")
r9 <- cor(df9_f$nb_9, df9_f$degree, method = "spearman", use = "complete.obs")
r10 <- cor(df10_f$nb_10, df10_f$degree, method = "spearman", use = "complete.obs")

# Afficher le résultat
print(r8)
print(r9)
print(r10)

#### With multilayer networks (global degree) -----
# Extraction des degrés globaux 
all_degree <- multinet::degree_ml(mnet2)
respondents <- actors_ml(mnet2)$actor #On extrait la colonne de texte brute "$actor" renvoyée par multinet pour pas qu'elle s'appelle actor 

# Création du tableau de comparaison
df_multicouche <- data.frame(
  Stakeholders_categories = respondents,
  total_degree = as.numeric(all_degree)
)
View(df_multicouche)

# Fusion avec les répondants
# j'utilise le nombre de répondant du réseau 8 car c'est le même pour tous 
df_final_multicouche <- left_join(df_multicouche, nb_respondents_8, by = "Stakeholders_categories")
View(df_final_multicouche)

df_final_multicouche <- df_final_multicouche[c(1:3, 5:15),] # si on veut enlever le nombre de répondants 

# Coefficient de Spearman global
r_multicouche <- cor(df_final_multicouche$nb_respondents_8, df_final_multicouche$total_degree, method = "spearman", use = "complete.obs")
print(r_multicouche)

###8.ROBUSTNESS OF THE NETWORK -------

#### Degree on edge ----
nsim=1000
# set up function for edge deletion for betweenness
resamp.edge.bt <- function(x) {
  # Calcule le degré initial (mode = "all" pour réseau non-orienté, utilise "in" ou "out" si orienté)
  bt.g <- igraph::degree(x, mode = "all", loops = FALSE)
  mat <- as.matrix(x)
  dim.x <- dim(mat)[1]
  out.mat <- matrix(NA, nsim, 9)  # create matrix for output and then name
  colnames(out.mat) <- c("S90", "S80", "S70", "S60", "S50", "S40", "S30", 
                         "S20", "S10")
  # this double loop goes through each sampling fraction and each random
  for (j in 1:9) {
    for (i in 1:nsim) {
      # our sampling fraction is defined here using the gsize function
      # which tells us the total number of active edges in a network object
      sub.samp <- sample(seq(1, gsize(x)), size = round(gsize(x) * (j/10), 0), replace = F)
      temp.net <- x
      net.reduced <- igraph::delete_edges(temp.net, sub.samp)
      temp.stats <- igraph::degree(net.reduced, mode = "all", loops = FALSE)
      # calculate spearman's rho for replicate and output result
      out.mat[i, j] <- cor(temp.stats, bt.g, method = "spearman")
    }
  }
  return(out.mat)
}  # return results

matrice_robustness_q8 <- resamp.edge.bt(net_q8av)
matrice_robustness_q9 <- resamp.edge.bt(net_q9av)
matrice_robustness_q10 <- resamp.edge.bt(net_q10av)

# display results as boxplot
bet.plot<-boxplot(matrice_robustness_q8,
                  ylim = c(-1, 1), 
                  main = "Degree collaboration", 
                  xlab = "Sampling fraction", 
                  ylab = "Spearmans rho")

bet.plot<-boxplot(matrice_robustness_q9,
                  ylim = c(-1, 1), 
                  main = "Degree information", 
                  xlab = "Sampling fraction", 
                  ylab = "Spearmans rho")

bet.plot<-boxplot(matrice_robustness_q10,
                  ylim = c(-1, 1), 
                  main = "Degree dependency", 
                  xlab = "Sampling fraction", 
                  ylab = "Spearmans rho")

#### Degree on nodes ----
resamp.test <- function(x) {
  # the next line can be replaced with any centrality measure you'd like
  bw.g <- sna::degree(x, gmode = "graph")
  mat <- as.matrix(x)
  dim.x <- dim(mat)[1]
  out.mat <- matrix(NA, nsim, 9)  # create matrix for output and then name
  colnames(out.mat) <- c("S90", "S80", "S70", "S60", "S50", "S40", "S30",
                         "S20", "S10")
  # this double loop goes through each sampling fraction and each random
  # replicate to cacluate centrality statistics and runs a Spearman's rho
  # correlation between the resulting centrality values and the original
  # sample
  for (j in 1:9) {
    for (i in 1:nsim) {
      # this sampling procedure samples without replacement from a sequence from 1
      # to the total number of nodes, the size of the sample being determined by
      # 10-j/10. For example if j=1, 10-1/10 = 0.9 or a 90% sub-sample.
      sub.samp <- sample(seq(1, dim.x), size = round(dim.x * ((10 - j)/10),
                                                     0), replace = F)
      # calculate the betweenness statistic for the matrix reduced to only include
      # the sub-sampled rows/columns
      temp.stats <- sna::degree(mat[sub.samp, sub.samp], gmode = "graph")
      # calcuate spearman's rho for replicate
      out.mat[i, j] <- suppressWarnings(cor(temp.stats, bw.g[sub.samp],
                                            method = "spearman"))
    }
  }
  return(out.mat)
}  # return the result

# display results as boxplot by sampling fraction
boxplot(resamp.test(mat_sym_q8), 
        ylim = c(0, 1), 
        main = "BR - Degree collaboration", 
        xlab = "sampling fraction",
        ylab = "Spearmans rho")

boxplot(resamp.test(mat_sym_q9), 
        ylim = c(0, 1), 
        main = "BR - Degree information", 
        xlab = "sampling fraction",
        ylab = "Spearmans rho")

boxplot(resamp.test(mat_sym_q10), 
        ylim = c(0, 1), 
        main = "BR - Degree dependency", 
        xlab = "sampling fraction",
        ylab = "Spearmans rho")


### 9.QUALITATIVE ANALYSIS ----
#### Most important factors for the success of the GLTCP ####
# Tableau avec la liste de toutes les réponses 
data_success <- tibble(
  reponse = c(
    "Concientizacao da populacao", "Local awareness and buy-in from land owners", "Comunicação", "Comunicação.",
    "Alcance junto a população regional", 
    "Conscientização e envolvimento da população, bem como divulgação dos sucessos do programa", 
    "Disseminação da cultura preservacionista", 
    "Educação das populações","Área protegidas", "Áreas protegidas", "Conectividade", "Confecção dos fragmentos floretais", 
    "Restauração florestal nas margens de cursos d'água", 
    "Reserva MD", "Population connectivity", "Habitat extenso e conectado", "Reflorestamento", 
    "Restauração florestal em geral", "Floresta contigua", "Healthy habitat", 
    "Continued expansion of forested area and connectivity across the GLT-relevant landscape", 
    "Restauracao das matas em areas de pasto","Mais áreas protegidas", "Restauração ecológica", 
    "Connecting corridors and replanting local forests", "Seriedade nos trabalhos", "Comprometimento da equipe", "Comprometimento da equipe.",
    "Equipe entende e participa da definição da meta e dos objetivos do programa", "great, passionate, knowledgeable people", 
    "Dedicação", "Equipe engajada e motivada", "Compromisso", "Profissionalismo", "Pessoas interessadas e preparadas para o serviço", 
    "equipe comprometida", "Esforço", "Engajamento", "Engajamento da.equipe", "Trabalho", "Equipe excelente, super comprometida e proativa", 
    "Seriedade", "dedicaçao","planejamento estratégico com manejo adaptativo", "a continuous cycle of participatory strategic planning",
    "Planejamento", "Programa baseado em transdisciplinaridade", "Conhecimento científico consolidado", "science-based", 
    "Monitoramento constante  das ações", "Multidisciplinaridade","Continuidade", "Continuidade.", "Trabalho contínuo ininterrompu ao longo do tempo", 
    "consistent involvement long term", "Histórico conservacionista na região", "Conequitadas", "good, long reputation", 
    "O fato do Mico-leão-dourado fazer parte de uma fauna bonita e estar ameaçado","Long-term financial sustainability of AMLD as the lead conservation actor for GLTs.", 
    "Financiamento confiável", "Estratégias de captação de recursos", "Fontes de financiamento internacional", "Apoio", 
    "Disponibilidade de recursos", "recursos suficientes", "Recursos financeiro para manter os projeto e a instituição", 
    "Transparent and efficient financial management", "aporte financeiro", "Comprometimento de parceiros, doadores, apoiadores",
    "ONG", "Cooperação internacional tem sido importante", "strong NGOs", "Diálogo com diferentes atores no cenário local, regional, nacional e internacional", 
    "Cooperacao", "múltiplos parceiros", "capable locally based staff and strong collaborations",
    "Comprometimento institutional e de parceiros", "Interesse de instituições e profissionais", "Investimentos de orgãos e instituições nacionais e internacionais", 
    "Parceria com outras instituições", "Mais proprietários ruas envolvidos nas atividades", "Parcerias com outras instituições", 
    "Políticas de proteção ao MLD e à Mata Atlântica", "Government support through land protection, legal support and financial", 
    "Ferramentas de gestão institucionais", "Strong governmental engagement in forest protection",
    "Engajamento da sociedade", "Engajamento social", "Engajamento das comunidades locais", "Bom relacionamento com moradores da região", 
    "Interação entre diversos atores da sociedade", "Engajamento local","Proprietários parceiros", 
    "Compreensão das necessidades dos atores locais, principalmente os agricultores", "Maior engajamento da comunidade e proprietários rurais",
    "Gestão", "Monitoramento", "Com fluxo de micos", "Translocação de micos","presença constante no território", 
    "Monitoramento de caça e tráfico de animais", "Monitoramento dos grupos de mico-leão-dourado e prevenção do tráfico",
    "Controle das doenças", "Vacinacao contra a febre amarela", "Disease control",
    "Monitoramento das espécies invasoras"
  )
)

# Attribuer les catégories aux réponses 
data_success <- data_success %>%
  mutate(
    category_success = case_when(
      reponse %in% c("Concientizacao da populacao", "Local awareness and buy-in from land owners", "Comunicação", "Comunicação.",
                     "Alcance junto a população regional", "Conscientização e envolvimento da população, bem como divulgação dos sucessos do programa", 
                     "Disseminação da cultura preservacionista", "Educação das populações") 
      ~ "Public awareness/education",
      
      reponse %in% c("Área protegidas", "Áreas protegidas", "Conectividade", "Confecção dos fragmentos floretais", 
                     "Restauração florestal nas margens de cursos d'água", "Reserva MD", "Population connectivity", 
                     "Habitat extenso e conectado", "Reflorestamento", "Restauração florestal em geral", "Floresta contigua", 
                     "Healthy habitat", "Continued expansion of forested area and connectivity across the GLT-relevant landscape", 
                     "Restauracao das matas em areas de pasto", "Mais áreas protegidas", "Restauração ecológica", 
                     "Connecting corridors and replanting local forests") 
      ~ "Restoration/protection of forests and habitats",
      
      reponse %in% c("Seriedade nos trabalhos", "Comprometimento da equipe", "Comprometimento da equipe.", "Equipe entende e participa da definição da meta e dos objetivos do programa", 
                     "great, passionate, knowledgeable people", "Dedicação", "Equipe engajada e motivada", "Compromisso", 
                     "Profissionalismo", "Pessoas interessadas e preparadas para o serviço", "equipe comprometida", 
                     "Esforço", "Engajamento", "Engajamento da.equipe", "Trabalho", "Equipe excelente, super comprometida e proativa", 
                     "Seriedade", "dedicaçao") 
      ~ "Pro engagement and teamwork",
      
      reponse %in% c("planejamento estratégico com manejo adaptativo", "a continuous cycle of participatory strategic planning", 
                     "Planejamento", "Programa baseado em transdisciplinaridade", "Conhecimento científico consolidado", 
                     "science-based", "Monitoramento constante  das ações", "Multidisciplinaridade") 
      ~ "Adaptive program management mode",
      
      reponse %in% c("Continuidade", "Continuidade.", "Trabalho contínuo ininterrompu ao longo do tempo", "consistent involvement long term", 
                     "Histórico conservacionista na região", "Conequitadas", "good, long reputation", 
                     "O fato do Mico-leão-dourado fazer parte de uma fauna bonita e estar ameaçado") 
      ~ "Sustaining the program over the long term",
      
      reponse %in% c("Long-term financial sustainability of AMLD as the lead conservation actor for GLTs.", 
                     "Financiamento confiável", "Estratégias de captação de recursos", "Fontes de financiamento internacional", 
                     "Apoio", "Disponibilidade de recursos", "recursos suficientes", "Recursos financeiro para manter os projeto e a instituição", 
                     "Transparent and efficient financial management", "aporte financeiro", "Comprometimento de parceiros, doadores, apoiadores") 
      ~ "Financial aspect",
      
      reponse %in% c("ONG", "Cooperação internacional tem sido importante", "strong NGOs", 
                     "Diálogo com diferentes atores no cenário local, regional, nacional e internacional", 
                     "Cooperacao", "múltiplos parceiros", "capable locally based staff and strong collaborations") 
      ~ "Effective programme governance, long-term commitment from organisms",
      
      reponse %in% c("Comprometimento institutional e de parceiros", "Interesse de instituições e profissionais", 
                     "Investimentos de orgãos e instituições nacionais e internacionais", "Parceria com outras instituições", 
                     "Mais proprietários ruas envolvidos nas atividades", "Parcerias com outras instituições", 
                     "Políticas de proteção ao MLD e à Mata Atlântica", "Government support through land protection, legal support and financial", 
                     "Ferramentas de gestão institucionais", "Strong governmental engagement in forest protection") 
      ~ "Effective programme governance, institutional partners commitment",
      
      reponse %in% c("Engajamento da sociedade", "Engajamento social", "Engajamento das comunidades locais", 
                     "Bom relacionamento com moradores da região", "Interação entre diversos atores da sociedade", 
                     "Engajamento local") 
      ~ "Effective programme governance, involvement of local communities",
      
      reponse %in% c("Proprietários parceiros", "Compreensão das necessidades dos atores locais, principalmente os agricultores", 
                     "Maior engajamento da comunidade e proprietários rurais") 
      ~ "Effective programme governance, involvement of local landowners",
      
      reponse %in% c("Gestão", "Monitoramento", "Com fluxo de micos", "Translocação de micos") 
      ~ "Monitoring of the tamarin population",
      
      reponse %in% c("presença constante no território", "Monitoramento de caça e tráfico de animais", 
                     "Monitoramento dos grupos de mico-leão-dourado e prevenção do tráfico") 
      ~ "Illegal trade prevention in tamarins",
      
      reponse %in% c("Controle das doenças", "Vacinacao contra a febre amarela", "Disease control") 
      ~ "Monitoring of diseases threats",
      
      reponse == "Monitoramento das espécies invasoras" 
      ~ "Monitoring of invasive species",
      
      TRUE ~ "Non classé" 
    )
  )

# Verification
table(data_success$category_success)
unique(data_success$category_success)

# Stats sur les réponses 
stat_success <- data_success %>%
  count(category_success, name = "number_success") %>%
  mutate(percentage_success = (number_success / sum(number_success)) * 100) %>%
  arrange(desc(percentage_success))

# Génération du graphique
# pourcentage
ggplot(stat_success, aes(x = reorder(category_success, percentage_success), y = percentage_success)) +
  geom_col(fill = "#BF3EFF", color = "black", width = 0.7) +
  coord_flip() + # Aligne les barres horizontalement 
  labs(
    x = "Category",
    y = "Percentage of factor for the success of GLTCP (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percentage_success, 1), "%")), 
            hjust = -0.1, size = 3.5) # Une seule parenthèse ici !# Ajoute la valeur du % au bout de chaque barre

# nombre
ggplot(stat_success, aes(x = reorder(category_success, number_success), y = number_success)) +
  geom_col(fill = "#68228B", color = "black", width = 0.7) +
  coord_flip() + # Aligne les barres horizontalement 
  labs(
    x = "Category",
    y = "Number of factor for the success of GLTCP"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(number_success)), 
            hjust = -0.2, size = 3.5)

## Si on regroupe tous les types de gouvernance 
stat_success_gouv <- data_success %>%
  mutate(category_success = if_else(
    str_detect(category_success, "^Effective programme governance"), 
    "Effective programme governance", 
    category_success               
  )) %>%
  count(category_success, name = "number_success") %>%
  mutate(percentage_success = (number_success / sum(number_success)) * 100) %>%
  arrange(desc(percentage_success))

# Génération du graphique
# pourcentage
ggplot(stat_success_gouv, aes(x = reorder(category_success, percentage_success), y = percentage_success)) +
  geom_col(fill = "#BF3EFF", color = "black", width = 0.7) +
  coord_flip() + # Aligne les barres horizontalement 
  labs(
    x = "Category",
    y = "Percentage of factor for the success of GLTCP (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percentage_success, 1), "%")), 
            hjust = -0.1, size = 3.5) # Une seule parenthèse ici !# Ajoute la valeur du % au bout de chaque barre

# Nombre
ggplot(stat_success_gouv, aes(x = reorder(category_success, number_success), y = number_success)) +
  geom_col(fill = "#68228B", color = "black", width = 0.7) +
  coord_flip() + # Aligne les barres horizontalement 
  labs(
    x = "Category",
    y = "Number of factor for the success of GLTCP"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(number_success)), 
            hjust = -0.2, size = 3.5)

#### Significant challenges in implementing the GLTCP ####
# Tableau avec la liste de toutes les réponses 
data_challenge <- tibble(
  reponse = c(
    # Public awareness/education
    "Informar apropriadamente o público adulto local", "Consciência ecológica da comunidade", 
    "Conscientização", "pouco conhecimento das comunidades sobre o trabalho desenvolvido", 
    "Desenvolver o turismo", "Falta de consciência da população", 
    "Divulgação científica de métodos e resultados", "Lack of empathy",
    "Aprender sobre o comportamento dos micos", "Lack of knowledge", 
    "Incentivar jovens da região para carreiras em ciências biologicas", 
    "Capacitação de mão de obra técnico científica",
    "Área protegida", "ter uma área continua de florestas de boa qualidade", 
    "Falta de áreas para restauração - liberação de áreas para corredores", 
    "Aumento da cobertura florestal", "Áreas", "Construcao de corredores florestais", 
    "Aumento da conectividade de fragmentos", "Fazer os corredores", "Floresta contigua", 
    "Grande quantidade de áreas a serem restauradas",
    "Instabilidade financeira", "recursos variáveis", "Falta de apoio financeiro", "Verbas", 
    "Raising sufficient funding to retain competent staff to carry out technical areas of the program (GIS, digital communications)", 
    "A verba prometida pela FAPERJ não será recebida, limitando a aquisição de materiais previstos", 
    "Sustentabilidade dos recursos financeiros", "Necessidade contínua na arrecadação de fundos", 
    "Current heavy need for fund-raising to support basic AMLD operations", "human greed", 
    "sustainability, both financial and staff", 
    "obtaining long-term funding commitments - conservation is long-term.  Significant results cannot be achieved in one year", 
    "Falta de opções économiques para o uso da terra (floresta em pé não dá dinheiro)", 
    "Disponibilidae de recursos", "Recursos financeiros",
    "sustainability of the NGOs", "Falta de apoio dos órgãos públicos", 
    "Alinhamento da agenda dos pesquisadores", "Equipe reduzida", "Participação dos órgãos públicos", 
    "Ampliação da área de ocorrência do mico-leão-dourado", "Maior sinergia entre as instituições", 
    "Reconhecimento do program e integração do projeto pelas instituições", "Governo Federal/ politica", "changing policy and government in Brazil",
    "Lack of government programming assisting", "Leia ambientais mais severas", "Implementação de políticas socioambientais", 
    "Participação da comunidade", "Convencimento de grandes proprietários de terra", 
    "Convencimento de proprietários rurais (não há fiscalização ambiental o suficiente)", "Resistência dos proprietários em restaurar Áreas de Proteção Permanente e Reserva Legal", 
    "Tornar áreas de pasto em florestas", "Convencimento dos pecuaristas", "Participação dos donos de terras", 
    "Conseguir apoio.dos proprietarios", "População de agricultores envelhecendo", "População e proprietários rurais comprometidos",
    "Instabilidade política e corrupção institucionalizada afetando a gestão pública", 
    "Mudanças de uso da terra por incompetência do poder público ou por manejo de proprietários privados", 
    "Federal programs allowing land clearing in GLT habitat", "Baixa prioridade das causas ambientais no contexto político e econômico nacional", 
    "Pagamento de PSA para quem protege", "Externalidades negativas, como mudanças de governo e forças políticas atuantes", 
    "Polarização política proposital apartando segmentos sociais e desprezando donos de terra nas ações de conservação", 
    "Alinhar políticas públicas ao desenvolvimento sustentável", "Crimes ambientais sem penalização", 
    "politicas variáveis", "Banalização e crescimento do crime gerando tráfico de animais, incêndios florestais e violência no campo",
    "Monitoramento", "Controle do tráfico de animais", "Combate ao tráfico de animais", "Tráfico de Micos", 
    "Falta de fiscalização constante (tráfico de animais e desmatamento)", "Tráfico de animais silvestres", 
    "garantir proteção à captura ilegal para o tráfico", "Controle sobre o tráfico", 
    "Retorno do tráfico de animais, associado ao crime organizado", "Potencial ferrovia (ou outros rompimentos do habitat)", "Controle do desmatamento", 
    "disputa de território para o mico", "Desmatamento", "Desmatamento.", "Urbanização", "continued threats (yellow fever, linear infrastructure, politics)", 
    "Mudanças climáticas", "Ongoing urban expansion, other development", "pressões sociais sobre meio ambiente continuas", "Ameaças",
    "Possible railway construction", "emergência de doenças de dificil contrôle", "Uso de práticas sustentáveis em áreas rurais da região"
  )
)

# Attribuer les catégories aux réponses 
data_challenge <- data_challenge %>%
  mutate(
    category_challenge = case_when(
      reponse %in% c("Informar apropriadamente o público adulto local", "Consciência ecológica da comunidade", 
                     "Conscientização", "pouco conhecimento das comunidades sobre o trabalho desenvolvido", 
                     "Desenvolver o turismo", "Falta de consciência da população", 
                     "Divulgação científica de métodos e resultados", "Lack of empathy") 
      ~ "Public awareness/education",
      
      reponse %in% c("Aprender sobre o comportamento dos micos", "Lack of knowledge", 
                     "Incentivar jovens da região para carreiras em ciências biologicas", 
                     "Capacitação de mão de obra técnico científica") 
      ~ "Improving scientific knowledge about tamarins and the programme",
      
      reponse %in% c("Área protegida", "ter uma área continua de florestas de boa qualidade", 
                     "Falta de áreas para restauração - liberação de áreas para corredores", 
                     "Aumento da cobertura florestal", "Áreas", "Construcao de corredores florestais", 
                     "Aumento da conectividade de fragmentos", "Fazer os corredores", "Floresta contigua", 
                     "Grande quantidade de áreas a serem restauradas") 
      ~ "Restoration/protection of forests and habitats",
      
      reponse %in% c("Instabilidade financeira", "recursos variáveis", "Falta de apoio financeiro", "Verbas", 
                     "Raising sufficient funding to retain competent staff to carry out technical areas of the program (GIS, digital communications)", 
                     "A verba prometida pela FAPERJ não será recebida, limitando a aquisição de materiais previstos", 
                     "Sustentabilidade dos recursos financeiros", "Necessidade contínua na arrecadação de fundos", 
                     "Current heavy need for fund-raising to support basic AMLD operations", "human greed", 
                     "sustainability, both financial and staff", 
                     "obtaining long-term funding commitments - conservation is long-term.  Significant results cannot be achieved in one year", 
                     "Falta de opções économiques para o uso da terra (floresta em pé não dá dinheiro)", 
                     "Disponibilidae de recursos", "Recursos financeiros") 
      ~ "Financial aspect",
      
      reponse %in% c("sustainability of the NGOs", "Falta de apoio dos órgãos públicos", 
                     "Alinhamento da agenda dos pesquisadores", "Equipe reduzida", "Participação dos órgãos públicos", 
                     "Ampliação da área de ocorrência do mico-leão-dourado") 
      ~ "Effective programme governance, long-term commitment from organisms",
      
      reponse %in% c("Maior sinergia entre as instituições", "Reconhecimento do program e integração do projeto pelas instituições", 
                     "Governo Federal/ politica", "changing policy and government in Brazil", "Lack of government programming assisting", 
                     "Leia ambientais mais severas", "Implementação de políticas socioambientais") 
      ~ "Effective programme governance, institutional partners commitment",
      
      reponse == "Participação da comunidade" 
      ~ "Effective programme governance, involvement of local communities",
      
      reponse %in% c("Convencimento de grandes proprietários de terra", "Convencimento de proprietários rurais (não há fiscalização ambiental o suficiente)", 
                     "Resistência dos proprietários em restaurar Áreas de Proteção Permanente e Reserva Legal", 
                     "Tornar áreas de pasto em florestas", "Convencimento dos pecuaristas", "Participação dos donos de terras", 
                     "Conseguir apoio.dos proprietarios", "População de agricultores envelhecendo", 
                     "População e proprietários rurais comprometidos") 
      ~ "Effective programme governance, involvement of local landowners",
      
      reponse %in% c("Instabilidade política e corrupção institucionalizada afetando a gestão pública", 
                     "Mudanças de uso da terra por incompetência do poder público ou por manejo de proprietários privados", 
                     "Federal programs allowing land clearing in GLT habitat", 
                     "Baixa prioridade das causas ambientais no contexto político e econômico nacional", 
                     "Pagamento de PSA para quem protege", "Externalidades negativas, como mudanças de governo e forças políticas atuantes", 
                     "Polarização política proposital apartando segmentos sociais e desprezando donos de terra nas ações de conservação", 
                     "Alinhar políticas públicas ao desenvolvimento sustentável", "Crimes ambientais sem penalização", 
                     "politicas variáveis", "Banalização e crescimento do crime gerando tráfico de animais, incêndios florestais e violência no campo") 
      ~ "Political instability",
      
      reponse == "Monitoramento" 
      ~ "Monitoring of the tamarin population",
      
      reponse %in% c("Controle do tráfico de animais", "Combate ao tráfico de animais", "Tráfico de Micos", 
                     "Falta de fiscalização constante (tráfico de animais e desmatamento)", "Tráfico de animais silvestres", 
                     "garantir proteção à captura ilegal para o tráfico", "Controle sobre o tráfico", 
                     "Retorno do tráfico de animais, associado ao crime organizado") 
      ~ "Illegal trade prevention in tamarins",
      
      reponse %in% c("Potencial ferrovia (ou outros rompimentos do habitat)", "Controle do desmatamento", 
                     "disputa de território para o mico", "Desmatamento", "Desmatamento.", "Urbanização") 
      ~ "Habitat fragmentation, deforestation",
      
      reponse %in% c("continued threats (yellow fever, linear infrastructure, politics)", "Mudanças climáticas", 
                     "Ongoing urban expansion, other development", "pressões sociais sobre meio ambiente continuas", 
                     "Ameaças", "Possible railway construction") 
      ~ "Human pressures on the tamarin’s environment",
      
      reponse == "emergência de doenças de dificil contrôle" 
      ~ "Monitoring of diseases threats",
      
      reponse == "Uso de práticas sustentáveis em áreas rurais da região" 
      ~ "Sustainable practices",
      
      TRUE ~ "Non classé"
    )
  )

# Verification 
table(data_challenge$category_challenge)
unique(data_challenge$category_challenge)

# Stats sur les réponses 
stat_challenge <- data_challenge %>%
  count(category_challenge, name = "number_challenge") %>%
  mutate(percentage_challenge = (number_challenge / sum(number_challenge)) * 100) %>%
  arrange(desc(percentage_challenge))

# Génération du graphique 
# pourcentage
ggplot(stat_challenge, aes(x = reorder(category_challenge, percentage_challenge), y = percentage_challenge)) +
  geom_col(fill = "#CD6889", color = "black", width = 0.7) +
  coord_flip() + # Aligne les barres horizontalement 
  labs(
    x = "Category",
    y = "Percentage of challenge in implementing the GLTCP"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percentage_challenge, 1), "%")), 
            hjust = -0.1, size = 3.5) # Une seule parenthèse ici !# Ajoute la valeur du % au bout de chaque barre

# nombre
ggplot(stat_challenge, aes(x = reorder(category_challenge, number_challenge), y = number_challenge)) +
  geom_col(fill = "#8B0A50", color = "black", width = 0.7) +
  coord_flip() + # Aligne les barres horizontalement 
  labs(
    x = "Category",
    y = "Number of challenge in implementing the GLTCP"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(number_challenge)), 
            hjust = -0.2, size = 3.5)

## Si on regroupe tous les types de gouvernance 
stat_challenge_gouv <- data_challenge %>%
  mutate(category_challenge = if_else(
    str_detect(category_challenge, "^Effective programme governance"), 
    "Effective programme governance", 
    category_challenge                
  )) %>%
  count(category_challenge, name = "number_challenge") %>%
  mutate(percentage_challenge = (number_challenge / sum(number_challenge)) * 100) %>%
  arrange(desc(percentage_challenge))

# graphiques 
# pourcentage
ggplot(stat_challenge_gouv, aes(x = reorder(category_challenge, percentage_challenge), y = percentage_challenge)) +
  geom_col(fill = "#CD6889", color = "black", width = 0.7) +
  coord_flip() + # Aligne les barres horizontalement 
  labs(
    x = "Category",
    y = "Percentage of challenge in implementing the GLTCP"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percentage_challenge, 1), "%")), 
            hjust = -0.1, size = 3.5) # Une seule parenthèse ici !# Ajoute la valeur du % au bout de chaque barre

# nombres
ggplot(stat_challenge_gouv, aes(x = reorder(category_challenge, number_challenge), y = number_challenge)) +
  geom_col(fill = "#8B0A50", color = "black", width = 0.7) +
  coord_flip() + # Aligne les barres horizontalement 
  labs(
    x = "Category",
    y = "Number of challenge in implementing the GLTCP"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(number_challenge)), 
            hjust = -0.2, size = 3.5)


### EXPORT -------
#### Cytoscape exports ----
# Node table
readr::write_csv(
  node_table,
  here(
    "outputs",
    "data",
    "SNA",
    "Cytoscape",
    "node_table.csv"
  ))

# Intra-layer edge table
readr::write_csv(
  intra_layer_table,
  here(
    "outputs",
    "data",
    "SNA",
    "Cytoscape",
    "intra_layer_table.csv"
  ))
