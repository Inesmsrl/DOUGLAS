################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr                # Manipulation des données
)


################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Effectifs de population (hommes et femmes) par âge de 1962 à 2021 et projetées jusqu'en 2120
population <- import(here("data", "INSEE_Population.xlsx"))

# Décès par âge de 1962 à 2021 et projetées jusqu'en 2120
deaths <- import(here("data", "INSEE_Deaths.xlsx"))

################################################################################################################################
#                                             3. Nettoyage des données                                                         #
################################################################################################################################

# Rendre les données homogènes (noms de variables, catégories d'âge,...) et utilisables pour l'EQIS

# Effectifs de population (H et F) par âge et par année
  population <- population %>% 
      rename("age" = "Âge au 1er janvier",
                 "2019" = "2019 (p)",
                 "2020" = "2020(p)",
                 "2021" = "2021(p)") %>%    # Renommer des variable
      select(-c("2121")) %>%                # Supprimer la colonne 2121 non présente dans "deaths"
      filter(row_number() %in% 1:106) %>%   # Supprimer la ligne Total
      mutate(age = recode(age,
                          "105+" = "105")) %>% # Transformer la valeur 105+ en 105 pour pouvoir numériser la variable age
      mutate(age = as.numeric(age))         # Transformer la colonne age en numérique
  

# Nombre de décès par âge et par année
  deaths <- deaths %>% 
      rename("age" = "Âge atteint dans l'année",
                 "2020" = "2020 p)"
                 ) %>%                      # Renommer des variables
      select(-c("...161")) %>%              # Supprimer la colonne "...161"
      filter(row_number() %in% 1:106) %>%   # Supprimer la ligne Total
      mutate(age = recode(age,
                          "105+" = "105")) %>% # Transformer la valeur 105+ en 105 pour pouvoir numériser la variable age
      mutate(age = as.numeric(age))            # Transformer la colonne age en numérique

################################################################################################################################
#                                             4. Vérifications                                                                 #
################################################################################################################################
  
# Les vérifications doivent renvoyer TRUE
  
  all(names(population == names(deaths)))   # Mêmes variables dans les jeux de données
  all(population$age == deaths$age)         # Mêmes catégories d'âge dans les jeux de données
  
################################################################################################################################
#                                             5. Exportation des données                                                       #
################################################################################################################################

# Projections des effectifs de population par âge et par année
  export(population,here("data_clean","population_clean.xlsx"))

# Projection des décès par âge et par année
  export(deaths,here("data_clean","deaths_clean.xlsx"))

