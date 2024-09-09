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
population <- import(here("data_clean", "population_clean.xlsx"))

# Décès par âge de 1962 à 2021 et projetées jusqu'en 2120
deaths <- import(here("data_clean", "deaths_clean.xlsx"))


################################################################################################################################
#                                             3. Calcul des taux de mortalité                                                  #
################################################################################################################################
                      
mortality_rates <- population %>%
  mutate(across(-"age",                                               
                ~ deaths[[cur_column()]] / population[[cur_column()]]
                ))

################################################################################################################################
#                                             5. Exportation des données                                                       #
################################################################################################################################

# Projections des taux de mortalité par âge et par année 
export(mortality_rates, here("data_clean", "MR_table.xlsx"))