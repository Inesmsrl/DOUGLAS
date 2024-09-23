################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation/Exportation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  tidyverse            # Data management, inclus ggplot
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Population
  # Taux de mortalité (INSEE)
  MR <- import(here("data_clean", "MR_table.xlsx"))
  
  # Effectifs de population par age et par année (INSEE)
  population <- import(here("data_clean", "population_clean.xlsx"))

# Expositions : régimes SISAE en 2050
  diets <- import(here("data", "DOUGLAS_diets.xlsx"))
  
# Risques relatifs / consommation (g/j) (Fadnes, 2022 & 2024)
  rr_table <- import(here("data_clean", "rr_table_interpolated.xlsx"))
  
################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################
  
# Bornes temporelles du modèle (années)
  year_i <- 2020 # Année initiale
  year_f <- 2050 # Année finale

# Borne inférieure de l'âge de la population du modèle (années)
  age_limit <- 18
  
# Dynamique d'implémentation des régimes (immediate, linear, cosine, sigmoidal)
  implementation <- "sigmoidal"
  
  # paramètre de la courbe d'interpolation cosinus
  p <- 1
  
  # paramètre de la courbe sigmoïdale
  lambda <- 5

#  Time to full effect
  
  # durée (années)
  ttfe_time <- 10
  
  # Dynamique (linear, cosine, sigmoidal, log)
  ttfe_dynamics <- "linear"
  
# Charte graphique
  col_scenario <- c("actuel" = "azure4",
                    "sc0" = "royalblue2",
                    "sc1" = "darkseagreen4",
                    "sc2" = "aquamarine2",
                    "sc3" = "lightpink",
                    "sc4" = "maroon",
                    "sc5" = "royalblue4")
  
  col_food_groups <- c("red_meat" = "#F60239",
                       "processed_meat" = "#A40122",
                       "white_meat" = "#FF9DC8",
                       "dairy" = "#00489E",
                       "fish" = "#790149",
                       "eggs" = "#EF0096",
                       "fruits" = "#00735C",
                       "nuts" = "#FFAC3B",
                       "vegetables" = "#86FFDE",
                       "legumes" = "#00CBA7",
                       "whole_grains" = "#0079FA",
                       "reffined_grains" = "#00E5F8",
                       "added_plant_oils" = "#FF6E3A",
                       "sugar_sweetened_beverages" = "#004002")
  
# Ordonner les groupes alimentaires
  order_food_groups <- c("red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy", 
                         "fruits", "vegetables", "legumes", "nuts","whole_grains", "reffined_grains",
                         "added_plant_oils", "sugar_sweetened_beverages")

################################################################################################################################
#                                             4. Préparation des données                                                       #
################################################################################################################################
  
  # Sélectionner les MR entre les bornes temporelles du modèle et au dessus de la limite d'age
  # Pivoter le dataframe en format long
  MR_select <- MR %>% 
    select(age, !!sym(as.character(year_i)) : !!sym(as.character(year_f))) %>%
    filter(age >= age_limit) %>% 
    pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
                 names_to = "year", 
                 values_to = "MR") %>% 
    mutate(year = as.numeric(year))
  
  # Sélectionner les effectifs de population entre les bornes temporelles du modèle et au dessus de la limite d'age 
  # Pivoter le dataframe en format long
  population_select <- population %>% 
    select(age, !!sym(as.character(year_i)) : !!sym(as.character(year_f))) %>% 
    filter(age >= age_limit) %>% 
    pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
                 names_to = "year", 
                 values_to = "population") %>% 
    mutate(year = as.numeric(year)) %>% 
    arrange(age)
  
################################################################################################################################
#                                             5. Fonctions d'implémentation des régimes                                        #
################################################################################################################################
  
# Implémentation linéaire
  calc_food_q_lin <- function(q_i, q_f, year_n, year_i, year_f){
    (q_f - q_i)/(year_f - year_i) * (year_n - year_f) + q_f
  }
  
# Implémentation par interpolation cosinus
  calc_food_q_cos <- function(q_i, q_f, year_n, year_i, year_f, p) {
    q_i + (q_f - q_i) * (1 - cos(pi * ((year_n - year_i) / (year_f - year_i))^p)) / 2
  }
  
# Implémentation sigmoïdale
  calc_food_q_sig <- function(q_i, q_f, year_n, year_i, year_f, lambda) {
    (q_i+q_f)/2 + (q_f - q_i) * (1 / (1 + exp(-lambda*((year_n - year_i)/(year_f - year_i)-1/2)))-1/2)*(-1/(2*(1/(1+exp(lambda/2))-1/2)))
  }
  
################################################################################################################################
#                                             6. Evolution des régimes                                                         #
################################################################################################################################
  
# Calcul des quantités de chaque aliment consommées chaque année 
  diets_evo <- diets %>%
    select("food_group", "actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5") %>% 
    mutate(q_i = actuel) %>% 
    pivot_longer(cols = c("actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5"), 
                 names_to = "scenario", 
                 values_to = "q_f") %>%  
    crossing(year_n = year_i:year_f) %>%
    mutate(quantity = case_when(
      implementation == "immediate" ~ q_f,
      implementation == "linear" ~ mapply(calc_food_q_lin, q_i, q_f, year_n, year_i, year_f),
      implementation == "cosine" ~ mapply(calc_food_q_cos, q_i, q_f, year_n, year_i, year_f, p),
      implementation == "sigmoidal" ~ mapply(calc_food_q_sig, q_i, q_f, year_n, year_i, year_f, lambda)
    )) %>% 
    select("food_group", "scenario", "year_n", "quantity") %>% 
    rename("year" = "year_n")

# Ordonnner les groupes alimentaires
  diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)

# Visualisation graphique
  
  ggplot(data = diets_evo, aes(x = year,
                               y = quantity,
                               fill = food_group))+
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6)+
    facet_wrap(~ scenario, ncol = 4)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.8)),
          legend.position = "bottom",
          legend.text = element_text(size = 6),
          legend.title = element_text(face = "bold", size = 7),
          legend.key.size = unit(0.2, "cm"),
          plot.margin = margin(1, 1, 1, 1, "cm"))+
    scale_fill_manual(values = col_food_groups)+
    labs(title = "Diet changes",
         x = "",
         y = "Quantities (g/day/pers)",
         fill = "Food type")+
    guides(fill = guide_legend(nrow = 2, 
                               title.position = "top",
                               title.hjust = 0.5))
  