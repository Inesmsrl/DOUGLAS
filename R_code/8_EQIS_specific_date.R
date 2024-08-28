################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation/Exportation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  tidyverse,           # Data management, inclus ggplot
  scico                # Palettes de couleur
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Effectifs de population par age et par année
population <- import(here("data_clean", "population_clean.xlsx"))

# Mortality rates par age et par année
MR <- import(here("data_clean", "MR_table.xlsx"))

# RR combinés par année pour chaque scénario (implémentation sigmoïdale)
combined_rr_sig <- import(here("data_clean", "combined_rr_sig.xlsx"))

# Bornes temporelles du modèle
year_i <- 2020 # Année initiale
year_c <- 2050 # Année d'intérêt

################################################################################################################################
#                                             3. Préparation des données                                                       #
################################################################################################################################

# Taux de mortalité de l'année initiale du modèle
  MR_year_i <- MR %>%
    select("age", !!sym(as.character(year_i))) %>% 
    rename("mr_year_i" = !!sym(as.character(year_i)))

# Structure de la population l'année initiale
  pop <- population %>% 
    select("age", !!sym(as.character(year_i))) %>% 
    rename("population" = !!sym(as.character(year_i)))

# RR combinés de l'année initiale et l'année d'intérêt pour chaque scénario
  rr <- combined_rr_sig %>% 
    filter(year %in% c(year_i, year_c)) %>% 
    select("scenario", "year","combined_rr")
  
################################################################################################################################
#                                             4. Ajustement des MR                                                             #
################################################################################################################################

MR_adjusted <- MR_year_i %>% 
    crossing(rr) %>% 
    mutate(adjusted_mr = mr_year_i * combined_rr / combined_rr[year == year_i]) %>% 
    select("age", "year", "scenario", "adjusted_mr")
  
################################################################################################################################
#                                             5. EQIS                                                                          #
################################################################################################################################

# Nombre de décès les années initiales et d'intérêt
  deaths <- pop %>% 
    inner_join(MR_adjusted, by = "age") %>% 
    mutate(deaths = population * adjusted_mr) %>% 
    select("age", "year", "scenario", "deaths")
  
# Nombre de décès évités/ajoutés l'année d'intérêt par rapport à l'année initiale
  avoided_deaths <- deaths %>%
    pivot_wider(names_from = "year", values_from = "deaths") %>% 
    mutate(avoided_deaths = get(as.character(year_i)) - get(as.character(year_c))) %>% 
    select("age", "scenario", "avoided_deaths")

################################################################################################################################
#                                             6. Représentation graphique                                                      #
################################################################################################################################
  
  graph_avoided_deaths_2035 <- ggplot(avoided_deaths, aes(x = age,
                                                          y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2035 compared to 2020",
         subtitle = "sigmoidal implementation of diets",
         x = "Age",
         y = "Avoided deaths")
  
  graph_avoided_deaths_2050 <- ggplot(avoided_deaths, aes(x = age,
                                                          y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "darkseagreen4")+
    labs(title = "Avoided deaths in 2050 compared to 2020",
         subtitle = "sigmoidal implementation of diets",
         x = "Age",
         y = "Avoided deaths")
  
  
################################################################################################################################
#                                             7. Exportation des données                                                       #
################################################################################################################################

# Décès évités en 2035 par rapport à 2020
  export(avoided_deaths ,here("results", "avoided_deaths_2035_2020.xlsx"))
  ggsave(here("results", "avoided_deaths_2035_2020.pdf"), plot = graph_avoided_deaths_2035)
  
# Décès évités en 2050 par rapport à 2020
  export(avoided_deaths ,here("results", "avoided_deaths_2050_2020.xlsx"))
  ggsave(here("results", "avoided_deaths_2050_2020.pdf"), plot = graph_avoided_deaths_2050)
  