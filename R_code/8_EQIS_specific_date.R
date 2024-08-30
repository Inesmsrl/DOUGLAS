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

# Taux de mortalité de l'année d'intérêt du modèle
  MR_year_c <- MR %>%
    select("age", !!sym(as.character(year_c))) %>% 
    rename("mr_year_c" = !!sym(as.character(year_c)))

# Structure de la population l'année initiale
  pop_i <- population %>% 
    select("age", !!sym(as.character(year_i))) %>% 
    rename("population" = !!sym(as.character(year_i)))
  
# Structure de la population l'année initiale
  pop_c <- population %>% 
    select("age", !!sym(as.character(year_c))) %>% 
    rename("population" = !!sym(as.character(year_c)))
  
# RR combinés de l'année initiale et l'année d'intérêt pour chaque scénario
  rr <- combined_rr_sig %>% 
    filter(year %in% c(year_i, year_c)) %>% 
    select("scenario", "year","combined_rr")
  
# RR de référence
  rr_ref <- combined_rr_sig$combined_rr[1]
  
# Charte graphique
  col_scenario <- c("actuel" = "azure4",
                    "sc0" = "royalblue2",
                    "sc1" = "darkseagreen4",
                    "sc2" = "aquamarine2",
                    "sc3" = "lightpink",
                    "sc4" = "maroon",
                    "sc5" = "royalblue4")
  
  
  
################################################################################################################################
#                                             4. Ajustement des MR                                                             #
################################################################################################################################

# Ajustement des MR de l'année initiale du modèle
  MR_adjusted_i <- MR_year_i %>% 
    crossing(rr) %>%
    group_by(scenario) %>% 
    mutate(adjusted_mr = mr_year_i * combined_rr / combined_rr[year == year_i]) %>% 
    select("age", "year", "scenario", "adjusted_mr")
  
# Ajustement des MR de l'année d'intérêt du modèle
  MR_adjusted_c <- MR_year_c %>% 
    crossing(rr) %>% 
    group_by(scenario) %>% 
    mutate(adjusted_mr = mr_year_c * combined_rr / combined_rr[year == year_c]) %>%
    select("age", "year", "scenario", "adjusted_mr")
  
  
################################################################################################################################
#                                             5. EQIS régimes appliqués à la population initiale                               #
################################################################################################################################

# Nombre de décès les années initiales et d'intérêt
  deaths_i <- pop_i %>% 
    inner_join(MR_adjusted_i, by = "age") %>% 
    mutate(deaths = population * adjusted_mr) %>% 
    select("age", "year", "scenario", "deaths")
  
# Nombre de décès évités/ajoutés l'année d'intérêt par rapport à l'année initiale
  avoided_deaths_i <- deaths_i %>%
    pivot_wider(names_from = "year", values_from = "deaths") %>% 
    mutate(avoided_deaths = get(as.character(year_i)) - get(as.character(year_c))) %>% 
    select("age", "scenario", "avoided_deaths")
  
# Nombre total de décès évités par scénario
  total_avoided_deaths_i <- avoided_deaths_i %>% 
    group_by(scenario) %>% 
    summarize(total_avoided_deaths = sum(avoided_deaths))

################################################################################################################################
#                                             6. EQIS régimes appliqués à la population d'intérêt                              #
################################################################################################################################
  
# Nombre de décès les années initiales et d'intérêt
  deaths_c <- pop_c %>% 
    inner_join(MR_adjusted_c, by = "age") %>% 
    mutate(deaths = population * adjusted_mr) %>% 
    select("age", "year", "scenario", "deaths")
  
# Nombre de décès évités/ajoutés l'année d'intérêt par rapport à l'année initiale
  avoided_deaths_c <- deaths_c %>%
    pivot_wider(names_from = "year", values_from = "deaths") %>% 
    mutate(avoided_deaths = get(as.character(year_i)) - get(as.character(year_c))) %>% 
    select("age", "scenario", "avoided_deaths")
  
# Nombre total de décès évités par scénario
  total_avoided_deaths_c <- avoided_deaths_c %>% 
    group_by(scenario) %>% 
    summarize(total_avoided_deaths = sum(avoided_deaths))
  
################################################################################################################################
#                                             6. Représentations graphiques                                                    #
################################################################################################################################

## Régimes appliqués à la structure de population de l'année initiale du modèle

# Décès évités par âge
  graph_avoided_deaths_2035_i <- ggplot(avoided_deaths_i, aes(x = age,
                                                              y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2035 compared to 2020",
         subtitle = "sigmoidal implementation of diets applied to 2020 population",
         x = "Age",
         y = "Avoided deaths")
  
  graph_avoided_deaths_2050_i <- ggplot(avoided_deaths_i, aes(x = age,
                                                              y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "darkseagreen4")+
    labs(title = "Avoided deaths in 2050 compared to 2020",
         subtitle = "sigmoidal implementation of diets applied to 2020 population",
         x = "Age",
         y = "Avoided deaths")
  
# Total des décès évités
  graph_total_avoided_2035_i <- ggplot(data = total_avoided_deaths_i, aes(x = scenario, 
                                                                          y = total_avoided_deaths, 
                                                                          fill = scenario))+
    geom_bar(stat = "identity", alpha = 0.8)+
    labs(title = "Total avoided deaths in 2035 compared to 2020",
         subtitle = "sigmoidal implementation of diets applied to 2020 population",
         x = "",
         y = "number of avoided deaths")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  graph_total_avoided_2050_i <- ggplot(data = total_avoided_deaths_i, aes(x = scenario, 
                                                                          y = total_avoided_deaths, 
                                                                          fill = scenario))+
    geom_bar(stat = "identity", alpha = 0.8)+
    labs(title = "Total avoided deaths in 2050 compared to 2020",
         subtitle = "sigmoidal implementation of diets applied to 2020 population",
         x = "",
         y = "number of avoided deaths")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Régimes appliqués à la structure de population de l'année d'intérêt du modèle
  
# Décès évités par âge
  graph_avoided_deaths_2035_c <- ggplot(avoided_deaths_c, aes(x = age,
                                                              y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2035 compared to 2020",
         subtitle = "sigmoidal implementation of diets applied to 2035 population",
         x = "Age",
         y = "Avoided deaths")
  
  graph_avoided_deaths_2050_c <- ggplot(avoided_deaths_c, aes(x = age,
                                                              y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "darkseagreen4")+
    labs(title = "Avoided deaths in 2050 compared to 2020",
         subtitle = "sigmoidal implementation of diets applied to 2050 population",
         x = "Age",
         y = "Avoided deaths")

# Total des décès évités
  graph_total_avoided_2035_c <- ggplot(data = total_avoided_deaths_c, aes(x = scenario, 
                                                                          y = total_avoided_deaths, 
                                                                          fill = scenario))+
    geom_bar(stat = "identity", alpha = 0.8)+
    labs(title = "Total avoided deaths in 2035 compared to 2020",
         subtitle = "sigmoidal implementation of diets applied to 2035 population",
         x = "",
         y = "number of avoided deaths")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  graph_total_avoided_2050_c <- ggplot(data = total_avoided_deaths_c, aes(x = scenario, 
                                                                          y = total_avoided_deaths, 
                                                                          fill = scenario))+
    geom_bar(stat = "identity", alpha = 0.8)+
    labs(title = "Total avoided deaths in 2050 compared to 2020",
         subtitle = "sigmoidal implementation of diets applied to 2050 population",
         x = "",
         y = "number of avoided deaths")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


  
################################################################################################################################
#                                             7. Exportation des données                                                       #
################################################################################################################################

## Régimes appliqués à la structure de population de l'année initiale du modèle

# Décès évités en 2035 par rapport à 2020
  export(avoided_deaths_i, here("results", "avoided_deaths_2020_pop_2035_diet.xlsx"))
  export(total_avoided_deaths_i, here("results", "total_avoided_2020_pop_2035_diet.xlsx"))
  
  ggsave(here("results", "avoided_deaths_2020_pop_2035_diet.pdf"), plot = graph_avoided_deaths_2035_i)
  ggsave(here("results", "total_avoided_2020_pop_2035_diet.pdf"), plot = graph_total_avoided_2035_i)
  
# Décès évités en 2050 par rapport à 2020
  export(avoided_deaths_i ,here("results", "avoided_deaths_2020_pop_2050_diet.xlsx"))
  export(total_avoided_deaths_i, here("results", "total_avoided_2020_pop_2050_diet.xlsx"))
  
  ggsave(here("results", "avoided_deaths_2020_pop_2050_diet.pdf"), plot = graph_avoided_deaths_2050_i)
  ggsave(here("results", "total_avoided_2020_pop_2050_diet.pdf"), plot = graph_total_avoided_2050_i)
  
## Régimes appliqués à la structure de population de l'année d'intérêt du modèle
  
# Décès évités en 2035 par rapport à 2020
  export(avoided_deaths_c, here("results", "avoided_deaths_2035_pop_2020_diet.xlsx"))
  export(total_avoided_deaths_c, here("results", "total_avoided_2035_pop_2020_diet.xlsx"))
  
  ggsave(here("results", "avoided_deaths_2035_pop_2020_diet.pdf"), plot = graph_avoided_deaths_2035_c)
  ggsave(here("results", "total_avoided_2035_pop_2020_diet.pdf"), plot = graph_total_avoided_2035_c)
  
# Décès évités en 2050 par rapport à 2020
  export(avoided_deaths_c ,here("results", "avoided_deaths_2050_pop_2020_diet.xlsx"))
  export(total_avoided_deaths_c, here("results", "total_avoided_2050_pop_2020_diet.xlsx"))
  
  ggsave(here("results", "avoided_deaths_2050_pop_2020_diet.pdf"), plot = graph_avoided_deaths_2050_c)
  ggsave(here("results", "total_avoided_2050_pop_2020_diet.pdf"), plot = graph_total_avoided_2050_c)
  