################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation/Exportation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  tidyverse,           # Data management, inclus ggplot
  psych                # Contient fonction moyenne géométrique
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################
# RR relatifs au régime actuel
  # central
    rr_central <- import(here("results", "visualization_tool","rr_evo_diets.xlsx"))
    
  #IC95
    rr_IC95 <- import(here("results", "visualization_tool_ic95", "IC95_rr_evo_diets_relative.xlsx"))

# Décès évités
  # central
    avoided_deaths_central <- import(here("results", "visualization_tool", "total_avoided_deaths.xlsx"))
    
  # IC95
    avoided_deaths_IC95 <- import(here("results", "visualization_tool_ic95", "IC95_total_avoided_deaths.xlsx"))
    
################################################################################################################################
#                                             3. Charte graphique                                                              #
################################################################################################################################
    
# Couleur de chaque scénario
    col_scenario <- c("actuel" = "azure4",
                      "sc0" = "palevioletred3",
                      "sc1" = "aquamarine3",
                      "sc2" = "#DDCC77",
                      "sc3" = "lightskyblue3",
                      "sc4" = "#882255",
                      "sc5" = "royalblue4")
    
# Couleur de chaque groupe d'aliments
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
    
# Etiquettes des scénarios et groupes d'aliments
    labels_scenario <- c("actuel" = "Current diet",
                         "sc1" = "Scenario 1",
                         "sc2" = "Scenario 2",
                         "sc3" = "Scenario 3",
                         "sc4" = "Scenario 4")
    
    labels_food_groups <- c("red_meat" = "Red meat",
                            "processed_meat" = "Processed meat",
                            "white_meat" = "White meat",
                            "dairy" = "Dairy",
                            "fish" = "Fish",
                            "eggs" = "Eggs",
                            "fruits" = "Fruits",
                            "nuts" = "Nuts",
                            "vegetables" = "Vegetables",
                            "legumes" = "Legumes",
                            "whole_grains" = "Whole grains",
                            "reffined_grains" = "Refine grains",
                            "added_plant_oils" = "Added plant oils",
                            "sugar_sweetened_beverages" = "Sugar-sweetened beverages")
    
   
################################################################################################################################
#                                             4. Préparation des données                                                       #
################################################################################################################################
# RR relatifs au régime actuel
    rr_relative <- rr_central %>% 
      select(-combined_rr) %>% 
      left_join(rr_IC95, by = c("scenario", "year"))

# Décès évités
    avoided_deaths <- avoided_deaths_central %>%
      select(-"avoided_deaths") %>% 
      pivot_longer(cols = "2015":"2070",
                   names_to = "year",
                   values_to = "avoided_deaths") %>% 
      mutate(year = as.numeric(year)) %>% 
      left_join(avoided_deaths_IC95, by = c("scenario", "year"))
    
################################################################################################################################
#                                             5. Représentations graphiques                                                    #
################################################################################################################################

# RR relatifs au régime actuel
    graph_rr_diets_relative <- ggplot(rr_relative %>% 
                                            filter(scenario != "actuel"),
                                      aes(x = year,
                                          y = relative_rr,
                                          color = scenario)) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5)+
      facet_wrap(~ scenario)+
      geom_line(size = 1, na.rm = TRUE)+ 
      labs(
        title = "RR values relative to keeping the current diet",
        x = "",
        y = "RR"
      )+
      scale_color_manual(values = col_scenario)+
      scale_fill_manual(values = col_scenario)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(1)),
            legend.position = "none")
    
    
# Décès évités
    graph_total_avoided_deaths  <- ggplot(avoided_deaths,
                                          aes(x = year,
                                              y = avoided_deaths,
                                              color = scenario)) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5)+
      facet_wrap(~ scenario)+
      geom_line(linewidth = 1, na.rm = TRUE)+ 
      labs(
        title = "Avoided deaths compared to keeping the current diet",
        x = "",
        y = "Number of avoided deaths"
      )+
      scale_color_manual(values = col_scenario)+
      scale_fill_manual(values = col_scenario)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(1)),
            legend.position = "none")
    
################################################################################################################################
#                                             6. Exportation des données                                                      #
################################################################################################################################
    
  ggsave(here("results", "visualisation_tool_central_ic95", "rr_diets_relative.pdf"), plot = graph_rr_diets_relative)
  ggsave(here("results", "visualisation_tool_central_ic95", "avoided_deaths.pdf"), plot = graph_total_avoided_deaths)
    
    