################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  purrr,               # Opérations itératives
  tidyverse,           # Data management, inclus ggplot
  scico                # Palettes de couleur
)


################################################################################################################################
#                                             2. Importation des données et paramètres                                         #
################################################################################################################################

# Régimes SISAE : Quantités d'aliments (g/j/pers)
diets <- import(here("data", "DOUGLAS_diets.xlsx"))

# Bornes temporelles du modèle
year_i <- 2020 # Année initiale
year_f <- 2050 # Année finale

# paramètre de modulation de la courbe d'interpolation cosinus d'implémentation des régimes dans le temps
p <- 1
p_values <- seq(0, 2, by = 0.25) # séquence de valeurs de p de 0 à 2 avec un pas de 0.25

# paramètre de modulation de la sigmoïde d'implémentation des régimes dans le temps
lambda <- 10
lambda_values <- seq(0, 20, by = 2) # séquence de valeurs de lambda de 0 à 20 avec un pas de 2

################################################################################################################################
#                                             3. Evolution linéaire des régimes dans le temps                                  #
################################################################################################################################

# Paramètres des fonctions affines
  
  # Coefficient directeur
    coeff <- diets %>%
      mutate(across(-"food_group",
                    ~ (. - actuel)/(year_f - year_i)  
      ))

   # Ordonnée à l'origine
    intercept <- diets %>%
      mutate(across(-"food_group",
                    ~ diets[[cur_column()]] - coeff[[cur_column()]]*year_f
      ))
    
  
# Quantités consommées de chaque aliment par année dans chaque scénario
  
    # Création d'un vecteur temporel contenant toutes les années d'étude du modèle
      years <- tibble(year = year_i:year_f) 

    # Fonction calculant les quantités consommées par année pour un scénario donné
      calc_food_q <- function(scenario) {
        a <- coeff %>%
          select(food_group, !!scenario)
        
        b <- intercept %>%
          select(food_group, !!scenario)
        
        a_b <- a %>%
          inner_join(b, by = "food_group", suffix = c("_coeff", "_intercept"))
        
        food_q <- expand_grid(food_group = a_b$food_group, year = years$year) %>%
          left_join(a_b, by = "food_group") %>%
          mutate(quantity = get(paste0(scenario, "_coeff")) * year + get(paste0(scenario, "_intercept"))) %>%
          select(food_group, year, quantity)
      }
    
    # Scénarios à traiter 
      scenarios <- colnames(diets)[-1]
      
    # Calculer les doses consommées pour chaque scénario
      scenario_q <- map(scenarios, calc_food_q)
    
    # Nommer les éléments de la liste selon les scénarios
      names(scenario_q) <- scenarios
    
    # Assembler tous les dataframes en un seul
      diets_evo_lin <- bind_rows(scenario_q, .id = "scenario")
    
    # Sélectionner les scénarios d'intérêt
      diets_evo_lin_filt <- diets_evo_lin %>% 
        filter(scenario %in% c("actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5"))

# Exemple : évolution de la viande rouge dans S1
      
      red_meat_s1_lin <- diets_evo_lin_filt %>% 
        filter(food_group == "red_meat",
               scenario == "sc1")
      
      graph_lin_implementation <- ggplot(red_meat_s1_lin, aes(x = year, 
                                                           y = quantity)) +
        geom_line(color = "royalblue3") +
        labs(title = "Red meat consumption implementation in S1",
             x = "",
             y = "Quantity (g/day/pers)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
################################################################################################################################
#                                             4. Evolution par interpolation cosinus des régimes dans le temps                 #
################################################################################################################################

# Fonction pour calculer la quantité consommée d'un aliment pour une année donnée (q_n)
    calc_food_q_cos <- function(q_i, q_f, year_n, year_i, year_f, p) {
      q_i + (q_f - q_i) * (1 - cos(pi * ((year_n - year_i) / (year_f - year_i))^p)) / 2
    }

# Appliquer la fonction sur chaque combinaison d'année et scénario
    diets_evo_cos_filt <- diets %>%
      select("food_group", "actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5") %>%
      mutate(q_i = actuel) %>% 
      pivot_longer(cols = c("actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5"), 
                   names_to = "scenario", 
                   values_to = "q_f") %>%  
      crossing(year_n = year_i:year_f) %>%
      mutate(quantity = mapply(calc_food_q_cos, q_i, q_f, year_n, year_i, year_f, p)) %>% 
      select("food_group", "scenario", "year_n", "quantity") %>% 
      rename("year" = "year_n")
    
# Variation de la valeur p et changement de régime pour un aliment dans un scénario
    
    red_meat_sc1 <- diets %>%
      filter(food_group == "red_meat") %>%
      select(food_group, actuel, sc1) %>%
      rename(q_i = actuel, q_f = sc1)
    
    expand_red_meat <- map_dfr(p_values, function(p) {
      red_meat_sc1 %>%
        crossing(year_n = year_i:year_f) %>%
        mutate(p = p, quantity = mapply(calc_food_q_cos, q_i, q_f, year_n, year_i, year_f, p))
    })
    
    expand_red_meat <- expand_red_meat %>% 
      select("food_group", "p", "year_n", "quantity") %>% 
      rename("year" = "year_n")
    
    # bisectrice à ajouter sur le graphe
    
      line_data <- data.frame(year = c(year_i, year_f),
                              quantity = c(red_meat_sc1$q_i, red_meat_sc1$q_f))
    
   
    # Représentation graphique
      graph_cos_variation <-   ggplot(expand_red_meat, aes(x = year, 
                                                           y = quantity, 
                                                           color = factor(p), 
                                                           group = p)) +
        geom_line() +
        geom_segment(data = line_data, 
                     aes(x = year[1], y = quantity[1], 
                                           xend = year[2], yend = quantity[2]),
                     color = "indianred3", size = 0.8) +
        labs(title = "Red meat consumption implementation in S1",
             x = "",
             y = "Quantity (g/day/pers)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_scico_d(palette = "bukavu")
      

################################################################################################################################
#                                             3. Evolution sigmoïdale des régimes dans le temps                                #
################################################################################################################################
      
 # Fonction pour calculer la quantité consommée d'un aliment pour une année donnée (q_n)
    calc_food_q_sig <- function(q_i, q_f, year_n, year_i, year_f, lambda) {
      (q_i+q_f)/2 + (q_f - q_i)* (1 / (1 + exp(-lambda*(year_n - year_i-(year_f - year_i)/2)/(year_f - year_i)))-0.5)*(-1/(2*(1/(1+exp(lambda/2))-0.5)))
    }
    
    # Appliquer la fonction sur chaque combinaison d'année et scénario
      diets_evo_sig_filt <- diets %>%
        select("food_group", "actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5") %>% 
        mutate(q_i = actuel) %>% 
        pivot_longer(cols = c("actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5"), 
                     names_to = "scenario", 
                     values_to = "q_f") %>%  
        crossing(year_n = year_i:year_f) %>%
        mutate(quantity = mapply(calc_food_q_sig, q_i, q_f, year_n, year_i, year_f, lambda)) %>% 
        select("food_group", "scenario", "year_n", "quantity") %>% 
        rename("year" = "year_n")
    
   
# Variation de la valeur lambda et changement de régime pour un aliment dans un scénario
   
   expand_red_meat_2 <- map_dfr(lambda_values, function(lambda) {
     red_meat_sc1 %>%
       crossing(year_n = year_i:year_f) %>%
       mutate(lambda = lambda, quantity = mapply(calc_food_q_sig, q_i, q_f, year_n, year_i, year_f, lambda))
   })
   
   expand_red_meat_2 <- expand_red_meat_2 %>% 
     select("food_group", "lambda", "year_n", "quantity") %>% 
     rename("year" = "year_n")
  
   # Représentation graphique
     graph_sig_variation <-   ggplot(expand_red_meat_2, aes(x = year, 
                                                            y = quantity, 
                                                            color = factor(lambda), 
                                                            group = lambda)) +
       geom_line() +
       geom_segment(data = line_data, 
                    aes(x = year[1], y = quantity[1], 
                        xend = year[2], yend = quantity[2]),
                    color = "indianred3", size = 0.8) +
       labs(title = "Red meat consumption implementation in S1",
            x = "",
            y = "Quantity (g/day/pers)") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
       scale_color_scico_d(palette = "bukavu")
     

################################################################################################################################
#                                             5. Représentations graphiques de l'implémentation des régimes complets           #
################################################################################################################################

# Attribuer une couleur à chaque groupe alimentaire (colorblind friendly)
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

  diets_evo_lin_filt$food_group <- factor(diets_evo_lin_filt$food_group, levels = order_food_groups)
  diets_evo_cos_filt$food_group <- factor(diets_evo_cos_filt$food_group, levels = order_food_groups)
  diets_evo_sig_filt$food_group <- factor(diets_evo_sig_filt$food_group, levels = order_food_groups)
    
# Graphique : Implémentation linéaire des régimes
  graph_diets_evo_lin <- ggplot(data = diets_evo_lin_filt, aes(x = year,
                                                                 y = quantity,
                                                                 fill = food_group))+
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6)+
    facet_wrap(~ scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
          axis.text.y = element_text(size = 5),
          strip.text = element_text(face = "bold",size = rel(0.6)),
          legend.position = c(0.5,0.1),
          legend.text = element_text(size = 4),
          legend.title = element_text(face = "bold", size = 5),
          legend.key.size = unit(0.1, "cm"))+
    scale_fill_manual(values = col_food_groups)+
    labs(title = "Diet changes",
         x = "",
         y = "Quantities (g/day/pers)",
         fill = "Food type")

# Graphique : Implémentation par interpolation cosinus des régimes 
  graph_diets_evo_cos <- ggplot(data = diets_evo_cos_filt, aes(x = year,
                                                               y = quantity,
                                                               fill = food_group))+
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6)+
    facet_wrap(~ scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
          axis.text.y = element_text(size = 5),
          strip.text = element_text(face = "bold",size = rel(0.6)),
          legend.position = c(0.5,0.1),
          legend.text = element_text(size = 4),
          legend.title = element_text(face = "bold", size = 5),
          legend.key.size = unit(0.1, "cm"))+
    scale_fill_manual(values = col_food_groups)+
    labs(title = "Diet changes",
         x = "",
         y = "Quantities (g/day/pers)",
         fill = "Food type")

# Graphique : Implémentation sigmoïdale des régimes
  graph_diets_evo_sig <- ggplot(data = diets_evo_sig_filt, aes(x = year,
                                                               y = quantity,
                                                               fill = food_group))+
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6)+
    facet_wrap(~ scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
          axis.text.y = element_text(size = 5),
          strip.text = element_text(face = "bold",size = rel(0.6)),
          legend.position = c(0.5,0.1),
          legend.text = element_text(size = 4),
          legend.title = element_text(face = "bold", size = 5),
          legend.key.size = unit(0.1, "cm"))+
    scale_fill_manual(values = col_food_groups)+
    labs(title = "Diet changes",
         x = "",
         y = "Quantities (g/day/pers)",
         fill = "Food type")
  
################################################################################################################################
#                                             6. Exportation des données                                                       #
################################################################################################################################

# Implémentation linéaire des régimes
  export(diets_evo_lin, here("data_clean", "diets_evo_lin.xlsx"))                 # Tous les scénarios et régimes
  export(diets_evo_lin_filt, here("data_clean", "diets_evo_lin_scenarios.xlsx"))  # Seulement les scénarios de l'ADEME

  ggsave(here("results", "diets_evo_lin.pdf"), plot = graph_diets_evo_lin)
  
  # Exemple d'implémentation linéaire pour un aliment
  ggsave(here("results", "linear_implementation_meat.pdf"), plot = graph_lin_implementation)

# Implémentation par interpolation cosinus des régimes dans tous les scénarios
  export(diets_evo_cos_filt, here("data_clean", "diets_evo_cos_scenarios.xlsx"))
  
  ggsave(here("results", "diets_evo_cos.pdf"), plot = graph_diets_evo_cos)

  # Variation du paramètre p de la courbe d'implémentation par interpolation cosinus d'un changement de consommation d'un aliment
  ggsave(here("results", "diet_cos_p_variation.pdf"), plot = graph_cos_variation)
  
# Implémentation logistique (exp) des régimes dans tous les scénarios
  export(diets_evo_sig_filt, here("data_clean", "diets_evo_sig_scenarios.xlsx"))
  
  ggsave(here("results", "diets_evo_sig.pdf"), plot = graph_diets_evo_sig)
  
  # Variation du paramètre lambda de la courbe d'implémentation d'un changement de consommation d'un aliment
  ggsave(here("results", "diet_sig_lambda_variation.pdf"), plot = graph_sig_variation)


  