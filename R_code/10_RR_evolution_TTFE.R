################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  purrr                # Opérations itératives
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# RR des régimes complets de chaque scénario par année
  rr_diet_lin <- import(here("data_clean", "combined_rr_lin_2.xlsx"))
  
# RR de chaque aliment par année
  rr_food_lin <-  import(here("data_clean", "rr_evo_lin_2.xlsx"))
  
# Time to full effect linéaire sur 20 ans
  ttfe_lin <- import(here("data_clean", "ttfe_lin_20.xlsx"))
  
################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Paramètre de modulation de la courbe d'interpolation cosinus
  p <- 1
  
# Paramètre de modulation de la courbe sigmoïde
  lambda <- 10
  
# Paramètre de modulation de la courbe logarithmique
  eta <- 1
  
  
################################################################################################################################
#                                             4. Calcul des RR avec ttfe linéaire                                              #
################################################################################################################################

# Implémentation linéaire des régimes 
# Time to full effect linéaire sur 20 ans 
  
# RR des régimes complets
  rr_evo <- rr_diet_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_diet_lin$year), 
                                 to = max(rr_diet_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
       1 + (combined_rr - 1) * ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)],
      year_n > year + max(ttfe_lin$x) ~ combined_rr
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_combined <- rr_evo %>% 
    group_by(scenario, year_n) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
  
# RR de chaque aliment
  rr_evo_food <- rr_food_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_food_lin$year), 
                             to = max(rr_food_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
        1 + (rr_interpolated - 1) * ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)],
      year_n > year + max(ttfe_lin$x) ~ rr_interpolated
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_food_combined <- rr_evo_food %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
  
# Exemple d'évolution d'un RR : Viande rouge dans S1 après changement en 2025
  
  rr_lin_red_meat_sc1_2025 <- rr_evo_food %>% 
    filter(scenario == "sc1",
           food_group == "red_meat",
           year_i == 2025) %>% 
    select(year_n, rr_n) %>% 
    rename("year" = "year_n",
           "rr" = "rr_n")
  
  graph_rr_red_meat_sc1_2025 <- ggplot(rr_lin_red_meat_sc1_2025, aes(x = year,
                                                                     y = rr))+
    geom_line(color = "indianred4", size = 1)+
    labs(title = "RR associated with red meat consumption change in 2025 in S1",
         x = "",
         y = "RR")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Exemple, évolution des RR à partir du changement en 2025 dans S1
  
  rr_lin_sc1 <- rr_evo_food %>% 
    filter(scenario == "sc1",
           year_i == 2025,
           year_n %in% c(2025:2050)) %>% 
    select(food_group, year_n, rr_n) %>% 
    rename("year" = "year_n",
           "rr" = "rr_n")
  
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
  
  order_food_groups <- c("red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy", 
                         "fruits", "vegetables", "legumes", "nuts","whole_grains", "reffined_grains",
                         "added_plant_oils", "sugar_sweetened_beverages")
  
  rr_lin_sc1$food_group <- factor(rr_lin_sc1$food_group, levels = order_food_groups)
  
  graph_rr_sc1_2025 <- ggplot(rr_lin_sc1, aes(x = year,
                                              y = rr,
                                              color = as.factor(food_group)))+
    geom_line(size = 1, alpha = 0.8)+
    scale_color_manual(values = col_food_groups)+
    labs(title = "RR associated with 2025 changes in diet in S1",
         x = "",
         y = "RR",
         color = "Food Group")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# RR des changements de consommation de viande rouge dans S1 de 2020 à 2050
  
  rr_lin_red_meat_sc1 <- rr_evo_food %>% 
    filter(scenario == "sc1",
           food_group == "red_meat") %>% 
    select(year_i ,year_n, rr_n) %>% 
    pivot_wider(names_from = year_n, values_from = rr_n)
  

################################################################################################################################
#                                             5. Calcul des RR avec ttfe par interpolation cosinus                             #
################################################################################################################################
  
# Implémentation linéaire des régimes 
# Time to full effect = 20 ans 
  
# RR de chaque aliment
  rr_evo_food <- rr_food_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_food_lin$year), 
                             to = max(rr_food_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
        1 + (rr_interpolated - 1) * ((1 - cos((pi * ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)])^p)) / 2),
      year_n > year + max(ttfe_lin$x) ~ rr_interpolated
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_food_combined <- rr_evo_food %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
  
################################################################################################################################
#                                             6. Calcul des RR avec ttfe sigmoïdal                                             #
################################################################################################################################
  
  # Implémentation linéaire des régimes 
  # Time to full effect = 20 ans 
  
  # RR de chaque aliment
  rr_evo_food <- rr_food_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_food_lin$year), 
                             to = max(rr_food_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
        (1 + rr_interpolated)/2 + (rr_interpolated -1) * (1 / (1 + exp(-lambda * (ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)] - 1/2)))- 1/2) * (-1 / (2 / (1 + exp(lambda/2)) - 1)),
      year_n > year + max(ttfe_lin$x) ~ rr_interpolated
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_food_combined <- rr_evo_food %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
################################################################################################################################
#                                             7. Calcul des RR avec ttfe logarithmique                                         #
################################################################################################################################
  
# Implémentation linéaire des régimes 
# Time to full effect = 20 ans 
  
# RR de chaque aliment
  rr_evo_food <- rr_food_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_food_lin$year), 
                             to = max(rr_food_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
        1 + (rr_interpolated - 1) * log(1 + eta * ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)]) / log(1 + eta),
      year_n > year + max(ttfe_lin$x) ~ rr_interpolated
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_food_combined <- rr_evo_food %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
  
################################################################################################################################
#                                             8. Combinaison des RR                                                            #
################################################################################################################################
  
  calc_combined_rr <- function(df) {
    df %>%
      group_by(scenario, year_n) %>%
      summarize(combined_rr = prod(mean_rr, na.rm = TRUE)) %>%
      ungroup()
  }  

# rr en fonction de l'implémentation linéaire des régimes 
  combined_rr_table_lin <- calc_combined_rr(rr_evo_food_combined)
  
################################################################################################################################
#                                             9. Représentations graphiques                                                    #
################################################################################################################################
  
# Charte graphique
  col_scenario <- c("actuel" = "azure4",
                    "sc0" = "royalblue2",
                    "sc1" = "darkseagreen4",
                    "sc2" = "aquamarine2",
                    "sc3" = "lightpink",
                    "sc4" = "maroon",
                    "sc5" = "royalblue4")
  
# Implémentation linéaire des régimes
  graph_rr_evo_lin <- ggplot(combined_rr_table_lin, aes(x = year_n,
                                                        y = combined_rr,
                                                        colour = scenario))+
    geom_line(size = 1)+
    scale_color_manual(values = col_scenario)+
    labs(title = "Whole diet RR evolution in each scenario",
         subtitle = "linear implementation of diets from 2020 to 2050",
         x = "",
         y = "Diet RR") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
################################################################################################################################
#                                             10. Exportation des données                                                       #
################################################################################################################################
  
# Implémentation linéaire des régimes et ttfe linéaire sur 20 ans
  
  # Valeurs des RR de chaque aliment apar année, après chaque changement
    export(rr_evo_food, here("data_clean", "rr_fg_lin_ttfe_lin_20.xlsx"))
    
  # Moyenne des RR de chaque aliment par année
    export(rr_evo_food_combined, here("data_clean", "rr_fg_lin_ttfe_lin_20_mean.xlsx"))
  
  # RR des régimes complets par année
    export(combined_rr_table_lin, here("data_clean", "combined_rr_lin_ttfe_lin_20.xlsx"))
    
    ggsave(here("results", "Diets_RR_evo_lin_ttfe_lin20.pdf"), plot = graph_rr_evo_lin)
    
  # Exemple de l'évolution du RR de la viande rouge après changement en 2025
    ggsave(here("results", "RR_red_meat_S1_2025.pdf"), plot = graph_rr_red_meat_sc1_2025)

  # Exemple de l'évolution de tous les RR de S1 après changement en 2025
    ggsave(here("results", "RR_S1_2025.pdf"), plot = graph_rr_sc1_2025)
    