################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation/Exportation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  tidyverse,           # Data management, inclus ggplot
  purrr,               # Opérations itératives
  psych                # Contient fonction moyenne géométrique
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Expositions : régimes SISAE en 2050
  diets <- import(here("data", "DOUGLAS_diets.xlsx"))

# Risques relatifs / consommation (g/j) (Fadnes, 2022 & 2024)
  
  # Lower
  rr_table_low <- import(here("data_clean", "rr_table_low_interpolated.xlsx"))
  
  #Mid
  rr_table_mid <- import(here("data_clean", "rr_table_interpolated.xlsx"))
  
  #Upper
  rr_table_up <- import(here("data_clean", "rr_table_up_interpolated.xlsx"))
  
################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################
  
# Nombre de simulations des valeurs de RR
  n <- 1000
  
# Bornes temporelles des changements de régime alimentaire (années)
  year_i <- 2025 # Année initiale
  year_f <- 2050 # Année finale
  
  
# Dynamique d'implémentation des régimes (immediate, linear, cosine, sigmoidal)
  implementation <- "cosine"
  
# paramètre de la courbe d'interpolation cosinus
  p <- 1
  
# paramètre de la courbe sigmoïdale
  lambda <- 5
  
# Paramètre de modification d'effet des RR 
  # 0.5 à 1 = réduction d'effet, modèle conservateur
  # 1 à 1.5 = augmentation d'effet, modèle radical
  m <- 0.75
  
# Durée time to full effect
  ttfe_time <- 10
  
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
  
  # Pivoter les dataframes des RR en format long
  rr_table_low <- rr_table_low %>% 
    pivot_longer(cols = "0":"800",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))
  
  rr_table_mid <- rr_table_mid %>% 
    pivot_longer(cols = "0":"800",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))
  
  rr_table_up <- rr_table_up %>% 
    pivot_longer(cols = "0":"800",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))
  
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
    select("food_group", "actuel", "sc1", "sc2", "sc3", "sc4") %>% 
    filter(food_group %in% c("red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy", 
                             "fruits", "vegetables", "legumes", "nuts","whole_grains", "reffined_grains",
                             "sugar_sweetened_beverages")) %>% 
    mutate(q_i = actuel) %>% 
    pivot_longer(cols = c("actuel", "sc1", "sc2", "sc3", "sc4"), 
                 names_to = "scenario", 
                 values_to = "q_f") %>%  
    crossing(year_n = (year_i - ttfe_time) : (year_f + 2*ttfe_time)) %>%
    mutate(quantity = case_when(
      implementation == "immediate" & year_n < year_i ~ q_i,
      implementation == "immediate" & year_n >= year_i ~ q_f,
      implementation == "linear" & year_n < year_i ~ q_i,
      implementation == "linear" & year_n %in% c(year_i : year_f) ~ mapply(calc_food_q_lin, q_i, q_f, year_n, year_i, year_f),
      implementation == "linear" & year_n > year_f ~ q_f,
      implementation == "cosine" & year_n < year_i ~ q_i,
      implementation == "cosine" & year_n %in% c(year_i : year_f) ~ mapply(calc_food_q_cos, q_i, q_f, year_n, year_i, year_f, p),
      implementation == "cosine" & year_n > year_f ~ q_f,
      implementation == "sigmoidal" & year_n < year_i ~ q_i,
      implementation == "sigmoidal" & year_n %in% c(year_i : year_f) ~ mapply(calc_food_q_sig, q_i, q_f, year_n, year_i, year_f, lambda),
      implementation == "sigmoidal" & year_n > year_f ~ q_f
    )) %>% 
    select("food_group", "scenario", "year_n", "quantity") %>% 
    rename("year" = "year_n")
  
################################################################################################################################
#                                             7. Modification d'effet des RR                                                         #
################################################################################################################################
  
  rr_table_low <- rr_table_low %>% 
    mutate(rr_a = case_when(
      rr < 1 ~ rr + (1 - rr) * (1 - m),
      rr >= 1 ~ 1 / (m/rr + 1 - m)
    )) %>% 
    select("food_group", "quantity", "rr_a") %>% 
    rename("rr" = "rr_a")
  
  rr_table_mid <- rr_table_mid %>% 
    mutate(rr_a = case_when(
      rr < 1 ~ rr + (1 - rr) * (1 - m),
      rr >= 1 ~ 1 / (m/rr + 1 - m)
    )) %>% 
    select("food_group", "quantity", "rr_a") %>% 
    rename("rr" = "rr_a")
  
  rr_table_up <- rr_table_up %>% 
    mutate(rr_a = case_when(
      rr < 1 ~ rr + (1 - rr) * (1 - m),
      rr >= 1 ~ 1 / (m/rr + 1 - m)
    )) %>% 
    select("food_group", "quantity", "rr_a") %>% 
    rename("rr" = "rr_a")
  
################################################################################################################################
#                                             8. Attribution des RR à chaque régime                                            #
################################################################################################################################
  
  diets_evo <- diets_evo %>% 
    mutate(quantity = round(quantity)) %>% 
    left_join(rr_table_low, by = c("food_group", "quantity")) %>% 
    rename("rr_low" = "rr") %>% 
    left_join(rr_table_mid, by = c("food_group", "quantity")) %>% 
    rename("rr_mid" = "rr") %>% 
    left_join(rr_table_up, by = c("food_group", "quantity")) %>% 
    rename("rr_up" = "rr") %>% 
    mutate(rr_low = ifelse(is.na(rr_low), rr_mid, rr_low),
           rr_up = ifelse(is.na(rr_up), rr_mid, rr_up))
  
################################################################################################################################
#                                             9. Génération des distributions normales pour chaque RR                          #
################################################################################################################################
  
  # Fixer une graine pour garantir la reproductibilité des simulations
  set.seed(123)
  
  # Fonction de génération des simulations selon une distribution normale
  generate_RR_distrib = function(food_group, RR, low, sup, N = n){
    #RR, low and sup are the RR and 95%CI of a specific risk ration
    #N is the number of random values from the distrib
    
    lRR = log(RR)
    l_low = log(low)
    l_sup = log(sup)
    sd1 = (lRR-l_low)/qnorm(1-0.05/2)
    sd2 = (l_sup-lRR)/qnorm(1-0.05/2)
    sd = mean(c(sd1, sd2))
    distr_RR = exp(rnorm(N, lRR, sd))
    
    # just need to truncat values
    distr_RR[distr_RR<0]=0
    
    #if (food_group %in% c("red_meat", "processed_meat", "sugar_sweetened_beverages"))
    #distr_RR[distr_RR<1]=1
    #if (food_group %in% c("fruits", "vegetables", "legumes", "whole_grains", "nuts"))
    #distr_RR[distr_RR>1]=1
    
    return(distr_RR)
  }
  
  
  diets_evo <- diets_evo %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(generate_RR_distrib(food_group, rr_mid, rr_low, rr_up)))
  
################################################################################################################################
#                                             10. Etude des distributions pour chaque aliment                                  #
################################################################################################################################
  
# Dairy
  dist_dairy <- diets_evo %>% 
    filter(scenario == "sc1",
           year == 2050,
           food_group == "dairy") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_dairy <- ggplot(dist_dairy, aes(x = rr_distrib,
                                                y = after_stat(count / sum(count)),
                                                fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Dairy intake in S1 in 2050",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_dairy$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
  # Quantiles proches des quantiles théoriques, pas de troncature
  
# Eggs
  dist_eggs <- diets_evo %>% 
    filter(scenario == "actuel",
           year == 2025,
           food_group == "eggs") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_eggs <- ggplot(dist_eggs, aes(x = rr_distrib,
                                                y = after_stat(count / sum(count)),
                                                fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Eggs intake in current diet",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_dairy$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
  
# Fish
  dist_fish <- diets_evo %>% 
    filter(scenario == "sc1",
           year == 2050,
           food_group == "fish") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_fish <- ggplot(dist_fish, aes(x = rr_distrib,
                                              y = after_stat(count / sum(count)),
                                              fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Fish intake in sc1 in 2050",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_fish$rr_distrib, probs= c(0.025, 0.5, 0.975))

   
# Fruits
  dist_fruits <- diets_evo %>% 
    filter(scenario == "sc1",
           year == 2050,
           food_group == "fruits") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_fruits <- ggplot(dist_fruits, aes(x = rr_distrib,
                                                  y = after_stat(count / sum(count)),
                                                  fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Fruits intake in S1 in 2050",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_fruits$rr_distrib, probs= c(0.025, 0.5, 0.975))

# Legumes
  dist_legumes <- diets_evo %>% 
    filter(scenario == "actuel",
           year == 2025,
           food_group == "legumes") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_legumes <- ggplot(dist_legumes, aes(x = rr_distrib,
                                                  y = after_stat(count / sum(count)),
                                                  fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Legumes intake in current diet",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_legumes$rr_distrib, probs= c(0.025, 0.5, 0.975))

  
# Nuts
  dist_nuts <- diets_evo %>% 
    filter(scenario == "actuel",
           year == 2025,
           food_group == "nuts") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_nuts <- ggplot(dist_nuts, aes(x = rr_distrib,
                                                    y = after_stat(count / sum(count)),
                                                    fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Nuts intake in current diet",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_nuts$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
# Processed meat
  dist_processed_meat <- diets_evo %>% 
    filter(scenario == "sc1",
           year == 2050,
           food_group == "processed_meat") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_processed_meat <- ggplot(dist_processed_meat, aes(x = rr_distrib,
                                              y = after_stat(count / sum(count)),
                                              fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Processed meat intake in S1 in 2050",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_processed_meat$rr_distrib, probs= c(0.025, 0.5, 0.975))

# Red meat
  dist_red_meat <- diets_evo %>% 
    filter(scenario == "sc1",
           year == 2050,
           food_group == "red_meat") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_red_meat <- ggplot(dist_red_meat, aes(x = rr_distrib,
                                                                  y = after_stat(count / sum(count)),
                                                                  fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Red meat intake in S1 in 2050",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_red_meat$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
  
# Refined grains
  dist_refined_grains <- diets_evo %>% 
    filter(scenario == "sc4",
           year == 2050,
           food_group == "reffined_grains") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_refined_grains <- ggplot(dist_refined_grains, aes(x = rr_distrib,
                                                      y = after_stat(count / sum(count)),
                                                      fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Refined grains intake in S4 in 2050",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_refined_grains$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
# Sugar-sweetened beverages
  dist_ssb <- diets_evo %>% 
    filter(scenario == "sc4",
           year == 2050,
           food_group == "sugar_sweetened_beverages") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_ssb <- ggplot(dist_ssb, aes(x = rr_distrib,
                                            y = after_stat(count / sum(count)),
                                            fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "SSB intake in S4 in 2050",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_ssb$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
# Vegetables
  dist_vegetables <- diets_evo %>% 
    filter(scenario == "actuel",
           year == 2025,
           food_group == "vegetables") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_vegetables <- ggplot(dist_vegetables, aes(x = rr_distrib,
                                                          y = after_stat(count / sum(count)),
                                                          fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Vegetables intake in Current diet",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_vegetables$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
# White meat
  dist_white_meat <- diets_evo %>% 
    filter(scenario == "sc1",
           year == 2050,
           food_group == "white_meat") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_white_meat <- ggplot(dist_white_meat, aes(x = rr_distrib,
                                                          y = after_stat(count / sum(count)),
                                                          fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "White meat intake in S1 in 2050",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_white_meat$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
# Whole grains
  dist_whole_grains <- diets_evo %>% 
    filter(scenario == "actuel",
           year == 2025,
           food_group == "whole_grains") %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_dist_whole_grains <- ggplot(dist_whole_grains, aes(x = rr_distrib,
                                                          y = after_stat(count / sum(count)),
                                                          fill = food_group))+
    geom_histogram(binwidth = 0.0005, 
                   position = "dodge",
                   alpha = 0.7) +
    scale_fill_manual(values = col_food_groups)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))+
    labs(title = "RR values",
         subtitle = "Whole grains intake in current diet",
         x = "RR",
         y = "Frequency")
  
  quantile(dist_whole_grains$rr_distrib, probs= c(0.025, 0.5, 0.975))
  
################################################################################################################################
#                                             11. Valeur des RR dans chaque simulation                                         #
################################################################################################################################
  
# Transformer les simulations en format long
  simulations_long <- diets_evo %>% 
    unnest_wider(rr_distrib, names_sep = "_") %>%  # Séparer les simulations en colonnes distinctes
    pivot_longer(
      cols = starts_with("rr_distrib_"),  # Sélectionner toutes les colonnes de simulations
      names_to = "simulation_id",  # Nom de la colonne contenant les identifiants de simulation
      values_to = "simulated_rr"  # Nom de la colonne contenant les valeurs simulées
    ) %>%
    mutate(simulation_id = str_remove(simulation_id, "rr_distrib_"),
           simulation_id = as.numeric(simulation_id))
  
# Représentation graphique
  ggplot(simulations_long, aes(x = simulation_id,
                               y = simulated_rr,
                               color = food_group))+
    geom_point()+
    facet_wrap(food_group ~.,
               scales = "free_y",
               ncol = 1,
               labeller = labeller(food_group = labels_food_groups))+
    scale_color_manual(values = col_food_groups)+
    labs(x = "simulation ID",
         y = "RR")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          strip.text = element_text(face = "bold",size = rel(0.8)),
          legend.position = "none")
    
  
################################################################################################################################
#                                             12. Exportation des données                                                      #
################################################################################################################################
  
# Graphiques des distributions normales de chaque aliment
  
  ggsave(here("results", "RR_norm_distributions", "dairy_dist.pdf"), plot = graph_rr_dist_dairy)
  ggsave(here("results", "RR_norm_distributions", "eggs_dist.pdf"), plot = graph_rr_dist_eggs)
  ggsave(here("results", "RR_norm_distributions", "fish_dist.pdf"), plot = graph_rr_dist_fish)  
  ggsave(here("results", "RR_norm_distributions", "fruits_dist.pdf"), plot = graph_rr_dist_fruits)  
  ggsave(here("results", "RR_norm_distributions", "legumes_dist.pdf"), plot = graph_rr_dist_legumes)  
  ggsave(here("results", "RR_norm_distributions", "nuts_dist.pdf"), plot = graph_rr_dist_nuts)  
  ggsave(here("results", "RR_norm_distributions", "processed_meat_dist.pdf"), plot = graph_rr_dist_processed_meat)  
  ggsave(here("results", "RR_norm_distributions", "red_meat_dist.pdf"), plot = graph_rr_dist_red_meat)  
  ggsave(here("results", "RR_norm_distributions", "refined_grains_dist.pdf"), plot = graph_rr_dist_refined_grains)  
  ggsave(here("results", "RR_norm_distributions", "ssb_dist.pdf"), plot = graph_rr_dist_ssb)  
  ggsave(here("results", "RR_norm_distributions", "vegetables_dist.pdf"), plot = graph_rr_dist_vegetables)  
  ggsave(here("results", "RR_norm_distributions", "white_meat_dist.pdf"), plot = graph_rr_dist_white_meat)  
  ggsave(here("results", "RR_norm_distributions", "whole_grains_dist.pdf"), plot = graph_rr_dist_whole_grains)  
  