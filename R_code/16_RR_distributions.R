################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  tidyverse,           # Data management, inclus ggplot
  purrr                # Opérations itératives
)


################################################################################################################################
#                                             2. Importation des données et paramètres                                         #
################################################################################################################################

# Table des risques relatifs pour chaque catégorie alimentaire, attribués à des quantités absolues (Fadnes)
rr_table_mid <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Mid")

# Table des RR, IC95 lower
rr_table_low <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Lower")

# Table des RR, IC95 upper
rr_table_up <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Upper")

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

# Méthode d'interpolation ("linear", "spline")
  interpolation <- "linear"
  
# Nombre de simulations des valeurs de RR
  n <- 1000
  
################################################################################################################################
#                                             3. Préparation des données                                                       #
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

# Un tableau unique
  rr_table <- rr_table_mid %>% 
    left_join(rr_table_low, by = c("food_group", "quantity")) %>% 
    left_join(rr_table_up, by = c("food_group", "quantity")) %>% 
    rename("rr_low" = "rr.x",
           "rr_mid" = "rr.y",
           "rr_up"="rr") %>% 
    mutate(rr_low = ifelse(is.na(rr_low), rr_mid, rr_low),
           rr_up = ifelse(is.na(rr_up), rr_mid, rr_up)) %>% 
    drop_na(rr_low, rr_mid, rr_up)
  
################################################################################################################################
#                                             4. Fonction de génération de distributions normales                              #
################################################################################################################################
  
# Fixer une graine pour garantir la reproductibilité des simulations
  set.seed(123)
  
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

################################################################################################################################
#                                             5. Génération des distributions normales pour chaque RR                          #
################################################################################################################################
  
  rr_table <- rr_table %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(generate_RR_distrib(food_group, rr_mid, rr_low, rr_up)))
  
################################################################################################################################
#                                             6. Tri des listes de RR par odre croissant                                       #
################################################################################################################################
  
  rr_table <- rr_table %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(sort(unlist(rr_distrib)))) %>%
    ungroup()

  
################################################################################################################################
#                                             7. Transformation au format long                                                 #
################################################################################################################################
  
# Transformer les simulations en format long
  rr_table_long <- rr_table %>% 
    unnest_wider(rr_distrib, names_sep = "_") %>%  # Séparer les simulations en colonnes distinctes
    pivot_longer(
      cols = starts_with("rr_distrib_"),  # Sélectionner toutes les colonnes de simulations
      names_to = "simulation_id",  # Nom de la colonne contenant les identifiants de simulation
      values_to = "simulated_rr"  # Nom de la colonne contenant les valeurs simulées
    )

  rr_table_interpolated <- rr_table_long %>%
    group_by(food_group, simulation_id) %>%
    complete(quantity = full_seq(0:800, 1)) %>%
    arrange(quantity) %>%
    mutate(rr_interpolated = case_when(
      interpolation == "linear" ~ if_else(is.na(simulated_rr), approx(quantity, simulated_rr, xout = quantity, method = "linear", rule = 1)$y, simulated_rr),
      interpolation == "spline" ~ if_else(is.na(simulated_rr), spline(quantity, simulated_rr, xout = quantity)$y, simulated_rr)
    )) %>% 
    # $y, rr, récupérer les valeurs interpolées en y de la fonction approx et les attribuer à rr
    mutate(rr_interpolated = if_else(quantity > max(quantity[!is.na(simulated_rr)]), NA_real_, rr_interpolated)) %>%
    ungroup() %>% 
    select("simulation_id", "food_group", "quantity", "rr_interpolated")
  
################################################################################################################################
#                                             8. Représentations graphique                                                     #
################################################################################################################################
  
# Dairy
  graph_dr_sim_dairy <- ggplot(rr_table_interpolated %>% 
                                 filter(food_group == "dairy"),
                               aes(x = quantity,
                                   y = rr_interpolated,
                                   group = simulation_id,
                                   color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for dairy intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Eggs
  graph_dr_sim_eggs <- ggplot(rr_table_interpolated %>% 
                                 filter(food_group == "eggs",
                                        quantity %in% 0:70),
                               aes(x = quantity,
                                   y = rr_interpolated,
                                   group = simulation_id,
                                   color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for eggs intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Fish
  graph_dr_sim_fish <- ggplot(rr_table_interpolated %>% 
                                filter(food_group == "fish",
                                       quantity %in% 0:250),
                              aes(x = quantity,
                                  y = rr_interpolated,
                                  group = simulation_id,
                                  color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for fish intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Fruits
  graph_dr_sim_fruits <- ggplot(rr_table_interpolated %>% 
                                filter(food_group == "fruits",
                                       quantity %in% 0:600),
                              aes(x = quantity,
                                  y = rr_interpolated,
                                  group = simulation_id,
                                  color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for fruits intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Legumes
  graph_dr_sim_legumes <- ggplot(rr_table_interpolated %>% 
                                  filter(food_group == "legumes",
                                         quantity %in% 0:600),
                                aes(x = quantity,
                                    y = rr_interpolated,
                                    group = simulation_id,
                                    color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for legumes intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Nuts
  graph_dr_sim_nuts <- ggplot(rr_table_interpolated %>% 
                                   filter(food_group == "nuts",
                                          quantity %in% 0:30),
                                 aes(x = quantity,
                                     y = rr_interpolated,
                                     group = simulation_id,
                                     color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for nuts intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Processed meat
  graph_dr_sim_processed_meat <- ggplot(rr_table_interpolated %>% 
                                   filter(food_group == "processed_meat",
                                          quantity %in% 0:200),
                                 aes(x = quantity,
                                     y = rr_interpolated,
                                     group = simulation_id,
                                     color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for processed meat intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Red meat
  graph_dr_sim_red_meat <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "red_meat",
                                                 quantity %in% 0:200),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for red meat intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
# Refined grains
  graph_dr_sim_refined_grains <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "reffined_grains",
                                                 quantity %in% 0:150),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for refined grains intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# SSB
  graph_dr_sim_ssb <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "sugar_sweetened_beverages",
                                                 quantity %in% 0:300),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for sugar_sweetened beverages intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Vegetables
  graph_dr_sim_vegetables <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "vegetables",
                                                 quantity %in% 0:600),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for vegetables intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# White meat
  graph_dr_sim_white_meat <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "white_meat",
                                                 quantity %in% 0:400),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for white meat intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Whole grains
  graph_dr_sim_whole_grains <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "whole_grains",
                                                 quantity %in% 0:250),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dose-response relationships for whole grains intake",
         x = "Quantity (g/day/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
################################################################################################################################
#                                             8. Exportation des données                                                       #
################################################################################################################################
  
# Table des relations dose-réponse simulées
  export(rr_table_interpolated, here("data_clean", "rr_table_interpolated_sim.csv"))
  
# Représentations graphiques
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_dairy.pdf"), plot = graph_dr_sim_dairy)
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_eggs.pdf"), plot = graph_dr_sim_eggs)
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_fish.pdf"), plot = graph_dr_sim_fish)
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_fruits.pdf"), plot = graph_dr_sim_fruits)
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_legumes.pdf"), plot = graph_dr_sim_legumes)
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_nuts.pdf"), plot = graph_dr_sim_nuts)
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_processed_meat.pdf"), plot = graph_dr_sim_processed_meat)
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_red_meat.pdf"), plot = graph_dr_sim_red_meat)  
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_refined_grains.pdf"), plot = graph_dr_sim_refined_grains)  
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_ssb.pdf"), plot = graph_dr_sim_ssb)  
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_vegetables.pdf"), plot = graph_dr_sim_vegetables)  
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_white_meat.pdf"), plot = graph_dr_sim_white_meat)  
  ggsave(here("results", "dose_response_curves_sim", "dr_sim_whole_grains.pdf"), plot = graph_dr_sim_whole_grains)  
  