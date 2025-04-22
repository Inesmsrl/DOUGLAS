################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  tidyverse,           # Data management, inclus ggplot
  patchwork            # Combinaison de graphes
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
col_food_groups <- c(
  "red_meat" = "#ff1047",
  "processed_meat" = "#650115",
  "white_meat" = "#FF9DC8",
  "dairy" = "#022f66",
  "fish" = "#4993a2",
  "eggs" = "#ff764d",
  "fruits" = "#00CBA7",
  "nuts" = "#ffc744",
  "vegetables" = "#00735C",
  "legumes" = "#703895",
  "whole_grains" = "#572d00",
  "reffined_grains" = "#cbb4a1",
  "added_plant_oils" = "#FF6E3A",
  "sugar_sweetened_beverages" = "#1b1b1b"
)

# Méthode d'interpolation ("linear", "spline")
  interpolation <- "spline"
  
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
    rename("rr_mid" = "rr.x",
           "rr_low" = "rr.y",
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
#                                             7. Interpolation                                                                 #
################################################################################################################################
  
# Transformer les simulations en format long
  rr_table_long <- rr_table %>% 
    unnest_wider(rr_distrib, names_sep = "_") %>%  # Séparer les simulations en colonnes distinctes
    pivot_longer(
      cols = starts_with("rr_distrib_"),  # Sélectionner toutes les colonnes de simulations
      names_to = "simulation_id",  # Nom de la colonne contenant les identifiants de simulation
      values_to = "simulated_rr"  # Nom de la colonne contenant les valeurs simulées
    )

# Interpolation 
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
  
  rr_table_interpolated <- rr_table_interpolated %>% 
    group_by(food_group) %>% 
    mutate(simulation_id = sample(unique(simulation_id))[match(simulation_id, unique(simulation_id))]) %>% 
    ungroup()
  
################################################################################################################################
#                                             8. Représentations graphique                                                     #
################################################################################################################################

# Nouvel ensemble de DRF 
rr_table_interpolated_1 <- import(here("data_clean", "CORRECTION", "rr_table_interpolated_sim.csv"))

rr_table_interpolated_1 <- rr_table_interpolated_1 %>% 
  filter(food_group %in% c("red_meat", "fish", "dairy", "vegetables", "fruits", "legumes", "processed_meat"))

rr_table_interpolated <- rr_table_interpolated %>%
  bind_rows(rr_table_interpolated_1)

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
    labs(title = "Dairy",
         x = "Intake (g/d/pers)",
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
    labs(title = "Eggs",
         x = "Intake (g/d/pers)",
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
    labs(title = "Fish",
         x = "Intake (g/d/pers)",
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
    labs(title = "Fruits",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Legumes
  graph_dr_sim_legumes <- ggplot(rr_table_interpolated %>% 
                                  filter(food_group == "legumes",
                                         quantity %in% 0:150),
                                aes(x = quantity,
                                    y = rr_interpolated,
                                    group = simulation_id,
                                    color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Legumes",
         x = "Intake (g/d/pers)",
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
    labs(title = "Nuts",
         x = "Intake (g/d/pers)",
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
    labs(title = "Processed meat",
         x = "Intake (g/d/pers)",
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
    labs(title = "Red meat",
         x = "Intake (g/d/pers)",
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
    labs(title = "Refined grains",
         x = "Intake (g/d/pers)",
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
    labs(title = "SSB",
         x = "Intake (g/d/pers)",
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
    labs(title = "Vegetables",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# White meat
  graph_dr_sim_white_meat <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "white_meat",
                                                 quantity %in% 0:100),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "White meat",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Whole grains
  graph_dr_sim_whole_grains <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "whole_grains",
                                                 quantity %in% 0:220),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Whole grains",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
# Toutes les DRF sur une figure
  
 list_dr <- list(graph_dr_sim_dairy, graph_dr_sim_eggs, graph_dr_sim_fish, graph_dr_sim_fruits, graph_dr_sim_legumes,
                 graph_dr_sim_nuts, graph_dr_sim_processed_meat, graph_dr_sim_red_meat, graph_dr_sim_refined_grains,
                 graph_dr_sim_ssb, graph_dr_sim_vegetables, graph_dr_sim_white_meat, graph_dr_sim_whole_grains)
 
 common_theme <- theme(
   plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
   axis.title = element_text(size = 8),
   axis.text = element_text(size = 7)
 )
 
 list_dr <- lapply(list_dr, function(p) p + common_theme)
 
 combined_plot <- reduce(list_dr, `+`) + plot_layout(ncol = 4)
 
 print(combined_plot) 
  
################################################################################################################################
#                                             8. Exportation des données                                                       #
################################################################################################################################
  
# Table des relations dose-réponse simulées
  export(rr_table_interpolated, here("data_clean", "CORRECTION", "rr_table_interpolated_sim.csv"))
  
# Représentations graphiques
  ggsave(here("results", "DRF", "CORRECTION", "drf_dairy.pdf"), plot = graph_dr_sim_dairy)
  ggsave(here("results", "DRF", "CORRECTION", "drf_eggs.pdf"), plot = graph_dr_sim_eggs)
  ggsave(here("results", "DRF", "CORRECTION", "drf_fish.pdf"), plot = graph_dr_sim_fish)
  ggsave(here("results", "DRF", "CORRECTION", "drf_fruits.pdf"), plot = graph_dr_sim_fruits)
  ggsave(here("results", "DRF", "CORRECTION", "drf_legumes.pdf"), plot = graph_dr_sim_legumes)
  ggsave(here("results", "DRF", "CORRECTION", "drf_nuts.pdf"), plot = graph_dr_sim_nuts)
  ggsave(here("results", "DRF", "CORRECTION", "drf_processed_meat.pdf"), plot = graph_dr_sim_processed_meat)
  ggsave(here("results", "DRF", "CORRECTION", "drf_red_meat.pdf"), plot = graph_dr_sim_red_meat)  
  ggsave(here("results", "DRF", "CORRECTION", "drf_refined_grains.pdf"), plot = graph_dr_sim_refined_grains)  
  ggsave(here("results", "DRF", "CORRECTION", "drf_ssb.pdf"), plot = graph_dr_sim_ssb)  
  ggsave(here("results", "DRF", "CORRECTION", "drf_vegetables.pdf"), plot = graph_dr_sim_vegetables)  
  ggsave(here("results", "DRF", "CORRECTION", "drf_white_meat.pdf"), plot = graph_dr_sim_white_meat)  
  ggsave(here("results", "DRF", "CORRECTION", "drf_whole_grains.pdf"), plot = graph_dr_sim_whole_grains)  
  
  ggsave(here("results", "DRF", "CORRECTION", "drf_all.pdf"), plot = combined_plot) 
  

# Nouvelles DRF
export(rr_table_interpolated, here("data_clean", "rr_table_interpolated_sim_2.csv"))

ggsave(here("results", "DRF", "NEW_DRF", "drf_white_meat_2.pdf"), plot = graph_dr_sim_white_meat)
ggsave(here("results", "DRF", "NEW_DRF", "drf_nuts_2.pdf"), plot = graph_dr_sim_nuts)
ggsave(here("results", "DRF", "NEW_DRF", "drf_whole_grains_2.pdf"), plot = graph_dr_sim_whole_grains)
ggsave(here("results", "DRF", "NEW_DRF", "drf_refined_grains_2.pdf"), plot = graph_dr_sim_refined_grains)
ggsave(here("results", "DRF", "NEW_DRF", "drf_ssb_2.pdf"), plot = graph_dr_sim_ssb)
ggsave(here("results", "DRF", "NEW_DRF", "drf_eggs_2.pdf"), plot = graph_dr_sim_eggs)
ggsave(here("results", "DRF", "NEW_DRF", "drf_dairy_2.pdf"), plot = graph_dr_sim_dairy)
ggsave(here("results", "DRF", "NEW_DRF", "drf_fish_2.pdf"), plot = graph_dr_sim_fish)
ggsave(here("results", "DRF", "NEW_DRF", "drf_vegetables_2.pdf"), plot = graph_dr_sim_vegetables)
ggsave(here("results", "DRF", "NEW_DRF", "drf_legumes_2.pdf"), plot = graph_dr_sim_legumes)
ggsave(here("results", "DRF", "NEW_DRF", "drf_fruits_2.pdf"), plot = graph_dr_sim_fruits)
ggsave(here("results", "DRF", "NEW_DRF", "drf_red_meat_2.pdf"), plot = graph_dr_sim_red_meat)
ggsave(here("results", "DRF", "NEW_DRF", "drf_processed_meat_2.pdf"), plot = graph_dr_sim_processed_meat)

ggsave(here("results", "DRF", "NEW_DRF", "drf_all_2.pdf"), plot = combined_plot)

