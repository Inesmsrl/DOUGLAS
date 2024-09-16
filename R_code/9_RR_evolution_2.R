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
rr_table <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Mid")

# Table des RR, IC95 lower
rr_table_low <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Lower")

# Table des RR, IC95 upper
rr_table_up <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Upper")

# Implémentation linéaire des régimes
diets_lin <- import(here("data_clean", "diets_evo_lin_scenarios.xlsx"))

# Implémentation par interpolation cosinus des régimes
diets_cos <- import(here("data_clean", "diets_evo_cos_scenarios.xlsx"))

# Implémentation par sigmoïdale des régimes
diets_sig <- import(here("data_clean", "diets_evo_sig_scenarios.xlsx"))


################################################################################################################################
#                                             3. Calcul des rr pour chaque quantité consommée                                  #
################################################################################################################################

# Mid
  rr_table_long <- rr_table %>% 
    pivot_longer(cols = `0`:`800`, names_to = "quantity", values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity))
  
  rr_table_interpolated <- rr_table_long %>%
    group_by(food_group) %>%
    complete(quantity = full_seq(0:800, 1)) %>%
    arrange(quantity) %>%
    mutate(rr_interpolated = if_else(is.na(rr), approx(quantity, rr, xout = quantity, method = "linear", rule = 1)$y, rr)) %>% 
    # $y, rr, récupérer les valeurs interpolées en y de la fonction approx et les attribuer à rr
    mutate(rr_interpolated = if_else(quantity > max(quantity[!is.na(rr)]), NA_real_, rr_interpolated)) %>%
    ungroup() %>% 
    select("food_group", "quantity", "rr_interpolated")
  
  rr_table_interpolated_wide <- rr_table_interpolated %>% 
    pivot_wider(names_from = quantity, values_from = rr_interpolated)

# Upper
  rr_table_up_long <- rr_table_up %>% 
    pivot_longer(cols = `0`:`800`, names_to = "quantity", values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity))
  
  rr_table_up_interpolated <- rr_table_up_long %>%
    group_by(food_group) %>%
    complete(quantity = full_seq(0:800, 1)) %>%
    arrange(quantity) %>%
    mutate(rr_interpolated = if_else(is.na(rr), approx(quantity, rr, xout = quantity, method = "linear", rule = 1)$y, rr)) %>%
    mutate(rr_interpolated = if_else(quantity > max(quantity[!is.na(rr)]), NA_real_, rr_interpolated)) %>%
    ungroup() %>% 
    select("food_group", "quantity", "rr_interpolated")
  
  rr_table_up_interpolated_wide <- rr_table_up_interpolated %>% 
    pivot_wider(names_from = quantity, values_from = rr_interpolated)
  
# Lower
  rr_table_low_long <- rr_table_low %>% 
    pivot_longer(cols = `0`:`800`, names_to = "quantity", values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity))
  
  rr_table_low_interpolated <- rr_table_low_long %>%
    group_by(food_group) %>%
    complete(quantity = full_seq(0:800, 1)) %>%
    arrange(quantity) %>%
    mutate(rr_interpolated = if_else(is.na(rr), approx(quantity, rr, xout = quantity, method = "linear", rule = 1)$y, rr)) %>%
    mutate(rr_interpolated = if_else(quantity > max(quantity[!is.na(rr)]), NA_real_, rr_interpolated)) %>%
    ungroup() %>% 
    select("food_group", "quantity", "rr_interpolated")
  
  rr_table_low_interpolated_wide <- rr_table_low_interpolated %>% 
    pivot_wider(names_from = quantity, values_from = rr_interpolated)
  
################################################################################################################################
#                                             4. Représentations graphiques des relations dose-réponse                         #
################################################################################################################################
  
# Tous les aliments
  dose_rep <- rr_table_interpolated %>% 
    rename("rr_mid" = "rr_interpolated") %>% 
    left_join(rr_table_low_interpolated, by = c("food_group", "quantity")) %>% 
    rename("rr_low" = "rr_interpolated") %>% 
    left_join(rr_table_up_interpolated, by = c("food_group", "quantity")) %>% 
    rename("rr_up" = "rr_interpolated")
  
# Red meat
  dose_rep_red_meat <- dose_rep %>% 
    filter(food_group == "red_meat") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_red_meat <- ggplot(dose_rep_red_meat, aes(x = quantity,
                                                           y = rr,
                                                           color = IC95,
                                                           linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for red meat intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_red_meat_200 <- dose_rep %>% 
    filter(food_group == "red_meat",
           quantity %in% 0:200) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_red_meat_200 <- ggplot(dose_rep_red_meat_200, aes(x = quantity,
                                                                   y = rr,
                                                                   color = IC95,
                                                                   linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for red meat intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Processed meat
  dose_rep_processed_meat <- dose_rep %>% 
    filter(food_group == "processed_meat") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_processed_meat <- ggplot(dose_rep_processed_meat, aes(x = quantity,
                                                                       y = rr,
                                                                       color = IC95,
                                                                       linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for processed meat intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_processed_meat_200 <- dose_rep %>% 
    filter(food_group == "processed_meat",
           quantity %in% 0:200) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_processed_meat_200 <- ggplot(dose_rep_processed_meat_200, aes(x = quantity,
                                                                               y = rr,
                                                                               color = IC95,
                                                                               linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for processed meat intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
# White meat
  dose_rep_white_meat <- dose_rep %>% 
    filter(food_group == "white_meat") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_white_meat <- ggplot(dose_rep_white_meat, aes(x = quantity,
                                                               y = rr,
                                                               color = IC95,
                                                               linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for white meat intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Dairy
  dose_rep_dairy <- dose_rep %>% 
    filter(food_group == "dairy") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_dairy <- ggplot(dose_rep_dairy, aes(x = quantity,
                                                     y = rr,
                                                     color = IC95,
                                                     linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for dairy intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Fish
  dose_rep_fish <- dose_rep %>% 
    filter(food_group == "fish") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_fish <- ggplot(dose_rep_fish, aes(x = quantity,
                                                   y = rr,
                                                   color = IC95,
                                                   linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for fish intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_fish_250 <- dose_rep %>% 
    filter(food_group == "fish",
           quantity %in% 0:250) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_fish_250 <- ggplot(dose_rep_fish_250, aes(x = quantity,
                                                           y = rr,
                                                           color = IC95,
                                                           linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for fish intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Eggs
  dose_rep_eggs <- dose_rep %>% 
    filter(food_group == "eggs") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_eggs <- ggplot(dose_rep_eggs, aes(x = quantity,
                                                   y = rr,
                                                   color = IC95,
                                                   linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for eggs intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_eggs_70 <- dose_rep %>% 
    filter(food_group == "eggs",
           quantity %in% 0:70) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_eggs_70 <- ggplot(dose_rep_eggs_70, aes(x = quantity,
                                                         y = rr,
                                                         color = IC95,
                                                         linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for eggs intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Fruits
  dose_rep_fruits <- dose_rep %>% 
    filter(food_group == "fruits") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_fruits <- ggplot(dose_rep_fruits, aes(x = quantity,
                                                       y = rr,
                                                       color = IC95,
                                                       linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for fruits intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_fruits_600 <- dose_rep %>% 
    filter(food_group == "fruits",
           quantity %in% 0:600) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_fruits_600 <- ggplot(dose_rep_fruits_600, aes(x = quantity,
                                                               y = rr,
                                                               color = IC95,
                                                               linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for fruits intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Nuts
  dose_rep_nuts <- dose_rep %>% 
    filter(food_group == "nuts") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_nuts <- ggplot(dose_rep_nuts, aes(x = quantity,
                                                     y = rr,
                                                     color = IC95,
                                                     linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for nuts intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_nuts_30 <- dose_rep %>% 
    filter(food_group == "nuts",
           quantity %in% 0:30) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_nuts_30 <- ggplot(dose_rep_nuts_30, aes(x = quantity,
                                                         y = rr,
                                                         color = IC95,
                                                         linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for nuts intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
# Vegetables
  dose_rep_veg <- dose_rep %>% 
    filter(food_group == "vegetables") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_veg <- ggplot(dose_rep_veg, aes(x = quantity,
                                                 y = rr,
                                                 color = IC95,
                                                 linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for vegatables intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_veg_600 <- dose_rep %>% 
    filter(food_group == "vegetables",
           quantity %in% 0:600) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_veg_600 <- ggplot(dose_rep_veg_600, aes(x = quantity,
                                                         y = rr,
                                                         color = IC95,
                                                         linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for vegatables intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
# Legumes
  dose_rep_leg <- dose_rep %>% 
    filter(food_group == "legumes") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_leg <- ggplot(dose_rep_leg, aes(x = quantity,
                                                 y = rr,
                                                 color = IC95,
                                                 linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for legumes intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_leg_200 <- dose_rep %>% 
    filter(food_group == "legumes",
           quantity %in% 0:200) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_leg_200 <- ggplot(dose_rep_leg_200, aes(x = quantity,
                                                         y = rr,
                                                         color = IC95,
                                                         linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for legumes intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Whole grains
  dose_rep_wg <- dose_rep %>% 
    filter(food_group == "whole_grains") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_wg <- ggplot(dose_rep_wg, aes(x = quantity,
                                               y = rr,
                                               color = IC95,
                                               linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for whole grains intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_wg_250 <- dose_rep %>% 
    filter(food_group == "whole_grains",
           quantity %in% 0:250) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_wg_250 <- ggplot(dose_rep_wg_250, aes(x = quantity,
                                                       y = rr,
                                                       color = IC95,
                                                       linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for whole grains intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Refined grains
  dose_rep_rg <- dose_rep %>% 
    filter(food_group == "reffined_grains") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_rg <- ggplot(dose_rep_rg, aes(x = quantity,
                                               y = rr,
                                               color = IC95,
                                               linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for refined grains intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_rg_150 <- dose_rep %>% 
    filter(food_group == "reffined_grains",
           quantity %in% 0:150) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_rg_150 <- ggplot(dose_rep_rg_150, aes(x = quantity,
                                                       y = rr,
                                                       color = IC95,
                                                       linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for refined grains intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
# Added plant oils
  dose_rep_apo <- dose_rep %>% 
    filter(food_group == "added_plant_oils") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_apo <- ggplot(dose_rep_apo, aes(x = quantity,
                                                 y = rr,
                                                 color = IC95,
                                                 linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for added plant oils intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Sugar-sweetened beverages
  dose_rep_ssb <- dose_rep %>% 
    filter(food_group == "sugar_sweetened_beverages") %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_ssb <- ggplot(dose_rep_ssb, aes(x = quantity,
                                                 y = rr,
                                                 color = IC95,
                                                 linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for sugar-sweetened beverages intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  dose_rep_ssb_300 <- dose_rep %>% 
    filter(food_group == "sugar_sweetened_beverages",
           quantity %in% 0:300) %>% 
    pivot_longer(cols = starts_with("rr_"),
                 names_to = "IC95",
                 values_to = "rr")
  
  graph_dose_rep_ssb_300 <- ggplot(dose_rep_ssb_300, aes(x = quantity,
                                                         y = rr,
                                                         color = IC95,
                                                         linetype = IC95))+
    geom_line(na.rm = TRUE)+
    scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
    scale_color_manual(values = c("black", "black", "black"))+
    labs(title = "dose-response curve for sugar-sweetened beverages intake",
         x = "intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
################################################################################################################################
#                                             5. Attribution des rr à chaque régime                                            #
################################################################################################################################

# Dynamique d'implémentation linéaire des régimes alimentaires
diets_lin <- diets_lin %>% 
  mutate(quantity = round(quantity))

rr_evo_lin <- diets_lin %>% 
  left_join(rr_table_interpolated, by = c("food_group", "quantity"))

# Dynamique d'implémentation par interpolation cosinus des régimes alimentaires
diets_cos <- diets_cos %>% 
  mutate(quantity = round(quantity))

rr_evo_cos <- diets_cos %>% 
  left_join(rr_table_interpolated, by = c("food_group", "quantity"))

# Dynamique d'implémentation sigmoïdale des régimes alimentaires
diets_sig <- diets_sig %>% 
  mutate(quantity = round(quantity))

rr_evo_sig <- diets_sig %>% 
  left_join(rr_table_interpolated, by = c("food_group", "quantity"))

################################################################################################################################
#                                             6. Combinaison des rr                                                            #
################################################################################################################################

# Pour chaque année et chaque scénario, multiplication des rr des aliments
# On obtient l'évolution rr du régime de chaque scénario par année

calc_combined_rr <- function(df) {
  df %>%
    group_by(scenario, year) %>%
    summarize(combined_rr = prod(rr_interpolated, na.rm = TRUE)) %>%
    ungroup()
}

# rr en fonction de l'implémentation linéaire des régimes 
combined_rr_table_lin <- calc_combined_rr(rr_evo_lin)

# rr en fonction de l'implémentation par interpolation cosinus des régimes
combined_rr_table_cos <- calc_combined_rr(rr_evo_cos)

# rr en fonction de l'implémentation sigmoïdale des régimes 
combined_rr_table_sig <- calc_combined_rr(rr_evo_sig)

################################################################################################################################
#                                             7. Représentations graphiques                                                    #
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
graph_rr_evo_lin <- ggplot(combined_rr_table_lin, aes(x = year,
                                                      y = combined_rr,
                                                      colour = scenario))+
  geom_line(size = 1)+
  scale_color_manual(values = col_scenario)+
  labs(title = "Whole diet RR evolution in each scenario",
       subtitle = "linear implementation of diets from 2019 to 2050",
       x = "",
       y = "Diet RR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Implémentation par interpolation cosinus des régimes
graph_rr_evo_cos <- ggplot(combined_rr_table_cos, aes(x = year,
                                                      y = combined_rr,
                                                      colour = scenario))+
  geom_line(size = 1)+
  scale_color_manual(values = col_scenario)+
  labs(title = "Whole diet RR evolution in each scenario",
       subtitle = "cosine interpolation implementation of diets from 2019 to 2050",
       x = "",
       y = "Diet RR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Implémentation sigmoïdale des régimes
graph_rr_evo_sig <- ggplot(combined_rr_table_sig, aes(x = year,
                                                      y = combined_rr,
                                                      colour = scenario))+
  geom_line(size = 1)+
  scale_color_manual(values = col_scenario)+
  labs(title = "Whole diet RR evolution in each scenario",
       subtitle = "sigmoidal implementation of diets from 2019 to 2050",
       x = "",
       y = "Diet RR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################################################################
#                                             8. Exportation des données                                                       #
################################################################################################################################

# Table complète des rr pour chaque quantité d'aliment
export(rr_table_interpolated_wide ,here("data_clean", "rr_table_interpolated.xlsx"))
export(rr_table_low_interpolated_wide ,here("data_clean", "rr_table_low_interpolated.xlsx"))
export(rr_table_up_interpolated_wide ,here("data_clean", "rr_table_up_interpolated.xlsx"))

# Courbes dose-réponse
ggsave(here("results", "dose_response_curves", "dr_curve_apo.pdf"), plot = graph_dose_rep_apo)
ggsave(here("results", "dose_response_curves", "dr_curve_red_meat.pdf"), plot = graph_dose_rep_red_meat)
ggsave(here("results", "dose_response_curves", "dr_curve_red_meat_200.pdf"), plot = graph_dose_rep_red_meat_200)
ggsave(here("results", "dose_response_curves", "dr_curve_processed_meat.pdf"), plot = graph_dose_rep_processed_meat)
ggsave(here("results", "dose_response_curves", "dr_curve_processed_meat_200.pdf"), plot = graph_dose_rep_processed_meat_200)
ggsave(here("results", "dose_response_curves", "dr_curve_white_meat.pdf"), plot = graph_dose_rep_white_meat)
ggsave(here("results", "dose_response_curves", "dr_curve_dairy.pdf"), plot = graph_dose_rep_dairy)
ggsave(here("results", "dose_response_curves", "dr_curve_fish.pdf"), plot = graph_dose_rep_fish)
ggsave(here("results", "dose_response_curves", "dr_curve_fish_250.pdf"), plot = graph_dose_rep_fish_250)
ggsave(here("results", "dose_response_curves", "dr_curve_eggs.pdf"), plot = graph_dose_rep_eggs)
ggsave(here("results", "dose_response_curves", "dr_curve_eggs_70.pdf"), plot = graph_dose_rep_eggs_70)
ggsave(here("results", "dose_response_curves", "dr_curve_fruits.pdf"), plot = graph_dose_rep_fruits)
ggsave(here("results", "dose_response_curves", "dr_curve_fruits_600.pdf"), plot = graph_dose_rep_fruits_600)
ggsave(here("results", "dose_response_curves", "dr_curve_nuts.pdf"), plot = graph_dose_rep_nuts)
ggsave(here("results", "dose_response_curves", "dr_curve_nuts_30.pdf"), plot = graph_dose_rep_nuts_30)
ggsave(here("results", "dose_response_curves", "dr_curve_vegetables.pdf"), plot = graph_dose_rep_veg)
ggsave(here("results", "dose_response_curves", "dr_curve_vegetables_600.pdf"), plot = graph_dose_rep_veg_600)
ggsave(here("results", "dose_response_curves", "dr_curve_legumes.pdf"), plot = graph_dose_rep_leg)
ggsave(here("results", "dose_response_curves", "dr_curve_whole_grains.pdf"), plot = graph_dose_rep_wg)
ggsave(here("results", "dose_response_curves", "dr_curve_whole_grains_250.pdf"), plot = graph_dose_rep_wg_250)
ggsave(here("results", "dose_response_curves", "dr_curve_refined_grains.pdf"), plot = graph_dose_rep_rg)
ggsave(here("results", "dose_response_curves", "dr_curve_refined_grains_150.pdf"), plot = graph_dose_rep_rg_150)
ggsave(here("results", "dose_response_curves", "dr_curve_ssb.pdf"), plot = graph_dose_rep_ssb)
ggsave(here("results", "dose_response_curves", "dr_curve_ssb_300.pdf"), plot = graph_dose_rep_ssb_300)

# rr par scénario, par année et pour chaque aliment selon la dose consomée
export(rr_evo_lin, here("data_clean", "rr_evo_lin_2.xlsx"))
export(rr_evo_cos, here("data_clean", "rr_evo_cos_2.xlsx"))
export(rr_evo_sig, here("data_clean", "rr_evo_sig_2.xlsx"))

# RR combinés pour chaque scénario, par année
export(combined_rr_table_lin, here("data_clean", "combined_rr_lin_2.xlsx"))
export(combined_rr_table_cos, here("data_clean", "combined_rr_cos_2.xlsx"))
export(combined_rr_table_sig, here("data_clean", "combined_rr_sig_2.xlsx"))

# Evolution dans le temps des RR combinés dans chaque scénario
ggsave(here("results", "Diets_RR_evo_lin_2.pdf"), plot = graph_rr_evo_lin)
ggsave(here("results", "Diets_RR_evo_cos_2.pdf"), plot = graph_rr_evo_cos)
ggsave(here("results", "Diets_RR_evo_sig_2.pdf"), plot = graph_rr_evo_sig)
