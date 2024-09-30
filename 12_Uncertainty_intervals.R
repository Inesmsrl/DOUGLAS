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

# Bornes temporelles des changements de régime alimentaire (années)
  year_i <- 2025 # Année initiale
  year_f <- 2050 # Année finale
  
# Temps pendant lequel le régime reste stationnaire avant et après le changement de régime (années)
  stability_time <- 25
  
# Dynamique d'implémentation des régimes (immediate, linear, cosine, sigmoidal)
  implementation <- "linear"
  
# paramètre de la courbe d'interpolation cosinus
  p <- 1
  
# paramètre de la courbe sigmoïdale
  lambda <- 5
  
# Paramètre de modification d'effet des RR 
  # 0.5 à 1 = réduction d'effet, modèle conservateur
  # 1 à 1.5 = augmentation d'effet, modèle radical
  m <- 0.75
  
#  Time to full effect
  # durée (années)
  ttfe_time <- 20
  
  # Dynamique (immediate, linear, cosine, sigmoidal, log)
  ttfe_dynamics <- "immediate"
  
  # paramètre de la courbe d'interpolation cosinus
  p_ttfe <- 1
  
  # paramètre de la courbe sigmoïdale
  lambda_ttfe <- 8
  
  # paramètre de la courbe log 
  eta_ttfe <- 1
  
# Combinaison des RR de chaque aliment par année (arithmetic mean, geometric mean)
  combinaison_rr_type <- "geometric mean"
  
# Charte graphique
  col_scenario <- c("actuel" = "azure4",
                    "sc0" = "palevioletred3",
                    "sc1" = "aquamarine2",
                    "sc2" = "#DDCC77",
                    "sc3" = "lightskyblue3",
                    "sc4" = "#882255",
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
    select("food_group", "actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5") %>% 
    mutate(q_i = actuel) %>% 
    pivot_longer(cols = c("actuel", "sc0", "sc1", "sc2", "sc3", "sc4", "sc5"), 
                 names_to = "scenario", 
                 values_to = "q_f") %>%  
    crossing(year_n = (year_i - stability_time) : (year_f + stability_time)) %>%
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
  
# Ordonnner les groupes alimentaires
  diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)

################################################################################################################################
#                                             7. Modification d'effet des RR                                                         #
################################################################################################################################
  
  rr_table_low <- rr_table_low %>% 
    mutate(rr_a = case_when(
      rr < 1 ~ rr + (1 - rr) * (1 - m),
      rr >= 1 ~ 1 / (1/rr + (1 - 1/rr) * (1 - m))
    )) %>% 
    select("food_group", "quantity", "rr_a") %>% 
    rename("rr" = "rr_a")
  
  rr_table_mid <- rr_table_mid %>% 
    mutate(rr_a = case_when(
      rr < 1 ~ rr + (1 - rr) * (1 - m),
      rr >= 1 ~ 1 / (1/rr + (1 - 1/rr) * (1 - m))
    )) %>% 
    select("food_group", "quantity", "rr_a") %>% 
    rename("rr" = "rr_a")

  rr_table_up <- rr_table_up %>% 
    mutate(rr_a = case_when(
      rr < 1 ~ rr + (1 - rr) * (1 - m),
      rr >= 1 ~ 1 / (1/rr + (1 - 1/rr) * (1 - m))
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
  
  generate_RR_distrib = function(RR, low, sup, N=200){
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
    distr_RR[distr_RR>1]=1
    distr_RR[distr_RR<0]=0
    return(distr_RR)
  }

  diets_evo <- diets_evo %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(generate_RR_distrib(rr_mid, rr_low, rr_up)))
  
################################################################################################################################
#                                             10. Simulations des valeurs de RR                                                #
################################################################################################################################
  
# Fixer une graine pour garantir la reproductibilité des simulations
  set.seed(123)
  
# Fonction de simulation
  simulate_rr <- function(distribution, N = 10) {
    sample(distribution, size = N, replace = TRUE)
  }

diets_evo <- diets_evo %>% 
  rowwise() %>% 
  mutate(simulations = list(simulate_rr(rr_distrib))) %>% 
  ungroup()

# Transformer les simulations en format long
  simulations_long <- diets_evo %>% 
    unnest_wider(simulations, names_sep = "_") %>%  # Séparer les simulations en colonnes distinctes
    pivot_longer(
      cols = starts_with("simulations_"),  # Sélectionner toutes les colonnes de simulations
      names_to = "simulation_id",  # Nom de la colonne contenant les identifiants de simulation
      values_to = "simulated_rr"  # Nom de la colonne contenant les valeurs simulées
    )

# Calculer la moyenne et les IC95 pour chaque année
  simulations_summary <- simulations_long %>%
    group_by(food_group, scenario, year, quantity) %>%
    summarise(
      mean_rr = mean(simulated_rr, na.rm = TRUE),  # Moyenne des simulations
      lower_ci = quantile(simulated_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(simulated_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    )

################################################################################################################################
#                                             11. Représentations graphiques des simulations des valeurs de RR                 #
################################################################################################################################
  
  # S0 = Tendanciel
    simulations_summary_sc0 <- simulations_summary %>% 
      filter(scenario == "sc0")
    
    ggplot(simulations_summary_sc0, aes(x = year,
                                    y = mean_rr,
                                    color = food_group)) +
      facet_wrap(~ food_group)+
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
      geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
      labs(
        title = "RR simulartions",
        x = "",
        y = "RR"
      )+
      scale_color_manual(values = col_food_groups)+
      scale_fill_manual(values = col_food_groups)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(0.5)))
    
    # S1
    simulations_summary_sc1 <- simulations_summary %>% 
      filter(scenario == "sc1")
    
    ggplot(simulations_summary_sc1, aes(x = year,
                                        y = mean_rr,
                                        color = food_group)) +
      facet_wrap(~ food_group)+
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
      geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
      labs(
        title = "RR simulartions",
        x = "",
        y = "RR"
      )+
      scale_color_manual(values = col_food_groups)+
      scale_fill_manual(values = col_food_groups)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(0.5)))
    
    # S2
    simulations_summary_sc2 <- simulations_summary %>% 
      filter(scenario == "sc2")
    
    ggplot(simulations_summary_sc2, aes(x = year,
                                        y = mean_rr,
                                        color = food_group)) +
      facet_wrap(~ food_group)+
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
      geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
      labs(
        title = "RR simulartions",
        x = "",
        y = "RR"
      )+
      scale_color_manual(values = col_food_groups)+
      scale_fill_manual(values = col_food_groups)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(0.5)))
    
    # S3
    simulations_summary_sc3 <- simulations_summary %>% 
      filter(scenario == "sc3")
    
    ggplot(simulations_summary_sc3, aes(x = year,
                                        y = mean_rr,
                                        color = food_group)) +
      facet_wrap(~ food_group)+
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
      geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
      labs(
        title = "RR simulartions",
        x = "",
        y = "RR"
      )+
      scale_color_manual(values = col_food_groups)+
      scale_fill_manual(values = col_food_groups)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(0.5)))

    # S4
    simulations_summary_sc4 <- simulations_summary %>% 
      filter(scenario == "sc4")
    
    ggplot(simulations_summary_sc4, aes(x = year,
                                        y = mean_rr,
                                        color = food_group)) +
      facet_wrap(~ food_group)+
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
      geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
      labs(
        title = "RR simulartions",
        x = "",
        y = "RR"
      )+
      scale_color_manual(values = col_food_groups)+
      scale_fill_manual(values = col_food_groups)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(0.5)))
    
    # S5
    simulations_summary_sc5 <- simulations_summary %>% 
      filter(scenario == "sc5")
    
    ggplot(simulations_summary_sc5, aes(x = year,
                                        y = mean_rr,
                                        color = food_group)) +
      facet_wrap(~ food_group)+
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
      geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
      labs(
        title = "RR simulartions",
        x = "",
        y = "RR"
      )+
      scale_color_manual(values = col_food_groups)+
      scale_fill_manual(values = col_food_groups)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(0.5)))
     
################################################################################################################################
#                                             12. TTFE                                                                         #
################################################################################################################################
    
# % du RR chaque année sur la période du time to full effect
  ttfe <- tibble(0:ttfe_time) %>% 
    rename("time" = "0:ttfe_time")
    
  ttfe <- ttfe %>% 
      mutate(ttfe = case_when(
        ttfe_time == 0 ~ 1,
        ttfe_dynamics == "immediate" & ttfe$time == 0 ~ 1,
        ttfe_dynamics == "immediate" & ttfe$time > 0 ~ NA_real_,
        ttfe_dynamics == "linear" ~ time/ttfe_time,
        ttfe_dynamics == "cosine" ~ (1 - cos(pi * (time/ttfe_time)^p_ttfe))/2,
        ttfe_dynamics == "sigmoidal" ~ (1 / (1 + exp(-lambda_ttfe * (time / ttfe_time - 1/2))) - 1 / (1 + exp(lambda_ttfe / 2))) / 
          (1 - 2 / (1 + exp(lambda_ttfe / 2))),
        ttfe_dynamics == "log" ~ log(1 + eta_ttfe * time/ttfe_time) / log(1 + eta_ttfe),
        TRUE ~ NA_real_
      ))
    
################################################################################################################################
#                                             13. Calcul des RR avec TTFE                                                      #
################################################################################################################################
  
# Calcul de la valeur des RR sur la durée du time to full effect
# Après le time to full effect : RR = NA
    simulations_long <- simulations_long %>% 
      rowwise() %>% 
      mutate(year_n = list(seq(from = (year_i - stability_time), to = (year_f + stability_time)))) %>% 
      unnest(year_n) %>% 
      mutate(simulated_rr_n = case_when(
        year_n < year ~ NA_real_,
        year_n >= year & year_n <= year + max(ttfe$time) ~ 1 + (simulated_rr - 1) * ttfe$ttfe[match(year_n - year, ttfe$time)],
        year_n > year + max(ttfe$time) ~ NA_real_
      )) %>%
      ungroup()

################################################################################################################################
#                                             14. Combinaison des RR de chaque aliment par année                               #
################################################################################################################################
  
# Calcul des RR de chaque aliment pour chaque année
  rr_evo_food_combined <- simulations_long %>% 
    group_by(scenario, year_n, food_group, simulation_id) %>% 
    summarize(mean_rr = case_when(
      combinaison_rr_type == "arithmetic mean" ~ mean(simulated_rr_n, na.rm = TRUE),
      combinaison_rr_type == "geometric mean"~ geometric.mean(simulated_rr_n, na.rm = TRUE))
    )
  
# Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_rr_fg_combined <- rr_evo_food_combined %>%
    group_by(food_group, scenario, year_n) %>%
    summarise(
      combined_rr = mean(mean_rr, na.rm = TRUE),  # Moyenne des simulations
      lower_ci = quantile(mean_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(mean_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    )
  
################################################################################################################################
#                                             15. Représentations graphiques des simulations des valeurs de RR combinés        #
################################################################################################################################
  
  # S0 = Tendanciel
  simulations_summary_sc0_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc0")
  
  ggplot(simulations_summary_sc0_c, aes(x = year_n,
                                      y = combined_rr,
                                      color = food_group)) +
    facet_wrap(~ food_group)+
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
    geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
    labs(
      title = "RR simulations",
      x = "",
      y = "RR"
    )+
    scale_color_manual(values = col_food_groups)+
    scale_fill_manual(values = col_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.5)))
  
  # S1
  simulations_summary_sc1_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc1")
  
  ggplot(simulations_summary_sc1_c, aes(x = year_n,
                                      y = combined_rr,
                                      color = food_group)) +
    facet_wrap(~ food_group)+
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
    geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
    labs(
      title = "RR simulations",
      x = "",
      y = "RR"
    )+
    scale_color_manual(values = col_food_groups)+
    scale_fill_manual(values = col_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.5)))
  
  # S2
  simulations_summary_sc2_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc2")
  
  ggplot(simulations_summary_sc2_c, aes(x = year_n,
                                      y = combined_rr,
                                      color = food_group)) +
    facet_wrap(~ food_group)+
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
    geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
    labs(
      title = "RR simulations",
      x = "",
      y = "RR"
    )+
    scale_color_manual(values = col_food_groups)+
    scale_fill_manual(values = col_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.5)))
  
  # S3
  simulations_summary_sc3_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc3")
  
  ggplot(simulations_summary_sc3_c, aes(x = year_n,
                                      y = combined_rr,
                                      color = food_group)) +
    facet_wrap(~ food_group)+
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
    geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
    labs(
      title = "RR simulations",
      x = "",
      y = "RR"
    )+
    scale_color_manual(values = col_food_groups)+
    scale_fill_manual(values = col_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.5)))
  
  # S4
  simulations_summary_sc4_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc4")
  
  ggplot(simulations_summary_sc4_c, aes(x = year_n,
                                      y = combined_rr,
                                      color = food_group)) +
    facet_wrap(~ food_group)+
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
    geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
    labs(
      title = "RR simulartions",
      x = "",
      y = "RR"
    )+
    scale_color_manual(values = col_food_groups)+
    scale_fill_manual(values = col_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.5)))
  
  # S5
  simulations_summary_sc5_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc5")
  
  ggplot(simulations_summary_sc5_c, aes(x = year_n,
                                      y = combined_rr,
                                      color = food_group)) +
    facet_wrap(~ food_group)+
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
    geom_line(size = 1, na.rm = TRUE) +  # Moyenne en trait plein
    labs(
      title = "RR simulartions",
      x = "",
      y = "RR"
    )+
    scale_color_manual(values = col_food_groups)+
    scale_fill_manual(values = col_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.5)))
  
################################################################################################################################
#                                             16. Combinaison des RR de chaque régime par année                                #
################################################################################################################################
  
  # Fonction produit des RR de chaque aliment par année
  calc_combined_rr <- function(df) {
    df %>%
      group_by(scenario, year, simulation_id) %>%
      summarize(combined_rr = prod(mean_rr, na.rm = TRUE)) %>%
      ungroup()
  }  
  
  # Calcul des RR des régimes de chaque scénario par année
  rr_evo_diets <- calc_combined_rr(rr_evo_food_combined) %>% 
    rename("year" = "year_n")
  
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_rr_diets <- rr_evo_diets %>%
    group_by(scenario, year) %>%
    summarise(
      mean_rr = mean(combined_rr, na.rm = TRUE),  # Moyenne des simulations
      lower_ci = quantile(combined_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(combined_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    )
  
################################################################################################################################
#                                             17. Représentations graphiques des simulations des valeurs de RR des régimes     #
################################################################################################################################
  

  ggplot(simulations_summary_rr_diets, aes(x = year,
                                           y = mean_rr,
                                           color = scenario)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5)+
    facet_wrap(~ scenario)+
    geom_line(size = 1, na.rm = TRUE)+ 
    labs(
      title = "RR simulations",
      x = "",
      y = "RR"
    )+
    scale_color_manual(values = col_scenario)+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.5)))
  
################################################################################################################################
#                                             18. Combinaison des RR de chaque régime par année et calcul relatif au RR actuel #
################################################################################################################################
  
 
  # Calcul des RR relatifs aux RR du scénario actuel
  rr_evo_diets <- rr_evo_diets %>% 
    group_by(scenario, year, simulation_id) %>% 
    mutate(relative_rr = combined_rr/combined_rr[scenario == "actuel"]) %>% 
    ungroup()
  
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_rr_diets <- rr_evo_diets %>%
    group_by(scenario, year) %>%
    summarise(
      mean_rr = mean(combined_rr, na.rm = TRUE),  # Moyenne des simulations
      lower_ci = quantile(combined_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(combined_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    )
  
  