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
  
# Population
  # Taux de mortalité (INSEE)
  MR <- import(here("data_clean", "MR_table.xlsx"))
  
  # Effectifs de population par age et par année (INSEE)
  population <- import(here("data_clean", "population_clean.xlsx"))
  
  
################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Nombre de simulations des valeurs de RR
  n <- 30
  
# Bornes temporelles des changements de régime alimentaire (années)
  year_i <- 2025 # Année initiale
  year_f <- 2050 # Année finale
  
# Borne inférieure de l'âge de la population du modèle (années)
  age_limit <- 18
  
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
  
#  Time to full effect
  # durée (années)
  ttfe_time <- 10
  
  # Durée du régime stationnaire 
    # Avant changement de régime : ttfe_time
    # Après changement de régime : 2 x ttfe_time
  
  # Dynamique (immediate, linear, cosine, sigmoidal, log)
  ttfe_dynamics <- "sigmoidal"
  
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
                    "sc1" = "aquamarine3",
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
  
# Sélectionner les MR entre les bornes temporelles du modèle et au dessus de la limite d'age
# Pivoter le dataframe en format long
  MR_select <- MR %>% 
    select(age, !!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time))) %>%
    filter(age >= age_limit) %>% 
    pivot_longer(cols = !!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)), 
                 names_to = "year", 
                 values_to = "MR") %>% 
    mutate(year = as.numeric(year))
  
# Sélectionner les effectifs de population entre les bornes temporelles du modèle et au dessus de la limite d'age 
# Pivoter le dataframe en format long
  population_select <- population %>% 
    select(age, !!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time))) %>% 
    filter(age >= age_limit) %>% 
    pivot_longer(cols = !!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)), 
                 names_to = "year", 
                 values_to = "population") %>% 
    mutate(year = as.numeric(year)) %>% 
    arrange(age)
  
  
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
  
# Ordonnner les groupes alimentaires
  diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)
  
# Visualisation graphique sur toute la période
  graph_diets_evo <- ggplot(data = diets_evo, aes(x = year,
                                                  y = quantity,
                                                  fill = food_group))+
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6)+
    facet_wrap(~ scenario, 
               ncol = 3,
               labeller = labeller(scenario = labels_scenario))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.8)),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          legend.title = element_text(face = "bold", size = 10),
          legend.key.size = unit(0.3, "cm"),
          plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))+
    scale_fill_manual(values = col_food_groups,
                      labels = labels_food_groups)+
    labs(title = "",
         x = "",
         y = "Quantities (g/day/pers)",
         fill = "Food type")+
    guides(fill = guide_legend(nrow = 3, 
                               title.position = "top",
                               title.hjust = 0.5))
  
diets_evo_shift <- diets_evo %>% 
    filter(year %in% c(year_i:year_f))

# Visualisation graphique sur la période de changement de régime
  graph_diets_evo <- ggplot(data = diets_evo_shift, aes(x = year,
                                                        y = quantity,
                                                        fill = food_group))+
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6)+
    facet_wrap(~ scenario, 
               ncol = 3,
               labeller = labeller(scenario = labels_scenario))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.8)),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          legend.title = element_text(face = "bold", size = 10),
          legend.key.size = unit(0.3, "cm"),
          plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))+
    scale_fill_manual(values = col_food_groups,
                      labels = labels_food_groups)+
    labs(title = "",
         x = "",
         y = "Quantities (g/day/pers)",
         fill = "Food type")+
    guides(fill = guide_legend(nrow = 3, 
                               title.position = "top",
                               title.hjust = 0.5))

  

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
  
  
# Exemple des consommations de 2035 dans S1
  
  diets_evo_sc1_2035 <- diets_evo %>% 
    filter(scenario == "sc1",
           year == 2035) %>% 
    select(food_group, rr_distrib) %>% 
    unnest(rr_distrib)
  
  graph_rr_sc1_2035_norm <- ggplot(diets_evo_sc1_2035, aes(x = rr_distrib,
                                                           y = after_stat(count / sum(count)),
                                                           fill = food_group)) + 
                              facet_wrap(~ food_group)+
                              geom_histogram(binwidth = 0.005, 
                                             position = "dodge",
                                             alpha = 0.7) +
                              scale_fill_manual(values = col_food_groups)+
                              theme(legend.position = "none",
                                    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                    axis.text.y = element_text(size = 7),
                                    strip.text = element_text(face = "bold",size = rel(0.5)))+
                              labs(title = "RR values",
                                   subtitle = "Food consumption in 2035 in S1",
                                   x = "RR",
                                   y = "Frequency")
  
  ssb_norm_sc1_2035 <- diets_evo_sc1_2035 %>% 
    filter(food_group == "sugar_sweetened_beverages")
  
  graph_ssb_sc1_2035_norm <- ggplot(ssb_norm_sc1_2035, aes(x = rr_distrib,
                                                           y = after_stat(count / sum(count)),
                                                           fill = food_group)) + 
                              geom_histogram(binwidth = 0.001, 
                                             position = "dodge",
                                             alpha = 0.7) +
                              scale_fill_manual(values = col_food_groups)+
                              theme(legend.position = "none",
                                    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                    axis.text.y = element_text(size = 7),
                                    strip.text = element_text(face = "bold",size = rel(0.5)))+
                              labs(title = "Sugar-sweetened beverages RR values",
                                   subtitle = "Food consumption in 2035 in S1",
                                   x = "RR",
                                   y = "Frequency")
  
  
################################################################################################################################
#                                             10. Simulations des valeurs de RR                                                #
################################################################################################################################
  
# # Fixer une graine pour garantir la reproductibilité des simulations
#   set.seed(123)

# Transformer les simulations en format long
  simulations_long <- diets_evo %>% 
    unnest_wider(rr_distrib, names_sep = "_") %>%  # Séparer les simulations en colonnes distinctes
    pivot_longer(
      cols = starts_with("rr_distrib_"),  # Sélectionner toutes les colonnes de simulations
      names_to = "simulation_id",  # Nom de la colonne contenant les identifiants de simulation
      values_to = "simulated_rr"  # Nom de la colonne contenant les valeurs simulées
    )

# Calculer la valeur centrales et les IC95 pour chaque année
  simulations_summary <- simulations_long %>%
    group_by(food_group, scenario, year, quantity) %>%
    summarise(
      mean_rr = mean(rr_mid, na.rm = TRUE),  # Moyenne des simulations
      lower_ci = quantile(simulated_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(simulated_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
            lower_ci > mean_rr ~ mean_rr,
            TRUE ~ lower_ci),
           upper_ci = case_when(
            upper_ci < mean_rr ~ mean_rr,
            TRUE ~ upper_ci))

################################################################################################################################
#                                             11. Représentations graphiques des simulations des valeurs de RR                 #
################################################################################################################################
  
    # S1
    simulations_summary_sc1 <- simulations_summary %>% 
      filter(scenario == "sc1")
    
    graph_rr_fg_sim_sc1 <- ggplot(simulations_summary_sc1, aes(x = year,
                                                               y = mean_rr,
                                                               color = food_group)) +
      facet_wrap(~ food_group)+
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5) +  # Intervalle de confiance
      geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
      labs(
        title = "RR simulations",
        x = "",
        y = "RR"
      )+
      scale_color_manual(values = col_food_groups)+
      scale_fill_manual(values = col_food_groups)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold",size = rel(0.5)),
            legend.position = "none")
    
    
    # S2
    simulations_summary_sc2 <- simulations_summary %>% 
      filter(scenario == "sc2")
    
    graph_rr_fg_sim_sc2 <- ggplot(simulations_summary_sc2, aes(x = year,
                                                               y = mean_rr,
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
            strip.text = element_text(face = "bold",size = rel(0.5)),
            legend.position = "none")
    
    
    # S3
    simulations_summary_sc3 <- simulations_summary %>% 
      filter(scenario == "sc3")
    
    graph_rr_fg_sim_sc3 <- ggplot(simulations_summary_sc3, aes(x = year,
                                                               y = mean_rr,
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
            strip.text = element_text(face = "bold",size = rel(0.5)),
            legend.position = "none")
    

    # S4
    simulations_summary_sc4 <- simulations_summary %>% 
      filter(scenario == "sc4")
    
    graph_rr_fg_sim_sc4 <- ggplot(simulations_summary_sc4, aes(x = year,
                                                               y = mean_rr,
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
            strip.text = element_text(face = "bold",size = rel(0.5)),
            legend.position = "none")
     
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
  
# Représentation graphique 
  graph_ttfe  <- ggplot(ttfe, aes(x = time,
                                  y = ttfe))+
    geom_line(color = "darkseagreen", size = 1, alpha = 0.8)+
    labs(title = "",
         x = "years",
         y = "% of RR value")  
  
    
################################################################################################################################
#                                             13. Calcul des RR avec TTFE                                                      #
################################################################################################################################
  
# Calcul de la valeur des RR sur la durée du time to full effect
# Après le time to full effect : RR = NA
    simulations_long <- simulations_long %>% 
      rowwise() %>% 
      mutate(year_n = list(seq(from = (year_i - ttfe_time), to = (year_f + 2*ttfe_time)))) %>% 
      unnest(year_n) %>% 
      mutate(simulated_rr_n = case_when(
                year_n < year ~ NA_real_,
                year_n >= year & year_n <= year + max(ttfe$time) ~ 1 + (simulated_rr - 1) * ttfe$ttfe[match(year_n - year, ttfe$time)],
                year_n > year + max(ttfe$time) ~ NA_real_),
              rr_mid_n = case_when(
                year_n < year ~ NA_real_,
                year_n >= year & year_n <= year + max(ttfe$time) ~ 1 + (rr_mid - 1) * ttfe$ttfe[match(year_n - year, ttfe$time)],
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
      combinaison_rr_type == "geometric mean"~ geometric.mean(simulated_rr_n, na.rm = TRUE)),
      mean_rr_mid = case_when(
        combinaison_rr_type == "arithmetic mean" ~ mean(rr_mid_n, na.rm = TRUE),
        combinaison_rr_type == "geometric mean"~ geometric.mean(rr_mid_n, na.rm = TRUE)
      ))
  
# Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_rr_fg_combined <- rr_evo_food_combined %>%
    group_by(food_group, scenario, year_n) %>%
    summarise(
      combined_rr = mean(mean_rr_mid, na.rm = TRUE),
      lower_ci = quantile(mean_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(mean_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > combined_rr ~ combined_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < combined_rr ~ combined_rr,
        TRUE ~ upper_ci))
  
################################################################################################################################
#                                             15. Représentations graphiques des simulations des valeurs de RR combinés        #
################################################################################################################################
  
  # S1
  simulations_summary_sc1_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc1")
  
  graph_rr_fg_combined_sim_sc1 <- ggplot(simulations_summary_sc1_c, aes(x = year_n,
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
          strip.text = element_text(face = "bold",size = rel(0.5)),
          legend.position = "none")
  
  
  # S2
  simulations_summary_sc2_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc2")
  
  graph_rr_fg_combined_sim_sc2 <- ggplot(simulations_summary_sc2_c, aes(x = year_n,
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
          strip.text = element_text(face = "bold",size = rel(0.5)),
          legend.position = "none")
  
  # S3
  simulations_summary_sc3_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc3")
  
  graph_rr_fg_combined_sim_sc3 <- ggplot(simulations_summary_sc3_c, aes(x = year_n,
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
          strip.text = element_text(face = "bold",size = rel(0.5)),
          legend.position = "none")
  
  
  # S4
  simulations_summary_sc4_c <- simulations_summary_rr_fg_combined %>% 
    filter(scenario == "sc4")
  
  graph_rr_fg_combined_sim_sc4 <- ggplot(simulations_summary_sc4_c, aes(x = year_n,
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
          strip.text = element_text(face = "bold",size = rel(0.5)),
          legend.position = "none")
  
  
################################################################################################################################
#                                             16. Combinaison des RR de chaque régime par année                                #
################################################################################################################################
  
  # Fonction produit des RR de chaque aliment par année
  calc_combined_rr <- function(df) {
    df %>%
      group_by(scenario, year_n, simulation_id) %>%
      summarize(combined_rr = prod(mean_rr, na.rm = TRUE),
                combined_rr_mid = prod(mean_rr_mid, na.rm = TRUE)) %>%
      ungroup()
  }  
  
  # Calcul des RR des régimes de chaque scénario par année
  rr_evo_diets <- calc_combined_rr(rr_evo_food_combined) %>% 
    rename("year" = "year_n")
  
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_rr_diets <- rr_evo_diets %>%
    group_by(scenario, year) %>%
    summarise(
      mean_rr = mean(combined_rr_mid, na.rm = TRUE),  
      lower_ci = quantile(combined_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(combined_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > mean_rr ~ mean_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < mean_rr ~ mean_rr,
        TRUE ~ upper_ci))
  
################################################################################################################################
#                                             17. Représentations graphiques des simulations des valeurs de RR des régimes     #
################################################################################################################################
  

graph_rr_diets_sim  <- ggplot(simulations_summary_rr_diets, aes(x = year,
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
                              strip.text = element_text(face = "bold",size = rel(0.5)),
                              legend.position = "none")
  
################################################################################################################################
#                                             18. Combinaison des RR de chaque régime par année et calcul relatif au RR actuel #
################################################################################################################################
  
# Calcul des RR relatifs aux RR du scénario actuel
  rr_evo_diets <- rr_evo_diets %>% 
    group_by(year, simulation_id) %>% 
    mutate(relative_rr = combined_rr/combined_rr[scenario == "actuel"],
           relative_rr_mid = combined_rr_mid/combined_rr_mid[scenario == "actuel"]) %>% 
    ungroup()
  
# Eliminer Les valeurs 5% les plus extrêmes
  rr_evo_diets <- rr_evo_diets %>% 
    group_by(scenario, year) %>% 
    filter(between(relative_rr, quantile(relative_rr, 0.025), quantile(relative_rr, 0.975)))
  
# Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_rr_diets_relative <- rr_evo_diets %>%
    group_by(scenario, year) %>%
    summarise(
      mean_rr = mean(relative_rr_mid, na.rm = TRUE),  
      lower_ci = quantile(relative_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(relative_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > mean_rr ~ mean_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < mean_rr ~ mean_rr,
        TRUE ~ upper_ci))
  

  
  
################################################################################################################################
#                                             19. Représentations graphiques des simulations des valeurs de RR relatives des régimes     #
################################################################################################################################
  
  
  graph_rr_diets_relative_sim <- ggplot(simulations_summary_rr_diets_relative %>% 
                                          filter(scenario != "actuel"),
                                        aes(x = year,
                                            y = mean_rr,
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
  
################################################################################################################################
#                                             20. Ajustement des taux de mortalité                                             #
################################################################################################################################
  
# Ajustement des taux de mortalité
  MR_adjusted <- MR_select %>% 
    inner_join(rr_evo_diets, by = "year", relationship = "many-to-many") %>%
    group_by(age, year, simulation_id) %>% 
    mutate(adjusted_mr = MR*relative_rr,
           adjusted_mr_mid = MR*relative_rr_mid) %>% 
    ungroup()
  
# Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_mr_adjusted <- MR_adjusted %>%
    group_by(age, scenario, year) %>%
    summarise(
      mean_rr = mean(adjusted_mr_mid, na.rm = TRUE),  
      lower_ci = quantile(adjusted_mr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(adjusted_mr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > mean_rr ~ mean_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < mean_rr ~ mean_rr,
        TRUE ~ upper_ci))
  
################################################################################################################################
#                                             21. Nombre de décès                                                              #
################################################################################################################################
  
# Décès dans chaque scénario 
  deaths <- MR_adjusted %>%
    left_join(population_select, by = c("age", "year"), relationship = "many-to-many") %>% 
    mutate(deaths = adjusted_mr*population,
           deaths_mid = adjusted_mr_mid*population)
  
# Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_deaths <- deaths %>%
    group_by(age, scenario, year) %>%
    summarise(
      mean_rr = mean(deaths_mid, na.rm = TRUE),  # Moyenne des simulations
      lower_ci = quantile(deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > mean_rr ~ mean_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < mean_rr ~ mean_rr,
        TRUE ~ upper_ci))
  
  deaths_wide <- deaths %>% 
    select("age", "year", "scenario", "simulation_id", "deaths") %>% 
    pivot_wider(names_from = "year", values_from = "deaths")
  
  deaths_mid_wide <- deaths %>% 
    select("age", "year", "scenario", "simulation_id", "deaths_mid") %>% 
    pivot_wider(names_from = "year", values_from = "deaths_mid")
    
  
# Nombre total de décès par année et par scénario
  total_deaths <- deaths_wide %>% 
    group_by(scenario, simulation_id) %>%                                 
    summarise(across(!!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)), sum)) %>%
    rowwise() %>%
    mutate(total_deaths = sum(c_across(!!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)))))  
  
  total_deaths_mid <- deaths_mid_wide %>% 
    group_by(scenario, simulation_id) %>%                                 
    summarise(across(!!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)), sum)) %>%
    rowwise() %>%
    mutate(total_deaths = sum(c_across(!!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)))))  
  
  total_deaths_long <- total_deaths %>% 
    select(-total_deaths) %>% 
    pivot_longer(cols = !!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)),
                 names_to = "year",
                 values_to = "total_deaths") %>% 
    mutate(year = as.numeric(year))
  
  total_deaths_mid_long <- total_deaths_mid %>% 
    select(-total_deaths) %>% 
    pivot_longer(cols = !!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)),
                 names_to = "year",
                 values_to = "total_deaths_mid") %>% 
    mutate(year = as.numeric(year))
  
  total_deaths_long <- total_deaths_long %>% 
    left_join(total_deaths_mid_long, by = c("scenario", "year", "simulation_id"))
  
################################################################################################################################
#                                             22. Nombre de décès évités                                                       #
################################################################################################################################
  
# Nombre total de décès évités par an
  total_avoided_deaths <- total_deaths_long %>% 
    group_by(year, simulation_id) %>% 
    mutate(avoided_deaths = total_deaths[scenario == "actuel"] - total_deaths,
           avoided_deaths_mid = total_deaths_mid[scenario == "actuel"] - total_deaths_mid)
  
# Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_avoided_deaths <- total_avoided_deaths %>%
    group_by(scenario, year) %>%
    summarise(
      mean_rr = mean(avoided_deaths_mid, na.rm = TRUE),  
      lower_ci = quantile(avoided_deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(avoided_deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > mean_rr ~ mean_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < mean_rr ~ mean_rr,
        TRUE ~ upper_ci))
  
# Résultats sur la période de changement de régime
  simulations_summary_avoided_deaths_shift <- simulations_summary_avoided_deaths %>% 
    filter(year %in% c(year_i:year_f))
  
# Nombre de décès évités par an et par age
  avoided_deaths <-  deaths %>% 
    group_by(age, year, simulation_id) %>% 
    mutate(avoided_deaths = deaths[scenario == "actuel"] - deaths,
           avoided_deaths_mid = deaths_mid[scenario == "actuel"] - deaths_mid)
  
# Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_avoided_deaths_age <- avoided_deaths %>%
    group_by(age, scenario, year) %>%
    summarise(
      mean_rr = mean(avoided_deaths_mid, na.rm = TRUE), 
      lower_ci = quantile(avoided_deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(avoided_deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > mean_rr ~ mean_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < mean_rr ~ mean_rr,
        TRUE ~ upper_ci))
  
# Nombre de décès évités par age, cumulés sur une période
  # année initiale du changement - 2035
  avoided_deaths_cum_2035 <- avoided_deaths %>% 
    filter(year >= year_i & year <= 2035) %>% 
    group_by(age, scenario, simulation_id) %>% 
    summarise(cum_avoided_deaths = sum(avoided_deaths),
              cum_avoided_deaths_mid = sum(avoided_deaths_mid))
  
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_avoided_deaths_cum_2035 <- avoided_deaths_cum_2035 %>%
    group_by(age, scenario) %>%
    summarise(
      mean_rr = mean(cum_avoided_deaths_mid, na.rm = TRUE),  
      lower_ci = quantile(cum_avoided_deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(cum_avoided_deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > mean_rr ~ mean_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < mean_rr ~ mean_rr,
        TRUE ~ upper_ci))
      
  
  # année initiale du changement - 2050
  avoided_deaths_cum_2050 <- avoided_deaths %>% 
    filter(year >= year_i & year <= 2050) %>% 
    group_by(age, scenario, simulation_id) %>% 
    summarise(cum_avoided_deaths = sum(avoided_deaths),
              cum_avoided_deaths_mid = sum(avoided_deaths_mid))
  
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_avoided_deaths_cum_2050 <- avoided_deaths_cum_2050 %>%
    group_by(age, scenario) %>%
    summarise(
      mean_rr = mean(cum_avoided_deaths_mid, na.rm = TRUE),  # Moyenne des simulations
      lower_ci = quantile(cum_avoided_deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(cum_avoided_deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) %>% 
    mutate(lower_ci = case_when(
      lower_ci > mean_rr ~ mean_rr,
      TRUE ~ lower_ci),
      upper_ci = case_when(
        upper_ci < mean_rr ~ mean_rr,
        TRUE ~ upper_ci))
  
################################################################################################################################
#                                             17. Report des décès d'une année sur l'autre                                     #
################################################################################################################################
  
  # deaths_report <- MR_adjusted %>%
  #   left_join(population_select, by = c("age", "year"), relationship = "many-to-many")
  #   
  # calc_deaths_report <- function(df) {
  #   
  #   # Calcul des décès
  #   df <- df %>%
  #     mutate(deaths = adjusted_mr * population,
  #            deaths_mid = adjusted_mr_mid * population)
  #   
  #   # Boucle sur chaque année sauf la dernière
  #   years <- unique(df$year)
  #   
  #   for (yr in years[years != max(years)]) {
  #     
  #     # Calcul des décès évités
  #     df <- df %>%
  #       group_by(age, year, simulation_id) %>%
  #       mutate(avoided_deaths = deaths[scenario == "actuel"] - deaths,
  #              avoided_deaths_mid = deaths_mid[scenario == "actuel"] - deaths_mid) %>%
  #       ungroup()
  #     
  #     # Ajout des décès évités à la population de l'année suivante et de l'âge suivant
  #     df <- df %>%
  #       group_by(scenario, simulation_id) %>%
  #       mutate(
  #         # Pour chaque ligne sauf les derniers âges et la dernière année
  #         population_next_year = if_else(year == yr & age < 105, 
  #                                        if_else(age == 18, 
  #                                                population + avoided_deaths,  # Utiliser la population actuelle et les décès évités
  #                                                lag(population) + lag(avoided_deaths, default = 0, order_by = age)), 
  #                                        population),
  #         population_next_year_mid = if_else(year == yr & age < 105, 
  #                                            if_else(age == 18, 
  #                                                    population + avoided_deaths_mid,  # Utiliser la population actuelle et les décès évités
  #                                                    lag(population) + lag(avoided_deaths_mid, default = 0, order_by = age)), 
  #                                            population),
  #         
  #         # Cas spécial pour la dernière catégorie d'âge (age = 105)
  #         population_next_year_105 = if_else(age == 105 & year == yr,
  #                                            population + avoided_deaths, 
  #                                            population_next_year),
  #         population_next_year_105_mid = if_else(age == 105 & year == yr,
  #                                                population + avoided_deaths_mid, 
  #                                                population_next_year_mid),
  #         
  #         # Calcul final de la population
  #         population_final = coalesce(population_next_year_105, population_next_year),
  #         population_final_mid = coalesce(population_next_year_105_mid, population_next_year_mid)
  #       ) %>%
  #       ungroup()
  #     
  #     # Vérification des mises à jour de population
  #     print(paste("Année:", yr))
  #     print(head(df %>% 
  #                  filter(year == yr,
  #                         scenario== "sc1",
  #                         simulation_id == "simulations_90",
  #                         age %in% c(18,19)) %>% 
  #                  select(age, population, avoided_deaths, population_final)))
  #   }
  #   
  #   # Supprimer les colonnes intermédiaires inutiles
  #   df <- df %>%
  #     select(-population_next_year, -population_next_year_mid, 
  #            -population_next_year_105, -population_next_year_105_mid)
  #   
  #   return(df)
  # }
  # 
  # 
  # deaths_report_inter <- calc_deaths_report(deaths_report)
  #   
  # deaths_report_inter <- deaths_report_inter %>% 
  #   select(age, year, scenario, simulation_id, population, deaths, avoided_deaths, population_final, deaths_mid, avoided_deaths_mid, population_final_mid)
  
  
################################################################################################################################
#                                             17. Représentations graphiques des simulations des décès évités par année        #
################################################################################################################################
  
# Sur toute la durée du modèle
  graph_total_avoided_deaths  <- ggplot(simulations_summary_avoided_deaths %>% 
                                          filter(scenario != "actuel"),
                                        aes(x = year,
                                            y = mean_rr,
                                            color = scenario)) +
                                        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5)+
                                        facet_wrap(~ scenario)+
                                        geom_line(size = 1, na.rm = TRUE)+ 
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
  
# Sur la période de changement de régime
  graph_total_avoided_deaths_shift <- ggplot(simulations_summary_avoided_deaths_shift %>% 
                                               filter(scenario != "actuel"),
                                             aes(x = year,
                                                 y = mean_rr,
                                                 color = scenario)) +
                                              geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5)+
                                              facet_wrap(~ scenario)+
                                              geom_line(size = 1, na.rm = TRUE)+ 
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
#                                             18. Représentations graphiques des simulations des décès évités par age          #
################################################################################################################################
  
# 2035
  simulations_summary_deaths_2035 <- simulations_summary_avoided_deaths_age %>% 
    filter(year == 2035)
  
  graph_avoided_deaths_2035_facet <- ggplot(simulations_summary_deaths_2035 %>% 
                                              filter(scenario != "actuel"),
                                            aes(x = age,
                                                y = mean_rr,
                                                color = scenario)) +
                                            facet_wrap(~ scenario)+
                                            geom_line(size = 0.8, na.rm = TRUE)+
                                            geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), 
                                                        alpha = 0.5,
                                                        size = 0.3,
                                                        linetype = "dashed")+
                                            labs(
                                              title = "Avoided deaths in 2035 compared to keeping the current diet",
                                              x = "Age",
                                              y = "Number of avoided deaths"
                                            )+
                                            scale_color_manual(values = col_scenario)+
                                            scale_fill_manual(values = col_scenario)+
                                            theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                                  axis.text.y = element_text(size = 7),
                                                  strip.text = element_text(face = "bold",size = rel(1)),
                                                  legend.position = "none")
        
  graph_avoided_deaths_2035 <- ggplot(simulations_summary_deaths_2035 %>% 
                                        filter(scenario != "actuel"),
                                      aes(x = age,
                                          y = mean_rr,
                                          group = scenario,
                                          color = scenario)) +
                                      geom_line(size = 1, na.rm = TRUE)+
                                      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), 
                                                  alpha = 0.1,
                                                  size = 0.3,
                                                  linetype = "dashed")+
                                      labs(
                                        title = "Avoided deaths in 2035",
                                        x = "Age",
                                        y = "Number of avoided deaths"
                                      )+
                                      scale_color_manual(values = col_scenario)+
                                      scale_fill_manual(values = col_scenario)+
                                      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                            axis.text.y = element_text(size = 7))
  
# 2050
  simulations_summary_deaths_2050 <- simulations_summary_avoided_deaths_age %>% 
    filter(year == 2050)
  
  graph_avoided_deaths_2050_facet <- ggplot(simulations_summary_deaths_2050 %>% 
                                              filter(scenario != "actuel"),
                                            aes(x = age,
                                                y = mean_rr,
                                                color = scenario)) +
                                            facet_wrap(~ scenario)+
                                            geom_line(size = 0.8, na.rm = TRUE)+
                                            geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), 
                                                        alpha = 0.5,
                                                        size = 0.3,
                                                        linetype = "dashed")+
                                            labs(
                                              title = "Avoided deaths in 2050",
                                              x = "Age",
                                              y = "Number of avoided deaths"
                                            )+
                                            scale_color_manual(values = col_scenario)+
                                            scale_fill_manual(values = col_scenario)+
                                            theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                                  axis.text.y = element_text(size = 7),
                                                  strip.text = element_text(face = "bold",size = rel(1)),
                                                  legend.position = "none")
  
  graph_avoided_deaths_2050 <- ggplot(simulations_summary_deaths_2050 %>% 
                                        filter(scenario != "actuel"),
                                      aes(x = age,
                                          y = mean_rr,
                                          group = scenario,
                                          color = scenario)) +
                                geom_line(size = 1, na.rm = TRUE)+
                                geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), 
                                            alpha = 0.1,
                                            size = 0.3,
                                            linetype = "dashed")+
                                labs(
                                  title = "Avoided deaths in 2050",
                                  x = "Age",
                                  y = "Number of avoided deaths"
                                )+
                                scale_color_manual(values = col_scenario)+
                                scale_fill_manual(values = col_scenario)+
                                theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                      axis.text.y = element_text(size = 7))
  
################################################################################################################################
#                                             19. Représentations graphiques des simulations des décès évités cumulés par age  #
################################################################################################################################
  
# Année initiale du changement de régime - 2035
  graph_avoided_deaths_cum_2035_facet <- ggplot(simulations_summary_avoided_deaths_cum_2035 %>% 
                                                  filter(scenario != "actuel"),
                                                aes(x = age,
                                                    y = mean_rr,
                                                    color = scenario)) +
                                          facet_wrap(~ scenario)+
                                          geom_line(size = 0.8, na.rm = TRUE)+
                                          geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), 
                                                      alpha = 0.5,
                                                      size = 0.3,
                                                      linetype = "dashed")+
                                          labs(
                                            title = "Cumulated avoided deaths 2025-2035",
                                            x = "Age",
                                            y = "Number of avoided deaths"
                                          )+
                                          scale_color_manual(values = col_scenario)+
                                          scale_fill_manual(values = col_scenario)+
                                          theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                                axis.text.y = element_text(size = 7),
                                                strip.text = element_text(face = "bold",size = rel(1)),
                                                legend.position = "none")
                                  
  graph_avoided_deaths_cum_2035 <- ggplot(simulations_summary_avoided_deaths_cum_2035 %>% 
                                            filter(scenario != "actuel"),
                                          aes(x = age,
                                          y = mean_rr,
                                          group = scenario,
                                          color = scenario)) +
                                    geom_line(size = 1, na.rm = TRUE)+
                                    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), 
                                                alpha = 0.1,
                                                size = 0.3,
                                                linetype = "dashed")+
                                    labs(
                                      title = "Cumulated avoided deaths 2025-2035",
                                      x = "Age",
                                      y = "Number of avoided deaths"
                                    )+
                                    scale_color_manual(values = col_scenario)+
                                    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                          axis.text.y = element_text(size = 7))
                                  
# Année initiale du changement de régime - 2050
  graph_avoided_deaths_cum_2050_facet <- ggplot(simulations_summary_avoided_deaths_cum_2050 %>% 
                                                  filter(scenario != "actuel"),
                                                aes(x = age,
                                                    y = mean_rr,
                                                    color = scenario)) +
                                                facet_wrap(~ scenario)+
                                                geom_line(size = 0.8, na.rm = TRUE)+
                                                geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), 
                                                            alpha = 0.5,
                                                            size = 0.3,
                                                            linetype = "dashed")+
                                                labs(
                                                  title = "Cumulated avoided deaths 2025-2050",
                                                  x = "Age",
                                                  y = "Number of avoided deaths"
                                                )+
                                                scale_color_manual(values = col_scenario)+
                                                scale_fill_manual(values = col_scenario)+
                                                theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                                      axis.text.y = element_text(size = 7),
                                                      strip.text = element_text(face = "bold",size = rel(1)),
                                                      legend.position = "none")
  
  graph_avoided_deaths_cum_2050 <- ggplot(simulations_summary_avoided_deaths_cum_2050 %>% 
                                            filter(scenario != "actuel"),
                                          aes(x = age,
                                              y = mean_rr,
                                              group = scenario,
                                              color = scenario)) +
                                          geom_line(size = 1, na.rm = TRUE)+
                                          geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), 
                                                      alpha = 0.1,
                                                      size = 0.3,
                                                      linetype = "dashed")+
                                          labs(
                                            title = "Cumulated avoided deaths 2025-2050",
                                            x = "Age",
                                            y = "Number of avoided deaths"
                                          )+
                                          scale_color_manual(values = col_scenario)+
                                          scale_fill_manual(values = col_scenario)+
                                          theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                                axis.text.y = element_text(size = 7))
  
################################################################################################################################
#                                             20. Exportation des données                                                      #
################################################################################################################################
  
# Régimes, valeurs des RR et IC95 RR, distribution normale des valeurs de RR et simulations
  export(diets_evo, here("results", "visualization_tool_ic95", "diets_rr_evo.xlsx"))
  
  ggsave(here("results", "visualization_tool_ic95", "diets_evo.pdf"), plot = graph_diets_evo)
  
# Distribution normale des RR de chaque aliment pour chaque année dans chaque scénario
  ggsave(here("results", "visualization_tool_ic95","norm_rr_sc1_2035.pdf"), plot = graph_rr_sc1_2035_norm)
  ggsave(here("results", "visualization_tool_ic95","norm_rr_ssb_sc1_2035.pdf"), plot = graph_ssb_sc1_2035_norm)
  
# Graphiques des valeurs de RR simulées par aliment, dans chaque scénario
  ggsave(here("results", "visualization_tool_ic95", "rr_fg_sim_sc1.pdf"), plot = graph_rr_fg_sim_sc1)
  ggsave(here("results", "visualization_tool_ic95", "rr_fg_sim_sc2.pdf"), plot = graph_rr_fg_sim_sc2)  
  ggsave(here("results", "visualization_tool_ic95", "rr_fg_sim_sc3.pdf"), plot = graph_rr_fg_sim_sc3)  
  ggsave(here("results", "visualization_tool_ic95", "rr_fg_sim_sc4.pdf"), plot = graph_rr_fg_sim_sc4)  
  
# Time to full effect
  ggsave(here("results", "visualization_tool_ic95", "ttfe.pdf"), plot = graph_ttfe)

# Valeurs des RR avec TTFE, pour chaque aliment, année, scénario et simulation
  #export(simulations_long, here("results", "visualization_tool_ic95", "rr_fg_ttfe_sim.csv"))
  # FICHIER TROP LOURD
  
# Valeurs des RR de chaque aliment, combinés par année
  export(rr_evo_food_combined, here("results", "visualization_tool_ic95", "rr_fg_evo_combined.xlsx"))
  export(simulations_summary_rr_fg_combined, here("results", "visualization_tool_ic95", "IC95_rr_fg_evo_combined.xlsx"))
  
  ggsave(here("results", "visualization_tool_ic95", "rr_fg_combined_sim_sc1.pdf"), plot = graph_rr_fg_combined_sim_sc1)
  ggsave(here("results", "visualization_tool_ic95", "rr_fg_combined_sim_sc2.pdf"), plot = graph_rr_fg_combined_sim_sc2)  
  ggsave(here("results", "visualization_tool_ic95", "rr_fg_combined_sim_sc3.pdf"), plot = graph_rr_fg_combined_sim_sc3)  
  ggsave(here("results", "visualization_tool_ic95", "rr_fg_combined_sim_sc4.pdf"), plot = graph_rr_fg_combined_sim_sc4)  
  
# Valeurs des RR des régimes par année
  export(rr_evo_diets, here("results", "visualization_tool_ic95", "rr_evo_diets.xlsx"))
  export(simulations_summary_rr_diets, here("results", "visualization_tool_ic95", "IC95_rr_evo_diets.xlsx"))
  
  ggsave(here("results", "visualization_tool_ic95", "rr_diets_sim.pdf"), plot = graph_rr_diets_sim)
  
# Valeurs des RR des régimes, relatifs au scénario actuel
  export(simulations_summary_rr_diets_relative, here("results", "visualization_tool_ic95", "IC95_rr_evo_diets_relative.xlsx"))
  
  ggsave(here("results", "visualization_tool_ic95", "rr_diets_relative_sim.pdf"), plot = graph_rr_diets_relative_sim)
  
# Taux de mortalité ajustés
  #export(MR_adjusted, here("results", "visualization_tool_ic95", "MR_adjusted.xlsx"))
  # FICHIER TROP LOURD
  export(simulations_summary_mr_adjusted, here("results", "visualization_tool_ic95", "IC95_MR_adjsuted.xlsx"))

# Nombre total de décès évités par annnée
  # Sur toute la période du modèle
  export(simulations_summary_avoided_deaths, here("results", "visualization_tool_ic95", "IC95_total_avoided_deaths.xlsx"))
  ggsave(here("results", "visualization_tool_ic95", "total_avoided_deaths.pdf"), plot = graph_total_avoided_deaths)
  
  # Sur la période de changement de régime
  export(simulations_summary_avoided_deaths_shift, here("results", "visualization_tool_ic95", "IC95_total_avoided_deaths_shift.xlsx"))
  ggsave(here("results", "visualization_tool_ic95", "total_avoided_deaths_shift.pdf"), plot = graph_total_avoided_deaths_shift)
  
# Nombre de décès évités par an et par age
  export(simulations_summary_avoided_deaths_age, here("results", "visualization_tool_ic95", "IC95_avoided_deaths.xlsx"))
  
  # 2035
  ggsave(here("results", "visualization_tool_ic95", "avoided_deaths_2035.pdf"), plot = graph_avoided_deaths_2035)
  ggsave(here("results", "visualization_tool_ic95", "avoided_deaths_2035_facet.pdf"), plot = graph_avoided_deaths_2035_facet)
  
  # 2050
  ggsave(here("results", "visualization_tool_ic95", "avoided_deaths_2050.pdf"), plot = graph_avoided_deaths_2050)
  ggsave(here("results", "visualization_tool_ic95", "avoided_deaths_2050_facet.pdf"), plot = graph_avoided_deaths_2050_facet)
  
# Nombre de décès évités par age, cumulés sur une période
  # début du changement - 2035
  export(simulations_summary_avoided_deaths_cum_2035, here("results", "visualization_tool_ic95", "IC95_avoided_deaths_cum_2035.xlsx"))
  ggsave(here("results", "visualization_tool_ic95", "avoided_deaths_cum_2035.pdf"), plot = graph_avoided_deaths_cum_2035)
  ggsave(here("results", "visualization_tool_ic95", "avoided_deaths_cum_2035_facet.pdf"), plot = graph_avoided_deaths_cum_2035_facet)
  
  # début du changement - 2050
  export(simulations_summary_avoided_deaths_cum_2050, here("results", "visualization_tool_ic95", "IC95_avoided_deaths_cum_2050.xlsx"))
  ggsave(here("results", "visualization_tool_ic95", "avoided_deaths_cum_2050.pdf"), plot = graph_avoided_deaths_cum_2050)
  ggsave(here("results", "visualization_tool_ic95", "avoided_deaths_cum_2050_facet.pdf"), plot = graph_avoided_deaths_cum_2050_facet)
  
  