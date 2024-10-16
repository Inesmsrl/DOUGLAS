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

# Population
  # Taux de mortalité (INSEE)
  MR <- import(here("data_clean", "MR_table.xlsx"))
  
  # Effectifs de population par age et par année (INSEE)
  population <- import(here("data_clean", "population_clean.xlsx"))

# Expositions : régimes actuel et 2050 (SISAE)
  diets <- import(here("data", "DOUGLAS_diets.xlsx"))
  
# Risques relatifs / consommation (g/j) (Fadnes, 2022 & 2024)
  rr_table <- import(here("data_clean", "rr_table_interpolated.xlsx"))
  
################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################
  
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
  
  
################################################################################################################################
#                                             4. Charte graphique                                                              #
################################################################################################################################
  
# Couleur de chaque scénario
  col_scenario <- c("actuel" = "azure4",
                    "sc0" = "palevioletred3",
                    "sc1" = "aquamarine2",
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
#                                             5. Préparation des données                                                       #
################################################################################################################################
  
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
  
# Pivoter le dataframe des RR en format long
  rr_table <- rr_table %>% 
    pivot_longer(cols = "0":"800",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))
  
################################################################################################################################
#                                             6. Fonctions d'implémentation des régimes                                        #
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
#                                             7. Evolution des régimes                                                         #
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
 
# Visualisation graphique sur toute la période de changement de régimes
 diets_evo_shift <- diets_evo %>% 
   filter(year %in% c(year_i:year_f))
 
 graph_diets_evo_shift <- ggplot(data = diets_evo_shift, aes(x = year,
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
#                                             8. Modification d'effet des RR                                                         #
################################################################################################################################
 rr_table <- rr_table %>% 
   mutate(rr_a = case_when(
     rr < 1 ~ rr + (1 - rr) * (1 - m),
     rr >= 1 ~ 1 / (m/rr + 1 - m)
   )) %>% 
   select("food_group", "quantity", "rr_a") %>% 
   rename("rr" = "rr_a")
 
################################################################################################################################
#                                             9. Attribution des RR à chaque régime                                            #
################################################################################################################################
  
  diets_evo <- diets_evo %>% 
    mutate(quantity = round(quantity)) %>% 
    left_join(rr_table, by = c("food_group", "quantity"))
  
################################################################################################################################
#                                             10. Time to full effect                                                           #
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
                  labs(title = "Time to full effect",
                       x = "",
                       y = "")  
  
################################################################################################################################
#                                             11. Calcul des RR avec TTFE                                                       #
################################################################################################################################
  
# Calcul de la valeur des RR sur la durée du time to full effect
# Après le time to full effect : RR = NA
  diets_evo <- diets_evo %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = (year_i - ttfe_time), to = (year_f + 2*ttfe_time)))) %>% 
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe$time) ~ 1 + (rr - 1) * ttfe$ttfe[match(year_n - year, ttfe$time)],
      year_n > year + max(ttfe$time) ~ NA_real_
    )) %>% 
    ungroup()

################################################################################################################################
#                                             12. Exemple : RR du régime de 2025 dans S1                                       #
################################################################################################################################
  
# Sélection du régime de S1 en 2025 et les valeurs des RR associées, jusqu'en 2050
  rr_sc1 <- diets_evo %>% 
    filter(scenario == "sc1",
           year == 2025,
           year_n %in% c(2025:2050)) %>% 
    select(food_group, year_n, rr_n) %>% 
    rename("year" = "year_n",
           "rr" = "rr_n")

# Ordonner les groupes d'aliments
  rr_sc1$food_group <- factor(rr_sc1$food_group, levels = order_food_groups)

# Visualisation graphique
  graph_rr_evo_sc1_2025  <- ggplot(rr_sc1, aes(x = year,
                                               y = rr,
                                               color = as.factor(food_group)))+
                                geom_line(size = 1, alpha = 0.8, na.rm = TRUE)+
                                scale_color_manual(values = col_food_groups,
                                                   labels = labels_food_groups)+
                                labs(title = "RR associated with 2025 changes in diet in S1",
                                     x = "",
                                     y = "RR",
                                     color = "Food Group")+
                                theme(axis.text.x = element_text(angle = 45, hjust = 1))  

################################################################################################################################
#                                             13. Combinaison des RR de chaque aliment par année                               #
################################################################################################################################
  
# Calcul des RR de chaque aliment pour chaque année
  rr_evo_food_combined <- diets_evo %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = case_when(
      combinaison_rr_type == "arithmetic mean" ~ mean(rr_n, na.rm = TRUE),
      combinaison_rr_type == "geometric mean"~ geometric.mean(rr_n, na.rm = TRUE))
    )
  
# Ordonner les groupes d'aliments
  rr_evo_food_combined$food_group <- factor(rr_evo_food_combined$food_group, levels = order_food_groups)
  
  
# Visualisation graphique
  graph_rr_evo_fg <-  ggplot(data = rr_evo_food_combined, aes(x = year_n,
                                                              y = mean_rr,
                                                              color = as.factor(food_group)))+
                        geom_line(size = 1, alpha = 0.8, na.rm = TRUE)+
                        scale_color_manual(values = col_food_groups,
                                           labels = labels_food_groups)+
                        facet_wrap(~ scenario, 
                                   ncol = 4,
                                    labeller = labeller(scenario = labels_scenario))+
                        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                              axis.text.y = element_text(size = 7),
                              strip.text = element_text(face = "bold",size = rel(0.8)),
                              legend.position = "bottom",
                              legend.text = element_text(size = 6),
                              legend.title = element_text(face = "bold", size = 7),
                              legend.key.size = unit(0.2, "cm"),
                              plot.margin = margin(1, 1, 1, 1, "cm"))+
                        labs(title = "Evolution of RR of each food group",
                             x = "",
                             y = "RR",
                             color = "Food group")+
                        guides(fill = guide_legend(nrow = 2, 
                                                   title.position = "top",
                                                   title.hjust = 0.5))
    
################################################################################################################################
#                                             14. Combinaison des RR de chaque régime par année                                #
################################################################################################################################
  
# Fonction produit des RR de chaque aliment par année
  calc_combined_rr <- function(df) {
    df %>%
      group_by(scenario, year_n) %>%
      summarize(combined_rr = prod(mean_rr, na.rm = TRUE)) %>%
      ungroup()
  }  
  
# Calcul des RR des régimes de chaque scénario par année
  rr_evo_diets <- calc_combined_rr(rr_evo_food_combined) %>% 
    rename("year" = "year_n")
  
  
# Visualisation graphique
  graph_rr_evo_diets <- ggplot(rr_evo_diets, aes(x = year,
                                                 y = combined_rr,
                                                 color = scenario))+
                          geom_line(size = 1)+
                          scale_color_manual(values = col_scenario)+
                          labs(title = "Whole diet RR evolution in each scenario",
                               x = "",
                               y = "RR") +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
  
# Valeur des RR des régimes relativement aux RR du régime actuel
  rr_evo_diets <- rr_evo_diets %>% 
    group_by("scenario", "year") %>% 
    mutate(relative_rr = combined_rr/combined_rr[scenario == "actuel"]) %>% 
    ungroup() %>% 
    select(scenario, year, combined_rr, relative_rr)
  
  
# Visualisation graphique
  graph_rr_evo_diets_relative <- ggplot(rr_evo_diets %>% 
                                          filter(scenario != "actuel"),
                                        aes(x = year,
                                            y = relative_rr,
                                            color = scenario))+
                                    geom_line(size = 1)+
                                    scale_color_manual(values = col_scenario)+
                                    labs(title = "Whole diet RR evolution in each scenario",
                                         x = "",
                                         y = "RR") +
                                    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
# RR de chaque scénario, sur la période de changement de régime
  rr_evo_diets_shift <-  rr_evo_diets %>% 
    filter(year %in% c(year_i:year_f))
  
# Visualisation graphique de la valeur absolue des RR de chaque régime
  graph_rr_evo_diets_shift <- ggplot(rr_evo_diets_shift, aes(x = year,
                                                             y = combined_rr,
                                                             color = scenario))+
                                geom_line(size = 1)+
                                scale_color_manual(values = col_scenario)+
                                labs(title = "Whole diet RR evolution in each scenario",
                                     x = "",
                                     y = "RR") +
                                theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
# Visualisation graphique des RR des régimes relativement aux RR du régime actuel
  graph_rr_evo_diets_relative_shift <- ggplot(rr_evo_diets_shift %>% 
                                                filter(scenario != "actuel"),
                                              aes(x = year,
                                                  y = relative_rr,
                                                  color = scenario))+
                                  geom_line(size = 1)+
                                  scale_color_manual(values = col_scenario)+
                                  labs(title = "Whole diet RR evolution in each scenario",
                                       x = "",
                                       y = "RR") +
                                  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
    

################################################################################################################################
#                                             15. Evaluation d'impact sanitaire par rapport au régime Actuel                   #
################################################################################################################################
  
# Ajustement des taux de mortalité
  MR_adjusted <- MR_select %>% 
    inner_join(rr_evo_diets, by = "year", relationship = "many-to-many") %>%
    group_by(age, year) %>% 
    mutate(adjusted_mr = MR*relative_rr) %>% 
    ungroup()
  
# Décès dans chaque scénario 
  deaths <- MR_adjusted %>%
    left_join(population_select, by = c("age", "year")) %>%
    mutate(deaths = adjusted_mr*population)
  
  deaths_wide <- deaths %>% 
    select("age", "year", "scenario", "deaths") %>% 
    pivot_wider(names_from = "year", values_from = "deaths")

# Nombre total de décès par année et par scénario
  total_deaths <- deaths_wide %>% 
    group_by(scenario) %>%                                 
    summarise(across(!!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)), sum)) %>%
    rowwise() %>%
    mutate(total_deaths = sum(c_across(!!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)))))  
  
  # Sur la période de changement de régime
  total_deaths_shift <- deaths_wide %>% 
    group_by(scenario) %>%                                 
    summarise(across(!!sym(as.character(year_i)) : !!sym(as.character(year_f)), sum)) %>%
    rowwise() %>%
    mutate(total_deaths = sum(c_across(!!sym(as.character(year_i)) : !!sym(as.character(year_f)))))    
  
  
# Nombre total de décès évités
  
  # Extraire les décès par année et totaux du scénario actuel
  total_deaths_actuel <- total_deaths %>% 
    filter(scenario == "actuel")
  
  total_avoided_deaths <- total_deaths %>% 
    filter(scenario %in% c("sc0", "sc1", "sc2", "sc3", "sc4", "sc5")) %>% 
    mutate(across(-"scenario",
                  ~ total_deaths_actuel[[cur_column()]] - .)) %>% 
    rename("avoided_deaths" = "total_deaths")
  
  # Format long pour représentation graphique
  total_avoided_deaths_long <-  total_avoided_deaths %>% 
    select(-avoided_deaths) %>% 
    pivot_longer(cols = !!sym(as.character(year_i - ttfe_time)) : !!sym(as.character(year_f + 2*ttfe_time)),
                 names_to = "year",
                 values_to = "avoided_deaths") %>% 
    mutate(year = as.numeric(year))
  
  # Sur la période de changement de régime
  total_deaths_actuel_shift <- total_deaths_shift %>% 
    filter(scenario == "actuel")
  
  total_avoided_deaths_shift <- total_deaths_shift %>% 
    filter(scenario %in% c("sc0", "sc1", "sc2", "sc3", "sc4", "sc5")) %>% 
    mutate(across(-"scenario",
                  ~ total_deaths_actuel[[cur_column()]] - .)) %>% 
    rename("avoided_deaths" = "total_deaths")
  
  total_avoided_deaths_shift_long <-  total_avoided_deaths_shift %>% 
    select(-avoided_deaths) %>% 
    pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)),
                 names_to = "year",
                 values_to = "avoided_deaths") %>% 
    mutate(year = as.numeric(year))
  
  
# Nombre de décès évités par age et par année
  
  # Filtrer les décès du scénario actuel
  deaths_actuel <- deaths_wide %>% 
    filter(scenario == "actuel") %>%
    select(-scenario) %>% 
    rename_with(~ paste0("actuel_", .), -age) # Renommer les colonnes pour éviter les conflits lors de la jointure
  
  # Joindre les données par âge et calculer les différences
  avoided_deaths <- deaths_wide %>%
    filter(scenario %in% c("sc0", "sc1", "sc2", "sc3", "sc4", "sc5")) %>%
    left_join(deaths_actuel, by = "age") %>%
    mutate(across(!!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
                  ~ get(paste0("actuel_", cur_column())) - .)) %>% 
    select("age", "scenario", !!sym(as.character(year_i)) : !!sym(as.character(year_f)))

  # Transformer en format long pour les graphs
  avoided_deaths_long <- avoided_deaths %>% 
    pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)),
                 names_to = "year",
                 values_to = "avoided_deaths") %>% 
    mutate(year = as.numeric(year))
  
# Décès évités par age et par scénario en 2035
  avoided_deaths_2035 <- avoided_deaths %>% 
    select(age, scenario, "2035") %>% 
    rename("avoided_deaths" = "2035")
  
# Décès évités par age et par scénario en 2050
  avoided_deaths_2050 <- avoided_deaths %>% 
    select(age, scenario, "2050") %>% 
    rename("avoided_deaths" = "2050")
  
  
################################################################################################################################
#                                             16. Graphiques : Evaluation d'impact sanitaire par rapport au régime Actuel      #
################################################################################################################################
  
# Nombre de décès total par scénario
  graph_total_deaths  <- ggplot(data = total_deaths, aes(x = scenario, 
                                                         y = total_deaths, 
                                                         fill = scenario))+
                                geom_bar(stat = "identity", alpha = 0.8)+
                                labs(title =  "Total number of deaths in each scenario",
                                     x = "Scenario", 
                                     y = "Number of deaths")+
                                scale_fill_manual(values = col_scenario)+
                                theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nombre total de décès évités par scénario
  graph_total_avoided_deaths <- ggplot(data = total_avoided_deaths, aes(x = scenario, 
                                                                        y = avoided_deaths, 
                                                                        fill = scenario))+
                                    geom_bar(stat = "identity", alpha = 0.8)+
                                    labs(title = "Total avoided deaths compared to keeping the current diet",
                                         x = "",
                                         y = "number of deaths avoided")+
                                    scale_fill_manual(values = col_scenario)+
                                    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Nombre total de décès évités par an dans chaque scénario
  graph_total_avoided_deaths_yearly <- ggplot(total_avoided_deaths_long, aes(x = year,
                                                                             y = avoided_deaths,
                                                                             color = scenario))+
                                          geom_line(size = 1)+
                                          scale_color_manual(values = col_scenario)+
                                          labs(title = "Total avoided deaths compared to keeping the current diet",
                                               x = "",
                                               y = "Avoided deaths") +
                                          theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
  graph_total_avoided_deaths_yearly_shift <- ggplot(total_avoided_deaths_shift_long, aes(x = year,
                                                                                         y = avoided_deaths,
                                                                                         color = scenario))+
                                                geom_line(size = 1)+
                                                scale_color_manual(values = col_scenario)+
                                                labs(title = "Total avoided deaths compared to keeping the current diet",
                                                     x = "",
                                                     y = "Avoided deaths") +
                                                theme(axis.text.x = element_text(angle = 45, hjust = 1))  
                                              
  
                                
# Nombre de décès évités par âge et par année, dans chaque scénario
  heat_map_avoided_deaths  <- ggplot(avoided_deaths_long, aes(x = year,
                                                              y = age,
                                                              fill = avoided_deaths)) +
                                geom_tile()+
                                facet_wrap(~ scenario, 
                                           scales = "fixed",
                                           labeller = labeller(scenario = labels_scenario)) +
                                scale_fill_gradient2(low = "dodgerblue4", mid = "grey80", high = "firebrick", limits = c(0,12000)) +
                                labs(title = "Avoided deaths compared to keeping the current diet",
                                     x = "",
                                     y = "Age",
                                     fill = "Avoided deaths") +
                                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                      strip.text = element_text(face = "bold",size = rel(0.8)))

# Nombre de décès évités par âge et par scénario en 2035
 graph_avoided_deaths_2035 <- ggplot(avoided_deaths_2035, aes(x = age,
                                                              y = avoided_deaths))+
                                facet_wrap(~ scenario, 
                                           scales = "fixed",
                                           labeller = labeller(scenario = labels_scenario))+
                                geom_bar(stat = "identity" ,
                                         fill = "maroon")+
                                labs(title = "Avoided deaths in 2035 compared to keeping the current diet",
                                     x = "Age",
                                     y = "Avoided deaths")
                              
# Nombre de décès évités par âge et par scénario en 2050
 graph_avoided_deaths_2050 <- ggplot(avoided_deaths_2050, aes(x = age,
                                                              y = avoided_deaths))+
                                facet_wrap(~ scenario, 
                                           scales = "fixed",
                                           labeller = labeller(scenario = labels_scenario))+
                                geom_bar(stat = "identity" ,
                                         fill = "darkseagreen4")+
                                labs(title = "Avoided deaths in 2050 compared to keeping the current diet",
                                     x = "Age",
                                     y = "Avoided deaths")
                              
################################################################################################################################
#                                             17. Exportation des données                                                      #
################################################################################################################################
  
# Régimes, valeurs des RR 100% et RR par année
  export(diets_evo, here("results", "visualization_tool", "diets_rr_evo.xlsx"))
  
# Evolution des régimes dans le temps
  ggsave(here("results", "visualization_tool", "diets_evo.pdf"), plot = graph_diets_evo)
  ggsave(here("results", "visualization_tool", "diets_evo_shift.pdf"), plot = graph_diets_evo_shift)

# Time to full effect
  ggsave(here("results", "visualization_tool", "ttfe.pdf"), plot = graph_ttfe)
  
# Exemple : Evolution des RR du régime de S1 en 2025
  ggsave(here("results", "visualization_tool", "rr_evo_sc1_2025.pdf"), plot = graph_rr_evo_sc1_2025)
  
# Evolution des RR de chaque groupe alimentaire 
  export(rr_evo_food_combined, here("results", "visualization_tool","rr_evo_fg.xlsx"))
  ggsave(here("results", "visualization_tool", "rr_evo_fg.pdf"), plot = graph_rr_evo_fg)

# Evolution des RR de chaque régime complet
  export(rr_evo_diets, here("results", "visualization_tool","rr_evo_diets.xlsx"))
  ggsave(here("results", "visualization_tool", "rr_evo_diets.pdf"), plot = graph_rr_evo_diets)
  ggsave(here("results", "visualization_tool", "rr_evo_diets_relative.pdf"), plot = graph_rr_evo_diets_relative)
  ggsave(here("results", "visualization_tool", "rr_evo_diets_shift.pdf"), plot = graph_rr_evo_diets_shift)
  ggsave(here("results", "visualization_tool", "rr_evo_diets_relative_shift.pdf"), plot = graph_rr_evo_diets_relative_shift)

# Nombre de décès dans chaque scénario
  export(deaths_wide, here("results", "visualization_tool", "deaths.xlsx"))

# Nombre total de décès
  export(total_deaths, here("results", "visualization_tool", "total_deaths.xlsx"))
  ggsave(here("results", "visualization_tool", "total_deaths.pdf"), plot = graph_total_deaths)

# Nombre total de décès évités  
  export(total_avoided_deaths, here("results", "visualization_tool", "total_avoided_deaths.xlsx"))
  export(total_avoided_deaths_shift, here("results", "visualization_tool", "total_avoided_deaths_shift.xlsx"))
  ggsave(here("results", "visualization_tool", "total_avoided_deaths.pdf"), plot = graph_total_avoided_deaths)
  
# Nombre total de décès évités par an
  ggsave(here("results", "visualization_tool", "total_avoided_deaths_yearly.pdf"), plot = graph_total_avoided_deaths_yearly)
  ggsave(here("results", "visualization_tool", "total_avoided_deaths_yearly_shift.pdf"), plot = graph_total_avoided_deaths_yearly_shift)

# Nombre de décès évités par age et année
  export(avoided_deaths, here("results", "visualization_tool", "avoided_deaths.xlsx"))
  ggsave(here("results", "visualization_tool", "avoided_deaths.pdf"), plot = heat_map_avoided_deaths)

# Nombre de décès évités par age en 2035
  export(avoided_deaths_2035, here("results", "visualization_tool", "avoided_deaths_2035.xlsx"))
  ggsave(here("results", "visualization_tool", "avoided_deaths_2035.pdf"), plot = graph_avoided_deaths_2035)
  
# Nombre de décès évités par age en 2050
  export(avoided_deaths_2050, here("results", "visualization_tool", "avoided_deaths_2050.xlsx"))
  ggsave(here("results", "visualization_tool", "avoided_deaths_2050.pdf"), plot = graph_avoided_deaths_2050)
  
  