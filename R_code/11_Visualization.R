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

# Expositions : régimes SISAE en 2050
  diets <- import(here("data", "DOUGLAS_diets.xlsx"))
  
# Risques relatifs / consommation (g/j) (Fadnes, 2022 & 2024)
  rr_table <- import(here("data_clean", "rr_table_interpolated.xlsx"))
  
################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################
  
# Bornes temporelles du modèle (années)
  year_i <- 2020 # Année initiale
  year_f <- 2050 # Année finale

# Borne inférieure de l'âge de la population du modèle (années)
  age_limit <- 18
  
# Dynamique d'implémentation des régimes (immediate, linear, cosine, sigmoidal)
  implementation <- "cosine"
  
  # paramètre de la courbe d'interpolation cosinus
  p <- 1
  
  # paramètre de la courbe sigmoïdale
  lambda <- 5

#  Time to full effect
  # durée (années)
  ttfe_time <- 10
  
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
                    "sc0" = "royalblue2",
                    "sc1" = "darkseagreen4",
                    "sc2" = "aquamarine2",
                    "sc3" = "lightpink",
                    "sc4" = "maroon",
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
  
  # Sélectionner les MR entre les bornes temporelles du modèle et au dessus de la limite d'age
  # Pivoter le dataframe en format long
  MR_select <- MR %>% 
    select(age, !!sym(as.character(year_i)) : !!sym(as.character(year_f))) %>%
    filter(age >= age_limit) %>% 
    pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
                 names_to = "year", 
                 values_to = "MR") %>% 
    mutate(year = as.numeric(year))
  
  # Sélectionner les effectifs de population entre les bornes temporelles du modèle et au dessus de la limite d'age 
  # Pivoter le dataframe en format long
  population_select <- population %>% 
    select(age, !!sym(as.character(year_i)) : !!sym(as.character(year_f))) %>% 
    filter(age >= age_limit) %>% 
    pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
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
    crossing(year_n = year_i:year_f) %>%
    mutate(quantity = case_when(
      implementation == "immediate" ~ q_f,
      implementation == "linear" ~ mapply(calc_food_q_lin, q_i, q_f, year_n, year_i, year_f),
      implementation == "cosine" ~ mapply(calc_food_q_cos, q_i, q_f, year_n, year_i, year_f, p),
      implementation == "sigmoidal" ~ mapply(calc_food_q_sig, q_i, q_f, year_n, year_i, year_f, lambda)
    )) %>% 
    select("food_group", "scenario", "year_n", "quantity") %>% 
    rename("year" = "year_n")

# Ordonnner les groupes alimentaires
  diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)

# Visualisation graphique
  ggplot(data = diets_evo, aes(x = year,
                               y = quantity,
                               fill = food_group))+
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6)+
    facet_wrap(~ scenario, ncol = 4)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(0.8)),
          legend.position = "bottom",
          legend.text = element_text(size = 6),
          legend.title = element_text(face = "bold", size = 7),
          legend.key.size = unit(0.2, "cm"),
          plot.margin = margin(1, 1, 1, 1, "cm"))+
    scale_fill_manual(values = col_food_groups)+
    labs(title = "Diet changes",
         x = "",
         y = "Quantities (g/day/pers)",
         fill = "Food type")+
    guides(fill = guide_legend(nrow = 2, 
                               title.position = "top",
                               title.hjust = 0.5))
  
################################################################################################################################
#                                             7. Attribution des RR à chaque régime                                            #
################################################################################################################################
  
  diets_evo <- diets_evo %>% 
    mutate(quantity = round(quantity)) %>% 
    left_join(rr_table, by = c("food_group", "quantity"))
  
################################################################################################################################
#                                             8. Time to full effect                                                           #
################################################################################################################################
  
# % du RR chaque année sur la période du time to full effect
  ttfe <- tibble(0:ttfe_time) %>% 
    rename("time" = "0:ttfe_time") %>% 
    mutate(ttfe = case_when(
      ttfe_time == 0 ~ 1,
      ttfe_dynamics == "immediate" ~ 1,
      ttfe_dynamics == "linear" ~ time/ttfe_time,
      ttfe_dynamics == "cosine" ~ (1 - cos(pi * (time/ttfe_time)^p_ttfe))/2,
      ttfe_dynamics == "sigmoidal" ~ (1 / (1 + exp(-lambda_ttfe * (time / ttfe_time - 1/2))) - 1 / (1 + exp(lambda_ttfe / 2))) / 
        (1 - 2 / (1 + exp(lambda_ttfe / 2))),
      ttfe_dynamics == "log" ~ log(1 + eta_ttfe * time/ttfe_time) / log(1 + eta_ttfe),
      TRUE ~ NA_real_
    ))

# Représentation graphique 
  ggplot(ttfe, aes(x = time,
                   y = ttfe))+
    geom_line(color = "darkseagreen", size = 1, alpha = 0.8)+
    labs(title = "Time to full effect",
         x = "",
         y = "")  
  
################################################################################################################################
#                                             9. Calcul des RR avec TTFE                                                       #
################################################################################################################################
  
# Calcul de la valeur des RR sur la durée du time to full effect
# Après le time to full effect : RR = NA
  diets_evo <- diets_evo %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = year_i, to = year_f))) %>% 
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe$time) ~ 1 + (rr - 1) * ttfe$ttfe[match(year_n - year, ttfe$time)],
      year_n > year + max(ttfe$time) ~ NA_real_
    )) %>% 
    ungroup()

################################################################################################################################
#                                             10. Exemple : RR du régime de 2025 dans S1                                       #
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
  ggplot(rr_sc1, aes(x = year,
                         y = rr,
                         color = as.factor(food_group)))+
    geom_line(size = 1, alpha = 0.8, na.rm = TRUE)+
    scale_color_manual(values = col_food_groups)+
    labs(title = "RR associated with 2025 changes in diet in S1",
         x = "",
         y = "RR",
         color = "Food Group")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  

################################################################################################################################
#                                             11. Combinaison des RR de chaque aliment par année                               #
################################################################################################################################
  
# Calcul des RR de chaque aliment pour chaque année, par moyenne géométrique
  rr_evo_food_combined <- diets_evo %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = case_when(
      combinaison_rr_type == "arithmetic mean" ~ mean(rr_n, na.rm = TRUE),
      combinaison_rr_type == "geometric mean"~ geometric.mean(rr_n, na.rm = TRUE))
    )
  
# Visualisation graphique
  ggplot(data = rr_evo_food_combined, aes(x = year_n,
                               y = mean_rr,
                               color = as.factor(food_group)))+
    geom_line(size = 1, alpha = 0.8, na.rm = TRUE)+
    scale_color_manual(values = col_food_groups)+
    facet_wrap(~ scenario, ncol = 4)+
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
#                                             12. Combinaison des RR de chaque régime par année                                #
################################################################################################################################
  
# Produit des RR de chaque aliment par année
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
  ggplot(rr_evo_diets, aes(x = year,
                           y = combined_rr,
                           color = scenario))+
    geom_line(size = 1)+
    scale_color_manual(values = col_scenario)+
    labs(title = "Whole diet RR evolution in each scenario",
         x = "",
         y = "RR") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  

################################################################################################################################
#                                             13. Evaluation d'impact sanitaire par rapport au régime Actuel                   #
################################################################################################################################
  
# Ajustement des taux de mortalité
  MR_adjusted <- MR_select %>% 
    inner_join(rr_evo_diets, by = "year", relationship = "many-to-many") %>%
    group_by(age, year) %>% 
    mutate(adjusted_mr = MR*combined_rr/combined_rr[scenario == "actuel"]) %>% 
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
  
# Nombre de décès évités par age et par année
  
  # Filtrer les décès du scénario "Tendanciel"
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
#                                             14. Graphiques : Evaluation d'impact sanitaire par rapport au régime Actuel      #
################################################################################################################################
  
# Nombre de décès total par scénario
  ggplot(data = total_deaths, aes(x = scenario, 
                                  y = total_deaths, 
                                  fill = scenario))+
    geom_bar(stat = "identity", alpha = 0.8)+
    labs(title =  "Total number of deaths in each scenario",
         x = "Scenario", 
         y = "Number of deaths")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nombre total de décès évités par scénario
  ggplot(data = total_avoided_deaths, aes(x = scenario, 
                                          y = avoided_deaths, 
                                          fill = scenario))+
      geom_bar(stat = "identity", alpha = 0.8)+
      labs(title = "Total avoided deaths compared to keeping the current diet",
           x = "",
           y = "number of deaths avoided")+
      scale_fill_manual(values = col_scenario)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Nombre de décès évités par âge et par année, dans chaque scénario
  ggplot(avoided_deaths_long, aes(x = year,
                                  y = age,
                                  fill = avoided_deaths)) +
    geom_tile()+
    facet_wrap(~ scenario, scales = "fixed") +
    scale_fill_gradient2(low = "dodgerblue4", mid = "grey80", high = "firebrick", limits = c(0,12000)) +
    labs(title = "Avoided deaths compared to keeping the current diet",
         x = "",
         y = "Age",
         fill = "Avoided deaths") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold",size = rel(0.8)))

# Nombre de décès évités par âge et par scénario en 2035
 ggplot(avoided_deaths_2035, aes(x = age,
                                 y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2035 compared to keeping the current diet",
         x = "Age",
         y = "Avoided deaths")
  
# Nombre de décès évités par âge et par scénario en 2050
  ggplot(avoided_deaths_2050, aes(x = age,
                                  y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "darkseagreen4")+
    labs(title = "Avoided deaths in 2050 compared to keeping the current diet",
         x = "Age",
         y = "Avoided deaths")