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

# Risques relatifs / consommation (g/j), relations dose-réponse simulées
rr_table <- import(here("data_clean", "rr_table_interpolated_sim.csv"))

# Population
# Taux de mortalité (INSEE)
MR <- import(here("data_clean", "MR_table.xlsx"))

# Effectifs de population par age et par année (INSEE)
population <- import(here("data_clean", "population_clean.xlsx"))


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
lambda <- 8

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
ttfe_dynamics <- "linear"

# paramètre de la courbe d'interpolation cosinus
p_ttfe <- 1

# paramètre de la courbe sigmoïdale
lambda_ttfe <- 8

# paramètre de la courbe log 
eta_ttfe <- 1

################################################################################################################################
#                                             4. Charte graphique                                                              #
################################################################################################################################

# Couleur de chaque scénario
col_scenario <- c("actuel" = "azure4",
                  "sc0" = "palevioletred3",
                  "sc1" = "aquamarine3",
                  "sc2" = "lightskyblue3",
                  "sc3" = "#882255",
                  "sc4" = "#DDCC77",
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
                        "reffined_grains" = "Refined grains",
                        "added_plant_oils" = "Added plant oils",
                        "sugar_sweetened_beverages" = "SSB")

################################################################################################################################
#                                             5. Préparation des données                                                       #
################################################################################################################################

# Renommer la variable RR
rr_table <- rr_table %>% 
  rename("rr" = "rr_interpolated")

# Sélectionner les MR entre les bornes temporelles du modèle et au dessus de la limite d'age
# Pivoter le dataframe en format long
  MR_select <- MR %>% 
    select(age, !!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time))) %>% 
    filter(age >= age_limit) %>% 
    pivot_longer(cols = !!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time)), 
                 names_to = "year", 
                 values_to = "MR") %>% 
    mutate(year = as.numeric(year)) 

# Sélectionner les effectifs de population entre les bornes temporelles du modèle et au dessus de la limite d'age 
# Pivoter le dataframe en format long
  population_select <- population %>% 
    select(age, !!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time))) %>% 
    filter(age >= age_limit) %>% 
    pivot_longer(cols = !!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time)), 
                 names_to = "year", 
                 values_to = "population") %>% 
    mutate(year = as.numeric(year)) %>% 
    arrange(age)

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
    crossing(year_n = (year_i - 2*ttfe_time) : (year_f + 2*ttfe_time)) %>%
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

# Calcul des variations de consommation de chaque aliment par rapport au régime actuel (%)
diets_evo <-  diets_evo %>%
  group_by(food_group, year) %>%
  mutate(var = (quantity - quantity[scenario == "actuel"]) / quantity[scenario == "actuel"] *100)

# Ordonnner les groupes alimentaires
  diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)

# Visualisation graphique des consommations sur toute la période
  graph_diets_evo <- ggplot(data = diets_evo, aes(x = year,
                                                  y = quantity,
                                                  fill = food_group))+
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6)+
    facet_wrap(~ scenario, 
               ncol = 3,
               labeller = labeller(scenario = labels_scenario))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9),
          strip.text = element_text(face = "bold",size = rel(0.8)),
          legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.title = element_text(face = "bold", size = 12),
          legend.key.size = unit(0.3, "cm"),
          plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))+
    scale_fill_manual(values = col_food_groups,
                      labels = labels_food_groups)+
    labs(title = "",
         x = "",
         y = "Intakes (g/d/pers)",
         fill = "Food type")+
    guides(fill = guide_legend(nrow = 2, 
                               title.position = "top",
                               title.hjust = 0.5))
  
# Visualisation graphique des variations de consommation sur toute la période
  # Tous les scénarios
  graph_diets_var <- ggplot(diets_evo %>% 
                              filter(scenario != "actuel"),
                            aes(x = year,
                                y = var,
                                color = food_group))+
    facet_wrap(~ scenario,
               labeller = labeller(scenario = labels_scenario)) +
    geom_line(size = 0.8, na.rm = TRUE)+ 
    labs(
      title = "",
      x = "",
      y = "Variations of food intake (%)",
      color = "Food group"
    )+
    scale_color_manual(values = col_food_groups,
                       labels = labels_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(1)),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, 
                                title.position = "top",
                                title.hjust = 0.5))
  
  # Scénario 1
  graph_diet_sc1_var <- ggplot(diets_evo %>% 
                                 filter(scenario == "sc1"),
                               aes(x = year,
                                   y = var,
                                   color = food_group))+
                          geom_line(size = 0.8, na.rm = TRUE)+ 
                          labs(
                            title = "Shifts in food intake in Scenario 1",
                            x = "",
                            y = "Variations of food intake (%)",
                            color = "Food group"
                          )+
                          scale_color_manual(values = col_food_groups,
                                             labels = labels_food_groups)+
                          theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                                axis.text.y = element_text(size = 7),
                                strip.text = element_text(face = "bold",size = rel(1)),
                                legend.position = "bottom") +
                          guides(color = guide_legend(nrow = 3, 
                                                      title.position = "top",
                                                      title.hjust = 0.5))
  
  # Scénario 2
  graph_diet_sc2_var <- ggplot(diets_evo %>% 
                                 filter(scenario == "sc2"),
                               aes(x = year,
                                   y = var,
                                   color = food_group))+
    geom_line(size = 0.8, na.rm = TRUE)+ 
    labs(
      title = "Shifts in food intake in Scenario 2",
      x = "",
      y = "Variations of food intake (%)",
      color = "Food group"
    )+
    scale_color_manual(values = col_food_groups,
                       labels = labels_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(1)),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, 
                                title.position = "top",
                                title.hjust = 0.5))
  
  # Scénario 3
  graph_diet_sc3_var <- ggplot(diets_evo %>% 
                                 filter(scenario == "sc3"),
                               aes(x = year,
                                   y = var,
                                   color = food_group))+
    geom_line(size = 0.8, na.rm = TRUE)+ 
    labs(
      title = "Shifts in food intake in Scenario 3",
      x = "",
      y = "Variations of food intake (%)",
      color = "Food group"
    )+
    scale_color_manual(values = col_food_groups,
                       labels = labels_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(1)),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, 
                                title.position = "top",
                                title.hjust = 0.5))
  
  # Scénario 4
  graph_diet_sc4_var <- ggplot(diets_evo %>% 
                                 filter(scenario == "sc4"),
                               aes(x = year,
                                   y = var,
                                   color = food_group))+
    geom_line(size = 0.8, na.rm = TRUE)+ 
    labs(
      title = "Shifts in food intake in Scenario 4",
      x = "",
      y = "Variations of food intake (%)",
      color = "Food group"
    )+
    scale_color_manual(values = col_food_groups,
                       labels = labels_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(1)),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, 
                                title.position = "top",
                                title.hjust = 0.5))
    

  diets_evo_shift <- diets_evo %>% 
    filter(year %in% c(year_i:year_f))

# Visualisation graphique des consommations sur la période de changement de régime
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
          legend.text = element_text(size = 9),
          legend.title = element_text(face = "bold", size = 10),
          legend.key.size = unit(0.3, "cm"),
          plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))+
    scale_fill_manual(values = col_food_groups,
                      labels = labels_food_groups)+
    labs(title = "",
         x = "",
         y = "Intakes (g/d/pers)",
         fill = "Food type")+
    guides(fill = guide_legend(nrow = 2, 
                               title.position = "top",
                               title.hjust = 0.5))

################################################################################################################################
#                                             8. Modification d'effet des RR                                                         #
################################################################################################################################

# Application du modification d'effet des RR (m)
rr_table <- rr_table %>% 
  mutate(rr_a = case_when(
    rr < 1 ~ rr + (1 - rr) * (1 - m),
    rr >= 1 ~ 1 / (m/rr + 1 - m)
  )) %>% 
  select("simulation_id", "food_group", "quantity", "rr_a") %>% 
  rename("rr" = "rr_a")

################################################################################################################################
#                                             9. Attribution des RR à chaque régime                                            #
################################################################################################################################

# Quantités arrondies à l'unité pour matcher avec le tableau des RR
diets_evo <- diets_evo %>% 
  mutate(quantity = round(quantity)) %>% 
  left_join(rr_table, by = c("food_group", "quantity"), relationship = "many-to-many")


# Calculer la valeur centrales et les IC95 pour chaque année
simulations_summary <- diets_evo %>%
  group_by(food_group, scenario, year, quantity) %>%
  summarise(
    mean_rr = mean(rr, na.rm = TRUE),  # Moyenne des simulations
    lower_ci = quantile(rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
    upper_ci = quantile(rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
  ) 

################################################################################################################################
#                                             10. Représentations graphiques des simulations des valeurs de RR                 #
################################################################################################################################

# S1
graph_rr_fg_sim_sc1 <- ggplot(simulations_summary %>% 
                                filter(scenario == "sc1"), 
                              aes(x = year,
                                  y = mean_rr,
                                  color = food_group)) +
  facet_wrap(~ food_group,
             labeller = labeller(food_group =labels_food_groups))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) +  # Intervalle de confiance
  geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
  labs(
    title = "",
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
graph_rr_fg_sim_sc2 <- ggplot(simulations_summary %>% 
                                filter(scenario == "sc2"), 
                              aes(x = year,
                                  y = mean_rr,
                                  color = food_group)) +
  facet_wrap(~ food_group,
             labeller = labeller(food_group =labels_food_groups))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) +  # Intervalle de confiance
  geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
  labs(
    title = "",
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
graph_rr_fg_sim_sc3 <- ggplot(simulations_summary %>% 
                                filter(scenario == "sc3"), 
                              aes(x = year,
                                  y = mean_rr,
                                  color = food_group)) +
  facet_wrap(~ food_group,
             labeller = labeller(food_group =labels_food_groups))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) +  # Intervalle de confiance
  geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
  labs(
    title = "",
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
graph_rr_fg_sim_sc4 <- ggplot(simulations_summary %>% 
                                filter(scenario == "sc4"), 
                              aes(x = year,
                                  y = mean_rr,
                                  color = food_group)) +
  facet_wrap(~ food_group,
             labeller = labeller(food_group =labels_food_groups))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) +  # Intervalle de confiance
  geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
  labs(
    title = "",
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
#                                             11. TTFE                                                                         #
################################################################################################################################

# Durée du TTFE
ttfe <- tibble(0:ttfe_time) %>% 
  rename("time" = "0:ttfe_time")

# Calcul du % accordé au RR sur la durée du TTFE
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
  geom_line(color = "darkseagreen", linewidth = 1, alpha = 0.8)+
  labs(title = "",
       x = "years",
       y = "Weight")  


################################################################################################################################
#                                             12. Calcul des RR avec TTFE                                                      #
################################################################################################################################

# Calcul de la valeur des RR sur la durée du TTFE
# Après la fin du TTFE : RR = NA
diets_evo <- diets_evo %>% 
  rowwise() %>% 
  mutate(year_n = list(seq(from = (year_i - 2*ttfe_time), to = (year_f + 2*ttfe_time)))) %>% 
  unnest(year_n) %>% 
  mutate(rr_n = case_when(
    year_n < year ~ NA_real_,
    year_n >= year & year_n <= year + max(ttfe$time) ~ rr * ttfe$ttfe[match(year_n - year, ttfe$time)],
    year_n > year + max(ttfe$time) ~ NA_real_)) %>% 
  ungroup()

################################################################################################################################
#                                             13. Combinaison des RR de chaque aliment par année                               #
################################################################################################################################

# Le RR d'un aliment une année n est la moyenne des RR de cet aliment générés avec le TTFE pour cette année
rr_evo_food_combined <- diets_evo %>% 
  group_by(scenario, year_n, food_group, simulation_id) %>% 
  summarize(mean_rr = sum(rr_n, na.rm = TRUE)/sum(ttfe$ttfe, na.rm = TRUE))

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_rr_fg_combined <- rr_evo_food_combined %>%
  group_by(food_group, scenario, year_n) %>%
  summarise(
    combined_rr = mean(mean_rr, na.rm = TRUE),
    lower_ci = quantile(mean_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
    upper_ci = quantile(mean_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             14. Représentations graphiques des simulations des valeurs de RR combinés        #
################################################################################################################################

# S1
graph_rr_fg_combined_sim_sc1 <- ggplot(simulations_summary_rr_fg_combined %>% 
                                         filter(scenario == "sc1"), 
                                       aes(x = year_n,
                                           y = combined_rr,
                                           color = food_group)) +
  facet_wrap(~ food_group,
             labeller = labeller(food_group =labels_food_groups))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) +  # Intervalle de confiance
  geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
  labs(
    title = "",
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
graph_rr_fg_combined_sim_sc2 <- ggplot(simulations_summary_rr_fg_combined %>% 
                                         filter(scenario == "sc2"), 
                                       aes(x = year_n,
                                           y = combined_rr,
                                           color = food_group)) +
  facet_wrap(~ food_group,
             labeller = labeller(food_group =labels_food_groups))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) +  # Intervalle de confiance
  geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
  labs(
    title = "",
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
graph_rr_fg_combined_sim_sc3 <- ggplot(simulations_summary_rr_fg_combined %>% 
                                         filter(scenario == "sc3"), 
                                       aes(x = year_n,
                                           y = combined_rr,
                                           color = food_group)) +
  facet_wrap(~ food_group,
             labeller = labeller(food_group =labels_food_groups))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) +  # Intervalle de confiance
  geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
  labs(
    title = "",
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
graph_rr_fg_combined_sim_sc4 <- ggplot(simulations_summary_rr_fg_combined %>% 
                                         filter(scenario == "sc4"), 
                                       aes(x = year_n,
                                           y = combined_rr,
                                           color = food_group)) +
  facet_wrap(~ food_group,
             labeller = labeller(food_group =labels_food_groups))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) +  # Intervalle de confiance
  geom_line(linewidth = 1, na.rm = TRUE) +  # Moyenne en trait plein
  labs(
    title = "",
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
#                                             15. Combinaison des RR de chaque régime par année                                #
################################################################################################################################

# Le RR d'un régime complet est calculé comme le produit des RR de chaque aliment pour une année

# Fonction produit des RR de chaque aliment par année
calc_combined_rr <- function(df) {
  df %>%
    group_by(scenario, year_n, simulation_id) %>%
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
    mean_rr = mean(combined_rr, na.rm = TRUE),  
    lower_ci = quantile(combined_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
    upper_ci = quantile(combined_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
  ) 

################################################################################################################################
#                                             16. Représentations graphiques des simulations des valeurs de RR des régimes     #
################################################################################################################################


graph_rr_diets_sim  <- ggplot(simulations_summary_rr_diets, aes(x = year,
                                                                y = mean_rr,
                                                                color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  facet_wrap(~ scenario,
             labeller = labeller(scenario = labels_scenario))+
  geom_line(size = 1, na.rm = TRUE)+ 
  labs(
    title = "",
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
#                                             17. RR relatif au RR actuel                                                  #
################################################################################################################################

# Calcul des RR des régimes complets relatifs aux RR du scénario actuel
rr_evo_diets <- rr_evo_diets %>% 
  group_by(year, simulation_id) %>% 
  mutate(relative_rr = combined_rr/combined_rr[scenario == "actuel"]) %>% 
  ungroup()

# Tant que l'implémentation des régimes n'a pas commencé, le RR relatif est égal à 1
rr_evo_diets <- rr_evo_diets %>% 
  mutate(relative_rr = case_when(
    year == year_i - 2*ttfe_time ~ 1,
    TRUE ~ relative_rr))

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_rr_diets_relative <- rr_evo_diets %>%
  group_by(scenario, year) %>%
  summarise(
    mean_rr = mean(relative_rr, na.rm = TRUE),  
    lower_ci = quantile(relative_rr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
    upper_ci = quantile(relative_rr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
  ) 

################################################################################################################################
#                                             18. Représentations graphiques des simulations des valeurs de RR relatives des régimes     #
################################################################################################################################


graph_rr_diets_relative_sim <- ggplot(simulations_summary_rr_diets_relative %>% 
                                        filter(scenario != "actuel"),
                                      aes(x = year,
                                          y = mean_rr,
                                          color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  geom_line(size = 0.6, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "RR",
    color = "Scenario",
    fill = "Scenario"
  )+
  scale_color_manual(values = col_scenario,
                     labels = labels_scenario)+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(face = "bold",size = rel(1)),
        legend.position = "bottom")+
  guides(color = guide_legend(title = NULL),
         fill = guide_legend(title = NULL))

################################################################################################################################
#                                             19. Ajustement des taux de mortalité                                             #
################################################################################################################################

#MRa = MRO*RR(scenario)/RR(actuel), où MRO est le taux de mortalité projeté par l'INSEE

# Ajustement des taux de mortalité
MR_adjusted <- MR_select %>% 
  inner_join(rr_evo_diets, by = "year", relationship = "many-to-many") %>%
  group_by(age, year, simulation_id) %>% 
  mutate(adjusted_mr = MR*relative_rr) %>% 
  ungroup()

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_mr_adjusted <- MR_adjusted %>%
  group_by(age, scenario, year) %>%
  summarise(
    mean_rr = mean(adjusted_mr, na.rm = TRUE),  
    lower_ci = quantile(adjusted_mr, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
    upper_ci = quantile(adjusted_mr, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
  ) 
################################################################################################################################
#                                             20. Nombre de décès                                                              #
################################################################################################################################

# Décès dans chaque scénario 
deaths <- MR_adjusted %>%
  left_join(population_select, by = c("age", "year"), relationship = "many-to-many") %>% 
  mutate(deaths = adjusted_mr*population)

# Eliminer Les valeurs 5% les plus extrêmes
deaths <- deaths %>%
  group_by(scenario, year, age) %>%
  filter(between(deaths, quantile(deaths, 0.025), quantile(deaths, 0.975)))

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_deaths <- deaths %>%
  group_by(age, scenario, year) %>%
  summarise(
    mean_rr = mean(deaths, na.rm = TRUE),  # Moyenne des simulations
    lower_ci = quantile(deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
    upper_ci = quantile(deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
  ) 

  # Transformation au format large
  deaths_wide <- deaths %>% 
    select("age", "year", "scenario", "simulation_id", "deaths") %>% 
    pivot_wider(names_from = "year", values_from = "deaths")

# Nombre total de décès par année et par scénario
total_deaths <- deaths_wide %>% 
  group_by(scenario, simulation_id) %>%                                 
  summarise(across(!!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time)), sum)) %>%
  rowwise() %>%
  mutate(total_deaths = sum(c_across(!!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time)))))  

  # Transformation au format long
  total_deaths_long <- total_deaths %>% 
    select(-total_deaths) %>% 
    pivot_longer(cols = !!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time)),
                 names_to = "year",
                 values_to = "total_deaths") %>% 
    mutate(year = as.numeric(year))

################################################################################################################################
#                                             21. Nombre de décès évités par rapport au baseline                               #
################################################################################################################################

# Nombre total de décès évités par an
total_avoided_deaths <- total_deaths_long %>% 
  group_by(year, simulation_id) %>% 
  mutate(avoided_deaths = total_deaths[scenario == "actuel"] - total_deaths)

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_avoided_deaths <- total_avoided_deaths %>%
  group_by(scenario, year) %>%
  summarise(
    mean_rr = mean(avoided_deaths, na.rm = TRUE),  
    lower_ci = quantile(avoided_deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
    upper_ci = quantile(avoided_deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
  )

# Résultats sur la période de changement de régime
simulations_summary_avoided_deaths_shift <- simulations_summary_avoided_deaths %>% 
  filter(year %in% c(year_i:year_f))

# Nombre de décès évités par an et par age
avoided_deaths <-  deaths %>% 
  group_by(age, year, simulation_id) %>% 
  mutate(avoided_deaths = deaths[scenario == "actuel"] - deaths)

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_avoided_deaths_age <- avoided_deaths %>%
  group_by(age, scenario, year) %>%
  summarise(
    mean_rr = mean(avoided_deaths, na.rm = TRUE), 
    lower_ci = quantile(avoided_deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
    upper_ci = quantile(avoided_deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
  )

# Nombre de décès évités par age, cumulés sur une période
  # année initiale du changement - 2035
  avoided_deaths_cum_2035 <- avoided_deaths %>% 
    filter(year >= year_i & year <= 2035) %>% 
    group_by(age, scenario, simulation_id) %>% 
    summarise(cum_avoided_deaths = sum(avoided_deaths))
  
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_avoided_deaths_cum_2035 <- avoided_deaths_cum_2035 %>%
    group_by(age, scenario) %>%
    summarise(
      mean_rr = mean(cum_avoided_deaths, na.rm = TRUE),  
      lower_ci = quantile(cum_avoided_deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(cum_avoided_deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    )

  # année initiale du changement - 2050
  avoided_deaths_cum_2050 <- avoided_deaths %>% 
    filter(year >= year_i & year <= 2050) %>% 
    group_by(age, scenario, simulation_id) %>% 
    summarise(cum_avoided_deaths = sum(avoided_deaths))
  
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_avoided_deaths_cum_2050 <- avoided_deaths_cum_2050 %>%
    group_by(age, scenario) %>%
    summarise(
      mean_rr = mean(cum_avoided_deaths, na.rm = TRUE),  # Moyenne des simulations
      lower_ci = quantile(cum_avoided_deaths, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(cum_avoided_deaths, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    )

# Nombre de décès évités / taille de la population
  # Taille de la population par année
    population_select_tot <- population_select %>% 
      pivot_wider(names_from = "year",
                  values_from = "population") %>% 
      summarize(across(!!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time)), sum)) %>% 
      pivot_longer(cols = !!sym(as.character(year_i - 20)) : !!sym(as.character(year_f + 2*ttfe_time)),
                   names_to = "year",
                   values_to = "population") %>% 
      mutate(year = as.numeric(year))
    
    # Calcul du nombre de décès évités / taille de la population
      total_avoided_deaths <- total_avoided_deaths %>% 
        left_join(population_select_tot, by = "year") %>% 
        group_by(scenario, simulation_id, year) %>% 
        mutate(avoided_deaths_rel = avoided_deaths/population)
      
    # Calculer la moyenne et les IC95 pour chaque année
      simulations_summary_avoided_deaths_rel <- total_avoided_deaths %>%
        group_by(scenario, year) %>%
        summarise(
          mean_rr = mean(avoided_deaths_rel, na.rm = TRUE),  
          lower_ci = quantile(avoided_deaths_rel, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
          upper_ci = quantile(avoided_deaths_rel, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
        )
      
################################################################################################################################
#                                             22. Représentations graphiques des simulations des décès évités par année        #
################################################################################################################################

# Sur toute la durée du modèle
graph_total_avoided_deaths_facet  <- ggplot(simulations_summary_avoided_deaths %>% 
                                        filter(scenario != "actuel"),
                                      aes(x = year,
                                          y = mean_rr,
                                          color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  facet_wrap(~ scenario,
             labeller = labeller(scenario = labels_scenario))+
  geom_line(size = 0.8, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "Number of deaths prevented"
  )+
  scale_color_manual(values = col_scenario)+
  scale_fill_manual(values = col_scenario)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(face = "bold",size = rel(1)),
        legend.position = "none")

graph_total_avoided_deaths  <- ggplot(simulations_summary_avoided_deaths %>% 
                                        filter(scenario != "actuel"),
                                      aes(x = year,
                                          y = mean_rr,
                                          group = scenario,
                                          color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  geom_line(size = 0.6, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "Number of deaths prevented"
  )+
  scale_color_manual(values = col_scenario,
                     labels = labels_scenario)+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(face = "bold",size = rel(1)),
        legend.position = "bottom")+
  guides(color = guide_legend(title = NULL),
         fill = guide_legend(title = NULL))

# Nombre de décès évités / taille de la population
  graph_total_avoided_deaths_rel  <- ggplot(simulations_summary_avoided_deaths_rel %>% 
                                        filter(scenario != "actuel"),
                                      aes(x = year,
                                          y = mean_rr,
                                          group = scenario,
                                          color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  geom_line(size = 0.6, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "Deaths prevented relative to the population"
  )+
  scale_color_manual(values = col_scenario,
                     labels = labels_scenario)+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(face = "bold",size = rel(1)),
        legend.position = "bottom")+
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL))


# Sur la période de changement de régime
graph_total_avoided_deaths_shift_facet <- ggplot(simulations_summary_avoided_deaths_shift %>% 
                                             filter(scenario != "actuel"),
                                           aes(x = year,
                                               y = mean_rr,
                                               color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  facet_wrap(~ scenario,
             labeller = labeller(scenario = labels_scenario))+
  geom_line(size = 0.8, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "Number of deaths prevented"
  )+
  scale_color_manual(values = col_scenario)+
  scale_fill_manual(values = col_scenario)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(face = "bold",size = rel(1)),
        legend.position = "none")

graph_total_avoided_deaths_shift <- ggplot(simulations_summary_avoided_deaths_shift %>% 
                                             filter(scenario != "actuel"),
                                           aes(x = year,
                                               y = mean_rr,
                                               group = scenario,
                                               color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  geom_line(size = 0.8, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "Number of deaths prevented"
  )+
  scale_color_manual(values = col_scenario,
                     labels = labels_scenario)+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(face = "bold",size = rel(1)),
        legend.position = "bottom")+
  guides(color = guide_legend(title = NULL),
         fill = guide_legend(title = NULL))

# En 2040, 2050 et 2060
graph_avoided_deaths_dates <- ggplot(simulations_summary_avoided_deaths %>% 
                           filter(year %in% c(2040, 2050, 2060),
                                  scenario != "actuel"),
                         aes(x = scenario,
                             y = mean_rr,
                             fill = scenario))+
  geom_bar(stat = "identity",
           position = "dodge",
           alpha = 0.7)+
  geom_errorbar(aes(ymin = lower_ci,
                    ymax = upper_ci),
                width = 0.2,
                position = position_dodge(0.9))+
  facet_wrap(~year,
             ncol = 3)+
  scale_y_continuous(labels = scales :: label_comma())+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  labs(title = "",
       x = "",
       y = "Deaths prevented")+
  guides(fill = guide_legend(title = NULL))


################################################################################################################################
#                                             23. Contributions de chaque aliment au résultat                                  #
################################################################################################################################
  
# RR de chaque aliment/année relatif au RR du scénario baseline
  rr_fg_relative <- rr_evo_food_combined %>% 
    group_by(year_n, food_group, simulation_id) %>% 
    mutate(rr_fg_relative = mean_rr / mean_rr[scenario == "actuel"]) %>% 
    ungroup()
  
# Tant que l'implémentation des régimes n'a pas commencé, le RR relatif est égal à 1
  rr_fg_relative <- rr_fg_relative %>% 
    mutate(rr_fg_relative = case_when(
      year_n == year_i - 2*ttfe_time ~ 1,
      TRUE ~ rr_fg_relative))
  
  
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_rr_fg_relative <- rr_fg_relative %>%
    group_by(scenario, year_n, food_group) %>%
    summarise(
      mean_rr_fg = mean(rr_fg_relative, na.rm = TRUE),  
      lower_ci = quantile(rr_fg_relative, 0.025, na.rm = TRUE),  # Limite inférieure de l'IC à 95%
      upper_ci = quantile(rr_fg_relative, 0.975, na.rm = TRUE)   # Limite supérieure de l'IC à 95%
    ) 
  
# Calcul de la contribution au résultat de la variation de consommation de chaque aliment par rapport au baseline
  contrib <- simulations_summary_rr_fg_relative %>% 
    mutate(delta = -(1-mean_rr_fg)*100,
           delta_low = -(1-lower_ci)*100,
           delta_upp = -(1-upper_ci)*100) %>% 
    select(scenario, year_n, food_group, delta, delta_low, delta_upp) %>% 
    rename("year" = "year_n")
  
  # Ordonner les groupes alimentaires
  contrib$food_group <- factor(contrib$food_group, levels = order_food_groups)
  
  # Représentation graphique 
  graph_contrib_fg <- ggplot(contrib %>% 
                               filter(scenario != "actuel"),
                             aes(x = year,
                                 y = delta,
                                 color = food_group)) +
    geom_ribbon(aes(ymin = delta_low, ymax = delta_upp, fill = food_group), alpha = 0.5, linetype = 0) +
    facet_wrap(~ scenario,
               labeller = labeller(scenario = labels_scenario)) +
    geom_line(size = 0.8, na.rm = TRUE)+ 
    labs(
      title = "",
      x = "",
      y = "% of deaths prevented",
      color = "Food group",
      fill = "Food group"
    )+
    scale_color_manual(values = col_food_groups,
                       labels = labels_food_groups)+
    scale_fill_manual(values = col_food_groups,
                      labels = labels_food_groups)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(face = "bold",size = rel(1)),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, 
                                title.position = "top",
                                title.hjust = 0.5))

################################################################################################################################
#                                             27. Exportation des données                                                      #
################################################################################################################################

# Implémentation des régimes
  # Régimes (quantités et variations par rapport au baseline), RR associés aux consommations, RR*TTFE
    export(diets_evo, here("results", "visualization_tool_ic95_sim", "diets_rr_evo.csv"))

  # Quantités consommées de chaque aliment
    ggsave(here("results", "visualization_tool_ic95_sim", "diets_evo.pdf"), plot = graph_diets_evo)
    ggsave(here("results", "visualization_tool_ic95_sim", "diets_evo_shift.pdf"), plot = graph_diets_evo_shift)
    
  # Variations de consommations de chaque aliment par rapport au baseline
    ggsave(here("results", "visualization_tool_ic95_sim", "diets_var.pdf"), plot = graph_diets_var)
    
    ggsave(here("results", "visualization_tool_ic95_sim", "diets_var_sc1.pdf"), plot = graph_diet_sc1_var)
    ggsave(here("results", "visualization_tool_ic95_sim", "diets_var_sc2.pdf"), plot = graph_diet_sc2_var)
    ggsave(here("results", "visualization_tool_ic95_sim", "diets_var_sc3.pdf"), plot = graph_diet_sc3_var)
    ggsave(here("results", "visualization_tool_ic95_sim", "diets_var_sc4.pdf"), plot = graph_diet_sc4_var)


# Valeurs de RR simulées par aliment, dans chaque scénario
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_fg_sim_sc1.pdf"), plot = graph_rr_fg_sim_sc1)
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_fg_sim_sc2.pdf"), plot = graph_rr_fg_sim_sc2)  
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_fg_sim_sc3.pdf"), plot = graph_rr_fg_sim_sc3)  
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_fg_sim_sc4.pdf"), plot = graph_rr_fg_sim_sc4)  

# Time to full effect
  ggsave(here("results", "visualization_tool_ic95_sim", "ttfe.pdf"), plot = graph_ttfe)

# Valeurs des RR de chaque aliment, combinés par année
  export(rr_evo_food_combined, here("results", "visualization_tool_ic95_sim", "rr_fg_evo_combined.csv"))
  export(simulations_summary_rr_fg_combined, here("results", "visualization_tool_ic95_sim", "IC95_rr_fg_evo_combined.xlsx"))
  
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_fg_combined_sim_sc1.pdf"), plot = graph_rr_fg_combined_sim_sc1)
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_fg_combined_sim_sc2.pdf"), plot = graph_rr_fg_combined_sim_sc2)  
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_fg_combined_sim_sc3.pdf"), plot = graph_rr_fg_combined_sim_sc3)  
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_fg_combined_sim_sc4.pdf"), plot = graph_rr_fg_combined_sim_sc4)  

# Valeurs des RR des régimes par année
  export(rr_evo_diets, here("results", "visualization_tool_ic95_sim", "rr_evo_diets.xlsx"))
  export(simulations_summary_rr_diets, here("results", "visualization_tool_ic95_sim", "IC95_rr_evo_diets.xlsx"))
  
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_diets_sim.pdf"), plot = graph_rr_diets_sim)

# Valeurs des RR des régimes, relatifs au scénario actuel
  export(simulations_summary_rr_diets_relative, here("results", "visualization_tool_ic95_sim", "IC95_rr_evo_diets_relative.xlsx"))
  
  ggsave(here("results", "visualization_tool_ic95_sim", "rr_diets_relative_sim.pdf"), plot = graph_rr_diets_relative_sim)

# Taux de mortalité ajustés
  export(MR_adjusted, here("results", "visualization_tool_ic95_sim", "MR_adjusted.csv"))
  export(simulations_summary_mr_adjusted, here("results", "visualization_tool_ic95_sim", "IC95_MR_adjsuted.xlsx"))

# Nombre de décès
  export(deaths, here("results", "visualization_tool_ic95_sim", "deaths.csv"))

# Nombre de décès évités
  export(avoided_deaths, here("results", "visualization_tool_ic95_sim", "avoided_deaths.csv"))

# Nombre total de décès évités par annnée
# Sur toute la période du modèle
  export(simulations_summary_avoided_deaths, here("results", "visualization_tool_ic95_sim", "IC95_total_avoided_deaths.xlsx"))
  ggsave(here("results", "visualization_tool_ic95_sim", "total_avoided_deaths.pdf"), plot = graph_total_avoided_deaths)
  ggsave(here("results", "visualization_tool_ic95_sim", "total_avoided_deaths_facet.pdf"), plot = graph_total_avoided_deaths_facet)

# Sur la période de changement de régime
  export(simulations_summary_avoided_deaths_shift, here("results", "visualization_tool_ic95_sim", "IC95_total_avoided_deaths_shift.xlsx"))
  ggsave(here("results", "visualization_tool_ic95_sim", "total_avoided_deaths_shift.pdf"), plot = graph_total_avoided_deaths_shift)
  ggsave(here("results", "visualization_tool_ic95_sim", "total_avoided_deaths_shift_facet.pdf"), plot = graph_total_avoided_deaths_shift_facet)

# EN 2040, 2050 et 2060
  ggsave(here("results", "visualization_tool_ic95_sim", "total_avoided_deaths_dates.pdf"), plot = graph_avoided_deaths_dates)

# Contributions de chaque aliment au résultat
  export(contrib, here("results", "visualization_tool_ic95_sim", "contributions.xlsx"))
  
  ggsave(here("results", "visualization_tool_ic95_sim", "FG_contributions.pdf"), plot = graph_contrib_fg)
