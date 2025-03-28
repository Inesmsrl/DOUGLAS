################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
    rio, # Importation de fichiers
    here, # Localisation des fichiers dans le dossier du projet
    dplyr, # Manipulation des données
    tidyr, # Manipulation des données
    tidyverse # Contient ggplot
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Risques relatifs / consommation (g/j), relations dose-réponse simulées
rr_table <- import(here("data_clean", "rr_table_interpolated_sim.csv"))

# Expositions : régimes au cours du temps
diets_evo <- import(here("results", "FADNES_2024_repro", "diets", "diets_rr_evo.csv"))

################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Bornes temporelles des changements de régime alimentaire (années)
year_i <- 2019 # Année initiale
year_f <- 2039 # Année finale

# Paramètre de modification d'effet des RR
# 0.5 à 1 = réduction d'effet, modèle conservateur
# 1 à 1.5 = augmentation d'effet, modèle radical
m <- 0.75

#  Time to full effect
# durée (années)
ttfe_time <- 20

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

################################################################################################################################
#                                             4. Charte graphique                                                              #
################################################################################################################################

# Couleur de chaque scénario
col_scenario <- c(
  "actuel" = "azure4",
  "sc0" = "palevioletred3",
  "sc1" = "#699cc2",
  "sc2" = "#974175",
  "sc3" = "#50cd9f",
  "sc4" = "#cb6c2d",
  "sc5" = "royalblue4"
)

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

# Ordonner les groupes alimentaires
order_food_groups <- c(
  "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
  "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
  "added_plant_oils", "sugar_sweetened_beverages"
)

# Etiquettes des scénarios
labels_scenario <- c(
  "actuel" = "Current diet",
  "sc1" = "Scenario 1",
  "sc2" = "Scenario 2",
  "sc3" = "Scenario 3",
  "sc4" = "Scenario 4"
)

# Etiquettes des groupes alimentaires
labels_food_groups <- c(
  "red_meat" = "Red meat",
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
  "sugar_sweetened_beverages" = "SSB"
)

################################################################################################################################
#                                             5. Préparation des données                                                       #
################################################################################################################################

# Renommer la variable RR
rr_table <- rr_table %>%
  rename("rr" = "rr_interpolated")

################################################################################################################################
#                                             6. Modification d'effet des RR                                                   #
################################################################################################################################

# Application du modification d'effet des RR (m)
rr_table <- rr_table %>%
  mutate(rr_a = case_when(
    rr < 1 ~ rr + (1 - rr) * (1 - m),
    rr >= 1 ~ 1 / (m / rr + 1 - m)
  )) %>%
  select("simulation_id", "food_group", "quantity", "rr_a") %>%
  rename("rr" = "rr_a")

################################################################################################################################
#                                             7. Attribution des RR à chaque régime                                            #
################################################################################################################################

# Quantités arrondies à l'unité pour matcher avec le tableau des RR
diets_evo <- diets_evo %>%
  mutate(quantity = round(quantity)) %>%
  left_join(rr_table, by = c("food_group", "quantity"), relationship = "many-to-many")


# Calculer la valeur centrales et les IC95 pour chaque année
simulations_summary <- diets_evo %>%
  group_by(food_group, scenario, year, quantity) %>%
  summarise(
    mean_rr = mean(rr, na.rm = TRUE), # Moyenne des simulations
    lower_ci = quantile(rr, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(rr, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             8. TTFE                                                                          #
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
    ttfe_dynamics == "linear" ~ time / ttfe_time,
    ttfe_dynamics == "cosine" ~ (1 - cos(pi * (time / ttfe_time)^p_ttfe)) / 2,
    ttfe_dynamics == "sigmoidal" ~ (1 / (1 + exp(-lambda_ttfe * (time / ttfe_time - 1 / 2))) - 1 / (1 + exp(lambda_ttfe / 2))) /
      (1 - 2 / (1 + exp(lambda_ttfe / 2))),
    ttfe_dynamics == "log" ~ log(1 + eta_ttfe * time / ttfe_time) / log(1 + eta_ttfe),
    TRUE ~ NA_real_
  ))

################################################################################################################################
#                                             9. Calcul des RR avec TTFE                                                       #
################################################################################################################################

# Calcul de la valeur des RR sur la durée du TTFE
# Après la fin du TTFE : RR = NA
diets_evo <- diets_evo %>%
  rowwise() %>%
  mutate(year_n = list(seq(from = (year_i - 2 * ttfe_time), to = (year_f + 2 * ttfe_time)))) %>%
  unnest(year_n) %>%
  mutate(rr_n = case_when(
    year_n < year ~ NA_real_,
    year_n >= year & year_n <= year + max(ttfe$time) ~ rr * ttfe$ttfe[match(year_n - year, ttfe$time)],
    year_n > year + max(ttfe$time) ~ NA_real_
  )) %>%
  ungroup()

################################################################################################################################
#                                             10. Combinaison des RR de chaque aliment par année                                #
################################################################################################################################

# Le RR d'un aliment une année n est la moyenne des RR de cet aliment générés avec le TTFE pour cette année
rr_evo_food_combined <- diets_evo %>%
  group_by(scenario, year_n, food_group, simulation_id) %>%
  summarize(
    mean_rr = sum(rr_n, na.rm = TRUE) / sum(ttfe$ttfe[match(pmin(year_n - year, ttfe_time), ttfe$time)], na.rm = TRUE), # pmin compare la valeur year_n-year à ttfe_time et choisit la plus faible
    .groups = "drop"
  )

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_rr_fg_combined <- rr_evo_food_combined %>%
  group_by(food_group, scenario, year_n) %>%
  summarise(
    combined_rr = mean(mean_rr, na.rm = TRUE),
    lower_ci = quantile(mean_rr, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(mean_rr, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             11. Figures : RR des groupes alimentaires                                         #
################################################################################################################################


graph_rr_fg <- function (scen) {
  ggplot(
    simulations_summary_rr_fg_combined %>%
      filter(scenario == scen),
    aes(
      x = year_n,
      y = combined_rr,
      color = food_group
    )
  ) +
    facet_wrap(~food_group,
      labeller = labeller(food_group = labels_food_groups)
    ) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = food_group), alpha = 0.5, linetype = 0) + # Intervalle de confiance
    geom_line(linewidth = 1, na.rm = TRUE) + # Moyenne en trait plein
    labs(
      title = "",
      x = "",
      y = "RR"
    ) +
    scale_color_manual(values = col_food_groups) +
    scale_fill_manual(values = col_food_groups) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      strip.text = element_text(face = "bold", size = rel(0.5)),
      legend.position = "none"
    )
}

graph_rr_fg_sc1 <- graph_rr_fg("sc1")
graph_rr_sc2 <- graph_rr_fg("sc2")
graph_rr_sc3 <- graph_rr_fg("sc3")
graph_rr_sc4 <- graph_rr_fg("sc4")

################################################################################################################################
#                                             12. Combinaison des RR de chaque régime par année                                #
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
    lower_ci = quantile(combined_rr, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(combined_rr, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             13. Figures : RR des régimes                                                      #
################################################################################################################################

graph_rr_diets <- ggplot(simulations_summary_rr_diets, aes(
  x = year,
  y = mean_rr,
  color = scenario
)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0) +
  facet_wrap(~scenario,
    labeller = labeller(scenario = labels_scenario)
  ) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  labs(
    title = "",
    x = "",
    y = "RR"
  ) +
  scale_color_manual(values = col_scenario) +
  scale_fill_manual(values = col_scenario) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = rel(0.5)),
    legend.position = "none"
  )
################################################################################################################################
#                                             14. RR relatif au RR actuel                                                      #
################################################################################################################################

# Calcul des RR des régimes complets relatifs aux RR du scénario actuel
rr_evo_diets <- rr_evo_diets %>%
  group_by(year, simulation_id) %>%
  mutate(relative_rr = combined_rr / combined_rr[scenario == "actuel"]) %>%
  ungroup()

# Tant que l'implémentation des régimes n'a pas commencé, le RR relatif est égal à 1
rr_evo_diets <- rr_evo_diets %>%
  mutate(relative_rr = case_when(
    year == year_i - 2 * ttfe_time ~ 1,
    TRUE ~ relative_rr
  ))

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_rr_diets_relative <- rr_evo_diets %>%
  group_by(scenario, year) %>%
  summarise(
    mean_rr = mean(relative_rr, na.rm = TRUE),
    lower_ci = quantile(relative_rr, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(relative_rr, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             15. Figures : RR des régimes relatifs au baseline                                  #
################################################################################################################################


graph_rr_diets_rel <- ggplot(
  simulations_summary_rr_diets_relative %>%
    filter(scenario != "actuel"),
  aes(
    x = year,
    y = mean_rr,
    color = scenario
  )
) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0) +
  geom_line(size = 0.6, na.rm = TRUE) +
  labs(
    title = "",
    x = "",
    y = "RR",
    color = "Scenario",
    fill = "Scenario"
  ) +
  scale_color_manual(
    values = col_scenario,
    labels = labels_scenario
  ) +
  scale_fill_manual(
    values = col_scenario,
    labels = labels_scenario
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = rel(1)),
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(title = NULL),
    fill = guide_legend(title = NULL)
  )

################################################################################################################################
#                                             16. Exportation des données                                                      #
################################################################################################################################

# RR des groupes alimentaires
export(rr_evo_food_combined, here("results", "FADNES_2024_repro", "RR", "rr_evo_fg.csv"))

ggsave(here("results", "FADNES_2024_repro", "RR", "rr_evo_fg_sc1.pdf"), graph_rr_fg_sc1)
ggsave(here("results", "FADNES_2024_repro", "RR", "rr_evo_fg_sc2.pdf"), graph_rr_sc2)
ggsave(here("results", "FADNES_2024_repro", "RR", "rr_evo_fg_sc3.pdf"), graph_rr_sc3)
ggsave(here("results", "FADNES_2024_repro", "RR", "rr_evo_fg_sc4.pdf"), graph_rr_sc4)


# RR des régimes
export(rr_evo_diets, here("results", "FADNES_2024_repro", "RR", "rr_evo_diets.csv"))

ggsave(here("results", "FADNES_2024_repro", "RR", "rr_evo_diets.pdf"), graph_rr_diets)
ggsave(here("results", "FADNES_2024_repro", "RR", "rr_evo_diets_rel.pdf"), graph_rr_diets_rel)
