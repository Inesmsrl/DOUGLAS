################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
    rio, # Importation de fichiers
    here, # Localisation des fichiers dans le dossier du projet
    dplyr, # Manipulation des données
    tidyr, # Manipulation des données
    tidyverse, # Data management, inclus ggplot
    flextable # Tableaux
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Expositions : régimes SISAE en 2050
diets <- import(here("data", "DOUGLAS_diets.xlsx"))

################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Bornes temporelles des changements de régime alimentaire (années)
year_i <- 2025 # Année initiale
year_f <- 2050 # Année finale

# Dynamique d'implémentation des régimes (immediate, linear, cosine, sigmoidal)
implementation <- "cosine"

# paramètre de la courbe d'interpolation cosinus
p <- 1

# paramètre de la courbe sigmoïdale
lambda <- 8

# Durée du time to full effect (années)
ttfe_time <- 10

################################################################################################################################
#                                             4. Charte graphique                                                              #
################################################################################################################################

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
    "meat3" = "Omnivore-1",
    "meat2" = "Omnivore-2",
    "meat1" = "Flexitarian",
    "pesce" = "Pescetarian",
    "vege" = "Vegetarian",
    "vegan" = "Vegan",
    "meat3_optim" = "Omnivore-1 optimized",
    "meat2_optim" = "Omnivore-2 optimized",
    "meat1_optim" = "Flexitarian optimized",
    "pesce_optim" = "Pescetarian optimized",
    "vege_optim" = "Vegetarian optimized",
    "vegan_optim" = "Vegan optimized",
    "actuel" = "Current diet",
    "actuel_calage" = "Current diet (calibrated)",
    "sc0" = "Tendancial",
    "sc1" = "Scenario 1",
    "sc2" = "Scenario 2",
    "sc3" = "Scenario 3",
    "sc4" = "Scenario 4",
    "sc5" = "SNBC"
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
#                                             4. Fonctions d'implémentation des régimes                                        #
################################################################################################################################

# Implémentation linéaire
calc_food_q_lin <- function(q_i, q_f, year_n, year_i, year_f) {
    (q_f - q_i) / (year_f - year_i) * (year_n - year_f) + q_f
}

# Implémentation par interpolation cosinus
calc_food_q_cos <- function(q_i, q_f, year_n, year_i, year_f, p) {
    q_i + (q_f - q_i) * (1 - cos(pi * ((year_n - year_i) / (year_f - year_i))^p)) / 2
}

# Implémentation sigmoïdale
calc_food_q_sig <- function(q_i, q_f, year_n, year_i, year_f, lambda) {
    (q_i + q_f) / 2 + (q_f - q_i) * (1 / (1 + exp(-lambda * ((year_n - year_i) / (year_f - year_i) - 1 / 2))) - 1 / 2) * (-1 / (2 * (1 / (1 + exp(lambda / 2)) - 1 / 2)))
}

################################################################################################################################
#                                             5. Evolution des régimes                                                         #
################################################################################################################################

# Calcul des quantités de chaque aliment consommées chaque année
diets_evo <- diets %>%
    select("food_group", "actuel", "sc1", "sc2", "sc3", "sc4") %>%
    filter(food_group %in% c(
        "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
        "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
        "sugar_sweetened_beverages"
    )) %>%
    mutate(q_i = actuel) %>%
    pivot_longer(
        cols = c("actuel", "sc1", "sc2", "sc3", "sc4"),
        names_to = "scenario",
        values_to = "q_f"
    ) %>%
    crossing(year_n = (year_i - 2 * ttfe_time):(year_f + 2 * ttfe_time)) %>%
    mutate(quantity = case_when(
        implementation == "immediate" & year_n < year_i ~ q_i,
        implementation == "immediate" & year_n >= year_i ~ q_f,
        implementation == "linear" & year_n < year_i ~ q_i,
        implementation == "linear" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_lin, q_i, q_f, year_n, year_i, year_f),
        implementation == "linear" & year_n > year_f ~ q_f,
        implementation == "cosine" & year_n < year_i ~ q_i,
        implementation == "cosine" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_cos, q_i, q_f, year_n, year_i, year_f, p),
        implementation == "cosine" & year_n > year_f ~ q_f,
        implementation == "sigmoidal" & year_n < year_i ~ q_i,
        implementation == "sigmoidal" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_sig, q_i, q_f, year_n, year_i, year_f, lambda),
        implementation == "sigmoidal" & year_n > year_f ~ q_f
    )) %>%
    select("food_group", "scenario", "year_n", "quantity") %>%
    rename("year" = "year_n")

# Ordonnner les groupes alimentaires
diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)

# Visualisation graphique des consommations sur toute la période
graph_diets_evo <- ggplot(
  data = diets_evo %>%
    filter(scenario != "actuel"),
  aes(
    x = year,
    y = quantity,
    fill = food_group
  )
) +
  geom_area(colour = "black", linewidth = 0.2, alpha = 0.6) +
  facet_wrap(~scenario,
    ncol = 2,
    labeller = labeller(scenario = labels_scenario)
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = rel(0.8)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 12),
    legend.key.size = unit(0.3, "cm"),
    plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")
  ) +
  scale_fill_manual(
    values = col_food_groups,
    labels = labels_food_groups
  ) +
  labs(
    title = "",
    x = "",
    y = "Intakes (g/d/pers)",
    fill = "Food type"
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    title.position = "top",
    title.hjust = 0.5
  ))

plot(graph_diets_evo)
diets_evo_shift <- diets_evo %>%
  filter(year %in% c(year_i:year_f))

# Visualisation graphique des consommations sur la période de changement de régime
graph_diets_evo_shift <- ggplot(data = diets_evo_shift, aes(
  x = year,
  y = quantity,
  fill = food_group
)) +
  geom_area(colour = "black", linewidth = 0.2, alpha = 0.6) +
  facet_wrap(~scenario,
    ncol = 3,
    labeller = labeller(scenario = labels_scenario)
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = rel(0.8)),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.3, "cm"),
    plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")
  ) +
  scale_fill_manual(
    values = col_food_groups,
    labels = labels_food_groups
  ) +
  labs(
    title = "",
    x = "",
    y = "Intakes (g/d/pers)",
    fill = "Food type"
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    title.position = "top",
    title.hjust = 0.5
  ))

################################################################################################################################
#                                             Régimes observés vers optimisés                                                  #
################################################################################################################################

function_diets_evo <- function(observed, optimized) {
  
  diets_evo <- diets %>%
    select("food_group", all_of(c(observed, optimized))) %>%
    filter(food_group %in% c(
      "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
      "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
      "sugar_sweetened_beverages"
    )) %>%
    mutate(q_i = .[[observed]]) %>%
    pivot_longer(
      cols = all_of(c(observed, optimized)),
      names_to = "scenario",
      values_to = "q_f"
    ) %>%
    crossing(year_n = (year_i - 2 * ttfe_time):(year_f + 2 * ttfe_time)) %>%
    mutate(quantity = case_when(
      implementation == "immediate" & year_n < year_i ~ q_i,
      implementation == "immediate" & year_n >= year_i ~ q_f,
      implementation == "linear" & year_n < year_i ~ q_i,
      implementation == "linear" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_lin, q_i, q_f, year_n, year_i, year_f),
      implementation == "linear" & year_n > year_f ~ q_f,
      implementation == "cosine" & year_n < year_i ~ q_i,
      implementation == "cosine" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_cos, q_i, q_f, year_n, year_i, year_f, p),
      implementation == "cosine" & year_n > year_f ~ q_f,
      implementation == "sigmoidal" & year_n < year_i ~ q_i,
      implementation == "sigmoidal" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_sig, q_i, q_f, year_n, year_i, year_f, lambda),
      implementation == "sigmoidal" & year_n > year_f ~ q_f
    )) %>%
    select("food_group", "scenario", "year_n", "quantity") %>%
    rename("year" = "year_n")

  return(diets_evo)
}

diets_evo_meat3 <- function_diets_evo("meat3", "meat3_optim")
diets_evo_meat2 <- function_diets_evo("meat2", "meat2_optim")
diets_evo_meat1 <- function_diets_evo("meat1", "meat1_optim")
diets_evo_pesce <- function_diets_evo("pesce", "pesce_optim")
diets_evo_vege <- function_diets_evo("vege", "vege_optim")
diets_evo_vegan <- function_diets_evo("vegan", "vegan_optim")

plot_diets_evo <- function(observed, optimized) {
  
  diets_evo <- function_diets_evo(observed, optimized)

  diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)

  graph_diets_evo <- ggplot(
    data = diets_evo %>%
      filter(scenario != observed,
             year %in% c(year_i:year_f)),
    aes(
      x = year,
      y = quantity,
      fill = food_group
    )
  ) +
    geom_area(colour = "black", linewidth = 0.2, alpha = 0.6) +
    facet_wrap(~scenario,
      ncol = 2,
      labeller = labeller(scenario = labels_scenario)
    ) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(face = "bold", size = rel(0.8)),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold", size = 12),
      legend.key.size = unit(0.3, "cm"),
      plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")
    ) +
    scale_fill_manual(
      values = col_food_groups,
      labels = labels_food_groups
    ) +
    labs(
      title = "",
      x = "",
      y = "Intakes (g/d/pers)",
      fill = "Food type"
    ) +
    guides(fill = guide_legend(
      nrow = 2,
      title.position = "top",
      title.hjust = 0.5
    ))

  return(graph_diets_evo)
}

graph_diets_evo_meat3 <- plot_diets_evo("meat3", "meat3_optim")
graph_diets_evo_meat2 <- plot_diets_evo("meat2", "meat2_optim")
graph_diets_evo_meat1 <- plot_diets_evo("meat1", "meat1_optim")
graph_diets_evo_pesce <- plot_diets_evo("pesce", "pesce_optim")
graph_diets_evo_vege <- plot_diets_evo("vege", "vege_optim")
graph_diets_evo_vegan <- plot_diets_evo("vegan", "vegan_optim")

function_diets_var <- function(observed, optimized) {
  
  diets_var <- function_diets_evo(observed, optimized) %>%
    group_by(food_group, year) %>%
    mutate(var = (quantity - quantity[scenario == observed]) / quantity[scenario == observed] * 100)

  return(diets_var)
}

diets_var_meat3 <- function_diets_var("meat3", "meat3_optim")
diets_var_meat2 <- function_diets_var("meat2", "meat2_optim")
diets_var_meat1 <- function_diets_var("meat1", "meat1_optim")
diets_var_pesce <- function_diets_var("pesce", "pesce_optim")
diets_var_vege <- function_diets_var("vege", "vege_optim")
diets_var_vegan <- function_diets_var("vegan", "vegan_optim")

table_diets_var <- diets_var %>%
  filter(
    year %in% c(2025, 2050),
    scenario %in% c("actuel", "sc1")
  ) %>%
  mutate(quantity = round(quantity, 1), var = round(var, 1)) %>%
  pivot_wider(
    names_from = c("scenario", "year"),
    values_from = c("quantity", "var")
  ) %>%  
  select(
    "food_group",
    "quantity_actuel_2025",
    "quantity_sc1_2050", "var_sc1_2050"
  ) %>%  
  mutate(food_group = labels_food_groups[food_group]) %>% # Remplacer les noms des groupes alimentaires par les labels)
  qflextable() %>% # Création du tableau et ajustement automatique de la largeur des colonnes
  add_header_row(
    top = TRUE, # Ajout d'une ligne d'en-tête
    values = c("Food group", "Observed", "Optimized", "")
  ) %>%
  set_header_labels( # Renommer des colonnes de la 2e ligne d'en-tête
    "food_group" = "",
    "quantity_actuel_2025" = "Intake(g/d/pers)",
    "quantity_sc1_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc1_2050" = "Intake variation vs observed in 2050 (%)"
  ) %>%
  vline(part = "all", j = 2) %>% # Ligne verticale après la colonne 2
  merge_at(i = 1, j = 3:4, part = "header") %>% # Fusion des cellules de la 1ère ligne d'en-tête
  align(align = "center", j = c(2:4), part = "all") %>% # Centrer le contenu des cellules sauf Food group
  bold(i = 1, part = "header") %>% # Mettre en gras la 1ère ligne d'en-tête
  bg(part = "all", bg = "white") # Fond blanc pour toutes les cellules

table_diets_var <- function(observed, optimized) {
  
  diets_var <- function_diets_var(observed, optimized)
  opt <- optimized[1]  # Prendre un seul nom de scénario pour l'affichage
  
  # Construction des noms de colonnes dynamiques
  col_obs_q <- paste0("quantity_", observed, "_2025")
  col_opt_q <- paste0("quantity_", opt, "_2050")
  col_opt_v <- paste0("var_", opt, "_2050")
  
  # Construction du tableau
  table <- diets_var %>%
    filter(
      year %in% c(2025, 2050),
      scenario %in% c(observed, opt)
    ) %>%
    mutate(quantity = round(quantity, 1), var = round(var, 1)) %>%
    pivot_wider(
      names_from = c("scenario", "year"),
      values_from = c("quantity", "var")
    ) %>%
    select(
      "food_group",
      all_of(c(col_obs_q, col_opt_q, col_opt_v))
    ) %>%
    mutate(food_group = labels_food_groups[food_group])
  
  # Vérification des noms de colonnes du tableau pour éviter les erreurs
  # print(colnames(table))  # Décommente si tu veux vérifier les noms de colonnes
  
  
  # Créer le tableau avec qflextable et ajouter l'en-tête
  table_ft <- table %>%
    qflextable() %>%
    add_header_row(
      top = TRUE,
      values = c("Food group", "Observed", "Optimized", ""),  # 4 valeurs pour l'en-tête
      colwidths = c(1, 1, 1, 1)  # Total = 4 colonnes (adapter si nécessaire)
    ) %>%
    vline(part = "all", j = 2) %>%
    merge_at(i = 1, j = 3:4, part = "header") %>%
    align(align = "center", j = 2:4, part = "all") %>%
    bold(i = 1, part = "header") %>%
    bg(part = "all", bg = "white")
  
  return(table_ft)
}

table_var_meat3 <- table_diets_var("meat3", "meat3_optim")
table_var_meat2 <- table_diets_var("meat2", "meat2_optim")
table_var_meat1 <- table_diets_var("meat1", "meat1_optim")
table_var_pesce <- table_diets_var("pesce", "pesce_optim")
table_var_vege <- table_diets_var("vege", "vege_optim")
table_var_vegan <- table_diets_var("vegan", "vegan_optim")


################################################################################################################################
#                                             5. Variations des consommations par rapport au baseline                          #
################################################################################################################################

# Calcul des variations de consommation de chaque aliment par rapport au régime actuel (%)
diets_var <- diets_evo %>%
    group_by(food_group, year) %>%
    mutate(var = (quantity - quantity[scenario == "actuel"]) / quantity[scenario == "actuel"] * 100)

# Visualisation graphique des variations de consommation sur toute la période
graph_diets_var <- ggplot(
  diets_var %>%
    filter(scenario != "actuel"),
  aes(
    x = year,
    y = var,
    color = food_group
  )
) +
  facet_wrap(~scenario,
    labeller = labeller(scenario = labels_scenario)
  ) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  labs(
    title = "",
    x = "",
    y = "Variations of food intake (%)",
    color = "Food group"
  ) +
  scale_color_manual(
    values = col_food_groups,
    labels = labels_food_groups
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = rel(1)),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(
    nrow = 3,
    title.position = "top",
    title.hjust = 0.5
  ))


# Tableau des variations de consommation

diets_var$food_group <- factor(diets_var$food_group, levels = order_food_groups)

table_diets_var <- diets_var %>%
  filter(
    year %in% c(2025, 2035, 2050)
  ) %>%
  mutate(quantity = round(quantity, 1), var = round(var, 1)) %>%
  pivot_wider(
    names_from = c("scenario", "year"),
    values_from = c("quantity", "var")
  ) %>%
  select(
    "food_group",
    "quantity_actuel_2025",
    "quantity_sc1_2050", "var_sc1_2035",, "var_sc1_2050",
    "quantity_sc2_2050", "var_sc2_2035", "var_sc2_2050",
    "quantity_sc3_2050", "var_sc3_2035", "var_sc3_2050",
    "quantity_sc4_2050", "var_sc4_2035", "var_sc4_2050"
  ) %>%
  mutate(food_group = labels_food_groups[food_group]) %>% # Remplacer les noms des groupes alimentaires par les labels)
  qflextable() %>% # Création du tableau et ajustement automatique de la largeur des colonnes
  add_header_row(
    top = TRUE, # Ajout d'une ligne d'en-tête
    values = c("Food group", "Baseline", "S1", "", "", "S2", "", "", "S3", "", "", "S4", "", "")
  ) %>%
  set_header_labels( # Renommer des colonnes de la 2e ligne d'en-tête
    "food_group" = "",
    "quantity_actuel_2025" = "Intake(g/d/pers)",
    "quantity_sc1_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc1_2035" = "Intake variation vs baseline in 2035 (%)",
    "var_sc1_2050" = "Intake variation vs baseline in 2050 (%)",
    "quantity_sc2_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc2_2035" = "Intake variation vs baseline in 2035 (%)",
    "var_sc2_2050" = "Intake variation vs baseline in 2050 (%)",
    "quantity_sc3_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc3_2035" = "Intake variation vs baseline in 2035 (%)",
    "var_sc3_2050" = "Intake variation vs baseline in 2050 (%)",
    "quantity_sc4_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc4_2035" = "Intake variation vs baseline in 2035 (%)",
    "var_sc4_2050" = "Intake variation vs baseline in 2050 (%)"
  ) %>%
  vline(part = "all", j = 2) %>% # Ligne verticale après la colonne 2
  vline(part = "all", j = 5) %>% # Ligne verticale après la colonne 6
  vline(part = "all", j = 8) %>% # Ligne verticale après la colonne 9
  vline(part = "all", j = 11) %>% # Ligne verticale après la colonne 13
  merge_at(i = 1, j = 3:5, part = "header") %>% # Fusion des cellules de la 1ère ligne d'en-tête
  merge_at(i = 1, j = 6:8, part = "header") %>%
  merge_at(i = 1, j = 9:11, part = "header") %>%
  merge_at(i = 1, j = 12:14, part = "header") %>%
  align(align = "center", j = c(2:14), part = "all") %>% # Centrer le contenu des cellules sauf Food group
  bold(i = 1, part = "header") %>% # Mettre en gras la 1ère ligne d'en-tête
  bg(part = "all", bg = "white") # Fond blanc pour toutes les cellules

################################################################################################################################
#                                             6. Exportation des données                                                      #
################################################################################################################################

# Implémentation des régimes
# Quantités (g/j/pers)
    export(diets_evo, here("results", "TEST", "diets", "diets_evo.csv"))
    ggsave(here("results", "TEST", "diets", "diets_evo.pdf"), graph_diets_evo)
    ggsave(here("results", "TEST", "diets", "diets_evo_shift.pdf"), graph_diets_evo_shift)

# Variations (%)
    export(diets_var, here("results", "TEST", "diets", "diets_var.csv"))
    ggsave(here("results", "TEST", "diets", "diets_var.pdf"), graph_diets_var)

# Tableau des variations
    save_as_image(table_diets_var, here("results", "TEST", "diets", "table_diets_var.png"))


# Régimes observés vers optimisés

  # Quantités (g/j/pers)
    export(diets_evo_meat3, here("results", "Observed_to_Optimized", "meat3", "diets", "diets_evo_meat3.csv"))
    ggsave(here("results", "Observed_to_Optimized", "meat3", "diets", "diets_evo_meat3.pdf"), graph_diets_evo_meat3)

    export(diets_evo_meat2, here("results", "Observed_to_Optimized", "meat2", "diets", "diets_evo_meat2.csv"))
    ggsave(here("results", "Observed_to_Optimized", "meat2", "diets", "diets_evo_meat2.pdf"), graph_diets_evo_meat2)

    export(diets_evo_meat1, here("results", "Observed_to_Optimized", "meat1", "diets", "diets_evo_meat1.csv"))
    ggsave(here("results", "Observed_to_Optimized", "meat1", "diets", "diets_evo_meat1.pdf"), graph_diets_evo_meat1)

    export(diets_evo_pesce, here("results", "Observed_to_Optimized", "pesce", "diets", "diets_evo_pesce.csv"))
    ggsave(here("results", "Observed_to_Optimized", "pesce", "diets", "diets_evo_pesce.pdf"), graph_diets_evo_pesce)

    export(diets_evo_vege, here("results", "Observed_to_Optimized", "vege", "diets", "diets_evo_vege.csv"))
    ggsave(here("results", "Observed_to_Optimized", "vege", "diets", "diets_evo_vege.pdf"), graph_diets_evo_vege)

    export(diets_evo_vegan, here("results", "Observed_to_Optimized", "vegan", "diets", "diets_evo_vegan.csv"))
    ggsave(here("results", "Observed_to_Optimized", "vegan", "diets", "diets_evo_vegan.pdf"), graph_diets_evo_vegan)

  # Variations (%)
    export(diets_var_meat3, here("results", "Observed_to_Optimized", "meat3", "diets", "diets_var_meat3.csv"))
    save_as_image(table_var_meat3, here("results", "Observed_to_Optimized", "meat3", "diets", "table_var_meat3.png"))
    
    export(diets_var_meat2, here("results", "Observed_to_Optimized", "meat2", "diets", "diets_var_meat2.csv"))
    save_as_image(table_var_meat2, here("results", "Observed_to_Optimized", "meat2", "diets", "table_var_meat2.png"))

    export(diets_var_meat1, here("results", "Observed_to_Optimized", "meat1", "diets", "diets_var_meat1.csv"))
    save_as_image(table_var_meat1, here("results", "Observed_to_Optimized", "meat1", "diets", "table_var_meat1.png"))

    export(diets_var_pesce, here("results", "Observed_to_Optimized", "pesce", "diets", "diets_var_pesce.csv"))
    save_as_image(table_var_pesce, here("results", "Observed_to_Optimized", "pesce", "diets", "table_var_pesce.png"))

    export(diets_var_vege, here("results", "Observed_to_Optimized", "vege", "diets", "diets_var_vege.csv"))
    save_as_image(table_var_vege, here("results", "Observed_to_Optimized", "vege", "diets", "table_var_vege.png"))

    export(diets_var_vegan, here("results", "Observed_to_Optimized", "vegan", "diets", "diets_var_vegan.csv"))
    save_as_image(table_var_vegan, here("results", "Observed_to_Optimized", "vegan", "diets", "table_var_vegan.png"))
