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
    export(diets_evo, here("results", "FADNES_2022_repro", "diets", "diets_rr_evo.csv"))
    ggsave(here("results", "FADNES_2022_repro", "diets", "diets_evo.pdf"), graph_diets_evo)
    ggsave(here("results", "FADNES_2022_repro", "diets", "diets_evo_shift.pdf"), graph_diets_evo_shift)

# Variations (%)
    export(diets_var, here("results", "FADNES_2022_repro", "diets", "diets_rr_var.csv"))
    ggsave(here("results", "FADNES_2022_repro", "diets", "diets_var.pdf"), graph_diets_var)

# Tableau des variations
    save_as_image(table_diets_var, here("results", "diets", "table_diets_var.png"))
