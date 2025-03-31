################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
    rio, # Importation de fichiers
    here, # Localisation des fichiers dans le dossier du projet
    dplyr, # Manipulation des données
    tidyr, # Manipulation des données
    tidyverse, # Data management, inclus ggplot
    flextable, # Création de tableaux
    scales, # Transformations en pourcentages notamment
    forestploter
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# RR de chaque aliment
rr_evo_food_combined <- import(here("results", "1_Main_analysis_newDRF", "RR", "rr_evo_fg.csv"))

# Variations des consommations alimentaires
diets_var <- import(here("results", "diets", "diets_rr_var.csv"))

# Décès totaux par scénario
simulations_summary_total_deaths <- import(here("results", "1_Main_analysis_newDRF", "HIA", "IC95_tot_deaths.xlsx"))

################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Bornes temporelles des changements de régime alimentaire (années)
year_i <- 2025 # Année initiale
year_f <- 2050 # Année finale

#  Time to full effect
# durée (années)
ttfe_time <- 10

################################################################################################################################
#                                             4. Charte graphique                                                              #
################################################################################################################################

# Ordonner les groupes alimentaires
order_food_groups <- c(
  "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
  "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
  "added_plant_oils", "sugar_sweetened_beverages"
)

# Etiquettes des groupes d'aliments
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

# Etiquettes des scénarios
labels_scenario <- c(
  "actuel" = "Current diet",
  "sc1" = "Scenario 1",
  "sc2" = "Scenario 2",
  "sc3" = "Scenario 3",
  "sc4" = "Scenario 4"
)
################################################################################################################################
#                                             5. RR des aliments relatifs au baseline                                         #
################################################################################################################################

# RR de chaque aliment/année relatif au RR du scénario baseline
rr_fg_relative <- rr_evo_food_combined %>%
  group_by(year_n, food_group, simulation_id) %>%
  mutate(rr_fg_relative = mean_rr / mean_rr[scenario == "actuel"]) %>%
  ungroup()

# Tant que l'implémentation des régimes n'a pas commencé, le RR relatif est égal à 1
rr_fg_relative <- rr_fg_relative %>%
  mutate(rr_fg_relative = case_when(
    year_n == year_i - 2 * ttfe_time ~ 1,
    TRUE ~ rr_fg_relative
  ))

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_rr_fg_relative <- rr_fg_relative %>%
  group_by(scenario, year_n, food_group) %>%
  summarise(
    mean_rr_fg = mean(rr_fg_relative, na.rm = TRUE),
    lower_ci = quantile(rr_fg_relative, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(rr_fg_relative, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             6. Contributions des aliments au résultat                                        #
################################################################################################################################

# Calcul de la contribution au résultat de la variation de consommation de chaque aliment par rapport au baseline
contrib <- simulations_summary_rr_fg_relative %>%
  mutate(
    delta = -(1 - mean_rr_fg) * 100,
    delta_low = -(1 - lower_ci) * 100,
    delta_upp = -(1 - upper_ci) * 100
  ) %>%
  select(scenario, year_n, food_group, delta, delta_low, delta_upp) %>%
  rename("year" = "year_n")

################################################################################################################################
#                                             7. Forest plots                                                                  #
################################################################################################################################

# Forestplot
forest_plot_contrib <- function(scen) {
  contrib_scen <- contrib %>%
    filter(
      scenario == scen,
      year == 2050
    ) %>%
    mutate(food_group = labels_food_groups[food_group])

  diets_var_scen <- diets_var %>%
    filter(
      scenario == scen,
      year == 2050
    ) %>%
    mutate(
      food_group = labels_food_groups[food_group],
      var = round(var, 1)
    )

  forest(
    data = setNames(
      data.frame(diets_var_scen$food_group, diets_var_scen$var, ""),
      c("Food Group", "Intake variation (%)", "                                              ")
    ),
    est = contrib_scen$delta,
    lower = contrib_scen$delta_low,
    upper = contrib_scen$delta_upp,
    ci_column = 3,
    ref_line = 0,
    xlim = c(-8, 5),
    xlab = "Contribution to the health impact (%)",
    title = paste("2050 - ", labels_scenario[scen]),
    footnote = "SSB = Sugar-sweetened beverages",
    theme = forest_theme(
      core = list(fg_params = list(hjust = 0.5, x = 0.5)),
      colhead = list(fg_params = list(hjust = 0.5, x = 0.5)),
      #footnote_gp = gpar(cex = 0.6, fontface = "italic", col = "azure4"),
      ci_pch = 20,
      ci_alpha = 0.8
    )
  )
}

forest_sc1 <- forest_plot_contrib("sc1")
forest_sc2 <- forest_plot_contrib("sc2")
forest_sc3 <- forest_plot_contrib("sc3")
forest_sc4 <- forest_plot_contrib("sc4")

################################################################################################################################
#                                             8. Heat maps                                                                     #
################################################################################################################################
diets_var$food_group <- factor(diets_var$food_group, levels = order_food_groups)

hm_var <- ggplot(data = diets_var %>%
               filter(scenario != "actuel",
                      year == 2050),
             aes(x = scenario, y = food_group, fill = var)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_gradient2(low = "#002414", 
                       high = "#8b0066", 
                       mid = "white", 
                       midpoint = 0,
                       limits = c(min(diets_var$var, na.rm = TRUE), max(diets_var$var, na.rm = TRUE))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = rel(1.5)),
  axis.title.y = element_text(size = rel(1.5)), 
  legend.position = "right") +
  labs(x = "", y = "", fill = "Intake variation (%)") +
  ggtitle("Variations of the intake of several food groups\n compared to the baseline") +
  scale_x_discrete(labels = labels_scenario[diets_var$scenario]) +
  scale_y_discrete(labels = labels_food_groups[diets_var$food_group])

plot(hm_var)

contrib$food_group <- factor(contrib$food_group, levels = order_food_groups)

hm_contrib <- ggplot(data = contrib %>%
               filter(scenario != "actuel",
                      year == 2050),
             aes(x = scenario, y = food_group, fill = delta)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_gradient2(low = "#00388d", high = "#850028", mid = "#ffffff", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = rel(1.5)),
  axis.title.y = element_text(size = rel(1.5)), 
  legend.position = "right") +
  labs(x = "", y = "", fill = "Contribution to\nmortality (%)") +
  ggtitle("Contributions of the intake variation of several food groups\n to mortality in each scenario compared to the baseline ") +
  scale_x_discrete(labels = labels_scenario[diets_var$scenario]) +
  scale_y_discrete(labels = labels_food_groups[diets_var$food_group])

plot(hm_contrib)

################################################################################################################################
#                                             8. Tableau                                                                       #
################################################################################################################################

# Tableau des variations et contributions des aliments aux résultats pour l'année 2050

summary_contrib <- contrib %>%
  inner_join(simulations_summary_total_deaths, by = c("scenario", "year")) %>%
  inner_join(diets_var, by = c("scenario", "year", "food_group")) %>%
  group_by("scenario", "year", "food_group") %>%
  mutate(
    deaths = delta * mean_tot_deaths / 100,
    var = percent(var / 100, accuracy = 0.1),
    delta = percent(delta / 100, accuracy = 0.1)
  ) %>%
  ungroup() %>%
  select(scenario, year, food_group, quantity, var, delta, deaths) %>%
  mutate(
    quantity = round(quantity),
    deaths = round(deaths)
  )


# Contributions pour l'année 2050

contrib_2050 <- summary_contrib %>%
  filter(
    year == 2050,
    scenario != "actuel"
  ) %>%
  pivot_wider(
    names_from = "scenario",
    values_from = c("quantity", "var", "delta", "deaths")
  ) %>%
  select(-"year") %>%
  select(
    "food_group",
    "quantity_sc1", "var_sc1", "delta_sc1", "deaths_sc1",
    "quantity_sc2", "var_sc2", "delta_sc2", "deaths_sc2",
    "quantity_sc3", "var_sc3", "delta_sc3", "deaths_sc3",
    "quantity_sc4", "var_sc4", "delta_sc4", "deaths_sc4"
  ) %>%
  mutate(food_group = labels_food_groups[food_group]) %>% # Remplacer les noms des groupes alimentaires par les labels)
  qflextable() %>% # Création du tableau et ajustement automatique de la largeur des colonnes
  add_header_row(
    top = TRUE, # Ajout d'une ligne d'en-tête
    values = c("Food group", "S1", "", "", "", "S2", "", "", "", "S3", "", "", "", "S4", "", "", "")
  ) %>%
  set_header_labels( # Renommer des colonnes de la 2e ligne d'en-tête
    "food_group" = "",
    "quantity_sc1" = "Intake (g/d/pers)",
    "var_sc1" = "Intake variation vs baseline",
    "delta_sc1" = "Contribution to results",
    "deaths_sc1" = "Deaths due to intake variation",
    "quantity_sc2" = "Intake (g/d/pers)",
    "var_sc2" = "Intake variation vs baseline",
    "delta_sc2" = "Contribution to results",
    "deaths_sc2" = "Deaths due to intake variation",
    "quantity_sc3" = "Intake (g/d/pers)",
    "var_sc3" = "Intake variation vs baseline",
    "delta_sc3" = "Contribution to results",
    "deaths_sc3" = "Deaths due to intake variation",
    "quantity_sc4" = "Intake (g/d/pers)",
    "var_sc4" = "Intake variation vs baseline",
    "delta_sc4" = "Contribution to results",
    "deaths_sc4" = "Deaths due to intake variation"
  ) %>%
  vline(part = "all", j = 5) %>% # Ligne verticale après la colonne 5
  vline(part = "all", j = 9) %>% # Ligne verticale après la colonne 9
  vline(part = "all", j = 13) %>% # Ligne verticale après la colonne 13
  merge_at(i = 1, j = 2:5, part = "header") %>% # Fusion des cellules de la 1ère ligne d'en-tête
  merge_at(i = 1, j = 6:9, part = "header") %>%
  merge_at(i = 1, j = 10:13, part = "header") %>%
  merge_at(i = 1, j = 14:17, part = "header") %>%
  align(align = "center", j = c(2:17), part = "all") %>% # Centrer le contenu des cellules sauf Food group
  bold(i = 1, part = "header") %>% # Mettre en gras la 1ère ligne d'en-tête
  bg(part = "all", bg = "white") %>% # Fond blanc pour toutes les cellules
  bg(., i = ~ food_group %in% c("Legumes", "Nuts", "Processed meat", "Red meat", "Eggs", "Vegetables", "Whole grains"), part = "body", bg = "aquamarine2") %>%
  bg(., i = ~ food_group %in% c("Fruits", "Fish", "White meat"), part = "body", bg = "indianred1")

################################################################################################################################
#                                             11. Exportation des données                                                      #
################################################################################################################################

# Données
export(contrib, here("results", "1_Main_analysis_newDRF", "contributions", "FG_contributions.xlsx"))

# Forest plots
ggsave(here("results", "1_Main_analysis_newDRF", "contributions", "forest_sc1.pdf"), forest_sc1)
ggsave(here("results", "1_Main_analysis_newDRF", "contributions", "forest_sc2.pdf"), forest_sc2)
ggsave(here("results", "1_Main_analysis_newDRF", "contributions", "forest_sc3.pdf"), forest_sc3)
ggsave(here("results", "1_Main_analysis_newDRF", "contributions", "forest_sc4.pdf"), forest_sc4)

# Tableau contributions 2050
save_as_image(contrib_2050, here("results", "1_Main_analysis_newDRF", "contributions", "contributions_2050.png"))


# Heat maps
ggsave(here("results", "1_Main_analysis_newDRF", "contributions", "hm_contrib.pdf"), hm_contrib)
ggsave(here("results", "1_Main_analysis_newDRF", "diets", "hm_var.pdf"), hm_var)
 