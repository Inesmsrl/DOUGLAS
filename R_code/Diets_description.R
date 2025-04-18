################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
    rio, # Importation de fichiers
    here, # Localisation des fichiers dans le dossier du projet
    dplyr, # Manipulation des données
    tidyr, # Manipulation des données
    tidyverse, # Data management, inclus ggplot
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

diets <- import(here("data", "DOUGLAS_diets_actuel.xlsx"))

DOUGLAS_diets <- import(here("data", "DOUGLAS_diets.xlsx"))

################################################################################################################################
#                                             4. Charte graphique                                                              #
################################################################################################################################

# Couleur de chaque régime
col_diets <- c(
    "meat3" = "#cd0030",
    "meat2" = "#d95668",
    "meat1" = "#fcb901",
    "pesce" = "#027474",
    "vege" = "#007643",
    "vegan" = "#54a300",
    "meat3_optim" = "#98606d",
    "meat2_optim" = "#f4a1ac",
    "meat1_optim" = "#f1d68c",
    "pesce_optim" = "#5a8787",
    "vege_optim" = "#568470",
    "vegan_optim" = "#a3c084"
)

# Etiquettes des régimes

label_diets <- c(
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

# Ordre des régimes

order_diets <- c(
    "actuel",
    "actuel_calage",
    "meat3",
    "meat3_optim",
    "meat2",
    "meat2_optim",
    "meat1",
    "meat1_optim",
    "pesce",
    "pesce_optim",
    "vege",
    "vege_optim",
    "vegan",
    "vegan_optim",
    "sc0",
    "sc1",
    "sc2",
    "sc3",
    "sc4",
    "sc5"
)

# Etiquettes des scénarios
labels_scenario <- c(
  "actuel" = "Current diet",
  "sc1" = "Scenario 1",
  "sc2" = "Scenario 2",
  "sc3" = "Scenario 3",
  "sc4" = "Scenario 4"
)

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

# Ordonner les groupes alimentaires
order_food_groups <- c(
  "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
  "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
  "added_plant_oils", "sugar_sweetened_beverages"
)

################################################################################################################################
#                                             5. Répartition des régimes                                                       #
################################################################################################################################

diets_long <- diets %>%
  pivot_longer(
    cols = -diet,
    names_to = "scenario",
    values_to = "freq"
  ) %>%
  mutate(diet = factor(diet, levels = order_diets))

graph_diets_rep <- ggplot(diets_long %>% filter(scenario %in% c("actuel", "sc1", "sc2", "sc3", "sc4")),
                        aes(x = diet, y = freq, fill = diet)) +
    geom_bar(stat = "identity", width = 0.7) +
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
  )+
  scale_fill_manual(
    values = col_diets,
    labels = label_diets
  )+
  labs(
    title = "",
    x = "",
    y = "Proportion",
    fill = "diets"
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    title.position = "top",
    title.hjust = 0.5
  ))

plot(graph_diets_rep)

graph_diets_rep_2 <- ggplot(diets_long %>% filter(scenario %in% c("actuel", "sc1", "sc2", "sc3", "sc4")),
                        aes(x = scenario, y = freq, fill = diet)) +
    geom_bar(stat = "identity", width = 0.7)+
    scale_fill_manual(
    values = col_diets,
    labels = label_diets
  )+
  labs(
    title = "Distribution of diets in each scenario",
    x = "",
    y = "Proportion",
    fill = "diets"
  )+
  scale_x_discrete(labels = labels_scenario)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
  plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = rel(1.5)),
  axis.title.y = element_text(size = rel(1.5)), 
  legend.position = "bottom",
  legend.title = element_text(face = "bold", size = rel(1.5)))+
  guides(fill = guide_legend(
    nrow = 2,
    title.position = "top",
    title.hjust = 0.5
  ))


plot(graph_diets_rep_2)

################################################################################################################################
#                                             6. Description des régimes                                                       #
################################################################################################################################

DOUGLAS_diets_long <- DOUGLAS_diets %>%
  pivot_longer(
    cols = -food_group,
    names_to = "diet",
    values_to = "intake"
  ) %>%
  mutate(food_group = factor(food_group, levels = order_food_groups),
         diet = factor(diet, levels = order_diets))

graph_diets_desc <- ggplot(data = DOUGLAS_diets_long %>%
    filter(food_group != "added_plant_oils",
           diet %in% c("actuel", "actuel_calage", "meat2")),
                            aes(x = diet, y = intake, fill = food_group)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(
    values = col_food_groups,
    labels = labels_food_groups
  )+
  labs(
    title = "Intakes in each diet",
    x = "",
    y = "Intake (g/d/pers)",
    fill = "Food groups"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = rel(1.5)),
  axis.title.y = element_text(size = rel(1.5)), 
  legend.position = "bottom",
  legend.title = element_text(face = "bold", size = rel(1.5)))+
  scale_x_discrete(labels = label_diets)+
  guides(fill = guide_legend(
    nrow = 2,
    title.position = "top",
    title.hjust = 0.5
  ))


plot(graph_diets_desc)
################################################################################################################################
#                                             6. Exportation des données                                                      #
################################################################################################################################

ggsave(here("results", "1_Main_analysis_newDRF", "diets", "diets_rep.pdf"), plot = graph_diets_rep)
ggsave(here("results", "1_Main_analysis_newDRF", "diets", "diets_rep_2.pdf"), plot = graph_diets_rep_2)
ggsave(here("results", "5_actuel_meat2", "diets", "diets_actuel.pdf"), plot = graph_diets_desc)
