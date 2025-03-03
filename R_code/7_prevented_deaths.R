################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
    rio, # Importation de fichiers
    here, # Localisation des fichiers dans le dossier du projet
    dplyr, # Manipulation des données
    tidyr, # Manipulation des données
    tidyverse # Data management, inclus ggplot
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Décès et décès évités par age et année
deaths_data <- import(here("results","Main analysis", "data_python.csv"))

################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Bornes temporelles des changements de régime alimentaire (années)
year_i <- 2025 # Année initiale
year_f <- 2050 # Année finale


ttfe_time <- 10

################################################################################################################################
#                                             4. Charte graphique                                                              #
################################################################################################################################

# Couleur de chaque scénario
col_scenario <- c(
  "actuel" = "azure4",
  "sc0" = "palevioletred3",
  "sc1" = "aquamarine3",
  "sc2" = "lightskyblue3",
  "sc3" = "#882255",
  "sc4" = "#DDCC77",
  "sc5" = "royalblue4"
)

# Couleur de chaque groupe d'aliments
col_food_groups <- c(
  "red_meat" = "#F60239",
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
  "sugar_sweetened_beverages" = "#004002"
)

# Ordonner les groupes alimentaires
order_food_groups <- c(
  "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
  "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
  "added_plant_oils", "sugar_sweetened_beverages"
)

# Etiquettes des scénarios et groupes d'aliments
labels_scenario <- c(
  "actuel" = "Current diet",
  "sc1" = "Scenario 1",
  "sc2" = "Scenario 2",
  "sc3" = "Scenario 3",
  "sc4" = "Scenario 4"
)

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
#                                             5. Total des décès évités par rapport au baseline                                #
################################################################################################################################

# Eliminer Les valeurs 5% les plus extrêmes
deaths_data <- deaths_data %>%
  group_by(scenario, year, age) %>%
  filter(between(deaths, quantile(deaths, 0.025), quantile(deaths, 0.975)))


# Calcul du total des décès évités / an / scenario
tot_av_deaths <- deaths_data %>%
  group_by(scenario, year, simulation_id) %>%
  summarise(total_av_deaths = sum(avoided_deaths, na.rm = TRUE))

# Calcul de la moyenne et des IC95
simulations_summary_tot_av_deaths <- tot_av_deaths %>%
  group_by(scenario, year) %>%
  summarise(
    mean_rr = mean(total_av_deaths, na.rm = TRUE),
    lower_ci = quantile(total_av_deaths, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(total_av_deaths, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             6. Figures : décès évités                                                        #
################################################################################################################################

graph_tot_av_deaths <- ggplot(
  simulations_summary_tot_av_deaths %>%
    filter(scenario != "actuel"),
  aes(
    x = year,
    y = mean_rr,
    group = scenario,
    color = scenario
  )
) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0) +
  geom_line(size = 0.6, na.rm = TRUE) +
  labs(
    title = "",
    x = "",
    y = "Number of deaths prevented"
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

graph_tot_av_deaths_dates <- ggplot(
  simulations_summary_tot_av_deaths %>%
    filter(
      year %in% c(2040, 2050, 2060),
      scenario != "actuel"
    ),
  aes(
    x = scenario,
    y = mean_rr,
    fill = scenario
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    alpha = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = lower_ci,
      ymax = upper_ci
    ),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  facet_wrap(~year,
    ncol = 3
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(
    values = col_scenario,
    labels = labels_scenario
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "",
    x = "",
    y = "Deaths prevented"
  ) +
  guides(fill = guide_legend(title = NULL))


################################################################################################################################
#                                             7. Exportation des données                                                      #
################################################################################################################################

export(simulations_summary_tot_av_deaths, here("results", "HIA", "tot_deaths_prev.csv"))

ggsave(here("results", "HIA", "tot_deaths_prev.pdf"), graph_tot_av_deaths)
ggsave(here("results", "HIA", "tot_deaths_prev_dates.pdf"), graph_tot_av_deaths_dates)
