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
deaths_data <- import(here("Python_code", "data_python.csv"))

################################################################################################################################
#                                             3. Charte graphique                                                              #
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

# Etiquettes des scénarios
labels_scenario <- c(
  "actuel" = "Current diet",
  "sc1" = "Scenario 1",
  "sc2" = "Scenario 2",
  "sc3" = "Scenario 3",
  "sc4" = "Scenario 4"
)

################################################################################################################################
#                                             4. Décès par âge                                                                 #
################################################################################################################################

# Eliminer Les valeurs 5% les plus extrêmes
deaths_data <- deaths_data %>%
  group_by(scenario, year, age) %>%
  filter(between(deaths, quantile(deaths, 0.025), quantile(deaths, 0.975)))

# Moyenne et IC95 des décès par âge
simulations_summary_deaths <- deaths_data %>%
  group_by(scenario, year, age) %>%
  summarise(
    mean_deaths = mean(deaths, na.rm = TRUE),
    lower_ci = quantile(deaths, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(deaths, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             5. Décès évités par âge par rapport au baseline                                  #
################################################################################################################################

av_deaths <- deaths_data %>%
  group_by(scenario, year, age, simulation_id) %>%
  summarise(prevented_deaths = sum(avoided_deaths, na.rm = TRUE))

# Calcul de la moyenne et des IC95
simulations_summary_av_deaths <- av_deaths %>%
  group_by(scenario, age, year) %>%
  summarise(
    mean_prev_deaths = mean(prevented_deaths, na.rm = TRUE),
    lower_ci = quantile(prevented_deaths, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(prevented_deaths, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             6. Total des décès par année                                                     #
################################################################################################################################

# Calcul du total des décès / an / scenario
tot_deaths <- deaths_data %>%
  group_by(scenario, year, simulation_id) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

# Calcul de la moyenne et des IC95
simulations_summary_tot_deaths <- tot_deaths %>%
  group_by(scenario, year) %>%
  summarise(
    mean_tot_deaths = mean(total_deaths, na.rm = TRUE),
    lower_ci = quantile(total_deaths, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(total_deaths, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

################################################################################################################################
#                                             7. Total des décès évités par rapport au baseline                                #
################################################################################################################################

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
#                                             8. Figures : décès évités                                                        #
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
#                                             9. Exportation des données                                                      #
################################################################################################################################

# Décès par âge
export(simulations_summary_deaths, here("results", "HIA", "deaths.csv"))

# Décès évités par âge
export(av_deaths, here("results", "HIA", "av_deaths.csv"))
export(simulations_summary_av_deaths, here("results", "HIA", "av_deaths.csv"))

# Total des décès
export(tot_deaths, here("results", "HIA", "tot_deaths.csv"))
export(simulations_summary_tot_deaths, here("results", "HIA", "IC95_tot_deaths.xlsx"))

# Total des décès évités
export(tot_av_deaths, here("results", "HIA", "tot_deaths_prev.csv"))
export(simulations_summary_tot_av_deaths, here("results", "HIA", "tot_deaths_prev.csv"))

ggsave(here("results", "HIA", "tot_deaths_prev.pdf"), graph_tot_av_deaths)
ggsave(here("results", "HIA", "tot_deaths_prev_dates.pdf"), graph_tot_av_deaths_dates)
