################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr
)


################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

population_evo <- import(here("results", "deaths.csv"))

col_scenario <- c("actuel" = "azure4",
                  "sc0" = "palevioletred3",
                  "sc1" = "aquamarine3",
                  "sc2" = "#DDCC77",
                  "sc3" = "lightskyblue3",
                  "sc4" = "#882255",
                  "sc5" = "royalblue4")

labels_scenario <- c("actuel" = "BAU",
                     "sc1" = "Scenario 1",
                     "sc2" = "Scenario 2",
                     "sc3" = "Scenario 3",
                     "sc4" = "Scenario 4")

################################################################################################################################
#                                             3. Espérance de vie conditionnelle                                               #
################################################################################################################################

population_evo <- population_evo %>% 
  select("age", "year", "scenario", "simulation_id", "adjusted_mr", "population", "deaths") %>% 
  group_by(year, simulation_id, age) %>% 
  mutate(avoided_deaths = deaths[scenario == "actuel"] - deaths)

calc_conditional_LE <- function(df) {
  df %>%
    arrange(year, age) %>%
    group_by(year, simulation_id, scenario) %>%
    mutate(
      qx = adjusted_mr,                           # Taux de mortalité par âge (déjà dans les données)
      px = 1 - qx,                                   # Probabilité de survie à chaque âge
      lx = cumprod(lag(px, default = 1)),            # Probabilité de survie jusqu'à chaque âge
      dx = lx * qx,                                  # Nombre de décès attendu à chaque âge
      Tx = rev(cumsum(rev(lx)))                      # Somme cumulée des survivants pour espérance de vie
    ) %>%
    mutate(
      ex = Tx / lx                                   # Espérance de vie conditionnelle à chaque âge
    ) 
}

population_evo <- calc_conditional_LE(population_evo)

population_evo <- population_evo %>% 
  group_by(age, year, scenario, simulation_id) %>% 
  mutate(le = age + ex,
         ylg = avoided_deaths * (le - age)) %>% 
  select(simulation_id, age, year, scenario, deaths, avoided_deaths, le, ylg)

################################################################################################################################
#                                             4. Années de vie préservées                                                      #
################################################################################################################################

yll <- population_evo %>% 
  group_by(simulation_id, scenario, year) %>% 
  summarise(yll = sum(ylg))

yll <- yll %>% 
  group_by(scenario, year) %>% 
  filter(between(yll, quantile(yll, 0.025), quantile(yll, 0.975)))

summary_yll <- yll %>% 
  group_by(scenario, year) %>% 
  summarise(
    mean_yll = mean(yll, na.rm = TRUE),
    lower_ci = quantile(yll, 0.025, na.rm = TRUE),
    upper_ci = quantile(yll, 0.975, na.rm = TRUE)
  )

graph_yll <- ggplot(summary_yll %>% 
                                        filter(scenario != "actuel"),
                                      aes(x = year,
                                          y = mean_yll,
                                          group = scenario,
                                          color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  geom_line(size = 0.6, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "YLL prevented"
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
#                                             5. Espérance de vie gagnée                                                       #
################################################################################################################################

le <- population_evo %>% 
  group_by()

################################################################################################################################
#                                             6. Exportation des données                                                       #
################################################################################################################################

export(summary_yll, here("results", "IC95_yll.xlsx"))
ggsave(here("results", "yll_reported.pdf"))

